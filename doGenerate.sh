#!/usr/bin/env bash

# This script generates the RISC-V processor simulator from the
# Gallina and Verilog source files.

source common.sh

verbose=0
rebuild=''
xlen=64
parallel=''
testcase=''
profile=''
heapdump=''
coqSim=0
haskellSim=0
verilogSim=0
noSimSelected=1

options=$(getopt --options="hrvsx:pt:" --longoptions="coq-sim,help,rebuild,verbose,haskell,haskell-sim,xlen:,parallel,profile,heapdump,test:,verilog-sim" -- "$@")
[ $? == 0 ] || error "Invalid command line. The command line includes one or more invalid command line parameters."

eval set -- "$options"
while true
do
  case "$1" in
    -h | --help)
      cat <<- EOF
USAGE
-----

./doGenerate.sh [OPTIONS]

SUMMARY
-------

This script generates programs that simulate the Kami Processor model.

OPTIONS
-------

  --coq-sim
  Generates the Coq simulator. See DETAILS for more information.

  --haskell-sim
  --haskell (DEPRECATED)
  -s        (DEPRECATED)
  Generates the haskell simulator. See DETAILS for more information.

  --heapdump
  Compile the Haskell simulator so that if called with the
  +RTS -xc -RTS option it will dump its heap state if it
  encounters an exception while executing.
  (Requires --profile)

  -h|--help
  Displays this message.

  -p|--parallel
  Parallel build

  --profile
  Compile the Haskell simulator to support heap tracing and profiling.

  -r|--rebuild
  Recompiles source files that Make believes have not changed.

  -v|--verbose
  Enables verbose output.

  --verilog-sim
  Generates the Verilog-based simulator. See DETAILS for more information.

  --xlen 32|64
  Specifies whether or not the generator should produce a 32 or 64
  bit RISC-V processor model. Default is 64.

EXAMPLES
--------

./doGenerate.sh --xlen 32 --verbose --verilog-sim
Generates the RISC-V 32-bit Verilog simulator.

./doGenerate.sh --xlen 64 --haskell-sim --parallel --heapdump
Generates the Haskell RISCV-V 64-bit simulator with heapdump
support. If you run runElf with --heapdump it will dump the heap
if it encounters an exception.
 
DETAILS
-------

This script can generate simulation programs using three different
methods.

When run using the --verilog-sim flag, this script will generate a
simulator using the following method:

  Kami Processor model (Kami)
             |
             v
  Verilog printer function (Coq function)
             |
             | [extraction]
             v
  Verilog printer function (Haskell function)
             |
             | [compile]
             v
  Verilog printer program
             |
             | [execute]
             v
         Verilog model
             |
             | [Verilator + C compile]
             v
  Kami Processor simulation program

The Verilog model will be written to models/rvXX/System.sv. The
resulting binaries are written to models/.

When run using the --haskell-sim flag, this script will generate a
simulator using the following method:

  Kami Processor model (Kami)
            |
            | [extraction]
            v
  Kami Processor model (Haskell datastructure) 
            |
            |                      Kami interpreter (Haskell function)
            |                                     |
            +--------------------+----------------+
                                 |
                                 | [compile]
                                 v
                Kami Processor simulation program

When run using the --coq-sim flag, this script will generate a
simulator using the following method

  Kami Processor model (Kami)
            |
            v
  Kami simulation function (Coq function)
            |
            | [extraction]
            v
  Kami Processor simulation function (Haskell function)
            |
            | [compile]
            v
  Kami Processor simulation program

AUTHORS
-------

* Murali Vijayaraghavan
* Larry Lee
* Evan Marzion
EOF
      exit 0;;
    -v|--verbose)
      verbose=1
      shift;;
    -r|--rebuild)
      rebuild='-B'
      shift;;
    -s|--haskell) # DEPRECATED
      haskellSim=1
      noSimSelected=0
      shift;;
    --haskell-sim)
      haskellSim=1
      noSimSelected=0
      shift;;
    -c|--coq-sim)
      coqSim=1
      noSimSelected=0
      shift;;
    --profile)
      profile='-prof -fprof-auto -rtsopts'
      shift;;
    --heapdump)
      heapdump='-fprof-cafs'
      shift;;
    -p|--parallel)
      parallel='-j'
      shift;;
    -x|--xlen)
      xlen=$2
      shift 2;;
    -t|--test)
      testcase=$2
      shift 2;;
    --verilog-sim)
      verilogSim=1
      noSimSelected=0
      shift;;
    --)
      shift
      break;;
  esac
done
shift $((OPTIND - 1))

execute "time make $rebuild $parallel"

function buildHaskellSim {
  local fileName=$1
  
  cd Kami && ./fixHaskell.sh ../HaskellGen .. && cd ..
  cp Haskell/HaskellTarget.hs HaskellGen
  cp Haskell/Main.hs HaskellGen
  cp Haskell/UART.hs HaskellGen
  execute "time ghc $GHCFLAGS $parallel $profile $heapdump -O2 --make -iHaskellGen -iKami ./Haskell/$fileName.hs -o ./Haskell/$fileName"
}

[[ $coqSim     == 1  ]] && buildHaskellSim 'CoqMain'
[[ $haskellSim == 1  ]] && buildHaskellSim 'Main'
[[ $testcase   != '' ]] && buildHaskellSim 'TestMain'

if [[ $verilogSim == 1 || $noSimSelected == 1 ]]
then
  if [[ $testcase == "" ]]
  then
    model=model$xlen
  else
    model=test$testcase
  fi

  cat Haskell/Target.raw > HaskellGen/Target.hs
  echo "rtlMod = separateModRemove $model" >> HaskellGen/Target.hs

  cd Kami && ./fixHaskell.sh ../HaskellGen .. && cd ..

  notice "Compiling the Verilog generator."
  execute "time ghc $GHCFLAGS $parallel $prof -O1 --make -iHaskellGen -iKami -iKami/Compiler Kami/Compiler/CompAction.hs"

  notice "Generating the Verilog model."
  execute "mkdir -p models/$model; time Kami/Compiler/CompAction > models/$model/System.sv"
    
  notice "Generating the simulator source code (i.e. C files)."
  execute "cd models/$model; time verilator --top-module system -Wno-CMPCONST -O0 -Wno-WIDTH --cc System.sv --trace --trace-underscore -Wno-fatal --exe System.cpp; cd ../.."
    
  if [ -x "$(command -v clang)" ]; then
    compiler=clang
  else
    compiler=g++
  fi

  notice "Compiling the simulation program."
  execute "cd models/$model; time make $parallel -C obj_dir -f Vsystem.mk Vsystem CXX=$compiler LINK=$compiler; cd ../.."
fi
notice "Done."
