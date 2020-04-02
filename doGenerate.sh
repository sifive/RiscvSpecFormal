#!/usr/bin/env bash

# This script generates the RISC-V processor simulator from the
# Gallina and Verilog source files.

source common.sh

verbose=0
rebuild=""
xlen=64
parallel=""
testcase=""
profile=""
heapdump=""
coqSim=0
haskellSim=0
verilogSim=0
verilogCore=0
noSimSelected=1

options=$(getopt --options="hrvsx:pt:" --longoptions="coq-sim,help,rebuild,verbose,haskell-sim,xlen:,parallel,profile,heapdump,test:,verilog-sim,verilog-core" -- "$@")
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

This script generates programs that simulate the Kami Processor
model, and may be used to generate a Verilog model of the Kami
Processor core.

OPTIONS
-------

  --coq-sim
  Generates the Coq simulator. See DETAILS for more information.

  --haskell-sim
  -s
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

  --verilog-core
  Generates a Verilog model of the Kami processor core. See DETAILS
  for more information. (Cannot be used with --verilog-sim).

  --verilog-sim
  Generates the Verilog-based simulator. See DETAILS for more information.
  (Cannot be used with --verilog-core).

  --xlen 32|64
  Specifies whether or not the generator should produce a 32 or 64
  bit RISC-V processor model. Default is 64.

  -t|--test
  Overrides --verilog-sim and --verilog-core.

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

When run using the --verilog-core flag, this script will generate
a Verilog model of the ProcKami processor that does not include the
device models. This model of a ProcKami core can be integrated into
the Chisel based frameworks provided by SiFive. The generated cores
will be written to models/coreXX/System.sv.

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
      rebuild="-B"
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
      profile="-prof -fprof-auto -rtsopts"
      shift;;
    --heapdump)
      heapdump="-fprof-cafs"
      shift;;
    -p|--parallel)
      parallel="-j"
      shift;;
    -x|--xlen)
      xlen=$2
      shift 2;;
    -t|--test)
      testcase=$2
      shift 2;;
    --verilog-core)
      verilogCore=1
      shift;;
    --verilog-sim)
      verilogSim=1
      shift;;
    --)
      shift
      break;;
  esac
done
shift $((OPTIND - 1))

execute "time make $rebuild $parallel"

cd Kami && ./fixHaskell.sh ../HaskellGen .. && cd ..
cp Haskell/*.hs HaskellGen
function buildSim {
  local fileName=$1
  execute "time ghc $GHCFLAGS $parallel $profile $heapdump -O2 --make -iHaskellGen -iKami ./HaskellGen/$fileName.hs -o ./HaskellGen/$fileName"
}

[[ $coqSim     == 1  ]] && buildSim "CoqSim"
[[ $haskellSim == 1  ]] && buildSim "HaskellSim"
[[ $testcase   != "" ]] && buildSim "TestMain"

if [[ $verilogSim == 1 && $verilogCore == 1 ]]
then
  error "The --verilogSim and --verilogCore flags are incompatible. See --help for more information."
  exit 1
fi

if [[ $verilogSim == 1 || $verilogCore == 1 || $noSimSelected == 1 ]]
then
  if [[ $testcase != ""   ]]; then model=test$testcase; fi
  if [[ $verilogSim == 1  ]]; then model=model$xlen; fi
  if [[ $verilogCore == 1 ]]; then model=core$xlen; fi
  
  echo "rtlMod = separateModRemove $model" >> HaskellGen/Target.hs

  execute "time ghc $GHCFLAGS $parallel $profile $heapdump -O2 --make -iHaskellGen -iKami -iKami/Compiler Kami/Compiler/CompAction.hs"

  notice "Generating the Verilog model."
  execute "mkdir -p models/$model; time Kami/Compiler/CompAction > models/$model/System.sv"
  exitCode=$?
 
  if [[ $verilogCore == 1 ]]
  then
    notice "Done."
    exit $exitCode
  fi

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
