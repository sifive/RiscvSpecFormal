#!/usr/bin/env bash

# This script generates the RISC-V processor simulator from the
# Gallina and Verilog source files.

source common.sh

verbose=0
rebuild=0 # indicates whether or not to recompile source files that Make thinks have not changed.
xlen=64
haskell=0
parallel=""
testcase=""
profile=""
heapdump=""

options=$(getopt --options="hrvsx:pt:" --longoptions="help,rebuild,verbose,haskell,xlen:,parallel,profile,heapdump,test:" -- "$@")
[ $? == 0 ] || error "Invalid command line. The command line includes one or more invalid command line parameters."

eval set -- "$options"
while true
do
  case "$1" in
    -h | --help)
      cat <<- EOF
Usage: ./doGenerate.sh [ARGUMENTS] [OPTIONS]
This script generates the RISC-V processor simulator from the
Gallina and Verilog source files.
By default this simulator program is ./obj_dir/Vsystem. It will
execute any program whose object code is stored in the hex file
stored in argument "+testfile".
When run, the simulator outputs trace messages describing the state
of its registers and actions during each clock cycle.
The simulator also outputs a VCD trace file, named trace.vcd,
that can be viewed using programs such as GTKWave.
Arguments:
  --xlen 32|64
  Specifies whether or not the generator should produce a 32 or 64
  bit RISC-V processor model. Default is 64.
Options:
  -h|--help
  Displays this message.
  -s|--haskell
  Generates the haskell simulator.
  -r|--rebuild
  Recompiles source files that Make believes have not changed.
  --profile
  Compile the Haskell simulator to support heap tracings
  (i.e. Profiling)
  --heapdump
  Compile the Haskell simulator so that if called with the
  +RTS -xc -RTS option it will dump its heap state if it
  encounters an exception while executing.
  (Requires the --profile)
  -p|--parallel
  Parallel build
  -v|--verbose
  Enables verbose output.
Example
./doGenerate.sh --xlen 32 --verbose
Generates the RISC-V 32-bit verilog simulator.
./doGenerate.sh --xlen 64 --haskell --parallel --heapdump
Generates the Haskell RISCV-V 64-bit simulator with heapdump
support. If you run runElf with --heapdump it will dump the heap
if it encounters an exception.
 
Authors
Murali Vijayaraghavan
Larry Lee
Evan Marzion
EOF
      exit 0;;
    -v|--verbose)
      verbose=1
      shift;;
    -r|--rebuild)
      rebuild=1
      shift;;
    -s|--haskell)
      haskell=1
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
    --)
      shift
      break;;
  esac
done
shift $((OPTIND - 1))

notice "Compiling the Gallina (COQ) source code."
cmd="time make $parallel"
if [[ $rebuild == 1 ]]
then
  cmd="$cmd -B"
fi
execute "$cmd"

cd Kami && ./fixHaskell.sh ../HaskellGen .. && cd ..

if [[ $testcase == "" ]]
then
  model=model$xlen
else
  model=test$testcase
fi

cp Haskell/UART.hs HaskellGen

if [[ $haskell == 0 ]]
then
  cat Haskell/Target.raw > HaskellGen/Target.hs
  echo "rtlMod = separateModRemove $model" >> HaskellGen/Target.hs

  notice "Compiling the Verilog generator."
  execute "time ghc $GHCFLAGS $parallel $prof -O1 --make -iHaskellGen -iKami -iKami/Compiler Kami/Compiler/CompAction.hs"
  #execute "time ghc $GHCFLAGS $parallel -prof -fprof-auto +RTS -A128m -n4m -s -RTS -O1 --make -iHaskellGen -iKami -iKami/Compiler Kami/Compiler/CompAction.hs"

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

if [[ $haskell == 1 ]]
then 
  if [[ $testcase == "" ]]
  then
    notice "Compiling the Haskell generator."
    cp Haskell/HaskellTarget.hs HaskellGen
    cp Haskell/Main.hs HaskellGen
    execute "time ghc $GHCFLAGS $parallel $profile $heapdump -O1 --make -iHaskellGen -iKami ./Haskell/Main.hs"
    notice "Done: Generated Main."
  else
    notice "Compiling Simulation Test."
    cp Haskell/HaskellTarget.hs HaskellGen
    execute "time ghc $GHCFLAGS $parallel $profile -O1 --make -iHaskellGen -iKami ./Haskell/TestMain.hs -o TestMain"
    notice "Done: Generated TestMain."
  fi
fi

notice "Done."
