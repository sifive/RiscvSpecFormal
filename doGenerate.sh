#!/bin/bash

# This script generates the RISC-V processor simulator from the
# Gallina and Verilog source files.

source common.sh

verbose=0
rebuild=0 # indicates whether or not to recompile source files that Make thinks have not changed.

xlen=32

haskell=0

options=$(getopt --options="hrvsx:" --longoptions="help,rebuild,verbose,haskell,xlen:" -- "$@")
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
  bit RISC-V processor model. Default is 32.
Options:
  -h|--help
  Displays this message.
  -s|--haskell
  Generates the haskell simulator.
  -r|--rebuild
  Recompiles source files that Make believes have not changed.
  -v|--verbose
  Enables verbose output.
Example
./doGenerate.sh --xlen 32 --verbose
Generates the RISC-V 32-bit verilog simulator.
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
    -x|--xlen)
      xlen=$2
      shift 2;;
    --)
      shift
      break;;
  esac
done
shift $((OPTIND - 1))

notice "Compiling the Gallina (COQ) source code."
cmd="time make -j"
if [[ $rebuild == 1 ]]
then
  cmd="$cmd -B"
fi
execute "$cmd"

./Kami/fixHaskell.sh ./Kami

if [[ $haskell == 0 ]]
then
  cat Haskell/Target.raw > Haskell/Target.hs
  echo "rtlMod = model$xlen" >> Haskell/Target.hs

  notice "Compiling the Verilog generator."
  execute "time ghc -j -O1 --make -iHaskell -iKami Kami/PrettyPrintVerilog.hs"
  #execute "time ghc -prof -fprof-auto -j +RTS -A128m -n4m -s -RTS -O1 --make Kami/PrettyPrintVerilog.hs"

  notice "Generating the Verilog model."
  execute "time Kami/PrettyPrintVerilog > System.sv"
    
  notice "Generating the simulator source code (i.e. C files)."
  execute "time verilator --top-module system -Wno-CMPCONST -O0 -Wno-WIDTH --cc System.sv --trace --trace-underscore -Wno-fatal --exe System.cpp"
    
  if [ -x "$(command -v clang)" ]; then
    compiler=clang
  else
    compiler=g++
  fi

  notice "Compiling the simulation program."
  execute "time make -j -C obj_dir -f Vsystem.mk Vsystem CXX=$compiler LINK=$compiler"
fi    

if [[ $haskell == 1 ]]
then 
  cat Haskell/HaskellTarget.raw > Haskell/HaskellTarget.hs
  echo "kami_model = (kami_model$xlen, $xlen)" >> Haskell/HaskellTarget.hs
  
  cp Main.raw Main.hs

  notice "Compiling the Haskell generator."
  execute "time ghc -j -O1 --make -iHaskell -iKami Main.hs"
#  execute "time ghc -prof -fprof-auto -j +RTS -A128m -n4m -s -RTS -O1 --make -iKami Main.hs"
  rm Main.hs
fi

notice "Done."
