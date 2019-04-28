# !/bin/bash

# This script generates the RISC-V processor simulator from the
# Gallina and Verilog source files.

source common.sh

verbose=0
rebuild=0 # indicates whether or not to recompile source files that Make thinks have not changed.
skip_kami=0
tune_ghc=1
travis=0

xlen=32

options=$(getopt --options="hgrktvx:" --longoptions="help,generic-ghc,rebuild,skip-kami,travis,verbose,version,xlen:" -- "$@")
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
MemoryInit.hex.

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

  -r|--rebuild
  Recompile source files that Make believes have not changed.

  -k|--skip-kami
  Skip compiling the Coq/Kami source files.

  -g|--generic-ghc
  Omit tuning GHC compiler flags.

  -t|--travis
  Tune GHC to run in a Travis environment.

  -v|--verbose
  Enables verbose output.

  --version
  Displays the current version of this program.

Example

./doGenerate.sh --xlen 32 --verbose

Generates the RISC-V 32-bit processor simulator.

Authors

1. Kade Phillips
2. Murali Vijayaraghavan
3. Larry Lee
EOF
      exit 0;;
    -v|--verbose)
      verbose=1
      shift;;
    -r|--rebuild)
      rebuild=1
      shift;;
    -k|--skip-kami)
      skip_kami=1
      shift;;
    -t|--travis)
      travis=1
      tune_ghc=0
      shift;;
    -g|--generic-ghc)
      tune_ghc=0
      shift;;
    --version)
      echo "version: 1.0.0"
      exit 0;;
    -x|--xlen)
      xlen=$2
      shift 2;;
    --)
      shift
      break;;
  esac
done
shift $((OPTIND - 1))

if [[ $skip_kami == 1 ]]
then
  notice "Skipping compiling the Coq/Kami source code files."
else
  notice "Compiling the Gallina (COQ) source code."
  cmd="make -j"
  if [[ $rebuild == 1 ]]
  then
    cmd="$cmd -B"
  fi
  execute "$cmd"
fi

echo $xlen >> Target.hs

if [[ $tune_ghc == 1 ]]
then
  ghcflags="-j +RTS -A128m -n4m -s -RTS"
fi

if [[ $travis == 1 ]]
then
  travisflags="-v +RTS -K128m -RTS -rtsopts"
fi

notice "Compiling the Verilog generator."
execute "time ghc $ghcflags $travisflags -O0 --make Kami/PrettyPrintVerilog.hs"

notice "Generating the Verilog model."
execute "time Kami/PrettyPrintVerilog $travisflags > System.sv"

notice "Generating the simulator source code."
execute "time verilator --top-module system -Wno-CMPCONST -O0 -Wno-WIDTH --cc System.sv --trace --trace-underscore -Wno-fatal --exe System.cpp"

notice "Compiling the simulation program."
if [ -x "$(command -v clang)" ]; then
  compiler=clang
else
  compiler=g++
fi

execute "time make -j -C obj_dir -f Vsystem.mk Vsystem CXX=$compiler LINK=$compiler"

notice "Done."
