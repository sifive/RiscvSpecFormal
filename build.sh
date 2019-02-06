# !/bin/bash

# This script updates the Kami source files and builds the RISC-V
# processor simulator.

source common.sh

options=$(getopt --options="hv" --longoptions="help,verbose,version" -- "$@")
[ $? == 0 ] || error "Invalid command line. The command line includes one or more invalid command line parameters."

eval set -- "$options"
while true
do
  case "$1" in
    -h | --help)
      cat <<- EOF
Usage: ./build.sh [OPTIONS]

This script updates the Kami source file dependencies and builds
the RISC-V processor simulator.

By default this simulator program is ./obj_dir/Vsystem. It will
execute any program whose object code is stored in the hex file
MemoryInit.hex.

When run, the simulator outputs trace messages describing the state
of its registers and actions during each clock cycle.

The simulator also outputs a VCD trace file, named trace.vcd,
that can be viewed using programs such as GTKWave.

Options:

  -h|--help
  Displays this message.

  -v|--verbose
  Enables verbose output.

  --version
  Displays the current version of this program.

Example

./build.sh --verbose

Generates the RISC-V processor simulator.

Authors

1. Kade Phillips
2. Murali Vijayaraghavan
3. Larry Lee
EOF
      exit 0;;
    -v|--verbose)
      verbose=1
      shift;;
    --version)
      echo "version: 1.0.0"
      exit 0;;
    --)
      shift
      break;;
  esac
done
shift $((OPTIND - 1))

notice "Updating the Kami dependencies."
execute "git submodule update --init"
execute "execute make -j -C coq-record-update"
execute "make -j -C bbv"
execute "make -j -C Kami"
execute "make -j -C FpuKami"
execute "make -j -C ProcKami"
execute "make -j"

if [[ $verbose == 1 ]]
then 
  execute "./doGenerate.sh --verbose"
else
  execute "./doGenerate.sh"
fi
