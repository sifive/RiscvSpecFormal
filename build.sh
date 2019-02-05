#/bin/bash
# This script updates the Kami source files and builds the RISC-V
# processor simulator.

verbose=0

# Accepts one argument: message, a message
# string; and prints the given message iff the
# verbose flag has been set.
function notice () {
  local msg=$1

  if [[ $verbose == 1 ]]
  then
    echo -e "\033[44mNotice:\033[0m $msg"
  fi
}

# Accepts one argument: message, a message
# string; and prints the given message iff the
# verbose flag has been set.
function error () {
  local emsg=$1

  echo -e "\033[41mError:\033[0m $emsg"
  exit 1
}

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

# Accepts one argument, $cmd, a bash command
# string, executes the command and returns an
# error message if it fails.
function execute () {
  local cmd=$1
  if [[ $verbose == 1 ]]
  then
    echo -e "\033[44mNotice:\033[0m $cmd"
  fi
  eval $cmd
  [ $? == 0 ] || error "An error occured while trying to execute the following command: "'"'$cmd'"'"."
}

notice "Updating the Kami dependencies."
git submodule update --init
make -j -C coq-record-update
make -j -C bbv
make -j -C Kami
make -j -C FpuKami
make -j -C ProcKami
make -j

if [[ $verbose == 1 ]]
then 
  ./doGenerate.sh --verbose
else
  ./doGenerate.sh
fi
