# !/bin/bash
# This script accepts one argument, a RISC-V binary program file
# path, and runs the program within the RISC-V processor simulator.

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
Usage: ./runELF.sh [OPTIONS] PATH

This script reads the RISC-V binary referenced by PATH and executes
it within the RISC-V processor simulator.

Options:

  -h|--help
  Displays this message.

  -v|--verbose
  Enables verbose output.

  --version
  Displays the current version of this program.

Example

./runELF.sh -v rv32ui-p-and

Simulates the rv32ui-p-and test suite program in the RISC-V
processor simulator.

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

[[ $# < 1 ]] && error "Invalid command line. The PATH argument is missing."
path=$1

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

notice "Generating a hex file from the binary program."
execute "riscv64-unknown-elf-objcopy -O verilog '$path' ./MemoryInit.hex"

notice "Remapping the memory addresses within the hex file."
execute "sed -i 's/@800/@000/g;' ./MemoryInit.hex || gsed -i 's/@800/@000/g;' ./MemoryInit.hex"

notice "Running $(basename $path)"
execute "./obj_dir/Vsystem"

notice "Done."
