# !/bin/bash
# This script accepts one argument, a RISC-V binary program file
# path, and runs the program within the RISC-V processor simulator.

source common.sh

options=$(getopt --options="hv" --longoptions="help,verbose" -- "$@")
[ $? == 0 ] || error "Invalid command line. The command line includes one or more invalid command line parameters."

eval set -- "$options"
while true
do
  case "$1" in
    -h | --help)
      cat <<- EOF
Usage: ./runElf.sh [OPTIONS] PATH
This script reads the RISC-V binary referenced by PATH and executes
it within the RISC-V processor simulator.
Options:
  -h|--help
  Displays this message.
  -v|--verbose
  Enables verbose output.
Example
./runELF.sh -v rv32ui-p-and
Simulates the rv32ui-p-and test suite program in the RISC-V
processor simulator.
Authors
Murali Vijayaraghavan
Larry Lee
EOF
      exit 0;;
    -v|--verbose)
      verbose=1
      shift;;
    --)
      shift
      break;;
  esac
done
shift $((OPTIND - 1))

[[ $# < 1 ]] && error "Invalid command line. The PATH argument is missing."
path=$1

base=$(basename $path)

mkdir -p dump

hexfile=dump/$base.hex

execute "riscv64-unknown-elf-objcopy -O verilog '$path' $hexfile"

tohost_address=$(riscv64-unknown-elf-readelf -a $path | grep '[^\.]\<tohost\>' | awk '{print $2}')

notice "Running $base"

cmd="./obj_dir/Vsystem +sign_size=8192 +signature=dump/$base.signature +testfile=$hexfile +tohost_address=$tohost_address > dump/$base.out"
execute "$cmd"
result=$?

exit $result
