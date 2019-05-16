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

hexfile=$(mktemp)

execute "riscv64-unknown-elf-objcopy -O verilog '$path' $hexfile"

pass_address=$(riscv64-unknown-elf-readelf -a $path | grep pass | awk '{print $2}')
fail_address=$(riscv64-unknown-elf-readelf -a $path | grep fail | awk '{print $2}')

if [[ ! $pass_address ]]
then
  pass_address=$(tail -n 1 $path.dump | awk '{print $1}')
fi
pass_address=${pass_address/:/}

notice "Running $(basename $path)"

cmd="./obj_dir/Vsystem +sign_size=8192 +signature=signature +testfile=$hexfile +pass_address=$pass_address"
if [[ $fail_address ]]
then
  cmd="$cmd +fail_address=$fail_address"
fi
fail_address=${fail_address/:/}
cmd="$cmd > system.out"
execute "$cmd"
result=$?

rm $hexfile
exit $result
