# !/bin/bash
# This script accepts one argument, a RISC-V binary program file
# path, and runs the program within the RISC-V processor simulator.

source common.sh

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

notice "Generating a hex file from the binary program."
execute "riscv64-unknown-elf-objcopy -O verilog '$path' ./MemoryInit.hex"

notice "Remapping the memory addresses within the hex file."
execute "sed -i 's/@800/@000/g;' ./MemoryInit.hex &> /dev/null || gsed -i 's/@800/@000/g;' ./MemoryInit.hex"

execute "riscv64-unknown-elf-objdump --disassemble $path > $path.asm"
fail_address=$(awk '/([[:alnum:]])* <fail>:/ {print $1}' $path.asm)
pass_address=$(awk '/([[:alnum:]])* <pass>:/ {print $1}' $path.asm)

echo "fail: $fail_address"
echo "pass: $pass_address"

notice "Running $(basename $path)"
execute "./obj_dir/Vsystem $fail_address $pass_address"

notice "Done."
