# !/bin/bash
# This script runs a RISC-V test program such as rv32ui-p-bge and
# performs a simple check to see whether or not the simulator passed
# all of the tests within the program.

source common.sh

[[ $# < 1 ]] && error "Invalid command line. The PATH argument is missing."
path=$1

execute "./runElf.sh $path > system.out"
execute "riscv64-unknown-elf-objdump --disassemble $path > $path.asm"
fail_address=$(awk '/([[:alnum:]])* <fail>:/ {print $1}' $path.asm)
pass_address=$(awk '/([[:alnum:]])* <pass>:/ {print $1}' $path.asm)
execute "grep --color --quiet $fail_address system.out"
failed=$?
execute "grep --color --quiet $pass_address system.out"
passed=$?
if [[ $failed == 1 && $passed == 0 ]]
then
  notice "passed"
else
  error "failed"
fi
