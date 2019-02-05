# !/bin/bash
# This script runs a RISC-V test program such as rv32ui-p-bge and
# performs a simple check to see whether or not the simulator passed
# all of the tests within the program.

# Accepts one argument: message, a message
# string; and prints the given message iff the
# verbose flag has been set.
function notice () {
  local msg=$1

  echo -e "\033[44mNotice:\033[0m $msg"
}

# Accepts one argument: message, a message
# string; and prints the given message iff the
# verbose flag has been set.
function error () {
  local emsg=$1

  echo -e "\033[41mError:\033[0m $emsg"
  exit 1
}

[[ $# < 1 ]] && error "Invalid command line. The PATH argument is missing."
path=$1

./runELF.sh $path > system.out
riscv64-unknown-elf-objdump --disassemble $path > $path.asm
sed -i 's/^8/0/;' $path.asm
fail_address=$(awk '/([[:alnum:]])* <fail>:/ {print $1}' $path.asm)
pass_address=$(awk '/([[:alnum:]])* <pass>:/ {print $1}' $path.asm)
grep --color --quiet $fail_address system.out
failed=$?
grep --color --quiet $pass_address system.out
passed=$?
if [[ $failed == 1 && $passed == 0 ]]
then
  notice "passed"
else
  error "failed"
fi
