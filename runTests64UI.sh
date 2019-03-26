# !/bin/bash
# This script accepts a path to the rv32ui test directory included
# in the RISC-V Test Suite and runs a subset of the tests contained
# therein.

source common.sh

options=$(getopt --options="hv" --longoptions="help,verbose,version" -- "$@")
[ $? == 0 ] || error "Invalid command line. The command line includes one or more invalid command line parameters."

eval set -- "$options"
while true
do
  case "$1" in
    -h | --help)
      cat <<- EOF
Usage: ./runTests.sh [OPTIONS] PATH

This script accepts a path, PATH, to the rv32ui test directory
included in the RISC-V Test Suite https://github.com/riscv/
riscv-tools) and runs a subset of these tests contained therein.

If all of these selected tests complete successfully, this
script returns 0.

Options:

  -h|--help
  Displays this message.

  -v|--verbose
  Enables verbose output.

  --version
  Displays the current version of this program.

Example

./runTests.sh --verbose riscv-tests/build/isa/rv32ui-p-simple

Generates the RISC-V processor simulator.

Authors

1. Murali Vijayaraghavan
2. Larry Lee
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

files="
rv64ui-p-add
rv64ui-p-auipc
rv64ui-p-bne
rv64ui-p-ld
rv64ui-p-or
rv64ui-p-sll
rv64ui-p-sltiu
rv64ui-p-srl
rv64ui-p-sw
rv64ui-p-addi
rv64ui-p-beq
rv64ui-p-fence_i
rv64ui-p-lh
rv64ui-p-ori
rv64ui-p-slli
rv64ui-p-sltu
rv64ui-p-srli
rv64ui-p-xor
rv64ui-p-addiw
rv64ui-p-bge
rv64ui-p-jal
rv64ui-p-lhu
rv64ui-p-sb
rv64ui-p-slliw
rv64ui-p-sra
rv64ui-p-srliw
rv64ui-p-xori
rv64ui-p-addw
rv64ui-p-bgeu
rv64ui-p-jalr
rv64ui-p-lui
rv64ui-p-sd
rv64ui-p-sllw
rv64ui-p-srai
rv64ui-p-srlw
rv64ui-p-and
rv64ui-p-blt
rv64ui-p-lb
rv64ui-p-lw
rv64ui-p-sh
rv64ui-p-slt
rv64ui-p-sraiw
rv64ui-p-sub
rv64ui-p-andi
rv64ui-p-bltu
rv64ui-p-lbu
rv64ui-p-lwu
rv64ui-p-simple
rv64ui-p-slti
rv64ui-p-sraw
rv64ui-p-subw
"

[[ $# < 1 ]] && error "Invalid command line. The PATH argument is missing."
path=$1

notice "Running tests in $path."
for file in $files
do
  echo "Running test $file."
  ./runElf.sh "$path/$file"
  if [[ $? != 0 ]]
  then
    echo "The test suite failed."
  fi
done
