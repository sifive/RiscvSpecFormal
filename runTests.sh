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
rv32uc-p-rvc
rv32ua-p-amoadd_w
rv32ua-p-amoand_w
rv32ua-p-amoor_w
rv32ua-p-amoxor_w
rv32ua-p-amoswap_w
rv32ua-p-amomax_w
rv32ua-p-amomaxu_w
rv32ua-p-amomin_w
rv32ua-p-amominu_w
rv32ui-p-add
rv32ui-p-addi
rv32ui-p-and
rv32ui-p-andi
rv32ui-p-auipc
rv32ui-p-beq
rv32ui-p-bgeu
rv32ui-p-blt
rv32ui-p-bltu
rv32ui-p-bne
rv32ui-p-jal
rv32ui-p-jalr
rv32ui-p-lb
rv32ui-p-lbu
rv32ui-p-lh
rv32ui-p-lhu
rv32ui-p-lui
rv32ui-p-lw
rv32ui-p-or
rv32ui-p-ori
rv32ui-p-sb
rv32ui-p-sh
rv32ui-p-sll
rv32ui-p-slli
rv32ui-p-slt
rv32ui-p-slti
rv32ui-p-sltu
rv32ui-p-sltiu
rv32ui-p-sltu
rv32ui-p-sra
rv32ui-p-srai
rv32ui-p-srl
rv32ui-p-srli
rv32ui-p-sub
rv32ui-p-sw
rv32ui-p-xor
rv32ui-p-xori
rv32ui-p-simple
rv32um-p-div
rv32um-p-divu
rv32um-p-mul
rv32um-p-mulh
rv32um-p-mulhsu
rv32um-p-mulhu
rv32um-p-rem
rv32um-p-remu
rv32uf-p-fadd
rv32uf-p-fclass
rv32uf-p-fcmp
rv32uf-p-fcvt
rv32uf-p-fcvt_w
rv32uf-p-ldst
rv32uf-p-fmadd
rv32uf-p-fmin
rv32uf-p-move
rv32uf-p-recoding
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
#notice "All tests passed."
