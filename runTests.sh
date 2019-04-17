# !/bin/bash
# This script accepts a path to the rv32ui test directory included
# in the RISC-V Test Suite and runs a subset of the tests contained
# therein.

source common.sh

verbose=0

xlen=32

options=$(getopt --options="hvx:p:" --longoptions="help,verbose,version,xlen:,path:" -- "$@")
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

Arguments:
  --xlen 32|64
  Specifies whether we are running 32-bit or 64-bit tests.

  --path location
  Path to the directory where all the tests are located.

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
    -x|--xlen)
      xlen=$2
      shift 2;;
    -p|--path)
      path=$2
      shift 2;;
    --)
      shift
      break;;
  esac
done
shift $((OPTIND - 1))


[[ -z "$path" ]] && error "Invalid command line. The PATH argument is missing."

notice "Generating model".
./doGenerate.sh --xlen $xlen

notice "Running tests in $path."
for file in $(ls $path/rv${xlen}u?-p-*)
do
  file $file | grep -iq elf
  if [[ $? == 0 ]]
  then
    echo "Running test $(basename $file)."
    ./runElf.sh "$file"
    if [[ $? != 0 ]]
    then
      echo "The test suite failed."
    fi
  fi
done
notice "All tests passed."

