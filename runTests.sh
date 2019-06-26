#!/bin/bash
# This script accepts a path to the rv32ui test directory included
# in the RISC-V Test Suite and runs a subset of the tests contained
# therein.

source common.sh

result=0
parallel=0

xlen=32

options=$(getopt --options="hvcsx:p:" --longoptions="help,verbose,parallel,haskell,xlen:,path:" -- "$@")
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
  --path location
  Path to the directory where all the tests are located.
  --xlen 32|64
  Specifies whether we are running 32-bit or 64-bit tests.
  Default 32.
Options:
  -h|--help
  Displays this message.
  -v|--verbose
  Enables verbose output.
Example
./runTests.sh --verbose riscv-tests/build/isa/rv32ui-p-simple
Generates the RISC-V processor simulator.
Authors
Murali Vijayaraghavan
Larry Lee
Evan Marzion
EOF
      exit 0;;
    -v|--verbose)
      verboseflag="-v"
      shift;;
    -c|--parallel)
      parallel=1
      shift;;
    -s|--haskell)
      haskell="--haskell"
      shift;;
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
execute "./doGenerate.sh $verboseflag $haskell --xlen $xlen"

notice "Running tests in $path."

files=$(./fileList.sh $path $xlen)

if [[ $parallel == 0 ]]
then
  for file in $files
  do
    ((file $file | (grep -iq elf && ./runElf.sh $verboseflag $haskell --path $file)) || (file $file | grep -viq elf));
    result=$(( $? | $result ));
  done
else
  printf "$files" | parallel --bar -P 0 -j0 "(file {} | (grep -iq elf && ./runElf.sh $verboseflag $haskell --path {})) || (file {} | grep -viq elf)"
  result=$?
fi

if [[ $result == 0 ]]
then
  notice "All the tests that ran passed."
else
  error "The test suite failed."
fi
exit $result
