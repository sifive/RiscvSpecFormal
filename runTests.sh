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
files=$(ls $path/rv${xlen}{u,m}?-{p,v}-*)
# files=$(ls $path/rv${xlen}ui-v-*)
for file in \
  rv32mi-p-csr \
  rv32mi-p-illegal \
  rv32mi-p-sbreak \
  rv32mi-p-shamt \
  rv32ua-v-amoadd_d \
  rv32ua-v-amoadd_w \
  rv32ua-v-amoand_d \
  rv32ua-v-amoand_w \
  rv32ua-v-amomax_d \
  rv32ua-v-amomaxu_d \
  rv32ua-v-amomaxu_w \
  rv32ua-v-amomax_w \
  rv32ua-v-amomin_d \
  rv32ua-v-amominu_d \
  rv32ua-v-amominu_w \
  rv32ua-v-amomin_w \
  rv32ua-v-amoor_d \
  rv32ua-v-amoor_w \
  rv32ua-v-amoswap_d \
  rv32ua-v-amoswap_w \
  rv32ua-v-amoxor_d \
  rv32ua-v-amoxor_w \
  rv32ua-v-lrsc \
  rv32uc-v-rvc \
  rv32ud-v-fadd \
  rv32ud-v-fcmp \
  rv32ud-v-fcvt \
  rv32ud-v-fcvt_w \
  rv32ud-v-fdiv \
  rv32ud-v-fmadd \
  rv32ud-v-fmin \
  rv32ud-v-ldst \
  rv32ud-v-recoding \
  rv32uf-v-fadd \
  rv32uf-v-fcmp \
  rv32uf-v-fcvt \
  rv32uf-v-fcvt_w \
  rv32uf-v-fdiv \
  rv32uf-v-fmadd \
  rv32uf-v-fmin \
  rv32uf-v-ldst \
  rv32uf-v-recoding \
  rv32ui-v-fence_i \
  rv32ui-v-lb \
  rv32ui-v-lbu \
  rv32ui-v-ld \
  rv32ui-v-lh \
  rv32ui-v-lhu \
  rv32ui-v-lw \
  rv32ui-v-lwu \
  rv32ui-v-sb \
  rv32ui-v-sd \
  rv32ui-v-sh \
  rv32ui-v-sub \
  rv32ui-v-subw \
  rv32ui-v-sw \
  rv32ui-v-xor \
  rv32ui-v-xori \
  rv64mi-p-access \
  rv64mi-p-csr \
  rv64mi-p-illegal \
  rv64mi-p-sbreak \
  rv64mi-p-scall \
  rv64ua-v-amoadd_d \
  rv64ua-v-amoadd_w \
  rv64ua-v-amoand_d \
  rv64ua-v-amoand_w \
  rv64ua-v-amomax_d \
  rv64ua-v-amomaxu_d \
  rv64ua-v-amomaxu_w \
  rv64ua-v-amomax_w \
  rv64ua-v-amomin_d \
  rv64ua-v-amominu_d \
  rv64ua-v-amominu_w \
  rv64ua-v-amomin_w \
  rv64ua-v-amoor_d \
  rv64ua-v-amoor_w \
  rv64ua-v-amoswap_d \
  rv64ua-v-amoswap_w \
  rv64ua-v-amoxor_d \
  rv64ua-v-amoxor_w \
  rv64ua-v-lrsc \
  rv64uc-v-rvc \
  rv64ud-v-fadd \
  rv64ud-v-fcmp \
  rv64ud-v-fcvt \
  rv64ud-v-fcvt_w \
  rv64ud-v-fdiv \
  rv64ud-v-fmadd \
  rv64ud-v-fmin \
  rv64ud-v-ldst \
  rv64ud-v-recoding \
  rv64uf-v-fadd \
  rv64uf-v-fcmp \
  rv64uf-v-fcvt \
  rv64uf-v-fcvt_w \
  rv64uf-v-fdiv \
  rv64uf-v-fmadd \
  rv64uf-v-fmin \
  rv64uf-v-ldst \
  rv64uf-v-recoding \
  rv64ui-v-fence_i \
  rv64ui-v-lb \
  rv64ui-v-lbu \
  rv64ui-v-ld \
  rv64ui-v-lh \
  rv64ui-v-lhu \
  rv64ui-v-lw \
  rv64ui-v-lwu \
  rv64ui-v-sb \
  rv64ui-v-sd \
  rv64ui-v-sh \
  rv64ui-v-sub \
  rv64ui-v-subw \
  rv64ui-v-sw \
  rv64ui-v-xor \
  rv64ui-v-xori
do
  files=${files/$path\/$file/}
done

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
