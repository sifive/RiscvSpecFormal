#!/bin/bash
# This script accepts one argument, a RISC-V binary program file
# path, and runs the program within the RISC-V processor simulator.

source common.sh

haskell=0
interactive=0
interrupts=''

options=$(getopt --options="hvsp:" --longoptions="help,verbose,interactive,haskell,path:,debug,enable-ext-interrupts" -- "$@")
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
  --path location
  Path to the directory where the test is located.
  -s|--haskell
  Runs the haskell simulator
  --debug
  Uses default values in place of random values (useful for when debugging against verilog)
  --enable-ext-interrupts
  Send random external interrupts to the processor model
Example
./runElf.sh -v rv32ui-p-and
Simulates the rv32ui-p-and test suite program in the RISC-V
processor simulator.
Authors
Murali Vijayaraghavan
Larry Lee
Evan Marzion
EOF
      exit 0;;
    -v|--verbose)
      verbose=1
      shift;;
    -p|--path)
      path=$2
      shift 2;;
      --interactive)
      interactive=1
      shift;;
    -s|--haskell)
      haskell=1
      shift;;
    --debug)
      debug="--debug"
      shift;;
    --enable-ext-interrupts)
      interrupts="--enable-ext-interrupts"
      shift;;
    --)
      shift
      break;;
  esac
done
shift $((OPTIND - 1))

[[ -z "$path" ]] && error "Invalid command line. The PATH argument is missing."

base=$(basename $path)

if [[ $haskell == 0 ]]
then
    dump=verilogdump
else
    dump=haskelldump
fi

mkdir -p $dump
mkdir -p $dump

binfile=$dump/$base.bin
hexfile=$dump/$base.hex

execute "riscv64-unknown-elf-objcopy -O binary $path $binfile"
execute "hexdump -v -e '16/1 \"%02x \" \"\n\"' $binfile > $hexfile"

tohost_addr=$(riscv64-unknown-elf-readelf -a $path | grep '[^\.]\<tohost\>' | awk '{print $2}')

tohost_address=$((0x$tohost_addr - 0x80000000))
tohost_address=$(printf "%x" $tohost_address)

notice "Running $base"

if [[ $haskell == 0 ]]
then
    cmd="./obj_dir/Vsystem +sign_size=8192 +signature=$dump/$base.signature +testfile=$hexfile +tohost_address=$tohost_address > $dump/$base.out"
else
  if [[ $interactive == 0 ]]
  then
    cmd="./Main testfile=$hexfile tohost_address:$tohost_address $debug $interrupts > $dump/$base.out"
  else
    cmd="./Main testfile=$hexfile tohost_address:$tohost_address --interactive $debug $interrupts"
  fi  
fi

execute "time $cmd"

result=$?

exit $result
