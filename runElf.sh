#!/usr/bin/env bash
# This script accepts one argument, a RISC-V binary program file
# path, and runs the program within the RISC-V processor simulator.

source common.sh

coqSim=0
haskellSim=0
verilogSim=0
interactive=""
interrupts=""
signature=""
sign_size=""
noprint=""
xlen=64
profile=""
heapdump=""

options=$(getopt --options="hvsp:x:" --longoptions="coq-sim,haskell-sim,signature:,sign_size:,help,verbose,interactive,haskell,noprint,path:,debug,enable-ext-interrupts,xlen:,profile,heapdump,verilog-sim" -- "$@")
[ $? == 0 ] || error "Invalid command line. The command line includes one or more invalid command line parameters."

eval set -- "$options"
while true
do
  case "$1" in
    -h | --help)
      cat <<- EOF
USAGE
-----

./runElf.sh [OPTIONS] PATH

This script reads the RISC-V binary referenced by PATH and executes
it within the RISC-V processor simulator.

OPTIONS
-------

  --debug
  Uses default values in place of random values (useful for when debugging against verilog)

  --enable-ext-interrupts
  Send random external interrupts to the processor model

  --coq-sim
  Runs the Coq simulator. See the doGenerate help docs for more information.

  --haskell-sim
  --haskell (DEPRECATED)
  -s        (DEPRECATED)
  Runs the haskell simulator. See the doGenerate help docs for more information.

  --heapdump
  Output a heap dump if an exception occurs during execution of the Haskell simulator.
  Note: the simulator must have been compiled with the --profile and --heapdump flags.

  -h|--help
  Displays this message.

  --path location
  Path to the directory where the test is located.

  --profile
  Output a heap trace during execution of the Haskell simulator.

  --sign_size size
  Size of the memory to dump

  --signature filename
  The name of the file to dump the memory

  -v|--verbose
  Enables verbose output.

  --verilog-sim
  Run the Verilog simulator. (DEFAULT). See the doGenerate help docs for more information.

  --xlen 32|64
  Specifies whether or not the generator should produce a 32 or 64
  bit RISC-V processor model. Default is 64.

EXAMPLES
--------

./runElf.sh -v rv32ui-p-and
Simulates the rv32ui-p-and test suite program in the RISC-V
processor simulator.

AUTHORS
-------

Murali Vijayaraghavan
Larry Lee
Evan Marzion
EOF
      exit 0;;
    --coq-sim)
      coqSim=1
      shift;;
    -v|--verbose)
      verbose=1
      shift;;
    -p|--path)
      path=$2
      shift 2;;
    --interactive)
      interactive="--interactive"
      shift;;
    -s|--haskell)
      haskellSim=1
      shift;;
    --haskell-sim)
      haskellSim=1
      shift;;
    --profile)
      profile="+RTS -p -RTS"
      shift;;
    --heapdump)
      heapdump="+RTS -xc -RTS"
      shift;;
    --debug)
      debug="--debug"
      shift;;
    --noprint)
      noprint="--noprint"
      shift;;
    --enable-ext-interrupts)
      interrupts="--enable-ext-interrupts"
      shift;;
    --signature)
      signaturev=$2
      signature=signature@$2
      shift 2;;
    --sign_size)
      sign_sizev=$2
      sign_size=sign_size@$2
      shift 2;;
    -x|--xlen)
      xlen=$2
      shift 2;;
    --verilog-sim)
      verilogSim=1
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

[[ $verilogSim == 1 ]] && execute "time ./models/model$xlen/obj_dir/Vsystem +sign_size=$sign_sizev +signature=$signaturev +testfile=$hexfile +boot_rom=boot_ROM_RV${xlen}.hex +tohost_address=$tohost_address > $dump/$base.out"
[[ $haskellSim == 1 ]] && execute "time ./Haskell/Main boot_rom=boot_ROM_RV${xlen}.hex $interactive $noprint testfile=$hexfile tohost_address:$tohost_address xlen@${xlen} $signature $sign_size $debug $interrupts $profile $heapdump > $dump/$base.out"
[[ $coqSim     == 1 ]] && execute "time ./Haskell/CoqMain boot_rom=boot_ROM_RV${xlen}.hex $interactive --debug $noprint testfile=$hexfile tohost_address:$tohost_address xlen@${xlen} $signature $sign_size $debug $interrupts > $dump/$base.out"

execute "time $cmd"

result=$?

exit $result
