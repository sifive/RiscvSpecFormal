# !/bin/bash
# This script generates the RISC-V processor simulator from the
# Gallina and Verilog source files.

verbose=0
rebuild=0 # indicates whether or not to recompile source files that Make thinks have not changed.

# Accepts one argument: message, a message
# string; and prints the given message iff the
# verbose flag has been set.
function notice () {
  local msg=$1

  if [[ $verbose == 1 ]]
  then
    echo -e "\033[44mNotice:\033[0m $msg"
  fi
}

# Accepts one argument: message, a message
# string; and prints the given message iff the
# verbose flag has been set.
function error () {
  local emsg=$1

  echo -e "\033[41mError:\033[0m $emsg"
  exit 1
}

options=$(getopt --options="hrv" --longoptions="help,rebuild,verbose,version" -- "$@")
[ $? == 0 ] || error "Invalid command line. The command line includes one or more invalid command line parameters."

eval set -- "$options"
while true
do
  case "$1" in
    -h | --help)
      cat <<- EOF
Usage: ./doGenerate.sh [OPTIONS]

This script generates the RISC-V processor simulator from the
Gallina and Verilog source files.

By default this simulator program is ./obj_dir/Vsystem. It will
execute any program whose object code is stored in the hex file
MemoryInit.hex.

When run, the simulator outputs trace messages describing the state
of its registers and actions during each clock cycle.

The simulator also outputs a VCD trace file, named trace.vcd,
that can be viewed using programs such as GTKWave.

Options:

  -h|--help
  Displays this message.

  -r|--rebuild
  Recompile source files that Make believes have not changed.

  -v|--verbose
  Enables verbose output.

  --version
  Displays the current version of this program.

Example

./doGenerate.sh --verbose

Generates the RISC-V processor simulator.

Authors

1. Kade Phillips
2. Murali Vijayaraghavan
3. Larry Lee
EOF
      exit 0;;
    -v|--verbose)
      verbose=1
      shift;;
    -r|--rebuild)
      rebuild=1
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

# Accepts one argument, $cmd, a bash command
# string, executes the command and returns an
# error message if it fails.
function execute () {
  local cmd=$1
  if [[ $verbose == 1 ]]
  then
    echo -e "\033[44mNotice:\033[0m $cmd"
  fi
  eval $cmd
  [ $? == 0 ] || error "An error occured while trying to execute the following command: "'"'$cmd'"'"."
}

notice "Compiling the Gallina (COQ) source code."
cmd="make -j"
if [[ $rebuild == 1 ]]
then
  cmd="$cmd -B"
fi
execute "$cmd"

notice "Compiling the Verilog generator."
execute "ghc -O0 --make Kami/PrettyPrintVerilog.hs"

notice "Generating the Verilog model."
execute "Kami/PrettyPrintVerilog > Processor.sv"

notice "Generating the simulator source code."
cmd="verilator"
if [[ $verbose == 1 ]]
then
  cmd="$cmd  --debug --no-dump-tree"
fi
cmd="$cmd --top-module system -Wno-CMPCONST -O0 -Wno-WIDTH --cc System.sv --trace --trace-underscore -Wno-fatal --exe System.cpp"
execute "$cmd"

notice "Compiling the simulation program."
execute "make -j -C obj_dir -f Vsystem.mk Vsystem"

notice "Done."
