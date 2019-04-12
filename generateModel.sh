# !/bin/bash

# This script generates simulation programs that simulate RISC-V
# processors.
#
# It accepts an argument that specifies whether or not the processor
# should operate on 32 or 64 bit values and then generates a simulation
# program that simulates the selected processor type.

source common.sh

verbose=0
xlen=


options=$(getopt --options="hvx:" --longoptions="help,verbose,version,xlen:" -- "$@")
[ $? == 0 ] || error "Invalid command line. The command line includes one or more invalid command line parameters."

eval set -- "$options"
while true
do
  case "$1" in
    -h | --help)
      cat <<- EOF
Usage: ./generateModel.sh [OPTIONS] --xlen XLEN

This script generates simulation programs that simulate RISC-V
processors.

It accepts an argument that specifies whether or not the processor
should operate on 32 or 64 bit values and then generates a simulation
program that simulates the selected processor type.

Arguments:

  --xlen 32|64
  Specifies whether or not the generator should produce a 32 or 64
  bit RISC-V processor model.

Options:

  -h|--help
  Displays this message.

  -v|--verbose
  Enables verbose output.

  --version
  Displays the current version of this program.

Example

  ./generateModel.sh -v --xlen 32

  Generates a 32 bit processor model.

Authors

1. Kade Phillips
2. Murali Vijayaraghavan
3. Larry Lee
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
    --)
      shift
      break;;
  esac
done
shift $((OPTIND - 1))

[[ -z $xlen ]]   && error "Invalid command line. The <xlen> argument is missing."

tee Target.hs <<- EOF
module Target (module Syntax, module Rtl, module Word, module Fin, module EclecticLib, module PeanoNat, rtlMod) where

import EclecticLib
import PeanoNat
import Fin
import System hiding (unsafeCoerce)
import Rtl
import Syntax hiding (unsafeCoerce)
import Word

rtlMod :: RtlModule
rtlMod = model$xlen
EOF

cmd="./doGenerate.sh"
if [[ $verbose == 1 ]]
then
  cmd="$cmd -v"
fi
execute "$cmd"
