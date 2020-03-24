#! /usr/bin/env bash
#
# Example Usage: ./simpleTests.sh '/nettmp/netapp1a/vmurali/riscv-tests/isa/' 'ui' 'add'

source common.sh

path=$1
[[ $path == '' ]] && error "Error: PATH argument is missing."

pre=$2
[[ $pre == '' ]] && error "Error: PRE argument is missing (example 'ui')."

test=$3
[[ $test == '' ]] && error "Error: TEST argument is missing (example 'add')."

rm -rf simpleTestsResult
mkdir -p simpleTestsResult

function runTest {
  local xlen=$1
  local type=$2
  local sim=$3

  local fileName="rv$xlen$pre-$type-$test"
  local resPath="simpleTestsResult/$fileName$sim.out"
  
  echo "run Test xlen=$xlen type=$type sim=$sim fileName=$fileName resPath=$resPath"

  rm -f $resPath
  ./runElf.sh --xlen $xlen $sim --path $path/$fileName &> $resPath
}

function runTests {
  local xlen=$1
  local sim=$2

  echo "run Tests xlen=$xlen sim=$sim"
  runTest $xlen 'p' $sim & runTest $xlen 'v' $sim
}

./doGenerate.sh --coq-sim --parallel
(runTests 64 --coq-sim & runTests 32 --coq-sim)
./doGenerate.sh --xlen 64 --parallel && ./doGenerate.sh --xlen 32 --parallel
(runTests 64 --verilog-sim & runTests 32 --verilog-sim)
