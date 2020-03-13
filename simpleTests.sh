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

resDir='simpleTestResults'
mkdir $resDir

function runTest {
  local xlen=$1
  local type=$2
  local sim=$3

  local fileName="rv$xlen$pre-$type-$test"
  local resPath="$resDir/$fileName.out"
  
  echo "runTest xlen=$xlen type=$type sim=$sim fileName=$fileName resPath=$resPath"

  rm -vf $resPath
  ./runElf.sh --xlen $xlen $sim --path $path/$fileName &> $resDir/$fileName$sim.out
}

function runTests {
  local xlen=$1
  local sim=$2

  echo "runTests xlen=$xlen sim=$sim"
  runTest $xlen 'p' $sim & runTest $xlen 'v' $sim
}

function runHaskellTests {
  local xlen=$1
  echo "runHaskellTests xlen=$xlen"
  runTests $xlen '--haskell-sim'
}

( \
  ./doGenerate.sh --haskell-sim --parallel && \
  ( \
    runHaskellTests 64 & \
    runHaskellTests 32 & \
  ) \
) && \
( \
  ./doGenerate.sh --xlen 64 --parallel && \
  ( \
    runTests 64 \
  ) \
) && \
( \
  ./doGenerate.sh --xlen 32 --parallel && \
  ( \
    runTests 32 \
  ) \
)
