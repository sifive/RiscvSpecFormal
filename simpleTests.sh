#! /usr/bin/env bash

path=$1
pre=$2
test=$3

( \
	./doGenerate.sh --haskell --parallel && \
	( \
		./runElf.sh --xlen 64 --haskell --path $1/rv64$pre-p-$test & \
		./runElf.sh --xlen 64 --haskell --path $1/rv64$pre-v-$test & \
		./runElf.sh --xlen 32 --haskell --path $1/rv32$pre-p-$test & \
		./runElf.sh --xlen 32 --haskell --path $1/rv32$pre-v-$test & \
	) \
) && \
( \
	./doGenerate.sh --xlen 64 --parallel && \
	( \
		./runElf.sh --xlen 64 --path $1/rv64$pre-p-$test & \
		./runElf.sh --xlen 64 --path $1/rv64$pre-v-$test &\
	) \
) && \
( \
	./doGenerate.sh --xlen 32 --parallel && \
	( \
		./runElf.sh --xlen 32 --path $1/rv32$pre-p-$test & \
		./runElf.sh --xlen 32 --path $1/rv32$pre-v-$test \
	) \
)
