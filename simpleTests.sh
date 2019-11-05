#! /usr/bin/env bash

( \
	./doGenerate.sh --haskell --parallel && \
	( \
		./runElf.sh --xlen 64 --haskell --path $1/rv64ui-p-add & \
		./runElf.sh --xlen 64 --haskell --path $1/rv64ui-v-add & \
		./runElf.sh --xlen 32 --haskell --path $1/rv32ui-p-add & \
		./runElf.sh --xlen 32 --haskell --path $1/rv32ui-v-add & \
	) \
) && \
( \
	( \
		./doGenerate.sh --xlen 64 --parallel && \
		( \
			./runElf.sh --xlen 64 --path $1/rv64ui-p-add & \
			./runElf.sh --xlen 64 --path $1/rv64ui-v-add \
		) \
	) && \
	( \
		./doGenerate.sh --xlen 32 --parallel && \
		( \
			./runElf.sh --xlen 32 --path $1/rv32ui-p-add & \
			./runElf.sh --xlen 32 --path $1/rv32ui-v-add \
		) \
	) \
)
