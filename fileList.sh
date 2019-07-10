#!/bin/bash

dir=$1
xlen=$2

allfilesp=$(find "$dir" -maxdepth 1 -regex ".*/rv$xlen[um].-p[^\.]*")
allfilesv=$(find "$dir" -maxdepth 1 -regex ".*/rv$xlen[um].-v[^\.]*")

allfiles="$allfilesp $allfilesv"

for file in $allfiles
do
  for badfile in \
    rv32mi-p-csr \
    rv32mi-p-illegal \
    rv32mi-p-sbreak \
    rv32mi-p-shamt \
    rv32uc-v-rvc \
    rv64mi-p-access \
    rv64mi-p-csr \
    rv64mi-p-illegal \
    rv64mi-p-sbreak \
    rv64uc-v-rvc
  do
    insert=1
    if [[ $file == "$dir/$badfile" ]]
    then
      insert=0
      break
    fi
  done
  if [[ $insert == 1 ]]
  then
    files=$(printf "$files\n$file")
  fi
done
echo "$files"
