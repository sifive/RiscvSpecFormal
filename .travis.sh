#!/bin/bash

sudo add-apt-repository ppa:hvr/ghc --yes
sudo apt-get update
sudo apt-get install ghc-8.6.5 clang --yes

wget https://static.dev.sifive.com/dev-tools/riscv64-unknown-elf-gcc-8.2.0-2019.02.0-x86_64-linux-ubuntu14.tar.gz && tar xzf riscv64-unknown-elf-gcc-8.2.0-2019.02.0-x86_64-linux-ubuntu14.tar.gz

git clone https://github.com/riscv/riscv-tests && cd riscv-tests && git submodule update --init --recursive && autoconf && ./configure --prefix=$HOME/riscv && sed -i -e "s/0x80000000/00000000/g" ./env/p/link.ld && make -j && make install && cd .. && rm -rf riscv-tests

cp $HOME/haskell-files/*.hs .

cat Target.raw > Target.hs
echo "rtlMod = model$1" >> Target.hs
ghc -O0 --make Kami/PrettyPrintVerilog.hs
./Kami/PrettyPrintVerilog > System.sv

verilator --top-module system -Wno-CMPCONST -O0 -Wno-WIDTH --cc System.sv --trace --trace-underscore -Wno-fatal --exe System.cpp
make -j -C obj_dir -f Vsystem.mk Vsystem CXX=clang LINK=clang

ls $HOME/riscv/rv${1}u?-p-* | parallel -P 0 -j0 "(file {} | (grep -iq elf && (./runElf.sh {} || exit 1))) || (file {} | grep -viq elf)"
result=$?

if [[ $result == 0 ]]
then
  notice "All tests passed."
else
  error "The test suite failed."
fi
exit $result
