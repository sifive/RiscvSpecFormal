# !/bin/bash

echo $1

cat Target.raw > Target.hs
echo "rtlMod = model$1" >> Target.hs
#ghc -j + RTS -A128m -n4m -O0 --make Kami/PrettyPrintVerilog.hs
ghc -O0 --make Kami/PrettyPrintVerilog.hs
./Kami/PrettyPrintVerilog > System.sv
verilator --top-module system -Wno-CMPCONST -O0 -Wno-WIDTH --cc System.sv --trace --trace-underscore -Wno-fatal --exe System.cpp
make -j -C obj_dir -f Vsystem.mk Vsystem CXX=clang LINK=clang

