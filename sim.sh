verilator -cc $(find src/verilog -name "*.sv") --exe --trace --top top src/testbench/top_tb.cpp
cd obj_dir
make -f Vtop.mk
./Vtop
