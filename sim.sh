verilator -cc $(find src/verilog -name "*.sv") --exe --top top src/testbench/top_tb.cpp
