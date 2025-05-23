#include "../../obj_dir/Vtop.h"
#include <stdlib.h>
#include <verilated.h>
#include <verilated_vcd_c.h>

#define MAX_SIM_TIME 2000
vluint64_t sim_time = 0;

int main(int argc, char **argv, char **env) {
  Vtop *dut = new Vtop;

  Verilated::traceEverOn(true);
  VerilatedVcdC *m_trace = new VerilatedVcdC;
  dut->trace(m_trace, 5);
  m_trace->open("top_waveform.vcd");

  while (sim_time < MAX_SIM_TIME) {
    if (sim_time < 10) {
      dut->i_rst = 1;
    } else {
      dut->i_rst = 0;
    }

    dut->i_clk ^= 1;
    dut->eval();
    m_trace->dump(sim_time);
    sim_time++;
  }

  m_trace->close();
  delete dut;
  exit(EXIT_SUCCESS);
}
