module top #(
    parameter string FILE = ""
) (
    input logic i_clk,
    input logic i_rst
);

  logic [15:0] mem_rd_addr;
  logic mem_wr_en;
  logic [15:0] mem_wr_addr;
  logic [7:0] mem_wr_data;
  logic [7:0] mem_rd_data;

  cpu u_cpu (
      .i_clk(i_clk),
      .i_rst(i_rst),

      .i_mem_rd_data(mem_rd_data),
      .o_mem_rd_addr(mem_rd_addr),
      .o_mem_wr_en  (mem_wr_en),
      .o_mem_wr_addr(mem_wr_addr),
      .o_mem_wr_data(mem_wr_data)
  );

  memory #(
      .FILE(FILE)
  ) u_memory (
      .i_clk(i_clk),
      .i_rst(i_rst),

      .i_mem_rd_addr(mem_rd_addr),
      .i_mem_wr_en  (mem_wr_en),
      .i_mem_wr_addr(mem_wr_addr),
      .i_mem_wr_data(mem_wr_data),
      .o_mem_rd_data(mem_rd_data)
  );

endmodule
