module cpu (
    input logic i_clk,
    input logic i_rst
);

  logic [2:0] reg_a_sel;
  logic [2:0] reg_b_sel;
  logic [2:0] reg_wr_sel;
  logic reg_wr_en;
  logic [7:0] reg_wr_data;
  logic [7:0] reg_a;
  logic [7:0] reg_b;

  reg_file u_reg_file (
      .i_clk(i_clk),
      .i_rst(i_rst),

      .i_reg_a_sel(reg_a_sel),
      .i_reg_b_sel(reg_b_sel),
      .i_reg_wr_sel(reg_wr_sel),
      .i_reg_wr_en(reg_wr_en),
      .i_reg_wr_data(reg_wr_data),
      .o_reg_a(reg_a),
      .o_reg_b(reg_b)
  );

endmodule
