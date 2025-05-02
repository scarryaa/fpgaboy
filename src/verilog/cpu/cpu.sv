module cpu (
    input logic i_clk,
    input logic i_rst
);

  reg_file u_reg_file (
      .i_clk(i_clk),
      .i_rst(i_rst)
  );

endmodule
