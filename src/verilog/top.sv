module top (
    input logic i_clk,
    input logic i_rst
);

  cpu u_cpu (
      .i_clk(i_clk),
      .i_rst(i_rst)
  );

  memory u_memory (
      .i_clk(i_clk),
      .i_rst(i_rst)
  );

endmodule
