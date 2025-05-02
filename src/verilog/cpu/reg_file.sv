module reg_file (
    input logic i_clk,
    input logic i_rst,

    input logic [2:0] i_reg_a_sel,
    input logic [2:0] i_reg_b_sel,
    input logic [2:0] i_reg_wr_sel,
    input logic i_reg_wr_en,
    input logic [7:0] i_reg_wr_data,
    output logic [7:0] o_reg_a,
    output logic [7:0] o_reg_b
);

  reg [7:0] registers[7:0];

  always_ff @(posedge i_clk, posedge i_rst) begin
    if (i_rst) begin
      for (int i = 0; i < 8; i++) begin
        registers[i] <= 8'd0;
      end
    end else begin
      if (i_reg_wr_en) begin
        registers[i_reg_wr_sel] <= i_reg_wr_data;
      end
    end
  end

  always_comb begin
    o_reg_a = registers[i_reg_a_sel];
    o_reg_b = registers[i_reg_b_sel];
  end

endmodule
