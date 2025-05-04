module reg_file (
    input logic i_clk,
    input logic i_rst,

    input logic [2:0] i_reg_a_sel,
    input logic [2:0] i_reg_b_sel,
    input logic [2:0] i_reg_c_sel,
    input logic [2:0] i_reg_wr_sel,
    input logic i_reg_wr_en,
    input logic i_hl_wr_en,
    input logic i_bc_wr_en,
    input logic i_de_wr_en,
    input logic [7:0] i_reg_wr_data,
    input logic [15:0] i_hl_wr_data,
    input logic [15:0] i_bc_wr_data,
    input logic [15:0] i_de_wr_data,
    output logic [7:0] o_reg_a,
    output logic [7:0] o_reg_b,
    output logic [7:0] o_reg_c
);

  reg [7:0] registers[7:0];

  always_ff @(posedge i_clk, posedge i_rst) begin
    if (i_rst) begin
      for (int i = 0; i < 8; i++) begin
        registers[i] <= 8'd0;
      end
    end else begin
      if (i_bc_wr_en) begin
        registers[0] <= i_bc_wr_data[15:8];
        registers[1] <= i_bc_wr_data[7:0];
      end

      if (i_de_wr_en) begin
        registers[2] <= i_de_wr_data[15:8];
        registers[3] <= i_de_wr_data[7:0];
      end

      if (i_hl_wr_en) begin
        registers[4] <= i_hl_wr_data[15:8];
        registers[5] <= i_hl_wr_data[7:0];
      end

      if (i_reg_wr_en) begin
        if (!(i_hl_wr_en && (i_reg_wr_sel == 4 || i_reg_wr_sel == 5)) &&
            !(i_bc_wr_en && (i_reg_wr_sel == 0 || i_reg_wr_sel == 1)) &&
            !(i_de_wr_en && (i_reg_wr_sel == 2 || i_reg_wr_sel == 3))) begin
          registers[i_reg_wr_sel] <= i_reg_wr_data;
        end
      end
    end
  end

  always_comb begin
    o_reg_a = registers[i_reg_a_sel];
    o_reg_b = registers[i_reg_b_sel];
    o_reg_c = registers[i_reg_c_sel];
  end

endmodule
