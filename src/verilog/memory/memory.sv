module memory (
    input logic i_clk,
    input logic i_rst,

    input logic [15:0] i_mem_rd_addr,
    input logic i_mem_wr_en,
    input logic [15:0] i_mem_wr_addr,
    input logic [7:0] i_mem_wr_data,
    output logic [7:0] o_mem_rd_data
);

  reg [7:0] memory[65535:0];

  always_ff @(posedge i_clk, posedge i_rst) begin
    if (i_rst) begin
      o_mem_rd_data <= 8'd0;
    end else begin
      if (i_mem_wr_en) begin
        memory[i_mem_wr_addr] <= i_mem_wr_data;
      end

      o_mem_rd_data <= memory[i_mem_rd_addr];
    end
  end

endmodule
