module cpu (
    input logic i_clk,
    input logic i_rst,

    input logic [7:0] i_mem_rd_data,
    output logic [15:0] o_mem_rd_addr,
    output logic o_mem_wr_en,
    output logic [15:0] o_mem_wr_addr,
    output logic [7:0] o_mem_wr_data
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

  typedef enum logic [2:0] {
    FETCH,
    DECODE,
    EXECUTE
  } cpu_state_t;

  cpu_state_t state, next_state;

  logic [7:0] ir;
  logic [15:0] pc, sp;
  logic [2:0] m_cycle;
  logic [2:0] t_state;

  function [2:0] get_m_cycles(logic [7:0] ir);
    case (ir)
      default: get_m_cycles = 1;
    endcase
  endfunction

  function [2:0] get_t_states();
    get_t_states = 4;
  endfunction

  // State logic
  always_comb begin
    case (state)
      FETCH:  next_state = DECODE;
      DECODE: next_state = EXECUTE;

      default: next_state = FETCH;
    endcase
  end

  // State machine
  always_ff @(posedge i_clk, posedge i_rst) begin
    if (i_rst) begin
      o_mem_wr_en <= 1'b0;
      o_mem_rd_addr <= 16'h0;
      o_mem_wr_addr <= 16'h0;
      o_mem_wr_data <= 8'h0;
      state <= FETCH;
    end else begin
      o_mem_wr_en   <= 1'b0;
      o_mem_wr_data <= 8'h0;

      case (state)
        FETCH: begin
          o_mem_rd_addr <= pc;
          pc <= pc + 1;
        end

        DECODE: begin
          ir <= i_mem_rd_data;

          case (ir)
            8'h00: ;

            default: ;
          endcase
        end

        EXECUTE: begin
        end

        default: ;
      endcase

      if (m_cycle == get_m_cycles(ir) - 1) begin
        m_cycle <= 0;
        t_state <= 0;
        state   <= next_state;
      end else if (t_state == get_t_states() - 1) begin
        t_state <= 0;
        m_cycle <= m_cycle + 1;
      end else begin
        t_state <= t_state + 1;
      end
    end
  end

endmodule
