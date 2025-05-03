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
    FETCH_IMM,
    LATCH_IMM,
    DECODE,
    EXECUTE,
    WRITEBACK
  } cpu_state_t;

  cpu_state_t state, next_state;

  logic [7:0] ir;
  logic [15:0] pc, sp;
  logic [2:0] m_cycle, next_m_cycle;
  logic [2:0] t_state, next_t_state;
  logic [2:0] M_CYCLE_MAX, T_STATE_MAX;
  logic [7:0] imm;

  logic is_ld_r_r;
  logic is_ld_r_imm;

  function [2:0] get_m_cycles(logic [7:0] ir);
    case (ir)
      8'h06, 8'h16, 8'h26, 8'h36, 8'h0E, 8'h1E, 8'h2E, 8'h3E: get_m_cycles = 2;

      default: get_m_cycles = 1;
    endcase
  endfunction

  function [2:0] get_t_states();
    get_t_states = 4;
  endfunction

  // State logic
  always_comb begin
    next_state   = state;
    next_m_cycle = m_cycle;
    next_t_state = t_state;

    if (t_state < T_STATE_MAX) begin
      next_t_state = t_state + 1;
    end else begin
      next_t_state = 0;

      if (m_cycle < M_CYCLE_MAX) begin
        next_m_cycle = m_cycle + 1;
      end else begin
        next_m_cycle = 0;

        case (state)
          FETCH:     next_state = DECODE;
          DECODE: begin
            if (is_ld_r_imm) next_state = FETCH_IMM;
            else next_state = EXECUTE;
          end
          FETCH_IMM: next_state = LATCH_IMM;
          LATCH_IMM: next_state = EXECUTE;
          EXECUTE:   next_state = WRITEBACK;
          WRITEBACK: next_state = FETCH;

          default: next_state = FETCH;
        endcase
      end
    end
  end

  // State machine
  always_ff @(posedge i_clk, posedge i_rst) begin
    if (i_rst) begin
      o_mem_wr_en <= 1'b0;
      o_mem_rd_addr <= 16'h0;
      o_mem_wr_addr <= 16'h0;
      o_mem_wr_data <= 8'h0;
      pc <= 16'h0100;
      sp <= 16'hFFFE;

      state <= FETCH;
      m_cycle <= 0;
      t_state <= 0;
      M_CYCLE_MAX <= 0;
      T_STATE_MAX <= 0;
    end else begin
      o_mem_wr_en <= 1'b0;
      o_mem_wr_data <= 8'h0;
      reg_wr_en <= 1'b0;
      state <= next_state;
      m_cycle <= next_m_cycle;
      t_state <= next_t_state;

      case (state)
        FETCH: begin
          if (m_cycle == 0 && t_state == 0) begin
            o_mem_rd_addr <= pc;
          end else if (m_cycle == 0 && t_state == T_STATE_MAX) begin
            pc <= pc + 1;
          end
        end

        DECODE: begin
          if (m_cycle == 0 && t_state == 0) begin
            ir <= i_mem_rd_data;
            M_CYCLE_MAX <= get_m_cycles(i_mem_rd_data) - 1;
            T_STATE_MAX <= get_t_states() - 1;

            is_ld_r_r <= 1'b0;
            is_ld_r_imm <= 1'b0;

            case (i_mem_rd_data)
              8'h41, 8'h42, 8'h43, 8'h44, 8'h45, 8'h47: is_ld_r_r <= 1'b1;
              8'h06, 8'h16, 8'h26, 8'h36, 8'h0E, 8'h1E, 8'h2E, 8'h3E: is_ld_r_imm <= 1'b1;

              default: ;
            endcase
          end
        end

        FETCH_IMM: begin
          if (m_cycle == 0 && t_state == 0) begin
            o_mem_rd_addr <= pc;
          end else if (m_cycle == 0 && t_state == T_STATE_MAX) begin
            pc <= pc + 1;
          end
        end

        LATCH_IMM: begin
          if (m_cycle == 0 && t_state == 0) begin
            imm <= i_mem_rd_data;
          end
        end

        EXECUTE: begin
          if (is_ld_r_r) begin
            if (m_cycle == 0 && t_state == 0) begin
              reg_a_sel  <= ir[2:0];
              reg_wr_sel <= 3'd0;
            end
          end else if (is_ld_r_imm) begin
            if (m_cycle == 0 && t_state == 0) begin
              case (ir)
                8'h06: reg_wr_sel <= 3'd0;
                8'h0E: reg_wr_sel <= 3'd1;
                8'h16: reg_wr_sel <= 3'd2;
                8'h1E: reg_wr_sel <= 3'd3;
                8'h26: reg_wr_sel <= 3'd4;
                8'h2E: reg_wr_sel <= 3'd5;
                8'h3E: reg_wr_sel <= 3'd7;

                default: reg_wr_sel <= 3'd0;
              endcase
            end
          end
        end

        WRITEBACK: begin
          if (is_ld_r_r) begin
            reg_wr_en   <= 1'b1;
            reg_wr_data <= reg_a;
          end else if (is_ld_r_imm) begin
            reg_wr_en   <= 1'b1;
            reg_wr_data <= imm;
          end
        end

        default: ;
      endcase
    end
  end
endmodule
