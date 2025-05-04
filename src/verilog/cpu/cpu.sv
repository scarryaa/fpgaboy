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
  logic [2:0] reg_c_sel;
  logic [2:0] reg_wr_sel;
  logic reg_wr_en;
  logic hl_wr_en;
  logic [7:0] reg_wr_data;
  logic [15:0] hl_wr_data;
  logic [7:0] reg_a;
  logic [7:0] reg_b;
  logic [7:0] reg_c;

  reg_file u_reg_file (
      .i_clk(i_clk),
      .i_rst(i_rst),

      .i_reg_a_sel(reg_a_sel),
      .i_reg_b_sel(reg_b_sel),
      .i_reg_c_sel(reg_c_sel),
      .i_reg_wr_sel(reg_wr_sel),
      .i_reg_wr_en(reg_wr_en),
      .i_hl_wr_en(hl_wr_en),
      .i_reg_wr_data(reg_wr_data),
      .i_hl_wr_data(hl_wr_data),
      .o_reg_a(reg_a),
      .o_reg_b(reg_b),
      .o_reg_c(reg_c)
  );

  typedef enum logic [3:0] {
    FETCH,
    FETCH_IMM,
    LATCH_IMM,
    FETCH_IMM16_LO,
    FETCH_IMM16_HI,
    LATCH_IMM16,
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
  logic [15:0] imm16;
  logic [7:0] imm16_lo;
  logic [7:0] reg_c_val;

  logic is_ld_r_r;
  logic is_ld_r_imm;
  logic is_ld_r_hl;
  logic is_ld_hl_imm;
  logic is_ld_hl_r;
  logic is_ld_rr_a;
  logic is_ld_r_rr;
  logic is_ld_hl_a_inc_dec;
  logic is_ld_a_hl_inc_dec;
  logic is_ldh_imm_a;
  logic is_ldh_a_imm;
  logic is_ld_imm16_a;
  logic is_ld_a_imm16;

  function [2:0] get_m_cycles(logic [7:0] ir);
    case (ir)
      8'h06, 8'h16, 8'h26, 8'h0E, 8'h1E, 8'h2E, 8'h3E: get_m_cycles = 2;
      8'h36: get_m_cycles = 3;
      8'h46, 8'h4E, 8'h56, 8'h5E, 8'h66, 8'h6E, 8'h7E: get_m_cycles = 2;
      8'h70, 8'h71, 8'h72, 8'h73, 8'h74, 8'h75, 8'h77: get_m_cycles = 2;
      8'h02, 8'h12, 8'h0A, 8'h1A: get_m_cycles = 2;
      8'h22, 8'h32, 8'h2A, 8'h3A: get_m_cycles = 2;
      8'hE0, 8'hF0: get_m_cycles = 3;
      8'hEA, 8'hFA: get_m_cycles = 4;

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
          FETCH:          next_state = DECODE;
          DECODE: begin
            if (is_ld_a_imm16 || is_ld_imm16_a) next_state = FETCH_IMM16_LO;
            else if (is_ld_r_imm) next_state = FETCH_IMM;
            else if (is_ld_hl_imm) next_state = FETCH_IMM;
            else if (is_ldh_imm_a) next_state = FETCH_IMM;
            else if (is_ldh_a_imm) next_state = FETCH_IMM;
            else next_state = EXECUTE;
          end
          FETCH_IMM:      next_state = LATCH_IMM;
          LATCH_IMM:      next_state = EXECUTE;
          FETCH_IMM16_LO: next_state = FETCH_IMM16_HI;
          FETCH_IMM16_HI: next_state = LATCH_IMM16;
          LATCH_IMM16:    next_state = EXECUTE;
          EXECUTE:        next_state = WRITEBACK;
          WRITEBACK:      next_state = FETCH;

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
            is_ld_hl_imm <= 1'b0;
            is_ld_r_hl <= 1'b0;
            is_ld_hl_r <= 1'b0;
            is_ld_rr_a <= 1'b0;
            is_ld_r_rr <= 1'b0;
            is_ld_hl_a_inc_dec <= 1'b0;
            is_ld_a_hl_inc_dec <= 1'b0;
            is_ldh_imm_a <= 1'b0;
            is_ldh_a_imm <= 1'b0;
            is_ld_imm16_a <= 1'b0;
            is_ld_a_imm16 <= 1'b0;

            case (i_mem_rd_data)
              8'h41, 8'h42, 8'h43, 8'h44, 8'h45, 8'h47, 8'h48, 8'h4A, 8'h4B, 8'h4C, 8'h4D, 8'h4F, 
                8'h50, 8'h51, 8'h53, 8'h54, 8'h55, 8'h57, 8'h58, 8'h59, 8'h5A, 8'h5C, 8'h5D, 8'h5F,
                8'h60, 8'h61, 8'h62, 8'h63, 8'h65, 8'h67, 8'h68, 8'h69, 8'h6A, 8'h6B, 8'h6C, 8'h6F,
                8'h78, 8'h79, 8'h7A, 8'h7B, 8'h7C, 8'h7D:
              is_ld_r_r <= 1'b1;
              8'h06, 8'h16, 8'h26, 8'h0E, 8'h1E, 8'h2E, 8'h3E: is_ld_r_imm <= 1'b1;
              8'h46, 8'h56, 8'h66, 8'h4E, 8'h5E, 8'h6E, 8'h7E: is_ld_r_hl <= 1'b1;
              8'h36: is_ld_hl_imm <= 1'b1;
              8'h70, 8'h71, 8'h72, 8'h73, 8'h74, 8'h75, 8'h77: is_ld_hl_r <= 1'b1;
              8'h02, 8'h12: is_ld_rr_a <= 1'b1;
              8'h0A, 8'h1A: is_ld_r_rr <= 1'b1;
              8'h22, 8'h32: is_ld_hl_a_inc_dec <= 1'b1;
              8'h2A, 8'h3A: is_ld_a_hl_inc_dec <= 1'b1;
              8'hE0: is_ldh_imm_a <= 1'b1;
              8'hF0: is_ldh_a_imm <= 1'b1;
              8'hEA: is_ld_imm16_a <= 1'b1;
              8'hFA: is_ld_a_imm16 <= 1'b1;

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

        FETCH_IMM16_LO: begin
          if (m_cycle == 0 && t_state == 0) o_mem_rd_addr <= pc;
          else if (m_cycle == 0 && t_state == T_STATE_MAX) begin
            pc <= pc + 1;
            imm16_lo <= i_mem_rd_data;
          end
        end

        FETCH_IMM16_HI: begin
          if (m_cycle == 0 && t_state == 0) o_mem_rd_addr <= pc;
          else if (m_cycle == 0 && t_state == T_STATE_MAX) begin
            pc <= pc + 1;
          end
        end

        LATCH_IMM16: begin
          if (m_cycle == 0 && t_state == 0) imm16 <= {i_mem_rd_data, imm16_lo};
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
          end else if (is_ld_r_hl) begin
            if (m_cycle == 1 && t_state == 0) begin
              reg_a_sel <= 3'd4;
              reg_b_sel <= 3'd5;
              o_mem_rd_addr <= {reg_a, reg_b};

              case (ir)
                8'h46: reg_wr_sel <= 3'd0;
                8'h4E: reg_wr_sel <= 3'd1;
                8'h56: reg_wr_sel <= 3'd2;
                8'h5E: reg_wr_sel <= 3'd3;
                8'h66: reg_wr_sel <= 3'd4;
                8'h6E: reg_wr_sel <= 3'd5;
                8'h7E: reg_wr_sel <= 3'd7;

                default: reg_wr_sel <= 3'd0;
              endcase
            end
          end else if (is_ld_hl_imm) begin
            if (m_cycle == 1 && t_state == 0) begin
              reg_a_sel <= 3'd4;
              reg_b_sel <= 3'd5;
              o_mem_wr_addr <= {reg_a, reg_b};
            end
          end else if (is_ld_hl_r) begin
            if (m_cycle == 0 && t_state == 0) begin
              reg_a_sel <= 3'd4;
              reg_b_sel <= 3'd5;

              case (ir)
                8'h70:   reg_c_sel <= 3'd0;
                8'h71:   reg_c_sel <= 3'd1;
                8'h72:   reg_c_sel <= 3'd2;
                8'h73:   reg_c_sel <= 3'd3;
                8'h74:   reg_c_sel <= 3'd4;
                8'h75:   reg_c_sel <= 3'd5;
                8'h77:   reg_c_sel <= 3'd7;
                default: reg_c_sel <= 3'd0;
              endcase
            end

            if (m_cycle == 1 && t_state == 0) begin
              o_mem_wr_addr <= {reg_a, reg_b};
              reg_c_val <= reg_c;
            end
          end else if (is_ld_rr_a) begin
            case (ir)
              8'h02: begin
                if (m_cycle == 0 && t_state == 0) begin
                  reg_a_sel <= 3'd0;
                  reg_b_sel <= 3'd1;
                  reg_c_sel <= 3'd7;
                end
              end

              8'h12: begin
                if (m_cycle == 0 && t_state == 0) begin
                  reg_a_sel <= 3'd2;
                  reg_b_sel <= 3'd3;
                  reg_c_sel <= 3'd7;
                end
              end
              default: ;
            endcase

            if (m_cycle == 1 && t_state == 0) begin
              o_mem_wr_addr <= {reg_a, reg_b};
              reg_c_val <= reg_c;
            end
          end else if (is_ld_r_rr) begin
            if (m_cycle == 0 && t_state == 0) begin
              reg_wr_sel <= 3'd7;

              case (ir)
                8'h0A: begin
                  reg_a_sel <= 3'd0;
                  reg_b_sel <= 3'd1;
                end

                8'h1A: begin
                  reg_a_sel <= 3'd2;
                  reg_b_sel <= 3'd3;
                end

                default: ;
              endcase
            end

            if (m_cycle == 1 && t_state == 0) begin
              o_mem_rd_addr <= {reg_a, reg_b};
            end
          end else if (is_ld_hl_a_inc_dec) begin
            if (m_cycle == 0 && t_state == 0) begin
              case (ir)
                8'h22: begin
                  if (m_cycle == 0 && t_state == 0) begin
                    reg_a_sel <= 3'd4;
                    reg_b_sel <= 3'd5;
                    reg_c_sel <= 3'd7;
                  end
                end

                8'h32: begin
                  if (m_cycle == 0 && t_state == 0) begin
                    reg_a_sel <= 3'd4;
                    reg_b_sel <= 3'd5;
                    reg_c_sel <= 3'd7;
                  end
                end

                default: ;
              endcase
            end

            if (m_cycle == 1 && t_state == 0) begin
              o_mem_wr_addr <= {reg_a, reg_b};
              reg_c_val <= reg_c;
            end
          end else if (is_ld_a_hl_inc_dec) begin
            if (m_cycle == 0 && t_state == 0) begin
              reg_wr_sel <= 3'd7;
              reg_a_sel  <= 3'd4;
              reg_b_sel  <= 3'd5;
            end

            if (m_cycle == 1 && t_state == 0) begin
              o_mem_rd_addr <= {reg_a, reg_b};
            end
          end else if (is_ldh_imm_a) begin
            if (m_cycle == 2 && t_state == 0) begin
              reg_a_sel <= 3'd7;
              o_mem_wr_addr <= 16'hFF00 + {8'b0, imm};
            end
          end else if (is_ldh_a_imm) begin
            if (m_cycle == 2 && t_state == 0) begin
              reg_wr_sel <= 3'd7;
              o_mem_rd_addr <= 16'hFF00 + {8'b0, imm};
            end
          end else if (is_ld_imm16_a) begin
            if (m_cycle == 3 && t_state == 0) begin
              o_mem_wr_addr <= imm16;
              reg_a_sel <= 3'd7;
            end
          end else if (is_ld_a_imm16) begin
            if (m_cycle == 3 && t_state == 0) begin
              reg_wr_sel <= 3'd7;
              o_mem_rd_addr <= imm16;
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
          end else if (is_ld_r_hl) begin
            reg_wr_en   <= 1'b1;
            reg_wr_data <= i_mem_rd_data;
          end else if (is_ld_hl_imm) begin
            o_mem_wr_en   <= 1'b1;
            o_mem_wr_data <= imm;
          end else if (is_ld_hl_r) begin
            o_mem_wr_data <= reg_c_val;
            o_mem_wr_en   <= 1'b1;
          end else if (is_ld_rr_a) begin
            o_mem_wr_data <= reg_c_val;
            o_mem_wr_en   <= 1'b1;
          end else if (is_ld_r_rr) begin
            reg_wr_data <= i_mem_rd_data;
            reg_wr_en   <= 1'b1;
          end else if (is_ld_hl_a_inc_dec) begin
            if (m_cycle == 1 && t_state == 0) begin
              o_mem_wr_data <= reg_c_val;
              o_mem_wr_en   <= 1'b1;

              if (ir == 8'h22) begin
                hl_wr_en   <= 1'b1;
                hl_wr_data <= {reg_a, reg_b} + 16'd1;
              end else if (ir == 8'h32) begin
                hl_wr_en   <= 1'b1;
                hl_wr_data <= {reg_a, reg_b} - 16'd1;
              end
            end
          end else if (is_ld_a_hl_inc_dec) begin
            if (m_cycle == 1 && t_state == 0) begin
              reg_wr_en   <= 1'b1;
              reg_wr_data <= i_mem_rd_data;

              hl_wr_en    <= 1'b1;
              if (ir == 8'h2A) hl_wr_data <= {reg_a, reg_b} + 16'd1;
              else if (ir == 8'h3A) hl_wr_data <= {reg_a, reg_b} - 16'd1;
            end
          end else if (is_ldh_imm_a) begin
            if (m_cycle == 2 && t_state == 0) begin
              o_mem_wr_data <= reg_a;
              o_mem_wr_en   <= 1'b1;
            end
          end else if (is_ldh_a_imm) begin
            if (m_cycle == 2 && t_state == 0) begin
              reg_wr_en   <= 1'b1;
              reg_wr_data <= i_mem_rd_data;
            end
          end else if (is_ld_imm16_a) begin
            if (m_cycle == 3 && t_state == 0) begin
              o_mem_wr_data <= reg_a;
              o_mem_wr_en   <= 1'b1;
            end
          end else if (is_ld_a_imm16) begin
            if (m_cycle == 3 && t_state == 0) begin
              reg_wr_data <= i_mem_rd_data;
              reg_wr_en   <= 1'b1;
            end
          end
        end

        default: ;
      endcase
    end
  end
endmodule
