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
  logic bc_wr_en;
  logic de_wr_en;
  logic hl_wr_en;
  logic [7:0] reg_wr_data;
  logic [15:0] bc_wr_data;
  logic [15:0] de_wr_data;
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
      .i_bc_wr_en(bc_wr_en),
      .i_de_wr_en(de_wr_en),
      .i_hl_wr_en(hl_wr_en),
      .i_reg_wr_data(reg_wr_data),
      .i_bc_wr_data(bc_wr_data),
      .i_de_wr_data(de_wr_data),
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
  logic [7:0] f;
  logic [15:0] temp_hl;
  logic [15:0] temp;
  logic [7:0] prev_mem_rd_data;
  logic stopped;
  logic [7:0] IE;
  logic [7:0] IF;
  logic IME, IME_next;
  logic ei_pending, ei_pending_next;
  logic halted, halted_next;

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
  logic is_ldh_c_a;
  logic is_ldh_a_c;
  logic is_ld_rr_imm;
  logic is_inc_rr;
  logic is_dec_rr;
  logic is_inc_r;
  logic is_dec_r;
  logic is_inc_hl_mem;
  logic is_dec_hl_mem;
  logic is_ld_a16_sp;
  logic is_add_hl_rr;
  logic is_ld_sp_hl;
  logic is_ld_hl_sp_e8;
  logic is_add_sp_e8;
  logic is_add_r_r;
  logic is_adc_r_r;
  logic is_sub_r_r;
  logic is_sbc_r_r;
  logic is_and_a_r;
  logic is_xor_a_r;
  logic is_or_a_r;
  logic is_cp_a_r;
  logic is_add_a_hl_mem;
  logic is_sub_a_hl_mem;
  logic is_and_a_hl_mem;
  logic is_or_a_hl_mem;
  logic is_adc_a_hl_mem;
  logic is_sbc_a_hl_mem;
  logic is_xor_a_hl_mem;
  logic is_cp_a_hl_mem;
  logic is_add_a_n8;
  logic is_sub_a_n8;
  logic is_and_a_n8;
  logic is_or_a_n8;
  logic is_adc_a_n8;
  logic is_sbc_a_n8;
  logic is_xor_a_n8;
  logic is_cp_a_n8;
  logic is_jr_nz_e8;
  logic is_jr_nc_e8;
  logic is_jr_e8;
  logic is_jr_z_e8;
  logic is_jr_c_e8;
  logic is_jp_nz_a16;
  logic is_jp_nc_a16;
  logic is_jp_a16;
  logic is_jp_hl;
  logic is_jp_z_a16;
  logic is_jp_c_a16;
  logic is_scf;
  logic is_ccf;
  logic is_cpl;
  logic is_daa;
  logic is_rlca;
  logic is_rla;
  logic is_rrca;
  logic is_rra;
  logic is_stop_n8;
  logic is_halt;
  logic is_ei;
  logic is_di;
  logic is_prefix;
  logic is_pop_bc;
  logic is_pop_de;
  logic is_pop_hl;
  logic is_pop_af;

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
      8'hE2, 8'hF2: get_m_cycles = 2;
      8'h01, 8'h11, 8'h21, 8'h31: get_m_cycles = 3;
      8'h03, 8'h13, 8'h23, 8'h33: get_m_cycles = 2;
      8'h0B, 8'h1B, 8'h2B, 8'h3B: get_m_cycles = 2;
      8'h04, 8'h14, 8'h24: get_m_cycles = 1;
      8'h05, 8'h15, 8'h25: get_m_cycles = 1;
      8'h34, 8'h35: get_m_cycles = 3;
      8'h08: get_m_cycles = 5;
      8'h09, 8'h19, 8'h29, 8'h39: get_m_cycles = 2;
      8'hF9: get_m_cycles = 2;
      8'hF8: get_m_cycles = 3;
      8'hE8: get_m_cycles = 4;
      8'h86, 8'h96, 8'hA6, 8'hB6, 8'hC6, 8'hD6, 8'hE6, 8'hF6: get_m_cycles = 2;
      8'h8E, 8'h9E, 8'hAE, 8'hBE, 8'hCE, 8'hDE, 8'hEE, 8'hFE: get_m_cycles = 2;
      8'h20: get_m_cycles = 2;
      8'h30: get_m_cycles = 2;
      8'h18: get_m_cycles = 3;
      8'h28: get_m_cycles = 2;
      8'h38: get_m_cycles = 2;
      8'hC2, 8'hD2, 8'hCA, 8'hDA: get_m_cycles = 3;
      8'hC3: get_m_cycles = 4;
      8'hC1, 8'hD1, 8'hE1, 8'hF1: get_m_cycles = 3;

      default: get_m_cycles = 1;
    endcase
  endfunction

  function [2:0] get_t_states();
    get_t_states = 4;
  endfunction

  // State logic
  always_comb begin
    next_state      = state;
    next_m_cycle    = m_cycle;
    next_t_state    = t_state;
    IME_next        = IME;
    ei_pending_next = ei_pending;
    halted_next     = halted;

    if (t_state < T_STATE_MAX) begin
      next_t_state = t_state + 1;
    end else begin
      next_t_state = 0;

      if (m_cycle < M_CYCLE_MAX) begin
        next_m_cycle = m_cycle + 1;
      end else begin
        next_m_cycle = 0;

        case (state)
          FETCH: begin
            if (ei_pending) begin
              IME_next        = 1'b1;
              ei_pending_next = 1'b0;
            end

            if (stopped || halted) begin
              if ((IE & IF) != 0) begin
                halted_next = 1'b0;
              end
              // TODO resume from stop
              next_state = FETCH;
            end else begin
              next_state = DECODE;
            end
          end

          DECODE: begin
            if (is_ld_a_imm16 || is_ld_imm16_a || is_ld_rr_imm) next_state = FETCH_IMM16_LO;
            else if (is_ld_r_imm || is_ld_hl_sp_e8 || is_add_sp_e8 ||
                   is_add_a_n8 || is_sub_a_n8 || is_and_a_n8 ||
                   is_or_a_n8 || is_adc_a_n8 || is_sbc_a_n8 ||
                   is_xor_a_n8 || is_cp_a_n8)
              next_state = FETCH_IMM;
            else if (is_ld_hl_imm || is_jr_nz_e8 || is_jr_nc_e8 ||
                   is_jr_e8 || is_jr_z_e8 || is_jr_c_e8 || is_stop_n8)
              next_state = FETCH_IMM;
            else if (is_ldh_imm_a) next_state = FETCH_IMM;
            else if (is_ldh_a_imm) next_state = FETCH_IMM;
            else if (is_ld_a16_sp) next_state = FETCH_IMM16_LO;
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

        if (is_ei) ei_pending_next = 1'b1;
        if (is_di) IME_next = 1'b0;
        if (is_halt) halted_next = 1'b1;
      end
    end
  end

  // State machine
  always_ff @(posedge i_clk, posedge i_rst) begin
    if (i_rst) begin
      o_mem_wr_en   <= 1'b0;
      o_mem_rd_addr <= 16'h0;
      o_mem_wr_addr <= 16'h0;
      o_mem_wr_data <= 8'h0;
      IME           <= 1'b0;
      ei_pending    <= 1'b0;
      halted        <= 1'b0;
      pc            <= 16'h0100;
      sp            <= 16'hFFFE;

      state         <= FETCH;
      m_cycle       <= 0;
      t_state       <= 0;
      M_CYCLE_MAX   <= 0;
      T_STATE_MAX   <= 0;
    end else begin
      o_mem_wr_en   <= 1'b0;
      o_mem_wr_data <= 8'h0;
      reg_wr_en     <= 1'b0;
      bc_wr_en      <= 1'b0;
      de_wr_en      <= 1'b0;
      hl_wr_en      <= 1'b0;
      state         <= next_state;
      m_cycle       <= next_m_cycle;
      t_state       <= next_t_state;
      IME           <= IME_next;
      ei_pending    <= ei_pending_next;
      halted        <= halted_next;

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
            is_ldh_c_a <= 1'b0;
            is_ldh_a_c <= 1'b0;
            is_ld_rr_imm <= 1'b0;
            is_inc_rr <= 1'b0;
            is_dec_rr <= 1'b0;
            is_inc_r <= 1'b0;
            is_dec_r <= 1'b0;
            is_dec_hl_mem <= 1'b0;
            is_inc_hl_mem <= 1'b0;
            is_ld_a16_sp <= 1'b0;
            is_add_hl_rr <= 1'b0;
            is_ld_sp_hl <= 1'b0;
            is_ld_hl_sp_e8 <= 1'b0;
            is_add_sp_e8 <= 1'b0;
            is_add_r_r <= 1'b0;
            is_adc_r_r <= 1'b0;
            is_sub_r_r <= 1'b0;
            is_sbc_r_r <= 1'b0;
            is_and_a_r <= 1'b0;
            is_or_a_r <= 1'b0;
            is_add_a_hl_mem <= 1'b0;
            is_and_a_hl_mem <= 1'b0;
            is_or_a_hl_mem <= 1'b0;
            is_adc_a_hl_mem <= 1'b0;
            is_sbc_a_hl_mem <= 1'b0;
            is_xor_a_hl_mem <= 1'b0;
            is_cp_a_hl_mem <= 1'b0;
            is_add_a_n8 <= 1'b0;
            is_sub_a_n8 <= 1'b0;
            is_and_a_n8 <= 1'b0;
            is_or_a_n8 <= 1'b0;
            is_adc_a_n8 <= 1'b0;
            is_sbc_a_n8 <= 1'b0;
            is_xor_a_n8 <= 1'b0;
            is_cp_a_n8 <= 1'b0;
            is_jr_nz_e8 <= 1'b0;
            is_jr_nc_e8 <= 1'b0;
            is_jr_e8 <= 1'b0;
            is_jr_z_e8 <= 1'b0;
            is_jr_c_e8 <= 1'b0;
            is_scf <= 1'b0;
            is_ccf <= 1'b0;
            is_cpl <= 1'b0;
            is_daa <= 1'b0;
            is_rlca <= 1'b0;
            is_rla <= 1'b0;
            is_rrca <= 1'b0;
            is_rra <= 1'b0;
            is_stop_n8 <= 1'b0;
            is_halt <= 1'b0;
            is_ei <= 1'b0;
            is_di <= 1'b0;
            is_prefix <= 1'b0;
            is_jp_nz_a16 <= 1'b0;
            is_jp_nc_a16 <= 1'b0;
            is_jp_a16    <= 1'b0;
            is_jp_hl <= 1'b0;
            is_jp_z_a16 <= 1'b1;
            is_jp_c_a16 <= 1'b1;
            is_pop_bc <= 1'b1;
            is_pop_de <= 1'b1;
            is_pop_hl <= 1'b1;
            is_pop_af <= 1'b1;

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
              8'hE2: is_ldh_c_a <= 1'b1;
              8'hF2: is_ldh_a_c <= 1'b1;
              8'h01, 8'h11, 8'h21, 8'h31: is_ld_rr_imm <= 1'b1;
              8'h03, 8'h13, 8'h23, 8'h33: is_inc_rr <= 1'b1;
              8'h0B, 8'h1B, 8'h2B, 8'h3B: is_dec_rr <= 1'b1;
              8'h04, 8'h14, 8'h24, 8'h0C, 8'h1C, 8'h2C, 8'h3C: is_inc_r <= 1'b1;
              8'h05, 8'h15, 8'h25, 8'h0D, 8'h1D, 8'h2D, 8'h3D: is_dec_r <= 1'b1;
              8'h34: is_inc_hl_mem <= 1'b1;
              8'h35: is_dec_hl_mem <= 1'b1;
              8'h08: is_ld_a16_sp <= 1'b1;
              8'h09, 8'h19, 8'h29, 8'h39: is_add_hl_rr <= 1'b1;
              8'hF8: is_ld_hl_sp_e8 <= 1'b1;
              8'hF9: is_ld_sp_hl <= 1'b1;
              8'hE8: is_add_sp_e8 <= 1'b1;
              8'h80, 8'h81, 8'h82, 8'h83, 8'h84, 8'h85, 8'h87: is_add_r_r <= 1'b1;
              8'h88, 8'h89, 8'h8A, 8'h8B, 8'h8C, 8'h8D, 8'h8F: is_adc_r_r <= 1'b1;
              8'h90, 8'h91, 8'h92, 8'h93, 8'h94, 8'h95, 8'h97: is_sub_r_r <= 1'b1;
              8'h98, 8'h99, 8'h9A, 8'h9B, 8'h9C, 8'h9D, 8'h9F: is_sbc_r_r <= 1'b1;
              8'hA0, 8'hA1, 8'hA2, 8'hA3, 8'hA4, 8'hA5, 8'hA7: is_and_a_r <= 1'b1;
              8'hA8, 8'hA9, 8'hAA, 8'hAB, 8'hAC, 8'hAD, 8'hAF: is_xor_a_r <= 1'b1;
              8'hB0, 8'hB1, 8'hB2, 8'hB3, 8'hB4, 8'hB5, 8'hB7: is_or_a_r <= 1'b1;
              8'hB8, 8'hB9, 8'hBA, 8'hBB, 8'hBC, 8'hBD, 8'hBF: is_cp_a_r <= 1'b1;
              8'h86: is_add_a_hl_mem <= 1'b1;
              8'h96: is_sub_a_hl_mem <= 1'b1;
              8'hA6: is_and_a_hl_mem <= 1'b1;
              8'hB6: is_or_a_hl_mem <= 1'b1;
              8'h8E: is_adc_a_hl_mem <= 1'b1;
              8'h9E: is_sbc_a_hl_mem <= 1'b1;
              8'hAE: is_xor_a_hl_mem <= 1'b1;
              8'hBE: is_cp_a_hl_mem <= 1'b1;
              8'hC6: is_add_a_n8 <= 1'b1;
              8'hD6: is_sub_a_n8 <= 1'b1;
              8'hE6: is_and_a_n8 <= 1'b1;
              8'hF6: is_or_a_n8 <= 1'b1;
              8'hCE: is_adc_a_n8 <= 1'b1;
              8'hDE: is_sbc_a_n8 <= 1'b1;
              8'hEE: is_xor_a_n8 <= 1'b1;
              8'hFE: is_cp_a_n8 <= 1'b1;
              8'h20: is_jr_nz_e8 <= 1'b1;
              8'h30: is_jr_nc_e8 <= 1'b1;
              8'h18: is_jr_e8 <= 1'b1;
              8'h28: is_jr_z_e8 <= 1'b1;
              8'h38: is_jr_c_e8 <= 1'b1;
              8'h37: is_scf <= 1'b1;
              8'h3F: is_ccf <= 1'b1;
              8'h2F: is_cpl <= 1'b1;
              8'h27: is_daa <= 1'b1;
              8'h07: is_rlca <= 1'b1;
              8'h17: is_rla <= 1'b1;
              8'h0F: is_rrca <= 1'b1;
              8'h1F: is_rra <= 1'b1;
              8'h10: is_stop_n8 <= 1'b1;
              8'h76: is_halt <= 1'b1;
              8'hFB: is_ei <= 1'b1;
              8'hF3: is_di <= 1'b1;
              8'hCB: is_prefix <= 1'b1;
              8'hC2: is_jp_nz_a16 <= 1'b1;
              8'hD2: is_jp_nc_a16 <= 1'b1;
              8'hC3: is_jp_a16 <= 1'b1;
              8'hE9: is_jp_hl <= 1'b1;
              8'hCA: is_jp_z_a16 <= 1'b1;
              8'hDA: is_jp_c_a16 <= 1'b1;
              8'hC1: is_pop_bc <= 1'b1;
              8'hD1: is_pop_de <= 1'b1;
              8'hE1: is_pop_hl <= 1'b1;
              8'hF1: is_pop_af <= 1'b1;

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
          end else if (is_ldh_c_a) begin
            if (m_cycle == 0 && t_state == 0) begin
              reg_b_sel <= 3'd1;
            end

            if (m_cycle == 1 && t_state == 0) begin
              reg_a_sel <= 3'd7;
              o_mem_wr_addr <= 16'hFF00 + {8'b0, reg_b};
            end
          end else if (is_ldh_a_c) begin
            if (m_cycle == 0 && t_state == 0) begin
              reg_b_sel <= 3'd1;
            end

            if (m_cycle == 1 && t_state == 0) begin
              reg_wr_sel <= 3'd7;
              o_mem_rd_addr <= 16'hFF00 + {8'b0, reg_b};
            end
          end else if (is_ld_rr_imm) begin
            case (ir)
              8'h01: bc_wr_data <= imm16;
              8'h11: de_wr_data <= imm16;
              8'h21: hl_wr_data <= imm16;
              8'h31: sp <= imm16;

              default: ;
            endcase
          end else if (is_inc_rr) begin
            case (ir)
              8'h03: begin
                reg_a_sel  <= 3'd0;
                reg_b_sel  <= 3'd1;
                bc_wr_data <= {reg_a, reg_b} + 1;
              end
              8'h13: begin
                reg_a_sel  <= 3'd2;
                reg_b_sel  <= 3'd3;
                de_wr_data <= {reg_a, reg_b} + 1;
              end
              8'h23: begin
                reg_a_sel  <= 3'd4;
                reg_b_sel  <= 3'd5;
                hl_wr_data <= {reg_a, reg_b} + 1;
              end
              8'h33: begin
                // No setup needed
              end

              default: ;
            endcase
          end else if (is_dec_rr) begin
            case (ir)
              8'h0B: begin
                reg_a_sel  <= 3'd0;
                reg_b_sel  <= 3'd1;
                bc_wr_data <= {reg_a, reg_b} - 1;
              end
              8'h1B: begin
                reg_a_sel  <= 3'd2;
                reg_b_sel  <= 3'd3;
                de_wr_data <= {reg_a, reg_b} - 1;
              end
              8'h2B: begin
                reg_a_sel  <= 3'd4;
                reg_b_sel  <= 3'd5;
                hl_wr_data <= {reg_a, reg_b} - 1;
              end
              8'h3B: begin
                // No setup needed
              end

              default: ;
            endcase
          end else if (is_inc_r) begin
            case (ir)
              8'h04: begin
                reg_wr_sel  <= 3'd0;
                reg_a_sel   <= 3'd0;
                reg_wr_data <= reg_a + 1;
              end
              8'h0C: begin
                reg_wr_sel  <= 3'd1;
                reg_a_sel   <= 3'd1;
                reg_wr_data <= reg_a + 1;
              end
              8'h14: begin
                reg_wr_sel  <= 3'd2;
                reg_a_sel   <= 3'd2;
                reg_wr_data <= reg_a + 1;
              end
              8'h1C: begin
                reg_wr_sel  <= 3'd3;
                reg_a_sel   <= 3'd3;
                reg_wr_data <= reg_a + 1;
              end
              8'h24: begin
                reg_wr_sel  <= 3'd4;
                reg_a_sel   <= 3'd4;
                reg_wr_data <= reg_a + 1;
              end
              8'h2C: begin
                reg_wr_sel  <= 3'd5;
                reg_a_sel   <= 3'd5;
                reg_wr_data <= reg_a + 1;
              end
              8'h3C: begin
                reg_wr_sel  <= 3'd7;
                reg_a_sel   <= 3'd7;
                reg_wr_data <= reg_a + 1;
              end

              default: ;
            endcase
          end else if (is_dec_r) begin
            case (ir)
              8'h05: begin
                reg_wr_sel  <= 3'd0;
                reg_a_sel   <= 3'd0;
                reg_wr_data <= reg_a - 1;
              end
              8'h0D: begin
                reg_wr_sel  <= 3'd1;
                reg_a_sel   <= 3'd1;
                reg_wr_data <= reg_a - 1;
              end
              8'h15: begin
                reg_wr_sel  <= 3'd2;
                reg_a_sel   <= 3'd2;
                reg_wr_data <= reg_a - 1;
              end
              8'h1D: begin
                reg_wr_sel  <= 3'd3;
                reg_a_sel   <= 3'd3;
                reg_wr_data <= reg_a - 1;
              end
              8'h25: begin
                reg_wr_sel  <= 3'd4;
                reg_a_sel   <= 3'd4;
                reg_wr_data <= reg_a - 1;
              end
              8'h2D: begin
                reg_wr_sel  <= 3'd5;
                reg_a_sel   <= 3'd5;
                reg_wr_data <= reg_a - 1;
              end
              8'h3D: begin
                reg_wr_sel  <= 3'd7;
                reg_a_sel   <= 3'd7;
                reg_wr_data <= reg_a - 1;
              end

              default: ;
            endcase
          end else if (is_inc_hl_mem) begin
            if (m_cycle == 2 && t_state == 0) begin
              reg_a_sel <= 3'd4;
              reg_b_sel <= 3'd5;
              o_mem_rd_addr <= {reg_a, reg_b};
            end
          end else if (is_dec_hl_mem) begin
            if (m_cycle == 2 && t_state == 0) begin
              reg_a_sel <= 3'd4;
              reg_b_sel <= 3'd5;
              o_mem_rd_addr <= {reg_a, reg_b};
            end
          end else if (is_add_hl_rr) begin
            if (m_cycle == 0 && t_state == 0) begin
              reg_a_sel <= 3'd4;
              reg_b_sel <= 3'd5;
              temp_hl   <= {reg_a, reg_b};
            end

            if (m_cycle == 1 && t_state == 0) begin
              case (ir)
                8'h09: begin
                  reg_a_sel <= 3'd0;
                  reg_b_sel <= 3'd1;
                  temp <= {reg_a, reg_b};
                end
                8'h19: begin
                  reg_a_sel <= 3'd2;
                  reg_b_sel <= 3'd3;
                  temp <= {reg_a, reg_b};
                end
                8'h29: begin
                  reg_a_sel <= 3'd4;
                  reg_b_sel <= 3'd5;
                  temp <= {reg_a, reg_b};
                end
                8'h39: begin
                  temp <= sp;
                end

                default: ;
              endcase
            end
          end else if (is_ld_hl_sp_e8) begin
            temp_hl <= sp + {{8{imm[7]}}, imm};
          end else if (is_add_r_r) begin
            reg_a_sel  <= 3'd7;
            reg_wr_sel <= 3'd7;

            case (ir)
              8'h80: reg_b_sel <= 3'd0;
              8'h81: reg_b_sel <= 3'd1;
              8'h82: reg_b_sel <= 3'd2;
              8'h83: reg_b_sel <= 3'd3;
              8'h84: reg_b_sel <= 3'd4;
              8'h85: reg_b_sel <= 3'd5;
              8'h87: reg_b_sel <= 3'd7;

              default: ;
            endcase
          end else if (is_adc_r_r) begin
            reg_a_sel  <= 3'd7;
            reg_wr_sel <= 3'd7;

            case (ir)
              8'h88: reg_b_sel <= 3'd0;
              8'h89: reg_b_sel <= 3'd1;
              8'h8A: reg_b_sel <= 3'd2;
              8'h8B: reg_b_sel <= 3'd3;
              8'h8C: reg_b_sel <= 3'd4;
              8'h8D: reg_b_sel <= 3'd5;
              8'h8F: reg_b_sel <= 3'd7;

              default: ;
            endcase
          end else if (is_sub_r_r) begin
            reg_a_sel  <= 3'd7;
            reg_wr_sel <= 3'd7;

            case (ir)
              8'h90: reg_b_sel <= 3'd0;
              8'h91: reg_b_sel <= 3'd1;
              8'h92: reg_b_sel <= 3'd2;
              8'h93: reg_b_sel <= 3'd3;
              8'h94: reg_b_sel <= 3'd4;
              8'h95: reg_b_sel <= 3'd5;
              8'h97: reg_b_sel <= 3'd7;

              default: ;
            endcase
          end else if (is_sbc_r_r) begin
            reg_a_sel  <= 3'd7;
            reg_wr_sel <= 3'd7;

            case (ir)
              8'h98: reg_b_sel <= 3'd0;
              8'h99: reg_b_sel <= 3'd1;
              8'h9A: reg_b_sel <= 3'd2;
              8'h9B: reg_b_sel <= 3'd3;
              8'h9C: reg_b_sel <= 3'd4;
              8'h9D: reg_b_sel <= 3'd5;
              8'h9F: reg_b_sel <= 3'd7;

              default: ;
            endcase
          end else if (is_and_a_r) begin
            reg_a_sel  <= 3'd7;
            reg_wr_sel <= 3'd7;

            case (ir)
              8'hA0: reg_b_sel <= 3'd0;
              8'hA1: reg_b_sel <= 3'd1;
              8'hA2: reg_b_sel <= 3'd2;
              8'hA3: reg_b_sel <= 3'd3;
              8'hA4: reg_b_sel <= 3'd4;
              8'hA5: reg_b_sel <= 3'd5;
              8'hA7: reg_b_sel <= 3'd7;

              default: ;
            endcase
          end else if (is_xor_a_r) begin
            reg_a_sel  <= 3'd7;
            reg_wr_sel <= 3'd7;

            case (ir)
              8'hA8: reg_b_sel <= 3'd0;
              8'hA9: reg_b_sel <= 3'd1;
              8'hAA: reg_b_sel <= 3'd2;
              8'hAB: reg_b_sel <= 3'd3;
              8'hAC: reg_b_sel <= 3'd4;
              8'hAD: reg_b_sel <= 3'd5;
              8'hAF: reg_b_sel <= 3'd7;

              default: ;
            endcase
          end else if (is_or_a_r) begin
            reg_a_sel  <= 3'd7;
            reg_wr_sel <= 3'd7;

            case (ir)
              8'hB0: reg_b_sel <= 3'd0;
              8'hB1: reg_b_sel <= 3'd1;
              8'hB2: reg_b_sel <= 3'd2;
              8'hB3: reg_b_sel <= 3'd3;
              8'hB4: reg_b_sel <= 3'd4;
              8'hB5: reg_b_sel <= 3'd5;
              8'hB7: reg_b_sel <= 3'd7;

              default: ;
            endcase
          end else if (is_cp_a_r) begin
            reg_a_sel  <= 3'd7;
            reg_wr_sel <= 3'd7;

            case (ir)
              8'hB8: reg_b_sel <= 3'd0;
              8'hB9: reg_b_sel <= 3'd1;
              8'hBA: reg_b_sel <= 3'd2;
              8'hBB: reg_b_sel <= 3'd3;
              8'hBC: reg_b_sel <= 3'd4;
              8'hBD: reg_b_sel <= 3'd5;
              8'hBF: reg_b_sel <= 3'd7;

              default: ;
            endcase
          end else if (is_add_a_hl_mem) begin
            if (m_cycle == 1 && t_state == 0) begin
              reg_a_sel <= 3'd4;
              reg_b_sel <= 3'd5;
              reg_c_sel <= 3'd7;
              o_mem_rd_addr <= {reg_a, reg_b};
            end
          end else if (is_sub_a_hl_mem) begin
            if (m_cycle == 1 && t_state == 0) begin
              reg_a_sel <= 3'd4;
              reg_b_sel <= 3'd5;
              reg_c_sel <= 3'd7;
              o_mem_rd_addr <= {reg_a, reg_b};
            end
          end else if (is_and_a_hl_mem) begin
            if (m_cycle == 1 && t_state == 0) begin
              reg_a_sel <= 3'd4;
              reg_b_sel <= 3'd5;
              reg_c_sel <= 3'd7;
              o_mem_rd_addr <= {reg_a, reg_b};
            end
          end else if (is_or_a_hl_mem) begin
            if (m_cycle == 1 && t_state == 0) begin
              reg_a_sel <= 3'd4;
              reg_b_sel <= 3'd5;
              reg_c_sel <= 3'd7;
              o_mem_rd_addr <= {reg_a, reg_b};
            end
          end else if (is_adc_a_hl_mem) begin
            if (m_cycle == 1 && t_state == 0) begin
              reg_a_sel <= 3'd4;
              reg_b_sel <= 3'd5;
              reg_c_sel <= 3'd7;
              o_mem_rd_addr <= {reg_a, reg_b};
            end
          end else if (is_sbc_a_hl_mem) begin
            if (m_cycle == 1 && t_state == 0) begin
              reg_a_sel <= 3'd4;
              reg_b_sel <= 3'd5;
              reg_c_sel <= 3'd7;
              o_mem_rd_addr <= {reg_a, reg_b};
            end
          end else if (is_xor_a_hl_mem) begin
            if (m_cycle == 1 && t_state == 0) begin
              reg_a_sel <= 3'd4;
              reg_b_sel <= 3'd5;
              reg_c_sel <= 3'd7;
              o_mem_rd_addr <= {reg_a, reg_b};
            end
          end else if (is_cp_a_hl_mem) begin
            if (m_cycle == 1 && t_state == 0) begin
              reg_a_sel <= 3'd4;
              reg_b_sel <= 3'd5;
              reg_c_sel <= 3'd7;
              o_mem_rd_addr <= {reg_a, reg_b};
            end
          end else if (is_add_a_n8) begin
            if (m_cycle == 1 && t_state == 0) begin
              reg_a_sel <= 3'd7;
            end
          end else if (is_sub_a_n8) begin
            if (m_cycle == 1 && t_state == 0) begin
              reg_a_sel <= 3'd7;
            end
          end else if (is_and_a_n8) begin
            if (m_cycle == 1 && t_state == 0) begin
              reg_a_sel <= 3'd7;
            end
          end else if (is_or_a_n8) begin
            if (m_cycle == 1 && t_state == 0) begin
              reg_a_sel <= 3'd7;
            end
          end else if (is_adc_a_n8) begin
            if (m_cycle == 1 && t_state == 0) begin
              reg_a_sel <= 3'd7;
            end
          end else if (is_sbc_a_n8) begin
            if (m_cycle == 1 && t_state == 0) begin
              reg_a_sel <= 3'd7;
            end
          end else if (is_xor_a_n8) begin
            if (m_cycle == 1 && t_state == 0) begin
              reg_a_sel <= 3'd7;
            end
          end else if (is_cp_a_n8) begin
            if (m_cycle == 1 && t_state == 0) begin
              reg_a_sel <= 3'd7;
            end
          end else if (is_jr_nz_e8) begin
            if (m_cycle == 0 && t_state == 0) begin
              if (!f[7]) begin
                M_CYCLE_MAX <= 2;
                pc <= pc + {{8{imm[7]}}, imm};
              end else begin
                M_CYCLE_MAX <= 1;
              end
            end
          end else if (is_jr_nc_e8) begin
            if (m_cycle == 0 && t_state == 0) begin
              if (!f[4]) begin
                pc <= pc + {{8{imm[7]}}, imm};
                M_CYCLE_MAX <= 2;
              end else begin
                M_CYCLE_MAX <= 1;
              end
            end
          end else if (is_scf) begin
            if (m_cycle == 0 && t_state == 0) begin
              f[7]   = f[7];
              f[6]   = 1'b0;
              f[5]   = 1'b0;
              f[4]   = 1'b1;
              f[3:0] = 4'b0000;
            end
          end else if (is_ccf) begin
            if (m_cycle == 0 && t_state == 0) begin
              f[7]   = f[7];
              f[6]   = 1'b0;
              f[5]   = 1'b0;
              f[4]   = ~f[4];
              f[3:0] = 4'b0000;
            end
          end else if (is_cpl) begin
            if (m_cycle == 0 && t_state == 0) begin
              reg_wr_sel  <= 3'd7;
              reg_wr_data <= ~reg_a;
              reg_wr_en   <= 1'b1;

              f[7]   = f[7];
              f[6]   = 1'b1;
              f[5]   = 1'b1;
              f[4]   = f[4];
              f[3:0] = 4'b0000;
            end
          end else if (is_daa) begin
            if (m_cycle == 0 && t_state == 0) begin
              reg [7:0] a_new;
              reg c_new;

              a_new = reg_a;
              c_new = f[4];

              if (!f[6]) begin
                if (f[5] || ((reg_a & 8'h0F) > 8'h09)) a_new = a_new + 8'h06;
                if (f[4] || (reg_a > 8'h99)) begin
                  a_new = a_new + 8'h60;
                  c_new = 1'b1;
                end
              end else begin
                if (f[5]) a_new = a_new - 8'h06;
                if (f[4]) begin
                  a_new = a_new - 8'h60;
                  c_new = 1'b1;
                end
              end

              reg_wr_sel  <= 3'd7;
              reg_wr_data <= a_new[7:0];
              reg_wr_en   <= 1'b1;
              f[7]   = (a_new[7:0] == 8'h00);
              f[5]   = 1'b0;
              f[4]   = c_new;
              f[3:0] = 4'b0000;
            end
          end else if (is_rlca) begin
            if (m_cycle == 0 && t_state == 0) begin
              reg_wr_sel  <= 3'd7;
              reg_wr_data <= {reg_a[6:0], reg_a[7]};
              reg_wr_en   <= 1'b1;
              f[7]   = 1'b0;
              f[6]   = 1'b0;
              f[5]   = 1'b0;
              f[4]   = reg_a[7];
              f[3:0] = 4'b0000;
            end
          end else if (is_rla) begin
            if (m_cycle == 0 && t_state == 0) begin
              reg_wr_sel  <= 3'd7;
              reg_wr_en   <= 1'b1;
              reg_wr_data <= {reg_a[6:0], f[4]};
              f[7]   = 1'b0;
              f[6]   = 1'b0;
              f[5]   = 1'b0;
              f[4]   = reg_a[7];
              f[3:0] = 4'b0000;
            end
          end else if (is_rrca) begin
            if (m_cycle == 0 && t_state == 0) begin
              reg_wr_sel  <= 3'd7;
              reg_wr_en   <= 1'b1;
              reg_wr_data <= {reg_a[0], reg_a[7:1]};
              f[7]   = 1'b0;
              f[6]   = 1'b0;
              f[5]   = 1'b0;
              f[4]   = reg_a[0];
              f[3:0] = 4'b0000;
            end
          end else if (is_rra) begin
            if (m_cycle == 0 && t_state == 0) begin
              reg_wr_sel  <= 3'd7;
              reg_wr_en   <= 1'b1;
              reg_wr_data <= {f[4], reg_a[7:1]};
              f[7]   = 1'b0;
              f[6]   = 1'b0;
              f[5]   = 1'b0;
              f[4]   = reg_a[0];
              f[3:0] = 4'b0000;
            end
          end else if (is_stop_n8) begin
            if (m_cycle == 0 && t_state == 0) begin
              pc <= pc + 2;
              stopped <= 1'b1;
            end
          end else if (is_halt) begin
            if (m_cycle == 0 && t_state == 0) begin
              halted <= 1'b1;
            end
          end else if (is_jr_e8) begin
            if (m_cycle == 2 && t_state == 0) begin
              pc <= pc + {{8{imm[7]}}, imm};
            end
          end else if (is_jr_z_e8) begin
            if (m_cycle == 0 && t_state == 0) begin
              if (f[7]) begin
                M_CYCLE_MAX <= 2;
                pc <= pc + {{8{imm[7]}}, imm};
              end else begin
                M_CYCLE_MAX <= 1;
              end
            end
          end else if (is_jr_c_e8) begin
            if (m_cycle == 0 && t_state == 0) begin
              if (f[4]) begin
                M_CYCLE_MAX <= 2;
                pc <= pc + {{8{imm[7]}}, imm};
              end else begin
                M_CYCLE_MAX <= 1;
              end
            end
          end else if (is_jp_nz_a16) begin
            if (!f[7]) begin
              pc <= imm16;
              M_CYCLE_MAX <= 3;
            end else begin
              M_CYCLE_MAX <= 2;
            end
          end else if (is_jp_nc_a16) begin
            if (!f[4]) begin
              pc <= imm16;
              M_CYCLE_MAX <= 3;
            end else begin
              M_CYCLE_MAX <= 2;
            end
          end else if (is_jp_a16) begin
            pc <= imm16;
          end else if (is_jp_hl) begin
            if (m_cycle == 0 && t_state == 0) begin
              reg_a_sel <= 3'd4;
              reg_b_sel <= 3'd5;
            end

            if (m_cycle == 3 && t_state == 0) begin
              pc <= {reg_a, reg_b};
            end
          end else if (is_jp_z_a16) begin
            if (f[7]) begin
              pc <= imm16;
              M_CYCLE_MAX <= 3;
            end
          end else if (is_jp_c_a16) begin
            if (f[4]) begin
              pc <= imm16;
              M_CYCLE_MAX <= 3;
            end
          end else if (is_prefix) begin
            // TODO CB instructions
          end else if (is_pop_bc || is_pop_de || is_pop_hl || is_pop_af) begin
            if (m_cycle == 0 && t_state == 0) begin
              o_mem_rd_addr <= sp;
            end else if (m_cycle == 0 && t_state == 3) begin
              prev_mem_rd_data <= i_mem_rd_data;
            end else if (m_cycle == 1 && t_state == 0) begin
              o_mem_rd_addr <= sp + 1;
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
          end else if (is_ldh_c_a) begin
            if (m_cycle == 1 && t_state == 0) begin
              o_mem_wr_data <= reg_a;
              o_mem_wr_en   <= 1'b1;
            end
          end else if (is_ldh_a_c) begin
            if (m_cycle == 1 && t_state == 0) begin
              reg_wr_en   <= 1'b1;
              reg_wr_data <= i_mem_rd_data;
            end
          end else if (is_ld_rr_imm) begin
            if (m_cycle == 2 && t_state == 0) begin
              case (ir)
                8'h01: bc_wr_en <= 1;
                8'h11: de_wr_en <= 1;
                8'h21: hl_wr_en <= 1;

                default: ;
              endcase
            end
          end else if (is_inc_rr) begin
            case (ir)
              8'h03: begin
                bc_wr_en <= 1'b1;
              end
              8'h13: begin
                de_wr_en <= 1'b1;
              end
              8'h23: begin
                hl_wr_en <= 1'b1;
              end
              8'h33: begin
                sp <= sp + 1;
              end

              default: ;
            endcase
          end else if (is_dec_rr) begin
            case (ir)
              8'h0B: begin
                bc_wr_en <= 1'b1;
              end
              8'h1B: begin
                de_wr_en <= 1'b1;
              end
              8'h2B: begin
                hl_wr_en <= 1'b1;
              end
              8'h3B: begin
                sp <= sp - 1;
              end

              default: ;
            endcase
          end else if (is_inc_r) begin
            if (m_cycle == 0 && t_state == 0) begin
              reg_wr_en <= 1'b1;

              f[7]   = (reg_wr_data == 8'h00);
              f[6]   = 1'b0;
              f[5]   = ((reg_a & 8'h0F) + 1 > 4'hF);
              f[4]   = f[4];
              f[3:0] = 4'b0000;
            end
          end else if (is_dec_r) begin
            if (m_cycle == 0 && t_state == 0) begin
              reg_wr_en <= 1'b1;

              f[7]   = (reg_wr_data == 8'h00);
              f[6]   = 1'b1;
              f[5]   = ((reg_a & 8'h0F) == 8'h00);
              f[4]   = f[4];
              f[3:0] = 4'b0000;
            end
          end else if (is_inc_hl_mem) begin
            if (m_cycle == 2 && t_state == 0) begin
              o_mem_wr_addr <= {reg_a, reg_b};
              o_mem_wr_data <= i_mem_rd_data + 1;
              o_mem_wr_en   <= 1'b1;

              f[7]   = (o_mem_wr_data == 8'h00);
              f[6]   = 1'b0;
              f[5]   = ((i_mem_rd_data & 8'h0F) + 1 > 4'hF);
              f[4]   = f[4];
              f[3:0] = 4'b0000;
            end
          end else if (is_dec_hl_mem) begin
            if (m_cycle == 2 && t_state == 0) begin
              o_mem_wr_addr <= {reg_a, reg_b};
              o_mem_wr_data <= i_mem_rd_data - 1;
              o_mem_wr_en   <= 1'b1;

              f[7]   = (o_mem_wr_data == 8'h00);
              f[6]   = 1'b1;
              f[5]   = ((i_mem_rd_data & 8'h0F) == 0);
              f[4]   = f[4];
              f[3:0] = 4'b0000;
            end
          end else if (is_ld_a16_sp) begin
            if (m_cycle == 0 && t_state == 0) begin
              o_mem_wr_addr <= imm16;
              o_mem_wr_data <= sp[7:0];
              o_mem_wr_en   <= 1'b1;
            end
            if (m_cycle == 1 && t_state == 0) begin
              o_mem_wr_addr <= imm16 + 16'd1;
              o_mem_wr_data <= sp[15:8];
              o_mem_wr_en   <= 1'b1;
            end
          end else if (is_add_hl_rr) begin
            if (m_cycle == 0 && t_state == 0) begin
              reg_a_sel <= 3'd4;
              reg_b_sel <= 3'd5;
              temp_hl   <= {reg_a, reg_b};

              case (ir)
                8'h09: begin
                  reg_a_sel <= 3'd0;
                  reg_b_sel <= 3'd1;
                end
                8'h19: begin
                  reg_a_sel <= 3'd2;
                  reg_b_sel <= 3'd3;
                end
                8'h29: begin
                  reg_a_sel <= 3'd4;
                  reg_b_sel <= 3'd5;
                end
                8'h39: ;

                default: ;
              endcase
            end

            if (m_cycle == 1 && t_state == 0) begin
              if (ir == 8'h39) temp <= sp;
              else temp <= {reg_a, reg_b};

              hl_wr_en   <= 1'b1;
              hl_wr_data <= temp_hl + temp;

              f[7]   = f[7];
              f[6]   = 1'b0;
              f[5]   = ((temp_hl & 16'h0FFF) + (((ir == 8'h39) ? sp : temp) & 16'h0FFF)) > 16'h0FFF;
              f[4]   = ((temp_hl + ((ir == 8'h39) ? sp : temp)) < temp_hl);
              f[3:0] = 4'b0000;

            end
          end else if (is_ld_sp_hl) begin
            if (m_cycle == 0 && t_state == 0) begin
              reg_a_sel <= 3'd4;
              reg_b_sel <= 3'd5;
            end

            if (m_cycle == 1 && t_state == 0) begin
              sp <= {reg_a, reg_b};
            end
          end else if (is_ld_hl_sp_e8) begin
            logic [15:0] imm_se;
            logic [ 4:0] hc_sum;
            logic [ 8:0] c_sum;
            imm_se = {{8{imm[7]}}, imm};
            hc_sum = sp[3:0] + imm[3:0];
            c_sum  = sp[7:0] + imm[7:0];

            hl_wr_en   <= 1'b1;
            hl_wr_data <= sp + imm_se;
            f[7]   = 1'b0;
            f[6]   = 1'b0;
            f[5]   = hc_sum[4];
            f[4]   = c_sum[8];
            f[3:0] = 4'b0000;
          end else if (is_add_sp_e8) begin
            if (m_cycle == 3 && t_state == 0) begin
              logic [15:0] imm_se;
              logic [ 4:0] hc_sum;
              logic [ 8:0] c_sum;
              imm_se = {{8{imm[7]}}, imm};
              hc_sum = sp[3:0] + imm[3:0];
              c_sum  = sp[7:0] + imm[7:0];

              sp <= sp + imm_se;

              f[7]   = 1'b0;
              f[6]   = 1'b0;
              f[5]   = hc_sum[4];
              f[4]   = c_sum[8];
              f[3:0] = 4'b0000;
            end
          end else if (is_add_r_r) begin
            if (m_cycle == 0 && t_state == 0) begin
              logic [7:0] sum8;
              logic [8:0] sum9;

              sum8   = reg_a + reg_b;
              sum9   = {1'b0, reg_a} + {1'b0, reg_b};

              f[7]   = (sum8 == 8'h00);
              f[6]   = 1'b0;
              f[5]   = (((reg_a & 8'h0F) + (reg_b & 8'h0F)) > 8'h0F);
              f[4]   = sum9[8];
              f[3:0] = 4'b0000;

              reg_wr_data <= sum8;
              reg_wr_en   <= 1'b1;
            end
          end else if (is_adc_r_r) begin
            if (m_cycle == 0 && t_state == 0) begin
              logic [7:0] sum8;
              logic [8:0] sum9;

              sum8   = reg_a + reg_b + {7'b0, f[4]};
              sum9   = {1'b0, reg_a} + {1'b0, reg_b} + {8'b0, f[4]};

              f[7]   = (sum8 == 8'h00);
              f[6]   = 1'b0;
              f[5]   = (((reg_a & 8'h0F) + (reg_b & 8'h0F) + {7'b0, f[4]}) > 8'h0F);
              f[4]   = sum9[8];
              f[3:0] = 4'b0000;

              reg_wr_data <= sum8;
              reg_wr_en   <= 1'b1;
            end
          end else if (is_sub_r_r) begin
            if (m_cycle == 0 && t_state == 0) begin
              logic [7:0] diff8;
              logic [8:0] diff9;

              diff8  = reg_a - reg_b;
              diff9  = {1'b0, reg_a} - {1'b0, reg_b};

              f[7]   = (diff8 == 8'h00);
              f[6]   = 1'b1;
              f[5]   = ((reg_a & 8'h0F) < (reg_b & 8'h0F));
              f[4]   = diff9[8];
              f[3:0] = 4'b0000;

              reg_wr_data <= diff8;
              reg_wr_en   <= 1'b1;
            end
          end else if (is_sbc_r_r) begin
            if (m_cycle == 0 && t_state == 0) begin
              logic [7:0] diff8;
              logic [8:0] diff9;
              logic carry;

              carry  = f[4];
              diff8  = reg_a - reg_b - {7'b0, carry};
              diff9  = {1'b0, reg_a} - {1'b0, reg_b} - {8'b0, carry};

              f[7]   = (diff8 == 8'h00);
              f[6]   = 1'b1;
              f[5]   = ((reg_a & 8'h0F) < ((reg_b & 8'h0F) + (carry ? 8'h01 : 8'h00)));
              f[4]   = diff9[8];
              f[3:0] = 4'b0000;

              reg_wr_data <= diff8;
              reg_wr_en   <= 1'b1;
            end
          end else if (is_and_a_r) begin
            if (m_cycle == 0 && t_state == 0) begin
              logic [7:0] and_result = reg_a & reg_b;
              f[7]   = (and_result == 8'h00);
              f[6]   = 1'b0;
              f[5]   = 1'b1;
              f[4]   = 1'b0;
              f[3:0] = 4'b0000;

              reg_wr_data <= and_result;
              reg_wr_en   <= 1'b1;
            end
          end else if (is_xor_a_r) begin
            if (m_cycle == 0 && t_state == 0) begin
              logic [7:0] xor_result = reg_a ^ reg_b;
              f[7]   = (xor_result == 8'h00);
              f[6]   = 1'b0;
              f[5]   = 1'b0;
              f[4]   = 1'b0;
              f[3:0] = 4'b0000;

              reg_wr_data <= xor_result;
              reg_wr_en   <= 1'b1;
            end
          end else if (is_or_a_r) begin
            if (m_cycle == 0 && t_state == 0) begin
              logic [7:0] or_result = reg_a | reg_b;
              f[7]   = (or_result == 8'h00);
              f[6]   = 1'b0;
              f[5]   = 1'b0;
              f[4]   = 1'b0;
              f[3:0] = 4'b0000;

              reg_wr_data <= or_result;
              reg_wr_en   <= 1'b1;
            end
          end else if (is_cp_a_r) begin
            if (m_cycle == 0 && t_state == 0) begin
              logic [7:0] diff8;
              logic [8:0] diff9;

              diff8  = reg_a - reg_b;
              diff9  = {1'b0, reg_a} - {1'b0, reg_b};

              f[7]   = (diff8 == 8'h00);
              f[6]   = 1'b1;
              f[5]   = ((reg_a & 8'h0F) < (reg_b & 8'h0F));
              f[4]   = diff9[8];
              f[3:0] = 4'b0000;
            end
          end else if (is_add_a_hl_mem) begin
            if (m_cycle == 1 && t_state == 0) begin
              logic [7:0] sum8;
              logic [8:0] sum9;
              sum8   = reg_c + i_mem_rd_data;
              sum9   = {1'b0, reg_c} + {1'b0, i_mem_rd_data};

              f[7]   = (sum8 == 8'h00);
              f[6]   = 1'b0;
              f[5]   = (((reg_c & 8'h0F) + (i_mem_rd_data & 8'h0F)) > 8'h0F);
              f[4]   = sum9[8];
              f[3:0] = 4'b0000;

              reg_wr_sel  <= 3'd7;
              reg_wr_data <= sum8;
              reg_wr_en   <= 1'b1;
            end
          end else if (is_sub_a_hl_mem) begin
            if (m_cycle == 1 && t_state == 0) begin
              logic [7:0] diff8;
              logic [8:0] diff9;

              diff8  = reg_c - i_mem_rd_data;
              diff9  = {1'b0, reg_c} - {1'b0, i_mem_rd_data};

              f[7]   = (diff8 == 8'h00);
              f[6]   = 1'b1;
              f[5]   = ((reg_c & 8'h0F) < (i_mem_rd_data & 8'h0F));
              f[4]   = diff9[8];
              f[3:0] = 4'b0000;

              reg_wr_sel  <= 3'd7;
              reg_wr_data <= diff8;
              reg_wr_en   <= 1'b1;
            end
          end else if (is_and_a_hl_mem) begin
            if (m_cycle == 1 && t_state == 0) begin
              logic [7:0] and8;

              and8   = reg_c & i_mem_rd_data;

              f[7]   = (and8 == 8'h00);
              f[6]   = 1'b0;
              f[5]   = 1'b1;
              f[4]   = 1'b0;
              f[3:0] = 4'b0000;

              reg_wr_sel  <= 3'd7;
              reg_wr_data <= and8;
              reg_wr_en   <= 1'b1;
            end
          end else if (is_or_a_hl_mem) begin
            if (m_cycle == 1 && t_state == 0) begin
              logic [7:0] or8;

              or8 = reg_c | i_mem_rd_data;

              f[7] = (or8 == 8'h00);
              f[6] = 1'b0;
              f[5] = 1'b0;
              f[4] = 1'b0;
              f[3:0] = 4'b0000;

              reg_wr_sel  <= 3'd7;
              reg_wr_data <= or8;
              reg_wr_en   <= 1'b1;
            end
          end else if (is_adc_a_hl_mem) begin
            if (m_cycle == 1 && t_state == 0) begin
              logic [7:0] adc8;
              logic [8:0] adc9;
              logic carry_in;

              carry_in = f[4];

              adc8 = reg_c + i_mem_rd_data + {7'b0, carry_in};
              adc9 = {1'b0, reg_c} + {1'b0, i_mem_rd_data} + {8'b0, carry_in};

              f[7] = (adc8 == 8'h00);
              f[6] = 1'b0;
              f[5] = (((reg_c & 8'h0F) + (i_mem_rd_data & 8'h0F) + {7'b0, carry_in}) > 8'h0F);
              f[4] = adc9[8];
              f[3:0] = 4'b0000;

              reg_wr_sel  <= 3'd7;
              reg_wr_data <= adc8;
              reg_wr_en   <= 1'b1;
            end
          end else if (is_sbc_a_hl_mem) begin
            if (m_cycle == 1 && t_state == 0) begin
              logic [7:0] sbc8;
              logic [8:0] sbc9;
              logic carry_in;

              carry_in = f[4];

              sbc8 = reg_c - i_mem_rd_data - {7'b0, carry_in};
              sbc9 = {1'b0, reg_c} - {1'b0, i_mem_rd_data} - {8'b0, carry_in};

              f[7] = (sbc8 == 8'h00);
              f[6] = 1'b1;
              f[5] = ((reg_c & 8'h0F) < ((i_mem_rd_data & 8'h0F) + {7'b0, carry_in})) ? 1'b1 : 1'b0;
              f[4] = sbc9[8];
              f[3:0] = 4'b0000;

              reg_wr_sel  <= 3'd7;
              reg_wr_data <= sbc8;
              reg_wr_en   <= 1'b1;
            end
          end else if (is_xor_a_hl_mem) begin
            if (m_cycle == 1 && t_state == 0) begin
              logic [7:0] xor8;
              xor8   = reg_c ^ i_mem_rd_data;

              f[7]   = (xor8 == 8'h00);
              f[6]   = 1'b0;
              f[5]   = 1'b0;
              f[4]   = 1'b0;
              f[3:0] = 4'b0000;

              reg_wr_sel  <= 3'd7;
              reg_wr_data <= xor8;
              reg_wr_en   <= 1'b1;
            end
          end else if (is_cp_a_hl_mem) begin
            if (m_cycle == 1 && t_state == 0) begin
              logic [7:0] diff8;
              logic [8:0] diff9;

              diff8  = reg_c - i_mem_rd_data;
              diff9  = {1'b0, reg_c} - {1'b0, i_mem_rd_data};

              f[7]   = (diff8 == 8'h00);
              f[6]   = 1'b1;
              f[5]   = ((reg_c & 8'h0F) < (i_mem_rd_data & 8'h0F)) ? 1'b1 : 1'b0;
              f[4]   = diff9[8];
              f[3:0] = 4'b0000;
            end
          end else if (is_add_a_n8) begin
            if (m_cycle == 1 && t_state == 0) begin
              logic [7:0] sum8;
              logic [8:0] sum9;
              sum8   = reg_a + imm;
              sum9   = {1'b0, reg_a} + {1'b0, imm};

              f[7]   = (sum8 == 8'h00);
              f[6]   = 1'b0;
              f[5]   = (((reg_a & 8'h0F) + (imm & 8'h0F)) > 8'h0F);
              f[4]   = sum9[8];
              f[3:0] = 4'b0000;

              reg_wr_sel  <= 3'd7;
              reg_wr_data <= sum8;
              reg_wr_en   <= 1'b1;
            end
          end else if (is_sub_a_n8) begin
            if (m_cycle == 1 && t_state == 0) begin
              logic [7:0] diff8;
              logic [8:0] diff9;

              diff8  = reg_a - imm;
              diff9  = {1'b0, reg_a} - {1'b0, imm};

              f[7]   = (diff8 == 8'h00);
              f[6]   = 1'b1;
              f[5]   = ((reg_a & 8'h0F) < (imm & 8'h0F));
              f[4]   = diff9[8];
              f[3:0] = 4'b0000;

              reg_wr_sel  <= 3'd7;
              reg_wr_data <= diff8;
              reg_wr_en   <= 1'b1;
            end
          end else if (is_and_a_n8) begin
            if (m_cycle == 1 && t_state == 0) begin
              logic [7:0] and8;

              and8   = reg_a & imm;

              f[7]   = (and8 == 8'h00);
              f[6]   = 1'b0;
              f[5]   = 1'b1;
              f[4]   = 1'b0;
              f[3:0] = 4'b0000;

              reg_wr_sel  <= 3'd7;
              reg_wr_data <= and8;
              reg_wr_en   <= 1'b1;
            end
          end else if (is_or_a_n8) begin
            if (m_cycle == 1 && t_state == 0) begin
              logic [7:0] or8;

              or8 = reg_a | imm;

              f[7] = (or8 == 8'h00);
              f[6] = 1'b0;
              f[5] = 1'b0;
              f[4] = 1'b0;
              f[3:0] = 4'b0000;

              reg_wr_sel  <= 3'd7;
              reg_wr_data <= or8;
              reg_wr_en   <= 1'b1;
            end
          end else if (is_adc_a_n8) begin
            if (m_cycle == 1 && t_state == 0) begin
              logic [7:0] adc8;
              logic [8:0] adc9;
              logic carry_in;

              carry_in = f[4];

              adc8 = reg_a + imm + {7'b0, carry_in};
              adc9 = {1'b0, reg_a} + {1'b0, imm} + {8'b0, carry_in};

              f[7] = (adc8 == 8'h00);
              f[6] = 1'b0;
              f[5] = (((reg_a & 8'h0F) + (imm & 8'h0F) + {7'b0, carry_in}) > 8'h0F);
              f[4] = adc9[8];
              f[3:0] = 4'b0000;

              reg_wr_sel  <= 3'd7;
              reg_wr_data <= adc8;
              reg_wr_en   <= 1'b1;
            end
          end else if (is_sbc_a_n8) begin
            if (m_cycle == 1 && t_state == 0) begin
              logic [7:0] sbc8;
              logic [8:0] sbc9;
              logic carry_in;

              carry_in = f[4];

              sbc8 = reg_a - imm - {7'b0, carry_in};
              sbc9 = {1'b0, reg_a} - {1'b0, imm} - {8'b0, carry_in};

              f[7] = (sbc8 == 8'h00);
              f[6] = 1'b1;
              f[5] = ((reg_a & 8'h0F) < ((imm & 8'h0F) + {7'b0, carry_in})) ? 1'b1 : 1'b0;
              f[4] = sbc9[8];
              f[3:0] = 4'b0000;

              reg_wr_sel  <= 3'd7;
              reg_wr_data <= sbc8;
              reg_wr_en   <= 1'b1;
            end
          end else if (is_xor_a_n8) begin
            if (m_cycle == 1 && t_state == 0) begin
              logic [7:0] xor8;
              xor8   = reg_a ^ imm;

              f[7]   = (xor8 == 8'h00);
              f[6]   = 1'b0;
              f[5]   = 1'b0;
              f[4]   = 1'b0;
              f[3:0] = 4'b0000;

              reg_wr_sel  <= 3'd7;
              reg_wr_data <= xor8;
              reg_wr_en   <= 1'b1;
            end
          end else if (is_cp_a_n8) begin
            if (m_cycle == 1 && t_state == 0) begin
              logic [7:0] diff8;
              logic [8:0] diff9;

              diff8  = reg_a - imm;
              diff9  = {1'b0, reg_a} - {1'b0, imm};

              f[7]   = (diff8 == 8'h00);
              f[6]   = 1'b1;
              f[5]   = ((reg_a & 8'h0F) < (imm & 8'h0F)) ? 1'b1 : 1'b0;
              f[4]   = diff9[8];
              f[3:0] = 4'b0000;
            end
          end else if (is_pop_bc) begin
            if (m_cycle == 1 && t_state == 0) begin
              bc_wr_en   <= 1'b1;
              bc_wr_data <= {i_mem_rd_data, prev_mem_rd_data};
              sp         <= sp + 16'd2;
            end
          end else if (is_pop_de) begin
            if (m_cycle == 1 && t_state == 0) begin
              de_wr_en   <= 1'b1;
              de_wr_data <= {i_mem_rd_data, prev_mem_rd_data};
              sp         <= sp + 16'd2;
            end
          end else if (is_pop_hl) begin
            if (m_cycle == 1 && t_state == 0) begin
              hl_wr_en   <= 1'b1;
              hl_wr_data <= {i_mem_rd_data, prev_mem_rd_data};
              sp         <= sp + 16'd2;
            end
          end else if (is_pop_af) begin
            if (m_cycle == 1 && t_state == 0) begin
              reg_wr_sel  <= 3'd7;
              reg_wr_data <= i_mem_rd_data;
              reg_wr_en   <= 1'b1;
              f = {prev_mem_rd_data & 8'hF0};
              sp <= sp + 16'd2;
            end
          end
        end

        default: ;
      endcase
    end
  end
endmodule
