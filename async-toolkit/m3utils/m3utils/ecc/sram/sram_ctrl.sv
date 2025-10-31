// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

// Copyright 2013 Intel Corporation. All Rights Reserved.

module sram_ctrl
#(
  parameter W_MGMT_ADDR                 = 24,    // mgmt addr width
  parameter W_MGMT_DATA                 = 64,    // mgmt data width
  parameter logic[W_MGMT_ADDR-1:0] ADDR = 24'b0, // register address
  parameter N_WORDS                     = 1,     // number of words in register
  parameter DEFAULT                     = '0,    // register default
  parameter STAGES                      = 2,     // number of stages between en/rw and u/c_err
  parameter ERR_FLOPS                   = 0,     // number of flops on i_*_err
  parameter N                           = 1      // number of SRAMs
)(
  input   logic                                     clk,
  input   logic                                     rst_n,

  //managment reads and writes
  input   logic[W_MGMT_ADDR-1:0]                    i_mgmt_addr,
  input   logic                                     i_mgmt_rw,
  input   logic[W_MGMT_DATA-1:0]                    i_mgmt_data,
  input   logic                                     i_mgmt_v,
  //read results, expect user to OR this into mgmt.data
  //will be zero if not reading
  output  logic[W_MGMT_DATA-1:0]                    o_mgmt_data,

  input  logic [N-1:0]                              i_en,
  input  logic [N-1:0]                              i_rw,
  input  logic [N-1:0]                              i_c_err,
  input  logic [N-1:0]                              i_u_err,
  input  logic [N-1:0]                              i_bist_pass,
  input  logic [N-1:0]                              i_bist_fail,
  output logic [N-1:0][1:0]                         o_err_write,

  output logic [1:0]                                o_intr
  );

  localparam C         = 0;
  localparam U         = 1;
  localparam P         = 0;
  localparam F         = 1;
  localparam ERR_WRITE_BITS  = 2;
  localparam UC_ERR_BITS  = 2;
  localparam BIST_BITS = 2;

  typedef struct packed {
    logic [N-1:0]                bist_fail;
    logic [N-1:0]                bist_pass;
    logic [N-1:0]                u_err;
    logic [N-1:0]                c_err;
    logic [N*ERR_WRITE_BITS-1:0] err_write;
  } register_t;

  localparam W_REG = $bits(register_t);
  localparam W_MI = (N_WORDS==1) ? 1 : $clog2(N_WORDS);

  typedef logic [W_MI-1:0]        mgmt_idx_t;
  typedef logic [W_MGMT_ADDR-1:0] mgmt_addr_t;
  logic                                 mgmt_match;
  mgmt_idx_t                            mgmt_index;
  logic [STAGES+ERR_FLOPS:0][N-1:0]     v;
  logic [ERR_FLOPS:0][N-1:0]            c_err, u_err;
  register_t                            ctrl, ctrl_d;

  assign v[0] = i_en & ~i_rw; // valid read
  assign c_err[0] = i_c_err;
  assign u_err[0] = i_u_err;

  generate
    if(N_WORDS == 1) begin : gen_single_word
      assign mgmt_match = (i_mgmt_addr == ADDR);
      assign mgmt_index = '0;
    end else begin         : gen_multi_word
      assign mgmt_match = (i_mgmt_addr >= ADDR) &
                          (i_mgmt_addr < (ADDR + mgmt_addr_t'(N_WORDS*W_MGMT_DATA/W_MGMT_DATA32)));
      assign mgmt_index = mgmt_match ?
                          mgmt_idx_t'((i_mgmt_addr - ADDR) /
                                      unsigned'(W_MGMT_DATA/W_MGMT_DATA32)):
                          '0;
    end
  endgenerate

  always_ff @(posedge clk) begin
    if (rst_n==1'b0) begin
      v[STAGES+ERR_FLOPS:1]    <= '0;
      ctrl                     <= DEFAULT;
      o_intr                   <= '0;
    end else begin
      v[STAGES+ERR_FLOPS:1]    <= v[STAGES+ERR_FLOPS-1:0];
      ctrl                     <= ctrl_d;
  
      // if any bit is set on a valid read, pass it on
      o_intr[C]                <= |(v[STAGES+ERR_FLOPS] & c_err[ERR_FLOPS]);
      o_intr[U]                <= |(v[STAGES+ERR_FLOPS] & u_err[ERR_FLOPS]);
    end
  end

  generate
    if(ERR_FLOPS>0) begin : gen_err_flops
      always_ff @(posedge clk) begin
        if (rst_n==1'b0) begin
          c_err[ERR_FLOPS:1]       <= '0;
          u_err[ERR_FLOPS:1]       <= '0;
        end else begin
          c_err[ERR_FLOPS:1]       <= c_err[ERR_FLOPS-1:0];
          u_err[ERR_FLOPS:1]       <= u_err[ERR_FLOPS-1:0];
        end
      end
    end
  endgenerate

  always_comb begin:write
    logic [W_MGMT_DATA*N_WORDS-1:0] mgmt_full;
    register_t                      ctrl_m;
    mgmt_full = '0;
    ctrl_m = '0;

    // normal update
    ctrl_d = ctrl;
    ctrl_d.c_err |= (v[STAGES+ERR_FLOPS] & c_err[ERR_FLOPS]);
    ctrl_d.u_err |= (v[STAGES+ERR_FLOPS] & u_err[ERR_FLOPS]);

    // mgmt write
    if(i_mgmt_v & i_mgmt_rw & mgmt_match) begin
      // start with old value
      ctrl_m = ctrl_d;
      // except for CW1
      ctrl_m.c_err = '0;
      ctrl_m.u_err = '0;
      mgmt_full[W_REG-1:0] = ctrl_m;
      // update the word from mgmt
      mgmt_full[mgmt_index * W_MGMT_DATA +: W_MGMT_DATA] = i_mgmt_data; // lintra s-0241 "mgmt_match guarantees mgmt_index is not oob"
      ctrl_m = mgmt_full[W_REG-1:0];
      // clear CW1
      ctrl_m.c_err = ctrl_d.c_err & ~ctrl_m.c_err;
      ctrl_m.u_err = ctrl_d.u_err & ~ctrl_m.u_err;
      // write the result back
      ctrl_d = ctrl_m;
    end

    // always use values from inputs
    ctrl_d.bist_pass = i_bist_pass;
    ctrl_d.bist_fail = i_bist_fail;
  end

  always_comb begin:read
    logic [W_MGMT_DATA*N_WORDS-1:0] mgmt_full;
    mgmt_full = '0;
    mgmt_full[W_REG-1:0] = ctrl;
    o_mgmt_data = i_mgmt_data;
    if(i_mgmt_v & ~i_mgmt_rw & mgmt_match) begin
      o_mgmt_data = mgmt_full[mgmt_index * W_MGMT_DATA +: W_MGMT_DATA]; // lintra s-0241 "mgmt_match guarantees mgmt_index is not oob"
    end
  end

  assign o_err_write = ctrl.err_write;

endmodule
