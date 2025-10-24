// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

//------------------------------------------------------------------------------
//
//  INTEL CONFIDENTIAL
//
//  Copyright 2006 - 2017 Intel Corporation All Rights Reserved.
//
//  The source code contained or described herein and all documents related
//  to the source code ("Material") are owned by Intel Corporation or its
//  suppliers or licensors. Title to the Material remains with Intel
//  Corporation or its suppliers and licensors. The Material contains trade
//  secrets and proprietary and confidential information of Intel or its
//  suppliers and licensors. The Material is protected by worldwide copyright
//  and trade secret laws and treaty provisions. No part of the Material may
//  be used, copied, reproduced, modified, published, uploaded, posted,
//  transmitted, distributed, or disclosed in any way without Intel's prior
//  express written permission.
//
//  No license under any patent, copyright, trade secret or other intellectual
//  property right is granted to or conferred upon you by disclosure or
//  delivery of the Materials, either expressly, by implication, inducement,
//  estoppel or otherwise. Any license under such intellectual property rights
//  must be express and approved by Intel in writing.
//
//------------------------------------------------------------------------------
// -- Author       : George Guo
// -- Project Name : Highland Park IP for Snow Ridge
// -- Description  : This is the main Control block in Packet Memory block.
//                   It interfaces with Mgmt ripple, and PM internal blocks.
//------------------------------------------------------------------------------

`include  "hlp_checkers_ext.vs"

module  hlp_pm_ctrl_in  (
  input  logic  clk,
  input  logic  rst_n,  // synchronized warm reset

  // Scheduler interface
  input   mby_pkg::pm_ctrl_t  i_pm_ctrl,  // r/w control

  // To pm_encode_ecc
  output  logic                                                  o_mgmt_wr_v,
  output  logic                  [hlp_pm_pkg::W_CHUNK_DATA-1:0]  o_mgmt_wr,
  output  logic                  [hlp_pm_pkg::N_CHUNKS-1:0]      o_wr_active,
  output  hlp_pm_pkg::ecc_err_t  [hlp_pm_pkg::N_CHUNKS-1:0]      o_wr_errors,

  // internal ripple
  output  hlp_pm_pkg::ctrl_ripple_t  o_ctrl_ripple,

  // Management Interface
  input   logic              i_mgmt_v,
  input   hlp_pkg::mgmt64_t  i_mgmt,
  output  logic              o_mgmt_v,
  output  hlp_pkg::mgmt64_t  o_mgmt
);

// import packages
import hlp_pkg::*;
import hlp_pm_pkg::*;
import hlp_c_pm_pkg::*;
import hlp_register_constants_pkg::*;

// internal interfaces

logic                     mgmt_rd_v, mgmt_rd_v_q, ctrl_ripple_v_q;
logic  [N_CHUNKS-1:0]     rd_active, en_rd_q;
logic  [W_SECT_ADDR-1:0]  a_rd_q,   a_rd_d;
logic  [N_CHUNKS-1:0]     en_wr_q;
logic  [W_SECT_ADDR-1:0]  a_wr_q,   a_wr_d;
logic  [N_SECTORS-1:0]    sect_rw_upper_q, sect_rw_lower_q, sect_rw_d;
logic  [N_SECTORS-1:0]    sect_en_upper_q, sect_en_lower_q, sect_en_d;

c_pm_mgmt_ctrl_t   c_pm_mgmt_ctrl;
c_pm_mgmt_write_t  c_pm_mgmt_write;
c_pm_err_write_t   c_pm_err_write;

pm_addr_t  pm_wr_addr, pm_rd_addr;  // lintra s-70036 not all bits used
logic  [$clog2(W_SEG+1)-1:0]  pm_wr_len;
logic  [$clog2(W_SEG+1)-1:0]  pm_rd_len;
logic  pm_wr_sop, pm_rd_sop;

logic  [W_N_CHUNKS:0] wr_chunk_len, rd_chunk_len;

logic  mgmt_wr, mgmt_rd, mgmt_done;
logic  [W_MGMT_DATA64-1:0]  wr_data, rd_data_d;

logic  bkdr_access_en, bkdr_access_rw;
logic  [W_N_CHUNKS-1:0]  bkdr_access_chunk;
logic  [W_PM_ADDR-1:0]   bkdr_access_addr;


always_ff  @( posedge clk or negedge rst_n )
  if( !rst_n ) begin
    o_mgmt_v <= '0;
    o_mgmt   <= '0;
  end
  else begin
    o_mgmt_v <= i_mgmt_v;
    if( i_mgmt_v ) begin
      o_mgmt      <= i_mgmt;
      o_mgmt.data <= rd_data_d;
      o_mgmt.done <= mgmt_done;
    end
  end
 
always_comb  begin
  pm_wr_addr = i_pm_ctrl.wr_addr;
  pm_rd_addr = i_pm_ctrl.rd_addr;
  pm_wr_len  = i_pm_ctrl.wr_len ;
  pm_rd_len  = i_pm_ctrl.rd_len ;
  pm_wr_sop  = i_pm_ctrl.wr_sop ;
  pm_rd_sop  = i_pm_ctrl.rd_sop ;

  wr_chunk_len = cal_num_chunks( pm_wr_len );
  rd_chunk_len = cal_num_chunks( pm_rd_len );

  mgmt_wr = i_mgmt_v & i_mgmt.rw;
  mgmt_rd = i_mgmt_v & !i_mgmt.rw;
  wr_data = i_mgmt.data;

  o_ctrl_ripple.v               = ctrl_ripple_v_q;
  o_ctrl_ripple.g.mgmt_rd_v     = mgmt_rd_v_q;
  o_ctrl_ripple.g.en_rd         = en_rd_q;
  o_ctrl_ripple.g.a_rd          = a_rd_q;
  o_ctrl_ripple.g.en_wr         = en_wr_q;
  o_ctrl_ripple.g.a_wr          = a_wr_q;
  o_ctrl_ripple.g.sect_rw_upper = sect_rw_upper_q;
  o_ctrl_ripple.g.sect_rw_lower = sect_rw_lower_q;
  o_ctrl_ripple.g.sect_en_upper = sect_en_upper_q;
  o_ctrl_ripple.g.sect_en_lower = sect_en_lower_q;
end

always_comb  begin
  bkdr_access_rw    = c_pm_mgmt_ctrl.rw;
  bkdr_access_chunk = c_pm_mgmt_ctrl.chunk;
  bkdr_access_addr  = c_pm_mgmt_ctrl.addr;

  mgmt_rd_v = 0;
  if( (pm_rd_len == '0) & 
      ((pm_wr_len == '0) | (bkdr_access_addr[W_SECT_ADDR +: W_N_SECTORS] != pm_wr_addr[W_SECT_ADDR +: W_N_SECTORS]))
    ) 
    mgmt_rd_v = bkdr_access_en & (!bkdr_access_rw); 

  rd_active = '0;
  if( pm_rd_len != '0 ) begin
    for( int cr=0; cr<N_CHUNKS; cr++ )
      if( cr < rd_chunk_len )
        rd_active[N_CHUNKS-1-cr] = 1;
      else if( pm_rd_sop & (cr > (N_CHUNKS - N_HEAD_CHUNKS - 1)) )
        rd_active[N_CHUNKS-1-cr] = 1;
  end
  else if( mgmt_rd_v )
    for( int cr=0; cr<N_CHUNKS; cr++ )
      if( cr == bkdr_access_chunk )
        rd_active[cr] = 1;

  o_mgmt_wr_v = '0;
  if( (pm_wr_len == '0) & 
      ((pm_rd_len == '0) | (bkdr_access_addr[W_SECT_ADDR +: W_N_SECTORS] != pm_rd_addr[W_SECT_ADDR +: W_N_SECTORS]))
    ) 
    o_mgmt_wr_v = bkdr_access_en & bkdr_access_rw;

  o_mgmt_wr = c_pm_mgmt_write[0 +: W_CHUNK_DATA];

  o_wr_active = '0;
  if( pm_wr_len != '0 ) begin
    for( int cw=0; cw<N_CHUNKS; cw++ )
      if( cw < wr_chunk_len )
        o_wr_active[N_CHUNKS-1-cw] = 1;
      else if( pm_wr_sop & (cw > (N_CHUNKS - N_HEAD_CHUNKS - 1)) )
        o_wr_active[N_CHUNKS-1-cw] = 1;
  end
  else if( o_mgmt_wr_v )
    for( int cw=0; cw<N_CHUNKS; cw++ )
      if( cw == bkdr_access_chunk )
        o_wr_active[cw] = 1;

  for( int i=0; i<N_CHUNKS; i++ )
    o_wr_errors[i] = c_pm_err_write[i*$bits(ecc_err_t) +: $bits(ecc_err_t)];
end

always_comb begin
  sect_en_d = '0;
  sect_rw_d = '0;
  a_wr_d    = '0;
  a_rd_d    = '0;
  
  if( pm_wr_len != '0 ) begin
    a_wr_d = pm_wr_addr[0 +: W_SECT_ADDR];

    for( int w=0; w<N_SECTORS; w++ )
      if( w == pm_wr_addr[W_SECT_ADDR +: W_N_SECTORS] ) begin
        sect_en_d[w] = 1; 
        sect_rw_d[w] = 1;
      end
  end
  else if( o_mgmt_wr_v ) begin
    a_wr_d = bkdr_access_addr[0 +: W_SECT_ADDR];

    for( int w=0; w<N_SECTORS; w++ )
      if( w == bkdr_access_addr[W_SECT_ADDR +: W_N_SECTORS] ) begin
        sect_en_d[w] = 1; 
        sect_rw_d[w] = 1;
      end
  end

  if( pm_rd_len != '0 ) begin
    a_rd_d = pm_rd_addr[0 +: W_SECT_ADDR];

    for( int r=0; r<N_SECTORS; r++ )
      if( r == pm_rd_addr[W_SECT_ADDR +: W_N_SECTORS] ) begin
        sect_en_d[r] = 1; 
        sect_rw_d[r] = 0;
      end
  end
  else if( mgmt_rd_v ) begin
    a_rd_d = bkdr_access_addr[0 +: W_SECT_ADDR];

    for( int r=0; r<N_SECTORS; r++ )
      if( r == bkdr_access_addr[W_SECT_ADDR +: W_N_SECTORS] ) begin
        sect_en_d[r] = 1; 
        sect_rw_d[r] = 0;
      end
  end
end

always_ff  @( posedge clk or negedge rst_n )
  if( !rst_n ) begin
    mgmt_rd_v_q     <= '0;
    en_rd_q         <= '0;
    a_rd_q          <= '0;
    en_wr_q         <= '0;
    a_wr_q          <= '0;
    sect_rw_upper_q <= '0;
    sect_rw_lower_q <= '0;
    sect_en_upper_q <= '0;
    sect_en_lower_q <= '0;
    ctrl_ripple_v_q <= '0;
  end
  else begin
    ctrl_ripple_v_q <= (|sect_en_d);
    if (|sect_en_d) begin
      mgmt_rd_v_q     <= mgmt_rd_v;
      en_rd_q         <= rd_active;
      a_rd_q          <= a_rd_d;
      en_wr_q         <= o_wr_active;
      a_wr_q          <= a_wr_d;
      sect_rw_upper_q <= sect_rw_d;
      sect_rw_lower_q <= sect_rw_d;
      sect_en_upper_q <= sect_en_d;
      sect_en_lower_q <= sect_en_d;
    end
  end

// Register write logic
always_ff  @( posedge clk or negedge rst_n ) 
  if( !rst_n ) begin
    c_pm_mgmt_ctrl  <= PM_MGMT_CTRL_DEFAULT;
    c_pm_mgmt_write <= PM_MGMT_WRITE_DEFAULT;
    c_pm_err_write  <= PM_ERR_WRITE_DEFAULT;

    bkdr_access_en <= '0;
  end 
  else begin
    if( mgmt_wr ) 
      casez( i_mgmt.addr )
        PM_MGMT_CTRL_ADDR: begin
          c_pm_mgmt_ctrl <= {1'b0, wr_data[PM_MGMT_CTRL_BITS-2:0]};
          bkdr_access_en <= 1;
        end

        PM_MGMT_WRITE_ADDR:
          c_pm_mgmt_write <= wr_data[PM_MGMT_WRITE_BITS-1:0];

        PM_ERR_WRITE_ADDR:
          c_pm_err_write <= wr_data[PM_ERR_WRITE_BITS-1:0];

        default: ;  // lintra s-60131
      endcase

    if( o_mgmt_wr_v | mgmt_rd_v ) begin
      c_pm_mgmt_ctrl.executed <= 1;
      bkdr_access_en          <= 0;
    end
  end

// Register read logic
always_comb  begin 
  rd_data_d = i_mgmt.data;
  if( mgmt_rd ) begin
    casez( i_mgmt.addr ) 
      PM_MGMT_CTRL_ADDR:
        rd_data_d = {{(W_MGMT_DATA64-PM_MGMT_CTRL_BITS){1'b0}}, c_pm_mgmt_ctrl};

      PM_MGMT_WRITE_ADDR:
        rd_data_d = {{(W_MGMT_DATA64-PM_MGMT_WRITE_BITS){1'b0}}, c_pm_mgmt_write};

      PM_ERR_WRITE_ADDR:
        rd_data_d = {{(W_MGMT_DATA64-PM_ERR_WRITE_BITS){1'b0}}, c_pm_err_write};

      default: ;  // lintra s-60131
    endcase
  end

  mgmt_done = i_mgmt.done;
  if( (i_mgmt.addr == PM_MGMT_CTRL_ADDR ) |
      (i_mgmt.addr == PM_MGMT_WRITE_ADDR) |
      (i_mgmt.addr == PM_ERR_WRITE_ADDR )
    ) mgmt_done = 1;

end

function automatic [W_N_CHUNKS:0] cal_num_chunks;
input  [$clog2(W_SEG+1)-1:0]  byte_len;

reg  [$clog2(W_SEG+1)-1:0] temp_len;

begin
  temp_len = byte_len + 7;  // W_CHUNK_DATA_B - 1; if 9B, temp_len = 16
  cal_num_chunks = temp_len[$clog2(W_CHUNK_DATA_B) +: (W_N_CHUNKS+1)];
end
endfunction


// Asserts
// assert property 
  
`HLP_INCLUDE_SVA_MON( "hlp_pm_ctrl_in_mon.sva" )

// VCS coverage off
  `HLP_INCLUDE_SVA_COV( "hlp_pm_ctrl_in_cov.sva" )
// VCS coverage on

endmodule

