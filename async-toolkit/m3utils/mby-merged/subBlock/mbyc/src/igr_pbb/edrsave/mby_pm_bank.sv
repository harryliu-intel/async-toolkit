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
// -- Author       : Edward C. Ross
// -- Project Name : MadisonBay
// -- Description  : This is a bank block in Packet Memory block.
//                   It contains N_CHUNKS of SRAM shells to interface with SRAM
//                   wrapper. The total pipeline mby_delay is 2, one for read data valid
//                   and one for flopping the outputs.
//------------------------------------------------------------------------------

`include  "hlp_pm_mem.def"
`include  "hlp_checkers_ext.vs"

module  mby_pm_bank  (
  input  logic  clk,
  input  logic  mem_rst_b,

  input  [$clog2(hlp_pkg::N_BANKS)-1:0] i_sector_id,

  // internal ripple
  input   hlp_pm_pkg::ctrl_ripple_t       i_ctrl_ripple,
  output  hlp_pm_pkg::ctrl_ripple_t       o_ctrl_ripple,

  // memory buses 
  input   hlp_pm_pkg::sect_rw_data_bus_t  i_w_bus, 
  input   hlp_pm_pkg::sect_rw_data_bus_t  i_r_bus, 
  output  hlp_pm_pkg::sect_rw_data_bus_t  o_w_bus, 
  output  hlp_pm_pkg::sect_rw_data_bus_t  o_r_bus, 

  // physical mem interface
  input   logic  [hlp_pm_pkg::N_CHUNKS-1:0] [`HLP_PM_SECTOR0_FROM_MEM_WIDTH-1:0]  i_frm_sector_mem_bus,
  output  logic  [hlp_pm_pkg::N_CHUNKS-1:0] [`HLP_PM_SECTOR0_TO_MEM_WIDTH-1:0]    o_tom_sector_mem_bus
);

// import packages
import hlp_pkg::*;
import hlp_pm_pkg::*;

// internal interfaces
logic  [N_CHUNKS-1:0][10:0] chunk_addr;
logic  [N_CHUNKS-1:0]       chunk_rd_en, chunk_wr_en, chunk_rd_valid; 
logic  [N_CHUNKS-1:0]       nc_chunk_init_done, nc_chunk_ecc_uerr;  // lintra s-70036
sram_data_t [N_CHUNKS-1:0]  chunk_rd_data;      
logic  [N_CHUNKS-1:0] [`HLP_PM_SECTOR0_TO_CTL_WIDTH-1:0] nc_pm_sector0_to_ctl;  // lintra s-70036

always_comb  begin  // flop if needed
  o_ctrl_ripple = i_ctrl_ripple;
  o_w_bus       = i_w_bus;
  o_r_bus       = i_r_bus;

  chunk_rd_en = '0;
  chunk_wr_en = '0;

  for( int i=0; i<N_CHUNKS; i++ ) 
    chunk_addr[i] = i_ctrl_ripple.g.a_rd;

  for( int i=0; i<N_CHUNKS/2; i++ ) 
    if( i_ctrl_ripple.v & i_ctrl_ripple.g.sect_en_lower[i_sector_id] ) begin // lintra s-0241 "i_sector_id part select maybe out of bounds"
      if( i_ctrl_ripple.g.sect_rw_lower[i_sector_id] ) begin  // lintra s-0241 // wr
        chunk_addr [i] = i_ctrl_ripple.g.a_wr;
        chunk_wr_en[i] = i_ctrl_ripple.g.en_wr[i];
      end
      else  // rd
        chunk_rd_en[i] = i_ctrl_ripple.g.en_rd[i];
    end

  for( int i=N_CHUNKS/2; i<N_CHUNKS; i++ ) 
    if( i_ctrl_ripple.v & i_ctrl_ripple.g.sect_en_upper[i_sector_id] ) begin
      if( i_ctrl_ripple.g.sect_rw_upper[i_sector_id] ) begin  // lintra s-0241 // wr
        chunk_addr [i] = i_ctrl_ripple.g.a_wr;
        chunk_wr_en[i] = i_ctrl_ripple.g.en_wr[i];
      end
      else  // rd
        chunk_rd_en[i] = i_ctrl_ripple.g.en_rd[i];
    end

  for( int i=0; i<N_CHUNKS; i++ ) begin
    if( chunk_rd_valid[i] ) begin
      o_r_bus.d[i] = chunk_rd_data[i];
      o_r_bus.v[i] = 1'b1;
    end
  end
end
 
generate 
  genvar chunk_i;
  for( chunk_i=0; chunk_i<N_CHUNKS; chunk_i++ )  begin: inst_shell
    hlp_mem_sector0_shell_2048x72  u_mem_sector_shell_2048x72(
      //------------------- clock and reset -------------------
      .clk       ( clk       ),
      .reset_n   ( mem_rst_b ),

      //----------------- Functional Interface ----------------
      .adr       ( chunk_addr   [chunk_i]   ),
      .rd_en     ( chunk_rd_en  [chunk_i]   ),
      .wr_en     ( chunk_wr_en  [chunk_i]   ),
      .wr_data   ( i_w_bus.d    [chunk_i]   ),

      .rd_data   ( chunk_rd_data[chunk_i]   ),  // q1
      .rd_valid  ( chunk_rd_valid[chunk_i]  ), 

      .init_done ( nc_chunk_init_done[chunk_i] ),  // not used in PM

      //--------------------- ECC Interface -------------------
      .ecc_uncor_err  ( nc_chunk_ecc_uerr[chunk_i] ),  // not used in PM

      //----------------- Memory Wrap Interface ---------------
      .pm_sector0_from_mem  ( i_frm_sector_mem_bus[chunk_i] ),
      .pm_sector0_to_mem    ( o_tom_sector_mem_bus[chunk_i] ),

      //------------------- Gen CTR Interface ( not used in PM )-----------------
      .pm_sector0_from_ctl  ( {`HLP_PM_SECTOR0_FROM_CTL_WIDTH{1'b0}} ),
      .pm_sector0_to_ctl    ( nc_pm_sector0_to_ctl[chunk_i] ),

      ////------------------ Dyn Light Sleep ------------------
      .mem_ls_enter  ( 1'b0 )
    );
  end
endgenerate 


// Asserts
// assert property 
  
`HLP_INCLUDE_SVA_MON( "hlp_pm_sector_mon.sva" )


endmodule

