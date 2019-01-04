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
// -- Description  : This is the top level of Packet Memory block.
//                   It interfaces with IPP, Scheduler and Modify blocks.
//------------------------------------------------------------------------------

`include  "hlp_pm_mem.def"
`include  "hlp_checkers_ext.vs"

module  hlp_pm  (
  input  logic  clk,
  input  logic  rst_n,  // synchronized warm reset

  // Scheduler interface
  input   hlp_pkg::pm_ctrl_t  i_pm_ctrl,  // r/w control

  // IPP interface
  input   hlp_pkg::pm_write_t  i_pm_write, // data to write into memory

  // Modify interface
  output  hlp_pkg::pm_read_t  o_pm_read,   // data read from memory plus an err bit

  // Management Interface
  input  hlp_pkg::imn_broadcast_t   i_broadcast,  // lintra s-70036 not all elements are used
  input  hlp_pkg::imn_rpl_frwd_t    i_rpl_frwd,
  output hlp_pkg::imn_rpl_bkwd_t    o_rpl_bkwd,
  output hlp_pkg::imn_rpl_frwd_t    o_rpl_frwd,
  input  hlp_pkg::imn_rpl_bkwd_t    i_rpl_bkwd,

  // SRAM interface
  input   logic  [hlp_pm_pkg::N_CHUNKS-1:0] [`HLP_PM_SECTOR0_FROM_MEM_WIDTH-1:0]  i_frm_sector0,
  input   logic  [hlp_pm_pkg::N_CHUNKS-1:0] [`HLP_PM_SECTOR0_FROM_MEM_WIDTH-1:0]  i_frm_sector1,
  input   logic  [hlp_pm_pkg::N_CHUNKS-1:0] [`HLP_PM_SECTOR0_FROM_MEM_WIDTH-1:0]  i_frm_sector2,
  input   logic  [hlp_pm_pkg::N_CHUNKS-1:0] [`HLP_PM_SECTOR0_FROM_MEM_WIDTH-1:0]  i_frm_sector3,
  input   logic  [hlp_pm_pkg::N_CHUNKS-1:0] [`HLP_PM_SECTOR0_FROM_MEM_WIDTH-1:0]  i_frm_sector4,
  input   logic  [hlp_pm_pkg::N_CHUNKS-1:0] [`HLP_PM_SECTOR0_FROM_MEM_WIDTH-1:0]  i_frm_sector5,
  input   logic  [hlp_pm_pkg::N_CHUNKS-1:0] [`HLP_PM_SECTOR0_FROM_MEM_WIDTH-1:0]  i_frm_sector6,
  input   logic  [hlp_pm_pkg::N_CHUNKS-1:0] [`HLP_PM_SECTOR0_FROM_MEM_WIDTH-1:0]  i_frm_sector7,
  input   logic  [hlp_pm_pkg::N_CHUNKS-1:0] [`HLP_PM_SECTOR0_FROM_MEM_WIDTH-1:0]  i_frm_sector8,
  input   logic  [hlp_pm_pkg::N_CHUNKS-1:0] [`HLP_PM_SECTOR0_FROM_MEM_WIDTH-1:0]  i_frm_sector9,
  input   logic  [hlp_pm_pkg::N_CHUNKS-1:0] [`HLP_PM_SECTOR0_FROM_MEM_WIDTH-1:0]  i_frm_sector10,
  input   logic  [hlp_pm_pkg::N_CHUNKS-1:0] [`HLP_PM_SECTOR0_FROM_MEM_WIDTH-1:0]  i_frm_sector11,

  output  logic  [hlp_pm_pkg::N_CHUNKS-1:0] [`HLP_PM_SECTOR0_TO_MEM_WIDTH-1:0]  o_tom_sector0,
  output  logic  [hlp_pm_pkg::N_CHUNKS-1:0] [`HLP_PM_SECTOR0_TO_MEM_WIDTH-1:0]  o_tom_sector1,
  output  logic  [hlp_pm_pkg::N_CHUNKS-1:0] [`HLP_PM_SECTOR0_TO_MEM_WIDTH-1:0]  o_tom_sector2,
  output  logic  [hlp_pm_pkg::N_CHUNKS-1:0] [`HLP_PM_SECTOR0_TO_MEM_WIDTH-1:0]  o_tom_sector3,
  output  logic  [hlp_pm_pkg::N_CHUNKS-1:0] [`HLP_PM_SECTOR0_TO_MEM_WIDTH-1:0]  o_tom_sector4,
  output  logic  [hlp_pm_pkg::N_CHUNKS-1:0] [`HLP_PM_SECTOR0_TO_MEM_WIDTH-1:0]  o_tom_sector5,
  output  logic  [hlp_pm_pkg::N_CHUNKS-1:0] [`HLP_PM_SECTOR0_TO_MEM_WIDTH-1:0]  o_tom_sector6,
  output  logic  [hlp_pm_pkg::N_CHUNKS-1:0] [`HLP_PM_SECTOR0_TO_MEM_WIDTH-1:0]  o_tom_sector7,
  output  logic  [hlp_pm_pkg::N_CHUNKS-1:0] [`HLP_PM_SECTOR0_TO_MEM_WIDTH-1:0]  o_tom_sector8,
  output  logic  [hlp_pm_pkg::N_CHUNKS-1:0] [`HLP_PM_SECTOR0_TO_MEM_WIDTH-1:0]  o_tom_sector9,
  output  logic  [hlp_pm_pkg::N_CHUNKS-1:0] [`HLP_PM_SECTOR0_TO_MEM_WIDTH-1:0]  o_tom_sector10,
  output  logic  [hlp_pm_pkg::N_CHUNKS-1:0] [`HLP_PM_SECTOR0_TO_MEM_WIDTH-1:0]  o_tom_sector11
);


// import packages
import hlp_pkg::*;
import hlp_pm_pkg::*;
import hlp_register_constants_pkg::*;

// internal interfaces
pm_ctrl_t                          pm_ctrl_dly;
pm_write_t                         pm_write_dly;
logic                              ci_o_mgmt_wr_v, co_i_mgmt_rd_v;
logic         [W_CHUNK_DATA-1:0]   ci_o_mgmt_wr,   co_i_mgmt_rd;
logic         [N_CHUNKS-1:0]       ci_o_wr_active;
ecc_err_t     [N_CHUNKS-1:0]       ci_o_wr_errors;
ecc_err_log_t                      co_i_ecc_err_log;

ctrl_ripple_t      [N_BANKS:0]   sect_i_c_rip;  // lintra s-70036 not all elements are used
ctrl_ripple_t      [N_BANKS-1:0] sect_o_c_rip;
sect_rw_data_bus_t [N_BANKS:0]   sect_i_r_bus,   sect_i_w_bus;  // lintra s-70036 not all elements are used
sect_rw_data_bus_t [N_BANKS-1:0] sect_o_r_bus,   sect_o_w_bus;
  
  
logic     ci_o_mgmt_v, ci_i_mgmt_v;
mgmt64_t  ci_o_mgmt,   ci_i_mgmt;
logic     co_i_mgmt_v, co_o_mgmt_v, co_o_int, co_o_int_dly;
mgmt64_t  co_i_mgmt,   co_o_mgmt;


// instantiate submodules
hlp_delay  #(
  .WIDTH ( $bits(pm_ctrl_t) ), 
  .STAGES( I_DELAY_CTRL_IN  )
)  u_delay_ctrl_in  (
   .clk    ( clk   ),
   .rst_n  ( rst_n ),

   .i_in   ( i_pm_ctrl   ),
   .o_out  ( pm_ctrl_dly )
);  

hlp_delay  #(
  .WIDTH ( $bits(pm_write_t) ), 
  .STAGES( I_DELAY_PM_WRITE  )
)  u_delay_pm_write  (
   .clk    ( clk   ),
   .rst_n  ( rst_n ),

   .i_in   ( i_pm_write   ),
   .o_out  ( pm_write_dly )
);  

hlp_pm_ctrl_in   u_pm_ctrl_in  (
  .clk    ( clk   ),
  .rst_n  ( rst_n ), 

  // Scheduler interface
  .i_pm_ctrl  ( pm_ctrl_dly ), 

  // To pm_encode_ecc
  .o_mgmt_wr_v  ( ci_o_mgmt_wr_v ),
  .o_mgmt_wr    ( ci_o_mgmt_wr   ),
  .o_wr_active  ( ci_o_wr_active ),
  .o_wr_errors  ( ci_o_wr_errors ),

  // internal ripple
  .o_ctrl_ripple ( sect_i_c_rip[0] ),

  // Management Interface
  .i_mgmt_v  ( ci_i_mgmt_v ),
  .i_mgmt    ( ci_i_mgmt   ),
  .o_mgmt_v  ( ci_o_mgmt_v ),
  .o_mgmt    ( ci_o_mgmt   )
);

hlp_pm_encode_ecc  u_pm_encode_ecc  (
  .clk    ( clk   ),
  .rst_n  ( rst_n ), 

  // Scheduler interface
  .i_pm_write   ( pm_write_dly ), 

  // From pm_ctrl_in
  .i_mgmt_wr_v  ( ci_o_mgmt_wr_v ),
  .i_mgmt_wr    ( ci_o_mgmt_wr   ),
  .i_wr_active  ( ci_o_wr_active ),
  .i_wr_errors  ( ci_o_wr_errors ),

  // To memories
  .o_w_bus      ( sect_i_w_bus[0] )
);

hlp_pm_ctrl_out  u_pm_ctrl_out  (
  .clk    ( clk   ),
  .rst_n  ( rst_n ), 

  // pm_chedk_ecc
  .i_mgmt_rd_v   ( co_i_mgmt_rd_v   ),
  .i_mgmt_rd     ( co_i_mgmt_rd     ),
  .i_ecc_err_log ( co_i_ecc_err_log ),

  // Management Interface
  .i_mgmt_v  ( co_i_mgmt_v ),
  .i_mgmt    ( co_i_mgmt   ),
  .o_mgmt_v  ( co_o_mgmt_v ),
  .o_mgmt    ( co_o_mgmt   ),
  .o_int     ( co_o_int    )
);

hlp_pm_check_ecc  u_pm_check_ecc  (
  .clk    ( clk   ),
  .rst_n  ( rst_n ), 

  // internal ripple from last sector
  .i_ctrl_rpl_v ( sect_i_c_rip[N_BANKS].v               ),
  .i_mgmt_rd_v  ( sect_i_c_rip[N_BANKS].g.mgmt_rd_v     ),
  .i_en_rd      ( sect_i_c_rip[N_BANKS].g.en_rd         ),
  .i_a_rd       ( sect_i_c_rip[N_BANKS].g.a_rd          ),
  .i_sect_rw    ( sect_i_c_rip[N_BANKS].g.sect_rw_upper ),
  .i_sect_en    ( sect_i_c_rip[N_BANKS].g.sect_en_upper ),
  .i_r_bus      ( sect_i_r_bus[N_BANKS]                 ),

  // ctrl_out
  .o_mgmt_rd_v    ( co_i_mgmt_rd_v   ),
  .o_mgmt_rd      ( co_i_mgmt_rd     ),
  .o_ecc_err_log  ( co_i_ecc_err_log ),

  // PM Read out to Modify
  .o_pm_read  ( o_pm_read )
);

logic  [hlp_pkg::N_BANKS-1:0] [hlp_pm_pkg::N_CHUNKS-1:0] [`HLP_PM_SECTOR0_FROM_MEM_WIDTH-1:0]  frm_pm_mem_bus;
logic  [hlp_pkg::N_BANKS-1:0] [hlp_pm_pkg::N_CHUNKS-1:0] [`HLP_PM_SECTOR0_TO_MEM_WIDTH-1:0]    tom_pm_mem_bus;

assign  frm_pm_mem_bus[0]  = i_frm_sector0;
assign  frm_pm_mem_bus[1]  = i_frm_sector1;
assign  frm_pm_mem_bus[2]  = i_frm_sector2;
assign  frm_pm_mem_bus[3]  = i_frm_sector3;
assign  frm_pm_mem_bus[4]  = i_frm_sector4;
assign  frm_pm_mem_bus[5]  = i_frm_sector5;
assign  frm_pm_mem_bus[6]  = i_frm_sector6;
assign  frm_pm_mem_bus[7]  = i_frm_sector7;
assign  frm_pm_mem_bus[8]  = i_frm_sector8;
assign  frm_pm_mem_bus[9]  = i_frm_sector9;
assign  frm_pm_mem_bus[10] = i_frm_sector10;
assign  frm_pm_mem_bus[11] = i_frm_sector11;

assign  o_tom_sector0  = tom_pm_mem_bus[0];
assign  o_tom_sector1  = tom_pm_mem_bus[1];
assign  o_tom_sector2  = tom_pm_mem_bus[2];
assign  o_tom_sector3  = tom_pm_mem_bus[3];
assign  o_tom_sector4  = tom_pm_mem_bus[4];
assign  o_tom_sector5  = tom_pm_mem_bus[5];
assign  o_tom_sector6  = tom_pm_mem_bus[6];
assign  o_tom_sector7  = tom_pm_mem_bus[7];
assign  o_tom_sector8  = tom_pm_mem_bus[8];
assign  o_tom_sector9  = tom_pm_mem_bus[9];
assign  o_tom_sector10 = tom_pm_mem_bus[10];
assign  o_tom_sector11 = tom_pm_mem_bus[11];

logic  [N_BANKS-1:0] [$clog2(N_BANKS)-1:0]  sector_id;
assign sect_i_r_bus[0] = '0;
  
generate 
  for( genvar sect_i=0; sect_i<N_BANKS; sect_i++ ) begin: inst_sector
    assign  sector_id[sect_i] = sect_i;

    hlp_pm_bank  u_pm_bank  (
      .clk             ( clk                   ),
      .mem_rst_b       ( i_broadcast.mem_rst_b ),
      .i_sector_id     ( sector_id[sect_i]     ),

      // internal ripple
      .i_ctrl_ripple   ( sect_i_c_rip[sect_i] ),
      .o_ctrl_ripple   ( sect_o_c_rip[sect_i] ),

      // memory buses
      .i_w_bus         ( sect_i_w_bus[sect_i] ),
      .i_r_bus         ( sect_i_r_bus[sect_i] ),
      .o_w_bus         ( sect_o_w_bus[sect_i] ),
      .o_r_bus         ( sect_o_r_bus[sect_i] ),

      // physical mem interface
      .i_frm_sector_mem_bus ( frm_pm_mem_bus[sect_i] ),
      .o_tom_sector_mem_bus ( tom_pm_mem_bus[sect_i] )
    );

    // Gate other control signals when not enabled.
    hlp_delay_v  #(
      .WIDTH ( $bits(ctrl_ripple_gated_t) ), 
      .STAGES( ((sect_i%SECT_PIPE_MOD)+1)/SECT_PIPE_MOD )
    ) u_delay_c_rip_gated (
       .clk     ( clk   ),
       .rst_n   ( rst_n ),
       .i_in    ( sect_o_c_rip[sect_i].g   ),
       .i_in_v  ( sect_o_c_rip[sect_i].v   ),
       .o_out   ( sect_i_c_rip[sect_i+1].g ),
       .o_out_v ( sect_i_c_rip[sect_i+1].v )
    );  

    // Gate the datapath read bus by chunk.
    for ( genvar chunk_i=0; chunk_i<N_CHUNKS; chunk_i++) begin: inst_chunk
      hlp_delay_v  #(
        .WIDTH ( $bits(sram_data_t)), 
        .STAGES( ((sect_i%SECT_PIPE_MOD)+1)/SECT_PIPE_MOD )
      ) u_delay_r_bus (
         .clk     ( clk   ),
         .rst_n   ( rst_n ),
         .i_in    ( sect_o_r_bus[sect_i].d[chunk_i]   ),
         .i_in_v  ( sect_o_r_bus[sect_i].v[chunk_i]   ),
         .o_out   ( sect_i_r_bus[sect_i+1].d[chunk_i] ),
         .o_out_v ( sect_i_r_bus[sect_i+1].v[chunk_i] )
      );  

    // Gate the datapath write bus by chunk. No need for flops at end of ripple.
      hlp_delay_v  #(
        .WIDTH ( $bits(sram_data_t)), 
        .STAGES( (((sect_i%SECT_PIPE_MOD)+1)/SECT_PIPE_MOD) & 
                 (sect_i < (N_BANKS-1)) )
      ) u_delay_w_bus (
         .clk     ( clk   ),
         .rst_n   ( rst_n ),
         .i_in    ( sect_o_w_bus[sect_i].d[chunk_i]   ),
         .i_in_v  ( sect_o_w_bus[sect_i].v[chunk_i]   ),
         .o_out   ( sect_i_w_bus[sect_i+1].d[chunk_i] ),
         .o_out_v ( sect_i_w_bus[sect_i+1].v[chunk_i] )
      );  
    end
  end
endgenerate

imn_rpl_frwd_t  rpl_frwd;
imn_rpl_bkwd_t  rpl_bkwd;

logic  nc_rj_o_r_mgmt_ll_e;  // lintra s-70036 
logic  ll_on;

  hlp_rpl_junc #(
    .MGMT_JUNC_N_MGMT_TOKENS( PM_MGMT_JUNC_N_MGMT_TOKENS ),
    .NUM_INTR               ( PM_INTR_BITS               )
  ) u_rpl_junc (
    .clk                       ( clk   ),
    .rst_n                     ( rst_n ),

    .i_strap_base_addr         ( PM_STRAP_MGMT_ADDR_ID_JUNC_1 ),
    .i_strap_base_mask         ( PM_MGMT_JUNC_ADDR_MASK_1     ),
    .i_strap_int_id            ( PM_STRAP_INT_ID              ),

    .i_fuse_rst_b              ( i_broadcast.fuse_rst_b ),

    .i_mem_init_done_frm_ip    ( 1'b1 ),
    .i_func_init_done_frm_ip   ( 1'b1 ),
    .i_int_frm_ip              ( co_o_int_dly ),

    .o_l_mgmt_ll               ( ci_i_mgmt   ),
    .o_l_mgmt_ll_v             ( ci_i_mgmt_v ),
    .i_l_mgmt_ll_e             ( 1'b1        ),
                                       
    .i_r_mgmt_ll               ( ci_o_mgmt   ),
    .i_r_mgmt_ll_v             ( ci_o_mgmt_v ),
    .o_r_mgmt_ll_e             ( nc_rj_o_r_mgmt_ll_e ),
 
  // Ripple Channel in
    .i_rpl_frwd                ( i_rpl_frwd ),
    .o_rpl_bkwd                ( o_rpl_bkwd ),

  // Ripple Channel out
    .o_rpl_frwd                ( rpl_frwd ),
    .i_rpl_bkwd                ( rpl_bkwd ),

    .i_disable                 ( 1'b0 ),                
    .o_clk_on                  ( ll_on )
  );

imn_rpl_frwd_t  mgmt_rpl_frwd;
imn_rpl_bkwd_t  mgmt_rpl_bkwd;

  hlp_rpl_junc_pipe  #( 
    .STAGES( N_RPL_JUNC_PIPE ) 
  )  u_rpl_junc_pipe  (
    .clk          ( clk ),
    .rst_n        ( i_broadcast.fuse_rst_b ),
 
    .i_l_rpl_frwd ( rpl_frwd ),
    .o_l_rpl_bkwd ( rpl_bkwd ),

    .i_r_rpl_bkwd ( mgmt_rpl_bkwd ),
    .o_r_rpl_frwd ( mgmt_rpl_frwd )
  );  

logic  nc_o_ll_on, nc_mj_o_r_mgmt_ll_e;  // lintra s-70036

  hlp_mgmt_junc  #(
    .N_MGMT_TOKENS( PM_MGMT_JUNC_N_MGMT_TOKENS )
  ) u_mgmt_junc_2 (
      .clk                      ( clk     ),
      .rst_n                    ( rst_n ),

      .i_strap_base_addr        ( PM_STRAP_MGMT_ADDR_ID_JUNC_2 ),  
      .i_strap_base_mask        ( PM_MGMT_JUNC_ADDR_MASK_2     ),

      .i_ll_on_req              ( ll_on      ),
      .o_ll_on_ack              ( nc_o_ll_on ),

      .i_l_mgmt_rpl             ( mgmt_rpl_frwd.mgmt   ),
      .i_l_mgmt_rpl_v           ( mgmt_rpl_frwd.mgmt_v ),
      .o_l_mgmt_rpl_e           ( mgmt_rpl_bkwd.mgmt_e ),

      .o_r_mgmt_rpl             ( o_rpl_frwd.mgmt   ),
      .o_r_mgmt_rpl_v           ( o_rpl_frwd.mgmt_v ),
      .i_r_mgmt_rpl_e           ( i_rpl_bkwd.mgmt_e ),

      .o_l_mgmt_ll              ( co_i_mgmt   ),
      .o_l_mgmt_ll_v            ( co_i_mgmt_v ),
      .i_l_mgmt_ll_e            ( 1'b1        ),
                                
      .i_r_mgmt_ll              ( co_o_mgmt   ),
      .i_r_mgmt_ll_v            ( co_o_mgmt_v ),
      .o_r_mgmt_ll_e            ( nc_mj_o_r_mgmt_ll_e )
  );

  assign o_rpl_frwd.mgmt_meta      = mgmt_rpl_frwd.mgmt_meta;
  assign o_rpl_frwd.curr_ptot      = mgmt_rpl_frwd.curr_ptot; 
  assign o_rpl_frwd.intr           = mgmt_rpl_frwd.intr;
  assign o_rpl_frwd.intr_v         = mgmt_rpl_frwd.intr_v;
  assign o_rpl_frwd.intr_slot      = mgmt_rpl_frwd.intr_slot;
  assign o_rpl_frwd.mem_init_done  = mgmt_rpl_frwd.mem_init_done;
  assign o_rpl_frwd.func_init_done = mgmt_rpl_frwd.func_init_done;

hlp_delay  #(
  .WIDTH ( 1          ), 
  .STAGES( N_INT_PIPE )
)  u_delay_int  (
   .clk    ( clk   ),
   .rst_n  ( rst_n ),

   .i_in   ( co_o_int     ),
   .o_out  ( co_o_int_dly )
);  

// Asserts
// assert property 
  
`HLP_INCLUDE_SVA_MON( "hlp_pm_mon.sva" )

// VCS coverage off
  `HLP_INCLUDE_SVA_COV( "hlp_pm_cov.sva" )
// VCS coverage on


endmodule

