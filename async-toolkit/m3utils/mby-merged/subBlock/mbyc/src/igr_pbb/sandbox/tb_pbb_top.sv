// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

//`timescale 1ns/100ps
//  `include "mby_rp_inq_pkg.sv"

module TB_PBB_TOP
  import shared_pkg::*, mby_igr_pkg::*, mby_rx_metadata_pkg::*, mby_rx_pb_pkg::* ;
(

);
  `include "rtlgen_pkg_mby_rx_pb_map.vh"
  
  // Clock.
  logic                               cclk;
  // Unsynchronized warm reset.
  logic                               sreset= '0;
  
  
  logic [1:0]                  grp_a_rx_port_num;
  logic [1:0]                  grp_b_rx_port_num;
  logic [1:0]                  grp_c_rx_port_num;
  logic [1:0]                  grp_d_rx_port_num;
  
  logic [7:0]                grp_a_rx_data_valid = '0;
  logic [7:0]                grp_b_rx_data_valid = '0;
  logic [7:0]                grp_c_rx_data_valid = '0;
  logic [7:0]                grp_d_rx_data_valid = '0;
  
  epl_md_t                     grp_a_rx_metadata = '0;
  epl_md_t                     grp_b_rx_metadata = '0;
  epl_md_t                     grp_c_rx_metadata = '0;
  epl_md_t                     grp_d_rx_metadata = '0;

  logic [EPL_IGR_TS_W-1:0]                   grp_a_rx_time_stamp = '0;
  logic [EPL_IGR_TS_W-1:0]                   grp_b_rx_time_stamp = '0;
  logic [EPL_IGR_TS_W-1:0]                   grp_c_rx_time_stamp = '0;
  logic [EPL_IGR_TS_W-1:0]                   grp_d_rx_time_stamp = '0;
   
  logic [0:7][63:0]      grp_a_rx_data = '0;
  logic [0:7][63:0]      grp_b_rx_data = '0;
  logic [0:7][63:0]      grp_c_rx_data = '0;
  logic [0:7][63:0]      grp_d_rx_data = '0;
  
 
  logic [3:0]                  grp_a_tx_pfc_xoff;
  logic [3:0]                  grp_b_tx_pfc_xoff;
  logic [3:0]                  grp_c_tx_pfc_xoff;
  logic [3:0]                  grp_d_tx_pfc_xoff;
  logic                     grp_a_tx_pfc_tc_sync;
  logic                     grp_b_tx_pfc_tc_sync;
  logic                     grp_c_tx_pfc_tc_sync;
  logic                     grp_d_tx_pfc_tc_sync;
  
  pb_port_cfg_t pb0_cfg = {2'h0,2'h0,2'h0,2'h3};  //default port0 400G
  pb_port_cfg_t pb1_cfg = {2'h0,2'h0,2'h0,2'h3};
  pb_port_cfg_t pb2_cfg = {2'h0,2'h0,2'h0,2'h3};
  pb_port_cfg_t pb3_cfg = {2'h0,2'h0,2'h0,2'h3};
 
  mby_rx_pb_cr_req_t csr_req = '0;

  logic [3:0] i_pb0_rd = 4'b0001;
  logic [3:0] i_pb1_rd = 4'b0001;
  logic [3:0] i_pb2_rd = 4'b0001;
  logic [3:0] i_pb3_rd = 4'b0001;
  
  logic [1:0] port_num_cnt;
  
initial
begin
cclk = 1'b0;
forever
 #1 cclk = ~cclk;
end

always @(posedge cclk) port_num_cnt <= (sreset)? '0: port_num_cnt+1;

always_comb begin
  case ({pb0_cfg.p3,pb0_cfg.p2,pb0_cfg.p1,pb0_cfg.p0}) inside
    8'h0_0_0_0: grp_a_rx_port_num <= '0;
    8'h?_?_?_3: grp_a_rx_port_num <= '0;
    8'h0_0_0_1: grp_a_rx_port_num <= port_num_cnt;
    8'h0_0_1_1: grp_a_rx_port_num <= port_num_cnt;
    8'h0_1_1_1: grp_a_rx_port_num <= port_num_cnt;
    8'h1_1_1_1: grp_a_rx_port_num <= port_num_cnt;
    8'h0_0_1_0: grp_a_rx_port_num <= port_num_cnt;
    8'h0_1_1_0: grp_a_rx_port_num <= port_num_cnt;
    8'h1_1_1_0: grp_a_rx_port_num <= port_num_cnt;
    8'h0_1_0_0: grp_a_rx_port_num <= port_num_cnt;
    8'h1_1_0_0: grp_a_rx_port_num <= port_num_cnt;
    8'h1_0_0_0: grp_a_rx_port_num <= port_num_cnt;
    8'h0_0_?_2: grp_a_rx_port_num <= '0;
    8'h0_2_?_2: grp_a_rx_port_num <= ((port_num_cnt == 1) || (port_num_cnt == 3))? 2: port_num_cnt;
    default: grp_a_rx_port_num <= port_num_cnt;
  endcase
  
  case ({pb1_cfg.p3,pb0_cfg.p2,pb0_cfg.p1,pb0_cfg.p0}) inside
    8'h0_0_0_0: grp_b_rx_port_num <= '0;
    8'h?_?_?_3: grp_b_rx_port_num <= '0;
    8'h0_0_0_1: grp_b_rx_port_num <= port_num_cnt;
    8'h0_0_1_1: grp_b_rx_port_num <= port_num_cnt;
    8'h0_1_1_1: grp_b_rx_port_num <= port_num_cnt;
    8'h1_1_1_1: grp_b_rx_port_num <= port_num_cnt;
    8'h0_0_1_0: grp_b_rx_port_num <= port_num_cnt;
    8'h0_1_1_0: grp_b_rx_port_num <= port_num_cnt;
    8'h1_1_1_0: grp_b_rx_port_num <= port_num_cnt;
    8'h0_1_0_0: grp_b_rx_port_num <= port_num_cnt;
    8'h1_1_0_0: grp_b_rx_port_num <= port_num_cnt;
    8'h1_0_0_0: grp_b_rx_port_num <= port_num_cnt;
    8'h0_0_?_2: grp_b_rx_port_num <= '0;
    8'h0_2_?_2: grp_b_rx_port_num <= ((port_num_cnt == 1) || (port_num_cnt == 3))? 2: port_num_cnt;
    default: grp_b_rx_port_num <= port_num_cnt;
  endcase

    case ({pb2_cfg.p3,pb2_cfg.p2,pb2_cfg.p1,pb2_cfg.p0}) inside
    8'h0_0_0_0: grp_c_rx_port_num <= '0;
    8'h?_?_?_3: grp_c_rx_port_num <= '0;
    8'h0_0_0_1: grp_c_rx_port_num <= port_num_cnt;
    8'h0_0_1_1: grp_c_rx_port_num <= port_num_cnt;
    8'h0_1_1_1: grp_c_rx_port_num <= port_num_cnt;
    8'h1_1_1_1: grp_c_rx_port_num <= port_num_cnt;
    8'h0_0_1_0: grp_c_rx_port_num <= port_num_cnt;
    8'h0_1_1_0: grp_c_rx_port_num <= port_num_cnt;
    8'h1_1_1_0: grp_c_rx_port_num <= port_num_cnt;
    8'h0_1_0_0: grp_c_rx_port_num <= port_num_cnt;
    8'h1_1_0_0: grp_c_rx_port_num <= port_num_cnt;
    8'h1_0_0_0: grp_c_rx_port_num <= port_num_cnt;
    8'h0_0_?_2: grp_c_rx_port_num <= '0;
    8'h0_2_?_2: grp_c_rx_port_num <= ((port_num_cnt == 1) || (port_num_cnt == 3))? 2: port_num_cnt;
    default: grp_c_rx_port_num <= port_num_cnt;
  endcase

      case ({pb3_cfg.p3,pb3_cfg.p2,pb3_cfg.p1,pb3_cfg.p0}) inside
    8'h0_0_0_0: grp_d_rx_port_num <= '0;
    8'h?_?_?_3: grp_d_rx_port_num <= '0;
    8'h0_0_0_1: grp_d_rx_port_num <= port_num_cnt;
    8'h0_0_1_1: grp_d_rx_port_num <= port_num_cnt;
    8'h0_1_1_1: grp_d_rx_port_num <= port_num_cnt;
    8'h1_1_1_1: grp_d_rx_port_num <= port_num_cnt;
    8'h0_0_1_0: grp_d_rx_port_num <= port_num_cnt;
    8'h0_1_1_0: grp_d_rx_port_num <= port_num_cnt;
    8'h1_1_1_0: grp_d_rx_port_num <= port_num_cnt;
    8'h0_1_0_0: grp_d_rx_port_num <= port_num_cnt;
    8'h1_1_0_0: grp_d_rx_port_num <= port_num_cnt;
    8'h1_0_0_0: grp_d_rx_port_num <= port_num_cnt;
    8'h0_0_?_2: grp_d_rx_port_num <= '0;
    8'h0_2_?_2: grp_d_rx_port_num <= ((port_num_cnt == 1) || (port_num_cnt == 3))? 2: port_num_cnt;
    default: grp_d_rx_port_num <= port_num_cnt;
  endcase   
end
    
 
  `include "mby_igr_pbb_mem_if_incl.sv"

mby_igr_pbb_gen_mem PBB_MEM(
  .i_reset(sreset), 
  .igr_pbb_pb0_pd0_0_if, 
  .igr_pbb_pb0_pd0_1_if, 
  .igr_pbb_pb0_pd0_2_if, 
  .igr_pbb_pb0_pd0_3_if, 
  .igr_pbb_pb0_pd0_4_if, 
  .igr_pbb_pb0_pd0_5_if, 
  .igr_pbb_pb0_pd0_6_if, 
  .igr_pbb_pb0_pd0_7_if, 
  .igr_pbb_pb0_pd1_0_if, 
  .igr_pbb_pb0_pd1_1_if, 
  .igr_pbb_pb0_pd1_2_if, 
  .igr_pbb_pb0_pd1_3_if, 
  .igr_pbb_pb0_pd1_4_if, 
  .igr_pbb_pb0_pd1_5_if, 
  .igr_pbb_pb0_pd1_6_if, 
  .igr_pbb_pb0_pd1_7_if, 
  .igr_pbb_pb0_pd2_0_if, 
  .igr_pbb_pb0_pd2_1_if, 
  .igr_pbb_pb0_pd2_2_if, 
  .igr_pbb_pb0_pd2_3_if, 
  .igr_pbb_pb0_pd2_4_if, 
  .igr_pbb_pb0_pd2_5_if, 
  .igr_pbb_pb0_pd2_6_if, 
  .igr_pbb_pb0_pd2_7_if, 
  .igr_pbb_pb0_pd3_0_if, 
  .igr_pbb_pb0_pd3_1_if, 
  .igr_pbb_pb0_pd3_2_if, 
  .igr_pbb_pb0_pd3_3_if, 
  .igr_pbb_pb0_pd3_4_if, 
  .igr_pbb_pb0_pd3_5_if, 
  .igr_pbb_pb0_pd3_6_if, 
  .igr_pbb_pb0_pd3_7_if, 
  .igr_pbb_pb1_pd0_0_if, 
  .igr_pbb_pb1_pd0_1_if, 
  .igr_pbb_pb1_pd0_2_if, 
  .igr_pbb_pb1_pd0_3_if, 
  .igr_pbb_pb1_pd0_4_if, 
  .igr_pbb_pb1_pd0_5_if, 
  .igr_pbb_pb1_pd0_6_if, 
  .igr_pbb_pb1_pd0_7_if, 
  .igr_pbb_pb1_pd1_0_if, 
  .igr_pbb_pb1_pd1_1_if, 
  .igr_pbb_pb1_pd1_2_if, 
  .igr_pbb_pb1_pd1_3_if, 
  .igr_pbb_pb1_pd1_4_if, 
  .igr_pbb_pb1_pd1_5_if, 
  .igr_pbb_pb1_pd1_6_if, 
  .igr_pbb_pb1_pd1_7_if, 
  .igr_pbb_pb1_pd2_0_if, 
  .igr_pbb_pb1_pd2_1_if, 
  .igr_pbb_pb1_pd2_2_if, 
  .igr_pbb_pb1_pd2_3_if, 
  .igr_pbb_pb1_pd2_4_if, 
  .igr_pbb_pb1_pd2_5_if, 
  .igr_pbb_pb1_pd2_6_if, 
  .igr_pbb_pb1_pd2_7_if, 
  .igr_pbb_pb1_pd3_0_if, 
  .igr_pbb_pb1_pd3_1_if, 
  .igr_pbb_pb1_pd3_2_if, 
  .igr_pbb_pb1_pd3_3_if, 
  .igr_pbb_pb1_pd3_4_if, 
  .igr_pbb_pb1_pd3_5_if, 
  .igr_pbb_pb1_pd3_6_if, 
  .igr_pbb_pb1_pd3_7_if, 
  .igr_pbb_pb2_pd0_0_if, 
  .igr_pbb_pb2_pd0_1_if, 
  .igr_pbb_pb2_pd0_2_if, 
  .igr_pbb_pb2_pd0_3_if, 
  .igr_pbb_pb2_pd0_4_if, 
  .igr_pbb_pb2_pd0_5_if, 
  .igr_pbb_pb2_pd0_6_if, 
  .igr_pbb_pb2_pd0_7_if, 
  .igr_pbb_pb2_pd1_0_if, 
  .igr_pbb_pb2_pd1_1_if, 
  .igr_pbb_pb2_pd1_2_if, 
  .igr_pbb_pb2_pd1_3_if, 
  .igr_pbb_pb2_pd1_4_if, 
  .igr_pbb_pb2_pd1_5_if, 
  .igr_pbb_pb2_pd1_6_if, 
  .igr_pbb_pb2_pd1_7_if, 
  .igr_pbb_pb2_pd2_0_if, 
  .igr_pbb_pb2_pd2_1_if, 
  .igr_pbb_pb2_pd2_2_if, 
  .igr_pbb_pb2_pd2_3_if, 
  .igr_pbb_pb2_pd2_4_if, 
  .igr_pbb_pb2_pd2_5_if, 
  .igr_pbb_pb2_pd2_6_if, 
  .igr_pbb_pb2_pd2_7_if, 
  .igr_pbb_pb2_pd3_0_if, 
  .igr_pbb_pb2_pd3_1_if, 
  .igr_pbb_pb2_pd3_2_if, 
  .igr_pbb_pb2_pd3_3_if, 
  .igr_pbb_pb2_pd3_4_if, 
  .igr_pbb_pb2_pd3_5_if, 
  .igr_pbb_pb2_pd3_6_if, 
  .igr_pbb_pb2_pd3_7_if, 
  .igr_pbb_pb3_pd0_0_if, 
  .igr_pbb_pb3_pd0_1_if, 
  .igr_pbb_pb3_pd0_2_if, 
  .igr_pbb_pb3_pd0_3_if, 
  .igr_pbb_pb3_pd0_4_if, 
  .igr_pbb_pb3_pd0_5_if, 
  .igr_pbb_pb3_pd0_6_if, 
  .igr_pbb_pb3_pd0_7_if, 
  .igr_pbb_pb3_pd1_0_if, 
  .igr_pbb_pb3_pd1_1_if, 
  .igr_pbb_pb3_pd1_2_if, 
  .igr_pbb_pb3_pd1_3_if, 
  .igr_pbb_pb3_pd1_4_if, 
  .igr_pbb_pb3_pd1_5_if, 
  .igr_pbb_pb3_pd1_6_if, 
  .igr_pbb_pb3_pd1_7_if, 
  .igr_pbb_pb3_pd2_0_if, 
  .igr_pbb_pb3_pd2_1_if, 
  .igr_pbb_pb3_pd2_2_if, 
  .igr_pbb_pb3_pd2_3_if, 
  .igr_pbb_pb3_pd2_4_if, 
  .igr_pbb_pb3_pd2_5_if, 
  .igr_pbb_pb3_pd2_6_if, 
  .igr_pbb_pb3_pd2_7_if, 
  .igr_pbb_pb3_pd3_0_if, 
  .igr_pbb_pb3_pd3_1_if, 
  .igr_pbb_pb3_pd3_2_if, 
  .igr_pbb_pb3_pd3_3_if, 
  .igr_pbb_pb3_pd3_4_if, 
  .igr_pbb_pb3_pd3_5_if, 
  .igr_pbb_pb3_pd3_6_if, 
  .igr_pbb_pb3_pd3_7_if, 
  .igr_pbb_pb0_md0_if, 
  .igr_pbb_pb0_md1_if, 
  .igr_pbb_pb0_md2_if, 
  .igr_pbb_pb0_md3_if, 
  .igr_pbb_pb1_md0_if, 
  .igr_pbb_pb1_md1_if, 
  .igr_pbb_pb1_md2_if, 
  .igr_pbb_pb1_md3_if, 
  .igr_pbb_pb2_md0_if, 
  .igr_pbb_pb2_md1_if, 
  .igr_pbb_pb2_md2_if, 
  .igr_pbb_pb2_md3_if, 
  .igr_pbb_pb3_md0_if, 
  .igr_pbb_pb3_md1_if, 
  .igr_pbb_pb3_md2_if, 
  .igr_pbb_pb3_md3_if, 
  .igr_pbb_vp_pd_0_if, 
  .igr_pbb_vp_pd_1_if, 
  .igr_pbb_vp_pd_2_if, 
  .igr_pbb_vp_pd_3_if, 
  .igr_pbb_vp_pd_4_if, 
  .igr_pbb_vp_pd_5_if, 
  .igr_pbb_vp_pd_6_if, 
  .igr_pbb_vp_pd_7_if, 
  .igr_pbb_vp_md_0_if, 
  .igr_pbb_vp_md_1_if, 
  .igr_pbb_vp_md_2_if, 
  .igr_pbb_vp_md_3_if, 
  .igr_pbb_vp_md_4_if, 
  .igr_pbb_vp_md_5_if, 
  .igr_pbb_vp_md_6_if, 
  .igr_pbb_vp_md_7_if,

//Input List
  .cclk(cclk),                                     
  .fary_enblfloat_sram('0),                      
  .fary_ensleep_sram('0),                        
  .fary_ffuse_data_misc_rf('0),                  
  .fary_ffuse_data_misc_sram('0),
  .fary_ffuse_data_red_sram('0),
  .fary_fwen_sram('0),                           
  .fary_pwren_b_rf('0),                          
  .fary_pwren_b_sram('0),                        
  .fary_stm_enable('0),                          
  .fary_stm_hilo('0),                            
  .fary_wakeup_sram('0),                         
  .fdfx_lbist_test_mode('0),                     
  .fscan_byprst_b(1'b1),                           
  .fscan_mode('0),                               
  .fscan_ram_awt_mode('0),                       
  .fscan_ram_awt_ren('0),                        
  .fscan_ram_awt_wen('0),                        
  .fscan_ram_bypsel('0),                         
  .fscan_ram_init_en('0),                        
  .fscan_ram_init_val('0),                       
  .fscan_ram_odis_b('0),                         
  .fscan_ram_rddis_b('0),                        
  .fscan_ram_wrdis_b('0),                        
  .fscan_rstbypen('0),                           

//Output List
  .aary_pwren_b_rf(),                          
  .aary_pwren_b_sram()                         
);


mby_igr_pbb
  PBB(
  // Clock.
  .cclk(cclk),
  // Unsynchronized warm reset.
  .i_sreset(sreset),
      
// EPL I/O from MBY FS Dataplane Interface signals.
  //EPL0
  .grp_a_rx_port_num(grp_a_rx_port_num),
  .grp_b_rx_port_num(grp_b_rx_port_num),
  .grp_c_rx_port_num(grp_c_rx_port_num),
  .grp_d_rx_port_num(grp_d_rx_port_num),
  
  .grp_a_rx_data_valid(grp_a_rx_data_valid),
  .grp_b_rx_data_valid(grp_b_rx_data_valid),
  .grp_c_rx_data_valid(grp_c_rx_data_valid),
  .grp_d_rx_data_valid(grp_d_rx_data_valid),
  
  .grp_a_rx_metadata(grp_a_rx_metadata),
  .grp_b_rx_metadata(grp_b_rx_metadata),
  .grp_c_rx_metadata(grp_c_rx_metadata),
  .grp_d_rx_metadata(grp_d_rx_metadata),

  .grp_a_rx_time_stamp(grp_a_rx_time_stamp),
  .grp_b_rx_time_stamp(grp_b_rx_time_stamp),
  .grp_c_rx_time_stamp(grp_c_rx_time_stamp),
  .grp_d_rx_time_stamp(grp_d_rx_time_stamp),
  
  .grp_a_rx_data(grp_a_rx_data),
  .grp_b_rx_data(grp_b_rx_data),
  .grp_c_rx_data(grp_c_rx_data),
  .grp_d_rx_data(grp_d_rx_data),

  .grp_a_tx_pfc_xoff(grp_a_tx_pfc_xoff),
  .grp_b_tx_pfc_xoff(grp_b_tx_pfc_xoff),
  .grp_c_tx_pfc_xoff(grp_c_tx_pfc_xoff),
  .grp_d_tx_pfc_xoff(grp_d_tx_pfc_xoff),
  
  .grp_a_tx_pfc_tc_sync(grp_a_tx_pfc_tc_sync),
  .grp_b_tx_pfc_tc_sync(grp_b_tx_pfc_tc_sync),
  .grp_c_tx_pfc_tc_sync(grp_c_tx_pfc_tc_sync),
  .grp_d_tx_pfc_tc_sync(grp_d_tx_pfc_tc_sync),
  
  .igr_pbb_pb0_pd0_0_if, 
  .igr_pbb_pb0_pd0_1_if, 
  .igr_pbb_pb0_pd0_2_if, 
  .igr_pbb_pb0_pd0_3_if, 
  .igr_pbb_pb0_pd0_4_if, 
  .igr_pbb_pb0_pd0_5_if, 
  .igr_pbb_pb0_pd0_6_if, 
  .igr_pbb_pb0_pd0_7_if, 
  .igr_pbb_pb0_pd1_0_if, 
  .igr_pbb_pb0_pd1_1_if, 
  .igr_pbb_pb0_pd1_2_if, 
  .igr_pbb_pb0_pd1_3_if, 
  .igr_pbb_pb0_pd1_4_if, 
  .igr_pbb_pb0_pd1_5_if, 
  .igr_pbb_pb0_pd1_6_if, 
  .igr_pbb_pb0_pd1_7_if, 
  .igr_pbb_pb0_pd2_0_if, 
  .igr_pbb_pb0_pd2_1_if, 
  .igr_pbb_pb0_pd2_2_if, 
  .igr_pbb_pb0_pd2_3_if, 
  .igr_pbb_pb0_pd2_4_if, 
  .igr_pbb_pb0_pd2_5_if, 
  .igr_pbb_pb0_pd2_6_if, 
  .igr_pbb_pb0_pd2_7_if, 
  .igr_pbb_pb0_pd3_0_if, 
  .igr_pbb_pb0_pd3_1_if, 
  .igr_pbb_pb0_pd3_2_if, 
  .igr_pbb_pb0_pd3_3_if, 
  .igr_pbb_pb0_pd3_4_if, 
  .igr_pbb_pb0_pd3_5_if, 
  .igr_pbb_pb0_pd3_6_if, 
  .igr_pbb_pb0_pd3_7_if, 
  .igr_pbb_pb1_pd0_0_if, 
  .igr_pbb_pb1_pd0_1_if, 
  .igr_pbb_pb1_pd0_2_if, 
  .igr_pbb_pb1_pd0_3_if, 
  .igr_pbb_pb1_pd0_4_if, 
  .igr_pbb_pb1_pd0_5_if, 
  .igr_pbb_pb1_pd0_6_if, 
  .igr_pbb_pb1_pd0_7_if, 
  .igr_pbb_pb1_pd1_0_if, 
  .igr_pbb_pb1_pd1_1_if, 
  .igr_pbb_pb1_pd1_2_if, 
  .igr_pbb_pb1_pd1_3_if, 
  .igr_pbb_pb1_pd1_4_if, 
  .igr_pbb_pb1_pd1_5_if, 
  .igr_pbb_pb1_pd1_6_if, 
  .igr_pbb_pb1_pd1_7_if, 
  .igr_pbb_pb1_pd2_0_if, 
  .igr_pbb_pb1_pd2_1_if, 
  .igr_pbb_pb1_pd2_2_if, 
  .igr_pbb_pb1_pd2_3_if, 
  .igr_pbb_pb1_pd2_4_if, 
  .igr_pbb_pb1_pd2_5_if, 
  .igr_pbb_pb1_pd2_6_if, 
  .igr_pbb_pb1_pd2_7_if, 
  .igr_pbb_pb1_pd3_0_if, 
  .igr_pbb_pb1_pd3_1_if, 
  .igr_pbb_pb1_pd3_2_if, 
  .igr_pbb_pb1_pd3_3_if, 
  .igr_pbb_pb1_pd3_4_if, 
  .igr_pbb_pb1_pd3_5_if, 
  .igr_pbb_pb1_pd3_6_if, 
  .igr_pbb_pb1_pd3_7_if, 
  .igr_pbb_pb2_pd0_0_if, 
  .igr_pbb_pb2_pd0_1_if, 
  .igr_pbb_pb2_pd0_2_if, 
  .igr_pbb_pb2_pd0_3_if, 
  .igr_pbb_pb2_pd0_4_if, 
  .igr_pbb_pb2_pd0_5_if, 
  .igr_pbb_pb2_pd0_6_if, 
  .igr_pbb_pb2_pd0_7_if, 
  .igr_pbb_pb2_pd1_0_if, 
  .igr_pbb_pb2_pd1_1_if, 
  .igr_pbb_pb2_pd1_2_if, 
  .igr_pbb_pb2_pd1_3_if, 
  .igr_pbb_pb2_pd1_4_if, 
  .igr_pbb_pb2_pd1_5_if, 
  .igr_pbb_pb2_pd1_6_if, 
  .igr_pbb_pb2_pd1_7_if, 
  .igr_pbb_pb2_pd2_0_if, 
  .igr_pbb_pb2_pd2_1_if, 
  .igr_pbb_pb2_pd2_2_if, 
  .igr_pbb_pb2_pd2_3_if, 
  .igr_pbb_pb2_pd2_4_if, 
  .igr_pbb_pb2_pd2_5_if, 
  .igr_pbb_pb2_pd2_6_if, 
  .igr_pbb_pb2_pd2_7_if, 
  .igr_pbb_pb2_pd3_0_if, 
  .igr_pbb_pb2_pd3_1_if, 
  .igr_pbb_pb2_pd3_2_if, 
  .igr_pbb_pb2_pd3_3_if, 
  .igr_pbb_pb2_pd3_4_if, 
  .igr_pbb_pb2_pd3_5_if, 
  .igr_pbb_pb2_pd3_6_if, 
  .igr_pbb_pb2_pd3_7_if, 
  .igr_pbb_pb3_pd0_0_if, 
  .igr_pbb_pb3_pd0_1_if, 
  .igr_pbb_pb3_pd0_2_if, 
  .igr_pbb_pb3_pd0_3_if, 
  .igr_pbb_pb3_pd0_4_if, 
  .igr_pbb_pb3_pd0_5_if, 
  .igr_pbb_pb3_pd0_6_if, 
  .igr_pbb_pb3_pd0_7_if, 
  .igr_pbb_pb3_pd1_0_if, 
  .igr_pbb_pb3_pd1_1_if, 
  .igr_pbb_pb3_pd1_2_if, 
  .igr_pbb_pb3_pd1_3_if, 
  .igr_pbb_pb3_pd1_4_if, 
  .igr_pbb_pb3_pd1_5_if, 
  .igr_pbb_pb3_pd1_6_if, 
  .igr_pbb_pb3_pd1_7_if, 
  .igr_pbb_pb3_pd2_0_if, 
  .igr_pbb_pb3_pd2_1_if, 
  .igr_pbb_pb3_pd2_2_if, 
  .igr_pbb_pb3_pd2_3_if, 
  .igr_pbb_pb3_pd2_4_if, 
  .igr_pbb_pb3_pd2_5_if, 
  .igr_pbb_pb3_pd2_6_if, 
  .igr_pbb_pb3_pd2_7_if, 
  .igr_pbb_pb3_pd3_0_if, 
  .igr_pbb_pb3_pd3_1_if, 
  .igr_pbb_pb3_pd3_2_if, 
  .igr_pbb_pb3_pd3_3_if, 
  .igr_pbb_pb3_pd3_4_if, 
  .igr_pbb_pb3_pd3_5_if, 
  .igr_pbb_pb3_pd3_6_if, 
  .igr_pbb_pb3_pd3_7_if, 
  .igr_pbb_pb0_md0_if, 
  .igr_pbb_pb0_md1_if, 
  .igr_pbb_pb0_md2_if, 
  .igr_pbb_pb0_md3_if, 
  .igr_pbb_pb1_md0_if, 
  .igr_pbb_pb1_md1_if, 
  .igr_pbb_pb1_md2_if, 
  .igr_pbb_pb1_md3_if, 
  .igr_pbb_pb2_md0_if, 
  .igr_pbb_pb2_md1_if, 
  .igr_pbb_pb2_md2_if, 
  .igr_pbb_pb2_md3_if, 
  .igr_pbb_pb3_md0_if, 
  .igr_pbb_pb3_md1_if, 
  .igr_pbb_pb3_md2_if, 
  .igr_pbb_pb3_md3_if, 
  .igr_pbb_vp_pd_0_if, 
  .igr_pbb_vp_pd_1_if, 
  .igr_pbb_vp_pd_2_if, 
  .igr_pbb_vp_pd_3_if, 
  .igr_pbb_vp_pd_4_if, 
  .igr_pbb_vp_pd_5_if, 
  .igr_pbb_vp_pd_6_if, 
  .igr_pbb_vp_pd_7_if, 
  .igr_pbb_vp_md_0_if, 
  .igr_pbb_vp_md_1_if, 
  .igr_pbb_vp_md_2_if, 
  .igr_pbb_vp_md_3_if, 
  .igr_pbb_vp_md_4_if, 
  .igr_pbb_vp_md_5_if, 
  .igr_pbb_vp_md_6_if, 
  .igr_pbb_vp_md_7_if,
  
//port configuration seting per EPL port from rx_pb_port_cfg_r
//  .i_pb0_cfg(i_pb0_cfg),
//  .i_pb1_cfg(i_pb1_cfg),
//  .i_pb2_cfg(i_pb2_cfg),
//  .i_pb3_cfg(i_pb3_cfg),
 //   .csr_req(csr_req),
//pre Rx_ppe PB read or flow control per EPL(4-ports) 
  .i_pb0_rd(i_pb0_rd),
  .i_pb1_rd(i_pb1_rd),
  .i_pb2_rd(i_pb2_rd),
  .i_pb3_rd(i_pb3_rd),
  
  .o_shim_pb0_data_p0(),
  .o_shim_pb0_data_p1(),
  .o_shim_pb0_data_p2(),
  .o_shim_pb0_data_p3(),
  .o_shim_pb0_md_p0(),
  .o_shim_pb0_md_p1(),
  .o_shim_pb0_md_p2(),
  .o_shim_pb0_md_p3(),
  .o_shim_pb0_v_p0(),  
  .o_shim_pb0_v_p1(),  
  .o_shim_pb0_v_p2(),  
  .o_shim_pb0_v_p3(),

  .o_shim_pb1_data_p0(),
  .o_shim_pb1_data_p1(),
  .o_shim_pb1_data_p2(),
  .o_shim_pb1_data_p3(),
  .o_shim_pb1_md_p0(),
  .o_shim_pb1_md_p1(),
  .o_shim_pb1_md_p2(),
  .o_shim_pb1_md_p3(),
  .o_shim_pb1_v_p0(),  
  .o_shim_pb1_v_p1(),  
  .o_shim_pb1_v_p2(),  
  .o_shim_pb1_v_p3(),

  .o_shim_pb2_data_p0(),
  .o_shim_pb2_data_p1(),
  .o_shim_pb2_data_p2(),
  .o_shim_pb2_data_p3(),
  .o_shim_pb2_md_p0(),
  .o_shim_pb2_md_p1(),
  .o_shim_pb2_md_p2(),
  .o_shim_pb2_md_p3(),
  .o_shim_pb2_v_p0(),  
  .o_shim_pb2_v_p1(),  
  .o_shim_pb2_v_p2(),  
  .o_shim_pb2_v_p3(),  

  .o_shim_pb3_data_p0(),
  .o_shim_pb3_data_p1(),
  .o_shim_pb3_data_p2(),
  .o_shim_pb3_data_p3(),
  .o_shim_pb3_md_p0(),
  .o_shim_pb3_md_p1(),
  .o_shim_pb3_md_p2(),
  .o_shim_pb3_md_p3(),
  .o_shim_pb3_v_p0(),  
  .o_shim_pb3_v_p1(),  
  .o_shim_pb3_v_p2(),  
  .o_shim_pb3_v_p3(), 

  


// Global Congestion (GCM) Management interface
  .i_rx_cm_wm_out('0),    // RX watermark
  .i_rx_cm_sm_wm_out('0)  // Shared watermark
  
  // global policer IGR
//  .o_gpolring0(),
//  .o_gpolring1(),
//  .i_gpolring_update0('0),
//  .i_gpolring_update1('0)

);

`include "tasks.sv"

initial
begin
`include "tst"
  $finish;
end


endmodule
