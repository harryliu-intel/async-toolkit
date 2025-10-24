// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

//`timescale 1ns/100ps
//  `include "mby_rp_inq_pkg.sv"

module TB_PBB_TOP
  import mby_igr_pkg::*, mby_rx_metadata_pkg::*,shared_pkg::*;
(

);

  // Clock.
  logic                               cclk;
  // Unsynchronized warm reset.
  logic                               sreset= '0;
  
  
  logic [1:0]                  grp_a_rx_port_num = '0;
  logic [1:0]                  grp_b_rx_port_num = '0;
  logic [1:0]                  grp_c_rx_port_num = '0;
  logic [1:0]                  grp_d_rx_port_num = '0;
  
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

  igr_rx_ppe_if     igr_rx_ppe();
  rx_ppe_igr_if     rx_ppe_igr();
  
  igr_rx_ppe_tail_t          igr_rx_ppe_intf0_tail; // Blasted Interface igr_rx_ppe_if.igr igr_rx_ppe
  igr_rx_ppe_head_t          igr_rx_ppe_intf0_head; 
  logic                       igr_rx_ppe_intf0_ack; 
  igr_rx_ppe_tail_t          igr_rx_ppe_intf1_tail; 
  igr_rx_ppe_head_t          igr_rx_ppe_intf1_head; 
  logic                       igr_rx_ppe_intf1_ack; 

  rx_ppe_igr_t                rx_ppe_igr_intf0; // Blasted Interface rx_ppe_igr_if.igr rx_ppe_igr
  rx_ppe_igr_t                rx_ppe_igr_intf1; 

    assign igr_rx_ppe_intf0_tail = igr_rx_ppe.intf0_tail;
    assign igr_rx_ppe_intf0_head = igr_rx_ppe.intf0_head;
    assign igr_rx_ppe.intf0_ack = igr_rx_ppe_intf0_ack;
    assign igr_rx_ppe_intf1_tail = igr_rx_ppe.intf1_tail;
    assign igr_rx_ppe_intf1_head = igr_rx_ppe.intf1_head;
    assign igr_rx_ppe.intf1_ack = igr_rx_ppe_intf1_ack;

    assign rx_ppe_igr.intf0 = rx_ppe_igr_t'(rx_ppe_igr_intf0);
    assign rx_ppe_igr.intf1 = rx_ppe_igr_t'(rx_ppe_igr_intf1);

    rx_ppe_bfm  rx_ppe
      (
      .cclk                                    ( cclk ),
      .rst                                     ( sreset ),

      .igr_rx_ppe_intf0_ack                    ( igr_rx_ppe_intf0_ack ),
      .igr_rx_ppe_intf1_ack                    ( igr_rx_ppe_intf1_ack ),
      .igr_rx_ppe_intf0_tail                   ( igr_rx_ppe_intf0_tail ),
      .igr_rx_ppe_intf0_head                   ( igr_rx_ppe_intf0_head ),
      .igr_rx_ppe_intf1_tail                   ( igr_rx_ppe_intf1_tail ),
      .igr_rx_ppe_intf1_head                   ( igr_rx_ppe_intf1_head ),
      .rx_ppe_igr_intf0                        ( rx_ppe_igr_intf0 ),
      .rx_ppe_igr_intf1                        ( rx_ppe_igr_intf1 )

       );
    

initial
begin
cclk = 1'b0;
forever
 #1 cclk = ~cclk;
end

module mby_igr_pbb_gen_mem (
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
  top(
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
  .i_pb0_cfg(),
  .i_pb1_cfg(),
  .i_pb2_cfg(),
  .i_pb3_cfg(),

//pre Rx_ppe PB read or flow control per EPL(4-ports) 
  .i_pb0_rd(),
  .i_pb1_rd(),
  .i_pb2_rd(),
  .i_pb3_rd(),
  
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
  .o_shim_pb3_v_p3() 

  


// Global Congestion (GCM) Management interface
//  .i_rx_cm_wm_out('0),    // RX watermark
//  .i_rx_cm_sm_wm_out('0),  // Shared watermark
  
  // global policer IGR
//  .o_gpolring0(),
//  .o_gpolring1(),
//  .i_gpolring_update0('0),
//  .i_gpolring_update1('0)

);


initial
begin
`include "tst"
  $finish;
end


endmodule
