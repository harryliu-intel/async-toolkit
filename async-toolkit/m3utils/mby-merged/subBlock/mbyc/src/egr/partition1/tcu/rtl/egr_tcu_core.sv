//
//  Copyright 2018 - 2028 Intel Corporation All Rights Reserved.
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
// -- Author       : John Lo          
// -- Project Name : Madison Bay (MBY)
// -- Description  : 
//
// EPL uses a:d this corresponds to 0:3 in EGR.
//------------------------------------------------------------------------------


/**************************************************************
 * File Name    : egr_tcu_core.sv
 * Author Name  : John Lo
 * Description  : 
 * Parent Module: 
 * Child  Module:
 * Interface Mod: many
 * Date Created : 2018-12-03
 * Notes        : cclk, hreset_n, sreset_n, egr_reset_n
 *
 ***************************************************************/

`include  "egr_tcu_defines.svh"

module egr_tcu_core (
  // epl_if
  input  logic [`TOT_EPL-1:0][1:0]          tx_enable_port_num, 
  input  logic [`TOT_EPL-1:0]               tx_enable,     
  output logic [`TOT_EPL-1:0]               tx_data_valid_resp, 
  output logic [`TOT_EPL-1:0][1:0]          tx_port_num,          
  output logic [`TOT_EPL-1:0][23:0]         tx_metadata,        
  output logic [`TOT_EPL-1:0][7:0]          tx_ecc,         
  output logic [`TOT_EPL-1:0][7:0]          tx_data_valid,      
  output logic [`TOT_EPL-1:0][71:0]         tx0_data_w_ecc, 
  output logic [`TOT_EPL-1:0][71:0]         tx1_data_w_ecc,  
  output logic [`TOT_EPL-1:0][71:0]         tx2_data_w_ecc, 
  output logic [`TOT_EPL-1:0][71:0]         tx3_data_w_ecc, 
  output logic [`TOT_EPL-1:0][71:0]         tx4_data_w_ecc, 
  output logic [`TOT_EPL-1:0][71:0]         tx5_data_w_ecc, 
  output logic [`TOT_EPL-1:0][71:0]         tx6_data_w_ecc, 
  output logic [`TOT_EPL-1:0][71:0]         tx7_data_w_ecc, 
  output logic [`TOT_EPL-1:0]               buf_ren_port0, // to packet scheduler
  output logic [`TOT_EPL-1:0]               buf_ren_port1, // to packet scheduler
  output logic [`TOT_EPL-1:0]               buf_ren_port2, // to packet scheduler
  output logic [`TOT_EPL-1:0]               buf_ren_port3, // to packet scheduler 
  input  logic [`TOT_EPL-1:0][23:0]         tx_metadata_port0,        // Per Fllit metadata
  input  logic [`TOT_EPL-1:0][23:0]         tx_metadata_port1,        // Per Fllit metadata
  input  logic [`TOT_EPL-1:0][23:0]         tx_metadata_port2,        // Per Fllit metadata
  input  logic [`TOT_EPL-1:0][23:0]         tx_metadata_port3,        // Per Fllit metadata
  input  logic [`TOT_EPL-1:0][7:0]          tx_ecc_port0,         // ECC for non-data TX buses except flow control i.e. (data_valid[7:0],metadata[23:0]} 
  input  logic [`TOT_EPL-1:0][7:0]          tx_ecc_port1,         // ECC for non-data TX buses except flow control i.e. (data_valid[7:0],metadata[23:0]} 
  input  logic [`TOT_EPL-1:0][7:0]          tx_ecc_port2,         // ECC for non-data TX buses except flow control i.e. (data_valid[7:0],metadata[23:0]} 
  input  logic [`TOT_EPL-1:0][7:0]          tx_ecc_port3,         // ECC for non-data TX buses except flow control i.e. (data_valid[7:0],metadata[23:0]} 
  input  logic [`TOT_EPL-1:0][7:0]          tx_data_valid_port0,      // Indicator of which flits (blocks of 8 bytes) contain valid data. 
  input  logic [`TOT_EPL-1:0][7:0]          tx_data_valid_port1,      // Indicator of which flits (blocks of 8 bytes) contain valid data. 
  input  logic [`TOT_EPL-1:0][7:0]          tx_data_valid_port2,      // Indicator of which flits (blocks of 8 bytes) contain valid data. 
  input  logic [`TOT_EPL-1:0][7:0]          tx_data_valid_port3,      // Indicator of which flits (blocks of 8 bytes) contain valid data. 
  input  logic [`TOT_EPL-1:0][7:0][71:0]    tx_data_w_ecc_port0,  // 
  input  logic [`TOT_EPL-1:0][7:0][71:0]    tx_data_w_ecc_port1,  // 
  input  logic [`TOT_EPL-1:0][7:0][71:0]    tx_data_w_ecc_port2,  // 
  input  logic [`TOT_EPL-1:0][7:0][71:0]    tx_data_w_ecc_port3,  // 
  input  logic [`TOT_EPL-1:0]               tc_rdy_port0,
  input  logic [`TOT_EPL-1:0]               tc_rdy_port1,
  input  logic [`TOT_EPL-1:0]               tc_rdy_port2,
  input  logic [`TOT_EPL-1:0]               tc_rdy_port3,
  output logic [`TOT_EPL-1:0]               port_num_err,          // to csr status bit
  input  logic [`TOT_EPL-1:0][3:0]          port_config,   // 0: 4ch, 1: 1 ch, 2: 2 ch, 3: rsvd
  // rx_pfc_if
  // global signals
  input  logic 	                        reset_n,
  input  logic 	                        clk   
  );

  // egr_tcu_epl
//  logic [`TOT_EPL-1:0][1:0]                    tx_enable_port_num;   // i
//  logic [`TOT_EPL-1:0]                         tx_enable;       // i
//  logic [`TOT_EPL-1:0]                         tx_data_valid_resp ;  // o
//  logic [`TOT_EPL-1:0][1:0]                    tx_port_num;          // o
//  logic [`TOT_EPL-1:0][23:0]                   tx_metadata;          // o
//  logic [`TOT_EPL-1:0][7:0]                    tx_ecc;               // o
//  logic [`TOT_EPL-1:0][7:0]                    tx_data_valid;        // o
//  logic [`TOT_EPL-1:0][71:0]                   tx0_data_w_ecc;       // o
//  logic [`TOT_EPL-1:0][71:0]                   tx1_data_w_ecc;       // o
//  logic [`TOT_EPL-1:0][71:0]                   tx2_data_w_ecc;       // o
//  logic [`TOT_EPL-1:0][71:0]                   tx3_data_w_ecc;       // o
//  logic [`TOT_EPL-1:0][71:0]                   tx4_data_w_ecc;       // o
//  logic [`TOT_EPL-1:0][71:0]                   tx5_data_w_ecc;       // o
//  logic [`TOT_EPL-1:0][71:0]                   tx6_data_w_ecc;       // o
//  logic [`TOT_EPL-1:0][71:0]                   tx7_data_w_ecc;       // o
//  logic [`TOT_EPL-1:0]                         buf_ren_port0;        // o
//  logic [`TOT_EPL-1:0]                         buf_ren_port1;        // o
//  logic [`TOT_EPL-1:0]                         buf_ren_port2;        // o
//  logic [`TOT_EPL-1:0]                         buf_ren_port3;        // o
//  logic [`TOT_EPL-1:0][23:0]                   tx_metadata_port0;    // i
//  logic [`TOT_EPL-1:0][23:0]                   tx_metadata_port1;    // i
//  logic [`TOT_EPL-1:0][23:0]                   tx_metadata_port2;    // i
//  logic [`TOT_EPL-1:0][23:0]                   tx_metadata_port3;    // i
//  logic [`TOT_EPL-1:0][7:0]                    tx_ecc_port0;         // i
//  logic [`TOT_EPL-1:0][7:0]                    tx_ecc_port1;         // i
//  logic [`TOT_EPL-1:0][7:0]                    tx_ecc_port2;         // i
//  logic [`TOT_EPL-1:0][7:0]                    tx_ecc_port3;         // i
//  logic [`TOT_EPL-1:0][7:0]                    tx_data_valid_port0;  // i
//  logic [`TOT_EPL-1:0][7:0]                    tx_data_valid_port1;  // i
//  logic [`TOT_EPL-1:0][7:0]                    tx_data_valid_port2;  // i
//  logic [`TOT_EPL-1:0][7:0]                    tx_data_valid_port3;  // i
//  logic [`TOT_EPL-1:0][7:0][71:0]              tx_data_w_ecc_port0;  // i
//  logic [`TOT_EPL-1:0][7:0][71:0]              tx_data_w_ecc_port1;  // i
//  logic [`TOT_EPL-1:0][7:0][71:0]              tx_data_w_ecc_port2;  // i
//  logic [`TOT_EPL-1:0][7:0][71:0]              tx_data_w_ecc_port3;  // i
//  logic [`TOT_EPL-1:0]                         tc_rdy_port0;         // i
//  logic [`TOT_EPL-1:0]                         tc_rdy_port1;         // i
//  logic [`TOT_EPL-1:0]                         tc_rdy_port2;         // i
//  logic [`TOT_EPL-1:0]                         tc_rdy_port3;         // i
//  logic [`TOT_EPL-1:0]                         port_num_err;         // o 
//  logic [`TOT_EPL][3:0]                        port_config,   // 0: 4ch, 1: 1 ch, 2: 2 ch, 3: rsvd
  //
  // egr_tcu_pkt_scheduler
  logic [`TOT_EPL-1:0][`TOT_LP-1:0][`TOT_TC-1:0]                              tcg_gnt    ; // o
  logic [`TOT_EPL-1:0][`TOT_LP-1:0][`TOT_TC-1:0]                              tcg_req    ; // i
  logic [`TOT_EPL-1:0][`TOT_LP-1:0][`TOT_TC-1:0]                              tcg_empty  ; // i
  logic [`TOT_EPL-1:0][`TOT_LP-1:0][`TOT_TC-1:0]                              tcg_ren    ; // i
  logic [`TOT_EPL-1:0][`TOT_LP-1:0]                                           win_grant  ; // o
  logic [`TOT_EPL-1:0][`TOT_LP-1:0][`TOT_PG-1:0]                              win_gnt    ; // o
  logic [`TOT_EPL-1:0][`TOT_LP-1:0][`TOT_PG-1:0][`QM_WIDTH-1:0]               win_quantum; // i
  logic [`TOT_EPL-1:0][`TOT_LP-1:0]                                           win_sp_mode; // i
  logic [`TOT_EPL-1:0][`TOT_LP-1:0][`SP_WIDTH-1:0]                            win_sp_top ; // i
  logic [`TOT_EPL-1:0][`TOT_LP-1:0][`TOT_PG-1:0][`TOT_TC-1:0]                 cfg_pg_tcg ; // i
  logic [`TOT_EPL-1:0][`TOT_LP-1:0][`TOT_PG-1:0][`TOT_TC-1:0][`QM_WIDTH-1:0]  pg_quantum ; // i
  logic [`TOT_EPL-1:0][`TOT_LP-1:0][`TOT_PG-1:0]                              pg_sp_mode ; // i
  logic [`TOT_EPL-1:0][`TOT_LP-1:0][`TOT_PG-1:0][`SP_WIDTH-1:0]               pg_sp_top  ; // i
  //

  genvar epl;
  generate  
    for (epl=0; epl< `TOT_EPL ; epl=epl+1) begin : FOUR_EPL_SHIM_GEN
      egr_tcu_epl_shim  egr_tcu_epl_shim
      (.tx_enable_port_num   (tx_enable_port_num [epl])  
      ,.tx_enable            (tx_enable          [epl])      
      ,.tx_data_valid_resp   (tx_data_valid_resp [epl])
      ,.tx_port_num          (tx_port_num        [epl])        
      ,.tx_metadata          (tx_metadata        [epl])        
      ,.tx_ecc               (tx_ecc             [epl])          
      ,.tx_data_valid        (tx_data_valid      [epl])  
      ,.tx0_data_w_ecc       (tx0_data_w_ecc     [epl]) 
      ,.tx1_data_w_ecc       (tx1_data_w_ecc     [epl]) 
      ,.tx2_data_w_ecc       (tx2_data_w_ecc     [epl]) 
      ,.tx3_data_w_ecc       (tx3_data_w_ecc     [epl]) 
      ,.tx4_data_w_ecc       (tx4_data_w_ecc     [epl]) 
      ,.tx5_data_w_ecc       (tx5_data_w_ecc     [epl]) 
      ,.tx6_data_w_ecc       (tx6_data_w_ecc     [epl]) 
      ,.tx7_data_w_ecc       (tx7_data_w_ecc     [epl]) 
      ,.buf_ren_port0        (buf_ren_port0      [epl]) 
      ,.buf_ren_port1        (buf_ren_port1      [epl]) 
      ,.buf_ren_port2        (buf_ren_port2      [epl]) 
      ,.buf_ren_port3        (buf_ren_port3      [epl]) 
      ,.tx_metadata_port0    (tx_metadata_port0  [epl])  
      ,.tx_metadata_port1    (tx_metadata_port1  [epl])  
      ,.tx_metadata_port2    (tx_metadata_port2  [epl])  
      ,.tx_metadata_port3    (tx_metadata_port3  [epl])  
      ,.tx_ecc_port0         (tx_ecc_port0       [epl])        
      ,.tx_ecc_port1         (tx_ecc_port1       [epl])        
      ,.tx_ecc_port2         (tx_ecc_port2       [epl])        
      ,.tx_ecc_port3         (tx_ecc_port3       [epl])        
      ,.tx_data_valid_port0  (tx_data_valid_port0[epl])
      ,.tx_data_valid_port1  (tx_data_valid_port1[epl])
      ,.tx_data_valid_port2  (tx_data_valid_port2[epl])
      ,.tx_data_valid_port3  (tx_data_valid_port3[epl])
      ,.tx_data_w_ecc_port0  (tx_data_w_ecc_port0[epl])
      ,.tx_data_w_ecc_port1  (tx_data_w_ecc_port1[epl])
      ,.tx_data_w_ecc_port2  (tx_data_w_ecc_port2[epl])
      ,.tx_data_w_ecc_port3  (tx_data_w_ecc_port3[epl])
      ,.tc_rdy_port0         (tc_rdy_port0       [epl])
      ,.tc_rdy_port1         (tc_rdy_port1       [epl])
      ,.tc_rdy_port2         (tc_rdy_port2       [epl])
      ,.tc_rdy_port3         (tc_rdy_port3       [epl])
      ,.port_num_err         (port_num_err       [epl]) 
      ,.port_config          (port_config        [epl])  
      ,.reset_n              (reset_n                 )
      ,.clk                  (clk                     ) 
      );
    end : FOUR_EPL_SHIM_GEN

    for (epl=0; epl< `TOT_EPL ; epl=epl+1) begin : FOUR_EPL_SCHEDULER_GEN
      egr_tcu_epl_scheduler  egr_tcu_epl_scheduler 
      (.tcg_gnt     (tcg_gnt[epl]    ) // o  [`TOT_EPL-1:0][`TOT_LP-1:0][`TOT_TC-1:0]                               
      ,.tcg_req     (tcg_req[epl]    ) // i  [`TOT_EPL-1:0][`TOT_LP-1:0][`TOT_TC-1:0]                               
      ,.tcg_empty   (tcg_empty[epl]  ) // i  [`TOT_EPL-1:0][`TOT_LP-1:0][`TOT_TC-1:0]                               
      ,.tcg_ren     (tcg_ren[epl]    ) // i  [`TOT_EPL-1:0][`TOT_LP-1:0][`TOT_TC-1:0]                               
      ,.win_grant   (win_grant[epl]  ) // o  [`TOT_EPL-1:0][`TOT_LP-1:0]                                            
      ,.win_gnt     (win_gnt[epl]    ) // o  [`TOT_EPL-1:0][`TOT_LP-1:0][`TOT_PG-1:0]                               
      ,.win_quantum (win_quantum[epl]) // i  [`TOT_EPL-1:0][`TOT_LP-1:0][`TOT_PG-1:0][`QM_WIDTH-1:0]                
      ,.win_sp_mode (win_sp_mode[epl]) // i  [`TOT_EPL-1:0][`TOT_LP-1:0]                                            
      ,.win_sp_top  (win_sp_top[epl] ) // i  [`TOT_EPL-1:0][`TOT_LP-1:0][`SP_WIDTH-1:0]                             
      ,.cfg_pg_tcg  (cfg_pg_tcg[epl] ) // i  [`TOT_EPL-1:0][`TOT_LP-1:0][`TOT_PG-1:0][`TOT_TC-1:0]                  
      ,.pg_quantum  (pg_quantum[epl] ) // i  [`TOT_EPL-1:0][`TOT_LP-1:0][`TOT_PG-1:0][`TOT_TC-1:0][`QM_WIDTH-1:0]   
      ,.pg_sp_mode  (pg_sp_mode[epl] ) // i  [`TOT_EPL-1:0][`TOT_LP-1:0][`TOT_PG-1:0]                               
      ,.pg_sp_top   (pg_sp_top[epl]  ) // i  [`TOT_EPL-1:0][`TOT_LP-1:0][`TOT_PG-1:0][`SP_WIDTH-1:0]                
      ,.reset_n     (reset_n         ) // i 	                                  
      ,.clk         (clk             )  // i 	                                  
      );
    end : FOUR_EPL_SCHEDULER_GEN
  endgenerate
      
endmodule : egr_tcu_core


