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
// -- Author       : John Lo          
// -- Project Name : Madison Bay (MBY)
// -- Description  : 
//
// EPL uses a:d this corresponds to 0:3 in EGR.
//------------------------------------------------------------------------------

/**************************************************************
 * File Name    : egr_tcu_pkt_scheduler.sv
 * Author Name  : John Lo
 * Description  : tcg_req  : used by tcu_dwrr_sm for req-gnt protocol. 
 *                tcg_empty: used by tcu_ledger for snapshot, replenish time.
 *                
 * Parent Module: 
 * Child  Module:
 * Interface Mod: many
 * Date Created : 2018-11-30 
 * Notes        : cclk, hreset_n, sreset_n, egr_reset_n
 *
 ***************************************************************/

//`include  "mby_egr_epl_defines.svh"

module egr_tcu_pkt_scheduler (
  // pkt_mod_if
  output logic [`TOT_TC-1:0]                              tcg_gnt    , 
  input  logic [`TOT_TC-1:0]                              tcg_req    , // used by tcu_dwrr_sm for req-gnt protocol. 
  input  logic [`TOT_TC-1:0]                              tcg_empty  , // used by tcu_ledger for snapshot, replenish time. 
  input  logic [`TOT_TC-1:0]                              tcg_ren    , 
  // csr_if
  output logic                                            win_grant  , // as status bit
  output logic [`TOT_PG-1:0]                              win_gnt    ,
  input  logic [`TOT_PG-1:0][`QM_WIDTH-1:0]               win_quantum,
  input  logic                                            win_sp_mode,
  input  logic [`SP_WIDTH-1:0]                            win_sp_top ,
  input  logic [`TOT_PG-1:0][`TOT_TC-1:0]                 cfg_pg_tcg ,
  input  logic [`TOT_PG-1:0][`TOT_TC-1:0][`QM_WIDTH-1:0]  pg_quantum ,
  input  logic [`TOT_PG-1:0]                              pg_sp_mode ,
  input  logic [`TOT_PG-1:0][`SP_WIDTH-1:0]               pg_sp_top  ,
  // global signals
  input  logic 	                                          reset_n    ,
  input  logic 	                                          clk          
  );

  logic [`TOT_PG-1:0]                              pg_grant   ; // internal signal
  logic [`TOT_PG-1:0][`TOT_TC-1:0]                 pg_gnt     ; // internal signal  
  logic [`TOT_PG-1:0][`TOT_TC-1:0]                 pg_req     ; // internal signal
  logic [`TOT_PG-1:0][`TOT_TC-1:0]                 pg_empty   ; // internal signal 
  logic [`TOT_PG-1:0][`TOT_TC-1:0]                 pg_ren     ; // internal signal
  // tcg and win signal direction is reversed. 
  logic [`TOT_PG-1:0]                              win_ren    ; // internal signal
  logic [`TOT_PG-1:0]                              win_req    ; // internal signal   
  logic [`TOT_PG-1:0]                              win_empty  ; // internal signal
  //
  // from pg to tgc
  genvar pg,tc;
  generate  
    for (tc=0; tc < `TOT_TC ; tc=tc+1) begin : TCG_GNT_GEN
      assign  tcg_gnt[tc] = (pg_gnt[0][tc] & win_gnt[0]) | 
                            (pg_gnt[1][tc] & win_gnt[1]) |
                            (pg_gnt[2][tc] & win_gnt[2]) |
                            (pg_gnt[3][tc] & win_gnt[3]) |
                            (pg_gnt[4][tc] & win_gnt[4]) |
                            (pg_gnt[5][tc] & win_gnt[5]) |
                            (pg_gnt[6][tc] & win_gnt[6]) |
                            (pg_gnt[7][tc] & win_gnt[7]) |
                            (pg_gnt[8][tc] & win_gnt[8]) ;
    end : TCG_GNT_GEN
  endgenerate
      
  // win_ren
  generate // win_ren 
    for (pg=0; pg < `TOT_PG ; pg=pg+1) begin : WIN_REN_GEN
      assign  win_ren[pg] = (pg_ren[pg][0] & win_gnt[pg]) | 
                            (pg_ren[pg][1] & win_gnt[pg]) |
                            (pg_ren[pg][2] & win_gnt[pg]) |
                            (pg_ren[pg][3] & win_gnt[pg]) |
                            (pg_ren[pg][4] & win_gnt[pg]) |
                            (pg_ren[pg][5] & win_gnt[pg]) |
                            (pg_ren[pg][6] & win_gnt[pg]) |
                            (pg_ren[pg][7] & win_gnt[pg]) |
                            (pg_ren[pg][8] & win_gnt[pg]) ;
    end : WIN_REN_GEN
  endgenerate

  // from tgc to pg 
  generate  
    for (pg=0; pg < `TOT_PG ; pg=pg+1) begin : PG_MUX_GEN
      for (tc=0; tc < `TOT_TC ; tc=tc+1) begin : TC_MUX_GEN
        assign  pg_req    [pg][tc]  = cfg_pg_tcg[pg][tc] ?   tcg_req    [tc] : '0;
        assign  pg_empty  [pg][tc]  = cfg_pg_tcg[pg][tc] ?   tcg_empty  [tc] : '0; 
        assign  pg_ren    [pg][tc]  = cfg_pg_tcg[pg][tc] ?   tcg_ren    [tc] : '0; 
      end : TC_MUX_GEN
    end : PG_MUX_GEN
  endgenerate
  
  // 9 pg arbiters
  generate  
    for (pg=0; pg < `TOT_PG ; pg=pg+1) begin : PG_DWRR_GEN
      egr_tcu_dwrr  egr_tcu_dwrr  
      (.grant    (pg_grant  [pg]) // o                               
      ,.gnt      (pg_gnt    [pg]) // o [`TOT_TC-1:0]                 
      ,.req      (pg_req    [pg]) // i [`TOT_TC-1:0]                 
      ,.sp_mode  (pg_sp_mode[pg]) // i                               
      ,.sp_top   (pg_sp_top [pg]) // i                          
      ,.empty    (pg_empty  [pg]) // i [`TOT_TC-1:0]                 
      ,.quantum  (pg_quantum[pg]) // i [`TOT_TC-1:0][`QM_WIDTH-1:0]  
      ,.ren      (pg_ren    [pg]) // i [`TOT_TC-1:0]                 
      ,.reset_n  (reset_n       ) // i                               
      ,.clk      (clk           ) // i                               
      );
    end : PG_DWRR_GEN
  endgenerate

  // winner arbiter
  assign  win_req   = pg_grant;
  assign  win_empty = '0;       // all 9 pg will participate in snapshot and replenish. 
  egr_tcu_dwrr  egr_tcu_dwrr_win        
  (.grant    (win_grant  ) // o , used as status only.                   
  ,.gnt      (win_gnt    ) // o [`TOT_PG-1:0]                 
  ,.req      (win_req    ) // i [`TOT_PG-1:0]                 
  ,.sp_mode  (win_sp_mode) // i csr                     
  ,.sp_top   (win_sp_top ) // i                          
  ,.empty    (win_empty  ) // i [`TOT_PG-1:0]                 
  ,.quantum  (win_quantum) // i [`TOT_PG-1:0][`QM_WIDTH-1:0]  
  ,.ren      (win_ren    ) // i [`TOT_PG-1:0]                 
  ,.reset_n  (reset_n    ) // i                               
  ,.clk      (clk        ) // i                               
  );

endmodule : egr_tcu_pkt_scheduler

