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
 * File Name    : egr_tcu_epl_scheduler.sv
 * Author Name  : John Lo
 * Description  : 4 egr_tcu_pkt_scheduler instantiation.
 *                Each egr_tcu_pkt_scheduler is for one logicl port.
 *                There are total 4 logical ports in one EPL. 
 * Parent Module: 
 * Child  Module:
 * Interface Mod: many
 * Date Created : 2018-12-03
 * Notes        : cclk, hreset_n, sreset_n, egr_reset_n
 *
 ***************************************************************/

//`include  "mby_egr_epl_defines.svh"

module egr_tcu_epl_scheduler (
  // pkt_mod_if
  output logic [`TOT_LP-1:0][`TOT_TC-1:0]                              tcg_gnt    , 
  input  logic [`TOT_LP-1:0][`TOT_TC-1:0]                              tcg_req    , 
  input  logic [`TOT_LP-1:0][`TOT_TC-1:0]                              tcg_empty  , 
  input  logic [`TOT_LP-1:0][`TOT_TC-1:0]                              tcg_ren    , 
  output logic [`TOT_LP-1:0]                                           win_grant  , // as status bit
  output logic [`TOT_LP-1:0][`TOT_PG-1:0]                              win_gnt    ,
  input  logic [`TOT_LP-1:0][`TOT_PG-1:0][`QM_WIDTH-1:0]               win_quantum,
  input  logic [`TOT_LP-1:0]                                           win_sp_mode,
  input  logic [`TOT_LP-1:0][`SP_WIDTH-1:0]                            win_sp_top ,
  input  logic [`TOT_LP-1:0][`TOT_PG-1:0][`TOT_TC-1:0]                 cfg_pg_tcg ,
  input  logic [`TOT_LP-1:0][`TOT_PG-1:0][`TOT_TC-1:0][`QM_WIDTH-1:0]  pg_quantum ,
  input  logic [`TOT_LP-1:0][`TOT_PG-1:0]                              pg_sp_mode ,
  input  logic [`TOT_LP-1:0][`TOT_PG-1:0][`SP_WIDTH-1:0]               pg_sp_top  ,
  input  logic 	                                                       reset_n    ,
  input  logic 	                                                       clk          
  );

  genvar lp;
  generate
    for (lp=0; lp < `TOT_LP ; lp=lp+1) begin : TCU_LP_SCHEDULER_GEN
      egr_tcu_pkt_scheduler  egr_tcu_pkt_scheduler 
      (.tcg_gnt     (tcg_gnt[lp]    ) // o  [`TOT_LP-1:0][`TOT_TC-1:0]                             
      ,.tcg_req     (tcg_req[lp]    ) // i  [`TOT_LP-1:0][`TOT_TC-1:0]                             
      ,.tcg_empty   (tcg_empty[lp]  ) // i  [`TOT_LP-1:0][`TOT_TC-1:0]                             
      ,.tcg_ren     (tcg_ren[lp]    ) // i  [`TOT_LP-1:0][`TOT_TC-1:0]                             
      ,.win_grant   (win_grant[lp]  ) // o  [`TOT_LP-1:0]                                          
      ,.win_gnt     (win_gnt[lp]    ) // o  [`TOT_LP-1:0][`TOT_PG-1:0]                             
      ,.win_quantum (win_quantum[lp]) // i  [`TOT_LP-1:0][`TOT_PG-1:0][`QM_WIDTH-1:0]              
      ,.win_sp_mode (win_sp_mode[lp]) // i  [`TOT_LP-1:0]                                          
      ,.win_sp_top  (win_sp_top[lp] ) // i  [`TOT_LP-1:0][`SP_WIDTH-1:0]                           
      ,.cfg_pg_tcg  (cfg_pg_tcg[lp] ) // i  [`TOT_LP-1:0][`TOT_PG-1:0][`TOT_TC-1:0]                
      ,.pg_quantum  (pg_quantum[lp] ) // i  [`TOT_LP-1:0][`TOT_PG-1:0][`TOT_TC-1:0][`QM_WIDTH-1:0] 
      ,.pg_sp_mode  (pg_sp_mode[lp] ) // i  [`TOT_LP-1:0][`TOT_PG-1:0]                             
      ,.pg_sp_top   (pg_sp_top[lp]  ) // i  [`TOT_LP-1:0][`TOT_PG-1:0][`SP_WIDTH-1:0]              
      ,.reset_n     (reset_n    ) // i 	                                  
      ,.clk         (clk        ) // i 	                                  
      );
    end : TCU_LP_SCHEDULER_GEN
  endgenerate
      
endmodule : egr_tcu_epl_scheduler


