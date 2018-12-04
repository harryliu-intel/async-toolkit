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
 * File Name    : egr_tcu_ledger.sv.sv
 * Author Name  : John Lo
 * Description  : Provide deficit_cnt_ok, snapshot/replenish_time,
 *                and replenish QM for all TC.
 * Parent Module:  
 * Child  Module:
 * Interface Mod: many
 * Date Created : 2018-11-30
 *
 ***************************************************************/

`include  "egr_tcu_defines.svh"

module egr_tcu_ledger  
  (output logic [`TOT_TC-1:0]                 deficit_cnt_ok
  ,input  logic                               sp_mode
  ,input  logic [`TOT_TC-1:0]                 empty  
  ,input  logic [`TOT_TC-1:0][`QM_WIDTH-1:0]  quantum
  ,input  logic [`TOT_TC-1:0]                 ren
   // global signals
  ,input  logic                               reset_n
  ,input  logic                               clk   
  );

  logic                                  all_empty      ;
  logic                                  all_empty_d    ;
  logic [`TOT_TC-1:0]                    replenish_time ;         
  logic [`TOT_TC-1:0]                    participant_map;
  logic                                  all_empty_trail;

  assign  all_empty             = &empty[`TOT_TC-1:0]  ;
  assign  replenish_time        = &(participant_map & ~deficit_cnt_ok) | all_empty_trail ; 

  always_ff @(posedge clk) begin  
    all_empty_d     <= all_empty;
  end

  always_ff @(posedge clk)   
    if (!reset_n) 
      participant_map <= '0; 
    else if (replenish_time)
      participant_map <= ~empty[`TOT_TC-1:0];
    else
      participant_map <= participant_map; // bit_map

  assign  all_empty_trail = ~all_empty & all_empty_d;

  generate
  genvar i;
    for (i=0; i< `TOT_TC ; i=i+1) begin : DC_CNT_GEN
      egr_tcu_dc_cnt  egr_tcu_dc_cnt 
      (.deficit_cnt_ok   (deficit_cnt_ok[i]               ) // output                 
      ,.sp_mode          (sp_mode                         ) // input                 
      ,.empty            (empty[i]                        ) // input                 
      ,.replenish_time   (replenish_time[i]               ) // input                 
      ,.quantum          (quantum[i][`QM_WIDTH-1:0]       ) // input  [`QM_WIDTH-1:0]
      ,.ren              (ren[i]                          ) // input                 
      ,.reset_n          (reset_n                         ) // input                 
      ,.clk              (clk                             ) // input                 
      );
    end : DC_CNT_GEN

  endgenerate
     


endmodule : egr_tcu_ledger

