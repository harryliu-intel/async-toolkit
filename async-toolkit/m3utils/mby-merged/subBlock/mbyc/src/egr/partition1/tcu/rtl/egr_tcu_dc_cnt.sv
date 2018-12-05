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
// EPL uses a:d this corresponds to 0:3 in ERG.
//------------------------------------------------------------------------------

/**************************************************************
 * File Name    : egr_tcu_dc_cnt.sv
 * Author Name  : John Lo
 * Description  : Provide deficit_cnt_ok signal
 * Parent Module:  
 * Child  Module:
 * Interface Mod: many
 * Date Created : 2018-11-30
 *
 ***************************************************************/

`include  "egr_tcu_defines.svh"

module egr_tcu_dc_cnt  
  (output logic                          deficit_cnt_ok  
  ,input  logic                          sp_mode
  ,input  logic                          empty 
  ,input  logic                          replenish_time
  ,input  logic [`QM_WIDTH-1:0]          quantum
  ,input  logic                          ren
   // global signals
  ,input  logic                          reset_n
  ,input  logic                          clk   
  );
  
  logic                    deficit_cnt_exceed_QM;
  logic  [`DC_WIDTH-1:0]   dc_cnt; 

  always_ff @(posedge clk) begin : DC_CNTR 
    if (!reset_n) 
      dc_cnt <= '0;
    else if (sp_mode | empty) 
      dc_cnt <= '0;
    else if (replenish_time) 
      dc_cnt <= dc_cnt - {{(`DC_WIDTH-`QM_WIDTH){1'b0}}, quantum};
    else if (ren)
      dc_cnt <= dc_cnt + 1;
    else
      dc_cnt <= dc_cnt;
  end : DC_CNTR

  assign  deficit_cnt_exceed_QM = dc_cnt > {{(`DC_WIDTH-`QM_WIDTH){1'b0}}, quantum};
  assign  deficit_cnt_ok        = ~deficit_cnt_exceed_QM;

endmodule : egr_tcu_dc_cnt

