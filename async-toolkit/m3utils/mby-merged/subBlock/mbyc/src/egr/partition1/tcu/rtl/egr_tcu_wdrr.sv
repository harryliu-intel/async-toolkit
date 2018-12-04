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
 * File Name    : egr_tcu_wdrr.sv
 * Author Name  : John Lo
 * Description  : One full wdrr function which includes wdrr_sm and ledger. 
 * Parent Module:  
 * Child  Module:
 * Interface Mod: many
 * Date Created : 2018-12-01
 *
 ***************************************************************/

`include  "egr_tcu_defines.svh"

module egr_tcu_wdrr  
  (output logic                               grant
  ,output logic [`TOT_TC-1:0]                 gnt  
  ,input  logic [`TOT_TC-1:0]                 req  
  ,input  logic                               sp_mode
  ,input  logic [`SP_WIDTH-1:0]               sp_top   
  ,input  logic [`TOT_TC-1:0]                 empty  
  ,input  logic [`TOT_TC-1:0][`QM_WIDTH-1:0]  quantum
  ,input  logic [`TOT_TC-1:0]                 ren     
  ,input  logic                               reset_n
  ,input  logic                               clk   
  );

  logic [`TOT_TC-1:0]                    deficit_cnt_ok;

  egr_tcu_wdrr_sm  egr_tcu_wdrr_sm 
  (.grant           (grant         ) // output logic // grant = |gnt;            
  ,.gnt             (gnt           ) // output logic [`TOT_TC-1:0]            
  ,.req             (req           ) // input  logic [`TOT_TC-1:0]              
  ,.deficit_cnt_ok  (deficit_cnt_ok) // input  logic [`TOT_TC-1:0]            
  ,.sp_mode         (sp_mode       ) // input  logic                          
  ,.sp_top          (sp_top        ) // input  logic                          
  ,.reset_n         (reset_n       ) // input  logic                          
  ,.clk             (clk           ) // input  logic                            
  );

  egr_tcu_ledger  egr_tcu_ledger  
  (.deficit_cnt_ok  (deficit_cnt_ok) // output[`TOT_TC-1:0]                 
  ,.sp_mode         (sp_mode       ) // input                               
  ,.empty           (empty         ) // input [`TOT_TC-1:0]                 
  ,.quantum         (quantum       ) // input [`TOT_TC-1:0][`QM_WIDTH-1:0]  
  ,.ren             (ren           ) // input [`TOT_TC-1:0]                 
  ,.reset_n         (reset_n       ) // input                               
  ,.clk             (clk           ) // input                               
  );


endmodule : egr_tcu_wdrr

