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
// -- Description  : This sm is for one EPL or 4 logical port worth of tcu-tqu.
//
// EPL uses a:d this corresponds to 0:3 in EGR.
//------------------------------------------------------------------------------

/**************************************************************
 * File Name    : egr_tcu_tqu_ctrl_data_epl.sv
 * Author Name  : John Lo
 * Description  :
 * Parent Module:  
 * Child  Module:
 * Interface Mod: many
 * Date Created : 2018-12-05
 ***************************************************************/

`include  "egr_tcu_defines.svh"

module egr_tcu_tqu_ctrl_data_epl  
  (
   input  logic [`TOT_LP-1:0][`TOT_TC-1:0]              tcg_gnt  
  ,output logic [`TOT_LP-1:0]                           dtq_ctrl_peek_pop     // dtq_ctrl_pull ; 1:Pop
  ,output logic [`TOT_LP-1:0]                           dtq_ctrl_pop_req      // dtq_ctrl_pull
  ,output logic [`TOT_LP-1:0][`W_TX_TC_SEL-1:0]         dtq_ctrl_sel          // dtq_ctrl_pull select one of the 9 queue
  ,input  logic [`TOT_LP-1:0][`TOT_TC-1:0]              dtq_ctrl_ready  // not empty for 9 queue
  ,input  logic [`TOT_LP-1:0][`CTRL_MDATA_W-1:0]        ctrl_mdata
  ,input  logic [`TOT_LP-1:0][`W_WORD-1:0][`W_BYTE-1:0] ctrl_word
  ,input  logic [`TOT_LP-1:0]                           ctrl_word_valid
  ,output logic [`TOT_LP-1:0]                           dtq_data_peek_pop     // dtq_data_pull ; 1:Pop
  ,output logic [`TOT_LP-1:0]                           dtq_data_pop_req      // dtq_data_pull
  ,output logic [`TOT_LP-1:0][`W_TX_TC_SEL-1:0]         dtq_data_sel          // dtq_data_pull select one of the 9 queue
  ,input  logic [`TOT_LP-1:0][`TOT_TC-1:0]              dtq_data_ready
  ,input  logic [`TOT_LP-1:0][`PKT_WORD_MDATA_W-1:0]    pkt_word_mdata
  ,input  logic [`TOT_LP-1:0][`W_WORD-1:0][`W_BYTE-1:0] pkt_word
  ,input  logic [`TOT_LP-1:0]                           data_word_valid
  ,output logic [`TOT_LP-1:0][`W_WORD-1:0][`W_BYTE-1:0] int_pkt_word
  ,input  logic                            reset_n
  ,input  logic                                clk   
  );

  
  genvar lp;
  generate
    for (lp=0; lp < `TOT_LP ; lp=lp+1) begin : TCU_LP_SCHEDULER_GEN
      egr_tcu_tqu_ctrl_data_sm   egr_tcu_tqu_ctrl_data_sm   
       (.tcg_gnt             (tcg_gnt           [lp])  // input  
       ,.dtq_ctrl_peek_pop   (dtq_ctrl_peek_pop [lp])  // output  
       ,.dtq_ctrl_pop_req    (dtq_ctrl_pop_req  [lp])  // output  
       ,.dtq_ctrl_sel        (dtq_ctrl_sel      [lp])  // output  
       ,.dtq_ctrl_ready      (dtq_ctrl_ready    [lp])  // input   
       ,.ctrl_mdata          (ctrl_mdata        [lp])  // input  
       ,.ctrl_word           (ctrl_word         [lp])  // input  
       ,.ctrl_word_valid     (ctrl_word_valid   [lp])  // input  
       ,.dtq_data_peek_pop   (dtq_data_peek_pop [lp])  // output  
       ,.dtq_data_pop_req    (dtq_data_pop_req  [lp])  // output  
       ,.dtq_data_sel        (dtq_data_sel      [lp])  // output  
       ,.dtq_data_ready      (dtq_data_ready    [lp])  // input  
       ,.pkt_word_mdata      (pkt_word_mdata    [lp])  // input  
       ,.pkt_word            (pkt_word          [lp])  // input  
       ,.data_word_valid     (data_word_valid   [lp])  // input  
       ,.int_pkt_word        (int_pkt_word      [lp])  // output 
       ,.reset_n             (reset_n               )  // input  
       ,.clk                 (clk                   )  // input  
       );
    end : TCU_LP_SCHEDULER_GEN
  endgenerate
 
  
endmodule : egr_tcu_tqu_ctrl_data_epl

