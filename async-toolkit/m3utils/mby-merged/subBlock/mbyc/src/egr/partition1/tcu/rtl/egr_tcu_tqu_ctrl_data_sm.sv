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
// -- Description  : This sm is for one logical port worth of tcu-tqu.
//
// EPL uses a:d this corresponds to 0:3 in EGR.
//------------------------------------------------------------------------------

/**************************************************************
 * File Name    : egr_tcu_tqu_ctrl_data_sm.sv
 * Author Name  : John Lo
 * Description  :
 * Parent Module:  
 * Child  Module:
 * Interface Mod: many
 * Date Created : 2018-12-05
 ***************************************************************/

`include  "egr_tcu_defines.svh"

module egr_tcu_tqu_ctrl_data_sm  
  (
  // dwrr intf
   input  logic [`TOT_TC-1:0]                tcg_gnt  
  //Service dtq_ctrl_pull
  ,output logic                            dtq_ctrl_peek_pop     // dtq_ctrl_pull ; 1:Pop
  ,output logic                            dtq_ctrl_pop_req      // dtq_ctrl_pull
  ,output logic [`W_TX_TC_SEL-1:0]         dtq_ctrl_sel          // dtq_ctrl_pull select one of the 9 queue
  ,input  logic [`TOT_TC-1:0]              dtq_ctrl_ready  // not empty for 9 queue
  ,input  logic [`CTRL_MDATA_W-1:0]        ctrl_mdata
  ,input  logic [`W_WORD-1:0][`W_BYTE-1:0] ctrl_word
  ,input  logic                            ctrl_word_valid
  //Service dtq_data_pull
  ,output logic                            dtq_data_peek_pop     // dtq_data_pull ; 1:Pop
  ,output logic                            dtq_data_pop_req      // dtq_data_pull
  ,output logic [`W_TX_TC_SEL-1:0]         dtq_data_sel          // dtq_data_pull select one of the 9 queue
  ,input  logic [`TOT_TC-1:0]              dtq_data_ready
  ,input  logic [`PKT_WORD_MDATA_W-1:0]    pkt_word_mdata
  ,input  logic [`W_WORD-1:0][`W_BYTE-1:0] pkt_word
  ,input  logic                            data_word_valid
  // to epl 
  ,output logic [`W_WORD-1:0][`W_BYTE-1:0] int_pkt_word
  ,input  logic                            reset_n
  ,input  logic                                clk   
  );

  logic                               ld_ctrl_word ; 
  logic                               ld_pkt_word  ;
  logic  [`PKT_WORD_MDATA_W-1:0]      int_pkt_word_mdata    ;   
  logic  [`CTRL_MDATA_W-1:0]          int_ctrl_mdata ;
  logic  [`W_WORD-1:0][`W_BYTE-1:0]   int_ctrl_word  ;
  logic                               ctrl_pop_req;
  logic                               data_pop_req;


  always_ff @ (posedge clk) begin
    dtq_ctrl_peek_pop <=  ctrl_pop_req  ; 
    dtq_ctrl_pop_req  <=  ctrl_pop_req  ; 
    dtq_ctrl_sel      <=  4'h0;  // always select TC0
    dtq_data_peek_pop <=  ctrl_pop_req  ; 
    dtq_data_pop_req  <=  ctrl_pop_req  ; 
    dtq_data_sel      <=  4'h0;  // always select TC0
  end

  typedef enum logic [1:0] { 
                            IDLE        = 2'b00
                           ,WAIT_CTRL_WD= 2'b01
                           ,DATA_PHASE  = 2'b10
                           ,WAIT_DATA_WD= 2'b11
                           } statetype;

  // sm flop
  statetype 		state;
  // sm comb
  statetype		nx_state;

  // begin egr_tcu_tqu_ctrl_data_sm sp_top
  always_comb begin   
    nx_state         = state;
    ctrl_pop_req     = '0   ;
    data_pop_req     = '0   ;
    ld_ctrl_word     = 1; 
    ld_pkt_word      = 1; 
    case (state)                                                                              
       IDLE : 
             if (|dtq_ctrl_ready) begin
               nx_state  = WAIT_CTRL_WD;
               ctrl_pop_req = 1; 
             end
             else 
               nx_state  = state   ;
       WAIT_CTRL_WD:
             if (|ctrl_word_valid) begin
               nx_state         = WAIT_CTRL_WD;
               ld_ctrl_word     = 1; 
             end
             else 
               nx_state  = state   ;
       DATA_PHASE:
             if (|dtq_data_ready & |tcg_gnt) begin
               nx_state         = WAIT_DATA_WD;
               data_pop_req     = 1; 
             end
             else 
               nx_state  = state   ;
       WAIT_DATA_WD:
             if (|data_word_valid) begin
               nx_state         = IDLE;
               ld_pkt_word      = 1; 
             end
             else 
               nx_state  = state   ;
      default:   nx_state = IDLE  ;    
    endcase
  end 

  always_ff @(posedge clk) begin 
    if (!reset_n) begin
      state <= IDLE;
    end
    else begin
      state <= nx_state;
    end
  end 

  always_ff @(posedge clk) begin 
    if (!reset_n) begin
      int_pkt_word_mdata <= '0   ;   
      int_pkt_word       <= '0   ;   
      int_ctrl_mdata     <= '0   ;
      int_ctrl_word      <= '0   ;
    end
    else begin
      int_pkt_word_mdata <=  ld_pkt_word  ?  pkt_word_mdata : int_pkt_word_mdata;   
      int_pkt_word       <=  ld_pkt_word  ?  pkt_word       : int_pkt_word      ;   
      int_ctrl_mdata     <=  ld_ctrl_word ?  ctrl_mdata     : int_ctrl_mdata    ;
      int_ctrl_word      <=  ld_ctrl_word ?  ctrl_word      : int_ctrl_word     ;
    end
  end

endmodule : egr_tcu_tqu_ctrl_data_sm

