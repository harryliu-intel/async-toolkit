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
 * File Name    : egr_tcu_dwrr_sm.sv
 * Author Name  : John Lo
 * Description  :
 * Parent Module:  
 * Child  Module:
 * Interface Mod: many
 * Date Created : 2018-11-29
 *
 * DWRR/DRR Algorithm:
 * 1.	After POR, wait for at least one of TC to assert ~empty (ready) 
 *      than take a snap shot of the participating TC's. 
 *      Do not let the late coming TC¿s participate in arbitration 
 *      until next quanta replenish time.
 * 2.	Use positive math. Start Deficit Count (DC) from 0 value.  
 *      Each transmitted byte will add to Deficit Count (DC). 
 *      The same TC will continue to send packets as long as the queue/fifo 
 *      is not empty and the DC did not reach/exceed its quanta/weight.  
 *      The moment it reach/exceed its quanta/weight, scheduler should continue 
 *      the current packet until EOP then switch to next participating TC.
 * 3.	Once all participating TC¿s DC¿s reach/exceed their quanta/weight, 
 *      replenish quanta by subtracting their associated quanta from DC i.e.
 * 	(DC(j) = DC(j) ¿ quanta(j)). 
 *      Take a snap shot of all participating TC¿s (queue/fifo ~empty) again. 
 *	Goto step 2.
 *
 * Note 1: If all TC[8:0] quanta is 0. The above Algorithm is reduced to simple RR.
 * Note 2: If TC is empty, its associated DC is reset to 0.
 * Note 3: Reset statemachine state to GNT7 and set deficit_cnt_ok to 1
 *         will reduce dwrr to sp (strick priority) algorithm.
 * Note 4: GNT8 has the lowest priority in default sp_mode.
 ***************************************************************/

`include  "egr_tcu_defines.svh"

module egr_tcu_dwrr_sm  
  (output logic                          grant
  ,output logic [`TOT_TC-1:0]            gnt  
  ,input  logic [`TOT_TC-1:0]            req  
  ,input  logic [`TOT_TC-1:0]            deficit_cnt_ok
  ,input  logic                          sp_mode
  ,input  logic [`SP_WIDTH-1:0]          sp_top
  ,input  logic                          reset_n
  ,input  logic                          clk   
  );


  typedef enum logic [3:0] { 
                            GNT8   = 4'h8 
                           ,GNT7   = 4'h7 
                           ,GNT6   = 4'h6 
                           ,GNT5   = 4'h5 
                           ,GNT4   = 4'h4 
                           ,GNT3   = 4'h3 
                           ,GNT2   = 4'h2 
                           ,GNT1   = 4'h1 
                           ,GNT0   = 4'h0  
                           } statetype;

  // sm flop
  statetype 		state;
  // sm comb
  statetype		nx_state;

  // begin egr_tcu_dwrr_sm sp_top
  always_comb begin : TCU_DWRR_SM_COMB
    nx_state = state;
    gnt      = '0   ;
    case (state)                                                                              
      GNT7 : if (req[7] & deficit_cnt_ok[7]) begin nx_state = GNT7; gnt[7] = 1'b1; end 
        else if (req[6] & deficit_cnt_ok[6]) begin nx_state = GNT6; gnt[6] = 1'b1; end 
        else if (req[5] & deficit_cnt_ok[5]) begin nx_state = GNT5; gnt[5] = 1'b1; end  
        else if (req[4] & deficit_cnt_ok[4]) begin nx_state = GNT4; gnt[4] = 1'b1; end 
        else if (req[3] & deficit_cnt_ok[3]) begin nx_state = GNT3; gnt[3] = 1'b1; end 
        else if (req[2] & deficit_cnt_ok[2]) begin nx_state = GNT2; gnt[2] = 1'b1; end 
        else if (req[1] & deficit_cnt_ok[1]) begin nx_state = GNT1; gnt[1] = 1'b1; end 
        else if (req[0] & deficit_cnt_ok[0]) begin nx_state = GNT0; gnt[0] = 1'b1; end 
        else if (req[8] & deficit_cnt_ok[8]) begin nx_state = GNT8; gnt[8] = 1'b1; end 
	else                                 begin nx_state = state;               end
      GNT6 : if (req[6] & deficit_cnt_ok[6]) begin nx_state = GNT6; gnt[6] = 1'b1; end 
        else if (req[5] & deficit_cnt_ok[5]) begin nx_state = GNT5; gnt[5] = 1'b1; end 
        else if (req[4] & deficit_cnt_ok[4]) begin nx_state = GNT4; gnt[4] = 1'b1; end 
        else if (req[3] & deficit_cnt_ok[3]) begin nx_state = GNT3; gnt[3] = 1'b1; end 
        else if (req[2] & deficit_cnt_ok[2]) begin nx_state = GNT2; gnt[2] = 1'b1; end 
        else if (req[1] & deficit_cnt_ok[1]) begin nx_state = GNT1; gnt[1] = 1'b1; end 
        else if (req[0] & deficit_cnt_ok[0]) begin nx_state = GNT0; gnt[0] = 1'b1; end 
        else if (req[8] & deficit_cnt_ok[8]) begin nx_state = GNT8; gnt[8] = 1'b1; end 
        else if (req[7] & deficit_cnt_ok[7]) begin nx_state = GNT7; gnt[7] = 1'b1; end 
	else                                 begin nx_state = state;               end
      GNT5 : if (req[5] & deficit_cnt_ok[5]) begin nx_state = GNT5; gnt[5] = 1'b1; end 
        else if (req[4] & deficit_cnt_ok[4]) begin nx_state = GNT4; gnt[4] = 1'b1; end 
        else if (req[3] & deficit_cnt_ok[3]) begin nx_state = GNT3; gnt[3] = 1'b1; end 
        else if (req[2] & deficit_cnt_ok[2]) begin nx_state = GNT2; gnt[2] = 1'b1; end 
        else if (req[1] & deficit_cnt_ok[1]) begin nx_state = GNT1; gnt[1] = 1'b1; end 
        else if (req[0] & deficit_cnt_ok[0]) begin nx_state = GNT0; gnt[0] = 1'b1; end 
        else if (req[8] & deficit_cnt_ok[8]) begin nx_state = GNT8; gnt[8] = 1'b1; end 
        else if (req[7] & deficit_cnt_ok[7]) begin nx_state = GNT7; gnt[7] = 1'b1; end 
        else if (req[6] & deficit_cnt_ok[6]) begin nx_state = GNT6; gnt[6] = 1'b1; end 
	else                                 begin nx_state = state;               end
      GNT4 : if (req[4] & deficit_cnt_ok[4]) begin nx_state = GNT4; gnt[4] = 1'b1; end 
        else if (req[3] & deficit_cnt_ok[3]) begin nx_state = GNT3; gnt[3] = 1'b1; end 
        else if (req[2] & deficit_cnt_ok[2]) begin nx_state = GNT2; gnt[2] = 1'b1; end 
        else if (req[1] & deficit_cnt_ok[1]) begin nx_state = GNT1; gnt[1] = 1'b1; end 
        else if (req[0] & deficit_cnt_ok[0]) begin nx_state = GNT0; gnt[0] = 1'b1; end 
        else if (req[8] & deficit_cnt_ok[8]) begin nx_state = GNT8; gnt[8] = 1'b1; end 
        else if (req[7] & deficit_cnt_ok[7]) begin nx_state = GNT7; gnt[7] = 1'b1; end 
        else if (req[6] & deficit_cnt_ok[6]) begin nx_state = GNT6; gnt[6] = 1'b1; end 
        else if (req[5] & deficit_cnt_ok[5]) begin nx_state = GNT5; gnt[5] = 1'b1; end 
	else                                 begin nx_state = state;               end
      GNT3 : if (req[3] & deficit_cnt_ok[3]) begin nx_state = GNT3; gnt[3] = 1'b1; end 
        else if (req[2] & deficit_cnt_ok[2]) begin nx_state = GNT2; gnt[2] = 1'b1; end 
        else if (req[1] & deficit_cnt_ok[1]) begin nx_state = GNT1; gnt[1] = 1'b1; end 
        else if (req[0] & deficit_cnt_ok[0]) begin nx_state = GNT0; gnt[0] = 1'b1; end 
        else if (req[8] & deficit_cnt_ok[8]) begin nx_state = GNT8; gnt[8] = 1'b1; end 
        else if (req[7] & deficit_cnt_ok[7]) begin nx_state = GNT7; gnt[7] = 1'b1; end 
        else if (req[6] & deficit_cnt_ok[6]) begin nx_state = GNT6; gnt[6] = 1'b1; end 
        else if (req[5] & deficit_cnt_ok[5]) begin nx_state = GNT5; gnt[5] = 1'b1; end 
        else if (req[4] & deficit_cnt_ok[4]) begin nx_state = GNT4; gnt[4] = 1'b1; end 
	else                                 begin nx_state = state;               end
      GNT2 : if (req[2] & deficit_cnt_ok[2]) begin nx_state = GNT2; gnt[2] = 1'b1; end 
        else if (req[1] & deficit_cnt_ok[1]) begin nx_state = GNT1; gnt[1] = 1'b1; end 
        else if (req[0] & deficit_cnt_ok[0]) begin nx_state = GNT0; gnt[0] = 1'b1; end 
        else if (req[8] & deficit_cnt_ok[8]) begin nx_state = GNT8; gnt[8] = 1'b1; end 
        else if (req[7] & deficit_cnt_ok[7]) begin nx_state = GNT7; gnt[7] = 1'b1; end 
        else if (req[6] & deficit_cnt_ok[6]) begin nx_state = GNT6; gnt[6] = 1'b1; end 
        else if (req[5] & deficit_cnt_ok[5]) begin nx_state = GNT5; gnt[5] = 1'b1; end 
        else if (req[4] & deficit_cnt_ok[4]) begin nx_state = GNT4; gnt[4] = 1'b1; end 
        else if (req[3] & deficit_cnt_ok[3]) begin nx_state = GNT3; gnt[3] = 1'b1; end 
	else                                 begin nx_state = state;               end
      GNT1 : if (req[1] & deficit_cnt_ok[1]) begin nx_state = GNT1; gnt[1] = 1'b1; end 
        else if (req[0] & deficit_cnt_ok[0]) begin nx_state = GNT0; gnt[0] = 1'b1; end 
        else if (req[8] & deficit_cnt_ok[8]) begin nx_state = GNT8; gnt[8] = 1'b1; end 
        else if (req[7] & deficit_cnt_ok[7]) begin nx_state = GNT7; gnt[7] = 1'b1; end 
        else if (req[6] & deficit_cnt_ok[6]) begin nx_state = GNT6; gnt[6] = 1'b1; end 
        else if (req[5] & deficit_cnt_ok[5]) begin nx_state = GNT5; gnt[5] = 1'b1; end 
        else if (req[4] & deficit_cnt_ok[4]) begin nx_state = GNT4; gnt[4] = 1'b1; end 
        else if (req[3] & deficit_cnt_ok[3]) begin nx_state = GNT3; gnt[3] = 1'b1; end 
        else if (req[2] & deficit_cnt_ok[2]) begin nx_state = GNT2; gnt[2] = 1'b1; end 
	else                                 begin nx_state = state;               end
      GNT0 : if (req[0] & deficit_cnt_ok[0]) begin nx_state = GNT0; gnt[0] = 1'b1; end 
        else if (req[8] & deficit_cnt_ok[8]) begin nx_state = GNT8; gnt[8] = 1'b1; end 
        else if (req[7] & deficit_cnt_ok[7]) begin nx_state = GNT7; gnt[7] = 1'b1; end 
        else if (req[6] & deficit_cnt_ok[6]) begin nx_state = GNT6; gnt[6] = 1'b1; end 
        else if (req[5] & deficit_cnt_ok[5]) begin nx_state = GNT5; gnt[5] = 1'b1; end 
        else if (req[4] & deficit_cnt_ok[4]) begin nx_state = GNT4; gnt[4] = 1'b1; end 
        else if (req[3] & deficit_cnt_ok[3]) begin nx_state = GNT3; gnt[3] = 1'b1; end 
        else if (req[2] & deficit_cnt_ok[2]) begin nx_state = GNT2; gnt[2] = 1'b1; end 
        else if (req[1] & deficit_cnt_ok[1]) begin nx_state = GNT1; gnt[1] = 1'b1; end 
	else                                 begin nx_state = state;               end
      GNT8 : if (req[8] & deficit_cnt_ok[8]) begin nx_state = GNT8; gnt[8] = 1'b1; end 
        else if (req[7] & deficit_cnt_ok[7]) begin nx_state = GNT7; gnt[7] = 1'b1; end 
        else if (req[6] & deficit_cnt_ok[6]) begin nx_state = GNT6; gnt[6] = 1'b1; end 
        else if (req[5] & deficit_cnt_ok[5]) begin nx_state = GNT5; gnt[5] = 1'b1; end 
        else if (req[4] & deficit_cnt_ok[4]) begin nx_state = GNT4; gnt[4] = 1'b1; end 
        else if (req[3] & deficit_cnt_ok[3]) begin nx_state = GNT3; gnt[3] = 1'b1; end 
        else if (req[2] & deficit_cnt_ok[2]) begin nx_state = GNT2; gnt[2] = 1'b1; end 
        else if (req[1] & deficit_cnt_ok[1]) begin nx_state = GNT1; gnt[1] = 1'b1; end 
        else if (req[0] & deficit_cnt_ok[0]) begin nx_state = GNT0; gnt[0] = 1'b1; end 
	else                                 begin nx_state = state;               end
      default:                               begin nx_state = GNT7; gnt    = '0  ; end    
    endcase
  end : TCU_DWRR_SM_COMB

  always_ff @(posedge clk) begin : TCU_DWRR_SM_SEQ
    if (!reset_n) begin
      state <= GNT7;
    end
    else if (sp_mode) begin
      case (sp_top)
        `SP_TOP0: state <=  GNT0;
        `SP_TOP1: state <=  GNT1;
        `SP_TOP2: state <=  GNT2;
        `SP_TOP3: state <=  GNT3;
        `SP_TOP4: state <=  GNT4;
        `SP_TOP5: state <=  GNT5;
        `SP_TOP6: state <=  GNT6;
        `SP_TOP7: state <=  GNT7;
        `SP_TOP8: state <=  GNT8;
	default : state <=  GNT7;
      endcase
    end
    else begin
      state <= nx_state;
    end
  end : TCU_DWRR_SM_SEQ

  assign  grant = |gnt; 

endmodule : egr_tcu_dwrr_sm

