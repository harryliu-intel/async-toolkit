///
///  INTEL CONFIDENTIAL
///
///  Copyright 2018 Intel Corporation All Rights Reserved.
///
///  The source code contained or described herein and all documents related
///  to the source code ("Material") are owned by Intel Corporation or its
///  suppliers or licensors. Title to the Material remains with Intel
///  Corporation or its suppliers and licensors. The Material contains trade
///  secrets and proprietary and confidential information of Intel or its
///  suppliers and licensors. The Material is protected by worldwide copyright
///  and trade secret laws and treaty provisions. No part of the Material may
///  be used, copied, reproduced, modified, published, uploaded, posted,
///  transmitted, distributed, or disclosed in any way without Intel's prior
///  express written permission.
///
///  No license under any patent, copyright, trade secret or other intellectual
///  property right is granted to or conferred upon you by disclosure or
///  delivery of the Materials, either expressly, by implication, inducement,
///  estoppel or otherwise. Any license under such intellectual property rights
///  must be express and approved by Intel in writing.
///
// ---------------------------------------------------------------------------------------------------------------------
// -- Author : Jim McCormick <jim.mccormick@intel.com>
// -- Project Name : Madison Bay (MBY) 
// -- Description  : This file defines an arbiter with weighted arbitration algorithm
// 
// -- Pipeline Stages: 
//
//      ... TBD ... 
//
// ---------------------------------------------------------------------------------------------------------------------

// note that the dashed lines above represents recommended viewing window width


`include "mby_msh_defines.vh"                                   // include file with `defines 


typedef logic [6:0]  msh_arb_weight_t;
typedef logic [6:0]  msh_arb_weight_multiple_t;
typedef logic [6:0]  msh_arb_weight_remainder_t;
typedef logic [4:0]  msh_fifo_depth_t;

module mby_msh_arb_weighted 
import mby_msh_pkg::*;                                          // import declarations from mby_msh_pkg.sv
# (
    parameter           NUM_BIDS = 2                            // the number of bids
)
(

    input                           mclk,                         // mesh clock                                 
    input                           i_reset,                      // reset

    input  logic [NUM_BIDS-1:0]     i_bids,                       // the bids
    input  msh_arb_weight_t         i_weights     [NUM_BIDS-1:0], // the weights ([NUM_BIDS-1] largest to [0] smallest)
    input  msh_fifo_depth_t         i_fifo_depths [NUM_BIDS-1:0], // the depth of each bidders FIFOs 

    output logic [NUM_BIDS-1:0]     o_winners                     // the winners

);


//-----------------------------------------------------------------------------
// Declarations 
//-----------------------------------------------------------------------------

msh_arb_weight_multiple_t   weight_multiples  [NUM_BIDS-1:0];      // weight multiples 
msh_arb_weight_remainder_t  weight_remainders [NUM_BIDS-1:0];      // weight remainders 
msh_arb_weight_t            multiplied_weights  [NUM_BIDS-1:0];       

msh_arb_weight_multiple_t   wm_counters [NUM_BIDS-1:0];            // weight multiple counters
msh_arb_weight_remainder_t  wr_counters [NUM_BIDS-1:0];            // weight multiple remainder counters
msh_arb_weight_multiple_t   nxt_wm_counters [NUM_BIDS-1:0];
msh_arb_weight_remainder_t  nxt_wr_counters [NUM_BIDS-1:0];

logic [NUM_BIDS-1:0]     highest_priority;                      // the highest priority bidder
logic [NUM_BIDS-1:0]     counter_resets;
logic [NUM_BIDS-1:0]    priority_winners;
logic [NUM_BIDS-1:0]    priority_winner;
logic [NUM_BIDS-1:0]    rr_winners;
logic [NUM_BIDS-1:0]    winners;

logic found_nonzero_counter;


//-----------------------------------------------------------------------------
// Code
//-----------------------------------------------------------------------------

//      EXAMPLE: 
//          weights[3] = 8 
//          weights[2] = 7 
//          weights[1] = 3 
//          weights[0] = 2 

//calculate weight_multiples and weight_remainders

always_comb for (int bid=0; bid < NUM_BIDS; bid++) begin
// FIXME:  need to handle weights of 0

    //EXAMPLE:  
    //
    //  weight_multiples    [0]   = weights[0]  = 2;
    //  weight_remainders   [0]                 = 0;
    //  multiplied_weights  [0]   = weights[0]  = 2;
    //
    //  weight_multiples    [1]   = weights         [1]   / multiplied_weights[0]   = 3/2 = 1;
    //  weight_remainders   [1]   = weights         [1]   % multiplied_weights[0]   = 3%2 = 1;
    //  multiplied_weights  [1]   = weight_multiples[1]   * multiplied_weights[0]   = 1*2 = 2;  
    //
    //  weight_multiples    [2]   = weights         [2]   / multiplied_weights[1]   = 7/2 = 3;
    //  weight_remainders   [2]   = weights         [2]   % multiplied_weights[1]   = 7%2 = 1;
    //  multiplied_weights  [2]   = weight_multiples[2]   * multiplied_weights[1]   = 3*2 = 6; 
    //
    //  weight_multiples    [3]   = weights         [3]   / multiplied_weights[2]   = 8/6 = 1;
    //  weight_remainders   [3]   = weights         [3]   % multiplied_weights[2]   = 8%6 = 2;
    //  multiplied_weights  [3]   = weight_multiples[3]   * multiplied_weights[2]   = 1*6 = 6; 
    //
    if (bid == 0) begin
        weight_multiples    [bid] = i_weights[bid]; 
        weight_remainders   [bid] = '0; 
        multiplied_weights  [bid] = i_weights[bid];
    end else begin
        weight_multiples    [bid] = i_weights         [bid] / multiplied_weights[bid-1];
        weight_remainders   [bid] = i_weights         [bid] % multiplied_weights[bid-1];
        multiplied_weights  [bid] = weight_multiples[bid] * multiplied_weights[bid-1]; 
    end


end // for bid < NUM_BIDS-1

// counters
always_ff @(posedge mclk) begin
    for (int bid=0; bid < NUM_BIDS; bid++) begin
        if (i_reset | counter_resets[bid]) begin
            wm_counters  [bid] <= weight_multiples  [bid];
            wr_counters  [bid] <= weight_remainders [bid];
        end else begin
            wm_counters  [bid] <= nxt_wm_counters   [bid];
            wr_counters  [bid] <= nxt_wr_counters   [bid];
        end
    end // for bid < NUM_BIDS-1
end


// determine highest priorty bidder

always_comb begin

//  EXAMPLE:
//
//          weights[3] = 8 
//          weights[2] = 7 
//          weights[1] = 3 
//          weights[0] = 2 
//
//      desired sequence:       b3, b2, b3, b2, b3, b2, b1, b0, b3, b2, b3, b2, b3, b2, b1, b0, b3, b3, b2, b1
//      ----------------
//       weight_multiples[3]=1: --
//       weight_multiples[2]=3: ------  ------  ------
//       weight_multiples[1]=1: --------------------------  
//       weight_multiples[0]=2: ------------------------------  ------------------------------ 
//      weight_remainders[3]=2:                                                                 --  --
//      weight_remainders[2]=1:                                                                         --
//      weight_remainders[1]=1:                                                                             --
//      weight_remainders[0]=0:
//
//      cycle sequence:
//      --------------
//      cycle 0:    wm_counters = {1,3,1,2}, wm_remainders = {2,1,1,0}, highest_priority = b3
//      cycle 1:    wm_counters = {0,3,1,2}, wm_remainders = {2,1,1,0}, highest_priority = b2 
//      cycle 2:    wm_counters = {1,2,1,2}, wm_remainders = {2,1,1,0}, highest_priority = b3 
//      cycle 3:    wm_counters = {0,2,1,2}, wm_remainders = {2,1,1,0}, highest_priority = b2 
//      cycle 4:    wm_counters = {1,1,1,2}, wm_remainders = {2,1,1,0}, highest_priority = b3 
//      cycle 4:    wm_counters = {0,1,1,2}, wm_remainders = {2,1,1,0}, highest_priority = b2 
//      cycle 4:    wm_counters = {0,0,1,2}, wm_remainders = {2,1,1,0}, highest_priority = b1 
//      cycle 4:    wm_counters = {0,0,0,2}, wm_remainders = {2,1,1,0}, highest_priority = b0 
//      cycle 4:    wm_counters = {1,3,1,1}, wm_remainders = {2,1,1,0}, highest_priority = b3 
//      cycle 4:    wm_counters = {0,3,1,1}, wm_remainders = {2,1,1,0}, highest_priority = b2 
//      cycle 4:    wm_counters = {1,2,1,1}, wm_remainders = {2,1,1,0}, highest_priority = b3 
//      cycle 4:    wm_counters = {0,2,1,1}, wm_remainders = {2,1,1,0}, highest_priority = b2 
//      cycle 4:    wm_counters = {1,1,1,1}, wm_remainders = {2,1,1,0}, highest_priority = b3 
//      cycle 4:    wm_counters = {0,1,1,1}, wm_remainders = {2,1,1,0}, highest_priority = b2
//      cycle 4:    wm_counters = {0,0,1,1}, wm_remainders = {2,1,1,0}, highest_priority = b1
//      cycle 4:    wm_counters = {0,0,0,1}, wm_remainders = {2,1,1,0}, highest_priority = b0
//      cycle 4:    wm_counters = {0,0,0,0}, wm_remainders = {2,1,1,0}, highest_priority = b3
//      cycle 4:    wm_counters = {0,0,0,0}, wm_remainders = {1,1,1,0}, highest_priority = b3
//      cycle 4:    wm_counters = {0,0,0,0}, wm_remainders = {0,1,1,0}, highest_priority = b2
//      cycle 4:    wm_counters = {0,0,0,0}, wm_remainders = {0,0,1,0}, highest_priority = b1
//
//      cycle 4:    wm_counters = {1,3,1,2}, wm_remainders = {2,1,1,0}, highest_priority = ... 
//



    // set default outcomes
    highest_priority        = '0;
    nxt_wm_counters         = wm_counters;
    nxt_wr_counters         = wr_counters;
    counter_resets          = '0;
    found_nonzero_counter   = '0; 

    // first non-zero weight multiple counter bit determines highest priority 
    for (int bid=NUM_BIDS-1; bid >= 0; bid--) begin

        if ((highest_priority == '0) && (wm_counters[bid] > 0)) begin
            highest_priority    [bid] = 1'b1;
            nxt_wm_counters     [bid] = wm_counters[bid] - 1;
            if ((wm_counters[bid] > 1) && (bid < NUM_BIDS-1))
                for (int crs=bid+1; crs < NUM_BIDS; crs++) 
                    counter_resets[crs] = 1'b1;
        end 
            
    end // bid < NUM_BIDS-1

    // else, first non-zero remainder counter bit determines highest priority 

    for (int bid=NUM_BIDS-1; bid >= 0; bid--) begin

        if ((highest_priority == '0) && (wr_counters[bid] > 0)) begin
            highest_priority    [bid] = 1'b1;
            nxt_wr_counters      [bid] = wr_counters[bid] - 1;
        end
            
    end // bid < NUM_BIDS-1

    // reset counters when all the counts have been used
    
    for (int bid=0; bid < NUM_BIDS; bid++) begin
        if ((nxt_wm_counters[bid] != 0) || (nxt_wr_counters[bid] != 0))
            found_nonzero_counter = 1'b1;
    end // bid < NUM_BIDS-1
     
    if (!found_nonzero_counter)
        counter_resets = '1;

end

// If there is a priority winner, then that is the winner; 
// Otherwise, just choose a winner with round robin arbitration. 

always_comb priority_winners = highest_priority & i_bids;
always_comb priority_winner = |priority_winners;

mby_arb_lprr #(                         // just using rr arbiter from APR right now because I'm lazy
    .N      (NUM_BIDS       ),
    .LOGN   ($clog2(NUM_BIDS))
) rr_arb (
    .clk        (mclk),
    .i_rst      (i_reset),
    .i_upd      (~priority_winner),
    .i_tail     (1'b1),                 // don't lock onto this arbitration 
    .i_req      (i_bids),
    .o_q_locked (),                     // will never be locked
    .o_idx      (),
    .o_req_vld  (),
    .o_gnt_tail (),
    .o_gnt      (rr_winners)
);

always_comb 
    if (priority_winner)
        winners = priority_winners;
    else
        winners = rr_winners;


//**********************************************************************************************************************
// OUTPUT SECTION 
//**********************************************************************************************************************

//-----------------------------------------------------------------------------
// Assign Output Signals
//-----------------------------------------------------------------------------

assign o_winners = winners;

endmodule // mby_msh_arb_weighted
