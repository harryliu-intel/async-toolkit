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
//-----------------------------------------------------------------------------
// -- Intel Proprietary
// -- Copyright (C) 2018 Intel Corporation
// -- All Rights Reserved
//-----------------------------------------------------------------------------
// -- Author:  John M. Greth (john.greth@intel.com)
// -- Project Name: Madison Bay
// -- FUB:  MBY_EGR_SCH
// -- Module: hfi_rx_psc_rsp_checker
// -- Description: This is an arbiter which can be configured for:
// --  - Static Priority
// --  - Round Robin
// --  - Deficit Round Robin
// --  - Weighted Round Robin
// --
// -- Outputs are all flops with the exception of arb_pop which has one cell.
// -- Inputs are expected to be flopped outside the block, as they do have some noticeable load.
// -- Latency from one arb to win is one clock. There is no internal bypass path.
// -- Config:
// --  - cfg_rr: 0- static priority, 1- round robin
// --  - cfg_rev: 0- arbitration starts at 0, 1- arbitration starts at WIDTH-1
// --  - cfg_cost: 0- each request consumes one credit, 1- each request consumes credits according to arb_cost
// --  - cfg_weight: Number of credits added for each requester on each iteration
// --      - A weight of zero indicates that all counting is disabled and this 
// --          requester is treated as always having enough credits to issue.
// --
//-----------------------------------------------------------------------------

module egr_fair_drr_arb #(parameter WIDTH=16, MAX_COST=64, MAX_LATENCY=4) (
    input logic clk, reset,
    input logic [WIDTH-1:0] arb,
    output logic [WIDTH-1:0] win, // Comb result on arb
    input logic pop, // May be asserted even if win is 0.

    input logic [WIDTH-1:0] consume,
    input logic [$clog2(MAX_COST)-1:0] cost);

localparam CTR_WIDTH = $clog2(MAX_LATENCY*MAX_COST);

// FLOPS
logic [WIDTH-1:0][$clog2(MAX_LATENCY*MAX_COST)-1:0] ctr;

// Intermediates
logic [CTR_WIDTH-1:0] all_ctr_fast, all_ctr_fast_rev;
logic [CTR_WIDTH-1:0] max_ctr_fast_rev, max_ctr_fast;
logic [CTR_WIDTH-1:0] global_inc, chosen_ctr;

logic do_global_inc;

logic [WIDTH-1:0] has_max_ctr;

always_comb begin
    all_ctr_fast = {CTR_WIDTH{1'b0}};
    for (int i = 0; i < WIDTH; i++) if (arb[i])
        all_ctr_fast |= ctr[i];
end
always_comb for (int i = 0; i < CTR_WIDTH; i++)
    all_ctr_fast_rev[i] = all_ctr_fast[CTR_WIDTH-i-1];
always_comb max_ctr_fast_rev = all_ctr_fast_rev & ~(all_ctr_fast_rev - 1); // TODO: lint fix
always_comb for (int i = 0; i < CTR_WIDTH; i++)
    max_ctr_fast[i] = max_ctr_fast_rev[CTR_WIDTH-i-1];
always_comb for (int i = 0; i < WIDTH; i++) has_max_ctr[i] = |(max_ctr_fast & ctr[i]);

always_comb begin
    chosen_ctr = {CTR_WIDTH{1'b1}};
    for (int i = 0; i < WIDTH; i++)
        if (consume[i]) chosen_ctr &= ctr[i];
end

always_comb global_inc = (chosen_ctr >= cost)
    ? {$clog2(MAX_LATENCY+MAX_COST){1'b0}}
    : (cost - chosen_ctr);

always_ff @(posedge clk) for (int i = 0; i < WIDTH; i++) begin
    if (reset || !arb[i]) ctr[i] <= {CTR_WIDTH{1'b0}};
    else begin
        case ({consume[i], win[i] & pop})
            2'b00: ctr[i] <= ctr[i];
            2'b01: ctr[i] <= ctr[i] + global_inc - MAX_COST;
            2'b10: ctr[i] <= ctr[i]              + MAX_COST - cost;
            2'b11: ctr[i] <= ctr[i] + global_inc            - cost;
        endcase
    end
end

always_comb win = (arb & has_max_ctr) & ~((arb & has_max_ctr) - 1); // TODO: lint fix

endmodule
