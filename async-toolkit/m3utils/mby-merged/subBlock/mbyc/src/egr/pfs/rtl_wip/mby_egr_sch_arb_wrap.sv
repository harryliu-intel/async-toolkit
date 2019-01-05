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

module mby_egr_sch_arb_wrap #(parameter WIDTH=16, TOTAL_QUANTA=100, MAX_COST=160, PAYLOAD=16) (
    input logic clk, reset,
    // should assert when there is something to send and only deassert after arb_pop
    input logic [WIDTH-1:0] arb, // slow
    input logic [WIDTH-1:0][$clog2(MAX_COST)-1:0] arb_cost, // slow
    input logic [WIDTH-1:0][PAYLOAD-1:0] arb_payload, // fast
    // Accepts this entry into the arbiter
    output logic [WIDTH-1:0] arb_pop, // fast
    // will assert when there is a winner and deassert after pop
    output logic [WIDTH-1:0] win, // fast
    output logic [$clog2(MAX_COST)-1:0] win_cost, // fast
    output logic [PAYLOAD-1:0] win_payload, // fast
    // indicates that the winner was accepted
    input logic pop, // fast

    input logic cfg_shift, cfg_in,
    output logic cfg_out);

logic [WIDTH-1:0][$clog2(TOTAL_QUANTA)-1:0] cfg_weight;
logic cfg_rr, cfg_rev, cfg_cost;

always_ff @(posedge clk) if (cfg_shift)
    {cfg_out, cfg_weight, cfg_rr, cfg_rev, cfg_cost} <=
        {cfg_weight, cfg_rr, cfg_rev, cfg_cost, cfg_in};

mby_egr_sch_arb #(.WIDTH(WIDTH), .TOTAL_QUANTA(TOTAL_QUANTA), .MAX_COST(MAX_COST), .PAYLOAD(PAYLOAD)) arbiter (
    .clk, .reset,
    .arb, .arb_cost, .arb_payload, .arb_pop,
    .win, .win_cost, .win_payload, .pop,
    .cfg_weight, .cfg_rr, .cfg_rev, .cfg_cost);

endmodule

