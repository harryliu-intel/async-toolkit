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

module mby_egr_sch_arb #(parameter WIDTH=16, TOTAL_QUANTA=100, MAX_COST=160, PAYLOAD=16) (
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

    input logic [WIDTH-1:0][$clog2(TOTAL_QUANTA)-1:0] cfg_weight,
    input logic cfg_rr, cfg_rev, cfg_cost);

logic [WIDTH-1:0] selected, accepted, smart_win, const_one, new_win;
logic [WIDTH-1:0][$clog2(MAX_COST)-1:0] accepted_cost;
logic [$clog2(MAX_COST)-1:0] selected_cost;
logic [PAYLOAD-1:0] selected_payload;
always_comb begin
    const_one = {WIDTH{1'b0}};
    const_one[0] = 1'b1;
end

logic [WIDTH-1:0] ready, quanta_ready, min_quanta;
logic [WIDTH-1:0][$clog2(MAX_COST)-1:0] cost;
logic [WIDTH-1:0][PAYLOAD-1:0] payload;
logic [WIDTH-1:0][$clog2(MAX_COST)-1:0] quanta_needed;

logic increment;
logic [$clog2(MAX_COST)-1:0] increment_quanta;

///////////////////////////////////////////////////////////////////////////////
// Round Robin and static priority arbitration
///////////////////////////////////////////////////////////////////////////////

logic [WIDTH-1:0] ready_rev, ready_find_rev, ready_find_short;
logic [2*WIDTH-1:0] ready_find, ready_pos;
always_comb for (int i = 0; i < WIDTH; i++) ready_rev[i] = cfg_rev
            ? ready[WIDTH-1-i] : ready[i];
always_comb for (int i = 0; i < WIDTH; i++) ready_find_rev[i] = cfg_rev
            ? ready_find_short[WIDTH-1-i] : ready_find_short[i];

always_comb ready_find = ({ready_rev, ready_rev} & ready_pos) & (~({ready_rev, ready_rev} & ready_pos) - const_one);
always_comb ready_find_short = ready_find[0+:WIDTH] | ready_find[WIDTH+:WIDTH];
always_ff @(posedge clk)
    if (reset || !cfg_rr) ready_pos[WIDTH+:WIDTH] <= {WIDTH{1'b1}};
    else if (pop || ~|win)
        ready_pos[WIDTH+:WIDTH] <= ready_find_short | (ready_find_short - const_one);
always_comb ready_pos[0+:WIDTH] = ~ready_pos[WIDTH+:WIDTH];

///////////////////////////////////////////////////////////////////////////////
// The meat of the arbiter
///////////////////////////////////////////////////////////////////////////////

mby_egr_sch_arb_counter #(.TOTAL_QUANTA(TOTAL_QUANTA), .MAX_COST(MAX_COST), .PAYLOAD(PAYLOAD)) counters[WIDTH-1:0] (
    .clk, .reset,
    .consume(selected & {WIDTH{pop | ~|win}}),
    .arb, .arb_cost, .arb_pop, .arb_payload,
    .ready, .quanta_ready, .cost, .payload, .quanta_needed,
    .increment, .increment_quanta,
    .cfg(cfg_weight), .cfg_cost);

always_comb increment = ~|ready && |quanta_ready;

always_comb begin
    if (|ready) begin
        selected = ready_find_rev;
    end else if (|quanta_ready) begin
        selected = min_quanta; // never happens if weights are disabled
    end else begin
        selected = arb & ~(arb - const_one);
    end
end

always_comb min_quanta = quanta_ready & ~(quanta_ready - const_one); // TODO: Does this need to find min_quanta?
always_comb begin
    increment_quanta = {$clog2(MAX_COST){1'b0}};
    for (int i = 0; i < WIDTH; i++)
        if (min_quanta[i]) increment_quanta |= quanta_needed[i];
end

///////////////////////////////////////////////////////////////////////////////
// Deal with IOs
///////////////////////////////////////////////////////////////////////////////

always_comb begin
    selected_cost = {$clog2(MAX_COST){1'b0}};
    for (int i = 0; i < WIDTH; i++)
        if (selected[i])    selected_cost |= cost[i];
end

always_comb begin
    selected_payload = {PAYLOAD{1'b0}};
    for (int i = 0; i < WIDTH; i++)
        if (selected[i])    selected_payload |= payload[i];
end

always_ff @(posedge clk)
    if(reset) win <= {WIDTH{1'b0}};
    else if (pop || ~|win) win <= selected;

always_ff @(posedge clk)
    if (pop || ~|win) win_cost <= selected_cost;

always_ff @(posedge clk)
    if (pop || ~|win) win_payload <= selected_payload;

///////////////////////////////////////////////////////////////////////////////
// Assertions
///////////////////////////////////////////////////////////////////////////////

// `ifdef VCSSIM
// 
// class drr_val;
//     logic [$clog2(MAX_COST)-1:0] cost[WIDTH-1:0][$];
//     logic [PAYLOAD-1:0] payload[WIDTH-1:0][$];
// endclass
// 
// drr_val s = new;
// 
// always_ff @(posedge clk) begin
//     if (pop) for (int i = 0; i < WIDTH; i++) if (win[i] && pop) s.cost[i].pop_front();
//     for (int i = 0; i < WIDTH; i++) if (arb_pop[i]) s.cost[i].push_back(arb_cost[i]);
// end
// 
// always_ff @(posedge clk) begin
//     if (pop) for (int i = 0; i < WIDTH; i++) if (win[i] && pop) s.payload[i].pop_front();
//     for (int i = 0; i < WIDTH; i++) if (arb_pop[i]) s.payload[i].push_back(arb_payload[i]);
// end
// 
// property no_out_x();
//     @(posedge clk) disable iff(reset !== 1'b0)
//     (^win !== 1'bx);
// endproperty
// property win_cost_good(int i);
//     @(negedge clk) disable iff(reset !== 1'b0)
//     win[i] |-> (s.cost[i][0] == win_cost);
// endproperty
// property win_payload_good(int i);
//     @(negedge clk) disable iff(reset !== 1'b0)
//     win[i] |-> (s.payload[i][0] == win_payload);
// endproperty
// 
// assert property (no_out_x())
// else $error("ERROR: DRR_ARB has Xs on outputs");
// 
// generate for (genvar i = 0; i < WIDTH; i++) begin : win_assertions
//     assert property (win_cost_good(i))
//     else $error("ERROR: DRR_ARB win has incorrect cost");
//     assert property (win_payload_good(i))
//     else $error("ERROR: DRR_ARB win has incorrect payload");
// end
// endgenerate
// 
// // TODO: consider assert no win bubbles, bw shape, x inputs
// 
// `endif

endmodule

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
// Counts credits for a single entry
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
module mby_egr_sch_arb_counter #(parameter TOTAL_QUANTA=100, MAX_COST=160, PAYLOAD=16) (
    input logic clk, reset,

    input logic consume,

    input logic arb,
    input logic [$clog2(MAX_COST)-1:0] arb_cost,
    input logic [PAYLOAD-1:0] arb_payload,
    output logic arb_pop,

    output logic ready, quanta_ready,
    output logic [$clog2(MAX_COST)-1:0] cost,
    output logic [PAYLOAD-1:0] payload,
    output logic [$clog2(MAX_COST)-1:0] quanta_needed,

    input logic increment,
    input logic [$clog2(MAX_COST)-1:0] increment_quanta,

    input logic [$clog2(TOTAL_QUANTA)-1:0] cfg,
    input logic cfg_cost);

///////////////////////////////////////////////////////////////////////////////
// Pre-compute derivatives of cfg.
///////////////////////////////////////////////////////////////////////////////

logic [$clog2($clog2(TOTAL_QUANTA)+1):0] cfg_bits, cfg_bits_d;
logic [$clog2(TOTAL_QUANTA)-1:0] cfg_dec, cfg_cap;
logic skip_count;
always_ff @(posedge clk) skip_count <= ~|cfg[$clog2(TOTAL_QUANTA)-1:0];
always_ff @(posedge clk) cfg_dec <= cfg[$clog2(TOTAL_QUANTA)-1:0] - 1;
always_ff @(posedge clk) cfg_bits <= cfg_bits_d;
always_ff @(posedge clk) cfg_cap <= (1 << cfg_bits) - 1;
always_comb begin
    cfg_bits = 0;
    for (int i = 0; i < $clog2(TOTAL_QUANTA); i++)
        if (cfg_dec[i]) cfg_bits_d = i + 1;
end

///////////////////////////////////////////////////////////////////////////////
// Counter + IOs
///////////////////////////////////////////////////////////////////////////////

logic [$clog2(MAX_COST)+$clog2(TOTAL_QUANTA):0] credits;
logic [$clog2(MAX_COST)-1:0] cost_q;
logic [PAYLOAD-1:0] payload_q;

always_comb arb_pop = !quanta_ready && arb;
always_comb ready = (arb || quanta_ready) && (skip_count || (!cfg_cost && |credits) || (cost <= credits));
always_ff @(posedge clk) quanta_ready <= !consume && arb;
always_ff @(posedge clk)
    if (!cfg_cost)
        quanta_needed <= (increment || ready) ? 0 : 1;
    else case ({increment, quanta_ready})
        2'b11: quanta_needed <= ((cost_q   - credits + cfg_cap) >> cfg_bits) - increment_quanta;
        2'b10: quanta_needed <= ((arb_cost - credits + cfg_cap) >> cfg_bits) - increment_quanta;
        2'b01: quanta_needed <= ((cost_q   - credits + cfg_cap) >> cfg_bits);
        2'b00: quanta_needed <= ((arb_cost - credits + cfg_cap) >> cfg_bits);
    endcase

always_ff @(posedge clk) begin
    if (reset) credits <= {($clog2(MAX_COST)+$clog2(TOTAL_QUANTA)+1){1'b0}};
    else if (!arb && !quanta_ready) credits <= {($clog2(MAX_COST)+$clog2(TOTAL_QUANTA)+1){1'b0}};
    else if (!cfg_cost) case ({consume, increment})
        2'b11: credits <=  credits + (cfg[$clog2(TOTAL_QUANTA)-1:0] * increment_quanta) - 1;
        2'b10: credits <=  credits - 1;
        2'b01: credits <=  credits + (cfg[$clog2(TOTAL_QUANTA)-1:0] * increment_quanta);
        2'b00: credits <=  credits;
    endcase
    else case ({consume, increment})
        2'b11: credits <=  credits + (cfg[$clog2(TOTAL_QUANTA)-1:0] * quanta_needed)    - cost_q;
        2'b10: credits <=  credits                            - cost  ;
        2'b01: credits <=  credits + (cfg[$clog2(TOTAL_QUANTA)-1:0] * increment_quanta)         ;
        2'b00: credits <=  credits                                    ;
    endcase
end

always_ff @(posedge clk) cost_q <= cost;
always_comb cost = quanta_ready ? cost_q : arb_cost;

always_ff @(posedge clk) payload_q <= payload;
always_comb payload = quanta_ready ? payload_q : arb_payload;

///////////////////////////////////////////////////////////////////////////////
// Assertions
///////////////////////////////////////////////////////////////////////////////

`ifdef VCSSIM

property no_bad_inc();
    @(posedge clk) disable iff(reset !== 1'b0)
    !(consume && increment && !quanta_ready && !skip_count);
endproperty
property quanta_bounds();
    @(negedge clk) disable iff(reset !== 1'b0)
    (quanta_ready && !ready && !skip_count) |-> (((cfg[$clog2(TOTAL_QUANTA)-1:0] * quanta_needed) >= (cost_q - credits)) && ((cfg[$clog2(TOTAL_QUANTA)-1:0] * quanta_needed) < (2 * (cost_q - credits))));
endproperty

assert property (no_bad_inc())
else $error("ERROR: DRR_ARB is incrementing and counting when not valid");
assert property (quanta_bounds())
else $error("ERROR: DRR_ARB has bad quanta_needed");

`endif

endmodule

