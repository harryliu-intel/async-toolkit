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
// -- Description: Selects a winner from 16 TCs and 16 MGPs to one lane:
// --
// -- Outputs are all flops with the exception of arb_pop which has one cell.
// -- Inputs are expected to be flopped outside the block, as they do have some noticeable load.
// -- Latency from one arb to win is one clock. There is no internal bypass path (yet).
// --
//-----------------------------------------------------------------------------

module mby_egr_sch_cluster_arb #(parameter MGPS=16, UTCGS=8, MTCGS=8, PGS=8, TOTAL_QUANTA=100, MAX_COST=160, PAYLOAD=16) (
    input logic clk, reset,
    // should assert when there is something to send and only deassert after arb_pop
    input logic [MGPS-1:0][UTCGS+MTCGS-1:0] arb, // slow
    input logic [MGPS-1:0][UTCGS+MTCGS-1:0][$clog2(MAX_COST)-1:0] arb_cost, // slow
    input logic [MGPS-1:0][UTCGS+MTCGS-1:0][PAYLOAD-1:0] arb_payload, // fast
    // Accepts this entry into the arbiter
    output logic [MGPS-1:0][UTCGS+MTCGS-1:0] arb_pop, // fast
    // will assert when there is a winner and deassert after pop
    output logic win, // fast
    output logic [$clog2(MAX_COST)-1:0] win_cost, // fast
    output logic [PAYLOAD-1:0] win_payload, // fast
    // indicates that the winner was accepted
    input logic pop, // fast

    input logic cfg_shift, cfg_in,
    output logic cfg_out);

///////////////////////////////////////////////////////////////////////////////
// CFG SR
///////////////////////////////////////////////////////////////////////////////

logic [UTCGS+MTCGS+PGS+3:0] cfg_sr;
logic [PGS-1:0][UTCGS+MTCGS-1:0] pg_cfg;
logic [UTCGS+MTCGS-1:0] low_cfg;
logic [UTCGS-1:0] high_cfg;

always_ff @(posedge clk) if (cfg_shift)
    {cfg_out, high_cfg, low_cfg, pg_cfg} <=
        {high_cfg, low_cfg, pg_cfg, cfg_sr[UTCGS+MTCGS+PGS+3]};

///////////////////////////////////////////////////////////////////////////////
// For each TCG, arb between the MCGs. Results in one stream per TCG.
///////////////////////////////////////////////////////////////////////////////

logic [UTCGS+MTCGS-1:0][MGPS-1:0] mgp_win;
logic [UTCGS+MTCGS-1:0] mgp_win_or;
logic [UTCGS+MTCGS-1:0][$clog2(MAX_COST)-1:0] mgp_win_cost;
logic [UTCGS+MTCGS-1:0][PAYLOAD-1:0] mgp_win_payload;
logic [UTCGS+MTCGS-1:0] mgp_pop;
mby_egr_sch_arb_wrap #(.WIDTH(MGPS), .TOTAL_QUANTA(TOTAL_QUANTA), .MAX_COST(MAX_COST), .PAYLOAD(PAYLOAD)) MGParb[UTCGS+MTCGS-1:0] (
    .clk, .reset,
    .arb, .arb_cost, .arb_payload, .arb_pop,
    .win(mgp_win), .win_cost(mgp_win_cost), .win_payload(mgp_win_payload), .pop(mgp_pop),
    .cfg_shift, .cfg_in({cfg_sr[0+:UTCGS+MTCGS-1], cfg_in}), .cfg_out(cfg_sr[0+:UTCGS+MTCGS]));
always_comb for (int i = 0; i < UTCGS+MTCGS; i++) mgp_win_or[i] = |mgp_win[i];


///////////////////////////////////////////////////////////////////////////////
// For each PG, arb between the TCGs in that PG. Results in one stream per PG.
///////////////////////////////////////////////////////////////////////////////

logic [PGS-1:0][MGPS-1:0] pg_win;
logic [PGS-1:0] pg_win_or;
logic [PGS-1:0][$clog2(MAX_COST)-1:0] pg_win_cost;
logic [PGS-1:0][PAYLOAD-1:0] pg_win_payload;
logic [PGS-1:0] pg_pop;
logic [PGS-1:0][UTCGS+MTCGS-1:0] pg_mgp_pop;
mby_egr_sch_arb_wrap #(.WIDTH(UTCGS+MTCGS), .TOTAL_QUANTA(TOTAL_QUANTA), .MAX_COST(MAX_COST), .PAYLOAD(PAYLOAD)) PGarb[PGS-1:0] (
    .clk, .reset,
    .arb({PGS{mgp_win_or}} & pg_cfg), .arb_cost(mgp_win_cost), .arb_payload(mgp_win_payload), .arb_pop(pg_mgp_pop),
    .win(pg_win), .win_cost(pg_win_cost), .win_payload(pg_win_payload), .pop(pg_pop),
    .cfg_shift, .cfg_in(cfg_sr[UTCGS+MTCGS-1+:PGS]), .cfg_out(cfg_sr[UTCGS+MTCGS+:PGS]));
always_comb for (int i = 0; i < PGS; i++) pg_win_or[i] = |pg_win[i];

///////////////////////////////////////////////////////////////////////////////
// For each PG, arb to one stream.
///////////////////////////////////////////////////////////////////////////////

logic [PGS-1:0] pga_win;
logic pga_win_or;
logic [$clog2(MAX_COST)-1:0] pga_win_cost;
logic [PAYLOAD-1:0] pga_win_payload;
logic pga_pop;
mby_egr_sch_arb_wrap #(.WIDTH(PGS), .TOTAL_QUANTA(TOTAL_QUANTA), .MAX_COST(MAX_COST), .PAYLOAD(PAYLOAD)) PGAarb (
    .clk, .reset,
    .arb(pg_win_or), .arb_cost(pg_win_cost), .arb_payload(pg_win_payload), .arb_pop(pg_pop),
    .win(pga_win), .win_cost(pga_win_cost), .win_payload(pga_win_payload), .pop(pga_pop),
    .cfg_shift, .cfg_in(cfg_sr[UTCGS+MTCGS+PGS-1+:1]), .cfg_out(cfg_sr[UTCGS+MTCGS+PGS+:1]));
always_comb pga_win_or = |pga_win;

///////////////////////////////////////////////////////////////////////////////
// For each Low-priority TCG, arb to one stream.
///////////////////////////////////////////////////////////////////////////////

logic [UTCGS+MTCGS-1:0] low_win;
logic low_win_or;
logic [$clog2(MAX_COST)-1:0] low_win_cost;
logic [PAYLOAD-1:0] low_win_payload;
logic low_pop;
logic [UTCGS+MTCGS-1:0] low_mgp_pop;
mby_egr_sch_arb_wrap #(.WIDTH(UTCGS+MTCGS), .TOTAL_QUANTA(TOTAL_QUANTA), .MAX_COST(MAX_COST), .PAYLOAD(PAYLOAD)) LOWarb (
    .clk, .reset,
    .arb(mgp_win_or & low_cfg), .arb_cost(mgp_win_cost), .arb_payload(mgp_win_payload), .arb_pop(low_mgp_pop),
    .win(low_win), .win_cost(low_win_cost), .win_payload(low_win_payload), .pop(low_pop),
    .cfg_shift, .cfg_in(cfg_sr[UTCGS+MTCGS+PGS+:1]), .cfg_out(cfg_sr[UTCGS+MTCGS+PGS+1+:1]));
always_comb low_win_or = |low_win;

///////////////////////////////////////////////////////////////////////////////
// For each High-priority TCG, arb to one stream.
///////////////////////////////////////////////////////////////////////////////

logic [UTCGS-1:0] high_win;
logic high_win_or;
logic [$clog2(MAX_COST)-1:0] high_win_cost;
logic [PAYLOAD-1:0] high_win_payload;
logic high_pop;
logic [UTCGS-1:0] high_mgp_pop;
mby_egr_sch_arb_wrap #(.WIDTH(UTCGS), .TOTAL_QUANTA(TOTAL_QUANTA), .MAX_COST(MAX_COST), .PAYLOAD(PAYLOAD)) HIGHarb (
    .clk, .reset,
    .arb(mgp_win_or[UTCGS-1:0] & high_cfg), .arb_cost(mgp_win_cost[UTCGS-1:0]), .arb_payload(mgp_win_payload[UTCGS-1:0]), .arb_pop(high_mgp_pop),
    .win(high_win), .win_cost(high_win_cost), .win_payload(high_win_payload), .pop(high_pop),
    .cfg_shift, .cfg_in(cfg_sr[UTCGS+MTCGS+PGS+1+:1]), .cfg_out(cfg_sr[UTCGS+MTCGS+PGS+2+:1]));
always_comb high_win_or = |high_win;

///////////////////////////////////////////////////////////////////////////////
// Arb between low, high, and pg streams.
///////////////////////////////////////////////////////////////////////////////

logic [2:0] all_win;
mby_egr_sch_arb_wrap #(.WIDTH(3), .TOTAL_QUANTA(TOTAL_QUANTA), .MAX_COST(MAX_COST), .PAYLOAD(PAYLOAD)) FINALarb (
    .clk, .reset,
    .arb({low_win_or, pga_win_or, high_win_or}),
    .arb_cost({low_win_cost, pga_win_cost, high_win_cost}),
    .arb_payload({low_win_payload, pga_win_payload, high_win_payload}),
    .arb_pop({low_pop, pga_pop, high_pop}),
    .win(all_win), .win_cost, .win_payload, .pop,
    .cfg_shift, .cfg_in(cfg_sr[UTCGS+MTCGS+PGS+2+:1]), .cfg_out(cfg_sr[UTCGS+MTCGS+PGS+3+:1]));
always_comb win = |all_win;

///////////////////////////////////////////////////////////////////////////////
// Stitch POPs together
///////////////////////////////////////////////////////////////////////////////

always_comb begin
    mgp_pop = 0;
    for (int i = 0; i < PGS; i++)
        mgp_pop |= pg_mgp_pop[i];
    mgp_pop |= high_mgp_pop;
    mgp_pop[UTCGS-1:0] |= low_mgp_pop;
end

endmodule

