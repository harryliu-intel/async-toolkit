
///
///  INTEL CONFIDENTIAL
///
///  Copyright 2015 Intel Corporation All Rights Reserved.
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
// ----------------------------------------------------------------------------
// -- Intel Proprietary
// -- Copyright (C) 2014 Intel Corporation
// -- All Rights Reserved
// ----------------------------------------------------------------------------
// -- Author:  John M. Greth (john.greth@intel.com)
// -- Project Name: Madison Bay (MBY)
// -- Description: Handles scheduling of segment reads for data transmit queues.
// ----------------------------------------------------------------------------

module fxr_rx_hp_fancy_arb import fxr_rx_hp_pkg::*; (
    input logic cclk,
    input logic i_sreset,

    // Incoming packets
    input logic tc_v,
    input fancy_arb_entry tc_entry,
    output logic tc_pop,

    // non-busy workers
    input logic [WORKERS-1:0] free_worker,
    input logic [WORKERS-1:0] worker_stalled,

    // Outgoing packets
    output logic [WORKERS-1:0] issue_worker,
    input logic issue_ready,
    output fancy_arb_entry issue_entry
    );

fancy_arb_entry [DEPTH-1:0] entry;
logic [DEPTH-1:0] valid_bits;

logic [DEPTH-1:0] ready_rank, early_ready_rank;
logic [2*DEPTH-1:0] sel_rank;
logic [DEPTH-1:0] sel_rank_q;
logic [DEPTH-1:0] sel_entry;
logic [DEPTH-1:0] entry_ready, early_entry_ready;

logic [DEPTH-1:0] next_pid_tail, next_early_pid_tail;
logic [DEPTH-1:0] next_entry;

logic [DEPTH-1:0] next_early_rank_entry, next_early_rank_entry_q, next_early_rank_ready;

///////////////////////////////////////////////////////////////////////////////
// Admit new entries
///////////////////////////////////////////////////////////////////////////////

always_comb for (int i = 0; i < DEPTH; i++) valid_bits[i] = entry[i].v | (|(tc_v & available) & next_entry[i]);
always_ff @(posedge cclk) begin
    if (i_sreset) next_entry <= { {(DEPTH-1){1'b0}}, 1'b1};
    else if (|sel_rank) next_entry <= sel_entry;
    else if (|tc_pop) next_entry <= ~valid_bits & ~(~valid_bits - { {(DEPTH-1){1'b0}}, 1'b1});
    end

always_ff @(posedge cclk) for (int i = 0; i < DEPTH; i++)
    entry[i].v <= ~i_sreset & ~sel_entry[i] & (entry[i].v | (tc_v & next_entry[i]));

always_ff @(posedge cclk) for (int i = 0; i < DEPTH; i++)
    if (next_entry[i]) begin // New entry
        entry[i].tc <= tc_entry.tc;
        end

///////////////////////////////////////////////////////////////////////////////
// PTE Status
///////////////////////////////////////////////////////////////////////////////

always_ff @(posedge cclk) for (int i = 0; i < DEPTH; i++) begin
    if (next_entry[i]) begin
        if (pte_valid & (tc_entry.pte_idx == pte_idx) & next_entry_in.pte_needed) begin
            entry[i].fp <= (pte_fp && |tc_entry.entry_point) || port_mirror;
            entry[i].unexp <= pte_unexp;
            entry[i].pte_update <= tc_entry.pte_update;
            end
        else begin
            entry[i].fp <= (tc_entry.fp && |next_entry_in.entry_point) || port_mirror;
            entry[i].unexp <= tc_entry.unexp;
            entry[i].pte_update <= tc_entry.pte_update;
            end
        end
    else begin
        if (pte_valid & (entry[i].pte_idx == pte_idx) & entry[i].pte_needed) begin
            entry[i].fp <= (pte_fp && |entry[i].entry_point) || port_mirror;
            entry[i].unexp <= pte_unexp;
            end
        end
    end

///////////////////////////////////////////////////////////////////////////////
// Priority Arbitration Out
///////////////////////////////////////////////////////////////////////////////

logic [DEPTH:0] next_early_rank, next_rank;
logic lp_disable, consume_early_entry;

always_ff @(posedge cclk)
    if (i_sreset) next_rank <= { {(DEPTH){1'b0}}, 1'b1};
    else if (|issue_worker_q & ~|(tc_v & available))
        next_rank <= {1'b0, next_rank[DEPTH:1]};
    else if (~|issue_worker_q & |(tc_v & available))
        next_rank <= {next_rank[DEPTH-1:0], 1'b0};

always_ff @(posedge cclk)
    if (i_sreset) next_early_rank <= { {(DEPTH){1'b0}}, 1'b1};
    else if (consume_early_entry & ~|next_early_rank_entry)
        next_early_rank <= {1'b0, next_early_rank[DEPTH:1]};
    else if (~consume_early_entry & |next_early_rank_entry)
        next_early_rank <= {next_early_rank[DEPTH-1:0], 1'b0};

always_comb begin
    ready_rank = {DEPTH{1'b0}};
    for (int i = 0; i < DEPTH; i++)
        ready_rank |= (entry[i].rank & {DEPTH{entry_ready[i]}});
    end

always_comb begin
    early_ready_rank = {DEPTH{1'b0}};
    for (int i = 0; i < DEPTH; i++)
        early_ready_rank |= (entry[i].early_rank & {DEPTH{early_entry_ready[i]}});
    end

logic [DEPTH*2-1:0] rank_dec;
logic [63-DEPTH*2:0] trash;
fxr_rx_hp_fast_adder rank_dec_adder (
        .A({ {(64-DEPTH*2){1'b0}}, early_ready_rank, ready_rank}), .B(64'hFFFF_FFFF_FFFF_FFFF),
        .Cin(1'b0),
        .S({trash, rank_dec}));

always_comb sel_rank =
    { {(2*DEPTH-1){~golden_locked}}, 1'b1} &
    {early_ready_rank, ready_rank} &
    ~rank_dec &
    {(2*DEPTH){issue_ready & ~pe_req}};
always_comb sel_rank_q = issue_entry_q.rank;

always_comb for (int i = 0; i < DEPTH; i++) entry_ready[i] = entry[i].v &
    ~(entry[i].pid_blocked | (~entry[i].pstat_ready & entry[i].fp) |
        entry[i].cluster_blocked | entry[i].dma_blocked | entry[i].eager_blocked);

always_comb for (int i = 0; i < DEPTH; i++) early_entry_ready[i] = entry[i].v &
    ~(entry[i].partial_pid_blocked | (~entry[i].pstat_ready & entry[i].fp) |
                entry[i].cluster_blocked | entry[i].dma_blocked | entry[i].eager_blocked);

always_comb lp_disable = arb_early_issue_disable;

always_ff @(posedge cclk) for (int i = 0; i < DEPTH; i++) begin
    if (next_entry[i])
        entry[i].rank <= |issue_worker_q ? next_rank[DEPTH:1] : next_rank[DEPTH-1:0];
    else entry[i].rank <= |(sel_rank_q & (entry[i].rank - { {(DEPTH-1){1'b0}}, 1'b1}))
        ? {1'b0, entry[i].rank[DEPTH-1:1]} : entry[i].rank;
    end

always_ff @(posedge cclk) for (int i = 0; i < DEPTH; i++) begin
    if (lp_disable)
        entry[i].early_rank <= '0;
    else if (next_entry[i])
        entry[i].early_rank <= '0;
    else if (|(entry[i].rank & next_early_rank_entry)) // Not allowed cycle after |issue_worker_q (rank cant change cycle before)
        entry[i].early_rank <= consume_early_entry ? next_early_rank[DEPTH:1] : next_early_rank[DEPTH-1:0];
    else entry[i].early_rank <= |(issue_entry_q.early_rank & (entry[i].early_rank - { {(DEPTH-1){1'b0}}, 1'b1}))
        ? {1'b0, entry[i].early_rank[DEPTH-1:1]} : entry[i].early_rank;
    end

logic [DEPTH-1:0][DEPTH-1:0] early_rank_debug;
always_comb for (int i = 0; i < DEPTH; i++) early_rank_debug[i] = {DEPTH{entry[i].v}} & entry[i].early_rank;

always_comb consume_early_entry = |issue_worker_q &&
    |issue_entry_q.early_rank ||
    |(next_early_rank_entry & issue_entry_q.rank) ||
    |(next_early_rank_entry_q & issue_entry_q.rank);
always_ff @(posedge cclk) next_early_rank_entry <= {DEPTH{~|issue_worker_q}} &
    (next_early_rank_ready & ~(next_early_rank_ready - { {(DEPTH-1){1'b0}}, 1'b1}));
always_ff @(posedge cclk) next_early_rank_entry_q <= next_early_rank_entry;

always_comb begin
    next_early_rank_ready = '0;
    for (int i = 0; i < DEPTH; i++)
        if (entry[i].early_rank_ready)
            next_early_rank_ready |= (entry[i].rank & ~next_early_rank_entry);
end

always_ff @(posedge cclk) for (int i = 0; i < DEPTH; i++) entry[i].early_rank_ready <=
            entry[i].v && ~|entry[i].early_rank && ~|entry[i].early_pid_block &&
            ~|(entry[i].rank & next_early_rank_entry) &&
            !entry[i].eager &&
            (entry[i].early_cmd_block ||
                ~|(entry[i].worker_pid_block_ptr[11:0] & (entry[i].worker_pid_block_ptr[11:0] - 12'h1)));

always_comb for (int i = 0; i < DEPTH; i++)
    sel_entry[i] = entry[i].v & |({entry[i].early_rank, entry[i].rank} & sel_rank);

///////////////////////////////////////////////////////////////////////////////
// Issue
///////////////////////////////////////////////////////////////////////////////

always_ff @(posedge cclk) issue_entry_q <= issue_entry;
always_comb begin
    issue_entry = {$bits(fancy_arb_entry){1'b0}};
    for (int i = 0; i < DEPTH; i++)
        issue_entry |= ({$bits(fancy_arb_entry){sel_entry[i]}} & entry[i]);
    end

logic [WORKERS-1:0] possible_dest, possible_dest_int, issue_worker_int;
always_comb possible_dest = issue_entry.dest & ~worker_busy;
always_ff @(posedge cclk) issue_worker_q <= issue_worker;
always_comb issue_worker_q_b = issue_worker_q & {WORKERS{issue_entry_q.pidB != 12'hFFF}};
logic issue_no_follow;
always_comb issue_no_follow = (issue_entry_q.entry_point == 5'd21) || !issue_entry_q.pte_needed;
always_comb issue_pte_worker_q = issue_worker_q & {WORKERS{issue_no_follow}};
always_comb issue_worker_int = possible_dest_int &
            ~(possible_dest_int - { {(WORKERS-1){1'b0}}, 1'b1});

always_comb begin
    issue_worker[FP_START+:5] = issue_worker_int[FP_START+:5];
    possible_dest_int[FP_START+:5] = possible_dest[FP_START+:5];
    for (int i = 0; i < FP_START / 2; i++) begin
        issue_worker[i] = issue_worker_int[2*i];
        issue_worker[i+FP_START/2] = issue_worker_int[2*i+1];
        possible_dest_int[2*i] = possible_dest[i];
        possible_dest_int[2*i+1] = possible_dest[i+FP_START/2];
    end
end

endmodule: fxr_rx_hp_fancy_arb

