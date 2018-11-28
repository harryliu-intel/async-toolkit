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
///  ---------------------------------------------------------------------------------------------------------------------
///  -- Author       : Isaac Perez-Andrade
///  -- Project Name : Madison Bay (MBY) 
///  -- Description  : Packet Fetch Scheduler (PFS) module. 
///                    Submodule of the Egress (EGR) partition.
///  ------------------------------------------------------------------------------

module pfs import shared_pkg::*;
(
    input logic           clk,
    input logic         rst_n,

    // Internal interfaces
    egr_dpb_pfs_if.pfs dpb_if, // DPB requests to PFS
    egr_pfs_prc_if.pfs prc_if, // PFS requests to PRC
    egr_lcm_pfs_if.pfs lcm_if, // PFS provides to LCM
    egr_pfs_tmu_if.pfs tmu_if, // TMU provides to PFS
    egr_tcu_pfs_if.pfs tcu_if  // PFS provides to TCU
);

// Intermediate values in arbitration
logic [EPL_PER_MGP-1:0][tmu_if.RX_TC_COUNT-1:0][MGP_COUNT-1:0] blocked, available, winner;
logic [EPL_PER_MGP-1:0][tmu_if.PORTS_PER_EPL-1:0][tmu_if.RX_TC_COUNT-1:0][MGP_COUNT-1:0] winner_q;
logic [EPL_PER_MGP-1:0][tmu_if.PORTS_PER_EPL-1:0][tmu_if.RX_TC_COUNT-1:0] pfc;

// Port being operated on for each EPL
logic [EPL_PER_MGP-1:0][$clog2(tmu_if.PORTS_PER_EPL)-1:0] port, next_port;

// Winners for each port
logic [EPL_PER_MGP-1:0][tmu_if.PORTS_PER_EPL-1:0] valid; // Indicates that there is a winning packet for the port.
logic [EPL_PER_MGP-1:0][tmu_if.PORTS_PER_EPL-1:0][$clog2(MGP_COUNT)-1:0] mgp; // Indicates the source MGP of the winning queue
logic [EPL_PER_MGP-1:0][tmu_if.PORTS_PER_EPL-1:0][$clog2(tmu_if.RX_TC_COUNT)-1:0] tc; // Indicates the TC of the winning queue

// Port select counters
// TODO: This assumes all EPLs are configured for 4 ports (all <= 100G mode)
always_comb for (int i = 0; i < EPL_PER_MGP; i++)
    {next_port[i][0], next_port[i][1]} = {port[i][0], port[i][1]} + 2'b1;
always_ff @(posedge clk)
    if (!rst_n) port <= 0;
    else port <= next_port;

// PFC flop/blocking
always_ff @(posedge clk) pfc <= tcu_if.pfc;
always_comb
    for (int i = 0; i < EPL_PER_MGP; i++)
        for (int j = 0; j < tmu_if.RX_TC_COUNT; j++)
            blocked[i][j] = {MGP_COUNT{pfc}};

// These queues are considered in arbitration
always_comb for (int i = 0; i < EPL_PER_MGP; i++)
    available[i] = tmu_if.queue_valid[i][next_port[i]] & ~blocked[i] & ~winner_q[i][next_port[i]];

// FIXME: simple find first
always_comb for (int i = 0; i < EPL_PER_MGP; i++)
    winner[i] = available[i] & ~(available[i] - { {(tmu_if.RX_TC_COUNT*MGP_COUNT-1){1'b0}}, 1'b1});


always_ff @(posedge clk) for (int i = 0; i < EPL_PER_MGP; i++) begin
    // block a queue from winning on consecutive cycles (just in case).
    winner_q[i] <= 0;
    tmu_if.pop[i] <= 0;

    // Clear valid on ready
    if (!rst_n || prc_if.ready[i][port[i]]) valid[i][port[i]] <= 1'b0;

    // Select next winner
    if (!valid[i][next_port[i]] || ((port[i] == next_port[i]) && prc_if.ready[i][port[i]])) begin
        tmu_if.pop[i] <= |winner[i];
        winner_q[i][next_port[i]] <= winner[i];
        valid[i][next_port[i]] <= |winner[i];
        mgp[i][next_port[i]] <= 0;
        tc[i][next_port[i]] <= 0;
        for (int j = 0; j < tmu_if.RX_TC_COUNT; j++) begin
            if (|winner[i][j]) tc[i][next_port[i]] <= j;
            for (int k = 0; k < MGP_COUNT; k++)
                if (winner[i][j][k]) mgp[i][next_port[i]] <= k;
        end
    end
end

// TODO: make flops
always_comb for (int i = 0; i < EPL_PER_MGP; i++) begin
    prc_if.valid[i] = valid[i][port[i]];
    prc_if.port[i]  =  port[i];
    prc_if.mgp[i]   =   mgp[i][port[i]];
    prc_if.tc[i]    =    tc[i][port[i]];

    tmu_if.pop_port[i] = port[i];
    tmu_if.pop_mgp[i ] =  mgp[i][port[i]];
    tmu_if.pop_tc[i]   =   tc[i][port[i]];
end

// TODO: Assert that winner_q matches tmu_if.pop...

endmodule : pfs
