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
///  ------------------------------------------------------------------------------
///  -- Author       : Isaac Perez-Andrade
///  -- Project Name : Madison Bay (MBY) 
///  -- Description  : Tag Management Unit (TMU) interface with 
///                    Packet Fetch Scheduler (PFS)
///  ------------------------------------------------------------------------------

interface egr_pfs_tmu_if import shared_pkg::*; ();

localparam POP_TAG_LATENCY = 2;

// signals

// If TMU presents a queue as valid, it is always allowed to be popped.

// If there is only one packet on a queue, que_valid should be cleared on the clock after pop.

// Indicates that there is a packet at the head of this queue.
logic [N_MAX_LP_PER_EPL-1:0][MGP_TC_CNT-1:0][MGP_COUNT-1:0] queue_valid;

// One clock after pop, TMU indicates if the queue can be popped again immediately
// This is the same as the bit in queue_valid which corresponds to the queue which was popped on the last clock.
logic allow_fast_pop;

// Pop the specified queue.
logic pop;
logic [$clog2(N_MAX_LP_PER_EPL)-1:0] pop_port;
logic [$clog2(MGP_TC_CNT)-1:0] pop_tc;
logic [$clog2(MGP_COUNT)-1:0] pop_mgp;

// Popped tag is valid <POP_TAG_LATENCY> clocks after pop.
mby_tag_ring_t tag;
logic [$clog2(MGP_COUNT)-1:0] tag_mgp;
// Is the current tag coming form the tail buffer or head buffer?
logic tag_tail_buffer;

logic timestamp_valid;
logic [23:0] timestamp;
logic [$clog2(N_MAX_LP_PER_EPL)-1:0] timestamp_port;
logic [$clog2(MGP_TC_CNT)-1:0] timestamp_tc;
logic [$clog2(MGP_COUNT)-1:0] timestamp_mgp;


modport pfs(
    // port list
    input queue_valid, tag, allow_fast_pop, tag_tail_buffer, tag_mgp,
        timestamp_valid, timestamp, timestamp_port, timestamp_tc, timestamp_mgp,
    output pop, pop_port, pop_tc, pop_mgp
    );

modport tmu(
    // port list
    output queue_valid, tag, allow_fast_pop, tag_tail_buffer, tag_mgp,
        timestamp_valid, timestamp, timestamp_port, timestamp_tc, timestamp_mgp,
    input pop, pop_port, pop_tc, pop_mgp
    );

endinterface : egr_pfs_tmu_if
