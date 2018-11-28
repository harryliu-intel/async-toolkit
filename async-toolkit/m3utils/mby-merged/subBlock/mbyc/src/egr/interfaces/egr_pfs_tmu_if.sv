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
localparam PORTS_PER_EPL = 4;
localparam RX_TC_COUNT = 16;
// signals

// If TMU presents a queue as valid, it is always allowed to be popped.

// If there is only one packet on a queue, que_valid should be cleared on the clock after pop.

// Indicates that there is a packet at the head of this queue.
logic [EPL_PER_MGP-1:0][PORTS_PER_EPL-1:0][RX_TC_COUNT-1:0][MGP_COUNT-1:0] queue_valid;

// For each EPL, pop a specified queue.
logic [EPL_PER_MGP-1:0] pop;
logic [EPL_PER_MGP-1:0][$clog2(PORTS_PER_EPL)-1:0] pop_port;
logic [EPL_PER_MGP-1:0][$clog2(RX_TC_COUNT)-1:0] pop_tc;
logic [EPL_PER_MGP-1:0][$clog2(MGP_COUNT)-1:0] pop_mgp;

// For each EPL, update the deficit counters for the specified queue.
// This should be fixed latency after pop for small packets.
logic [EPL_PER_MGP-1:0] update;
logic [EPL_PER_MGP-1:0][$clog2(PORTS_PER_EPL)-1:0] update_port;
logic [EPL_PER_MGP-1:0][$clog2(RX_TC_COUNT)-1:0] update_tc;
logic [EPL_PER_MGP-1:0][$clog2(MGP_COUNT)-1:0] update_mgp;
logic [EPL_PER_MGP-1:0][6:0] update_length; // the actual length (in multiples of 64 bytes)

modport pfs(
    // port list
    input queue_valid, update, update_port, update_tc, update_mgp, update_length,
    output pop, pop_port, pop_tc, pop_mgp
    );

modport tmu(
    // port list
    output queue_valid, update, update_port, update_tc, update_mgp, update_length,
    input pop, pop_port, pop_tc, pop_mgp
    );

endinterface : egr_pfs_tmu_if
