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
//------------------------------------------------------------------------------
// -- Author : Luis Alfonso Maeda-Nunez
// -- Project Name : Madison Bay (MBY) 
// -- Description  : PFS to PRC interface
//                   For connecting the Packet Fetch Scheduler to the Packet 
//                   Read Controller
//------------------------------------------------------------------------------

interface egr_pfs_prc_if import shared_pkg::*; ();

localparam DATA_TRANSMIT_QUEUE_COUNT = 8;
// PFS tells PRC which queue to pop a packet from.

mby_tag_ring_t tag;
logic [$clog2(MGP_COUNT)-1:0] tag_mgp;
logic [$clog2(DATA_TRANSMIT_QUEUE_COUNT)-1:0] dtq; // Indicates the data transmit queue of the winning tag
logic [N_MAX_LP_PER_EPL-1:0][DATA_TRANSMIT_QUEUE_COUNT-1:0] fund_credit;

modport pfs(
    output tag, dtq, tag_mgp,
    input fund_credit
    );

modport prc(
    input tag, dtq, tag_mgp,
    output fund_credit
    );

endinterface : egr_pfs_prc_if

