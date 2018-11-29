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
///  -- Description  : Packet Read Controller (PRC) interface with 
///                    Local Congestion Manager (LCM)
///  ------------------------------------------------------------------------------

interface egr_prc_lcm_if();
    // Local Parameters
    localparam N_EPL = 4;

    // Typedefs
    typedef logic   [7:0] toq_sel_t;
    typedef logic  [10:0] tiq_sel_t;
    // signals
    logic     [N_EPL-1:0] dequeue_req;
    toq_sel_t [N_EPL-1:0] dequeue_toq_sel;
    tiq_sel_t [N_EPL-1:0] dequeue_tiq_sel;
    
// PRC sends segment counts to LCM
modport prc(
    // port list
    output dequeue_req,
    output dequeue_toq_sel,
    output dequeue_tiq_sel
    );

// LCM receives segment counts from PRC
modport lcm(
    // port list
    input dequeue_req,
    input dequeue_toq_sel,
    input dequeue_tiq_sel
    );

endinterface : egr_prc_lcm_if
