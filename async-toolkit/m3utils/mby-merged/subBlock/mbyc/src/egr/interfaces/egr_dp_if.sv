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
///  -- Description  : Dirty Pod Broker (DPB) interface with
///                    Tag Management Unit (TMU),
///                    Packet Read Controller (PRC) and 
///  ------------------------------------------------------------------------------

interface egr_dp_if #(parameter N_PAR_PTRS = 1)();
    // Import EGR package
    import mby_egr_pkg::*;
    
    //signals
    logic                                stall;
    logic        [N_PAR_PTRS-1:0] ptr_rel_reqs;
    seg_handle_t [N_PAR_PTRS-1:0]     seg_ptrs;

// TMU, PRC request from DPB
modport requestor(
    //port list
    input         stall,
    output ptr_rel_reqs,
    output     seg_ptrs
    );

// DPB provides to TMU, PRC
modport dpb(
    //port list
    output        stall,
    input  ptr_rel_reqs,
    input      seg_ptrs
    );

endinterface : egr_dp_if

