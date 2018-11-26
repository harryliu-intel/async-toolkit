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

module pfs
(
    input logic           clk,
    input logic         rst_n,

    // Internal interfaces
    egr_dpb_pfs_if.pfs dpb_if, // DPB requests to PFS
    egr_pfs_prc_if.pfs prc_if, // PFS requests to PRC
    egr_lcm_pfs_if.pfs lcm_if, // PFS provides to LCM
    egr_tmu_pfs_if.pfs tmu_if, // PFS provides to TMU
    egr_tcu_pfs_if.pfs tcu_if, // PFS provides to TMU

);

endmodule : pfs
