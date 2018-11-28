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
///  -- Description  : Tag Management Unit (TMU) module. 
///                    Submodule of the Egress (EGR) partition.
///  ------------------------------------------------------------------------------

module tmu
(
    input logic                       clk,
    input logic                     rst_n,

    // Internal inferfaces
    egr_dp_if.requestor            dpb_if, // TMU requests from DPB
    egr_pfs_tmu_if.tmu             pfs_if, // PFS requests from TMU
    egr_prc_tmu_if.tmu             prc_if, // PRC requests from TMU

    egr_rrq_if.requestor       rrq_mri_if, // TMU requests from MRI
    egr_rrs_if.requestor       rrs_mri_if, // TMU receives responses from MRI

    // External interfaces
    egr_tagring_if.egr         tagring_if,
    egr_mce_tagring_if.egr mce_tagring_if
);

endmodule : tmu
