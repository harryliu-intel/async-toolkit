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
// ---------------------------------------------------------------------------------------------------------------------
// -- Author : Luis Alfonso Maeda-Nunez
// -- Project Name : Madison Bay (MBY) 
// -- Description  : Mesh Read Interface
//------------------------------------------------------------------------------

module mri
(
    input logic       clk,
    input logic     rst_n, 

    //EGR Internal Interfaces    
    egr_rrq_if.mri rrq_cpb_if, //Read Request  Interface. Provides to Clean Pointer Broker
    egr_rrs_if.mri rrs_cpb_if, //Read Response Interface. Provides to Clean Pointer Broker
    
    egr_rrq_if.mri rrq_tmu_if, //Read Request  Interface. Provides to Tag Management Unit 
    egr_rrs_if.mri rrs_tmu_if, //Read Response Interface. Provides to Tag Management Unit 
    
    egr_rrq_if.mri rrq_prc_if, //Read Request  Interface. Provides to Packet Read Controller 
    egr_rrs_if.mri rrs_tqu_if, //Read Response Interface. Provides to Transmit Queuing Unit 

    //EGR External Interfaces
    mim_rd_if.request    mim_rd_if0_0, //MRI-MIM Read Interface Row 0 Line 0
    mim_rd_if.request    mim_rd_if0_1, //MRI-MIM Read Interface Row 0 Line 1
    mim_rd_if.request    mim_rd_if0_2, //MRI-MIM Read Interface Row 0 Line 2

    mim_rd_if.request    mim_rd_if1_0, //MRI-MIM Read Interface Row 1 Line 0
    mim_rd_if.request    mim_rd_if1_1, //MRI-MIM Read Interface Row 1 Line 1
    mim_rd_if.request    mim_rd_if1_2, //MRI-MIM Read Interface Row 1 Line 2
    
);

endmodule : mri
