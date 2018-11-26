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
// -- Description  : Packet Read Controller
//------------------------------------------------------------------------------

module prc 
(
    input logic             clk,
    input logic           rst_n, 

    //EGR Internal Interfaces    
    egr_dp_if.requestor  dpb_if, //Dirty Pointer Interface. Requests from the Dirty Pointer Broker
    egr_prc_lcm_if.prc   lcm_if, //Packet Read Controller - Local Congestion Manager Interface
    egr_tmu_prc_if.prc   tmu_if, //Tag Management Unit    - Packet Read Controller   Interface
    egr_pfs_prc_if.prc   pfs_if, //Packet Fetch Scheduler - Packet Read Controller   Interface
    egr_rrq_if.requestor mri_if, //Read Request Interface.  Requests from the Mesh Read Interface
    egr_prc_tqu_if.prc   tqu_if  //Transmit Queuing Unit  - Packet Read Controller   Interface

);

endmodule : prc
