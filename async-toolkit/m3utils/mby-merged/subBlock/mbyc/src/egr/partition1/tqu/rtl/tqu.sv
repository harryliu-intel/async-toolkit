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
// -- Description  : Tag Queuing Unit
//------------------------------------------------------------------------------

module tqu
(
    input logic             clk,
    input logic           rst_n, 

    //EGR Internal Interfaces 
    egr_prc_tqu_if.tqu   prc_if, // Packet Read Controller   - Transmit Queuing Unit Interface
    egr_tcu_tqu_if.tqu   tcu_if, // Transmit Controller Unit - Transmit Queunig Unit Interface

    egr_rrs_if.requestor mri_if  // Read Response Interface. Connected with the Mesh Read Interface

);

endmodule : tqu
