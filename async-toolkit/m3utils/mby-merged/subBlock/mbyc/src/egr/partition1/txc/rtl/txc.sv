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
// -- Description  : Transmit Controller
//------------------------------------------------------------------------------

module txc
(
    input logic       clk,
    input logic     rst_n, 

    //EGR Internal Interfaces
    txc_prc_if.txc prc_if, //Transmit Controller      - Packet Read Controller Interface
    
    epb_txc_if.txc epb_if, //Egress Packet Buffer     - Transmit Controller    Interface
    pes_txc_if.txc pes_if, //Packet Egress Scheduler  - Transmit Controller    Interface
    lcm_txc_if.txc lcm_if, //Local Congestion Manager - Transmit Controller    Interface

    //EGR External Interfaces
    egr_epl_if.egr epl_if0, //TXC - EPL0 Interface
    egr_epl_if.egr epl_if1, //TXC - EPL1 Interface
    egr_epl_if.egr epl_if2, //TXC - EPL2 Interface
    egr_epl_if.egr epl_if3  //TXC - EPL3 Interface

);

endmodule : txc
