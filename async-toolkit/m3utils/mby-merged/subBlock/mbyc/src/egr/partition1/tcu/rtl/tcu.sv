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
// -- Description  : Transmit Controller Unit
//------------------------------------------------------------------------------

module tcu
(
    input logic       clk,
    input logic     rst_n, 

    //EGR Internal Interfaces
    egr_tcu_pfs_if.tcu pfs_if, //Transmit Controller Unit - Packet Fetch Scheduler Interface
    egr_tcu_tqu_if.tcu tqu_if, //Transmit Controller Unit - Transmit Queuing Unit Interface

    //EGR External Interfaces

    egr_tx_ppe_if.egr     tx_ppe_if0, //EGR-TxPPE 0 Interface //TODO Check how many interfaces needed
    egr_tx_ppe_if.egr     tx_ppe_if1, //EGR-TxPPE 1 Interface //TODO Check how many interfaces needed
    egr_ppe_stm_if.egr    ppe_stm_if, //EGR-PPE Shared Table Memory Interface  //TODO Check how many interfaces needed
    egr_mc_table_if.egr mc_table_if0, //EGR-MultiCast Shared Table 0 Interface //TODO Check how many interfaces needed
    egr_mc_table_if.egr mc_table_if1, //EGR-MultiCast Shared Table 1 Interface //TODO Check how many interfaces needed
    //TODO Interface to ACL TABLE

    egr_epl_if.egr        epl_if0, //TCU - EPL0 Interface
    egr_epl_if.egr        epl_if1, //TCU - EPL1 Interface
    egr_epl_if.egr        epl_if2, //TCU - EPL2 Interface
    egr_epl_if.egr        epl_if3  //TCU - EPL3 Interface

);

endmodule : tcu
