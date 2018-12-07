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
    import egr_int_pkg::*;
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
//modport tcu(
//    //Service dtq_ctrl_pull
//    output  dtq_ctrl_pull,
//    input  dtq_ctrl_ready,
//    input      ctrl_mdata,
//    input       ctrl_word,
//    input ctrl_word_valid,
//    //Service dtq_data_pull
//    output  dtq_data_pull,
//    input  dtq_data_ready,
//    input  pkt_word_mdata,
//    input        pkt_word,
//    input data_word_valid
//    );
//tqu_if.ctrl_word
//                     EPL
//                      |  LP
//                      |  |  TC 
//                      |  |  |
//                      V  V  V
//tqu_if.dtq_data_ready[0][0][0]

///////////////////////////////////////////////////////////////////////////////
// JMG: TEMP CODE FOR FIRST_PACKET
// vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv

always_comb pfs_if.pfc = 0;
always_comb begin
    tqu_if.dtq_ctrl_pull = 0;
    tqu_if.dtq_ctrl_pull[0] = tqu_if.dtq_ctrl_ready;
    tqu_if.dtq_data_pull = 0;
    tqu_if.dtq_data_pull[0] = tqu_if.dtq_data_ready;
    
    epl_if0.tx_data_valid = 0;
    epl_if1.tx_data_valid = 0;
    epl_if2.tx_data_valid = 0;
    epl_if3.tx_data_valid = 0;
    
    epl_if0.tx_data_valid[0] = tqu_if.data_word_valid[0];
    epl_if0.tx_data_valid[0] = tqu_if.data_word_valid[0];
    epl_if0.tx0_data_w_ecc.data = tqu_if.pkt_word[0][0*8+:8];
    epl_if0.tx1_data_w_ecc.data = tqu_if.pkt_word[0][1*8+:8];
    epl_if0.tx2_data_w_ecc.data = tqu_if.pkt_word[0][2*8+:8];
    epl_if0.tx3_data_w_ecc.data = tqu_if.pkt_word[0][3*8+:8];
    epl_if0.tx4_data_w_ecc.data = tqu_if.pkt_word[0][4*8+:8];
    epl_if0.tx5_data_w_ecc.data = tqu_if.pkt_word[0][5*8+:8];
    epl_if0.tx6_data_w_ecc.data = tqu_if.pkt_word[0][6*8+:8];
    epl_if0.tx7_data_w_ecc.data = tqu_if.pkt_word[0][7*8+:8];
end

// ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
// JMG: END TEMP CODE FOR FIRST_PACKET
///////////////////////////////////////////////////////////////////////////////

endmodule : tcu
