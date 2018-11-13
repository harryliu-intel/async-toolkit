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
// -- Description  : This is the interface for interconnecting 
//                   Egress (EGR) with the EPL
//------------------------------------------------------------------------------

interface egr_epl_if
  import mby_egr_pkg::*;
();
//TODO Cleanup signals    
//    logic [W_EPL_TX_ECC-1:0]                         tx_ecc;        
//    logic [W_EPL_TX_ENABLE_PORT_NUM-1:0] tx_enable_port_num;
//    logic                                         tx_enable;
//    logic [W_EPL_TX_DATA_VALID-1:0]           tx_data_valid;
//    logic [W_EPL_TX_PORT_NUM-1:0]               tx_port_num;
//    logic                                     tx_valid_resp;
//    logic [W_EPL_TX_METADATA-1:0]               tx_metadata;
//    logic [W_EPL_TX_DATA_W_ECC-1:0]          tx0_data_w_ecc;
//    logic [W_EPL_TX_DATA_W_ECC-1:0]          tx1_data_w_ecc;
//    logic [W_EPL_TX_DATA_W_ECC-1:0]          tx2_data_w_ecc;
//    logic [W_EPL_TX_DATA_W_ECC-1:0]          tx3_data_w_ecc;
//    logic [W_EPL_TX_DATA_W_ECC-1:0]          tx4_data_w_ecc;
//    logic [W_EPL_TX_DATA_W_ECC-1:0]          tx5_data_w_ecc;
//    logic [W_EPL_TX_DATA_W_ECC-1:0]          tx6_data_w_ecc;
//    logic [W_EPL_TX_DATA_W_ECC-1:0]          tx7_data_w_ecc;
//    logic                                       tx_pfc_xoff; 
//    logic [W_EPL_TX_FLOW_CONTROL_TC-1:0] tx_flow_control_tc;
  
    epl_tx_ecc_t                         tx_ecc;        
    epl_tx_enable_port_num_t tx_enable_port_num;
    epl_tx_enable_t                   tx_enable;
    epl_tx_data_valid_t           tx_data_valid;
    epl_tx_port_num_t               tx_port_num;
    epl_tx_valid_resp_t           tx_valid_resp;
    epl_tx_metadata_t               tx_metadata;
    epl_tx_flit_w_ecc_t          tx0_data_w_ecc;
    epl_tx_flit_w_ecc_t          tx1_data_w_ecc;
    epl_tx_flit_w_ecc_t          tx2_data_w_ecc;
    epl_tx_flit_w_ecc_t          tx3_data_w_ecc;
    epl_tx_flit_w_ecc_t          tx4_data_w_ecc;
    epl_tx_flit_w_ecc_t          tx5_data_w_ecc;
    epl_tx_flit_w_ecc_t          tx6_data_w_ecc;
    epl_tx_flit_w_ecc_t          tx7_data_w_ecc;
    epl_tx_pfc_xoff_t               tx_pfc_xoff; 
    epl_tx_flow_control_tc_t tx_flow_control_tc;
    
modport egr(
    output            tx_ecc,
    input tx_enable_port_num,
    input          tx_enable,
    output     tx_data_valid,
    output       tx_port_num,
    output     tx_valid_resp,
    output       tx_metadata,
    output    tx0_data_w_ecc,
    output    tx1_data_w_ecc,
    output    tx2_data_w_ecc,
    output    tx3_data_w_ecc,
    output    tx4_data_w_ecc,
    output    tx5_data_w_ecc,
    output    tx6_data_w_ecc,
    output    tx7_data_w_ecc,
    output       tx_pfc_xoff,
    output tx_flow_control_tc
    );

modport epl(
    input              tx_ecc,
    output tx_enable_port_num,
    output          tx_enable,
    input       tx_data_valid,
    input         tx_port_num,
    input       tx_valid_resp,
    input         tx_metadata,
    input      tx0_data_w_ecc,
    input      tx1_data_w_ecc,
    input      tx2_data_w_ecc,
    input      tx3_data_w_ecc,
    input      tx4_data_w_ecc,
    input      tx5_data_w_ecc,
    input      tx6_data_w_ecc,
    input      tx7_data_w_ecc,
    input         tx_pfc_xoff,
    input tx_flow_control_tc
    );
    
    
    
endinterface : egr_epl_if
