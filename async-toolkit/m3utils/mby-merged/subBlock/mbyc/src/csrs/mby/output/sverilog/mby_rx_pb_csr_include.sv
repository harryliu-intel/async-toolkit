//------------------------------------------------------------------------------
//
//  INTEL CONFIDENTIAL
//
//  Copyright 2019 Intel Corporation All Rights Reserved.
//
//  The source code contained or described herein and all documents related
//  to the source code ("Material") are owned by Intel Corporation or its
//  suppliers or licensors. Title to the Material remains with Intel
//  Corporation or its suppliers and licensors. The Material contains trade
//  secrets and proprietary and confidential information of Intel or its
//  suppliers and licensors. The Material is protected by worldwide copyright
//  and trade secret laws and treaty provisions. No part of the Material may
//  be used, copied, reproduced, modified, published, uploaded, posted,
//  transmitted, distributed, or disclosed in any way without Intel's prior
//  express written permission.
//
//  No license under any patent, copyright, trade secret or other intellectual
//  property right is granted to or conferred upon you by disclosure or
//  delivery of the Materials, either expressly, by implication, inducement,
//  estoppel or otherwise. Any license under such intellectual property rights
//  must be express and approved by Intel in writing.
//
//------------------------------------------------------------------------------
//
// CSRS Include:  mby_rx_pb_csr_include.sv
// Creator:       edwardro
// Time:          Thu Jan 10 09:39:20 2019
//
// Created from:  mby_rx_pb.sv
// Created by:    ../i_csrs/csrs_include.pl
//

// ** NOTE ** If you have `include "rtlgen_pkg_v6.vh" in your top level modules, change it to the latest version to compile properly
//
import mby_rx_pb_pkg::*;
import rtlgen_pkg_mby_rx_pb_map::*;
// ** NOTE ** The following _pkg includes should be copied to outside the calling module declaration
//
// `include "rtlgen_pkg_mby_rx_pb_map.vh"
// `include "mby_rx_pb_pkg.vh"

// Wires from: mby_rx_pb.sv
//
// Clocks
// logic                                                                  gated_clk;

// Resets
// logic                                                                  rst_n;


// Register Inputs
RX_PB_WM_t                                                             handcode_reg_rdata_RX_PB_WM;

handcode_rvalid_RX_PB_WM_t                                             handcode_rvalid_RX_PB_WM;

handcode_wvalid_RX_PB_WM_t                                             handcode_wvalid_RX_PB_WM;

handcode_error_RX_PB_WM_t                                              handcode_error_RX_PB_WM;


// Register Outputs
RX_PB_PORT_CFG_t                                                       RX_PB_PORT_CFG;


// Register signals for HandCoded registers
RX_PB_WM_t                                                             handcode_reg_wdata_RX_PB_WM;

we_RX_PB_WM_t                                                          we_RX_PB_WM;

re_RX_PB_WM_t                                                          re_RX_PB_WM;




// Config Access
mby_rx_pb_cr_req_t                                                     req;
mby_rx_pb_cr_ack_t                                                     ack;
    



mby_rx_pb mby_rx_pb_inst (
    // Clocks
    .gated_clk(cclk),

    // Resets
    .rst_n(rst),


    // Register Inputs
    .handcode_reg_rdata_RX_PB_WM(handcode_reg_rdata_RX_PB_WM),

    .handcode_rvalid_RX_PB_WM(handcode_rvalid_RX_PB_WM),

    .handcode_wvalid_RX_PB_WM(handcode_wvalid_RX_PB_WM),

    .handcode_error_RX_PB_WM(handcode_error_RX_PB_WM),


    // Register Outputs
    .RX_PB_PORT_CFG(RX_PB_PORT_CFG),


    // Register signals for HandCoded registers
    .handcode_reg_wdata_RX_PB_WM(handcode_reg_wdata_RX_PB_WM),

    .we_RX_PB_WM(we_RX_PB_WM),

    .re_RX_PB_WM(re_RX_PB_WM),




    // Config Access
    .req(req),
    .ack(ack)
    

);

