//------------------------------------------------------------------------------
//
//  INTEL CONFIDENTIAL
//
//  Copyright 2018 Intel Corporation All Rights Reserved.
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
// CSRS Include:  mby_mesh_csr_include.sv
// Creator:       solson
// Time:          Wed Dec 12 15:47:07 2018
//
// Created from:  mby_mesh_row_map.sv
// Created by:    /nfs/sc/disks/slx_1593/solson/mby/work_root/i_csrs/csrs_include.pl
//

// ** NOTE ** If you have `include "rtlgen_pkg_v6.vh" in your top level modules, change it to the latest version to compile properly
//
import mby_mesh_row_map_pkg::*;
import rtlgen_pkg_mby_top_map::*;
// ** NOTE ** The following _pkg includes should be copied to outside the calling module declaration
//
// `include "rtlgen_pkg_mby_top_map.vh"
// `include "mby_mesh_row_map_pkg.vh"

// Wires from: mby_mesh_row_map.sv
//
// Clocks
// logic                                                                  gated_clk;

// Resets
// logic                                                                  rst_n;


// Register Inputs


// Register Outputs
MESH_ARB_RREQ_Y_t                                                      [7:0] MESH_ARB_RREQ_Y;
MESH_ARB_RRSP_X_t                                                      [7:0] MESH_ARB_RRSP_X;
MESH_ARB_RRSP_Y_t                                                      [7:0] MESH_ARB_RRSP_Y;
MESH_ARB_WREQ_Y_t                                                      [7:0] MESH_ARB_WREQ_Y;


// Register signals for HandCoded registers



// Config Access
mby_mesh_row_map_cr_req_t                                              req;
mby_mesh_row_map_cr_ack_t                                              ack;
    



mby_mesh_row_map mby_mesh_row_map_inst (
    // Clocks
    .gated_clk(mclk),

    // Resets
    .rst_n(mshreset),


    // Register Inputs


    // Register Outputs
    .MESH_ARB_RREQ_Y(MESH_ARB_RREQ_Y),
    .MESH_ARB_RRSP_X(MESH_ARB_RRSP_X),
    .MESH_ARB_RRSP_Y(MESH_ARB_RRSP_Y),
    .MESH_ARB_WREQ_Y(MESH_ARB_WREQ_Y),


    // Register signals for HandCoded registers



    // Config Access
    .req(req),
    .ack(ack)
    

);

