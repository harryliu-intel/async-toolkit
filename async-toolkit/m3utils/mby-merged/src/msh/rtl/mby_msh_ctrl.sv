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
// -- Author : Jim McCormick <jim.mccormick@intel.com>
// -- Project Name : Madison Bay (MBY) 
// -- Description  : This file defines a mesh node 
// 
// -- Pipeline Stages: 
//
//      ... TBD ... 
//                                                                                  ---------------
// ---------------------------------------------------------------------------------------------------------------------

// note that the dashed lines above represents recommended viewing window width


`include "mby_msh_defines.vh"                                  // include file with `defines 

module mby_msh_ctrl 
import mby_msh_pkg::*;                                        // import declarations from mby_msh_pkg.sv
(

    input                   mclk,                                 // mesh clock                                 
    input                   i_reset,                              // reset

    input  msh_col_wr_req_t i_nb_wr_req  [NUM_MSH_PLANES-1:0],    // incoming northbound write request 
    input  msh_col_wr_req_t i_sb_wr_req  [NUM_MSH_PLANES-1:0],    // incoming southbound write request 
    input  msh_row_wr_req_t i_eb_wr_req  [NUM_MSH_PLANES-1:0],    // incoming eastbound write request 
    input  msh_row_wr_req_t i_wb_wr_req  [NUM_MSH_PLANES-1:0],    // incoming westbound write request 

    input  msh_col_rd_req_t i_nb_rd_req  [NUM_MSH_PLANES-1:0],    // incoming northbound read request 
    input  msh_col_rd_req_t i_sb_rd_req  [NUM_MSH_PLANES-1:0],    // incoming southbound read request 
    input  msh_row_rd_req_t i_eb_rd_req  [NUM_MSH_PLANES-1:0],    // incoming eastbound read request 
    input  msh_row_rd_req_t i_wb_rd_req  [NUM_MSH_PLANES-1:0],    // incoming westbound read request 

    input  msh_col_rd_rsp_t i_nb_rd_rsp  [NUM_MSH_PLANES-1:0],    // incoming northbound read response 
    input  msh_col_rd_rsp_t i_sb_rd_rsp  [NUM_MSH_PLANES-1:0],    // incoming southbound read response 
    input  msh_row_rd_rsp_t i_eb_rd_rsp  [NUM_MSH_PLANES-1:0],    // incoming eastbound read response 
    input  msh_row_rd_rsp_t i_wb_rd_rsp  [NUM_MSH_PLANES-1:0],    // incoming westbound read response 

    // credit returns for the corresponding incoming messages
    input  logic             i_crdt_rtn_for_nb_wr_req   [NUM_MSH_PLANES-1:0],
    input  logic             i_crdt_rtn_for_sb_wr_req   [NUM_MSH_PLANES-1:0],
    input  msh_row_crdts_t   i_crdt_rtns_for_eb_wr_reqs [NUM_MSH_PLANES-1:0],
    input  msh_row_crdts_t   i_crdt_rtns_for_wb_wr_reqs [NUM_MSH_PLANES-1:0],

    input  logic             i_crdt_rtn_for_nb_rd_req   [NUM_MSH_PLANES-1:0],
    input  logic             i_crdt_rtn_for_sb_rd_req   [NUM_MSH_PLANES-1:0],
    input  msh_row_crdts_t   i_crdt_rtns_for_eb_rd_reqs [NUM_MSH_PLANES-1:0],
    input  msh_row_crdts_t   i_crdt_rtns_for_wb_rd_reqs [NUM_MSH_PLANES-1:0],

    input  logic             i_crdt_rtn_for_nb_rd_rsp   [NUM_MSH_PLANES-1:0],
    input  logic             i_crdt_rtn_for_sb_rd_rsp   [NUM_MSH_PLANES-1:0],
    input  logic             i_crdt_rtn_for_eb_rd_rsp   [NUM_MSH_PLANES-1:0],
    input  logic             i_crdt_rtn_for_wb_rd_rsp   [NUM_MSH_PLANES-1:0],


    // outputs
   
    output msh_col_wr_req_t o_nb_wr_req  [NUM_MSH_PLANES-1:0],    // outgoing northbound write request 
    output msh_col_wr_req_t o_sb_wr_req  [NUM_MSH_PLANES-1:0],    // outgoing southbound write request 
    output msh_row_wr_req_t o_eb_wr_req  [NUM_MSH_PLANES-1:0],    // outgoing eastbound write request 
    output msh_row_wr_req_t o_wb_wr_req  [NUM_MSH_PLANES-1:0],    // outgoing westbound write request 

    output msh_col_rd_req_t o_nb_rd_req  [NUM_MSH_PLANES-1:0],    // outgoing northbound read request 
    output msh_col_rd_req_t o_sb_rd_req  [NUM_MSH_PLANES-1:0],    // outgoing southbound read request 
    output msh_row_rd_req_t o_eb_rd_req  [NUM_MSH_PLANES-1:0],    // outgoing eastbound read request 
    output msh_row_rd_req_t o_wb_rd_req  [NUM_MSH_PLANES-1:0],    // outgoing westbound read request 

    output msh_col_rd_rsp_t o_nb_rd_rsp  [NUM_MSH_PLANES-1:0],    // outgoing northbound read response 
    output msh_col_rd_rsp_t o_sb_rd_rsp  [NUM_MSH_PLANES-1:0],    // outgoing southbound read response 
    output msh_row_rd_rsp_t o_eb_rd_rsp  [NUM_MSH_PLANES-1:0],    // outgoing eastbound read response 
    output msh_row_rd_rsp_t o_wb_rd_rsp  [NUM_MSH_PLANES-1:0],    // outgoing westbound read response 

    // credit returns for the corresponding incoming messages
    output logic            o_crdt_rtn_for_nb_wr_req   [NUM_MSH_PLANES-1:0],
    output logic            o_crdt_rtn_for_sb_wr_req   [NUM_MSH_PLANES-1:0],
    output msh_row_crdts_t  o_crdt_rtns_for_eb_wr_reqs [NUM_MSH_PLANES-1:0],
    output msh_row_crdts_t  o_crdt_rtns_for_wb_wr_reqs [NUM_MSH_PLANES-1:0],

    output logic            o_crdt_rtn_for_nb_rd_req   [NUM_MSH_PLANES-1:0],
    output logic            o_crdt_rtn_for_sb_rd_req   [NUM_MSH_PLANES-1:0],
    output msh_row_crdts_t  o_crdt_rtns_for_eb_rd_reqs [NUM_MSH_PLANES-1:0],
    output msh_row_crdts_t  o_crdt_rtns_for_wb_rd_reqs [NUM_MSH_PLANES-1:0],

    output logic            o_crdt_rtn_for_nb_rd_rsp   [NUM_MSH_PLANES-1:0],
    output logic            o_crdt_rtn_for_sb_rd_rsp   [NUM_MSH_PLANES-1:0],
    output logic            o_crdt_rtn_for_eb_rd_rsp   [NUM_MSH_PLANES-1:0],
    output logic            o_crdt_rtn_for_wb_rd_rsp   [NUM_MSH_PLANES-1:0]

);


//-----------------------------------------------------------------------------
// Declarations 
//-----------------------------------------------------------------------------

// Pipeline stage names are appended to the end of signal names to ease understanding of signal timing relationships.
// Any equation that mixes signals with different pipeline stage names should be given careful thought.

/// ... stage signals

//-----------------------------------------------------------------------------
// ..., ... stages :  <short logic description>
//-----------------------------------------------------------------------------

mby_msh_wr_req wr_req (

    .mclk   (mclk)

);

mby_msh_rd_req rd_req (

    .mclk   (mclk)

);

//**********************************************************************************************************************
// OUTPUT SECTION 
//**********************************************************************************************************************

//-----------------------------------------------------------------------------
// Assign Output Signals
//-----------------------------------------------------------------------------


endmodule // mby_msh_ctrl
