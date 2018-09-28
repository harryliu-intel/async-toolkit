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
// -- Description  : This is the top level of the mesh.  It defines an interconnected 2-dimensional array of mesh nodes. 
//
//      Node array indexing 
//      -------------------
//
//      Nodes (N) are instantiated as a row x column array (N[row][col]) with the northmost row being row 0 and the westmost 
//      column being column 0:
//  
//                                       (north)
//                 |---------|---------|---------|---------|---------|
//                 |         | N[0][0] |         | N[0][1] |         | 
//          (west) |---------|---------|---------|---------|---------| ... (east)
//                 |         | N[1][0] |         | N[1][1] |         | 
//                 |---------|---------|---------|---------|---------|
//                                         ...
//                                       (south)
//
//  Inter node signal aray indexing
//  -------------------------------
//      <signal name>_in:   indexed the same as the node it is entering 
//      <signal name>_out:  indexed the same as the node it is leaving
//
//
//  Signal naming conventions
//  -------------------------
//  msh         = mesh
//  col         = column
//
//  nb          = northbound
//  sb          = southbound
//  eb          = eastbound
//  wb          = westbound
//
//  wr_req      = write request
//  wr_dbus     = write data bus
//  rd_req      = read request
//  rd_rsp      = read response
//  rd_dbus     = read data bus
//
//  crdt_rtn    = credit return
//
//
// -- Pipeline Stages: 
//
//      ... TBD ... 
//                                                                                  ---------------
// -- Instantiation Hierarchy:                                                      definition file
//                                                                                  ---------------
//          module  msh                                                             msh.sv
//
//              `include "msh_defines.vh"                                           msh_defines.vh
//              import msh_pkg                                                      msh_pkg.sv
//
//              for (genvar gv_row=0; gv_row < NUM_MSH_ROWS; gv_row++) begin : mesh_rows
//                  for (genvar gv_col=0; gv_col < NUM_MSH_colS; gv_col++) begin : mesh_cols
//
//                      msh_node    msh_node                                        msh_node.sv
//
//                          msh_ctrl    msh_ctrl                                    msh_ctrl.sv
//                              msh_wr_req  msh_wr_req                              msh_wr_req.sv
//                              msh_rd_req  msh_rd_req                              msh_rd_req.sv
//
//                          for (genvar gv_c=0; gv_c < NUM_MSH_DP_CHUNKS; gv_c++) begin : dp_chunks 
//
//                              msh_dp      msh_dp                                  msh_dp.sv
//                                  msh_wr_dp   msh_wr_dp                           msh_wr_dp.sv
//                                  msh_rd_dp   msh_rd_dp                           msh_rd_dp.sv
//                                  msh_mem_dp  msh_mem_dp                          msh_mem_dp.sv
//
//                                  for (genvar gv_b=0; gv_b < NUM_MSH_NODE_MEM_BANKS; gv_b++) begin : mem_banks
//
//                                      msh_mem     msh_mem                         msh_mem_dp.sv
//
//                                  end : mem_banks
//                          end : dp_chunks
//
//                  end : msh_cols
//              end : msh_rows
//
// 
// ---------------------------------------------------------------------------------------------------------------------

// note that the dashed lines above represents recommended viewing window width

`include "msh_defines.vh";                                     // include file with `defines 

module msh 
import msh_pkg::*;                                             // import declarations from msh_pkg.sv
(

    // inputs
    
    input                   mclk,                                  // mesh clock                                 
    input                   i_reset,                               // reset

    input  msh_col_wr_req_t i_nb_wr_req                                   [NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0],
    input  msh_col_wr_req_t i_sb_wr_req                                   [NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0],
    input  msh_row_wr_req_t i_eb_wr_req                 [NUM_MSH_ROWS-1:0]                  [NUM_MSH_PLANES-1:0],
    input  msh_row_wr_req_t i_wb_wr_req                 [NUM_MSH_ROWS-1:0]                  [NUM_MSH_PLANES-1:0],

    input  msh_dbus_t       i_nb_wr_dbus                                  [NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0],
    input  msh_dbus_t       i_sb_wr_dbus                                  [NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0],
    input  msh_dbus_t       i_eb_wr_dbus                [NUM_MSH_ROWS-1:0]                  [NUM_MSH_PLANES-1:0],
    input  msh_dbus_t       i_wb_wr_dbus                [NUM_MSH_ROWS-1:0]                  [NUM_MSH_PLANES-1:0],

    input  msh_col_rd_req_t i_nb_rd_req                                   [NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0],
    input  msh_col_rd_req_t i_sb_rd_req                                   [NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0],
    input  msh_row_rd_req_t i_eb_rd_req                 [NUM_MSH_ROWS-1:0]                  [NUM_MSH_PLANES-1:0],
    input  msh_row_rd_req_t i_wb_rd_req                 [NUM_MSH_ROWS-1:0]                  [NUM_MSH_PLANES-1:0],

    input  msh_col_rd_rsp_t i_nb_rd_rsp                                   [NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0],
    input  msh_col_rd_rsp_t i_sb_rd_rsp                                   [NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0],
    input  msh_row_rd_rsp_t i_eb_rd_rsp                 [NUM_MSH_ROWS-1:0]                  [NUM_MSH_PLANES-1:0],
    input  msh_row_rd_rsp_t i_wb_rd_rsp                 [NUM_MSH_ROWS-1:0]                  [NUM_MSH_PLANES-1:0],

    input  msh_dbus_t       i_nb_rd_dbus                                  [NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0],
    input  msh_dbus_t       i_sb_rd_dbus                                  [NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0],
    input  msh_dbus_t       i_eb_rd_dbus                [NUM_MSH_ROWS-1:0]                  [NUM_MSH_PLANES-1:0],
    input  msh_dbus_t       i_wb_rd_dbus                [NUM_MSH_ROWS-1:0]                  [NUM_MSH_PLANES-1:0],

    input  logic             i_crdt_rtn_for_nb_wr_req                     [NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0],
    input  logic             i_crdt_rtn_for_sb_wr_req                     [NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0],
    input  msh_row_crdts_t   i_crdt_rtns_for_eb_wr_reqs [NUM_MSH_ROWS-1:0]                  [NUM_MSH_PLANES-1:0],
    input  msh_row_crdts_t   i_crdt_rtns_for_wb_wr_reqs [NUM_MSH_ROWS-1:0]                  [NUM_MSH_PLANES-1:0],

    input  logic             i_crdt_rtn_for_nb_rd_req                     [NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0],
    input  logic             i_crdt_rtn_for_sb_rd_req                     [NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0],
    input  msh_row_crdts_t   i_crdt_rtns_for_eb_rd_reqs [NUM_MSH_ROWS-1:0]                  [NUM_MSH_PLANES-1:0],
    input  msh_row_crdts_t   i_crdt_rtns_for_wb_rd_reqs [NUM_MSH_ROWS-1:0]                  [NUM_MSH_PLANES-1:0],

    input  logic             i_crdt_rtn_for_nb_rd_rsp                     [NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0],
    input  logic             i_crdt_rtn_for_sb_rd_rsp                     [NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0],
    input  logic             i_crdt_rtn_for_eb_rd_rsp   [NUM_MSH_ROWS-1:0]                  [NUM_MSH_PLANES-1:0],
    input  logic             i_crdt_rtn_for_wb_rd_rsp   [NUM_MSH_ROWS-1:0]                  [NUM_MSH_PLANES-1:0],


    // outputs
    
    output msh_col_wr_req_t o_nb_wr_req                                   [NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0],
    output msh_col_wr_req_t o_sb_wr_req                                   [NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0],
    output msh_row_wr_req_t o_eb_wr_req                 [NUM_MSH_ROWS-1:0]                  [NUM_MSH_PLANES-1:0],
    output msh_row_wr_req_t o_wb_wr_req                 [NUM_MSH_ROWS-1:0]                  [NUM_MSH_PLANES-1:0],

    output msh_dbus_t       o_nb_wr_dbus                                  [NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0],
    output msh_dbus_t       o_sb_wr_dbus                                  [NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0],
    output msh_dbus_t       o_eb_wr_dbus                [NUM_MSH_ROWS-1:0]                  [NUM_MSH_PLANES-1:0],
    output msh_dbus_t       o_wb_wr_dbus                [NUM_MSH_ROWS-1:0]                  [NUM_MSH_PLANES-1:0],

    output msh_col_rd_req_t o_nb_rd_req                                   [NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0],
    output msh_col_rd_req_t o_sb_rd_req                                   [NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0],
    output msh_row_rd_req_t o_eb_rd_req                 [NUM_MSH_ROWS-1:0]                  [NUM_MSH_PLANES-1:0],
    output msh_row_rd_req_t o_wb_rd_req                 [NUM_MSH_ROWS-1:0]                  [NUM_MSH_PLANES-1:0],

    output msh_col_rd_rsp_t o_nb_rd_rsp                                   [NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0],
    output msh_col_rd_rsp_t o_sb_rd_rsp                                   [NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0],
    output msh_row_rd_rsp_t o_eb_rd_rsp                 [NUM_MSH_ROWS-1:0]                  [NUM_MSH_PLANES-1:0],
    output msh_row_rd_rsp_t o_wb_rd_rsp                 [NUM_MSH_ROWS-1:0]                  [NUM_MSH_PLANES-1:0],

    output msh_dbus_t       o_nb_rd_dbus                                  [NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0],
    output msh_dbus_t       o_sb_rd_dbus                                  [NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0],
    output msh_dbus_t       o_eb_rd_dbus                [NUM_MSH_ROWS-1:0]                  [NUM_MSH_PLANES-1:0],
    output msh_dbus_t       o_wb_rd_dbus                [NUM_MSH_ROWS-1:0]                  [NUM_MSH_PLANES-1:0],

    output logic             o_crdt_rtn_for_nb_wr_req                     [NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0],
    output logic             o_crdt_rtn_for_sb_wr_req                     [NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0],
    output msh_row_crdts_t   o_crdt_rtns_for_eb_wr_reqs [NUM_MSH_ROWS-1:0]                  [NUM_MSH_PLANES-1:0],
    output msh_row_crdts_t   o_crdt_rtns_for_wb_wr_reqs [NUM_MSH_ROWS-1:0]                  [NUM_MSH_PLANES-1:0],

    output logic             o_crdt_rtn_for_nb_rd_req                     [NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0],
    output logic             o_crdt_rtn_for_sb_rd_req                     [NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0],
    output msh_row_crdts_t   o_crdt_rtns_for_eb_rd_reqs [NUM_MSH_ROWS-1:0]                  [NUM_MSH_PLANES-1:0],
    output msh_row_crdts_t   o_crdt_rtns_for_wb_rd_reqs [NUM_MSH_ROWS-1:0]                  [NUM_MSH_PLANES-1:0],

    output logic             o_crdt_rtn_for_nb_rd_rsp                     [NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0],
    output logic             o_crdt_rtn_for_sb_rd_rsp                     [NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0],
    output logic             o_crdt_rtn_for_eb_rd_rsp   [NUM_MSH_ROWS-1:0]                  [NUM_MSH_PLANES-1:0],
    output logic             o_crdt_rtn_for_wb_rd_rsp   [NUM_MSH_ROWS-1:0]                  [NUM_MSH_PLANES-1:0] 

);


//-----------------------------------------------------------------------------
// Declarations 
//-----------------------------------------------------------------------------

// messages entering mesh nodes

msh_col_wr_req_t nb_wr_req_in                   [NUM_MSH_ROWS-1:0][NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0];
msh_col_wr_req_t sb_wr_req_in                   [NUM_MSH_ROWS-1:0][NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0];
msh_row_wr_req_t eb_wr_req_in                   [NUM_MSH_ROWS-1:0][NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0];
msh_row_wr_req_t wb_wr_req_in                   [NUM_MSH_ROWS-1:0][NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0];

msh_dbus_t       nb_wr_dbus_in                  [NUM_MSH_ROWS-1:0][NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0];
msh_dbus_t       sb_wr_dbus_in                  [NUM_MSH_ROWS-1:0][NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0];
msh_dbus_t       eb_wr_dbus_in                  [NUM_MSH_ROWS-1:0][NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0];
msh_dbus_t       wb_wr_dbus_in                  [NUM_MSH_ROWS-1:0][NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0];

msh_col_rd_req_t nb_rd_req_in                   [NUM_MSH_ROWS-1:0][NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0];
msh_col_rd_req_t sb_rd_req_in                   [NUM_MSH_ROWS-1:0][NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0];
msh_row_rd_req_t eb_rd_req_in                   [NUM_MSH_ROWS-1:0][NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0];
msh_row_rd_req_t wb_rd_req_in                   [NUM_MSH_ROWS-1:0][NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0];

msh_col_rd_rsp_t nb_rd_rsp_in                   [NUM_MSH_ROWS-1:0][NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0];
msh_col_rd_rsp_t sb_rd_rsp_in                   [NUM_MSH_ROWS-1:0][NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0];
msh_row_rd_rsp_t eb_rd_rsp_in                   [NUM_MSH_ROWS-1:0][NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0];
msh_row_rd_rsp_t wb_rd_rsp_in                   [NUM_MSH_ROWS-1:0][NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0];

msh_dbus_t       nb_rd_dbus_in                  [NUM_MSH_ROWS-1:0][NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0];
msh_dbus_t       sb_rd_dbus_in                  [NUM_MSH_ROWS-1:0][NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0];
msh_dbus_t       eb_rd_dbus_in                  [NUM_MSH_ROWS-1:0][NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0];
msh_dbus_t       wb_rd_dbus_in                  [NUM_MSH_ROWS-1:0][NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0];

// credit returns entering mesh nodes

logic             crdt_rtn_for_nb_wr_req_in     [NUM_MSH_ROWS-1:0][NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0];
logic             crdt_rtn_for_sb_wr_req_in     [NUM_MSH_ROWS-1:0][NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0];
msh_row_crdts_t   crdt_rtns_for_eb_wr_reqs_in   [NUM_MSH_ROWS-1:0][NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0];
msh_row_crdts_t   crdt_rtns_for_wb_wr_reqs_in   [NUM_MSH_ROWS-1:0][NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0];

logic             crdt_rtn_for_nb_rd_req_in     [NUM_MSH_ROWS-1:0][NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0];
logic             crdt_rtn_for_sb_rd_req_in     [NUM_MSH_ROWS-1:0][NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0];
msh_row_crdts_t   crdt_rtns_for_eb_rd_reqs_in   [NUM_MSH_ROWS-1:0][NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0];
msh_row_crdts_t   crdt_rtns_for_wb_rd_reqs_in   [NUM_MSH_ROWS-1:0][NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0];

logic             crdt_rtn_for_nb_rd_rsp_in     [NUM_MSH_ROWS-1:0][NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0];
logic             crdt_rtn_for_sb_rd_rsp_in     [NUM_MSH_ROWS-1:0][NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0];
logic             crdt_rtn_for_eb_rd_rsp_in     [NUM_MSH_ROWS-1:0][NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0];
logic             crdt_rtn_for_wb_rd_rsp_in     [NUM_MSH_ROWS-1:0][NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0];

// messages leaving mesh nodes

msh_col_wr_req_t nb_wr_req_out                  [NUM_MSH_ROWS-1:0][NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0];
msh_col_wr_req_t sb_wr_req_out                  [NUM_MSH_ROWS-1:0][NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0];
msh_row_wr_req_t eb_wr_req_out                  [NUM_MSH_ROWS-1:0][NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0];
msh_row_wr_req_t wb_wr_req_out                  [NUM_MSH_ROWS-1:0][NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0];

msh_dbus_t       nb_wr_dbus_out                 [NUM_MSH_ROWS-1:0][NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0];
msh_dbus_t       sb_wr_dbus_out                 [NUM_MSH_ROWS-1:0][NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0];
msh_dbus_t       eb_wr_dbus_out                 [NUM_MSH_ROWS-1:0][NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0];
msh_dbus_t       wb_wr_dbus_out                 [NUM_MSH_ROWS-1:0][NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0];

msh_col_rd_req_t nb_rd_req_out                  [NUM_MSH_ROWS-1:0][NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0];
msh_col_rd_req_t sb_rd_req_out                  [NUM_MSH_ROWS-1:0][NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0];
msh_row_rd_req_t eb_rd_req_out                  [NUM_MSH_ROWS-1:0][NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0];
msh_row_rd_req_t wb_rd_req_out                  [NUM_MSH_ROWS-1:0][NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0];

msh_col_rd_rsp_t nb_rd_rsp_out                  [NUM_MSH_ROWS-1:0][NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0];
msh_col_rd_rsp_t sb_rd_rsp_out                  [NUM_MSH_ROWS-1:0][NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0];
msh_row_rd_rsp_t eb_rd_rsp_out                  [NUM_MSH_ROWS-1:0][NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0];
msh_row_rd_rsp_t wb_rd_rsp_out                  [NUM_MSH_ROWS-1:0][NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0];

msh_dbus_t       nb_rd_dbus_out                 [NUM_MSH_ROWS-1:0][NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0];
msh_dbus_t       sb_rd_dbus_out                 [NUM_MSH_ROWS-1:0][NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0];
msh_dbus_t       eb_rd_dbus_out                 [NUM_MSH_ROWS-1:0][NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0];
msh_dbus_t       wb_rd_dbus_out                 [NUM_MSH_ROWS-1:0][NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0];

// credit returns leaving mesh nodes

logic             crdt_rtn_for_nb_wr_req_out    [NUM_MSH_ROWS-1:0][NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0];
logic             crdt_rtn_for_sb_wr_req_out    [NUM_MSH_ROWS-1:0][NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0];
msh_row_crdts_t   crdt_rtns_for_eb_wr_reqs_out  [NUM_MSH_ROWS-1:0][NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0];
msh_row_crdts_t   crdt_rtns_for_wb_wr_reqs_out  [NUM_MSH_ROWS-1:0][NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0];

logic             crdt_rtn_for_nb_rd_req_out    [NUM_MSH_ROWS-1:0][NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0];
logic             crdt_rtn_for_sb_rd_req_out    [NUM_MSH_ROWS-1:0][NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0];
msh_row_crdts_t   crdt_rtns_for_eb_rd_reqs_out  [NUM_MSH_ROWS-1:0][NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0];
msh_row_crdts_t   crdt_rtns_for_wb_rd_reqs_out  [NUM_MSH_ROWS-1:0][NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0];

logic             crdt_rtn_for_nb_rd_rsp_out    [NUM_MSH_ROWS-1:0][NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0];
logic             crdt_rtn_for_sb_rd_rsp_out    [NUM_MSH_ROWS-1:0][NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0];
logic             crdt_rtn_for_eb_rd_rsp_out    [NUM_MSH_ROWS-1:0][NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0];
logic             crdt_rtn_for_wb_rd_rsp_out    [NUM_MSH_ROWS-1:0][NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0];



//**********************************************************************************************************************
// MESH NODE INSTANTIATIONS 
//**********************************************************************************************************************

//-----------------------------------------------------------------------------
// => ..., ... stages :  <short logic description>
//-----------------------------------------------------------------------------

generate
for (genvar gv_row=0; gv_row < NUM_MSH_ROWS; gv_row++) begin : mesh_rows
    for (genvar gv_col=0; gv_col < NUM_MSH_COLS; gv_col++) begin : mesh_cols

msh_node msh_node (

    .mclk                           (mclk                                           ),
    .i_reset                        (                                               ),

    .i_nb_wr_req                    (nb_wr_req_in                   [gv_row][gv_col]),
    .i_sb_wr_req                    (sb_wr_req_in                   [gv_row][gv_col]),
    .i_eb_wr_req                    (eb_wr_req_in                   [gv_row][gv_col]),
    .i_wb_wr_req                    (wb_wr_req_in                   [gv_row][gv_col]),

    .i_nb_wr_dbus                   (nb_wr_dbus_in                  [gv_row][gv_col]),
    .i_sb_wr_dbus                   (sb_wr_dbus_in                  [gv_row][gv_col]),
    .i_eb_wr_dbus                   (eb_wr_dbus_in                  [gv_row][gv_col]),
    .i_wb_wr_dbus                   (wb_wr_dbus_in                  [gv_row][gv_col]),

    .i_nb_rd_req                    (nb_rd_req_in                   [gv_row][gv_col]),
    .i_sb_rd_req                    (sb_rd_req_in                   [gv_row][gv_col]),
    .i_eb_rd_req                    (eb_rd_req_in                   [gv_row][gv_col]),
    .i_wb_rd_req                    (wb_rd_req_in                   [gv_row][gv_col]),

    .i_nb_rd_rsp                    (nb_rd_rsp_in                   [gv_row][gv_col]),
    .i_sb_rd_rsp                    (sb_rd_rsp_in                   [gv_row][gv_col]),
    .i_eb_rd_rsp                    (eb_rd_rsp_in                   [gv_row][gv_col]),
    .i_wb_rd_rsp                    (wb_rd_rsp_in                   [gv_row][gv_col]),

    .i_nb_rd_dbus                   (nb_rd_dbus_in                  [gv_row][gv_col]),
    .i_sb_rd_dbus                   (sb_rd_dbus_in                  [gv_row][gv_col]),
    .i_eb_rd_dbus                   (eb_rd_dbus_in                  [gv_row][gv_col]),
    .i_wb_rd_dbus                   (wb_rd_dbus_in                  [gv_row][gv_col]),

    .i_crdt_rtn_for_nb_wr_req       (crdt_rtn_for_nb_wr_req_in      [gv_row][gv_col]),
    .i_crdt_rtn_for_sb_wr_req       (crdt_rtn_for_sb_wr_req_in      [gv_row][gv_col]),
    .i_crdt_rtns_for_eb_wr_reqs     (crdt_rtns_for_eb_wr_reqs_in    [gv_row][gv_col]),
    .i_crdt_rtns_for_wb_wr_reqs     (crdt_rtns_for_wb_wr_reqs_in    [gv_row][gv_col]),

    .i_crdt_rtn_for_nb_rd_req       (crdt_rtn_for_nb_rd_req_in      [gv_row][gv_col]),
    .i_crdt_rtn_for_sb_rd_req       (crdt_rtn_for_sb_rd_req_in      [gv_row][gv_col]),
    .i_crdt_rtns_for_eb_rd_reqs     (crdt_rtns_for_eb_rd_reqs_in    [gv_row][gv_col]),
    .i_crdt_rtns_for_wb_rd_reqs     (crdt_rtns_for_wb_rd_reqs_in    [gv_row][gv_col]),

    .i_crdt_rtn_for_nb_rd_rsp       (crdt_rtn_for_nb_rd_rsp_in      [gv_row][gv_col]),
    .i_crdt_rtn_for_sb_rd_rsp       (crdt_rtn_for_sb_rd_rsp_in      [gv_row][gv_col]),
    .i_crdt_rtn_for_eb_rd_rsp       (crdt_rtn_for_eb_rd_rsp_in      [gv_row][gv_col]),
    .i_crdt_rtn_for_wb_rd_rsp       (crdt_rtn_for_wb_rd_rsp_in      [gv_row][gv_col]),

    // outputs
   
    .o_nb_wr_req                    (nb_wr_req_out                  [gv_row][gv_col]),
    .o_sb_wr_req                    (sb_wr_req_out                  [gv_row][gv_col]),
    .o_eb_wr_req                    (eb_wr_req_out                  [gv_row][gv_col]),
    .o_wb_wr_req                    (wb_wr_req_out                  [gv_row][gv_col]),

    .o_nb_wr_dbus                   (nb_wr_dbus_out                 [gv_row][gv_col]),
    .o_sb_wr_dbus                   (sb_wr_dbus_out                 [gv_row][gv_col]),
    .o_eb_wr_dbus                   (eb_wr_dbus_out                 [gv_row][gv_col]),
    .o_wb_wr_dbus                   (wb_wr_dbus_out                 [gv_row][gv_col]),

    .o_nb_rd_req                    (nb_rd_req_out                  [gv_row][gv_col]),
    .o_sb_rd_req                    (sb_rd_req_out                  [gv_row][gv_col]),
    .o_eb_rd_req                    (eb_rd_req_out                  [gv_row][gv_col]),
    .o_wb_rd_req                    (wb_rd_req_out                  [gv_row][gv_col]),

    .o_nb_rd_rsp                    (nb_rd_rsp_out                  [gv_row][gv_col]),
    .o_sb_rd_rsp                    (sb_rd_rsp_out                  [gv_row][gv_col]),
    .o_eb_rd_rsp                    (eb_rd_rsp_out                  [gv_row][gv_col]),
    .o_wb_rd_rsp                    (wb_rd_rsp_out                  [gv_row][gv_col]),

    .o_nb_rd_dbus                   (nb_rd_dbus_out                 [gv_row][gv_col]),
    .o_sb_rd_dbus                   (sb_rd_dbus_out                 [gv_row][gv_col]),
    .o_eb_rd_dbus                   (eb_rd_dbus_out                 [gv_row][gv_col]),
    .o_wb_rd_dbus                   (wb_rd_dbus_out                 [gv_row][gv_col]),

    .o_crdt_rtn_for_nb_wr_req       (crdt_rtn_for_nb_wr_req_out     [gv_row][gv_col]),
    .o_crdt_rtn_for_sb_wr_req       (crdt_rtn_for_sb_wr_req_out     [gv_row][gv_col]),
    .o_crdt_rtns_for_eb_wr_reqs     (crdt_rtns_for_eb_wr_reqs_out   [gv_row][gv_col]),
    .o_crdt_rtns_for_wb_wr_reqs     (crdt_rtns_for_wb_wr_reqs_out   [gv_row][gv_col]),
                                                                                          
    .o_crdt_rtn_for_nb_rd_req       (crdt_rtn_for_nb_rd_req_out     [gv_row][gv_col]),
    .o_crdt_rtn_for_sb_rd_req       (crdt_rtn_for_sb_rd_req_out     [gv_row][gv_col]),
    .o_crdt_rtns_for_eb_rd_reqs     (crdt_rtns_for_eb_rd_reqs_out   [gv_row][gv_col]),
    .o_crdt_rtns_for_wb_rd_reqs     (crdt_rtns_for_wb_rd_reqs_out   [gv_row][gv_col]),
                                                                                          
    .o_crdt_rtn_for_nb_rd_rsp       (crdt_rtn_for_nb_rd_rsp_out     [gv_row][gv_col]),
    .o_crdt_rtn_for_sb_rd_rsp       (crdt_rtn_for_sb_rd_rsp_out     [gv_row][gv_col]),
    .o_crdt_rtn_for_eb_rd_rsp       (crdt_rtn_for_eb_rd_rsp_out     [gv_row][gv_col]),
    .o_crdt_rtn_for_wb_rd_rsp       (crdt_rtn_for_wb_rd_rsp_out     [gv_row][gv_col])

);
    
    end : mesh_cols
end : mesh_rows
endgenerate


//**********************************************************************************************************************
// MESH NODE CONNECTIONS
//**********************************************************************************************************************

always_comb begin 
    for (int row=0; row < NUM_MSH_ROWS; row++)  begin
        for (int col=0; col < NUM_MSH_COLS; col++)  begin
            for (int plane=0; plane < NUM_MSH_PLANES; plane++) begin

                // northbound signals
                if (row == NUM_MSH_ROWS-1) begin
                    nb_wr_req_in                [row][col][plane] = i_nb_wr_req                        [col][plane];
                    nb_wr_dbus_in               [row][col][plane] = i_nb_wr_dbus                       [col][plane];
                    nb_rd_req_in                [row][col][plane] = i_nb_rd_req                        [col][plane];
                    nb_rd_rsp_in                [row][col][plane] = i_nb_rd_rsp                        [col][plane];
                    nb_rd_dbus_in               [row][col][plane] = i_nb_rd_dbus                       [col][plane];
                    crdt_rtn_for_nb_wr_req_in   [row][col][plane] = i_crdt_rtn_for_nb_wr_req           [col][plane];
                    crdt_rtn_for_nb_rd_req_in   [row][col][plane] = i_crdt_rtn_for_nb_rd_req           [col][plane];
                    crdt_rtn_for_nb_rd_rsp_in   [row][col][plane] = i_crdt_rtn_for_nb_rd_rsp           [col][plane];
                end else begin
                    nb_wr_req_in                [row][col][plane] = nb_wr_req_out               [row+1][col][plane];
                    nb_wr_dbus_in               [row][col][plane] = nb_wr_dbus_out              [row+1][col][plane];
                    nb_rd_req_in                [row][col][plane] = nb_rd_req_out               [row+1][col][plane];
                    nb_rd_rsp_in                [row][col][plane] = nb_rd_rsp_out               [row+1][col][plane];
                    nb_rd_dbus_in               [row][col][plane] = nb_rd_dbus_out              [row+1][col][plane];
                    crdt_rtn_for_nb_wr_req_in   [row][col][plane] = crdt_rtn_for_nb_wr_req_out  [row+1][col][plane];
                    crdt_rtn_for_nb_rd_req_in   [row][col][plane] = crdt_rtn_for_nb_rd_req_out  [row+1][col][plane];
                    crdt_rtn_for_nb_rd_rsp_in   [row][col][plane] = crdt_rtn_for_nb_rd_rsp_out  [row+1][col][plane];
                end
            
                // southbound signals
                
                if (row == 0) begin
                    sb_wr_req_in                [row][col][plane] = i_sb_wr_req                        [col][plane];
                    sb_wr_dbus_in               [row][col][plane] = i_sb_wr_dbus                       [col][plane];
                    sb_rd_req_in                [row][col][plane] = i_sb_rd_req                        [col][plane];
                    sb_rd_rsp_in                [row][col][plane] = i_sb_rd_rsp                        [col][plane];
                    sb_rd_dbus_in               [row][col][plane] = i_sb_rd_dbus                       [col][plane];
                    crdt_rtn_for_sb_wr_req_in   [row][col][plane] = i_crdt_rtn_for_sb_wr_req           [col][plane];
                    crdt_rtn_for_sb_rd_req_in   [row][col][plane] = i_crdt_rtn_for_sb_rd_req           [col][plane];
                    crdt_rtn_for_sb_rd_rsp_in   [row][col][plane] = i_crdt_rtn_for_sb_rd_rsp           [col][plane];
                end else begin
                    sb_wr_req_in                [row][col][plane] = sb_wr_req_out               [row-1][col][plane];
                    sb_wr_dbus_in               [row][col][plane] = sb_wr_dbus_out              [row-1][col][plane];
                    sb_rd_req_in                [row][col][plane] = sb_rd_req_out               [row-1][col][plane];
                    sb_rd_rsp_in                [row][col][plane] = sb_rd_rsp_out               [row-1][col][plane];
                    sb_rd_dbus_in               [row][col][plane] = sb_rd_dbus_out              [row-1][col][plane];
                    crdt_rtn_for_sb_wr_req_in   [row][col][plane] = crdt_rtn_for_sb_wr_req_out  [row-1][col][plane];
                    crdt_rtn_for_sb_rd_req_in   [row][col][plane] = crdt_rtn_for_sb_rd_req_out  [row-1][col][plane];
                    crdt_rtn_for_sb_rd_rsp_in   [row][col][plane] = crdt_rtn_for_sb_rd_rsp_out  [row-1][col][plane];
                end
            
                // eastbound signals
                
                if (col == 0) begin
                    eb_wr_req_in                [row][col][plane] = i_eb_wr_req                 [row]     [plane];
                    eb_wr_dbus_in               [row][col][plane] = i_eb_wr_dbus                [row]     [plane];
                    eb_rd_req_in                [row][col][plane] = i_eb_rd_req                 [row]     [plane];
                    eb_rd_rsp_in                [row][col][plane] = i_eb_rd_rsp                 [row]     [plane];
                    eb_rd_dbus_in               [row][col][plane] = i_eb_rd_dbus                [row]     [plane];
                    crdt_rtns_for_eb_wr_reqs_in [row][col][plane] = i_crdt_rtns_for_eb_wr_reqs  [row]     [plane];
                    crdt_rtns_for_eb_rd_reqs_in [row][col][plane] = i_crdt_rtns_for_eb_rd_reqs  [row]     [plane];
                    crdt_rtn_for_eb_rd_rsp_in   [row][col][plane] = i_crdt_rtn_for_eb_rd_rsp    [row]     [plane];
                end else begin
                    eb_wr_req_in                [row][col][plane] = eb_wr_req_out               [row][col-1][plane];
                    eb_wr_dbus_in               [row][col][plane] = eb_wr_dbus_out              [row][col-1][plane];
                    eb_rd_req_in                [row][col][plane] = eb_rd_req_out               [row][col-1][plane];
                    eb_rd_rsp_in                [row][col][plane] = eb_rd_rsp_out               [row][col-1][plane];
                    eb_rd_dbus_in               [row][col][plane] = eb_rd_dbus_out              [row][col-1][plane];
                    crdt_rtns_for_eb_wr_reqs_in [row][col][plane] = crdt_rtns_for_eb_wr_reqs_out[row][col-1][plane];
                    crdt_rtns_for_eb_rd_reqs_in [row][col][plane] = crdt_rtns_for_eb_rd_reqs_out[row][col-1][plane];
                    crdt_rtn_for_eb_rd_rsp_in   [row][col][plane] = crdt_rtn_for_eb_rd_rsp_out  [row][col-1][plane];
                end
            
                // westbound signals
                
                if (col == NUM_MSH_COLS-1) begin
                    wb_wr_req_in                [row][col][plane] = i_wb_wr_req                 [row]       [plane];
                    wb_wr_dbus_in               [row][col][plane] = i_wb_wr_dbus                [row]       [plane];
                    wb_rd_req_in                [row][col][plane] = i_wb_rd_req                 [row]       [plane];
                    wb_rd_rsp_in                [row][col][plane] = i_wb_rd_rsp                 [row]       [plane];
                    wb_rd_dbus_in               [row][col][plane] = i_wb_rd_dbus                [row]       [plane];
                    crdt_rtns_for_wb_wr_reqs_in [row][col][plane] = i_crdt_rtns_for_wb_wr_reqs  [row]       [plane];
                    crdt_rtns_for_wb_rd_reqs_in [row][col][plane] = i_crdt_rtns_for_wb_rd_reqs  [row]       [plane];
                    crdt_rtn_for_wb_rd_rsp_in   [row][col][plane] = i_crdt_rtn_for_wb_rd_rsp    [row]       [plane];
                end else begin
                    wb_wr_req_in                [row][col][plane] = wb_wr_req_out               [row][col+1][plane];
                    wb_wr_dbus_in               [row][col][plane] = wb_wr_dbus_out              [row][col+1][plane];
                    wb_rd_req_in                [row][col][plane] = wb_rd_req_out               [row][col+1][plane];
                    wb_rd_rsp_in                [row][col][plane] = wb_rd_rsp_out               [row][col+1][plane];
                    wb_rd_dbus_in               [row][col][plane] = wb_rd_dbus_out              [row][col+1][plane];
                    crdt_rtns_for_wb_wr_reqs_in [row][col][plane] = crdt_rtns_for_wb_wr_reqs_out[row][col+1][plane];
                    crdt_rtns_for_wb_rd_reqs_in [row][col][plane] = crdt_rtns_for_wb_rd_reqs_out[row][col+1][plane];
                    crdt_rtn_for_wb_rd_rsp_in   [row][col][plane] = crdt_rtn_for_wb_rd_rsp_out  [row][col+1][plane];
                end

            end // for plane 
        end // for col
    end // for row 
end // always_comb            
        


//-----------------------------------------------------------------------------
// Assign Output Signals
//-----------------------------------------------------------------------------

always_comb begin 
    for (int row=0; row < NUM_MSH_ROWS; row++)  begin
        for (int col=0; col < NUM_MSH_COLS; col++)  begin
            for (int plane=0; plane < NUM_MSH_PLANES; plane++) begin

                // northbound output signals

                if (row == 0) begin

                    o_nb_wr_req                        [col][plane] = nb_wr_req_out               [row][col][plane];
                    o_nb_wr_dbus                       [col][plane] = nb_wr_dbus_out              [row][col][plane];
                    o_nb_rd_req                        [col][plane] = nb_rd_req_out               [row][col][plane];
                    o_nb_rd_rsp                        [col][plane] = nb_rd_rsp_out               [row][col][plane];
                    o_nb_rd_dbus                       [col][plane] = nb_rd_dbus_out              [row][col][plane];
                    o_crdt_rtn_for_nb_wr_req           [col][plane] = crdt_rtn_for_nb_wr_req_out  [row][col][plane];
                    o_crdt_rtn_for_nb_rd_req           [col][plane] = crdt_rtn_for_nb_rd_req_out  [row][col][plane];
                    o_crdt_rtn_for_nb_rd_rsp           [col][plane] = crdt_rtn_for_nb_rd_rsp_out  [row][col][plane];

                end // row == 0

                // southbound output signals

                if (row == NUM_MSH_ROWS-1) begin

                    o_sb_wr_req                        [col][plane] = sb_wr_req_out               [row][col][plane];
                    o_sb_wr_dbus                       [col][plane] = sb_wr_dbus_out              [row][col][plane];
                    o_sb_rd_req                        [col][plane] = sb_rd_req_out               [row][col][plane];
                    o_sb_rd_rsp                        [col][plane] = sb_rd_rsp_out               [row][col][plane];
                    o_sb_rd_dbus                       [col][plane] = sb_rd_dbus_out              [row][col][plane];
                    o_crdt_rtn_for_sb_wr_req           [col][plane] = crdt_rtn_for_sb_wr_req_out  [row][col][plane];
                    o_crdt_rtn_for_sb_rd_req           [col][plane] = crdt_rtn_for_sb_rd_req_out  [row][col][plane];
                    o_crdt_rtn_for_sb_rd_rsp           [col][plane] = crdt_rtn_for_sb_rd_rsp_out  [row][col][plane];

                end // row == NUM_MSH_ROWS

                // eastbound output signals

                if (col == NUM_MSH_COLS-1) begin
                
                    o_eb_wr_req                   [row][col][plane] = eb_wr_req_out               [row][col][plane];
                    o_eb_wr_dbus                  [row][col][plane] = eb_wr_dbus_out              [row][col][plane];
                    o_eb_rd_req                   [row][col][plane] = eb_rd_req_out               [row][col][plane];
                    o_eb_rd_rsp                   [row][col][plane] = eb_rd_rsp_out               [row][col][plane];
                    o_eb_rd_dbus                  [row][col][plane] = eb_rd_dbus_out              [row][col][plane];
                    o_crdt_rtns_for_eb_wr_reqs    [row][col][plane] = crdt_rtns_for_eb_wr_reqs_out[row][col][plane];
                    o_crdt_rtns_for_eb_rd_reqs    [row][col][plane] = crdt_rtns_for_eb_rd_reqs_out[row][col][plane];
                    o_crdt_rtn_for_eb_rd_rsp      [row][col][plane] = crdt_rtn_for_eb_rd_rsp_out  [row][col][plane];

                end // col == NUM_MSH_COLS-1
            
                // westbound output signals
                
                if (col == 0) begin
                    o_wb_wr_req                   [row][col][plane] = wb_wr_req_out               [row][col][plane];
                    o_wb_wr_dbus                  [row][col][plane] = wb_wr_dbus_out              [row][col][plane];
                    o_wb_rd_req                   [row][col][plane] = wb_rd_req_out               [row][col][plane];
                    o_wb_rd_rsp                   [row][col][plane] = wb_rd_rsp_out               [row][col][plane];
                    o_wb_rd_dbus                  [row][col][plane] = wb_rd_dbus_out              [row][col][plane];
                    o_crdt_rtns_for_wb_wr_reqs    [row][col][plane] = crdt_rtns_for_wb_wr_reqs_out[row][col][plane];
                    o_crdt_rtns_for_wb_rd_reqs    [row][col][plane] = crdt_rtns_for_wb_rd_reqs_out[row][col][plane];
                    o_crdt_rtn_for_wb_rd_rsp      [row][col][plane] = crdt_rtn_for_wb_rd_rsp_out  [row][col][plane];
                end // col ==0 

            end // for plane 
        end // for col
    end // for row 
end // always_comb            

//msh_arb_weighted # (
//    .NUM_BIDS(4)
//) arb (
//    .mclk           (mclk),
//    .i_reset        (i_reset),
//    .i_bids         (4'b1111),
//    .i_weights      ({7'd8, 7'd7, 7'd3, 7'd2}),
//    .i_fifo_depths  ({5'd0, 5'd0, 5'd0, 5'd0}),
//
//    .o_winners      ()
//);


endmodule // mesh
