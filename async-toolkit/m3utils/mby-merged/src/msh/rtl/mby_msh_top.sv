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
// -- Description  : THIS FILE NOT CURRENTLY USED.  IT MIGHT BECOME A MSH WRAPPER.
//
//      Node array indexing 
//      -------------------
//
//      Nodes (N) are instantiated as a row x column array (N[row][col]) with the northmost row being row 0 and the 
//      westmost column being column 0:
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
// ---------------------------------------------------------------------------------------------------------------------

// note that the dashed lines above represents recommended viewing window width

`include "mby_msh_defines.vh"                                  // include file with `defines 

module mby_msh_top 
import mby_msh_pkg::*;                                         // import declarations from mby_msh_pkg.sv
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

    input  logic            i_crdt_rtn_for_nb_wr_req                      [NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0],
    input  logic            i_crdt_rtn_for_sb_wr_req                      [NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0],
    input  msh_row_crdts_t  i_crdt_rtns_for_eb_wr_reqs  [NUM_MSH_ROWS-1:0]                  [NUM_MSH_PLANES-1:0],
    input  msh_row_crdts_t  i_crdt_rtns_for_wb_wr_reqs  [NUM_MSH_ROWS-1:0]                  [NUM_MSH_PLANES-1:0],

    input  logic            i_crdt_rtn_for_nb_rd_req                      [NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0],
    input  logic            i_crdt_rtn_for_sb_rd_req                      [NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0],
    input  msh_row_crdts_t  i_crdt_rtns_for_eb_rd_reqs  [NUM_MSH_ROWS-1:0]                  [NUM_MSH_PLANES-1:0],
    input  msh_row_crdts_t  i_crdt_rtns_for_wb_rd_reqs  [NUM_MSH_ROWS-1:0]                  [NUM_MSH_PLANES-1:0],

    input  logic            i_crdt_rtn_for_nb_rd_rsp                      [NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0],
    input  logic            i_crdt_rtn_for_sb_rd_rsp                      [NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0],
    input  logic            i_crdt_rtn_for_eb_rd_rsp    [NUM_MSH_ROWS-1:0]                  [NUM_MSH_PLANES-1:0],
    input  logic            i_crdt_rtn_for_wb_rd_rsp    [NUM_MSH_ROWS-1:0]                  [NUM_MSH_PLANES-1:0],


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

    output logic            o_crdt_rtn_for_nb_wr_req                      [NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0],
    output logic            o_crdt_rtn_for_sb_wr_req                      [NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0],
    output msh_row_crdts_t  o_crdt_rtns_for_eb_wr_reqs  [NUM_MSH_ROWS-1:0]                  [NUM_MSH_PLANES-1:0],
    output msh_row_crdts_t  o_crdt_rtns_for_wb_wr_reqs  [NUM_MSH_ROWS-1:0]                  [NUM_MSH_PLANES-1:0],

    output logic            o_crdt_rtn_for_nb_rd_req                      [NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0],
    output logic            o_crdt_rtn_for_sb_rd_req                      [NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0],
    output msh_row_crdts_t  o_crdt_rtns_for_eb_rd_reqs  [NUM_MSH_ROWS-1:0]                  [NUM_MSH_PLANES-1:0],
    output msh_row_crdts_t  o_crdt_rtns_for_wb_rd_reqs  [NUM_MSH_ROWS-1:0]                  [NUM_MSH_PLANES-1:0],

    output logic            o_crdt_rtn_for_nb_rd_rsp                      [NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0],
    output logic            o_crdt_rtn_for_sb_rd_rsp                      [NUM_MSH_COLS-1:0][NUM_MSH_PLANES-1:0],
    output logic            o_crdt_rtn_for_eb_rd_rsp    [NUM_MSH_ROWS-1:0]                  [NUM_MSH_PLANES-1:0],
    output logic            o_crdt_rtn_for_wb_rd_rsp    [NUM_MSH_ROWS-1:0]                  [NUM_MSH_PLANES-1:0] 

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
// MESH NODE INTERFACE INSTANTIATIONS
//**********************************************************************************************************************

// north boundary interfaces

mby_msh_col_wr_if north_nb_wr_ifs[NUM_MSH_COLS-1:0]();
mby_msh_col_rd_if north_nb_rd_ifs[NUM_MSH_COLS-1:0]();
mby_msh_col_wr_if north_sb_wr_ifs[NUM_MSH_COLS-1:0]();
mby_msh_col_rd_if north_sb_rd_ifs[NUM_MSH_COLS-1:0]();

// south boundary interfaces

mby_msh_col_wr_if south_nb_wr_ifs[NUM_MSH_COLS-1:0]();
mby_msh_col_rd_if south_nb_rd_ifs[NUM_MSH_COLS-1:0]();
mby_msh_col_wr_if south_sb_wr_ifs[NUM_MSH_COLS-1:0]();
mby_msh_col_rd_if south_sb_rd_ifs[NUM_MSH_COLS-1:0]();

// east boundary interfaces

mby_msh_row_wr_if east_eb_wr_ifs[NUM_MSH_ROWS-1:0]();
mby_msh_row_rd_if east_eb_rd_ifs[NUM_MSH_ROWS-1:0]();
mby_msh_row_wr_if east_wb_wr_ifs[NUM_MSH_ROWS-1:0]();
mby_msh_row_rd_if east_wb_rd_ifs[NUM_MSH_ROWS-1:0]();

// west boundary interfaces

mby_msh_row_wr_if west_eb_wr_ifs[NUM_MSH_ROWS-1:0]();
mby_msh_row_rd_if west_eb_rd_ifs[NUM_MSH_ROWS-1:0]();
mby_msh_row_wr_if west_wb_wr_ifs[NUM_MSH_ROWS-1:0]();
mby_msh_row_rd_if west_wb_rd_ifs[NUM_MSH_ROWS-1:0]();

//**********************************************************************************************************************
// MESH INSTANTIATION
//**********************************************************************************************************************

mby_msh msh(

    
    .mclk(mclk),                                // mesh clock                                 
    .i_reset(i_reset),                          // reset

    // north boundary interfaces

    .o_north_nb_wr_ifs    (o_north_nb_wr_ifs),
    .o_north_nb_rd_ifs    (o_north_nb_rd_ifs),
    .i_north_sb_wr_ifs    (i_north_sb_wr_ifs),
    .i_north_sb_rd_ifs    (i_north_sb_rd_ifs),

    // south boundary interfaces

    .i_south_nb_wr_ifs    (i_south_nb_wr_ifs),
    .i_south_nb_rd_ifs    (i_south_nb_rd_ifs),
    .o_south_sb_wr_ifs    (o_south_sb_wr_ifs),
    .o_south_sb_rd_ifs    (o_south_sb_rd_ifs),

    // east boundary interfaces

    .o_east_eb_wr_ifs     (o_east_eb_wr_ifs),
    .o_east_eb_rd_ifs     (o_east_eb_rd_ifs),
    .i_east_wb_wr_ifs     (i_east_wb_wr_ifs),
    .i_east_wb_rd_ifs     (i_east_wb_rd_ifs),

    // west boundary interfaces

    .west_eb_wr_ifs     (west_eb_wr_ifs),
    .west_eb_rd_ifs     (west_eb_rd_ifs),
    .west_wb_wr_ifs     (west_wb_wr_ifs),
    .west_wb_rd_ifs     (west_wb_rd_ifs)

);


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

endmodule // mby_mesh_top
