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
// -- Description  : This is the top level of the mesh. 
// 
// -- Pipeline Stages: 
//
//      ... TBD ... 
//                                                                                  ---------------
// -- Instantiation Hierarchy:                                                      definition file
//                                                                                  ---------------
//          module  mesh                                                            mesh.sv
//
//              `include "mesh_defines.vh"                                          mesh_defines.vh
//              import mesh_pkg                                                     mesh_pkg.sv
//
//              for (genvar gv_row=0; gv_row < NUM_MESH_ROWS; gv_row++) begin : mesh_rows
//                  for (genvar gv_col=0; gv_col < NUM_MESH_colS; gv_col++) begin : mesh_cols
//
//                      msh_node    msh_node                                        mesh_node.sv
//
//                          msh_ctrl    msh_ctrl                                    msh_ctrl.sv
//                              msh_wr_req  msh_wr_req                              msh_wr_req.sv
//                              msh_rd_req  msh_rd_req                              msh_rd_req.sv
//
//                          for (genvar gv_c=0; gv_c < NUM_MESH_DP_CHUNKS; gv_c++) begin : dp_chunks 
//
//                              msh_dp      msh_dp                                  msh_dp.sv
//                                  msh_wr_dp   msh_wr_dp                           msh_wr_dp.sv
//                                  msh_rd_dp   msh_rd_dp                           msh_rd_dp.sv
//                                  msh_mem_dp  msh_mem_dp                          msh_mem_dp.sv
//
//                                  for (genvar gv_b=0; gv_b < NUM_MESH_NODE_MEM_BANKS; gv_b++) begin : mem_banks
//
//                                      msh_mem     msh_mem                         msh_mem_dp.sv
//
//                                  end : mem_banks
//                          end : dp_chunks
//
//                  end : mesh_cols
//              end : mesh_rows
//
// 
// ---------------------------------------------------------------------------------------------------------------------

// note that the dashed lines above represents recommended viewing window width

`include "mesh_defines.vh";                                     // include file with `defines 

module mesh 
import mesh_pkg::*;                                             // import declarations from mesh_pkg.sv
(


    // inputs
    
    input                mclk,                                  // mesh clock                                 
    input                i_reset,                               // reset

    input  mesh_col_wr_req_t i_nb_wr_req  [NUM_MESH_PLANES-1:0],    // incoming northbound write request 
    input  mesh_col_wr_req_t i_sb_wr_req  [NUM_MESH_PLANES-1:0],    // incoming southbound write request 
    input  mesh_row_wr_req_t i_eb_wr_req  [NUM_MESH_PLANES-1:0],    // incoming eastbound write request 
    input  mesh_row_wr_req_t i_wb_wr_req  [NUM_MESH_PLANES-1:0],    // incoming westbound write request 

    input  mesh_dbus_t       i_nb_wr_dbus [NUM_MESH_PLANES-1:0],    // incoming northbound write data bus 
    input  mesh_dbus_t       i_sb_wr_dbus [NUM_MESH_PLANES-1:0],    // incoming southbound write data bus 
    input  mesh_dbus_t       i_eb_wr_dbus [NUM_MESH_PLANES-1:0],    // incoming eastbound write data bus 
    input  mesh_dbus_t       i_wb_wr_dbus [NUM_MESH_PLANES-1:0],    // incoming westbound write data bus 

    input  mesh_col_rd_req_t i_nb_rd_req  [NUM_MESH_PLANES-1:0],    // incoming northbound read request 
    input  mesh_col_rd_req_t i_sb_rd_req  [NUM_MESH_PLANES-1:0],    // incoming southbound read request 
    input  mesh_row_rd_req_t i_eb_rd_req  [NUM_MESH_PLANES-1:0],    // incoming eastbound read request 
    input  mesh_row_rd_req_t i_wb_rd_req  [NUM_MESH_PLANES-1:0],    // incoming westbound read request 

    input  mesh_row_rd_rsp_t i_nb_rd_rsp  [NUM_MESH_PLANES-1:0],    // incoming northbound read response 
    input  mesh_row_rd_rsp_t i_sb_rd_rsp  [NUM_MESH_PLANES-1:0],    // incoming southbound read response 
    input  mesh_row_rd_rsp_t i_eb_rd_rsp  [NUM_MESH_PLANES-1:0],    // incoming eastbound read response 
    input  mesh_row_rd_rsp_t i_wb_rd_rsp  [NUM_MESH_PLANES-1:0],    // incoming westbound read response 

    input  mesh_dbus_t       i_nb_rd_dbus [NUM_MESH_PLANES-1:0],    // incoming northbound read data bus 
    input  mesh_dbus_t       i_sb_rd_dbus [NUM_MESH_PLANES-1:0],    // incoming southbound read data bus 
    input  mesh_dbus_t       i_eb_rd_dbus [NUM_MESH_PLANES-1:0],    // incoming eastbound read data bus 
    input  mesh_dbus_t       i_wb_rd_dbus [NUM_MESH_PLANES-1:0],    // incoming westbound read data bus 


    // outputs
   
    output mesh_col_wr_req_t o_nb_wr_req  [NUM_MESH_PLANES-1:0],    // outgoing northbound write request 
    output mesh_col_wr_req_t o_sb_wr_req  [NUM_MESH_PLANES-1:0],    // outgoing southbound write request 
    output mesh_row_wr_req_t o_eb_wr_req  [NUM_MESH_PLANES-1:0],    // outgoing eastbound write request 
    output mesh_row_wr_req_t o_wb_wr_req  [NUM_MESH_PLANES-1:0],    // outgoing westbound write request 

    output mesh_dbus_t       o_nb_wr_dbus [NUM_MESH_PLANES-1:0],    // outgoing northbound write data bus 
    output mesh_dbus_t       o_sb_wr_dbus [NUM_MESH_PLANES-1:0],    // outgoing southbound write data bus 
    output mesh_dbus_t       o_eb_wr_dbus [NUM_MESH_PLANES-1:0],    // outgoing eastbound write data bus 
    output mesh_dbus_t       o_wb_wr_dbus [NUM_MESH_PLANES-1:0],    // outgoing westbound write data bus 

    output mesh_col_rd_req_t o_nb_rd_req  [NUM_MESH_PLANES-1:0],    // outgoing northbound read request 
    output mesh_col_rd_req_t o_sb_rd_req  [NUM_MESH_PLANES-1:0],    // outgoing southbound read request 
    output mesh_row_rd_req_t o_eb_rd_req  [NUM_MESH_PLANES-1:0],    // outgoing eastbound read request 
    output mesh_row_rd_req_t o_wb_rd_req  [NUM_MESH_PLANES-1:0],    // outgoing westbound read request 

    output mesh_col_rd_rsp_t o_nb_rd_rsp  [NUM_MESH_PLANES-1:0],    // outgoing northbound read response 
    output mesh_col_rd_rsp_t o_sb_rd_rsp  [NUM_MESH_PLANES-1:0],    // outgoing southbound read response 
    output mesh_row_rd_rsp_t o_eb_rd_rsp  [NUM_MESH_PLANES-1:0],    // outgoing eastbound read response 
    output mesh_row_rd_rsp_t o_wb_rd_rsp  [NUM_MESH_PLANES-1:0],    // outgoing westbound read response 

    output mesh_dbus_t       o_nb_rd_dbus [NUM_MESH_PLANES-1:0],    // outgoing northbound read data bus 
    output mesh_dbus_t       o_sb_rd_dbus [NUM_MESH_PLANES-1:0],    // outgoing southbound read data bus 
    output mesh_dbus_t       o_eb_rd_dbus [NUM_MESH_PLANES-1:0],    // outgoing eastbound read data bus 
    output mesh_dbus_t       o_wb_rd_dbus [NUM_MESH_PLANES-1:0],    // outgoing westbound read data bus 

    output logic             o_crdt_rtn_for_nb_wr_req,              // credit return for incoming northbound write req
    output logic             o_crdt_rtn_for_sb_wr_req,              // credit return for incoming southbound write req
    output mesh_row_crdts_t  o_crdt_rtns_for_eb_wr_reqs,            // credit returns for all eb_wr_reqs in row
    output mesh_row_crdts_t  o_crdt_rtns_for_wb_wr_reqs,            // credit retunrs for all wb_wr_reqs in row

    output logic             o_crdt_rtn_for_nb_rd_req,              // credit return for incoming northbound read req
    output logic             o_crdt_rtn_for_sb_rd_req,              // credit return for incoming southbound read req
    output mesh_row_crdts_t  o_crdt_rtns_for_eb_rd_reqs,            // credit returns for all eb_rd_reqs in row
    output mesh_row_crdts_t  o_crdt_rtns_for_wb_rd_reqs,            // credit retunrs for all wb_rd_reqs in row

    output logic             o_crdt_rtn_for_nb_rd_rsp,              // credit return for incoming northbound read rsp
    output logic             o_crdt_rtn_for_sb_rd_rsp,              // credit return for incoming southbound read rsp
    output logic             o_crdt_rtn_for_eb_rd_rsp,              // credit return for incoming southbound read rsp
    output logic             o_crdt_rtn_for_wb_rd_rsp               // credit return for incoming southbound read rsp

);


//-----------------------------------------------------------------------------
// Declarations 
//-----------------------------------------------------------------------------

// Pipeline stage names are appended to the end of signal names to ease understanding of signal timing relationships.
// Any equation that mixes signals with different pipeline stage names should be given careful thought.

/// ... stage signals


//**********************************************************************************************************************
// MESH NODE INSTANTIATIONS 
//**********************************************************************************************************************

//-----------------------------------------------------------------------------
// => ..., ... stages :  <short logic description>
//-----------------------------------------------------------------------------

generate
for (genvar gv_row=0; gv_row < NUM_MESH_ROWS; gv_row++) begin : mesh_rows
    for (genvar gv_col=0; gv_col < NUM_MESH_COLS; gv_col++) begin : mesh_cols

msh_node msh_node (

    .mclk                   (mclk       )

);
    
    end : mesh_cols
end : mesh_rows
endgenerate


//**********************************************************************************************************************
// OUTPUT SECTION 
//**********************************************************************************************************************

//-----------------------------------------------------------------------------
// Assign Output Signals
//-----------------------------------------------------------------------------


endmodule // mesh
