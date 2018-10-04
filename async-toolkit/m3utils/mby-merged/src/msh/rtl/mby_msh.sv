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
//          module  msh                                                             mby_msh.sv
//
//              `include "msh_defines.vh"                                           mby_msh_defines.vh
//              import mby_msh_pkg                                                  mby_msh_pkg.sv
//
//              for (genvar gv_row=0; gv_row < NUM_MSH_ROWS; gv_row++) begin : mesh_rows
//                  for (genvar gv_col=0; gv_col < NUM_MSH_colS; gv_col++) begin : mesh_cols
//
//                      mby_msh_node    node                                        mby_msh_node.sv
//
//                          mby_msh_ctrl    ctrl                                    mby_msh_ctrl.sv
//                              mby_msh_wr_req  wr_req                              mby_msh_wr_req.sv
//                              mby_msh_rd_req  rd_req                              mby_msh_rd_req.sv
//
//                          for (genvar gv_c=0; gv_c < NUM_MSH_DP_CHUNKS; gv_c++) begin : dp_chunks 
//
//                              mby_msh_dp      dp                                  mby_msh_dp.sv
//                                  mby_msh_wr_dp   wr_dp                           mby_msh_wr_dp.sv
//                                  mby_msh_rd_dp   rd_dp                           mby_msh_rd_dp.sv
//                                  mby_msh_mem_dp  mem_dp                          mby_msh_mem_dp.sv
//
//                                  for (genvar gv_b=0; gv_b < NUM_MSH_NODE_MEM_BANKS; gv_b++) begin : mem_banks
//
//                                      mby_msh_mem     mem                         mby_msh_mem_dp.sv
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

`include "mby_msh_defines.vh"                                  // include file with `defines 

module mby_msh 
import mby_msh_pkg::*;                                         // import declarations from mby_msh_pkg.sv
(

    input                           mclk,                                  // mesh clock                                 
    input                           i_reset,                               // reset

    // north boundary interfaces

    mby_msh_col_wr_if.requestor     o_north_nb_wr_ifs[NUM_MSH_COLS-1:0],
    mby_msh_col_rd_if.requestor     o_north_nb_rd_ifs[NUM_MSH_COLS-1:0],
    mby_msh_col_wr_if.responder     i_north_sb_wr_ifs[NUM_MSH_COLS-1:0],
    mby_msh_col_rd_if.responder     i_north_sb_rd_ifs[NUM_MSH_COLS-1:0],

    // south boundary interfaces

    mby_msh_col_wr_if.responder     i_south_nb_wr_ifs[NUM_MSH_COLS-1:0],
    mby_msh_col_rd_if.responder     i_south_nb_rd_ifs[NUM_MSH_COLS-1:0],
    mby_msh_col_wr_if.requestor     o_south_sb_wr_ifs[NUM_MSH_COLS-1:0],
    mby_msh_col_rd_if.requestor     o_south_sb_rd_ifs[NUM_MSH_COLS-1:0],

    // east boundary interfaces

    mby_msh_row_wr_if.requestor     o_east_eb_wr_ifs[NUM_MSH_ROWS-1:0],
    mby_msh_row_rd_if.requestor     o_east_eb_rd_ifs[NUM_MSH_ROWS-1:0],
    mby_msh_row_wr_if.responder     i_east_wb_wr_ifs[NUM_MSH_ROWS-1:0],
    mby_msh_row_rd_if.responder     i_east_wb_rd_ifs[NUM_MSH_ROWS-1:0],

    // west boundary interfaces

    mby_msh_row_wr_if.responder     i_west_eb_wr_ifs[NUM_MSH_ROWS-1:0],
    mby_msh_row_rd_if.responder     i_west_eb_rd_ifs[NUM_MSH_ROWS-1:0],
    mby_msh_row_wr_if.requestor     o_west_wb_wr_ifs[NUM_MSH_ROWS-1:0],
    mby_msh_row_rd_if.requestor     o_west_wb_rd_ifs[NUM_MSH_ROWS-1:0]

);


//-----------------------------------------------------------------------------
// Declarations 
//-----------------------------------------------------------------------------


//**********************************************************************************************************************
// MESH NODE INTERFACE INSTANTIATIONS
//**********************************************************************************************************************

// internal mesh node interfaces

generate
for (genvar gv_row=0; gv_row < NUM_MSH_ROWS+1; gv_row++) begin : int_if_row
    for (genvar gv_col=0; gv_col < NUM_MSH_COLS+1; gv_col++) begin : int_if_col

mby_msh_col_wr_if msh_node_nb_wr_if();
mby_msh_col_rd_if msh_node_nb_rd_if();
mby_msh_col_wr_if msh_node_sb_wr_if();
mby_msh_col_rd_if msh_node_sb_rd_if();
mby_msh_row_wr_if msh_node_eb_wr_if();
mby_msh_row_rd_if msh_node_eb_rd_if();
mby_msh_row_wr_if msh_node_wb_wr_if();
mby_msh_row_rd_if msh_node_wb_rd_if();

    end : int_if_col
end : int_if_row
endgenerate


//**********************************************************************************************************************
// MESH NODE INSTANTIATIONS 
//**********************************************************************************************************************

//-----------------------------------------------------------------------------
// => ..., ... stages :  <short logic description>
//-----------------------------------------------------------------------------


generate
for (genvar gv_row=0; gv_row < NUM_MSH_ROWS; gv_row++) begin : mesh_rows
    for (genvar gv_col=0; gv_col < NUM_MSH_COLS; gv_col++) begin : mesh_cols

        if ((gv_row==0) && (gv_col==0)) begin : nw

// row 0, col 0 mesh node (northwest corner)

mby_msh_node node (

    .mclk       (mclk                                                                   ),
    .i_reset    (                                                                       ),

    // north side interface
    .i_sb_wr_if (i_north_sb_wr_ifs[gv_col]                                                ),
    .i_sb_rd_if (i_north_sb_rd_ifs[gv_col]                                                ),
    .o_nb_wr_if (o_north_nb_wr_ifs[gv_col]                                                ),
    .o_nb_rd_if (o_north_nb_rd_ifs[gv_col]                                                ),

    // south side interface
    .i_nb_wr_if (int_if_row[gv_row+1]       .int_if_col[gv_col]     .msh_node_nb_wr_if  ),
    .i_nb_rd_if (int_if_row[gv_row+1]       .int_if_col[gv_col]     .msh_node_nb_rd_if  ),
    .o_sb_rd_if (int_if_row[gv_row+1]       .int_if_col[gv_col]     .msh_node_sb_rd_if  ),
    .o_sb_wr_if (int_if_row[gv_row+1]       .int_if_col[gv_col]     .msh_node_sb_wr_if  ),

    // east side interface
    .i_wb_wr_if (int_if_row[gv_row]         .int_if_col[gv_col+1]   .msh_node_wb_wr_if  ),
    .i_wb_rd_if (int_if_row[gv_row]         .int_if_col[gv_col+1]   .msh_node_wb_rd_if  ),
    .o_eb_wr_if (int_if_row[gv_row]         .int_if_col[gv_col+1]   .msh_node_eb_wr_if  ),
    .o_eb_rd_if (int_if_row[gv_row]         .int_if_col[gv_col+1]   .msh_node_eb_rd_if  ),

    // west side interface
    .i_eb_wr_if (i_west_eb_wr_ifs[gv_row]                                                 ),
    .i_eb_rd_if (i_west_eb_rd_ifs[gv_row]                                                 ),
    .o_wb_wr_if (o_west_wb_wr_ifs[gv_row]                                                 ),
    .o_wb_rd_if (o_west_wb_rd_ifs[gv_row]                                                 )

);

        end : nw else if ((gv_row==NUM_MSH_ROWS-1) && (gv_col==0)) begin : sw

// row NUM_MSH_ROWS-1, col 0 mesh node (southwest corner)
mby_msh_node node (

    .mclk       (mclk                                                                   ),
    .i_reset    (                                                                       ),

    // north side interface
    .i_sb_wr_if (int_if_row[gv_row]         .int_if_col[gv_col]     .msh_node_sb_wr_if  ),
    .i_sb_rd_if (int_if_row[gv_row]         .int_if_col[gv_col]     .msh_node_sb_rd_if  ),
    .o_nb_rd_if (int_if_row[gv_row]         .int_if_col[gv_col]     .msh_node_nb_rd_if  ),
    .o_nb_wr_if (int_if_row[gv_row]         .int_if_col[gv_col]     .msh_node_nb_wr_if  ),

    // south side interface
    .i_nb_wr_if (i_south_nb_wr_ifs[gv_col]                                                ),
    .i_nb_rd_if (i_south_nb_rd_ifs[gv_col]                                                ),
    .o_sb_wr_if (o_south_sb_wr_ifs[gv_col]                                                ),
    .o_sb_rd_if (o_south_sb_rd_ifs[gv_col]                                                ),

    // east side interface
    .i_wb_wr_if (int_if_row[gv_row]         .int_if_col[gv_col+1]   .msh_node_wb_wr_if  ),
    .i_wb_rd_if (int_if_row[gv_row]         .int_if_col[gv_col+1]   .msh_node_wb_rd_if  ),
    .o_eb_wr_if (int_if_row[gv_row]         .int_if_col[gv_col+1]   .msh_node_eb_wr_if  ),
    .o_eb_rd_if (int_if_row[gv_row]         .int_if_col[gv_col+1]   .msh_node_eb_rd_if  ),

    // west side interface
    .i_eb_wr_if (i_west_eb_wr_ifs[gv_row]                                                 ),
    .i_eb_rd_if (i_west_eb_rd_ifs[gv_row]                                                 ),
    .o_wb_wr_if (o_west_wb_wr_ifs[gv_row]                                                 ),
    .o_wb_rd_if (o_west_wb_rd_ifs[gv_row]                                                 )

);

        end : sw else if ((gv_row==NUM_MSH_ROWS-1) && (gv_col==NUM_MSH_COLS-1)) begin : se

// row NUM_MSH_ROWS-1, col NUM_MSH_COLS-1 mesh node (southeast corner)

mby_msh_node node (

    .mclk       (mclk                                                                   ),
    .i_reset    (                                                                       ),

    // north side interface
    .i_sb_wr_if (int_if_row[gv_row]         .int_if_col[gv_col]     .msh_node_sb_wr_if  ),
    .i_sb_rd_if (int_if_row[gv_row]         .int_if_col[gv_col]     .msh_node_sb_rd_if  ),
    .o_nb_rd_if (int_if_row[gv_row]         .int_if_col[gv_col]     .msh_node_nb_rd_if  ),
    .o_nb_wr_if (int_if_row[gv_row]         .int_if_col[gv_col]     .msh_node_nb_wr_if  ),

    // south side interface
    .i_nb_wr_if (i_south_nb_wr_ifs[gv_col]                                                ),
    .i_nb_rd_if (i_south_nb_rd_ifs[gv_col]                                                ),
    .o_sb_wr_if (o_south_sb_wr_ifs[gv_col]                                                ),
    .o_sb_rd_if (o_south_sb_rd_ifs[gv_col]                                                ),

    // east side interface
    .i_wb_wr_if (i_east_wb_wr_ifs[gv_row]                                                 ),
    .i_wb_rd_if (i_east_wb_rd_ifs[gv_row]                                                 ),
    .o_eb_wr_if (o_east_eb_wr_ifs[gv_row]                                                 ),
    .o_eb_rd_if (o_east_eb_rd_ifs[gv_row]                                                 ),

    // west side interface
    .i_eb_wr_if (int_if_row[gv_row]         .int_if_col[gv_col]     .msh_node_eb_wr_if  ),
    .i_eb_rd_if (int_if_row[gv_row]         .int_if_col[gv_col]     .msh_node_eb_rd_if  ),
    .o_wb_wr_if (int_if_row[gv_row]         .int_if_col[gv_col]     .msh_node_wb_wr_if  ),
    .o_wb_rd_if (int_if_row[gv_row]         .int_if_col[gv_col]     .msh_node_wb_rd_if  ) 
);

        end : se else if ((gv_row==0) && (gv_col==NUM_MSH_COLS-1)) begin : ne

// row NUM_MSH_ROWS-1, col NUM_MSH_COLS-1 mesh node (northeast corner)

mby_msh_node node (

    .mclk       (mclk                                                                   ),
    .i_reset    (                                                                       ),

    // north side interface
    .i_sb_wr_if (i_north_sb_wr_ifs[gv_col]                                                ),
    .i_sb_rd_if (i_north_sb_rd_ifs[gv_col]                                                ),
    .o_nb_wr_if (o_north_nb_wr_ifs[gv_col]                                                ),
    .o_nb_rd_if (o_north_nb_rd_ifs[gv_col]                                                ),

    // south side interface
    .i_nb_wr_if (int_if_row[gv_row+1]       .int_if_col[gv_col]     .msh_node_nb_wr_if  ),
    .i_nb_rd_if (int_if_row[gv_row+1]       .int_if_col[gv_col]     .msh_node_nb_rd_if  ),
    .o_sb_rd_if (int_if_row[gv_row+1]       .int_if_col[gv_col]     .msh_node_sb_rd_if  ),
    .o_sb_wr_if (int_if_row[gv_row+1]       .int_if_col[gv_col]     .msh_node_sb_wr_if  ),

    // east side interface
    .i_wb_wr_if (i_east_wb_wr_ifs[gv_row]                                                 ),
    .i_wb_rd_if (i_east_wb_rd_ifs[gv_row]                                                 ),
    .o_eb_wr_if (o_east_eb_wr_ifs[gv_row]                                                 ),
    .o_eb_rd_if (o_east_eb_rd_ifs[gv_row]                                                 ),

    // west side interface
    .i_eb_wr_if (int_if_row[gv_row]         .int_if_col[gv_col]     .msh_node_eb_wr_if  ),
    .i_eb_rd_if (int_if_row[gv_row]         .int_if_col[gv_col]     .msh_node_eb_rd_if  ),
    .o_wb_wr_if (int_if_row[gv_row]         .int_if_col[gv_col]     .msh_node_wb_wr_if  ),
    .o_wb_rd_if (int_if_row[gv_row]         .int_if_col[gv_col]     .msh_node_wb_rd_if  )
);

        end :ne else if (gv_row==0) begin : n

// north row mesh nodes (not corner nodes)

mby_msh_node node (

    .mclk       (mclk                                                                   ),
    .i_reset    (                                                                       ),

    // north side interface
    .i_sb_wr_if (i_north_sb_wr_ifs[gv_col]                                                ),
    .i_sb_rd_if (i_north_sb_rd_ifs[gv_col]                                                ),
    .o_nb_wr_if (o_north_nb_wr_ifs[gv_col]                                                ),
    .o_nb_rd_if (o_north_nb_rd_ifs[gv_col]                                                ),

    // south side interface
    .i_nb_wr_if (int_if_row[gv_row+1]       .int_if_col[gv_col]     .msh_node_nb_wr_if  ),
    .i_nb_rd_if (int_if_row[gv_row+1]       .int_if_col[gv_col]     .msh_node_nb_rd_if  ),
    .o_sb_rd_if (int_if_row[gv_row+1]       .int_if_col[gv_col]     .msh_node_sb_rd_if  ),
    .o_sb_wr_if (int_if_row[gv_row+1]       .int_if_col[gv_col]     .msh_node_sb_wr_if  ),

    // east side interface
    .i_wb_wr_if (int_if_row[gv_row]         .int_if_col[gv_col+1]   .msh_node_wb_wr_if  ),
    .i_wb_rd_if (int_if_row[gv_row]         .int_if_col[gv_col+1]   .msh_node_wb_rd_if  ),
    .o_eb_wr_if (int_if_row[gv_row]         .int_if_col[gv_col+1]   .msh_node_eb_wr_if  ),
    .o_eb_rd_if (int_if_row[gv_row]         .int_if_col[gv_col+1]   .msh_node_eb_rd_if  ),

    // west side interface
    .i_eb_wr_if (int_if_row[gv_row]         .int_if_col[gv_col]     .msh_node_eb_wr_if  ),
    .i_eb_rd_if (int_if_row[gv_row]         .int_if_col[gv_col]     .msh_node_eb_rd_if  ),
    .o_wb_wr_if (int_if_row[gv_row]         .int_if_col[gv_col]     .msh_node_wb_wr_if  ),
    .o_wb_rd_if (int_if_row[gv_row]         .int_if_col[gv_col]     .msh_node_wb_rd_if  )

);

        end : n else if (gv_row==NUM_MSH_ROWS-1) begin : s

// south row mesh nodes (not corner nodes)

mby_msh_node node (

    .mclk       (mclk                                                                   ),
    .i_reset    (                                                                       ),

    // north side interface
    .i_sb_wr_if (int_if_row[gv_row]         .int_if_col[gv_col]     .msh_node_sb_wr_if  ),
    .i_sb_rd_if (int_if_row[gv_row]         .int_if_col[gv_col]     .msh_node_sb_rd_if  ),
    .o_nb_rd_if (int_if_row[gv_row]         .int_if_col[gv_col]     .msh_node_nb_rd_if  ),
    .o_nb_wr_if (int_if_row[gv_row]         .int_if_col[gv_col]     .msh_node_nb_wr_if  ),

    // south side interface
    .i_nb_wr_if (i_south_nb_wr_ifs[gv_col]                                                ),
    .i_nb_rd_if (i_south_nb_rd_ifs[gv_col]                                                ),
    .o_sb_wr_if (o_south_sb_wr_ifs[gv_col]                                                ),
    .o_sb_rd_if (o_south_sb_rd_ifs[gv_col]                                                ),

    // east side interface
    .i_wb_wr_if (int_if_row[gv_row]         .int_if_col[gv_col+1]   .msh_node_wb_wr_if  ),
    .i_wb_rd_if (int_if_row[gv_row]         .int_if_col[gv_col+1]   .msh_node_wb_rd_if  ),
    .o_eb_wr_if (int_if_row[gv_row]         .int_if_col[gv_col+1]   .msh_node_eb_wr_if  ),
    .o_eb_rd_if (int_if_row[gv_row]         .int_if_col[gv_col+1]   .msh_node_eb_rd_if  ),

    // west side interface
    .i_eb_wr_if (int_if_row[gv_row]         .int_if_col[gv_col]     .msh_node_eb_wr_if  ),
    .i_eb_rd_if (int_if_row[gv_row]         .int_if_col[gv_col]     .msh_node_eb_rd_if  ),
    .o_wb_wr_if (int_if_row[gv_row]         .int_if_col[gv_col]     .msh_node_wb_wr_if  ),
    .o_wb_rd_if (int_if_row[gv_row]         .int_if_col[gv_col]     .msh_node_wb_rd_if  )

);

        end :s else if (gv_col==0) begin : w

// west column mesh nodes (not corner nodes)

mby_msh_node node (

    .mclk       (mclk                                                                   ),
    .i_reset    (                                                                       ),

    // north side interface
    .i_sb_wr_if (int_if_row[gv_row]         .int_if_col[gv_col]     .msh_node_sb_wr_if  ),
    .i_sb_rd_if (int_if_row[gv_row]         .int_if_col[gv_col]     .msh_node_sb_rd_if  ),
    .o_nb_rd_if (int_if_row[gv_row]         .int_if_col[gv_col]     .msh_node_nb_rd_if  ),
    .o_nb_wr_if (int_if_row[gv_row]         .int_if_col[gv_col]     .msh_node_nb_wr_if  ),

    // south side interface
    .i_nb_wr_if (int_if_row[gv_row+1]       .int_if_col[gv_col]     .msh_node_nb_wr_if  ),
    .i_nb_rd_if (int_if_row[gv_row+1]       .int_if_col[gv_col]     .msh_node_nb_rd_if  ),
    .o_sb_rd_if (int_if_row[gv_row+1]       .int_if_col[gv_col]     .msh_node_sb_rd_if  ),
    .o_sb_wr_if (int_if_row[gv_row+1]       .int_if_col[gv_col]     .msh_node_sb_wr_if  ),

    // east side interface
    .i_wb_wr_if (int_if_row[gv_row]         .int_if_col[gv_col+1]   .msh_node_wb_wr_if  ),
    .i_wb_rd_if (int_if_row[gv_row]         .int_if_col[gv_col+1]   .msh_node_wb_rd_if  ),
    .o_eb_wr_if (int_if_row[gv_row]         .int_if_col[gv_col+1]   .msh_node_eb_wr_if  ),
    .o_eb_rd_if (int_if_row[gv_row]         .int_if_col[gv_col+1]   .msh_node_eb_rd_if  ),

    // west side interface
    .i_eb_wr_if (i_west_eb_wr_ifs[gv_row]                                                 ),
    .i_eb_rd_if (i_west_eb_rd_ifs[gv_row]                                                 ),
    .o_wb_wr_if (o_west_wb_wr_ifs[gv_row]                                                 ),
    .o_wb_rd_if (o_west_wb_rd_ifs[gv_row]                                                 )

);

        end : w else if (gv_col==NUM_MSH_COLS-1) begin : e

// east column mesh nodes (not corner nodes)

mby_msh_node node (

    .mclk       (mclk                                                                   ),
    .i_reset    (                                                                       ),

    // north side interface
    .i_sb_wr_if (int_if_row[gv_row]         .int_if_col[gv_col]     .msh_node_sb_wr_if  ),
    .i_sb_rd_if (int_if_row[gv_row]         .int_if_col[gv_col]     .msh_node_sb_rd_if  ),
    .o_nb_rd_if (int_if_row[gv_row]         .int_if_col[gv_col]     .msh_node_nb_rd_if  ),
    .o_nb_wr_if (int_if_row[gv_row]         .int_if_col[gv_col]     .msh_node_nb_wr_if  ),

    // south side interface
    .i_nb_wr_if (int_if_row[gv_row+1]       .int_if_col[gv_col]     .msh_node_nb_wr_if  ),
    .i_nb_rd_if (int_if_row[gv_row+1]       .int_if_col[gv_col]     .msh_node_nb_rd_if  ),
    .o_sb_rd_if (int_if_row[gv_row+1]       .int_if_col[gv_col]     .msh_node_sb_rd_if  ),
    .o_sb_wr_if (int_if_row[gv_row+1]       .int_if_col[gv_col]     .msh_node_sb_wr_if  ),

    // east side interface
    .i_wb_wr_if (i_east_wb_wr_ifs[gv_row]                                                 ),
    .i_wb_rd_if (i_east_wb_rd_ifs[gv_row]                                                 ),
    .o_eb_wr_if (o_east_eb_wr_ifs[gv_row]                                                 ),
    .o_eb_rd_if (o_east_eb_rd_ifs[gv_row]                                                 ),

    // west side interface
    .i_eb_wr_if (int_if_row[gv_row]         .int_if_col[gv_col]     .msh_node_eb_wr_if  ),
    .i_eb_rd_if (int_if_row[gv_row]         .int_if_col[gv_col]     .msh_node_eb_rd_if  ),
    .o_wb_wr_if (int_if_row[gv_row]         .int_if_col[gv_col]     .msh_node_wb_wr_if  ),
    .o_wb_rd_if (int_if_row[gv_row]         .int_if_col[gv_col]     .msh_node_wb_rd_if  )

);


        end : e else begin : i

// internal mesh nodes

mby_msh_node node (

    .mclk       (mclk                                                                   ),
    .i_reset    (                                                                       ),

    // north side interface
    .i_sb_wr_if (int_if_row[gv_row]         .int_if_col[gv_col]     .msh_node_sb_wr_if  ),
    .i_sb_rd_if (int_if_row[gv_row]         .int_if_col[gv_col]     .msh_node_sb_rd_if  ),
    .o_nb_rd_if (int_if_row[gv_row]         .int_if_col[gv_col]     .msh_node_nb_rd_if  ),
    .o_nb_wr_if (int_if_row[gv_row]         .int_if_col[gv_col]     .msh_node_nb_wr_if  ),

    // south side interface
    .i_nb_wr_if (int_if_row[gv_row+1]       .int_if_col[gv_col]     .msh_node_nb_wr_if  ),
    .i_nb_rd_if (int_if_row[gv_row+1]       .int_if_col[gv_col]     .msh_node_nb_rd_if  ),
    .o_sb_rd_if (int_if_row[gv_row+1]       .int_if_col[gv_col]     .msh_node_sb_rd_if  ),
    .o_sb_wr_if (int_if_row[gv_row+1]       .int_if_col[gv_col]     .msh_node_sb_wr_if  ),

    // east side interface
    .i_wb_wr_if (int_if_row[gv_row]         .int_if_col[gv_col+1]   .msh_node_wb_wr_if  ),
    .i_wb_rd_if (int_if_row[gv_row]         .int_if_col[gv_col+1]   .msh_node_wb_rd_if  ),
    .o_eb_wr_if (int_if_row[gv_row]         .int_if_col[gv_col+1]   .msh_node_eb_wr_if  ),
    .o_eb_rd_if (int_if_row[gv_row]         .int_if_col[gv_col+1]   .msh_node_eb_rd_if  ),

    // west side interface
    .i_eb_wr_if (int_if_row[gv_row]         .int_if_col[gv_col]     .msh_node_eb_wr_if  ),
    .i_eb_rd_if (int_if_row[gv_row]         .int_if_col[gv_col]     .msh_node_eb_rd_if  ),
    .o_wb_wr_if (int_if_row[gv_row]         .int_if_col[gv_col]     .msh_node_wb_wr_if  ),
    .o_wb_rd_if (int_if_row[gv_row]         .int_if_col[gv_col]     .msh_node_wb_rd_if  )

);

        end : i

    end : mesh_cols
end : mesh_rows
endgenerate


//**********************************************************************************************************************
// MESH NODE CONNECTIONS
//**********************************************************************************************************************

//-----------------------------------------------------------------------------
// Assign Output Signals
//-----------------------------------------------------------------------------


//mby_msh_arb_weighted # (
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


endmodule // mby_mesh
