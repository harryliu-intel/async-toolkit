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


`include "mby_msh_defines.vh"                                 // include file with `defines 

module mby_msh_node 
import mby_msh_pkg::*;                                        // import declarations from mby_msh_pkg.sv
(

    input                   mclk,                                 // mesh clock                                 
    input                   i_reset,                              // reset

    mby_msh_col_wr_if.responder i_nb_wr_if,
    mby_msh_col_rd_if.responder i_nb_rd_if,
    mby_msh_col_wr_if.responder i_sb_wr_if,
    mby_msh_col_rd_if.responder i_sb_rd_if,
    mby_msh_row_wr_if.responder i_eb_wr_if,
    mby_msh_row_rd_if.responder i_eb_rd_if,
    mby_msh_row_wr_if.responder i_wb_wr_if,
    mby_msh_row_rd_if.responder i_wb_rd_if,

    mby_msh_col_wr_if.requestor o_nb_wr_if,
    mby_msh_col_rd_if.requestor o_nb_rd_if,
    mby_msh_col_wr_if.requestor o_sb_wr_if,
    mby_msh_col_rd_if.requestor o_sb_rd_if,
    mby_msh_row_wr_if.requestor o_eb_wr_if,
    mby_msh_row_rd_if.requestor o_eb_rd_if,
    mby_msh_row_wr_if.requestor o_wb_wr_if,
    mby_msh_row_rd_if.requestor o_wb_rd_if 

);


//-----------------------------------------------------------------------------
// Declarations 
//-----------------------------------------------------------------------------

// Pipeline stage names are appended to the end of signal names to ease understanding of signal timing relationships.
// Any equation that mixes signals with different pipeline stage names should be given careful thought.

/// ... stage signals

msh_dp_chunk_t nb_wr_dp_chunks_in[NUM_MSH_DP_CHUNKS-1:0][NUM_MSH_PLANES];
msh_dp_chunk_t sb_wr_dp_chunks_in[NUM_MSH_DP_CHUNKS-1:0][NUM_MSH_PLANES];
msh_dp_chunk_t eb_wr_dp_chunks_in[NUM_MSH_DP_CHUNKS-1:0][NUM_MSH_PLANES];
msh_dp_chunk_t wb_wr_dp_chunks_in[NUM_MSH_DP_CHUNKS-1:0][NUM_MSH_PLANES];

msh_dp_chunk_t nb_rd_dp_chunks_in[NUM_MSH_DP_CHUNKS-1:0][NUM_MSH_PLANES];
msh_dp_chunk_t sb_rd_dp_chunks_in[NUM_MSH_DP_CHUNKS-1:0][NUM_MSH_PLANES];
msh_dp_chunk_t eb_rd_dp_chunks_in[NUM_MSH_DP_CHUNKS-1:0][NUM_MSH_PLANES];
msh_dp_chunk_t wb_rd_dp_chunks_in[NUM_MSH_DP_CHUNKS-1:0][NUM_MSH_PLANES];

msh_dp_chunk_t nb_wr_dp_chunks_out[NUM_MSH_DP_CHUNKS-1:0][NUM_MSH_PLANES];
msh_dp_chunk_t sb_wr_dp_chunks_out[NUM_MSH_DP_CHUNKS-1:0][NUM_MSH_PLANES];
msh_dp_chunk_t eb_wr_dp_chunks_out[NUM_MSH_DP_CHUNKS-1:0][NUM_MSH_PLANES];
msh_dp_chunk_t wb_wr_dp_chunks_out[NUM_MSH_DP_CHUNKS-1:0][NUM_MSH_PLANES];

msh_dp_chunk_t nb_rd_dp_chunks_out[NUM_MSH_DP_CHUNKS-1:0][NUM_MSH_PLANES];
msh_dp_chunk_t sb_rd_dp_chunks_out[NUM_MSH_DP_CHUNKS-1:0][NUM_MSH_PLANES];
msh_dp_chunk_t eb_rd_dp_chunks_out[NUM_MSH_DP_CHUNKS-1:0][NUM_MSH_PLANES];
msh_dp_chunk_t wb_rd_dp_chunks_out[NUM_MSH_DP_CHUNKS-1:0][NUM_MSH_PLANES];

msh_dbus_t nb_wr_dbus_out [NUM_MSH_PLANES-1:0];
msh_dbus_t sb_wr_dbus_out [NUM_MSH_PLANES-1:0];
msh_dbus_t eb_wr_dbus_out [NUM_MSH_PLANES-1:0];
msh_dbus_t wb_wr_dbus_out [NUM_MSH_PLANES-1:0];

msh_dbus_t nb_rd_dbus_out [NUM_MSH_PLANES-1:0];
msh_dbus_t sb_rd_dbus_out [NUM_MSH_PLANES-1:0];
msh_dbus_t eb_rd_dbus_out [NUM_MSH_PLANES-1:0];
msh_dbus_t wb_rd_dbus_out [NUM_MSH_PLANES-1:0];

//**********************************************************************************************************************
// CONNECT TO INTERFACES
//**********************************************************************************************************************

//**********************************************************************************************************************
// MESH CONTROL BLOCK INSTANTIATION
//**********************************************************************************************************************

//-----------------------------------------------------------------------------
// ..., ... stages :  <short logic description>
//-----------------------------------------------------------------------------
// separate module to support physical grouping

mby_msh_ctrl ctrl (

    .mclk                           (mclk                               ),
    .i_reset                        (i_reset                            ),

    .i_nb_wr_req                    (i_nb_wr_if.i_wr_req                ),
    .i_sb_wr_req                    (i_sb_wr_if.i_wr_req                ),
    .i_eb_wr_req                    (i_eb_wr_if.i_wr_req                ),
    .i_wb_wr_req                    (i_wb_wr_if.i_wr_req                ),

    .i_nb_rd_req                    (i_nb_rd_if.i_rd_req                ),
    .i_sb_rd_req                    (i_sb_rd_if.i_rd_req                ),
    .i_eb_rd_req                    (i_eb_rd_if.i_rd_req                ),
    .i_wb_rd_req                    (i_wb_rd_if.i_rd_req                ),

    .i_nb_rd_rsp                    (o_sb_rd_if.i_rd_rsp                ),
    .i_sb_rd_rsp                    (o_nb_rd_if.i_rd_rsp                ),
    .i_eb_rd_rsp                    (o_wb_rd_if.i_rd_rsp                ),
    .i_wb_rd_rsp                    (o_eb_rd_if.i_rd_rsp                ),

    .i_crdt_rtn_for_nb_wr_req       (o_nb_wr_if.i_crdt_rtn_for_wr_req   ),
    .i_crdt_rtn_for_sb_wr_req       (o_sb_wr_if.i_crdt_rtn_for_wr_req   ),
    .i_crdt_rtns_for_eb_wr_reqs     (o_eb_wr_if.i_crdt_rtns_for_wr_reqs ),
    .i_crdt_rtns_for_wb_wr_reqs     (o_wb_wr_if.i_crdt_rtns_for_wr_reqs ),

    .i_crdt_rtn_for_nb_rd_req       (o_nb_rd_if.i_crdt_rtn_for_rd_req   ),
    .i_crdt_rtn_for_sb_rd_req       (o_sb_rd_if.i_crdt_rtn_for_rd_req   ),
    .i_crdt_rtns_for_eb_rd_reqs     (o_eb_rd_if.i_crdt_rtns_for_rd_reqs ),
    .i_crdt_rtns_for_wb_rd_reqs     (o_wb_rd_if.i_crdt_rtns_for_rd_reqs ),

    .i_crdt_rtn_for_nb_rd_rsp       (i_nb_rd_if.i_crdt_rtn_for_rd_rsp    ),
    .i_crdt_rtn_for_sb_rd_rsp       (i_sb_rd_if.i_crdt_rtn_for_rd_rsp    ),
    .i_crdt_rtn_for_eb_rd_rsp       (i_eb_rd_if.i_crdt_rtn_for_rd_rsp    ),
    .i_crdt_rtn_for_wb_rd_rsp       (i_wb_rd_if.i_crdt_rtn_for_rd_rsp    ),

    .o_nb_wr_req                    (o_nb_wr_if.o_wr_req                 ),
    .o_sb_wr_req                    (o_sb_wr_if.o_wr_req                 ),
    .o_eb_wr_req                    (o_eb_wr_if.o_wr_req                 ),
    .o_wb_wr_req                    (o_wb_wr_if.o_wr_req                 ),

    .o_nb_rd_req                    (o_nb_rd_if.o_rd_req                 ),
    .o_sb_rd_req                    (o_sb_rd_if.o_rd_req                 ),
    .o_eb_rd_req                    (o_eb_rd_if.o_rd_req                 ),
    .o_wb_rd_req                    (o_wb_rd_if.o_rd_req                 ),

    .o_nb_rd_rsp                    (i_nb_rd_if.o_rd_rsp                 ),
    .o_sb_rd_rsp                    (i_sb_rd_if.o_rd_rsp                 ),
    .o_eb_rd_rsp                    (i_eb_rd_if.o_rd_rsp                 ),
    .o_wb_rd_rsp                    (i_wb_rd_if.o_rd_rsp                 ),

    .o_crdt_rtn_for_nb_wr_req       (i_nb_wr_if.o_crdt_rtn_for_wr_req    ),
    .o_crdt_rtn_for_sb_wr_req       (i_sb_wr_if.o_crdt_rtn_for_wr_req    ),
    .o_crdt_rtns_for_eb_wr_reqs     (i_eb_wr_if.o_crdt_rtns_for_wr_reqs  ),
    .o_crdt_rtns_for_wb_wr_reqs     (i_wb_wr_if.o_crdt_rtns_for_wr_reqs  ),

    .o_crdt_rtn_for_nb_rd_req       (i_nb_rd_if.o_crdt_rtn_for_rd_req    ),
    .o_crdt_rtn_for_sb_rd_req       (i_sb_rd_if.o_crdt_rtn_for_rd_req    ),
    .o_crdt_rtns_for_eb_rd_reqs     (i_eb_rd_if.o_crdt_rtns_for_rd_reqs  ),
    .o_crdt_rtns_for_wb_rd_reqs     (i_wb_rd_if.o_crdt_rtns_for_rd_reqs  ),

    .o_crdt_rtn_for_nb_rd_rsp       (o_nb_rd_if.o_crdt_rtn_for_rd_rsp    ),
    .o_crdt_rtn_for_sb_rd_rsp       (o_sb_rd_if.o_crdt_rtn_for_rd_rsp    ),
    .o_crdt_rtn_for_eb_rd_rsp       (o_eb_rd_if.o_crdt_rtn_for_rd_rsp    ),
    .o_crdt_rtn_for_wb_rd_rsp       (o_wb_rd_if.o_crdt_rtn_for_rd_rsp    )

);

//**********************************************************************************************************************
// MESH DATA PATH BLOCK INSTANTIATIONS
//**********************************************************************************************************************

generate
for (genvar gv_c=0; gv_c < NUM_MSH_DP_CHUNKS; gv_c++) begin : dp_chunks


// re-group inputs

for (genvar gv_p=0; gv_p < NUM_MSH_PLANES; gv_p++) begin : planes_in 

assign nb_wr_dp_chunks_in[gv_c][gv_p] = i_nb_wr_if.i_wr_dbus[gv_p][((MSH_DP_CHUNK_WIDTH-1)+(gv_c*MSH_DP_CHUNK_WIDTH)):(0+(gv_c*MSH_DP_CHUNK_WIDTH))];
assign sb_wr_dp_chunks_in[gv_c][gv_p] = i_sb_wr_if.i_wr_dbus[gv_p][((MSH_DP_CHUNK_WIDTH-1)+(gv_c*MSH_DP_CHUNK_WIDTH)):(0+(gv_c*MSH_DP_CHUNK_WIDTH))];
assign eb_wr_dp_chunks_in[gv_c][gv_p] = i_eb_wr_if.i_wr_dbus[gv_p][((MSH_DP_CHUNK_WIDTH-1)+(gv_c*MSH_DP_CHUNK_WIDTH)):(0+(gv_c*MSH_DP_CHUNK_WIDTH))];
assign wb_wr_dp_chunks_in[gv_c][gv_p] = i_wb_wr_if.i_wr_dbus[gv_p][((MSH_DP_CHUNK_WIDTH-1)+(gv_c*MSH_DP_CHUNK_WIDTH)):(0+(gv_c*MSH_DP_CHUNK_WIDTH))];

assign nb_rd_dp_chunks_in[gv_c][gv_p] = o_nb_rd_if.i_rd_dbus[gv_p][((MSH_DP_CHUNK_WIDTH-1)+(gv_c*MSH_DP_CHUNK_WIDTH)):(0+(gv_c*MSH_DP_CHUNK_WIDTH))];
assign sb_rd_dp_chunks_in[gv_c][gv_p] = o_sb_rd_if.i_rd_dbus[gv_p][((MSH_DP_CHUNK_WIDTH-1)+(gv_c*MSH_DP_CHUNK_WIDTH)):(0+(gv_c*MSH_DP_CHUNK_WIDTH))];
assign eb_rd_dp_chunks_in[gv_c][gv_p] = o_eb_rd_if.i_rd_dbus[gv_p][((MSH_DP_CHUNK_WIDTH-1)+(gv_c*MSH_DP_CHUNK_WIDTH)):(0+(gv_c*MSH_DP_CHUNK_WIDTH))];
assign wb_rd_dp_chunks_in[gv_c][gv_p] = o_wb_rd_if.i_rd_dbus[gv_p][((MSH_DP_CHUNK_WIDTH-1)+(gv_c*MSH_DP_CHUNK_WIDTH)):(0+(gv_c*MSH_DP_CHUNK_WIDTH))];

end : planes_in

// instantiate data path blocks

mby_msh_dp dp (
    
    .mclk              (mclk                       ),
    .i_reset           (i_reset                    ),

    .i_nb_wr_dp_chunk  (nb_wr_dp_chunks_in[gv_c]   ),
    .i_sb_wr_dp_chunk  (sb_wr_dp_chunks_in[gv_c]   ),
    .i_eb_wr_dp_chunk  (eb_wr_dp_chunks_in[gv_c]   ),
    .i_wb_wr_dp_chunk  (wb_wr_dp_chunks_in[gv_c]   ),

    .i_nb_rd_dp_chunk  (nb_rd_dp_chunks_in[gv_c]   ),
    .i_sb_rd_dp_chunk  (sb_rd_dp_chunks_in[gv_c]   ),
    .i_eb_rd_dp_chunk  (eb_rd_dp_chunks_in[gv_c]   ),
    .i_wb_rd_dp_chunk  (wb_rd_dp_chunks_in[gv_c]   ),

    .o_nb_wr_dp_chunk  (nb_wr_dp_chunks_out[gv_c]  ),
    .o_sb_wr_dp_chunk  (sb_wr_dp_chunks_out[gv_c]  ),
    .o_eb_wr_dp_chunk  (eb_wr_dp_chunks_out[gv_c]  ),
    .o_wb_wr_dp_chunk  (wb_wr_dp_chunks_out[gv_c]  ),

    .o_nb_rd_dp_chunk  (nb_rd_dp_chunks_out[gv_c]  ),
    .o_sb_rd_dp_chunk  (sb_rd_dp_chunks_out[gv_c]  ),
    .o_eb_rd_dp_chunk  (eb_rd_dp_chunks_out[gv_c]  ),
    .o_wb_rd_dp_chunk  (wb_rd_dp_chunks_out[gv_c]  )

);
    
// re-group outputs

for (genvar gv_p=0; gv_p < NUM_MSH_PLANES; gv_p++) begin : planes_out 

assign nb_wr_dbus_out[gv_p][((MSH_DP_CHUNK_WIDTH-1)+(gv_c*MSH_DP_CHUNK_WIDTH)):(0+(gv_c*MSH_DP_CHUNK_WIDTH))] = nb_wr_dp_chunks_out[gv_c][gv_p];
assign sb_wr_dbus_out[gv_p][((MSH_DP_CHUNK_WIDTH-1)+(gv_c*MSH_DP_CHUNK_WIDTH)):(0+(gv_c*MSH_DP_CHUNK_WIDTH))] = sb_wr_dp_chunks_out[gv_c][gv_p];
assign eb_wr_dbus_out[gv_p][((MSH_DP_CHUNK_WIDTH-1)+(gv_c*MSH_DP_CHUNK_WIDTH)):(0+(gv_c*MSH_DP_CHUNK_WIDTH))] = eb_wr_dp_chunks_out[gv_c][gv_p];
assign wb_wr_dbus_out[gv_p][((MSH_DP_CHUNK_WIDTH-1)+(gv_c*MSH_DP_CHUNK_WIDTH)):(0+(gv_c*MSH_DP_CHUNK_WIDTH))] = nb_wr_dp_chunks_out[gv_c][gv_p];

assign nb_rd_dbus_out[gv_p][((MSH_DP_CHUNK_WIDTH-1)+(gv_c*MSH_DP_CHUNK_WIDTH)):(0+(gv_c*MSH_DP_CHUNK_WIDTH))] = nb_rd_dp_chunks_out[gv_c][gv_p];
assign sb_rd_dbus_out[gv_p][((MSH_DP_CHUNK_WIDTH-1)+(gv_c*MSH_DP_CHUNK_WIDTH)):(0+(gv_c*MSH_DP_CHUNK_WIDTH))] = sb_rd_dp_chunks_out[gv_c][gv_p];
assign eb_rd_dbus_out[gv_p][((MSH_DP_CHUNK_WIDTH-1)+(gv_c*MSH_DP_CHUNK_WIDTH)):(0+(gv_c*MSH_DP_CHUNK_WIDTH))] = eb_rd_dp_chunks_out[gv_c][gv_p];
assign wb_rd_dbus_out[gv_p][((MSH_DP_CHUNK_WIDTH-1)+(gv_c*MSH_DP_CHUNK_WIDTH)):(0+(gv_c*MSH_DP_CHUNK_WIDTH))] = wb_rd_dp_chunks_out[gv_c][gv_p];

end : planes_out

end : dp_chunks
endgenerate


//**********************************************************************************************************************
// OUTPUT SECTION 
//**********************************************************************************************************************

//-----------------------------------------------------------------------------
// Assign Output Signals
//-----------------------------------------------------------------------------

assign o_nb_wr_if.o_wr_dbus = nb_wr_dbus_out;
assign o_sb_wr_if.o_wr_dbus = sb_wr_dbus_out;
assign o_eb_wr_if.o_wr_dbus = eb_wr_dbus_out;
assign o_wb_wr_if.o_wr_dbus = wb_wr_dbus_out;

assign i_nb_rd_if.o_rd_dbus = sb_rd_dbus_out;
assign i_sb_rd_if.o_rd_dbus = nb_rd_dbus_out;
assign i_eb_rd_if.o_rd_dbus = wb_rd_dbus_out;
assign i_wb_rd_if.o_rd_dbus = eb_rd_dbus_out;

endmodule // mby_msh_node
