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
//
//                          for (genvar gv_c=0; gv_c < NUM_DP_CHUNKS; gv_c++) begin : dp_chunks 
//
//                              msh_dp      msh_dp                                  msh_dp.sv
//                                  msh_wr_dp   msh_wr_dp                           msh_wr_dp.sv
//                                  msh_rd_dp   msh_rd_dp                           msh_rd_dp.sv
//                                  msh_mem_dp  msh_mem_dp                          msh_mem_dp.sv
//
//                                  for (genvar gv_b=0; gv_b < NUM_MEM_BANKS; gv_b++) begin : mem_banks
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


    input               mclk,                                   // mesh clock                                 
    input               i_reset                                 // reset

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
