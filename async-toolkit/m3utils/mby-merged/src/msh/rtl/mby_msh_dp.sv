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

module mby_msh_dp 
import mby_msh_pkg::*;                                        // import declarations from mby_msh_pkg.sv
(
    // inputs
    
    input                   mclk,                                   // mesh clock                                 
    input                   i_reset,                                // reset

    input  msh_dp_chunk_t   i_nb_wr_dp_chunk [NUM_MSH_PLANES-1:0],  // incoming northbound write data path chunk
    input  msh_dp_chunk_t   i_sb_wr_dp_chunk [NUM_MSH_PLANES-1:0],  // incoming southbound write data path chunk
    input  msh_dp_chunk_t   i_eb_wr_dp_chunk [NUM_MSH_PLANES-1:0],  // incoming eastbound write data path chunk
    input  msh_dp_chunk_t   i_wb_wr_dp_chunk [NUM_MSH_PLANES-1:0],  // incoming westbound write data path chunk

    input  msh_dp_chunk_t   i_nb_rd_dp_chunk [NUM_MSH_PLANES-1:0],  // incoming northbound read data path chunk
    input  msh_dp_chunk_t   i_sb_rd_dp_chunk [NUM_MSH_PLANES-1:0],  // incoming southbound read data path chunk
    input  msh_dp_chunk_t   i_eb_rd_dp_chunk [NUM_MSH_PLANES-1:0],  // incoming eastbound read data path chunk
    input  msh_dp_chunk_t   i_wb_rd_dp_chunk [NUM_MSH_PLANES-1:0],  // incoming westbound read data path chunk


    // outputs
   
    output msh_dp_chunk_t   o_nb_wr_dp_chunk [NUM_MSH_PLANES-1:0],    // outgoing northbound write data path chunk
    output msh_dp_chunk_t   o_sb_wr_dp_chunk [NUM_MSH_PLANES-1:0],    // outgoing southbound write data path chunk
    output msh_dp_chunk_t   o_eb_wr_dp_chunk [NUM_MSH_PLANES-1:0],    // outgoing eastbound write data path chunk
    output msh_dp_chunk_t   o_wb_wr_dp_chunk [NUM_MSH_PLANES-1:0],    // outgoing westbound write data path chunk

    output msh_dp_chunk_t   o_nb_rd_dp_chunk [NUM_MSH_PLANES-1:0],    // outgoing northbound read data path chunk
    output msh_dp_chunk_t   o_sb_rd_dp_chunk [NUM_MSH_PLANES-1:0],    // outgoing southbound read data path chunk
    output msh_dp_chunk_t   o_eb_rd_dp_chunk [NUM_MSH_PLANES-1:0],    // outgoing eastbound read data path chunk
    output msh_dp_chunk_t   o_wb_rd_dp_chunk [NUM_MSH_PLANES-1:0]     // outgoing westbound read data path chunk

);


//-----------------------------------------------------------------------------
// Declarations 
//-----------------------------------------------------------------------------

// Pipeline stage names are appended to the end of signal names to ease understanding of signal timing relationships.
// Any equation that mixes signals with different pipeline stage names should be given careful thought.

/// ... stage signals

//**********************************************************************************************************************
// MESH SUB-BLOCK INSTANTIATIONS
//**********************************************************************************************************************

// mesh write datapath
mby_msh_wr_dp wr_dp (
    
    .mclk   (mclk)

);
    
// mesh read datapath
mby_msh_rd_dp rd_dp (
    
    .mclk   (mclk)

);
    
// mesh memory datapath
mby_msh_mem_dp mem_dp (
    
    .mclk   (mclk)

);
    

generate
for (genvar gv_b=0; gv_b < NUM_MSH_NODE_MEM_BANKS; gv_b++) begin : mem_banks 

//-----------------------------------------------------------------------------
// => ..., ... stages :  <short logic description>
//-----------------------------------------------------------------------------
// separate modules to support physical grouping

mby_msh_mem mem (
    
    .mclk   (mclk)

);
    
end : mem_banks
endgenerate


//**********************************************************************************************************************
// OUTPUT SECTION 
//**********************************************************************************************************************

//-----------------------------------------------------------------------------
// Assign Output Signals
//-----------------------------------------------------------------------------


endmodule // mby_msh_dp
