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


`include "mesh_defines.vh";                                     // include file with `defines 

module msh_node 
import mesh_pkg::*;                                             // import declarations from mesh_pkg.sv
(

    input               mclk                                   // mesh clock                                 

);


//-----------------------------------------------------------------------------
// Declarations 
//-----------------------------------------------------------------------------

// Pipeline stage names are appended to the end of signal names to ease understanding of signal timing relationships.
// Any equation that mixes signals with different pipeline stage names should be given careful thought.

/// ... stage signals

//**********************************************************************************************************************
// MESH CONTROL BLOCK INSTANTIATION
//**********************************************************************************************************************

//-----------------------------------------------------------------------------
// ..., ... stages :  <short logic description>
//-----------------------------------------------------------------------------
// separate module to support physical grouping

msh_ctrl msh_ctrl (

    .mclk   (mclk)

);


//**********************************************************************************************************************
// MESH DATA PATH BLOCK INSTANTIATIONS
//**********************************************************************************************************************

generate
for (genvar gv_c=0; gv_c < NUM_MESH_DP_CHUNKS; gv_c++) begin : dp_chunks 

//-----------------------------------------------------------------------------
// => ..., ... stages :  <short logic description>
//-----------------------------------------------------------------------------
// separate modules to support physical grouping

msh_dp msh_dp (
    
    .mclk   (mclk)

);
    
end : dp_chunks
endgenerate


//**********************************************************************************************************************
// OUTPUT SECTION 
//**********************************************************************************************************************

//-----------------------------------------------------------------------------
// Assign Output Signals
//-----------------------------------------------------------------------------


endmodule // msh_node
