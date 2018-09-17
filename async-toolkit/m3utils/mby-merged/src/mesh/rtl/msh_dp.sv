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

module msh_dp 
import mesh_pkg::*;                                             // import declarations from mesh_pkg.sv
(


    input               mclk                                    // mesh clock                                 

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
msh_wr_dp msh_wr_dp (
    
    .mclk   (mclk)

);
    
// mesh read datapath
msh_rd_dp msh_rd_dp (
    
    .mclk   (mclk)

);
    
// mesh memory datapath
msh_mem_dp msh_mem_dp (
    
    .mclk   (mclk)

);
    

generate
for (genvar gv_b=0; gv_b < NUM_MEM_BANKS; gv_b++) begin : mem_banks 

//-----------------------------------------------------------------------------
// => ..., ... stages :  <short logic description>
//-----------------------------------------------------------------------------
// separate modules to support physical grouping

msh_mem msh_mem (
    
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


endmodule // msh_dp
