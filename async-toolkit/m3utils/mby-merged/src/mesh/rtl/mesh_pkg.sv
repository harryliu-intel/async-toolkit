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
// -- Description :  Mesh package file 
// --
// ---------------------------------------------------------------------------------------------------------------------

package mesh_pkg;


//-----------------------------------------------------------------------------
// parameters
//-----------------------------------------------------------------------------

localparam NUM_MESH_ROWS    = 16;       // number of mesh rows 
localparam NUM_MESH_COLS    = 8;        // number of mesh columns 

localparam NUM_DP_CHUNKS    = 4;        // the data path is broken bitwise into this number of chunks 

localparam NUM_MEM_BANKS    = 4;        // the memory is broken into this number of banks 

//-----------------------------------------------------------------------------
// derived parameters
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
// simple typedefs
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
// enum typedefs
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// struct and union typedefs
//-----------------------------------------------------------------------------
//  - struct typedefs enable fields to be specified by name (instead of bit position) in RTL and in debuggers 
//  - union typedefs enable specification of multiple different encodings for a single set of bits 

//-----------------------------------------------------------------------------
// array typedefs
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// constants
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// functions
//-----------------------------------------------------------------------------

endpackage : mesh_pkg
