// vim: noai : ts=3 : sw=3 : expandtab : ft=systemverilog

//------------------------------------------------------------------------------
//
// INTEL CONFIDENTIAL
//
// Copyright 2018 Intel Corporation All Rights Reserved.
//
// The source code contained or described herein and all documents related to
// the source code ("Material") are owned by Intel Corporation or its suppliers
// or licensors. Title to the Material remains with Intel Corporation or its
// suppliers and licensors. The Material contains trade secrets and proprietary
// and confidential information of Intel or its suppliers and licensors.  The
// Material is protected by worldwide copyright and trade secret laws and
// treaty provisions. No part of the Material may be used, copied, reproduced,
// modified, published, uploaded, posted, transmitted, distributed, or
// disclosed in any way without Intel's prior express written permission.
//
// No license under any patent, copyright, trade secret or other intellectual
// property right is granted to or conferred upon you by disclosure or delivery
// of the Materials, either expressly, by implication, inducement, estoppel or
// otherwise. Any license under such intellectual property rights must be
// express and approved by Intel in writing.
//
//------------------------------------------------------------------------------
//   Author        : Dhivya Sankar
//   Project       : Madison Bay
//------------------------------------------------------------------------------

//   Defines : mby_mesh_defines
//
//  This file contain any PARAMETERS or Defines.  Also contains Topology
//  configuration ENUM.

`ifndef __MBY_MESH_DEFINES_GUARD
`define __MBY_MESH_DEFINES_GUARD

`ifndef __INSIDE_MBY_MESH_ENV_PKG
`error "Attempt to include file outside of mby_mesh_env_pkg."
`endif


class mby_mesh_defines extends uvm_object;

    `uvm_object_utils(mby_mesh_env_pkg::mby_mesh_defines)

    //---------------------------------------------------------------------------
    //  Constructor: new
    //  Collect any plusargs and re-configure variables from default, if used.
    //  Arguments:
    //  name   - Mesh Defines object name.
    //---------------------------------------------------------------------------
    function       new(string name = "mby_mesh_defines");
        super.new(name);

    endfunction: new

endclass: mby_mesh_defines

`endif // __MBY_MESH_DEFINES_GUARD
