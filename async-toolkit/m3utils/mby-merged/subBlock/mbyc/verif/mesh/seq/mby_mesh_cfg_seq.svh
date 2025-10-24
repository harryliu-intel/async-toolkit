
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

//   Class:  mby_mesh_env_base_seq
//
//   This is the Mesh base seq extended from MBY Base seq which is in-turn extended 
//   from shdv_base_seq. This sequence class has methods to setup mesh env object handle 
//   and methods to perform register access both for RTL.
//
//  All the sequences in mesh env will extend from this base sequence to inherit its
//  functionality.  


`ifndef __MBY_MESH_CFG_SEQ_GUARD
`define __MBY_MESH_CFG_SEQ_GUARD

`ifndef __INSIDE_MBY_MESH_SEQ_LIB
`error "Attempt to include file outside of mby_mesh_seq_lib."
`endif

class mby_mesh_cfg_seq extends shdv_base_configure_sequence; 

    // Variable: dut_cfg
    // Mesh dut cfg.
    mby_mesh_env_pkg::mby_mesh_dut_cfg     dut_cfg;

    // ------------------------------------------------------------------------
    //  Constructor: new
    //  Arguments:
    //  string name   - Mesh env base sequence object name.
    // ------------------------------------------------------------------------
    function new(string name = "mby_mesh_cfg_seq");
        super.new();

    endfunction : new

    //---------------------------------------------------------------------------
    // Task: body
    //
    // Main task of configure sequence.  
    //---------------------------------------------------------------------------
    virtual task body();
      `uvm_info(get_name(), "configure sequence started", UVM_LOW)
    endtask
 
endclass : mby_mesh_cfg_seq

`endif 