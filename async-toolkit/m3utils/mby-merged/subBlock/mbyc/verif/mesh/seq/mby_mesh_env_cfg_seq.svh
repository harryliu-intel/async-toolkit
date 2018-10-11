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

//   Class:    mby_mesh_env_cfg_seq
//
//   This is the Mesh ENV Config sequence file.

`ifndef __MBY_MESH_ENV_CFG_SEQ_GUARD
`define __MBY_MESH_ENV_CFG_SEQ_GUARD

`ifndef __INSIDE_MBY_MESH_SEQ_LIB
`error "Attempt to include file outside of mby_mesh_seq_lib."
`endif

class mby_mesh_env_cfg_seq extends shdv_base_config_seq;


    `uvm_object_utils(mby_mesh_env_cfg_seq)
    `uvm_declare_p_sequencer(slu_sequencer)

    //------------------------------------------------------------------------------
    //  Constructor: new
    //  New Mesh env Config Sequence Object.
    //  Gets handle to the Mesh ENV.
    //
    //  Arguments:
    //  string name  - Mesh env config sequence object name.
    //------------------------------------------------------------------------------
    function new(input string name = "mby_mesh_warm_reset_seq");
        super.new(name);
    endfunction: new

    //------------------------------------------------------------------------------
    //  Function: sm_config
    //  This finction calls the "allocate_mem" function which allocates memory
    //  address space for exclusive use.
    //------------------------------------------------------------------------------
    virtual function void sm_config();
        sm.ag.allocate_mem(ag_result, "MMIO_LOW", 32'h2_0000, "GBE_MEM_LOW",32'h1_FFFF);
    endfunction

    //------------------------------------------------------------------------------
    //  Task: body
    //  Configures Mesh DUT.
    //------------------------------------------------------------------------------
    virtual task     body();
        `uvm_info(get_name(), "MeshTop Env Configuration Sequence", UVM_MEDIUM);

    //Configure DUT here..

    endtask : body

endclass : mby_mesh_env_cfg_seq

`endif // __MBY_MESH_ENV_CFG_SEQ_GUARD
