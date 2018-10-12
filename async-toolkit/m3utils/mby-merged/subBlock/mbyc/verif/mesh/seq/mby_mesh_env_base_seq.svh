
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


`ifndef __MBY_MESH_ENV_BASE_SEQ_GUARD
`define __MBY_MESH_ENV_BASE_SEQ_GUARD

`ifndef __INSIDE_MBY_MESH_SEQ_LIB
`error "Attempt to include file outside of mby_mesh_seq_lib."
`endif

class mby_mesh_env_base_seq extends mby_common_pkg::mby_base_seq;

    // Variable: dut_cfg
    // Mesh dut cfg.
    mby_mesh_env_pkg::mby_mesh_dut_cfg     dut_cfg;

    // Variable: vif
    // Handle to Mesh Tb interface.
    virtual mby_mesh_tb_if                 vif;

    // ------------------------------------------------------------------------
    //  Constructor: new
    //  Arguments:
    //  string name   - Mesh env base sequence object name.
    // ------------------------------------------------------------------------
    function new(string name = "mby_mesh_env_base_seq");
        super.new();
        set_cfg(slu_tb_env::get_top_tb_env());
    endfunction : new

    // ------------------------------------------------------------------------
    // Function : set_cfg
    // Sets the handle to the dut_cfg and tb vif. 
    // ------------------------------------------------------------------------
    virtual function void set_cfg(slu_tb_env tb_env);

        mby_mesh_env_pkg::mby_mesh_env temp_env;
        mby_mesh_env_pkg::mby_mesh_tb_top_cfg temp_tb_cfg;
        bit stat;

        stat = $cast(temp_env,tb_env);
        if(!stat) begin
            `uvm_fatal(get_name(), "Cast of sla_tb_env failed");
        end
        if(temp_env == null) begin
            `uvm_fatal(get_name(), "Could not fetch sla_tb_env handle!!!");
        end

        temp_tb_cfg  = temp_env.get_tb_cfg();
        this.dut_cfg = temp_tb_cfg.dut_cfg;
        this.vif     = temp_env.get_tb_vif();

    endfunction : set_cfg

endclass : mby_mesh_env_base_seq

`endif // __MBY_MESH_ENV_BASE_SEQ_GUARD
