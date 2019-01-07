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


`ifndef __MBY_MESH_HARD_RESET_SEQ_GUARD
`define __MBY_MESH_HARD_RESET_SEQ_GUARD

`ifndef __INSIDE_MBY_MESH_SEQ_LIB
`error "Attempt to include file outside of mby_mesh_seq_lib."
`endif

//   Class:  mby_mesh_hard_reset_seq
//
//   This is the main IP Hard Reset Sequence. execute in Hard_Reset_Phase
//
//   Sets both Hard and Warm Resets.   Delays for some time and drops Hard Reset.
class mby_mesh_hard_reset_seq extends shdv_base_reset_sequence;

    // Variable: shdv_env
    // Protected shdv base env
    protected shdv_base_env                         env;
 
    // Variable: env
    // Protected Mesh Top Level Env
    protected mby_mesh_env_pkg::mby_mesh_env        env;

    // Variable: tb_vif;
    // Handle to Mesh TB interface.
    virtual mby_mesh_tb_if                          tb_vif;

    `uvm_object_utils(mby_mesh_hard_reset_seq)

    //------------------------------------------------------------------------------
    //  Constructor: new
    //  New Mplex Hard Reset Sequence Object.
    //  Gets handle to the Mesh ENV.
    //
    //  Arguments:
    //  string name  - Mesh TOP Hard Reset sequence object name.
    //------------------------------------------------------------------------------
    function new(input string name = "mby_mesh_hard_reset_seq");
        super.new(name);
        set_env(shdv_base_env::get_top_tb_env());
    endfunction: new

    //------------------------------------------------------------------------------
    //  Function: set_env
    //  Handle to Mplex Top Level env for use in sequences
    //
    //  Arguments:
    //  shdv_base_env tb_env  -  Handle to the ENV
    //------------------------------------------------------------------------------
    virtual function void set_env(shdv_base_env tb_env);
        mby_mesh_env_pkg::mby_mesh_env temp_env;

        $cast(temp_env,tb_env);

        this.env    = temp_env;
        tb_vif      = temp_env.get_tb_vif();
    endfunction : set_env

    //------------------------------------------------------------------------------
    //  Task: body
    //  Sequence body is used to control Hard_Reset (Set -> Delay -> Clear),
    //  as well as Warm_Reset (Set)
    //------------------------------------------------------------------------------
    task body();
        // Core/Mesh hard reset
        repeat (200) @(posedge tb_vif.fab_clk);
       
        `uvm_info(get_name(), $sformatf("Hard_Reset Set"), UVM_HIGH);
        tb_vif.chard_reset                 = 1;
        tb_vif.mhard_reset                 = 1;

        repeat (100) @(posedge tb_vif.fab_clk);

        `uvm_info(get_name(), $sformatf("Hard_Reset Cleared"), UVM_HIGH);
        tb_vif.chard_reset                 = 0;
        tb_vif.mhard_reset                 = 0;

        repeat (100) @(posedge tb_vif.fab_clk);
    endtask: body

endclass: mby_mesh_hard_reset_seq

`endif // __MBY_MESH_HARD_RESET_SEQ_GUARD
