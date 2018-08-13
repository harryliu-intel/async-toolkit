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
//   Author        : Akshay Kotian
//   Project       : Madison Bay
//------------------------------------------------------------------------------

//   Class:  mby_mc_hard_reset_seq
//
//   This is the main IP Hard Reset Sequence. execute in Hard_Reset_Phase
//
//   Sets both Hard and Warm Resets.   Delays for some time and drops Hard Reset.

`ifndef __MBY_MC_HARD_RESET_SEQ_GUARD
`define __MBY_MC_HARD_RESET_SEQ_GUARD

`ifndef __INSIDE_MBY_MC_SEQ_LIB
`error "Attempt to include file outside of mby_mc_seq_lib."
`endif

class mby_mc_hard_reset_seq extends shdv_base_reset_seq;

    // Variable: env
    // Protected Mplex Top Level Env
    protected mby_mc_env_pkg::mby_mc_env            env;

    // Variable: tb_vif;
    // Handle to Mplex TB interface.
    virtual mby_mc_tb_if                            tb_vif;

    `uvm_object_utils(mby_mc_hard_reset_seq)
    `uvm_declare_p_sequencer(slu_sequencer)

    //------------------------------------------------------------------------------
    //  Constructor: new
    //  New Mplex Hard Reset Sequence Object.
    //  Gets handle to the Mplex ENV.
    //
    //  Arguments:
    //  string name  - Mplex TOP Hard Reset sequence object name.
    //------------------------------------------------------------------------------
    function new(input string name = "mby_mc_hard_reset_seq");
        super.new(name);
        set_env(slu_tb_env::get_top_tb_env());

        tb_vif = env.get_tb_vif();
    endfunction: new

    //------------------------------------------------------------------------------
    //  Function: set_env
    //  Handle to Mplex Top Level env for use in sequences
    //
    //  Arguments:
    //  slu_tb_env tb_env  -  Handle to the ENV
    //------------------------------------------------------------------------------
    virtual function void set_env(slu_tb_env tb_env);
        mby_mc_env_pkg::mby_mc_env temp_env;
        bit stat;

        stat = $cast(temp_env,tb_env);
        `slu_assert(    stat, ($psprintf("Cast of $s(type: $s) failed!!!",tb_env.get_name(),tb_env.get_type_name())));
        `slu_assert(temp_env, ("Could not fetch slu_tb_env handle!!!"));

        this.env    = temp_env;
    endfunction : set_env

    //------------------------------------------------------------------------------
    //  Task: body
    //  Sequence body is used to control Hard_Reset (Set -> Delay -> Clear),
    //  as well as Warm_Reset (Set)
    //------------------------------------------------------------------------------
    task body();

        `uvm_info(get_name(), $sformatf("Hard_Reset Set"), UVM_NONE);
        tb_vif.hard_reset                 = 1;
        `uvm_info(get_name(), $sformatf("Hard_Reset Set"), UVM_NONE);
        tb_vif.warm_reset                 = 1;

        repeat (200) @(posedge tb_vif.clk);

        `uvm_info(get_name(), $sformatf("Hard_Reset Cleared"), UVM_NONE);
        tb_vif.hard_reset                 = 0;

    endtask: body

endclass: mby_mc_hard_reset_seq

`endif // __MBY_MC_HARD_RESET_SEQ_GUARD
