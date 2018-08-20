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

//   Class:    mby_mc_axi_random_access_test
//

`ifndef MBY_MC_AXI_RANDOM_ACCESS__SV
`define MBY_MC_AXI_RANDOM_ACCESS__SV

`ifndef __INSIDE_MBY_MC_TEST_LIB
`error "Attempt to include file outside of mby_mc_test_lib."
`endif

class mby_mc_axi_random_access_test extends mby_mc_base_test;

    `uvm_component_utils(mby_mc_axi_random_access_test)
    //------------------------------------------------------------------------------
    // Constructor: new
    //  Arguments:
    //  name   - Mplex alive test object name.
    //  parent - Component parent object.
    //------------------------------------------------------------------------------
    function new (string name="mby_mc_axi_random_access_test", uvm_component parent=null);
        super.new (name, parent);
    endfunction :  new

    //------------------------------------------------------------------------------
    // Function: build_phase
    //  Arguments:
    //  phase - uvm_phase object.
    //------------------------------------------------------------------------------
    function void build_phase(uvm_phase phase);
        super.build_phase(phase);
        cfg.env_cfg.axi_num_masters = 1;
        `ovm_info(get_name(), $sformatf("TEST: setting num_master = %0d ",cfg.env_cfg.axi_num_masters), OVM_MEDIUM);
        set_config_object("env", "mby_mc_tb_top_cfg", cfg, 0);
    endfunction : build_phase

    //------------------------------------------------------------------------------
    // Function: connect_phase
    // Sets USER_DATA_PHASE sequence.
    //
    //  Arguments:
    //  phase - uvm_phase object.
    //------------------------------------------------------------------------------
    function void connect_phase(uvm_phase phase);
        super.connect_phase(phase);
        env.set_test_phase_type("env", "USER_DATA_PHASE", "mby_mc_axi_random_seq");
    endfunction : connect_phase

endclass : mby_mc_axi_random_access_test

class mby_mc_axi_random_seq extends mby_mc_seq_lib::mby_mc_env_base_seq;

    `uvm_object_utils(mby_mc_axi_random_seq)

    //------------------------------------------------------------------------------
    //  Constructor: new
    //  Arguments:
    //  name   - Mplex alive test  seq object name.
    //------------------------------------------------------------------------------
    function new (string name="mby_mc_axi_random_seq");
        super.new (name);
        set_env(slu_tb_env::get_top_tb_env());
    endfunction :  new

    //------------------------------------------------------------------------------
    //  Task:  body
    //------------------------------------------------------------------------------
    task body ();

        // Variable: axi_directed_seq
        // Sequence to be executed in this test.  "axi_master_directed_sequence"
        svt_axi_bfm_pkg::axi_master_directed_sequence  axi_directed_seq;

        `ovm_info(get_name(), "mby_mc_axi_random_seq is running!", OVM_MEDIUM);

        axi_directed_seq = svt_axi_bfm_pkg::axi_master_directed_sequence::type_id::create("seq");
        `uvm_do_on_with(axi_directed_seq,
            env.axi_bfm.axi_system_env.master[0].sequencer,
            {sequence_length == 1;              // number of write followed by read transaction
            })

        `ovm_info(get_name(), $sformatf("mby_mc_axi_random_seq Complete"), OVM_HIGH);

    endtask

endclass : mby_mc_axi_random_seq

`endif // MBY_MC_AXI_RANDOM_ACCESS__SV
