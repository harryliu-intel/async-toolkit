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

//   Class:    mby_mesh_base_test
//
//   This is the Base Test file.


`ifndef MBY_MESH_BASE_TEST__SV
`define MBY_MESH_BASE_TEST__SV

class mby_mesh_base_test extends shdv_base_test;

    // Variable: cfg
    // Top level Mesh env configuration
    mby_mesh_tb_top_cfg cfg;

    // Variable: env
    // Top level ENV
    mby_mesh_env    env;

    `uvm_component_utils_begin(mby_mesh_base_test)
        `uvm_field_object(cfg,  UVM_DEFAULT)
        `uvm_field_object(env,      UVM_DEFAULT)
    `uvm_component_utils_end

    //------------------------------------------------------------------------------
    // Constructor: new
    // Configure the TimeFormat - $timeformat(-9, 0, "ns", 10);
    //
    //  Arguments:
    //  name   - Mesh base test object name.
    //  parent - Component parent object.
    //------------------------------------------------------------------------------
    function      new(string name = "mby_mesh_base_test", uvm_component parent = null);
        super.new (name, parent);
        $timeformat(-9, 0, "ns", 10);
    endfunction: new

    //------------------------------------------------------------------------------
    // Function: report_phase
    // Checks the Global report server as well as the Test for Errors.  Indicates End_of_Test PASS or FAIL
    //
    //  Arguments:
    //  phase - uvm_phase object.
    //------------------------------------------------------------------------------
    virtual function void report_phase(uvm_phase phase);
        uvm_report_server foo;
        int err_cnt;
        super.report_phase(phase);
        foo = uvm_top.get_report_server();

        // sum num of errors from sequences (use global report server)
        err_cnt = foo.get_severity_count(UVM_ERROR) + foo.get_severity_count(UVM_FATAL);

        // sum num of errors from this test.
        foo = get_report_server();
        err_cnt += (foo.get_severity_count(UVM_ERROR) + foo.get_severity_count(UVM_FATAL));

        if (err_cnt == 0) begin
            uvm_report_info(get_full_name(),  "                                                    ", UVM_LOG);
            uvm_report_info(get_full_name(),  "         _______  _______  _______  _______         ", UVM_LOG);
            uvm_report_info(get_full_name(),  "        |       ||   _   ||       ||       |        ", UVM_LOG);
            uvm_report_info(get_full_name(),  "        |    _  ||  |_|  ||  _____||  _____|        ", UVM_LOG);
            uvm_report_info(get_full_name(),  "        |   |_| ||       || |_____ | |_____         ", UVM_LOG);
            uvm_report_info(get_full_name(),  "        |    ___||       ||_____  ||_____  |        ", UVM_LOG);
            uvm_report_info(get_full_name(),  "        |   |    |   _   | _____| | _____| |        ", UVM_LOG);
            uvm_report_info(get_full_name(),  "        |___|    |__| |__||_______||_______|        ", UVM_LOG);
            uvm_report_info(get_full_name(),  "                                                    ", UVM_LOG);
        end
        else begin
            uvm_report_info(get_full_name(),  "                                                    ", UVM_LOG);
            uvm_report_info(get_full_name(),  "         _______  _______  ___   ___                ", UVM_LOG);
            uvm_report_info(get_full_name(),  "        |       ||   _   ||   | |   |               ", UVM_LOG);
            uvm_report_info(get_full_name(),  "        |    ___||  |_|  ||   | |   |               ", UVM_LOG);
            uvm_report_info(get_full_name(),  "        |   |___ |       ||   | |   |               ", UVM_LOG);
            uvm_report_info(get_full_name(),  "        |    ___||       ||   | |   |___            ", UVM_LOG);
            uvm_report_info(get_full_name(),  "        |   |    |   _   ||   | |       |           ", UVM_LOG);
            uvm_report_info(get_full_name(),  "        |___|    |__| |__||___| |_______|           ", UVM_LOG);
            uvm_report_info(get_full_name(),  "                                                    ", UVM_LOG);
            uvm_report_info(get_full_name(),  "                                                    ", UVM_LOG);
        end
    endfunction

    //------------------------------------------------------------------------------
    // Function: build_phase
    // Creates cfg, then Randomizes cfg's. Sets the cfg object. Then creates the mby_mesh_env.
    //
    //  Arguments:
    //  phase - uvm_phase object.
    //------------------------------------------------------------------------------
    virtual function void build_phase(uvm_phase phase);
        uvm_report_info(get_full_name(),"Build", UVM_LOG);

        cfg = mby_mesh_tb_top_cfg::type_id::create("cfg", this);                                 // Create a Top-Level Configuration object

        randomize_cfg();                                                                // Randomize the Top_Config

        set_config_object("env", "mby_mesh_tb_top_cfg", cfg, 0);

        env     = mby_mesh_env::type_id::create("env",this);                              // Create the Top Environment object
    endfunction : build_phase


    //------------------------------------------------------------------------------
    // Function: connect_phase
    //
    //  Arguments:
    //  phase - uvm_phase object.
    //------------------------------------------------------------------------------
    virtual function void connect_phase(uvm_phase phase);
        super.connect_phase(phase);
//PJP        env.set_test_phase_type("env", "POWER_GOOD_PHASE", "mby_mesh_power_good_seq");   // Specify the Phase to run the Power_Good sequence
//PJP        env.set_test_phase_type("env", "HARD_RESET_PHASE", "mby_mesh_hard_reset_seq");   // Specify the Phase to run the Hard_Reset sequence
//PJP        env.set_test_phase_type("env", "WARM_RESET_PHASE", "mby_mesh_warm_reset_seq");   // Specify the Phase to run the Warm_Reset sequence
//PJP        env.set_test_phase_type("env", "CONFIG_PHASE",     "mby_mesh_env_cfg_seq");
    endfunction : connect_phase

    //------------------------------------------------------------------------------
    // Function: randomize_cfg()
    // Randomizes the Top_cfg, which will randomizes any sub-level cfg as well.
    //------------------------------------------------------------------------------
    virtual function void randomize_cfg();
        `uvm_info(get_name(), "mby_mesh_base_test randomize_cfg was called!", UVM_HIGH);
        if (!this.randomize(cfg)) begin
            `uvm_fatal(get_name, "randomization of mby_mesh_tb_top_cfg failed");
        end

    endfunction : randomize_cfg

endclass : mby_mesh_base_test

`endif // MBY_MESH_BASE_TEST__SV
