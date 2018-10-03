// -----------------------------------------------------------------------------
// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :  Raghu P Gudla
// Created On   :  09/22/2018
// Description  : cfg_base_test class where all FC config functional tests are ext
//
// -----------------------------------------------------------------------------
`ifndef _Fc_func_cfg_base_test_svh_
`define _Fc_func_cfg_base_test_svh_

class fc_cfg_base_test extends fc_base_test;

    local const string CLASSNAME = "fc_cfg_base_test";

    // ------------------------------------------------------------------------
    function new(string name = "tb_base_test", uvm_component p=null);
        super.new(name, p);
        set_timeout(300us, "USER_DATA_PHASE");
    endfunction : new
    
    // ------------------------------------------------------------------------
    virtual function void build_phase(uvm_phase phase);
        super.build_phase(phase);
        
        `uvm_info(get_name(), "fc_cfg_base_test build phase started", UVM_MEDIUM);

        `uvm_info(get_name(), "fc_cfg_base_test build phase end", UVM_MEDIUM);

    endfunction : build_phase

    // ------------------------------------------------------------------------
    virtual function void connect_phase(uvm_phase phase);
        super.connect_phase(phase);
        tb_env.set_test_phase_type(FC::FC_TBENV_NAME, "POWER_GOOD_PHASE", "fc_seq_pkg::fc_powergood_seq");
        tb_env.set_test_phase_type(FC::FC_TBENV_NAME, "TRAINING_PHASE",   "fc_seq_pkg::fc_training_seq");
        tb_env.set_test_phase_type(FC::FC_TBENV_NAME, "CONFIG_PHASE",     "fc_seq_pkg::fc_config_seq");
        tb_env.set_test_phase_type(FC::FC_TBENV_NAME, "FLUSH_PHASE",      "fc_seq_pkg::fc_flush_seq");

    endfunction : connect_phase

    // -------------------------------------------------------------------------
    virtual function void start_of_simulation_phase (uvm_phase phase);
        super.start_of_simulation_phase(phase);

    endfunction : start_of_simulation_phase 

    // ---------------------------------------------------------------------------
    // standard UVM run task
    // ---------------------------------------------------------------------------
    task run_phase (uvm_phase phase);
        super.run_phase(phase);

    endtask

    // ------------------------------------------------------------------------
    // UVM macros
    // ------------------------------------------------------------------------
    `uvm_component_param_utils_begin(fc_cfg_base_test)
    `uvm_component_utils_end

endclass

`endif

