// -----------------------------------------------------------------------------
// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :  Raghu P Gudla
// Created On   :  09/22/2018
// Description  :  base_test class where all FC functional tests are extended 
// -----------------------------------------------------------------------------
`ifndef _Fc_func_base_test_svh_
`define _Fc_func_base_test_svh_

class fc_base_test extends fc_test;

    local const string CLASSNAME = "fc_base_test";
    sla_pkg::slu_im_env            im;

    virtual sig_if pins;
    time timeouts[string];
    bit  push_timeouts = 0;
    int  uvm_max_error = 1000;
    bit exi_test = 0;
    
    fc_cfg_obj fcCfgobj; // config object where items are randomized

    // ------------------------------------------------------------------------
    function new(string name = "tb_base_test", uvm_component p=null);
        super.new(name, p);
        //pins = FC::getsig_if();
        uvm_config_db#(virtual sig_if)::get(this, "", "sig_if", pins);

        // set timeout defaults for functional tests
        set_timeout(1000us, "POWER_GOOD_PHASE");
        set_timeout(3000us, "HARD_RESET_PHASE");
        set_timeout(500us, "WARM_RESET_PHASE");
        set_timeout(500us, "CONFIG_PHASE");
        set_timeout(500us, "TRAINING_PHASE");
        set_timeout(500us, "USER_DATA_PHASE");
        set_timeout(200us, "FLUSH_PHASE");

    endfunction : new
    
    // ------------------------------------------------------------------------
    virtual function void build_phase(uvm_phase phase);  
        super.build_phase(phase);
        
        `uvm_info(get_name(), "fc_base_test build phase started", UVM_MEDIUM);

        uvm_default_table_printer.knobs.name_width     = 50;
        uvm_default_table_printer.knobs.type_width     = 50;
        uvm_default_table_printer.knobs.value_width    = 50;
        uvm_default_table_printer.knobs.begin_elements = 10;
        uvm_default_table_printer.knobs.end_elements   = 10;
        uvm_default_table_printer.knobs.reference      =  0;

        //slu_sm_config::apply_all_overrides();
        
        // change UVM max error count to prevent simulation terminated earlier.
        $value$plusargs("UVM_MAX_ERR=%d", uvm_max_error);
        set_report_max_quit_count(uvm_max_error);

        // build the cfg obj
        build_cfg_obj();

        uvm_config_int::set(this, "*_sequencer", "count", 0);
        uvm_config_string::set(this, "*", "fuse_slu_env_name", "fusectrl_slu_env");

        `uvm_info(get_name(), "fc_base_test build phase end", UVM_MEDIUM);

    endfunction : build_phase

    // ------------------------------------------------------------------------
    function void build_cfg_obj(); 

        fcCfgobj = fc_cfg_obj::type_id::create(FC::CFG_OBJ, this);
        uvm_config_object::set(this, "*", FC::CFG_OBJ, fcCfgobj);
        randomize(fcCfgobj);
        `uvm_info(get_name(), {"randomized config object hierarchy:\n",fcCfgobj.sprint()}, UVM_HIGH);

    endfunction : build_cfg_obj

    // ------------------------------------------------------------------------
    virtual function void connect_phase(uvm_phase phase);
        super.connect_phase(phase);
        tb_env.set_test_phase_type(FC::FC_TBENV_NAME, "POWER_GOOD_PHASE", "fc_seq_pkg::fc_powergood_seq");
        tb_env.set_test_phase_type(FC::FC_TBENV_NAME, "TRAINING_PHASE",   "fc_seq_pkg::fc_training_seq");
        tb_env.set_test_phase_type(FC::FC_TBENV_NAME, "CONFIG_PHASE",     "fc_seq_pkg::fc_config_seq");
        tb_env.set_test_phase_type(FC::FC_TBENV_NAME, "FLUSH_PHASE",      "fc_seq_pkg::fc_flush_seq");

    endfunction : connect_phase


    function void end_of_elaboration_phase(uvm_phase phase);
        super.end_of_elaboration_phase(phase);
    endfunction : end_of_elaboration_phase

    // -------------------------------------------------------------------------
    virtual function void start_of_simulation_phase (uvm_phase phase);
        super.start_of_simulation_phase(phase);
        FC::apply_forces.forces_set_by_testbench = 1;

        if($test$plusargs("DIS_SPI_FASTSIM")) begin
        FC::apply_forces.bypass_spi_desc_ss_ld = 0;
        end      

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
    `uvm_component_param_utils_begin(fc_base_test)
    `uvm_component_utils_end

endclass

`endif

