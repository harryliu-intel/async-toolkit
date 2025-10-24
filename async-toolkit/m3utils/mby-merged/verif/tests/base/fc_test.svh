// -----------------------------------------------------------------------------
// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :  Raghu P Gudla
// Created On   :  09/22/2018
// Description  :  base test - parent of all FC (full chip) tests.
//                 instantiates the BFMs and testbench verification components.
//                 defines the test sequence flow used by all FC tests.
// -----------------------------------------------------------------------------

`ifndef _Fc_base_test_svh_
`define _Fc_base_test_svh_

class fc_test extends uvm_test;
//class fc_test extends xvm_uvm_test;
   
    const string CLASSNAME = "fc_test";
    fc_tb_env tb_env;
    time timeouts[string];
    bit  push_timeouts = 0;
    int  uvm_max_error = 20000;

    `uvm_component_utils_begin(fc_test)
    `uvm_component_utils_end

    // -------------------------------------------------------------------------
    // slu_sm_test_randomization_members macro is required in the class body so
    //   that the sm randomization flow is setup properly
//    `slu_sm_test_randomization_members;

    // -------------------------------------------------------------------------
    function new(string name = "tb_base_test", uvm_component p = null);
        super.new(name, p);

        // set timeout defaults
        set_timeout(500us, "POWER_GOOD_PHASE");
        set_timeout(3000us, "HARD_RESET_PHASE");
        set_timeout(500us, "WARM_RESET_PHASE");
        set_timeout(500us, "CONFIG_PHASE");
        set_timeout(500us, "TRAINING_PHASE");
        set_timeout(100000us, "USER_DATA_PHASE");
        set_timeout(200us, "FLUSH_PHASE");
    endfunction


    // -------------------------------------------------------------------------
    virtual function void build_phase(uvm_phase phase); 
        pull_cmdline_overrides();

        super.build_phase(phase);
		
        `uvm_info(get_name(), "fc_test build phase started", UVM_MEDIUM);
        
	// change UVM max error count to prevent simulation terminated earlier.
        $value$plusargs("UVM_MAX_ERR=%d", uvm_max_error);
        set_report_max_quit_count(uvm_max_error);

        tb_env = fc_tb_env::type_id::create("tb_env", this);
        //$cast(tb_env, xvm_wrapper.create_component("fc_tb_env", {get_full_name(),".tb_env"}, get_full_name(), OVM, UVM, .uparent(this)));

        uvm_config_int::set(this, "tb_env.*_sequencer", "count", 0);

        // -------------------------------------------------------------------
        // default test constraints

        // - disable saola watchdog timer
        uvm_config_int::set(this, "tb_env", "max_run_clocks", 20_000_000);

        // - disable saola random data phase
        uvm_config_int::set(this, "tb_env", "data_phase_mode", SLA_RANDOM_NONE);

        `uvm_info(get_name(), "fc_test build phase completed", UVM_MEDIUM);
        
    endfunction

    // -------------------------------------------------------------------------
    virtual function void connect_phase(uvm_phase phase);
        super.connect_phase(phase);

        tb_env.set_report_hier();

        `uvm_info(get_name(), "fc_test connect phase started", UVM_MEDIUM);

        // remove delays between saola phases
        tb_env.set_phase_delay("POWER_GOOD_PHASE", 0);
        tb_env.set_phase_delay("HARD_RESET_PHASE", 0);
        tb_env.set_phase_delay("WARM_RESET_PHASE", 0);
        tb_env.set_phase_delay("CONFIG_PHASE", 0);
        tb_env.set_phase_delay("TRAINING_PHASE", 0);
        tb_env.set_phase_delay("FLUSH_PHASE", 0);
        tb_env.set_phase_delay("RANDOM_DATA_PHASE", 0);
        tb_env.set_phase_delay("USER_DATA_PHASE",  0);

        // default saola phases
        //tb_env.set_test_phase_type("tb_env", "RANDOM_DATA_PHASE", "slu_random_sequence");
        
       `uvm_info(get_name(), "fc_test connect phase completed", UVM_MEDIUM);

    endfunction

    // -------------------------------------------------------------------------
    virtual function void start_of_simulation_phase (uvm_phase phase);
        `uvm_info(get_name(), "fc_test start_of_simulation phase started", UVM_MEDIUM);
        super.start_of_simulation_phase(phase);

        set_timeout(1000us, "FLUSH_PHASE");

        // set timeouts in saola
        push_timeouts = 1;
        foreach (timeouts[i]) begin
            set_timeout(timeouts[i], i);
        end      

        // - set global timeout to 1 second
        if (!$test$plusargs("UVM_GLOBAL_TIMEOUT")) begin
            uvm_pkg::set_global_timeout(1s);
        end      

        // set max quit count again (in case sub-env changed it)
        set_report_max_quit_count(uvm_max_error);
        `uvm_info(get_name(), "fc_test start_of_simulation phase completed", UVM_MEDIUM);
        $display("# fc_test start_of_simulation");

    endfunction

    // -------------------------------------------------------------------------
    task run_phase (uvm_phase phase);
	    $display("# fc_test start_of_simulation");
        super.run_phase(phase);
    endtask

    // -------------------------------------------------------------------------
    virtual function time set_timeout(time timeout, string phase = "USER_DATA_PHASE");
        timeouts[phase] = timeout;
        if (push_timeouts) begin
            string str;
            if ($value$plusargs({phase,"_SET_TIME=%s"}, str)) begin
                timeout = FC::parse_sim_time(str, {phase,"_SET_TIME"});
            end      

            if ($value$plusargs({phase,"_ADD_TIME=%s"}, str)) begin
                timeout = timeout + FC::parse_sim_time(str, {phase,"_ADD_TIME"});
            end      

            `uvm_info("set_timeout", $sformatf("setting %s timeout to %t", phase, timeout), UVM_NONE);
            tb_env.set_phase_timeout(phase, timeout / 10ns);
        end      
    endfunction

    // -------------------------------------------------------------------------
    virtual function void pull_cmdline_overrides();
        string setting;
        string setting_str[$];
        int setting_int;

        `uvm_info(get_name(), "enter CMDLINE OVERRIDES", UVM_NONE);

        if ($value$plusargs("set_config_int_%s", setting)) begin
            if ((split_string(setting, "=", setting_str) == 2) && ($sscanf(setting_str[1], "%d", setting_int))) begin
                `uvm_info(get_name(), $sformatf("uvm_top.set_config_int(\"*\", \"%s\", %0d);", setting_str[0], setting_int), UVM_NONE);
                uvm_top.set_config_int("*", setting_str[0], setting_int);
            end       else begin
                `uvm_fatal("bad set_config_int", $sformatf("could not parse set_config_int(%s", setting));
            end      
        end      
        if ($value$plusargs("set_config_string_%s", setting)) begin
            if (split_string(setting, "=", setting_str) == 2) begin
                `uvm_info(get_name(), $sformatf("uvm_top.set_config_string(\"*\", \"%s\", \"%s\");", setting_str[0], setting_str[1]), UVM_NONE);
                uvm_top.set_config_string("*", setting_str[0], setting_str[1]);
            end       else begin
                `uvm_fatal("bad set_config_string", $sformatf("could not parse set_config_string(%s)", setting));
            end      
        end      
        if ($value$plusargs("set_type_override_%s", setting)) begin
            if (split_string(setting, "=", setting_str) == 2) begin
                `uvm_info(get_name(), $sformatf("set_type_override(\"%s\", \"%s\", 1);", setting_str[0], setting_str[1]), UVM_NONE);
                set_type_override(setting_str[0], setting_str[1], 1);
            end       else begin
                `uvm_fatal("bad set_type_override", $sformatf("could not parse set_type_override(%s)", setting));
            end      
        end      
        if ($test$plusargs ("FILTER_ALL_ERRORS")) begin
            $display("ACE_ERR_FILTER: filter all errors in the test due to FILTER_ALL_ERRORS plusarg");
        end      
    endfunction

    // -------------------------------------------------------------------------
    static function int split_string (string str, byte sep, ref string values[$]);
        int s = 0, e = 0;
        values.delete();

        while (e < str.len()) begin
            for (s=e; e<str.len(); ++e) if (str[e] == sep) break;
            if (s != e) values.push_back(str.substr(s,e-1));
            e++;
        end      
        return values.size();
    endfunction

    // -------------------------------------------------------------------------

endclass

`endif
