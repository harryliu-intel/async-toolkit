// -----------------------------------------------------------------------------
// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :  Raghu P Gudla
// Created On   :  09/22/2018
// Description  :  APM BFM env - configures the BFM agents, Cfg, RAL (master/slave)
// -----------------------------------------------------------------------------

class apb_bfm_env extends uvm_env;

    // APB Master env
    svt_apb_system_env   apb_master_env;

    // APB Slave env
    svt_apb_system_env   apb_slave_env;

    // AMBA/APB system configuration object to customize for requried AXI Master/Slave config
    apb_shared_cfg apb_cfg;

    // Adapter
    svt_apb_reg_adapter reg2apb_adapter;

    // Interface handle to the SVT APB Master and Slave Interfaces
    svt_apb_vif apb_master_vif;
    svt_apb_vif apb_slave_vif;
    
    virtual ApbResetIf apb_reset_vif;

    // Virtual Sequencer class for Synopsys AXI BFM
    //apb_virtual_sequencer sequencer;


//    apb_scoreboard apb_scoreboard;

    `uvm_component_utils_begin(apb_bfm_env)
        `uvm_field_object(apb_master_env,       UVM_ALL_ON)
        `uvm_field_object(apb_slave_env,       UVM_ALL_ON)
        `uvm_field_object(apb_cfg,              UVM_ALL_ON)
    `uvm_component_utils_end

    // ------------------------------------------------------------------------
    // Constructor: new
    // Create a Static pointer to this environment. Define the config type. Create
    // the env_cfg for controlling the SVT BFM.
    //
    // Arguments:
    // name   - apb_bfm_env object name.
    // parent - parent component.
    // ------------------------------------------------------------------------
    function new(string name = "apb_bfm_env", uvm_component parent = null);
        super.new(name, parent);

    // Set SLA CFG object type    -  (Refer to Saola user guide for info about Saola CFG obj)
    //config_type = "apb_shared_cfg";


    endfunction: new

    // ------------------------------------------------------------------------
    // Function: build_phase()
    // Get Handle to the Test Island Path.
    // Create Synopsys AXI BFM Configuration and Agent. Based on selected mode.
    // As well set up the custom configuration for those objects.
    // Create CallBack's.
    //
    // Arguments:
    // phase   - uvm_phase handle.
    // ------------------------------------------------------------------------
    virtual function void build_phase(uvm_phase phase);
        super.build_phase(phase);

        if (!uvm_config_db#(svt_apb_vif)::get(uvm_root::get(), "uvm_test_top.tb_env.apb_subenv.apb_bfm.apb_master_env", "vif", apb_master_vif)) begin
            `uvm_fatal(get_name(),"Unable to acquire handle to apb_master_vif!");
        end

        if (!uvm_config_db#(svt_apb_vif)::get(uvm_root::get(), "uvm_test_top.tb_env.apb_subenv.apb_bfm.apb_slave_env", "vif", apb_slave_vif)) begin
            `uvm_fatal(get_name(),"Unable to acquire handle to apb_slave_vif!");
        end

        if (!uvm_config_db#(virtual ApbResetIf)::get(uvm_root::get(), "*", "apb_reset_vif", apb_reset_vif)) begin
            `uvm_fatal(get_name(),"Unable to acquire handle to apb_reset_vif!");
        end

        /*
        //Construct the APB Cfg agent
        if (apb_cfg == null) begin
            `uvm_fatal(get_name(),"apb_cfg object not set! Use set_apb_cfg method to set the cfg.");
        end
        else begin
            //Pass down the configuration to SVT environment
            uvm_config_db#(apb_shared_cfg)::set(this, "", "cfg", apb_cfg);
        end*/ 

        /** Apply the configuration to the Master ENV */
        uvm_config_db#(svt_apb_system_configuration)::set(this, "apb_master_env", "cfg", apb_cfg.master_cfg);

        /** Apply the configuration to the Slave ENV */
        uvm_config_db#(svt_apb_system_configuration)::set(this, "apb_slave_env", "cfg", apb_cfg.slave_cfg);

        /** Construct the system agents */
        apb_master_env = svt_apb_system_env::type_id::create("apb_master_env", this);
        apb_slave_env = svt_apb_system_env::type_id::create("apb_slave_env", this);

        // Creating Adapter and connecting port_config
        reg2apb_adapter = svt_apb_reg_adapter::type_id::create("reg2apb_adapter", this);
        reg2apb_adapter.p_cfg  = apb_cfg.master_cfg;

        // Create the scoreboard
        //apb_scoreboard = apb_scoreboard::type_id::create("apb_scoreboard", this);

        //apb_scoreboard.apb_reset_vif = apb_reset_vif; 

    endfunction

    // ------------------------------------------------------------------------
    // Function: connect_phase()
    // Connect the CallBack's.  Connect the Virtual Sequencer.
    //
    // Arguments:
    // phase   - uvm_phase handle.
    // ------------------------------------------------------------------------
    function void connect_phase(uvm_phase phase);
        super.connect_phase(phase);

        //
        //Connect the master and slave agent's analysis ports with
        //item_observed_before_export and item_observed_after_export ports of the
        //scoreboard.
/*        if(apb_cfg.num_masters) begin
            apb_system_env.master[0].monitor.item_observed_port.connect(apb_scoreboard.item_observed_initiated_export);
        end
        if(apb_cfg.num_slaves) begin
            apb_system_env.slave[0].monitor.item_observed_port.connect(apb_scoreboard.item_observed_response_export);
        end */
    //
    //Connect the listener class instances with analysis ports of Master and Slave
    //agent.
    //
    //apb_system_env.master[0].monitor.item_observed_port.connect(master_listener.analysis_export);
    // apb_system_env.slave[0].monitor.item_observed_port.connect(slave_listener.analysis_export);

    endfunction: connect_phase

    // ------------------------------------------------------------------------
    // Function: end_of_elaboration_phase()
    //
    // Arguments:
    // phase   - uvm_phase handle.
    // ------------------------------------------------------------------------
    function void end_of_elaboration_phase(uvm_phase phase);
        super.end_of_elaboration_phase(phase);
    endfunction: end_of_elaboration_phase

    // ------------------------------------------------------------------------
    // Function: start_of_simulation_phase()
    //
    // Arguments:
    // phase   - uvm_phase handle.
    // ------------------------------------------------------------------------
    function void start_of_simulation_phase(uvm_phase phase);
        super.start_of_simulation_phase(phase);
    endfunction: start_of_simulation_phase

    // ------------------------------------------------------------------------
    // Function set_bfm_cfg()
    // Set the Synopsys AXI BFM Custom Configuration object.
    //
    // Arguments:
    // cust_cfg   - handle to the custom svt configuration object.
    // ------------------------------------------------------------------------
    function void set_apb_cfg(apb_shared_cfg cust_cfg);
        apb_cfg    = cust_cfg;
    endfunction: set_apb_cfg

    // ------------------------------------------------------------------------
    // Function setup_bfm()
    // Set the number of AXI BFM Masters, Slaves and data width.
    //
    // Arguments:
    // int num_masters   -Number of AXI masters.
    // int num_slaves    -Number of AXI Slaves.
    // int data_width    -Apb bus width.

    // ------------------------------------------------------------------------
    function void setup_bfm(int num_masters, int num_slaves, int data_width );
/*        apb_cfg.num_masters    = num_masters;
        apb_cfg.num_slaves    = num_slaves;
        apb_cfg.num_slaves    = num_slaves;
        apb_cfg.data_width    = data_width;*/
    endfunction: setup_bfm

    // ------------------------------------------------------------------------
    // Function set_apb_vif()
    // Set the Synopsys AXI BFM Interface.
    //
    // Arguments:
    // vif   - handle to the apb virtual interface.
    // ------------------------------------------------------------------------
    function void set_apb_master_vif(svt_apb_vif vif);
        apb_master_vif    = vif;
    endfunction: set_apb_master_vif

    function void set_apb_slave_vif(svt_apb_vif vif);
        apb_slave_vif    = vif;
    endfunction: set_apb_slave_vif
endclass

