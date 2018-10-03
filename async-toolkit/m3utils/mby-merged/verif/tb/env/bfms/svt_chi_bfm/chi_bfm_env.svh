// -----------------------------------------------------------------------------
// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :  Raghu P Gudla
// Created On   :  09/22/2018
// Description  :  SNPS CHI BFM env configuring master/slave agent
// -----------------------------------------------------------------------------
  
class chi_bfm_env extends uvm_env;

    // Defines a System ENV for the CHI SVT suite. The CHI System ENV encapsulates the master agents,
    // slave agents, Interconnect Env, system monitor, system sequencer and the system configuration.
    // The number of master and slave agents is configured based on the system configuration provided by the user.
    // The user can specify whether Interconnect Env is required in the System Env, using the system configuration
    svt_amba_system_env   chi_system_env;

    // AMBA/CHI system configuration object to customize for requried CHI Master/Slave config
    chi_system_cfg            cfg;

    virtual svt_chi_if      chi_vif;

    // Reset Interface handle
    virtual ChiResetIf.chi_reset_modport chi_reset_mp;

    `uvm_component_utils_begin(chi_bfm_env)
        `uvm_field_object(chi_system_env,       UVM_ALL_ON)
        `uvm_field_object(cfg,              UVM_ALL_ON)
    `uvm_component_utils_end

    // ------------------------------------------------------------------------
    // Constructor: new
    // Create a Static pointer to this environment. Define the config type. Create
    // the env_cfg for controlling the SVT BFM.
    //
    // Arguments:
    // name   - chi_bfm_env object name.
    // parent - parent component.
    // ------------------------------------------------------------------------
    function new(string name = "chi_bfm_env", uvm_component parent = null);
        super.new(name, parent);

    // Set SLA CFG object type    -  (Refer to Saola user guide for info about Saola CFG obj)


    endfunction: new

    // ------------------------------------------------------------------------
    // Function: build_phase()
    // Get Handle to the Test Island Path.
    // Create Synopsys CHI BFM Configuration and Agent. Based on selected mode.
    // As well set up the custom configuration for those objects.
    // Create CallBack's.
    //
    // Arguments:
    // phase   - uvm_phase handle.
    // ------------------------------------------------------------------------
    virtual function void build_phase(uvm_phase phase);
        super.build_phase(phase);

        chi_system_env = svt_amba_system_env::type_id::create("chi_system_env", this);

        if (!uvm_config_db#(virtual ChiResetIf.chi_reset_modport)::get(this, "", "chi_reset_mp", chi_reset_mp)) begin
            `uvm_fatal(get_name(),"Unable to acquire handle to chi_reset_mp!");
        end

        if (!uvm_config_db#(svt_chi_vif)::get(this, "", "chi_vif", chi_vif)) begin
            `uvm_fatal(get_name(),"Unable to acquire handle to chi_vif!");
        end
        else begin
            uvm_config_db#(svt_chi_vif)::set(this, "chi_system_env", "chi_vif", chi_vif);
            uvm_config_db#(svt_chi_vif)::set(this, "chi_system_env", "vif", chi_vif);
        end

        //Construct the system agent
        if ( cfg == null) begin
            `uvm_fatal(get_name(),"cfg object not set! Use set_cfg method to set the cfg.");
        end
        else begin
            //Pass down the configuration to SVT environment
            uvm_config_db#(svt_amba_system_configuration)::set(this, "chi_system_env", "cfg", cfg);
            uvm_config_db#(svt_chi_system_configuration)::set(this, "chi_system_env", "cfg", cfg.chi_sys_cfg[0]);
        end

        // 
        //uvm_config_db::dump();

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
    // Set the Synopsys CHI BFM Custom Configuration object.
    //
    // Arguments:
    // cust_cfg   - handle to the custom svt configuration object.
    // ------------------------------------------------------------------------
    function void set_chi_cfg(chi_system_cfg cust_cfg);
        cfg    = cust_cfg;
    endfunction: set_chi_cfg

    // ------------------------------------------------------------------------
    // Function setup_bfm()
    // Set the number of CHI BFM Masters, Slaves and data width.
    //
    // Arguments:
    // int num_masters   -Number of CHI masters.
    // int num_slaves    -Number of CHI Slaves.
    // int data_width    -CHI bus width.

    // ------------------------------------------------------------------------
    function void setup_bfm(int num_masters, int num_slaves, int data_width );
        //cfg.num_masters    = num_masters;
        //cfg.num_slaves    = num_slaves;
        //cfg.data_width    = data_width; 
    endfunction: setup_bfm

    // ------------------------------------------------------------------------
    // Function set_chi_vif()
    // Set the Synopsys CHI BFM Interface.
    //
    // Arguments:
    // vif   - handle to the chi virtual interface.
    // ------------------------------------------------------------------------
    function void set_chi_vif(virtual svt_chi_if vif);
        chi_vif    = vif;
    endfunction: set_chi_vif
endclass

