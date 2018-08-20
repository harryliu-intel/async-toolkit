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

//   Class:    svt_bfm_env
//
//   This is the SVT_AXI_BFM Environment file.
//   It creates and configures the BFM agenst for MAC,PCS, and/or SERDES

`ifndef __SVT_AXI_BFM_ENV_GUARD
`define __SVT_AXI_BFM_ENV_GUARD

`ifndef __INSIDE_SVT_AXI_BFM_PKG__
`error "File is meant to be used only through the svt_axi_bfm_pkg.  Do not include it individually."
`endif

class svt_axi_bfm_env extends shdv_base_env;

    // Defines a System ENV for the AXI SVT suite. The AXI System ENV encapsulates the master agents,
    // slave agents, Interconnect Env, system monitor, system sequencer and the system configuration.
    // The number of master and slave agents is configured based on the system configuration provided by the user.
    // The user can specify whether Interconnect Env is required in the System Env, using the system configuration
    svt_axi_system_env   axi_system_env;

    // Variable:  axi_vif
    // Interface handle to the Main SVT AXI
    svt_axi_vif axi_vif;

    // Variable: sequencer
    // Virtual Sequencer class for Synopsys AXI BFM
    axi_virtual_sequencer sequencer;

    // Variable: axi_cfg
    // Synopsys AXI BFM custom configuration object for AXI Master/Slave config
    cust_svt_axi_system_configuration axi_cfg;

    axi_uvm_scoreboard axi_scoreboard;

    // Master Sink
    //listener master_listener;

    //Slave Sink
    //listener slave_listener;

    `uvm_component_utils_begin(svt_axi_bfm_env)
        `uvm_field_object(axi_system_env,       UVM_ALL_ON)
        `uvm_field_object(sequencer,            UVM_ALL_ON)
        `uvm_field_object(axi_cfg,              UVM_ALL_ON)
    `uvm_component_utils_end

    // ------------------------------------------------------------------------
    // Constructor: new
    // Create a Static pointer to this environment. Define the config type. Create
    // the env_cfg for controlling the SVT BFM.
    //
    // Arguments:
    // name   - svt_axi_bfm_env object name.
    // parent - parent component.
    // ------------------------------------------------------------------------
    function new(string name = "svt_axi_bfm_env", uvm_component parent = null);
        super.new(name, parent);

    // Set SLA CFG object type    -  (Refer to Saola user guide for info about Saola CFG obj)
    //config_type = "cust_svt_axi_system_configuration";


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


        if (!uvm_config_db#(svt_axi_vif)::get(this, "", "axi_vif", axi_vif)) begin
            `ovm_fatal(get_name(),"Unable to acquire handle to axi_vif!");
        end
        else begin
            uvm_config_db#(svt_axi_vif)::set(this, "axi_system_env", "vif", axi_vif);
        end

        //Construct the system agent
        if ( axi_cfg == null) begin
            `ovm_fatal(get_name(),"axi_cfg object not set! Use set_axi_cfg method to set the cfg.");
        end
        else begin
            //Pass down the configuration to SVT environment
            uvm_config_db#(svt_axi_system_configuration)::set(this, "axi_system_env", "cfg", axi_cfg);
        end

        axi_system_env = svt_axi_system_env::type_id::create("axi_system_env", this);

        //Construct the virtual sequencer
        sequencer = axi_virtual_sequencer::type_id::create("sequencer", this);

        // Create the scoreboard
        axi_scoreboard = axi_uvm_scoreboard::type_id::create("axi_scoreboard", this);

    //Create the Master Sink
    //master_listener = new("master_listener", this);

    //Create the Master Sink
    //slave_listener = new("slave_listener", this);

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
        //TODO: fix the connection for multi master/slave.
        if(axi_cfg.num_masters) begin
            axi_system_env.master[0].monitor.item_observed_port.connect(axi_scoreboard.item_observed_initiated_export);
        end
        if(axi_cfg.num_slaves) begin
            axi_system_env.slave[0].monitor.item_observed_port.connect(axi_scoreboard.item_observed_response_export);
        end
    //
    //Connect the listener class instances with analysis ports of Master and Slave
    //agent.
    //
    //axi_system_env.master[0].monitor.item_observed_port.connect(master_listener.analysis_export);
    // axi_system_env.slave[0].monitor.item_observed_port.connect(slave_listener.analysis_export);

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
    function void set_axi_cfg(cust_svt_axi_system_configuration cust_cfg);
        axi_cfg    = cust_cfg;
    endfunction: set_axi_cfg

    // ------------------------------------------------------------------------
    // Function setup_bfm()
    // Set the number of AXI BFM Masters, Slaves and data width.
    //
    // Arguments:
    // int num_masters   -Number of AXI masters.
    // int num_slaves    -Number of AXI Slaves.
    // int data_width    -Axi bus width.

    // ------------------------------------------------------------------------
    function void setup_bfm(int num_masters, int num_slaves, int data_width );
        axi_cfg.num_masters    = num_masters;
        axi_cfg.num_slaves    = num_slaves;
        axi_cfg.num_slaves    = num_slaves;
        axi_cfg.data_width    = data_width;
    endfunction: setup_bfm

    // ------------------------------------------------------------------------
    // Function set_axi_vif()
    // Set the Synopsys AXI BFM Interface.
    //
    // Arguments:
    // vif   - handle to the axi virtual interface.
    // ------------------------------------------------------------------------
    function void set_axi_vif(svt_axi_vif vif);
        axi_vif    = vif;
    endfunction: set_axi_vif
endclass

`endif // __SVT_AXI_BFM_ENV_GUARD
