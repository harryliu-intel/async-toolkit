// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :  Raghu P Gudla
// Created On   :  09/22/2018
// Description  :  CHI SVT integration environment
// -----------------------------------------------------------------------------

`ifdef CHI_ENV_ENABLE

class chi_integ_env extends subsystem_base_env;

    `uvm_component_utils(chi_integ_env)
    
    // defines a system ENV for the CHI SVT suite. the CHI system ENV encapsulates the master agents,
    // slave agents, interconnect env, system monitor, system sequencer and the system configuration.
    // the number of master and slave agents is configured based on the system configuration provided by the user.
    // the user can specify whether interconnect env is required in the system env, using the system configuration
    chi_bfm_env                         chi_bfm;

    // synopsys CHI BFM custom configuration object for CHI master/slave config
    chi_system_cfg                      chi_bfm_cfg;

    // interface handle to the main SVT CHI
    //svt_chi_if         chi_vif;

    // CHI data bus width.
    int chi_data_width;

    function new( string name ="chi_integ_env", uvm_component parent = null);
        super.new(name, parent);
    endfunction : new

    function void build_phase(uvm_phase phase);
        `uvm_info(get_type_name(), "start CHI ENV build phase", UVM_MEDIUM)
        super.build_phase(phase);

         build_chi_env();

        `uvm_info(get_type_name(), "end of CHI ENV build phase", UVM_MEDIUM)
    endfunction : build_phase



    function void connect_phase(uvm_phase phase);
        `uvm_info(get_type_name(), "start CHI ENV connect phase", UVM_MEDIUM)
        super.connect_phase(phase);
        `uvm_info(get_type_name(), "end of CHI ENV connect phase", UVM_MEDIUM)
    endfunction : connect_phase


    function void end_of_elaboration_phase(uvm_phase phase);
        `uvm_info(get_type_name(), "start CHI ENV end_of_elaboration phase", UVM_MEDIUM)
        super.end_of_elaboration_phase(phase);
        `uvm_info(get_type_name(), "end of CHI ENV end_of_elaboration phase", UVM_MEDIUM)
    endfunction : end_of_elaboration_phase


    function void build_chi_env();

        super.build();
        
        `uvm_info(get_type_name(), "start CHI BFM environment", UVM_MEDIUM)

        chi_bfm_cfg = chi_system_cfg::type_id::create("chi_bfm_cfg");
        chi_bfm_cfg.set_amba_sys_config();

        if (chi_bfm_cfg == null) begin
            `uvm_fatal(get_full_name(), "unable to acquire handle to chi_system_cfg object!")
        end

        //build BFMs and push down knobs
        build_chi_bfm();

        `uvm_info(get_type_name(), "end of CHI BFM environment", UVM_MEDIUM)
    endfunction : build_chi_env

        //---------------------------------------------------------------------------
    //  function: build_chi_bfm
    //  build and configure CHI master and slave BFMs.
    //---------------------------------------------------------------------------
    function void build_chi_bfm();

        chi_bfm =  chi_bfm_env::type_id::create("chi_bfm", this);
        chi_bfm.set_chi_cfg(chi_bfm_cfg);

    endfunction: build_chi_bfm

    //---------------------------------------------------------------------------
    // function: get_tb_cfg()
    // returns object handle to mplex env configuration (mby_mc_tb_top_cfg)
    //---------------------------------------------------------------------------
    function chi_system_cfg get_tb_cfg();
        return chi_bfm_cfg;
    endfunction : get_tb_cfg

endclass

`endif

