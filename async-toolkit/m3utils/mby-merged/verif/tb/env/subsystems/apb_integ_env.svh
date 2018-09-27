// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :  Raghu P Gudla
// Created On   :  09/22/2018
// Description  :  APB SVT integration environment
// -----------------------------------------------------------------------------

`ifdef APB_ENV_ENABLE

class apb_integ_env extends subsystem_base_env;

    `uvm_component_utils(apb_integ_env)
    
    // defines a system ENV for the APB SVT suite. the APB system ENV encapsulates the master agents,
    // slave agents, interconnect env, system monitor, system sequencer and the system configuration.
    // the number of master and slave agents is configured based on the system configuration provided by the user.
    // the user can specify whether interconnect env is required in the system env, using the system configuration
    apb_bfm_env                         apb_bfm;

    // synopsys APB BFM custom configuration object for APB master/slave config
    apb_shared_cfg       apb_bfm_cfg;

    // interface handle to the main SVT APB
    svt_apb_vif apb_vif;

    // virtual sequencer class for synopsys APB BFM
    svt_apb_system_sequencer apb_sequencer;

    // APB data bus width.
    int apb_data_width;

    function new( string name ="apb_integ_env", uvm_component parent = null);
        super.new(name, parent);
    endfunction : new

    function void build_phase(uvm_phase phase);
        `uvm_info(get_type_name(), "start APB ENV build phase", OVM_MEDIUM)
        super.build_phase(phase);

         build_apb_env();

        `uvm_info(get_type_name(), "end of APB ENV build phase", OVM_MEDIUM)
    endfunction : build_phase



    function void connect_phase(uvm_phase phase);
        `uvm_info(get_type_name(), "start APB ENV connect phase", OVM_MEDIUM)
        super.connect_phase(phase);
        `uvm_info(get_type_name(), "end of APB ENV connect phase", OVM_MEDIUM)
    endfunction : connect_phase


    function void end_of_elaboration_phase(uvm_phase phase);
        `uvm_info(get_type_name(), "start APB ENV end_of_elaboration phase", OVM_MEDIUM)
        super.end_of_elaboration_phase(phase);
        `uvm_info(get_type_name(), "end of APB ENV end_of_elaboration phase", OVM_MEDIUM)
    endfunction : end_of_elaboration_phase


    function void build_apb_env();

        super.build();
        
        apb_bfm_cfg = apb_shared_cfg::type_id::create("apb_bfm_cfg");

        if (apb_bfm_cfg == null) begin
            `uvm_fatal(get_full_name(), "unable to acquire handle to apb_shared_cfg object!")
        end

        //build BFMs and push down knobs
        build_apb_bfm();

        //`uvm_info (get_full_name , $sformatf("mplex top _cfg : %s", apb_bfm_cfg.sprint()), UVM_FULL)

    endfunction : build_apb_env

        //---------------------------------------------------------------------------
    //  function: build_apb_bfm
    //  build and configure APB master and slave BFMs.
    //---------------------------------------------------------------------------
    function void build_apb_bfm();

        apb_bfm =  apb_bfm_env::type_id::create("apb_bfm", this);
        apb_bfm.set_apb_cfg(apb_bfm_cfg);
        `uvm_info(get_full_name(),$sformatf("setting APB BFM cfg:  MAPB addr_width= %0d, MAPB data_width = %0d, SAPB addr_width= %0d,SAPB data_width = %0d",apb_bfm_cfg.master_cfg.paddr_width,apb_bfm_cfg.master_cfg.pdata_width,apb_bfm_cfg.slave_cfg.paddr_width,apb_bfm_cfg.slave_cfg.pdata_width),UVM_LOW)

        //apb_bfm.setup_bfm(apb_bfm_cfg.num_masters, apb_bfm_cfg.num_slaves,apb_bfm_cfg.data_width);
        
        //apb_bfm.set_report_verbosity_level_hier(UVM_LOW);

    endfunction: build_apb_bfm

    //---------------------------------------------------------------------------
    // function: get_tb_cfg()
    // returns object handle to mplex env configuration (mby_mc_tb_top_cfg)
    //---------------------------------------------------------------------------
    function apb_shared_cfg get_tb_cfg();
        return apb_bfm_cfg;
    endfunction : get_tb_cfg

endclass

`endif

