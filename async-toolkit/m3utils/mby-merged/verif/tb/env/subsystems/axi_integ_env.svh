// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :  Raghu P Gudla
// Created On   :  09/22/2018
// Description  :  AXI SVT integration environment
// -----------------------------------------------------------------------------

`ifdef AXI_ENV_ENABLE

class axi_integ_env extends subsystem_base_env;

    `uvm_component_utils(axi_integ_env)
    
    // defines a system ENV for the AXI SVT suite. the AXI system ENV encapsulates the master agents,
    // slave agents, interconnect env, system monitor, system sequencer and the system configuration.
    // the number of master and slave agents is configured based on the system configuration provided by the user.
    // the user can specify whether interconnect env is required in the system env, using the system configuration
    axi_bfm_env                         axi_bfm;

    // synopsys AXI BFM custom configuration object for AXI master/slave config
    axi_system_cfg       axi_bfm_cfg;

    // variable:  axi_vif
    // interface handle to the main SVT AXI
    svt_axi_vif axi_vif;

    // virtual sequencer class for synopsys AXI BFM
    //axi_virtual_sequencer sequencer;
    svt_axi_system_sequencer axi_sequencer;

    // variable: axi_data_width
    // AXI data bus width.
    int axi_data_width;

    function new( string name ="axi_integ_env", uvm_component parent = null);
        super.new(name, parent);
    endfunction : new

    function void build_phase(uvm_phase phase);
        `uvm_info(get_type_name(), "start AXI ENV build phase", UVM_MEDIUM)
        super.build_phase(phase);

         build_axi_env();

        `uvm_info(get_type_name(), "end of AXI ENV build phase", UVM_MEDIUM)
    endfunction : build_phase



    function void connect_phase(uvm_phase phase);
        `uvm_info(get_type_name(), "start AXI ENV connect phase", UVM_MEDIUM)
        super.connect_phase(phase);
        `uvm_info(get_type_name(), "end of AXI ENV connect phase", UVM_MEDIUM)
    endfunction : connect_phase


    function void end_of_elaboration_phase(uvm_phase phase);
        `uvm_info(get_type_name(), "start AXI ENV end_of_elaboration phase", UVM_MEDIUM)
        super.end_of_elaboration_phase(phase);
        `uvm_info(get_type_name(), "end of AXI ENV end_of_elaboration phase", UVM_MEDIUM)
    endfunction : end_of_elaboration_phase


    function void build_axi_env();

        uvm_object tmp_cfg;

        super.build();
        
        axi_bfm_cfg = axi_system_cfg::type_id::create("axi_bfm_cfg");

        if (axi_bfm_cfg == null) begin
            `uvm_fatal(get_full_name(), "unable to acquire handle to axi_system_cfg object!")
        end

        //build BFMs and push down knobs
        build_axi_bfm();

    endfunction : build_axi_env

        //---------------------------------------------------------------------------
    //  function: build_axi_bfm
    //  build and configure AXI master and slave BFMs.
    //---------------------------------------------------------------------------
    function void build_axi_bfm();

        axi_bfm =  axi_bfm_env::type_id::create("axi_bfm", this);
        axi_bfm.set_axi_cfg(axi_bfm_cfg);
        `uvm_info(get_full_name(),$sformatf("setting AXI BFM cfg: num_masters =%0d, num_slaves = %0d\
                data_width = %0d",axi_bfm_cfg.num_masters,axi_bfm_cfg.num_slaves,
                axi_bfm_cfg.data_width),UVM_LOW)

        axi_bfm.setup_bfm(axi_bfm_cfg.num_masters, axi_bfm_cfg.num_slaves,axi_bfm_cfg.data_width);
        
        //axi_bfm.set_report_verbosity_level_hier(UVM_LOW);

    endfunction: build_axi_bfm

    //---------------------------------------------------------------------------
    // function: get_tb_cfg()
    // returns object handle to mplex env configuration (mby_mc_tb_top_cfg)
    //---------------------------------------------------------------------------
    function axi_system_cfg get_tb_cfg();
        return axi_bfm_cfg;
    endfunction : get_tb_cfg

endclass

`endif

