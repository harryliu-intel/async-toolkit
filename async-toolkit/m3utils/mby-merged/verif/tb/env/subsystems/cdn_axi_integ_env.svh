// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :  Raghu P Gudla
// Created On   :  09/22/2018
// Description  :  CDN AXI SVT integration environment
// -----------------------------------------------------------------------------

`ifdef CDN_AXI_ENV_ENABLE

class cdn_axi_integ_env extends subsystem_base_env;

    `uvm_component_utils(cdn_axi_integ_env)
    
    // defines a system ENV for the AXI SVT suite. the AXI system ENV encapsulates the master agents,
    // slave agents, interconnect env, system monitor, system sequencer and the system configuration.
    // the number of master and slave agents is configured based on the system configuration provided by the user.
    // the user can specify whether interconnect env is required in the system env, using the system configuration
    cdn_axi_env                         cdn_axi_bfm;

    cdn_axi_virtual_sequencer            cdn_virtual_seqr;

    function new( string name ="cdn_axi_integ_env", uvm_component parent = null);
        super.new(name, parent);
    endfunction : new

    function void build_phase(uvm_phase phase);
        `uvm_info(get_type_name(), "start CDN AXI ENV build phase", UVM_MEDIUM)
        super.build_phase(phase);

        build_CdnAxiEnv();

        `uvm_info(get_type_name(), "end of CDN AXI ENV build phase", UVM_MEDIUM)
    endfunction : build_phase



    function void connect_phase(uvm_phase phase);
        `uvm_info(get_type_name(), "start CDN AXI ENV connect phase", UVM_MEDIUM)
        super.connect_phase(phase);
        $cast(cdn_virtual_seqr.slaveSeqr, cdn_axi_bfm.active_slave.sequencer);
        $cast(cdn_virtual_seqr.master_seqr, cdn_axi_bfm.active_master.sequencer);
        $cast(cdn_virtual_seqr.p_env, cdn_axi_bfm);
        `uvm_info(get_type_name(), "end of CDN AXI ENV connect phase", UVM_MEDIUM)
    endfunction : connect_phase


    function void end_of_elaboration_phase(uvm_phase phase);
        `uvm_info(get_type_name(), "start CDN AXI ENV end_of_elaboration phase", UVM_MEDIUM)
        super.end_of_elaboration_phase(phase);
        `uvm_info(get_type_name(), "end of CDN AXI ENV end_of_elaboration phase", UVM_MEDIUM)
    endfunction : end_of_elaboration_phase


    function void build_CdnAxiEnv();

        super.build();
        cdn_axi_bfm = cdn_axi_env::type_id::create("cdn_axi_bfm", this);
        cdn_virtual_seqr = cdn_axi_virtual_sequencer::type_id::create("cdn_virtual_seqr", this);    

    endfunction : build_CdnAxiEnv

    //---------------------------------------------------------------------------
    // function: get_tb_cfg()
    // returns object handle to mplex env configuration (mby_mc_tb_top_cfg)
    //---------------------------------------------------------------------------
    //function cdn_axi_system_cfg get_tb_cfg();
    //   return cdn_axi_bfm_cfg;
    //endfunction : get_tb_cfg

endclass

`endif

