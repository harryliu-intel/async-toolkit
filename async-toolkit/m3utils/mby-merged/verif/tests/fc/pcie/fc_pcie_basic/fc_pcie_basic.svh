// -----------------------------------------------------------------------------
// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :  Jerry Shaw
// Created On   :  10/02/2018
// Description  :  FC pcie basic read/write test
// -----------------------------------------------------------------------------

class fc_pcie_basic_seq extends fc_seq_pkg::fc_base_seq;
    `uvm_object_utils(fc_pcie_basic_seq) 
    `uvm_declare_p_sequencer(slu_sequencer)

    sla_pkg::slu_sequencer virtual_seqr;

    slu_status_t status;
    sla_status_t sla_status;
    slu_ral_data_t ral_data;
    slu_ral_data_t wr_data;

    function new(string name = "fc_pcie_basic_seq");
        super.new(name);
    endfunction

    virtual task body();
        
        // simple read followed by write
        cdn_pcie_pkg::pcie_TLReadAfterWriteSeq pcieRdWrSeq;
    
        `uvm_info(get_name(), "enter fc_pcie_basic_seq", UVM_LOW);

        // run test on RC0 BFM
        `uvm_do_on_with(pcieRdWrSeq, 
                        tb_env.pep_subenv.rcBfmEnv0.rc_bfm.sequencer,
                        {}) 

        #1us;

        `uvm_info(get_name(), "exit fc_pcie_basic_seq", UVM_LOW);
    endtask : body

endclass : fc_pcie_basic_seq

class fc_pcie_basic extends fc_base_test;
    `uvm_component_utils(fc_pcie_basic)

   function new(string name = "fc_pcie_basic", uvm_component parent = null);
        super.new(name, parent);
    endfunction

    virtual function void build_phase(uvm_phase phase);
        set_timeout(1000us, "USER_DATA_PHASE");
        super.build_phase(phase);
        tb_env.skip_test_phase("CONFIG_PHASE");
    endfunction

    virtual function void connect_phase(uvm_phase phase);
        super.connect_phase(phase);
        tb_env.set_test_phase_type( "tb_env", "USER_DATA_PHASE", "fc_pcie_basic_seq");

    endfunction

    function void end_of_elaboration_phase(uvm_phase phase);                                       
        super.end_of_elaboration_phase (phase);                                            
    endfunction : end_of_elaboration_phase                                       
                                                                          
    function void start_of_simulation_phase(uvm_phase phase); 
        super.start_of_simulation_phase (phase);   
        if($test$plusargs("SIMPROFILING")) begin
            set_timeout(10000us, "USER_DATA_PHASE");
        end      
    endfunction : start_of_simulation_phase              


endclass


