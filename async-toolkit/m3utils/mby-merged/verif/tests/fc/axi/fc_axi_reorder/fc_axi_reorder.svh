// -----------------------------------------------------------------------------
// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :  Ashwini Isarapu
// Created On   :  09/22/2018
// description  :  AXI re-order test
// -----------------------------------------------------------------------------
`ifdef AXI_ENV_ENABLE

class fc_axi_reorder_seq extends fc_seq_pkg::fc_base_seq;
    `uvm_object_utils(fc_axi_reorder_seq) 
    `uvm_declare_p_sequencer(slu_sequencer)

    sla_pkg::slu_sequencer virtual_seqr;

    slu_status_t status;
    slu_ral_data_t ral_data;
    slu_ral_data_t wr_data;

    function new(string name = "fc_axi_reorder_seq");
        super.new(name);
    endfunction

    virtual task body();
       fc_axi_reorder_txn_seq  axi_reorder_seq;
 
        `uvm_info(get_name(), "enter fc_axi_reorder_seq", UVM_LOW);

        #10us;
	
        `uvm_do_on(axi_reorder_seq,
                   tb_env.axi_seqr)
         
        `uvm_info(get_name(), "exit fc_axi_reorder_seq", UVM_LOW);
    endtask : body
endclass : fc_axi_reorder_seq

class fc_axi_reorder extends fc_base_test;
    `uvm_component_utils(fc_axi_reorder)

   function new(string name = "fc_axi_reorder", uvm_component parent = null);
        super.new(name, parent);

    endfunction

    virtual function void build_phase(uvm_phase phase);
        super.build_phase(phase);
        tb_env.skip_test_phase("CONFIG_PHASE");

//        uvm_config_db#(uvm_object_wrapper)::set(this, "tb_env.axi_subenv.axi_bfm.axi_system_env.sequencer.main_phase", "default_sequence", fc_axi_reorder_seq::type_id::get());
        uvm_config_db#(int unsigned)::set(this, "tb_env.axi_subenv.axi_bfm.axi_system_env.fc_axi_reorder_txn_seq", "sequence_length", 10);

        uvm_config_db#(uvm_object_wrapper)::set(this, "tb_env.axi_subenv.axi_bfm.axi_system_env.slave*.sequencer.run_phase", "default_sequence", fc_axi_slave_mem_rsp_seq::type_id::get());
    endfunction

    virtual function void connect_phase(uvm_phase phase);
        super.connect_phase(phase);
        tb_env.set_test_phase_type( "tb_env", "USER_DATA_PHASE", "fc_axi_reorder_seq");

    endfunction

    // UVM method -> end_of_elaboration function                             
    function void end_of_elaboration_phase(uvm_phase phase);                                       
        super.end_of_elaboration_phase (phase);                                            
    endfunction : end_of_elaboration_phase                                       
                                                                          
    // UVM method -> start_of_simulation function                      
    function void start_of_simulation_phase(uvm_phase phase); 
        super.start_of_simulation_phase (phase);   
        if($test$plusargs("SIMPROFILING")) begin
            set_timeout(10000us, "USER_DATA_PHASE");
        end      
    endfunction : start_of_simulation_phase              

   // testing

endclass
`endif
