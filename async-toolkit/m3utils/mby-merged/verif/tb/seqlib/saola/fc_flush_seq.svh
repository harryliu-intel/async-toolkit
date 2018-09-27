// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :  Raghu P Gudla
// Created On   :  09/22/2018
// Description  :  FC flush sequence
// -----------------------------------------------------------------------------

class fc_flush_seq extends fc_base_seq;

    const string CLASSNAME = "fc_flush_seq";
    slu_status_t status;  
    slu_ral_data_t ral_data;

    // -----------------------------------------------------------------------
    function new(string name = "fc_flush_seq");
        super.new(name);
    endfunction

    // -----------------------------------------------------------------------
    `uvm_object_utils(fc_seq_pkg::fc_flush_seq) 
    `uvm_declare_p_sequencer(slu_sequencer)

    // -----------------------------------------------------------------------
    virtual task body();
        `uvm_info(get_name(), "enter fc_flush_seq", UVM_MEDIUM);

        `uvm_info(get_name(), "exit fc_flush_seq", UVM_MEDIUM);

        // required by postsim script to signal test completion
        $display("TEST_COMPLETED");
    endtask

endclass
