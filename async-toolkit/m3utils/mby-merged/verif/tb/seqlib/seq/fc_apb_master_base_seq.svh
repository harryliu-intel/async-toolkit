// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :  Raghu P Gudla
// Created On   :  09/22/2018
// Description  :  APB master base sequence extending the APB system master seq.
// ----------------------------------------------------------------------------------------------------

class fc_apb_master_base_seq extends svt_apb_master_base_sequence;

    /*// parameter that controls the number of transactions that will be generated 
    rand int unsigned sequence_length = 10;

    //constrain the sequence length to a reasonable value 
    constraint reasonable_sequence_length {
        sequence_length <= 100;
    } */

    //UVM object utility macro 
    `uvm_object_utils(fc_apb_master_base_seq)

    // class constructor 
    function new(string name="fc_apb_master_base_seq");
        super.new(name);
    endfunction

    virtual task body();
        svt_apb_master_transaction write_tran, read_tran;
        apb_master_txn wr_txn, rd_txn;
        bit status;

        `uvm_info("body", "started running fc_apb_master_base_seq..", UVM_LOW)

        super.body();

        `uvm_info("body", "exiting...", UVM_LOW)
    endtask: body

endclass: fc_apb_master_base_seq



