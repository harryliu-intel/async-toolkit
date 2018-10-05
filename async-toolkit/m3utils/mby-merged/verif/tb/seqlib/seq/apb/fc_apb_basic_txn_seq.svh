// -----------------------------------------------------------------------------
// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :  Raghu P Gudla
// Created On   :  09/22/2018
// Description  :  APB Transaction Sequence
// ----------------------------------------------------------------------------------------------------

class fc_apb_basic_txn_seq extends fc_apb_master_base_seq;

    // parameter that controls the number of transactions that will be generated 
    rand int unsigned sequence_length = 10;

    //constrain the sequence length to a reasonable value 
    constraint reasonable_sequence_length {
        sequence_length <= 100;
    }
    

    //UVM object utility macro 
    `uvm_object_utils(fc_apb_basic_txn_seq)

    // class constructor 
    function new(string name="fc_apb_basic_txn_seq");
        super.new(name);
    endfunction

    virtual task body();

    //svt_apb_master_transaction write_tran, read_tran;
    apb_master_txn write_tran, read_tran;

    bit status;
    `uvm_info("body", "entered ...", UVM_LOW)

    super.body();

    status = uvm_config_db #(int unsigned)::get(null, get_full_name(), "sequence_length", sequence_length);
    `uvm_info("body", $sformatf("sequence_length is %0d as a result of %0s.", sequence_length, status ? "config DB" : "randomization"), UVM_LOW);

    for(int i = 0; i < sequence_length; i++) begin

      /** set up the write transaction */
      `uvm_create(write_tran)
      write_tran.cfg       = cfg;
      write_tran.xact_type = svt_apb_transaction::WRITE;
      write_tran.address   = 32'h0000_0100 | ('h100 * i);
      write_tran.data      = i;
      write_tran.num_wait_cycles = 3;

      /** send the write transaction */
      `uvm_send(write_tran)

      /** wait for the write transaction to complete */
      get_response(rsp);

      `uvm_info("body", "APB WRITE transaction completed", UVM_LOW);

      /** set up the read transaction */
      `uvm_create(read_tran)
      read_tran.cfg       = cfg;
      read_tran.xact_type = svt_apb_transaction::READ;
      read_tran.address   = 32'h0000_0100 | ('h100 * i);

      /** send the read transaction */
      `uvm_send(read_tran)

      /** wait for the read transaction to complete */
      get_response(rsp);
    
      `uvm_info("body", "APB READ transaction completed", UVM_LOW);
    end

    `uvm_info("body", "exiting...", UVM_LOW)    
    endtask: body

endclass: fc_apb_basic_txn_seq



