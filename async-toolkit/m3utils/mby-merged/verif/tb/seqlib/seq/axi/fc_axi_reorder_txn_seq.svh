// -----------------------------------------------------------------------------
// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :  Raghu P Gudla
// Created On   :  09/22/2018
// Description  :  Sequence to issue Rd/Wr in random order
// ----------------------------------------------------------------------------------------------------

class fc_axi_reorder_txn_seq extends fc_axi_master_base_seq;

  /** parameter that controls the number of transactions that will be generated */
  rand int unsigned sequence_length = 10;

  /** constrain the sequence length to a reasonable value */
  constraint reasonable_sequence_length {
    sequence_length <= 70;
  }

  /** UVM object utility macro */
  `uvm_object_utils(fc_axi_reorder_txn_seq)

  svt_axi_master_transaction write_tran[$],read_tran[$];

  /** class constructor */
  function new(string name="fc_axi_reorder_txn_seq");
    super.new(name);
  endfunction
  
  virtual task body();
    bit status;
    `uvm_info("body", "entered ...", UVM_LOW)

    super.body();

    status = uvm_config_db #(int unsigned)::get(null, get_full_name(), "sequence_length", sequence_length);
    `uvm_info("body", $sformatf("sequence_length is %0d as a result of %0s.", sequence_length, status ? "config DB" : "randomization"), UVM_LOW);

    fork
    forever begin
      get_response(rsp);
    end
    join_none
    
    for (int i = 0;i < sequence_length;i++) begin 
      `uvm_do_with(write_tran[i], 
        { 
          xact_type == svt_axi_transaction::WRITE;
          if (i == 0) {
            id == 0;
          }
          else {
            id == 1;
          }  
          addr_valid_delay == 0; 
          data_before_addr == 0;
          burst_type   == svt_axi_transaction::INCR;
          bready_delay == 0;
          foreach(wvalid_delay[ix]) {
            wvalid_delay[ix] == 0;
          }
        })
    end 
    for (int i = 0;i <sequence_length;i++) begin 
      write_tran[i].wait_for_transaction_end();
    end  
    
    for (int x = 0;x < sequence_length;x++) begin 
      `uvm_do_with(read_tran[x], 
        { 
          xact_type == svt_axi_transaction::READ;
          if (x == 0) {
            id == 10;
          }
          else {
            id == 20;
          } 
          addr_valid_delay == 0; 
          data_before_addr == 0;
          foreach(rready_delay[i]) {
            rready_delay[i] == 0;
          }  
        })
    end
    for (int x = 0;x <sequence_length;x++) begin 
      read_tran[x].wait_for_transaction_end();
    end  

    `uvm_info("body", "exiting...", UVM_LOW)
  endtask: body

endclass: fc_axi_reorder_txn_seq

































