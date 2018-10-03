// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :  Raghu P Gudla
// Created On   :  09/22/2018
// Description  :  APB transaction sequence
// ----------------------------------------------------------------------------------------------------

class fc_apb_txn_seq extends fc_apb_master_base_seq;

 rand int unsigned sequence_length = 10;

    //constrain the sequence length to a reasonable value 
    constraint reasonable_sequence_length {
        sequence_length <= 100;
    } 

    `uvm_object_utils(fc_apb_txn_seq)

    rand svt_apb_transaction::xact_type_enum        apb_xact_type;

    rand bit [63:0]   apb_addr;
    rand bit [7:0]    data;
    rand bit [2:0]   apb_wait_cycles;
    // class constructor 
    function new(string name="fc_apb_txn_seq");
        super.new(name);
    endfunction

    virtual task body();
        svt_apb_master_transaction apb_tran;
        svt_configuration get_cfg;
        bit status;
        `uvm_info("body", "started running fc_apb_txn_seq..", UVM_LOW)

        super.body();

        // obtain a handle to the port configuration 
        p_sequencer.get_cfg(get_cfg);
        if (!$cast(cfg, get_cfg)) begin
            `uvm_fatal("body", "unable to $cast the configuration to a svt_apb_port_configuration class");
        end
      `uvm_create(apb_tran)

if(apb_xact_type == svt_apb_transaction::WRITE) begin 

      /** set up the write transaction */
      `uvm_create(apb_tran)
      apb_tran.cfg       = cfg;
      apb_tran.xact_type = apb_xact_type;
      apb_tran.address   = apb_addr;
      apb_tran.data      = data ;
      apb_tran.num_wait_cycles = apb_wait_cycles ;

       `uvm_info("body", "APB WRITE transaction",UVM_LOW);
end

else if(apb_xact_type == svt_apb_transaction::READ) begin 
 
 `uvm_create(apb_tran)
      apb_tran.cfg       = cfg;
      apb_tran.xact_type = svt_apb_transaction::READ;
      apb_tran.address   = apb_addr;

      `uvm_info("body", "APB READ transaction ", UVM_LOW);
end

        `uvm_send(apb_tran)

        // wait for the  transaction to complete 
        get_response(rsp);

 

     `uvm_info("body", "exiting...", UVM_LOW)
    endtask: body

endclass: fc_apb_txn_seq



