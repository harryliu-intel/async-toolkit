// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :  Raghu P Gudla
// Created On   :  09/22/2018
// Description  :  AXI master base sequence extending the SNPS AXI master seq.
// -----------------------------------------------------------------------------

class fc_axi_master_base_seq extends svt_axi_master_base_sequence;

    /*// parameter that controls the number of transactions that will be generated 
    rand int unsigned sequence_length = 10;

    //constrain the sequence length to a reasonable value 
    constraint reasonable_sequence_length {
        sequence_length <= 100;
    } */

    svt_axi_master_transaction axi_txn;
    svt_configuration get_cfg;

    rand svt_axi_transaction::xact_type_enum        axi_xact_type;
    rand svt_axi_transaction::atomic_type_enum      axi_atomic_type;
    rand svt_axi_transaction::burst_type_enum       axi_burst_type;
    rand svt_axi_transaction::burst_size_enum       axi_burst_size;

    rand bit [1023:0] axi_data[];
    rand bit [63:0]   axi_addr;
    rand bit [10:0]   axi_burst_length;
    rand bit [127:0]  axi_wstrb[];
    rand int    axi_wvalid_delay;

    //UVM object utility macro 
    `uvm_object_utils(fc_axi_master_base_seq)

    // class constructor 
    function new(string name="fc_axi_master_base_seq");
        super.new(name);
    endfunction

    virtual task body();
        bit status;

        `uvm_info("body", "started running fc_axi_master_base_seq..", UVM_LOW)

        super.body();

        // obtain a handle to the port configuration 
        p_sequencer.get_cfg(get_cfg);
        if (!$cast(cfg, get_cfg)) begin
            `uvm_fatal("body", "unable to $cast the configuration to a svt_axi_port_configuration class");
        end
            
        `uvm_info("body", "exiting...", UVM_LOW)
    endtask: body

endclass: fc_axi_master_base_seq



