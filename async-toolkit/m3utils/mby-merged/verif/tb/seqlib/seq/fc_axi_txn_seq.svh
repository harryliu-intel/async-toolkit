// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :  Raghu P Gudla
// Created On   :  09/22/2018
// Description  :  AXI transaction sequence
// ----------------------------------------------------------------------------------------------------

class fc_axi_txn_seq extends fc_axi_master_base_seq;

    `uvm_object_utils(fc_axi_txn_seq)

    // class constructor 
    function new(string name="fc_axi_txn_seq");
        super.new(name);
    endfunction

    virtual task body();
        bit status;
        `uvm_info("body", "started running fc_axi_txn_seq..", UVM_LOW)

        super.body();
       
        // set up the write transaction 
        `uvm_create(axi_txn)

        if(axi_xact_type == svt_axi_transaction::WRITE) begin
            axi_txn.port_cfg     = cfg;
            axi_txn.xact_type    = axi_xact_type;
            axi_txn.addr         = axi_addr; 
            axi_txn.burst_type   = axi_burst_type;
            axi_txn.burst_size   = axi_burst_size;
            axi_txn.atomic_type  = axi_atomic_type;
            axi_txn.burst_length = axi_burst_length;
            axi_txn.data         = new[axi_txn.burst_length];
            axi_txn.wstrb        = new[axi_txn.burst_length];
            axi_txn.data_user    = new[axi_txn.burst_length];
            axi_txn.wvalid_delay = new[axi_txn.burst_length];
            foreach (axi_txn.wvalid_delay[i]) begin
                axi_txn.wvalid_delay[i] = i;
            end
            foreach (axi_txn.data[i]) begin
                axi_txn.data[i] = axi_data[i];
            end
            foreach(axi_txn.wstrb[i]) begin
                axi_txn.wstrb[i] = axi_wstrb[i];
            end
        
            `uvm_info("body", "sending AXI WRITE transaction", UVM_LOW);
        end
        else begin

            axi_txn.port_cfg     = cfg;
            axi_txn.xact_type    = svt_axi_transaction::READ;
            axi_txn.addr         = axi_addr;
            axi_txn.burst_type   = svt_axi_transaction::INCR;
            axi_txn.burst_size   = svt_axi_transaction::BURST_SIZE_32BIT;
            axi_txn.atomic_type  = svt_axi_transaction::NORMAL;
            `ifdef SVT_AXI_MAX_BURST_LENGTH_WIDTH_1
            axi_txn.burst_length = 1;
            `elsif SVT_AXI_MAX_BURST_LENGTH_WIDTH_2
            axi_txn.burst_length = 2;
            `elsif SVT_AXI_MAX_BURST_LENGTH_WIDTH_3
            axi_txn.burst_length = 4;
            `elsif SVT_AXI_MAX_BURST_LENGTH_WIDTH_4
            axi_txn.burst_length = 8;
            `else
            axi_txn.burst_length = 16;
            `endif
            axi_txn.rresp        = new[axi_txn.burst_length];
            axi_txn.data         = new[axi_txn.burst_length];
            axi_txn.rready_delay = new[axi_txn.burst_length];
            axi_txn.data_user    = new[axi_txn.burst_length];
            foreach (axi_txn.rready_delay[i]) begin
                axi_txn.rready_delay[i]=i;
            end

            `uvm_info("body", "sending AXI READ transaction", UVM_LOW);
        end
            
        // send the AXI transaction 
        `uvm_send(axi_txn)

        // wait for the read transaction to complete 
        get_response(rsp);

        `uvm_info("body", "AXI transaction completed", UVM_LOW);

        `uvm_info("body", "exiting...", UVM_LOW)
    endtask: body

endclass: fc_axi_txn_seq




