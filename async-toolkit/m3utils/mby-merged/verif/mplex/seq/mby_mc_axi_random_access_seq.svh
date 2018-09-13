
// vim: noai : ts=3 : sw=3 : expandtab : ft=systemverilog

//------------------------------------------------------------------------------
//
// INTEL CONFIDENTIAL
//
// Copyright 2018 Intel Corporation All Rights Reserved.
//
// The source code contained or described herein and all documents related to
// the source code ("Material") are owned by Intel Corporation or its suppliers
// or licensors. Title to the Material remains with Intel Corporation or its
// suppliers and licensors. The Material contains trade secrets and proprietary
// and confidential information of Intel or its suppliers and licensors.  The
// Material is protected by worldwide copyright and trade secret laws and
// treaty provisions. No part of the Material may be used, copied, reproduced,
// modified, published, uploaded, posted, transmitted, distributed, or
// disclosed in any way without Intel's prior express written permission.
//
// No license under any patent, copyright, trade secret or other intellectual
// property right is granted to or conferred upon you by disclosure or delivery
// of the Materials, either expressly, by implication, inducement, estoppel or
// otherwise. Any license under such intellectual property rights must be
// express and approved by Intel in writing.
//
//=======================================================================
// COPYRIGHT (C)  2012 SYNOPSYS INC.
// This software and the associated documentation are confidential and
// proprietary to Synopsys, Inc. Your use or disclosure of this software
// is subject to the terms and conditions of a written license agreement
// between you, or your company, and Synopsys, Inc. In the event of
// publications, the following notice is applicable:
//
// ALL RIGHTS RESERVED
//
// The entire notice above must be reproduced on all authorized copies.
//
//------------------------------------------------------------------------------
//   Author        : Akshay Kotian
//   Project       : Madison Bay
//------------------------------------------------------------------------------

//Class:    mby_mc_axi_random_access_seq
//
//This sequence will perform random AXI access to all Peripheral device registers
//and mailbox registers.
//


`ifndef __AXI_RANDOM_ACCESS_SEQ_GUARD
`define __AXI_RANDOM_ACCESS_SEQ_GUARD


class mby_mc_axi_random_access_seq extends svt_axi_bfm_pkg::axi_master_directed_sequence;

    // Parameter that controls the number of transactions that will be generated
    rand int unsigned sequence_length = 10;

    //Constrain the sequence length to a reasonable value
    constraint reasonable_sequence_length {
        sequence_length <= 100;
    }

    //UVM Object Utility macro
    `uvm_object_utils(mby_mc_axi_random_access_seq)

    // ------------------------------------------------------------------------
    //  Constructor: new
    //  Arguments:
    //  string name   - Seq object name.
    // ------------------------------------------------------------------------
    function new(string name="mby_mc_axi_random_access_seq");
        super.new(name);
    endfunction

    //------------------------------------------------------------------------------
    //  Task: body
    //  Initiate random AXI accesses.
    //------------------------------------------------------------------------------
    virtual task body();
        bit status;

        `uvm_info("body", "Started running mby_mc_axi_random_access_seq..", UVM_LOW)

        //super.body();

        // Obtain a handle to the port configuration
        get_port_cfg();


        //Read default values from one CSR from each Peripheral device
        for(int i = 0; i < sequence_length; i++) begin
            axi_read(.addr(32'h0400_0000 | ('h400 * i)), .burst_len(1));
        end

        //Perform writes to those CSRs.
        for(int i = 0; i < sequence_length; i++) begin
            axi_write( .addr(32'h0400_0000 | ('h400 * i)), .burst_len(1), .wr_data('1)  );
        end

        //Read back the modified values.
        for(int i = 0; i < sequence_length; i++) begin
            axi_read(.addr(32'h0400_0000 | ('h400 * i)), .burst_len(1));
        end

        `uvm_info("body", "Exiting...", UVM_LOW)

    endtask: body

    //------------------------------------------------------------------------------
    //  Task: axi_write
    //  Initiates a AXI write request.
    //
    //  Arguments:
    //  int addr     : AXI Write address.
    //  int burst_len: burst length of AXI write.
    //  bit [63:0] wr_data: Write data.
    //------------------------------------------------------------------------------
    task axi_write(int addr, int burst_len , bit [63:0] wr_data);

        svt_axi_bfm_pkg::cust_axi_master_transaction write_tran;

        // Set up the write transaction
        `uvm_create(write_tran)
        write_tran.port_cfg     = cfg;
        write_tran.xact_type    = svt_axi_bfm_pkg::cust_axi_master_transaction::WRITE;
        write_tran.addr         = addr;
        write_tran.burst_type   = svt_axi_bfm_pkg::cust_axi_master_transaction::INCR;
        write_tran.burst_size   = svt_axi_bfm_pkg::cust_axi_master_transaction::BURST_SIZE_64BIT;
        write_tran.atomic_type  = svt_axi_bfm_pkg::cust_axi_master_transaction::NORMAL;
        write_tran.burst_length = burst_len;                 
        write_tran.data         = new[write_tran.burst_length];
        write_tran.wstrb        = new[write_tran.burst_length];
        write_tran.data_user    = new[write_tran.burst_length];
        foreach (write_tran.data[i]) begin
            write_tran.data[i] = wr_data;
        end
        foreach(write_tran.wstrb[i]) begin
            write_tran.wstrb[i] = 8'hff;
        end
        write_tran.wvalid_delay = new[write_tran.burst_length];
        foreach (write_tran.wvalid_delay[i]) begin
            write_tran.wvalid_delay[i]=i;
        end

        // Send the write transaction
        `uvm_send(write_tran)

        // Wait for the write transaction to complete
        get_response(rsp);

    endtask

    //------------------------------------------------------------------------------
    //  Task: axi_read
    //  Initiates a AXI Read request.
    //
    //  Arguments:
    //  int addr     : AXI Write address.
    //  int burst_len: burst length of AXI read.
    //------------------------------------------------------------------------------
    task axi_read(int addr, int burst_len);

        svt_axi_bfm_pkg::cust_axi_master_transaction read_tran;

        // Set up the read transaction
        `uvm_create(read_tran)
        read_tran.port_cfg     = cfg;
        read_tran.xact_type    = svt_axi_bfm_pkg::cust_axi_master_transaction::READ;
        read_tran.addr         = addr;  
        read_tran.burst_type   = svt_axi_bfm_pkg::cust_axi_master_transaction::INCR;
        read_tran.burst_size   = svt_axi_bfm_pkg::cust_axi_master_transaction::BURST_SIZE_64BIT; //BURST_SIZE_32BIT;
        read_tran.atomic_type  = svt_axi_bfm_pkg::cust_axi_master_transaction::NORMAL;
        read_tran.burst_length = burst_len;   
        read_tran.rresp        = new[read_tran.burst_length];
        read_tran.data         = new[read_tran.burst_length];
        read_tran.rready_delay = new[read_tran.burst_length];
        read_tran.data_user    = new[read_tran.burst_length];
        foreach (read_tran.rready_delay[i]) begin
            read_tran.rready_delay[i]=i;
        end

        // Send the read transaction
        `uvm_send(read_tran)

        // Wait for the read transaction to complete
        get_response(rsp);

    endtask

endclass: mby_mc_axi_random_access_seq

`endif // __AXI_RANDOM_ACCESS_SEQ_GUARD


