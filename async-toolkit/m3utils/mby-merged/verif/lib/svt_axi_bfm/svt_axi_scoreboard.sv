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


//Class:    axi_uvm_scoreboard

//This file defines a scoreboard class that axi__env uses for
//comparing the data coming out of the analysis ports of the Master & Slave
//agents. This class defines the following:
//
//The scoreboard contains two analysis ports - item_observed_initiated_export
//and item_observed_response_export. item_observed_initiated_export is
//connected to the master agent's analysis port, and
//item_observed_response_export is connected to slave agent's analysis port.
//The connection is done in the axi_intermediate_env.  The analysis ports call
//write_response and write_initiated tasks.  write_response and write_initiated
//tasks store the transactions in the response_xact_list and
//initiated_xact_list respectively.
//
//At the negedge of the clock the list are sorted. The sorting is required
//because READ and WRITE transactions may complete at the same time. After
//sorting, the transactions are compared via  compare_xact() task and then the
//queue is flushed via flush_queue() task.
//

`ifndef __SVT_AXI_SCOREBOARD_GUARD
`define __SVT_AXI_SCOREBOARD_GUARD


`ifndef __INSIDE_SVT_AXI_BFM_PKG__
`error "File is meant to be used only through the svt_axi_bfm_pkg.  Do not include it individually."
`endif

/** Macro that define two analysis ports with unique suffixes */
`uvm_analysis_imp_decl(_initiated)
`uvm_analysis_imp_decl(_response)

class axi_uvm_scoreboard extends uvm_scoreboard;

    /** Analysis port connected to the AXI Master Agent */
    uvm_analysis_imp_initiated#(svt_axi_transaction, axi_uvm_scoreboard) item_observed_initiated_export;

    /** Analysis port conneted to the AXI Slave Agent */
    uvm_analysis_imp_response#(svt_axi_transaction, axi_uvm_scoreboard) item_observed_response_export;

    /** Queue to store the write request from the Master */
    svt_axi_transaction initiated_xact_queue[$];

    /** Queue to store the write response from the Slave */
    svt_axi_transaction response_xact_queue[$];

    /** Event used to indicate that there are finished transactions which were not processed by compare_xact() */
    event non_empty_xact_queue;

    /** Customized UVM comparer policy class */
    uvm_comparer axi_comparer;
    
    virtual axi_reset_if axi_reset_if;

    /** UVM Component Utility macro */
    `uvm_component_utils(axi_uvm_scoreboard)

    /** Class Constructor */
    function new (string name = "axi_uvm_scoreboard", uvm_component parent=null);
        super.new(name, parent);
    endfunction : new


    /** Flush the transaction queues after the compare */
    virtual function void flush_queue();
        initiated_xact_queue.delete();
        response_xact_queue.delete();
    endfunction

    /**
     * Build Phase
     * - Construct the analysis ports
     * - Customize the compare policy
     */
    virtual function void build_phase(uvm_phase phase);
        `uvm_info("build_phase", "Entered...", UVM_LOW)

        super.build_phase(phase);

        /** Construct the analysis ports */
        item_observed_initiated_export = new("item_observed_initiated_export", this);
        item_observed_response_export = new("item_observed_response_export", this);

        /** Customize the compare policy */
        axi_comparer = new;
        axi_comparer.show_max = 100;
        axi_comparer.check_type = 0;
        axi_comparer.sev = UVM_ERROR;
        axi_comparer.policy = UVM_DEEP;
        axi_comparer.abstract = 0;
        axi_comparer.physical = 1;

        `uvm_info("build_phase", "Exiting...", UVM_LOW)
    endfunction

    /**
     * Run Phase
     * - Compares the received transactions ad the negedge of the clock
     */
    virtual task run_phase(uvm_phase phase);
        `uvm_info("run_phase", "Entered...", UVM_LOW)

        forever begin

            // To avoid waking up at every clock, watch for some indication that there is
            // something to wake up for
            if (initiated_xact_queue.size() == 0 || response_xact_queue.size() == 0)
                @(non_empty_xact_queue);

            // Compare the lists at negedge of the SystemClock
            @(negedge axi_reset_if.clk);
            compare_xact();
        end

        `uvm_info("run_phase", "Exiting...", UVM_LOW)
    endtask


    /** This method is called by item_observed_initiated_export */
    virtual function void write_initiated(input svt_axi_transaction xact);
        svt_axi_transaction init_xact;

        if (!$cast(init_xact, xact.clone())) begin
            `uvm_fatal("write_initiated", "Unable to $cast the received transaction to svt_axi_transaction");
        end

        `uvm_info("write_initiated", $sformatf("xact:\n%s", init_xact.sprint()), UVM_FULL)
        initiated_xact_queue.push_back(init_xact);
        -> non_empty_xact_queue;
    endfunction

    /** This method is called by item_observed_response_export */
    virtual function void write_response(input svt_axi_transaction xact);
        svt_axi_transaction  resp_xact;

        if (!$cast(resp_xact, xact.clone())) begin
            `uvm_fatal("write_response", "Unable to $cast the received transaction to svt_axi_transaction");
        end

        `uvm_info("write_response", $sformatf("xact:\n%s", resp_xact.sprint()), UVM_FULL)
        response_xact_queue.push_back(resp_xact);
        -> non_empty_xact_queue;
    endfunction

    /** Compares the elements from the queue */
    virtual function void compare_xact();
        bit val;

        /** Sort the transactions initiated comparing */
        `uvm_info("compare_xact_sort", "Sorting both the lists with object_id", UVM_FULL);
        initiated_xact_queue.sort() with (item.object_id);
        response_xact_queue.sort() with (item.object_id);

        if (initiated_xact_queue.size() != response_xact_queue.size()) begin
            `uvm_error("compare_xact", $sformatf("Size Mismatch. initiated_xact_queue size = %0d , response_xact_queue size = %0d",initiated_xact_queue.size(),response_xact_queue.size()));
        end
        else begin

            /** Compare each transaction and issue error on mis-match */
            foreach (initiated_xact_queue[i]) begin
                `uvm_info("compare_xact", $sformatf("Comparing transactions with object_id %0d  - %0d\n", initiated_xact_queue[i].object_id, response_xact_queue[i].object_id), UVM_LOW);
                if (initiated_xact_queue[i].compare(response_xact_queue[i], axi_comparer)) begin
                    `uvm_info("compare_xact_local", " Comparator Match", UVM_LOW);
                end
                else begin
                    `uvm_error("compare_xact_local", "comparator mismatch");
                end
            end
        end

        flush_queue();
    endfunction

endclass: axi_uvm_scoreboard

`endif // __SVT_AXI_SCOREBOARD_GUARD

