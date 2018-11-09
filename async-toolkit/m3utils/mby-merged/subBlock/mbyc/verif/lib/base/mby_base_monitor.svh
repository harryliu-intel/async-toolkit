//-----------------------------------------------------------------------------
// Title         : Madison Bay Base Monitor Class
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : mby_base_monitor.svh
// Author        : jose.j.godinez.carrillo  <jjgodine@ichips.intel.com>
// Created       : 29.10.2018
//-----------------------------------------------------------------------------
// Description :
// Base monitor class for Madison Bay
//-----------------------------------------------------------------------------
// Copyright (c) 2018 by Intel Corporation This model is the confidential and
// proprietary property of Intel Corporation and the possession or use of this
// file requires a written license from Intel Corporation.
//
// The source code contained or described herein and all documents related to
// the source code ("Material") are owned by Intel Corporation or its suppliers
// or licensors. Title to the Material remains with Intel Corporation or its
// suppliers and licensors. The Material contains trade secrets and proprietary
// and confidential information of Intel or its suppliers and licensors. The
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
//-----------------------------------------------------------------------------
`ifndef __MBY_BASE_PKG__
`error "Attempt to include file outside of mby_igr_env_pkg."
`endif
`ifndef __MBY_BASE_MONITOR__
`define __MBY_BASE_MONITOR__
//-----------------------------------------------------------------------------
// CLASS: mby_base_monitor
//
// This is a parameterized class used by mby_base_agent.
//
// PARAMETERS:
//     T_req - sequence item type to be used (defaults to mby_base_sequence_item)
//     T_vif - virtual interface to be used
//
//-----------------------------------------------------------------------------
class mby_base_monitor
   #(
      type T_req = mby_base_sequence_item,
      type T_vif
   )
   extends uvm_monitor;

   // VARIABLE: mon_ap
   // Analysis Port TODO: can it be a fifo instead?
   uvm_analysis_port #(T_req) mon_ap;

   // VARIABLE: vintf
   // Virtual Interface
   T_vif vintf;

   // VARIABLE: mon_item
   // Monitor sequence item
   T_req mon_item;

   // VARIABLE: cloned_item
   // Cloned sequence item to send out
   T_req cloned_item;

   // Registering class with the factory
   `uvm_component_utils(mby_base_monitor#(T_req, T_vif))

   // -------------------------------------------------------------------------
   // CONSTRUCTOR: new
   //
   // Constructor
   //
   // ARGUMENTS:
   //    string name          - An instance name of the monitor.
   //    uvm_component parent - The monitor's parent component.
   // -------------------------------------------------------------------------
   function new(string name, uvm_component parent);
      super.new(name, parent);
   endfunction : new

   // -------------------------------------------------------------------------
   // FUNCTION: build_phase()
   //
   // The monitor's analysis port is created here.
   //
   // -------------------------------------------------------------------------
   function void build_phase(uvm_phase phase);
      super.build_phase(phase);
      // Create the monitor's analysis port
      mon_ap = new("mon_ap", this);
   endfunction : build_phase

   // -------------------------------------------------------------------------
   // FUNCTION: assign_vif()
   //
   // This function is called by the agent once the monitor is created and the
   // agent has accessed the configuration database to obtain the virtual intf.
   // This is to reduce the number of config db accesses as it has a toll in
   // performance.
   //
   // ARGUMENTS:
   //     T_vif vif - A pointer to the virtual interface to be used by the
   //                 monitor
   //
   // -------------------------------------------------------------------------
   function void assign_vif(T_vif vif);
      this.vintf = vif;
   endfunction

   // -------------------------------------------------------------------------
   // TASK: monitor
   //
   // Main monitor task: monitors the interface for valid activity by calling
   // the virtual interface's 'mon_start' method, creates a new sequence item,
   // gets the values from the interface and populates the sequence item, clones
   // it and sends it out through the analysis port.
   // This is a virtual protected task that can be override in sub-classes.
   //
   // -------------------------------------------------------------------------
   virtual protected task monitor_if();
      forever begin : main_monitor_thread
         // Wait for a new transaction to appear
         vintf.mon_start();

         // Create the seq_item
         `uvm_info("monitor_if()", "Creating a new transaction", UVM_DEBUG);
         mon_item = T_req::type_id::create("mon_item", this);

         // Get actual data pkt and debug info from the interface
         vintf.mon_data(mon_item.data_pkt, mon_item.debug_pkt);
         if (this.get_report_verbosity_level() < UVM_HIGH) begin
            `uvm_info("monitor_if()::got: ", mon_item.convert2string(), UVM_MEDIUM);
         end else begin
            `uvm_info("monitor_if()::got: ", mon_item.sprint(),         UVM_HIGH);
         end

         // Write seq_item to analysis port
         mon_ap.write(mon_item);

         // TODO: send more info on the seq item?
         `uvm_info("monitor_if()", "Sent transaction to the analysis port", UVM_DEBUG);
      end
   endtask : monitor_if

   // -------------------------------------------------------------------------
   // TASK: run_phase
   //
   // Main monitor thread starts: calls the monitor_if() virtual task
   //
   // -------------------------------------------------------------------------
   task run_phase (uvm_phase phase);
      fork
         begin : monitor_thread
            monitor_if();
         end
      join
  endtask : run_phase

endclass : mby_base_monitor

`endif
