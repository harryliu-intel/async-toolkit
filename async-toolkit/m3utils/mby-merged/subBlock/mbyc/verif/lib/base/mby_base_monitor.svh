//-----------------------------------------------------------------------------
// Title         : Madison Bay Base Monitor Class
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : mby_base_monitor.svh
// Author        : jose.j.godinez.carrillo  <jjgodine@ichips.intel.com>
// Created       : 29.10.2018
// Last modified : 29.10.2018
//-----------------------------------------------------------------------------
// Description :
// Base monitor class for Madison Bay
//-----------------------------------------------------------------------------
// Copyright (c) 2018 by Intel Corporation This model is the confidential and
// proprietary property of Intel Corporation and the possession or use of this
// file requires a written license from Intel Corporation.
//------------------------------------------------------------------------------
// Modification history :
// 29.10.2018 : created
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
   // Analysis Port
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
   // The vintf is obtained and analysis port is created
   //
   // -------------------------------------------------------------------------
   function void build_phase(uvm_phase phase);
      super.build_phase(phase);
      // Create the monitor's analysis port
      mon_ap = new("mon_ap", this);
      // Get virtual interface from config db
      if(!uvm_config_db #(T_vif)::get(this, "", "vintf", vintf)) begin
         `uvm_fatal("VIF_ERROR", { "Virtual interface (vintf) must be set for: ", get_full_name() })
      end
   endfunction : build_phase

   // -------------------------------------------------------------------------
   // TASK: run_phase
   //
   // Main monitor thread: monitors the interface for valid activity by calling
   // the virtual interface's 'mon_start' method, creates a new sequence item,
   // gets the values from the interface and populates the sequence item, clones
   // it and sends it out through the analysis port.
   //
   // -------------------------------------------------------------------------
   task run_phase (uvm_phase phase);
      string msg;
      forever begin
         // Wait for a new transaction to appear
         vintf.mon_start();
         `uvm_info(get_full_name(), "Creating a new transaction", UVM_DEBUG);

         // Create the seq_item
         mon_item = T_req::type_id::create("mon_item", this);

         // Get actual data pkt and debug info from the interface and print debug msgs
         vintf.mon_data(mon_item.data_pkt, mon_item.debug_pkt);
         $sformat(msg, "Captured the following data_pkt %h and debug_pkt = %h)",
                  mon_item.data_pkt, mon_item.debug_pkt);
         `uvm_info(get_full_name(), msg,               UVM_MEDIUM);
         `uvm_info(get_full_name(), mon_item.sprint(), UVM_HIGH);

         // Write seq_item to analysis port
         $cast(cloned_item, mon_item.clone());
         mon_ap.write(cloned_item);
         `uvm_info(get_full_name(), "Sent transaction to the analysis port", UVM_DEBUG);
      end
  endtask : run_phase

endclass : mby_base_monitor

`endif
