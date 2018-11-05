//-----------------------------------------------------------------------------
// Title         : Madison Bay Base Agent Class
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : mby_base_agent.svh
// Author        : jose.j.godinez.carrillo  <jjgodine@ichips.intel.com>
// Created       : 30.10.2018
//-----------------------------------------------------------------------------
// Description :
// This is the base agent class for Madison Bay.
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
`ifndef __MBY_BASE_AGENT__
`define __MBY_BASE_AGENT__
//-----------------------------------------------------------------------------
// CLASS: mby_base_agent
//
// This is a parameterized class for agent creation in Madison Bay. Uses the
// base drv/mon/seq classes by default, but those can be changed by parameters
// or factory overrides.
//
// PARAMETERS:
//   T_req - request sequence item type to be used (defaults to uvm_sequence_item)
//   T_rsp - response sequence item type to be used (defaults to T_req)
//   T_vif - virtual interface type to use for driving/monitoring
//   T_seq - sequencer class type to be used
//    (defaults to mby_base_sequencer#(.T_req(T_req), .T_rsp(T_rsp)))
//   T_drv - driver class type to be used
//    (defaults to mby_base_driver#(.T_req(T_req), .T_rsp(T_rsp), .T_vif(T_vif)))
//   T_mon - monitor class type to be used
//    (defaults to mby_base_monitor#(.T_req(T_req), .T_vif(T_vif)))
//
//-----------------------------------------------------------------------------
class mby_base_agent
   #(
     type T_req = mby_base_sequence_item,
     type T_rsp = T_req,
     type T_vif,
     type T_seq = mby_base_sequencer#(.T_req(T_req), .T_rsp(T_rsp)),
     type T_drv = mby_base_driver   #(.T_req(T_req), .T_rsp(T_rsp), .T_vif(T_vif)),
     type T_mon = mby_base_monitor  #(.T_req(T_req), .T_vif(T_vif))
   )
   extends uvm_agent;

   // VARIABLE: cfg_obj
   // The agent's configuration object
   mby_base_config cfg_obj;

   // VARIABLE: vintf
   // Virtual Interface pointer
   T_vif vintf;

   // VARIABLE: sequencer
   // Base sequencer instance
   T_seq sequencer;

   // VARIABLE: driver
   // Base driver instance
   T_drv driver;

   // VARIABLE: monitor
   // Base monitor instance
   T_mon monitor;

   // TODO: Add coverage monitor too?

   // -------------------------------------------------------------------------
   // Macro to register new class type
   // -------------------------------------------------------------------------
   `uvm_component_param_utils_begin(mby_base_agent#(T_req, T_rsp, T_vif))
   //`uvm_field_object(cfg_obj, UVM_ALL_ON)
   `uvm_component_utils_end

   // -------------------------------------------------------------------------
   // CONSTRUCTOR: new
   //
   // Constructor
   //
   // ARGUMENTS:
   //    string name          - An instance name of the agent.
   //    uvm_component parent - The agent's parent component pointer.
   // -------------------------------------------------------------------------
   function new(string name, uvm_component parent);
      super.new(name, parent);
   endfunction : new

   // ------------------------------------------------------------------------
   // FUNCTION: build_phase
   //
   // Gets the agent's configuration object and vif from the config db.
   // Creates monitor, sequencer and driver as specified in configuration object
   //
   // ------------------------------------------------------------------------
   function void build_phase(uvm_phase phase);

      // ----------------------------------------------------------------------
      // Get the configuration object and virtual interface
      // ----------------------------------------------------------------------
      // TODO: accesses to config db has performance issues, consider adding the
      // vif to the config object and then pass it over to the driver/mon
      //
      if(!uvm_config_db #(mby_base_config)::get(this, "", "cfg_obj", cfg_obj)) begin
         `uvm_fatal("CFG_ERROR", {"Config object must be set for: ", get_full_name(), ".cfg_obj"})
      end
      if(cfg_obj.driver_active == UVM_ACTIVE || cfg_obj.monitor_active == UVM_ACTIVE) begin
         if(!uvm_config_db #(T_vif)::get(this, "", "vintf", vintf)) begin
            `uvm_fatal("VIF_ERROR", {"Virtual interface must be set for: ",get_full_name(),".vintf"})
         end
      end

      // ----------------------------------------------------------------------
      // Create the monitor
      // ----------------------------------------------------------------------
      if(cfg_obj.monitor_active == UVM_ACTIVE) begin
         monitor = T_mon::type_id::create("monitor", this);
         monitor.assign_vif(vintf);
      end

      // ----------------------------------------------------------------------
      // Create the Driver & Sequencer
      // ----------------------------------------------------------------------
      if(cfg_obj.driver_active == UVM_ACTIVE) begin
         sequencer = T_seq::type_id::create("sequencer", this);
         driver    = T_drv::type_id::create("driver", this);
         driver.assign_vif(vintf);
         driver.assign_cfg(cfg_obj);
      end

   endfunction : build_phase

   // ------------------------------------------------------------------------
   // FUNCTION: connect_phase
   //
   // Connects driver with sequencer
   //
   // ------------------------------------------------------------------------
   function void connect_phase(uvm_phase phase);
      if(cfg_obj.driver_active == UVM_ACTIVE) begin
         // Connect the Driver seq_item_port to the sequencer seq_item_export
         driver.seq_item_port.connect(sequencer.seq_item_export);
      end
    endfunction : connect_phase

endclass : mby_base_agent

`endif
