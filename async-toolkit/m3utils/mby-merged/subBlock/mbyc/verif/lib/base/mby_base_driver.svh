//-----------------------------------------------------------------------------
// Title         : Madison Bay Base Driver Class
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : mby_base_driver.svh
// Author        : jose.j.godinez.carrillo  <jjgodine@ichips.intel.com>
// Created       : 30.10.2018
//-----------------------------------------------------------------------------
// Description :
// Base driver class for Madison Bay
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
`ifndef __MBY_BASE_DRIVER__
`define __MBY_BASE_DRIVER__
//-----------------------------------------------------------------------------
// CLASS: mby_base_driver
//
// This is a parameterized class used by mby_base_agent.
//
// PARAMETERS:
//     T_seq - sequence item type to be used (defaults to uvm_sequence_item)
//     T_vif - virtual interface to be used
//
//-----------------------------------------------------------------------------
class mby_base_driver
   #(
      type T_req = mby_base_sequence_item,
      type T_rsp = T_req,
      type T_vif
   )
   extends uvm_driver
   #(
      T_req,
      T_rsp
   );

   // VARIABLE: cfg_obj
   // The agent's configuration object
   mby_base_config cfg_obj;

   // VARIABLE: vintf
   // Virtual Interface
   T_vif vintf;

   // -------------------------------------------------------------------------
   // Macro to register new class type
   // -------------------------------------------------------------------------
   `uvm_component_param_utils_begin(mby_base_driver#(T_req, T_rsp, T_vif))
   `uvm_component_utils_end

   // -------------------------------------------------------------------------
   // CONSTRUCTOR: new
   //
   // Constructor
   //
   // ARGUMENTS:
   //    string name          - An instance name of the driver.
   //    uvm_component parent - The driver's parent component pointer.
   // -------------------------------------------------------------------------
   function new(string name, uvm_component parent);
      super.new(name, parent);
   endfunction : new

   // -------------------------------------------------------------------------
   // FUNCTION: build_phase()
   //
   //
   // -------------------------------------------------------------------------
   function void build_phase(uvm_phase phase);
      super.build_phase(phase);
   endfunction : build_phase

   // -------------------------------------------------------------------------
   // FUNCTION: assign_vif()
   //
   // This function is called by the agent once the driver is created and the
   // agent has accessed the configuration database to obtain the virtual intf.
   // This is to reduce the number of config db accesses as it has a toll in
   // performance.
   //
   // ARGUMENTS:
   //     T_vif vif - A pointer to the virtual interface to be used by the
   //                 driver
   //
   // -------------------------------------------------------------------------
   function void assign_vif(T_vif vif);
      this.vintf = vif;
   endfunction

   // -------------------------------------------------------------------------
   // FUNCTION: assign_cfg()
   //
   // This function is called by the agent once the driver is created and the
   // agent has accessed the configuration database to obtain the config obj.
   // This is to reduce the number of config db accesses as it has a toll in
   // performance.
   //
   // ARGUMENTS:
   //     mby_base_config cfg - A pointer to the config obj to be used by the
   //                 driver
   //
   // -------------------------------------------------------------------------
   function void assign_cfg(mby_base_config cfg);
      this.cfg_obj = cfg;
   endfunction

   // -------------------------------------------------------------------------
   // FUNCTION: connect_phase()
   //
   //
   // -------------------------------------------------------------------------
   function void connect_phase(uvm_phase phase);
      super.connect_phase(phase);
   endfunction : connect_phase

   // -------------------------------------------------------------------------
   // TASK: pre_drive_cb
   //
   // User defined hook, gets called before passing sequence item to interface
   // -------------------------------------------------------------------------
   virtual protected task pre_drive_cb(T_rsp item);
   endtask : pre_drive_cb

   // -------------------------------------------------------------------------
   // TASK: post_drive_cb
   //
   // User defined hook, gets called after sequence item is returned from interface
   // -------------------------------------------------------------------------
   virtual protected task post_drive_cb(T_rsp item);
   endtask : post_drive_cb

   // -------------------------------------------------------------------------
   // TASK: drive_data
   //
   // Driver task passing in seq_item fields into interface task
   // -------------------------------------------------------------------------
   virtual protected task drive_data();
      vintf.drive_data(
         rsp.data_pkt ,
         rsp.debug_pkt,
         rsp.delay
      );
   endtask : drive_data

   // -------------------------------------------------------------------------
   // TASK: get_and_drive
   //
   // Main driver task thread gets the next sequence item from the sequencer,
   // clones it, calls the pre-drive callback task (which can be defined in a
   // sub-class), then it drives the sequence item by calling the virtual
   // interface's drive_data task, calls the post-drive callback (also can be
   // defined in a sub-class), then marks the sequence item done.
   //
   // -------------------------------------------------------------------------
   virtual protected task get_and_drive();
      forever begin : main_drive_thread
         // Get the next sequence item from sequencer
         seq_item_port.get_next_item(req);
         if (this.get_report_verbosity_level() < UVM_HIGH) begin
            `uvm_info("get_and_drive()::starts driving ",          req.convert2string(), UVM_MEDIUM)
         end else begin
            `uvm_info("get_and_drive()::starts driving ",          req.sprint(),         UVM_HIGH)
         end

         // Clone the req item and copy id info
         $cast(rsp, req.clone());
         rsp.set_id_info(req);

         // Call pre-drive methods (may be defined in a sub-class)
         `uvm_info("get_and_drive()::calling pre_drive callback",  rsp.convert2string(), UVM_DEBUG)
         pre_drive_cb(rsp);

         // Call the interface's drive_data task
         `uvm_info("get_and_drive()::calling drive_data",          rsp.convert2string(), UVM_DEBUG)
         drive_data();

         // Call post-drive method (may be defined in a sub-class)
         `uvm_info("get_and_drive()::calling post_drive callback", rsp.convert2string(), UVM_DEBUG)
         post_drive_cb(rsp);

         // Mark sequence item as done
         `uvm_info("get_and_drive()::setting item done",           rsp.convert2string(), UVM_DEBUG)
         seq_item_port.item_done();

         // Send the response back
         if (req.rsp_req || cfg_obj.rsp_req) begin
            `uvm_info("get_and_drive()::sending back a response",  rsp.convert2string(), UVM_DEBUG)
            seq_item_port.put(rsp);
         end

         // Print final debug message
         `uvm_info("get_and_drive()::exiting!",                    rsp.convert2string(), UVM_DEBUG)
      end // main_drive_thread
   endtask : get_and_drive

   // -------------------------------------------------------------------------
   // TASK: run_phase
   //
   // The run phase
   //
   // -------------------------------------------------------------------------
   task run_phase(uvm_phase phase);
      fork
         begin : drive_thread
            get_and_drive();
         end
      join
   endtask : run_phase

endclass
`endif
