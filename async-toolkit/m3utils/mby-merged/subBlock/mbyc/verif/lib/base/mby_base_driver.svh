//-----------------------------------------------------------------------------
// Title         : Madison Bay Base Driver Class
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : mby_base_driver.svh
// Author        : jose.j.godinez.carrillo  <jjgodine@ichips.intel.com>
// Created       : 30.10.2018
// Last modified : 30.10.2018
//-----------------------------------------------------------------------------
// Description :
// Base driver class for Madison Bay
//-----------------------------------------------------------------------------
// Copyright (c) 2018 by Intel Corporation This model is the confidential and
// proprietary property of Intel Corporation and the possession or use of this
// file requires a written license from Intel Corporation.
//------------------------------------------------------------------------------
// Modification history :
// 30.10.2018 : created
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
      type          T_req = mby_base_sequence_item,
      type          T_rsp = T_req,
      type          T_vif
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
      `uvm_field_object(cfg_obj, UVM_ALL_ON)
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
   // Gets the driver's config object and virtual interface
   //
   // -------------------------------------------------------------------------
   function void build_phase(uvm_phase phase);
      super.build_phase(phase);
      // Obtain a handle to the Configuration Object
      if(!uvm_config_db#(mby_base_config)::get(this, "", "cfg_obj", cfg_obj)) begin
         `uvm_fatal("CFG_ERROR", {"Configuration object must be set for: ", get_full_name(), ".cfg_obj"})
      end
      // Obtain the Virtual Interface
      if(!uvm_config_db #(T_vif)::get(this, "", "vintf", vintf)) begin
         `uvm_fatal("VIF_ERROR", {"Virtual interface must be set for: ", get_full_name(), ".vintf"})
      end
   endfunction : build_phase

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
   virtual task pre_drive_cb(T_rsp item);
   endtask : pre_drive_cb

   // -------------------------------------------------------------------------
   // TASK: pst_drive_cb
   //
   // User defined hook, gets called after sequence item is returned from interface
   // -------------------------------------------------------------------------
   virtual task pst_drive_cb(T_rsp item);
   endtask : pst_drive_cb

   // -------------------------------------------------------------------------
   // TASK: drive_data
   //
   // Driver task passing in seq_item fields into interface task
   // -------------------------------------------------------------------------
   task drive_data();
      vintf.drive_data(
         rsp.data_pkt ,
         rsp.debug_pkt,
         rsp.delay
      );
   endtask : drive_data

   // -------------------------------------------------------------------------
   // TASK: run_phase
   //
   // The run phase gets the next sequence item from the sequencer, clones it,
   // calls the pre-drive callback task (which can be defined in a sub-class), 
   // then it drives the sequence item by calling the virtual interface's 
   // drive_data task, calls the post-drive callback (also can be defined in
   // a sub-class), then marks the sequence item done.
   // 
   // -------------------------------------------------------------------------
   task run_phase(uvm_phase phase);
      forever begin
         // Get the next sequence item from sequencer
         seq_item_port.get_next_item(req);
         `uvm_info("gt_uvm_driver::run_phase():: Got seq_item below...", req.sprint(), UVM_NONE)
         // Clone the req item and copy id info
         $cast(rsp, req.clone());
         rsp.set_id_info(req);
         // Call pre-drive methods (may be defined in a sub-class)
         pre_drive_cb(rsp);
         // Call the interface's drive_data task
         drive_data();
         // Call post-drive method (may be defined in a sub-class)
         pst_drive_cb(rsp);
         // Mark sequence item as done
         seq_item_port.item_done();
         // Send the response back
         if (req.rsp_req) begin
            seq_item_port.put(rsp);
         end
      end // forever
   endtask : run_phase

endclass
`endif
