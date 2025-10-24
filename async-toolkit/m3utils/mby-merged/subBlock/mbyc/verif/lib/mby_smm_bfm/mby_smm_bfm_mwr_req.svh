//-----------------------------------------------------------------------------
// Title         : Madison Bay SMM BFM - Memory Mesh 
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : mby_smm_bfm_mwr_req.svh
// Author        : Roman Bernal <r.bernal@intel.com>
// Created       : 01.11.2018
//-----------------------------------------------------------------------------
// Description :
// This is the Address Translation class
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
//------------------------------------------------------------------------------
`ifndef __MBY_SMM_BFM_PKG__
`error "Attempt to include file outside of mby_smm_bfm_pkg."
`endif
`ifndef __MBY_SMM_BFM_MWR_REQ__
`define __MBY_SMM_BFM_MWR_REQ__
//-----------------------------------------------------------------------------
// CLASS: mby_smm_bfm_mwr_req
//
// This class is instantiated in the smm bfm. This class in turn instantiates
// an array of mem node classes (mby_smm_bfm_mem_node) to build the Memory Mesh. 
// It consumes memory requests from the different interfaces and execute Data
// MemWrs/MemRds to a given Mem Node in the Mesh and location in the Memory Node
// based on Row, Col, Address coming in Request.
//
// PARAMETERS::
//     type T_req - sequence item type to be handled.
//-----------------------------------------------------------------------------

class mby_smm_bfm_mwr_req
   #(
      type T_req  = mby_base_sequence_item
   )
   extends uvm_pkg::uvm_subscriber #(T_req);

   // VARIABLE: cfg_obj
   //    The BFM's mwr request configuration object
   mby_smm_bfm_cfg cfg_obj;

   // VARIABLE: wr_req_agent_prt
   //    Pointer to the SMM BFM write request agent.
   smm_bfm_wr_req_agent wr_req_agent_ptr;

   // VARIABLE: mesh_ptr
   //    Pointer to the SMM BFM nodes array.
   smm_bfm_mem_node  mesh_ptr[MAX_NUM_MSH_ROWS-1:0][MAX_NUM_MSH_COLS-1:0];
   
   // VARIABLE: operation_mode
   //    Defines the behavior of this subscriber in order to operate with other RTL/BFM components.
   operation_mode_e operation_mode;
   
   // VARIABLE: delay_simulation
   //    Defines whether a delay should be simulated for memory read/write operations. If set a delay will be added
   //    for each operation, otherwise all transactions will take place on 0 simulation time.
   delay_simulation_e delay_simulation;
   
   // VARIABLE: mwr_req_pending
   //    Counter of the current memory write requests that are pending to be delivered to the mesh.
   int mwr_req_pending;

   // Registering class with the factory
   `uvm_component_utils(mby_smm_bfm_mwr_req#(T_req))

   // -------------------------------------------------------------------------
   // CONSTRUCTOR: new
   //
   // Constructor
   //
   // ARGUMENTS:
   //    string name          - An instance name of the address translator.
   //    uvm_component parent - The translator's parent component.
   // -------------------------------------------------------------------------
   function new(string name, uvm_component parent);
      super.new(name, parent);
   endfunction : new

   // -------------------------------------------------------------------------
   // FUNCTION: set_mesh_ptr
   //
   // Assigns the internal mesh pointer to be the same as the input argument.
   //
   // ARGUMENTS:
   //    smm_bfm_mem_node mesh_ptr [MAX_NUM_MSH_ROWS-1:0][MAX_NUM_MSH_COLS-1:0]  - An instance name of the address translator.
   // -------------------------------------------------------------------------
   function void set_mesh_ptr(smm_bfm_mem_node  mesh_ptr[MAX_NUM_MSH_ROWS-1:0][MAX_NUM_MSH_COLS-1:0]);
      this.mesh_ptr = mesh_ptr;
   endfunction : set_mesh_ptr

   // -------------------------------------------------------------------------
   // FUNCTION: set_agent_ptr
   //
   // Assigns the internal agent pointer to be the same as the input argument.
   //
   // ARGUMENTS:
   //    smm_bfm_wr_req_agent wr_req_agent_ptr  - An instance name of the address translator.
   // -------------------------------------------------------------------------
   function void set_agent_ptr(smm_bfm_wr_req_agent wr_req_agent_ptr);
      this.wr_req_agent_ptr = wr_req_agent_ptr;
   endfunction : set_agent_ptr

   // -------------------------------------------------------------------------
   // FUNCTION: set_operation_opts
   //
   // Sets operation options for this subscriber.
   //
   // Operation modes
   //    It defines how the subscriber should behave depending on the component that will be connected to. which can
   //    be either Ingress or Global Pointer Manager (GPM). Mode of operation helps to differentiate the subscriber
   //    instance and handle the requirements specific to Ingress or GPM.
   //
   //    INGRESS_MODE: The subscriber gets connected to one of the six Ingress-to-Mesh interfaces, and sets its
   //       behavior to get and process memory write requests from Ingress.
   //    GPM_MODE: The subscriber gets connected to a GPM port, and sets its behavior to get, process and respond 
   //       memory write requests from GPM.
   //    Note: EGRESS_MODE doesn't apply to this subscriber as Egress doesn't issue memory writes.
   //
   // Delay simulation:
   //    If set, then the subscriber will hold the memory write request for a random number of cycles in order to
   //    simulate the time required to  drive and write the data to to the mesh.
   //
   //    WITH_DELAY_SIMULATION: The memory write request gets hold for a random number of cycles before writing the
   //       data into the mesh. It will take some cycles until that data gets visible to all other components.
   //    WITHOUT_DELAY_SIMULATION: The memory write request gets reflected immediately in the mesh , taking it 0
   //       simulation time to complete.
   //
   // ARGUMENTS:
   //    operation_mode_e operation_mode     -  Defines the subscriber behavior.
   //    delay_simulation_e delay_simulation -  Indicates whether delay simulation takes place.
   // -------------------------------------------------------------------------
   function void set_operation_opts(operation_mode_e operation_mode = INGRESS_MODE, delay_simulation_e delay_simulation = WITHOUT_DELAY_SIMULATION);
      if (operation_mode inside {EGRESS_MODE}) begin
         `uvm_fatal(get_type_name(), "set_operation_opts(): %s is not supported for mwr_req subscriber.")
      end
      
      this.operation_mode     = operation_mode;
      this.delay_simulation   = delay_simulation;
   endfunction : set_operation_opts

   // ------------------------------------------------------------------------
   // FUNCTION: build_phase
   //
   // Gets the agent's configuration object and vif from the config db.
   // Creates monitor, sequencer and driver as specified in configuration object
   //
   // ARGUMENTS:
   //    uvm_phase phase - phase object.
   // ------------------------------------------------------------------------
   function void build_phase(uvm_phase phase);
      super.build_phase(phase);
      
      mwr_req_pending   = 0;
   endfunction : build_phase

   // -------------------------------------------------------------------------
   //
   // TODO : This is an empty shell for the address translation block
   //        - add analysis port declarations, build, connect and run phases.
   //
   // -------------------------------------------------------------------------

   // ------------------------------------------------------------------------
   // FUNCTION: write
   //
   // Method that must be defined in each uvm_subscriber subclass. Access
   // to this method by outside components should be done via the
   // analysis_export.
   //
   // ARGUMENTS:
   //    T_req ap_item - memory write request issued to the SMM BFM.
   // ------------------------------------------------------------------------
   function void write(T_req ap_item);
      start_mwr_req_rsp_task(ap_item);
   endfunction

   // ------------------------------------------------------------------------
   // FUNCTION: start_mwr_req_rsp_task
   //
   // Sets up an separate context for the received memory write request and
   // start the corresponding write request task depending on delay_simulation
   // specified.
   //
   // ARGUMENTS:
   //    T_req mem_req - contains a memory write request to the SMM BFM.
   // ------------------------------------------------------------------------
   function void start_mwr_req_rsp_task(T_req mem_req);
      fork
         mwr_req(mem_req);
      join_none
   endfunction

   // ------------------------------------------------------------------------
   // TASK: mwr_req
   //
   // Receives a memory write request from Ingress or Global Pointer Manager, and stores the data into the SMM BFM
   // memory. Write request delay can take place depending on the configuration set at set_operation_opts().
   //
   // ARGUMENTS:
   //    T_req mem_req - memory write request issued to the SMM BFM.
   // ------------------------------------------------------------------------
   virtual protected task automatic mwr_req(T_req mem_req);
      bit [SMM_BFM_ADDR_WIDTH-1:0]     addr;     // Memory write address
      bit [SMM_BFM_DATA_WIDTH-1:0]     wr_data;  // Memory write data
      bit [SMM_BFM_NUM_MSH_ROWS-1:0]   node_row; // SMM BFM node column
      bit [SMM_BFM_NUM_MSH_COLS-1:0]   node_col; // SMM BFM node row
      
      int unsigned   mwr_req_delay;    // Modeled write request delay
      
      string msg_str;
      string mwr_req_str;
      
      // Decode memory write request
      addr     = mem_req.data.mim_wr_seg_ptr[13:0];
      node_row = mem_req.data.mim_wr_seg_ptr[17:14];
      node_col = {mem_req.data.mim_wr_seg_ptr[1:0]^mem_req.data.mim_wr_wd_sel,mem_req.data.mim_wr_seg_ptr[18]};
      wr_data  = mem_req.data.mim_wr_data;
      
      mwr_req_str = $sformatf("NodeRow = 0x%0x, NodeCol = 0x%0x, Address = 0x%04x", node_row, node_col, addr);
      
      msg_str  = $sformatf("mwr_req(): %s Received a memory write request for ", operation_mode.name());
      msg_str  = {msg_str, $sformatf("%s WrData = 0x%0128x", mwr_req_str, wr_data)};
      
      `uvm_info(get_type_name(), msg_str, UVM_MEDIUM)
      
      if (delay_simulation inside {WITH_DELAY_SIMULATION}) begin
         // Increase the number of pending write requests so the main thread can raise an objection to let
         // this task finish properly
         mwr_req_pending++;
         
         // Generate profile based latencies, min and max got adjusted based on the configured profile.
         mwr_req_delay = $urandom_range(cfg_obj.mwr_req_delay_min, cfg_obj.mwr_req_delay_max);
         
         msg_str = $sformatf("mwr_req(): %s Delay of write request is ", operation_mode.name());
         msg_str = {msg_str, $sformatf("%0d clocks. %s", mwr_req_delay, mwr_req_str)};
         
         `uvm_info(get_type_name(), msg_str,  UVM_MEDIUM)
         
         // Simulate read request/response latency
         repeat(mwr_req_delay) @(posedge this.wr_req_agent_ptr.driver.io_policy.vintf.clk);
      end
      
      mesh_ptr[node_row][node_col].mwr(addr,wr_data);
      
      msg_str  = $sformatf("mwr_req(): %s Memory write request done. ", operation_mode.name());
      msg_str  = {msg_str, mwr_req_str};
      
      `uvm_info(get_type_name(), msg_str, UVM_MEDIUM)
      
      if (delay_simulation inside {WITH_DELAY_SIMULATION}) begin
         // Decrease the number of pending read requests, so the main thread can drop the objection if there aren't
         // more pending read requests
         mwr_req_pending--;
      end
   endtask

   // -------------------------------------------------------------------------
   // TASK: run_phase
   //
   // Main monitor thread starts: calls the monitor_if() virtual task
   //
   // ARGUMENTS:
   //    uvm_phase phase - phase object.
   // -------------------------------------------------------------------------
   task run_phase (uvm_phase phase);
      bit      objection_raised = 0;
      string   msg_str;
      
      forever @ (posedge this.wr_req_agent_ptr.driver.io_policy.vintf.clk) begin
         // Maintain track of the memory write requests in flight, will hold on test
         // completion until all write requests are completed.
         if (!objection_raised && mwr_req_pending > 0) begin
            phase.raise_objection(this, "SMM BFM: Pending memory read requests.", 1);
            
            msg_str = $sformatf("run_phase(): %s. ", operation_mode.name());
            msg_str = {msg_str, $sformatf("%0d pending memory write requests, raised phase objection.",
               mwr_req_pending)};
            
            `uvm_info(get_type_name(), msg_str, UVM_DEBUG)
            
            objection_raised = 1;
         end else if (objection_raised && mwr_req_pending == 0) begin
            phase.drop_objection(this, "SMM BFM: No pending memory read requests.", 1);
            
            msg_str = $sformatf("run_phase(): %s. ", operation_mode.name());
            msg_str = {msg_str, $sformatf("%0d pending memory write requests, dropped phase objection.",
               mwr_req_pending)};
            
            `uvm_info(get_type_name(), msg_str, UVM_DEBUG)
            
            objection_raised = 0;
         end
      end
   endtask : run_phase
endclass : mby_smm_bfm_mwr_req

`endif

