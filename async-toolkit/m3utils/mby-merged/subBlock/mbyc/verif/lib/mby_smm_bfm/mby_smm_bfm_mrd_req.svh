// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

//-----------------------------------------------------------------------------
// Title         : Madison Bay SMM BFM - Memory Mesh 
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : mby_smm_bfm_mrd_req.svh
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
`ifndef __MBY_SMM_BFM_MEM_REQ__
`define __MBY_SMM_BFM_MEM_REQ__
//-----------------------------------------------------------------------------
// CLASS: mby_smm_bfm_mrd_req
//
// This mrd req class is instantiated in the top smm_bfm class. Under this class
// there is a pointer to the Memory Mesh (array of mem node classes -mby_smm_bfm_mem_node)
// instantiated also on smm_bfm class.
// It consumes memory requests from the different interfaces and execute Data
// MemWrs/MemRds to a given Mem Node in the Mesh and location in the Memory Node
// based on Row, Col, Address coming in Request.
//
// PARAMETERS:
//     type T_req - sequence item type to be handled.
//-----------------------------------------------------------------------------

class mby_smm_bfm_mrd_req
   #(
      type T_req  = mby_base_sequence_item
   )
   extends uvm_pkg::uvm_subscriber #(T_req);

   // VARIABLE: cfg_obj
   //    The bfm's mrd request configuration object
   mby_smm_bfm_cfg cfg_obj;

   // VARIABLE: rd_req_agent_prt
   //    Pointer to the SMM BFM read request agent.
   smm_bfm_rd_req_agent rd_req_agent_ptr;

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
   
   // VARIABLE: mrd_req_pending
   //    Counter of the current memory read requests that are pending to be delivered to egress.
   int mrd_req_pending;
   
   // Registering class with the factory
   `uvm_component_utils(mby_smm_bfm_mrd_req#(T_req))

   // -------------------------------------------------------------------------
   // CONSTRUCTOR: new
   //
   // Constructor
   //
   // ARGUMENTS
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
   //    smm_bfm_rd_req_agent rd_req_agent_ptr  - An instance name of the address translator.
   // -------------------------------------------------------------------------
   function void set_agent_ptr(smm_bfm_rd_req_agent rd_req_agent_ptr);
      this.rd_req_agent_ptr = rd_req_agent_ptr;
   endfunction : set_agent_ptr

   // -------------------------------------------------------------------------
   // FUNCTION: set_operation_opts
   //
   // Sets operation options for this subscriber as follows
   //
   // Operation modes
   //    It defines how the subscriber should behave depending on the component that will be connected to. which can
   //    be either Egress or Global Pointer Manager (GPM). Mode of operation helps to differentiate the subscriber
   //    instance and handle the requirements specific to Egress or GPM.
   //
   //    EGRESS_MODE: The subscriber gets connected to one of the six Egress-to-Mesh interfaces, and sets its behavior
   //       to get, process and respond memory read requests from Egress.
   //    GPM_MODE: The subscriber gets connected to a GPM port, and sets its behavior to get, process and respond 
   //       memory write requests from GPM.
   //    Note: INGRESS_MODE doesn't apply to this subscriber as Ingress doesn't issue memory reads.
   //
   // Delay simulation:
   //    If set, then the subscriber will hold the memory read response for a random number of cycles in order to
   //    simulate the time required to look for and retrieve the requested data from the mesh.
   //
   //    WITH_DELAY_SIMULATION: The memory read response gets hold for a random number of cycles before sending it
   //       back to the DUT.
   //    WITHOUT_DELAY_SIMULATION: The memory read response gets returned to the DUT immediately after the memory
   //       read request got received, taking it 0 simulation time to complete.
   //
   // ARGUMENTS:
   //    operation_mode_e operation_mode     -  Defines the subscriber behavior.
   //    delay_simulation_e delay_simulation -  Indicates whether delay simulation takes place.
   // -------------------------------------------------------------------------
   function void set_operation_opts(operation_mode_e operation_mode = EGRESS_MODE, delay_simulation_e delay_simulation = WITH_DELAY_SIMULATION);
      if (operation_mode inside {INGRESS_MODE}) begin
         `uvm_fatal(get_type_name(), "set_operation_opts(): %s is not supported for mrd_req subscriber.")
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
      
      mrd_req_pending   = 0;
   endfunction : build_phase

   // ------------------------------------------------------------------------
   // FUNCTION: write
   //
   // Method that must be defined in each uvm_subscriber subclass. Access
   // to this method by outside components should be done via the
   // analysis_export.
   //
   // ARGUMENTS:
   //    T_req ap_item - memory read request issued to the SMM BFM.
   // ------------------------------------------------------------------------
   function void write(T_req ap_item);
      start_mrd_req_rsp_task(ap_item);
   endfunction

   // ------------------------------------------------------------------------
   // FUNCTION: start_mrd_req_rsp_task
   //
   // Sets up an separate context for the received memory read request and
   // start the corresponding read request/response task depending on delay_simulation
   // specified.
   //
   // ARGUMENTS:
   //    T_req mem_req - contains a memory read request to the SMM BFM.
   // ------------------------------------------------------------------------
   function void start_mrd_req_rsp_task(T_req mem_req);
      fork
         mrd_req_rsp(mem_req);
      join_none
   endfunction

   // ------------------------------------------------------------------------
   // TASK: mrd_req_rsp
   //
   // Receives a memory read request from Egress or Global Pointer Manager, adds the request/response time delays
   // depending on the configuration set at set_operation_opts(), and generate a memory read response for the model.
   //
   // ARGUMENTS:
   //    T_req mem_req - contains a memory read request to the SMM BFM.
   // ------------------------------------------------------------------------
   virtual protected task automatic mrd_req_rsp(T_req mem_req);
      smm_bfm_mrd_seq   rdrsp_seq;  // Sequence object used to drive the generated memory response
      T_req             mem_rsp;    // Sequence item to store the generated memory response
      
      bit [SMM_BFM_W_REQ_ID-1:0]          req_id;     // Memory read request ID
      bit [SMM_BFM_ADDR_WIDTH-1:0]        addr;       // Memory read address
      bit [SMM_BFM_DATA_WIDTH-1:0]        rd_data;    // Memory read data
      bit [SMM_BFM_W_RRSP_DEST_BLOCK-1:0] dest_block; // Memory read response Dest Block
      bit [SMM_BFM_NUM_MSH_ROWS-1:0]      node_row;   // SMM BFM node column
      bit [SMM_BFM_NUM_MSH_COLS-1:0]      node_col;   // SMM BFM node row
      
      int unsigned   mrd_req_delay;    // Modeled read request delay
      
      string   msg_str;
      string   mrd_req_str;
      
      rdrsp_seq   = smm_bfm_mrd_seq::type_id::create("rdrsp_seq", this);
      mem_rsp     = T_req::type_id::create("mem_rsp", this);
      
      // Decode memory read request data
      req_id      = mem_req.data.mim_req_id;
      dest_block  = mem_req.data.mim_wd_sel;   //TODO: what is dest block? - for now assigned with wd_sel
      addr        = mem_req.data.mim_seg_ptr[13:0];
      node_row    = mem_req.data.mim_seg_ptr[17:14];
      node_col    = {mem_req.data.mim_seg_ptr[1:0]^mem_req.data.mim_wd_sel,mem_req.data.mim_seg_ptr[18]};
      rd_data     = mesh_ptr[node_row][node_col].mrd(addr);
      
      mrd_req_str = $sformatf("NodeRow = 0x%0x, NodeCol = 0x%0x, Address = 0x%04x", node_row, node_col, addr);
      
      msg_str     = $sformatf("mrd_req_rsp(): %s Received a memory read request for ", operation_mode.name());
      msg_str     = {msg_str, $sformatf("%s, RdData = 0x%0128x", mrd_req_str, rd_data)};
      
      `uvm_info(get_type_name(), msg_str,  UVM_MEDIUM)
      
      // Generate the memory read response accordingly
      mem_rsp.data.mim_rrsp_valid        = 1;
      mem_rsp.data.mim_rd_data           = rd_data;
      mem_rsp.data.mim_rrsp_req_id       = req_id;
      mem_rsp.data.mim_rrsp_dest_block   = dest_block;
      
      if (delay_simulation inside {WITH_DELAY_SIMULATION}) begin
         // Increase the number of pending read requests so the main thread can raise an objection to let
         // this task finish properly
         mrd_req_pending++;
         
         // Generate profile based latencies, min and max got adjusted based on the configured profile.
         mrd_req_delay = $urandom_range(cfg_obj.mrd_req_delay_min, cfg_obj.mrd_req_delay_max);
         
         msg_str = $sformatf("mrd_req_rsp(): %s Delay of read request/response is ", operation_mode.name());
         msg_str = {msg_str, $sformatf("%0d clocks. %s", mrd_req_delay, mrd_req_str)};
         
         `uvm_info(get_type_name(), msg_str,  UVM_MEDIUM)
         
         // Simulate read request/response latency
         repeat(mrd_req_delay) @(posedge this.rd_req_agent_ptr.driver.io_policy.vintf.clk);
      end
      
      // Once the delay got simulated, the memory read response gets sent through the sequencer
      rdrsp_seq.mem_rsp = mem_rsp;
      rdrsp_seq.start(this.rd_req_agent_ptr.sequencer);
      
      mem_rsp.data.mim_rrsp_valid = 0;
      
      msg_str  = $sformatf("mrd_req_rsp(): %s Memory read request done. ", operation_mode.name());
      msg_str  = {msg_str, mrd_req_str};
      
      `uvm_info(get_type_name(), msg_str, UVM_MEDIUM)
      
      if (delay_simulation inside {WITH_DELAY_SIMULATION}) begin
         // Decrease the number of pending read requests, so the main thread can drop the objection if there aren't
         // more pending read requests
         mrd_req_pending--;
      end
   endtask : mrd_req_rsp

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
      
      forever @ (posedge this.rd_req_agent_ptr.driver.io_policy.vintf.clk) begin
         // Maintain track of the memory read requests in flight, will hold on test
         // completion until all read requests are completed.
         if (!objection_raised && mrd_req_pending > 0) begin
            phase.raise_objection(this, "SMM BFM: Pending memory read requests.", 1);
            
            msg_str = $sformatf("run_phase(): %0d pending memory read requests, raised phase objection.",
               mrd_req_pending);
            
            `uvm_info(get_type_name(), msg_str, UVM_DEBUG)
            
            objection_raised = 1;
         end else if (objection_raised && mrd_req_pending == 0) begin
            phase.drop_objection(this, "SMM BFM: No pending memory read requests.", 1);
            
            msg_str = $sformatf("run_phase(): %0d pending memory read requests, dropped phase objection.",
               mrd_req_pending);
            
            `uvm_info(get_type_name(), msg_str, UVM_DEBUG)
            
            objection_raised = 0;
         end
      end
   endtask : run_phase
endclass : mby_smm_bfm_mrd_req

`endif

