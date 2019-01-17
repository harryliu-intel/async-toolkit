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
// PARAMETERS::
//     type T_req - sequence item type to be handled.
//-----------------------------------------------------------------------------

class mby_smm_bfm_mrd_req
   #(
      type T_req  = mby_base_sequence_item
   )
   extends uvm_pkg::uvm_subscriber #(T_req);

   // VARIABLE: rd_req_cfg_obj
   //    The bfm's mrd request configuration objects
   mby_smm_bfm_cfg rd_req_cfg_obj;

   // VARIABLE: rd_req_agent_prt
   //    Pointer to the SMM BFM read request agent.
   smm_bfm_row_rd_req_agent rd_req_agent_ptr;

   // VARIABLE: mesh_ptr
   //    Pointer to the SMM BFM nodes array.
   smm_bfm_mem_node  mesh_ptr[MAX_NUM_MSH_ROWS-1:0][MAX_NUM_MSH_COLS-1:0];
   
   // VARIABLE: rd_req_pending
   //    Counter of the current memory read requests that are pending to be delivered to egress.
   int rd_req_pending;
   
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
   //    smm_bfm_row_rd_req_agent rd_req_agent_ptr  - An instance name of the address translator.
   // -------------------------------------------------------------------------
   function void set_agent_ptr(smm_bfm_row_rd_req_agent rd_req_agent_ptr);
      this.rd_req_agent_ptr = rd_req_agent_ptr;
   endfunction : set_agent_ptr

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
      
      rd_req_cfg_obj = new("rd_req_cfg_obj");
      rd_req_pending = 0;
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
   // Sets up an separate context for the received memory write request and
   // start the corresponding read request/response task.
   //
   // ARGUMENTS:
   //    T_req mem_req - contains a memory read request to the SMM BFM.
   // ------------------------------------------------------------------------
   function void start_mrd_req_rsp_task(T_req mem_req);
      // Have a local copy of mem_req, task can be forked safely.
      fork
         mrd_req_rsp(mem_req);
      join_none
   endfunction

   // ------------------------------------------------------------------------
   // FUNCTION: mrd_req_rsp
   //
   // Receives a memory read request, adds the request/response modeled time delays,
   // and generate a memory read response for the model.
   //
   // ARGUMENTS:
   //    T_req mem_req - contains a memory read request to the SMM BFM.
   // ------------------------------------------------------------------------
   virtual protected task automatic mrd_req_rsp(T_req mem_req);
      T_req          mem_rsp;       // Sequence item to store the generated memory response
      
      smm_bfm_rdrsp_seq  rdrsp_seq;
      
      // TODO: parameterize these definitions, these come from a file
      // TODO : change logics by bits
      bit [W_REQ_ID-1:0]   req_id;        // Memory read request ID
      bit [ADDR_WIDTH-1:0]   addr;          // Memory read address
      bit [MSH_DATA_WIDTH-1:0]  rd_data;       // Memory read data
      
      bit [NUM_MSH_ROWS-1:0]    node_row;      // SMM BFM node column
      bit [NUM_MSH_COLS-1:0]    node_col;      // SMM BFM node row
      
      int unsigned   req_rsp_delay; // Modeled read request delay
      
      string         msg_str;
      
      rd_req_pending++;
      
      rdrsp_seq = smm_bfm_rdrsp_seq::type_id::create("rdrsp_seq", this);
      
      // Decode memory read request data
      req_id      = mem_req.data.mim_req_id;
      addr        = mem_req.data.mim_seg_ptr[13:0];
      node_row    = mem_req.data.mim_seg_ptr[17:14];
      node_col    = {mem_req.data.mim_seg_ptr[1:0]^mem_req.data.mim_wd_sel,mem_req.data.mim_seg_ptr[18]};
      rd_data     = mesh_ptr[node_row][node_col].mrd(addr);
      
      msg_str     = $sformatf("mrd_req_rsp() : Received a memory read request for NodeRow = 0x%0x, NodeCol = 0x%0x, \
                        Address = 0x%0x, RdData = 0x%0x. ReqID = 0x%x", node_row, node_col, addr, rd_data, req_id);
      
      `uvm_info(get_type_name(), msg_str,  UVM_MEDIUM)
      
      // Generate the memory read response
      mem_rsp = T_req::type_id::create("mem_rsp", this);
      mem_rsp.data.mim_rrsp_valid  = 1;
      mem_rsp.data.mim_rd_data     = rd_data;
      mem_rsp.data.mim_rrsp_req_id = req_id;
      
      // TODO ;   Profile based latencies.
      req_rsp_delay  = rd_req_cfg_obj.mrd_req_rsp_delay_extra;
      
      msg_str     = $sformatf("mrd_req_rsp() : Delay of read request/response operation is %0d clocks. ReqID = 0x%x",
                       req_rsp_delay, req_id);
      
      `uvm_info(get_type_name(), msg_str,  UVM_MEDIUM)
      
      // Look for the driver in this agent then for a vif and then a clock in it
      repeat(req_rsp_delay) @(posedge this.rd_req_agent_ptr.driver.io_policy.vintf.clk);
      
      // Use this instead
      rdrsp_seq.mem_rsp = mem_rsp;
      rdrsp_seq.start(this.rd_req_agent_ptr.sequencer);
      
      mem_rsp.data.mim_rrsp_valid = 0;
      
      msg_str     = $sformatf("mrd_req_rsp() : Memory read request done. ReqID = 0x%x", req_id);
      
      `uvm_info(get_type_name(), msg_str, UVM_MEDIUM)
      
      rd_req_pending--;
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
         if (!objection_raised && rd_req_pending > 0) begin
            phase.raise_objection(this, "SMM BFM: Pending memory read requests.", 1);
            
            msg_str = $sformatf("run_phase(): There are %0d pending memory read requests, raised phase objection.",
                         rd_req_pending);
            
            `uvm_info(get_type_name(), msg_str, UVM_DEBUG)
            
            objection_raised = 1;
         end else if (objection_raised && rd_req_pending == 0) begin
            phase.drop_objection(this, "SMM BFM: No pending memory read requests.", 1);
            
            msg_str = $sformatf("run_phase(): There are %0d pending memory read requests, dropped phase objection.",
                         rd_req_pending);
            
            `uvm_info(get_type_name(), msg_str, UVM_DEBUG)
            
            objection_raised = 0;
         end
      end
   endtask : run_phase
endclass : mby_smm_bfm_mrd_req

`endif

