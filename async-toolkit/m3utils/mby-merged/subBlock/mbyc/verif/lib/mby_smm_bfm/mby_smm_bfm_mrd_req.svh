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
//-----------------------------------------------------------------------------

class mby_smm_bfm_mrd_req
   #(
      type T_req  = mby_base_sequence_item
   )
   extends uvm_pkg::uvm_subscriber #(T_req);

//   T_req mem_req;
//   T_req mem_rsp;
   
   smm_bfm_row_rd_req_agent rd_req_agent_ptr;

   smm_bfm_mem_node  mesh_ptr[MAX_NUM_MSH_ROWS-1:0][MAX_NUM_MSH_COLS-1:0];
   
   
//   logic [13:0]  addr;
//   logic [3:0]   node_row;
//   logic [2:0]   node_col;
//   logic [511:0] wr_data;
//   logic [511:0] rd_data;
//   logic [12:0]  req_id;
//   logic         rreq_valid;
//   logic         rrsp_valid;
//   time          rreq_delay;
//   time          rrsp_delay;
 

// Registering class with the factory
   `uvm_component_utils(mby_smm_bfm_mrd_req#(T_req))

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

   function void set_mesh_ptr(smm_bfm_mem_node  mem_nodes_ptr[MAX_NUM_MSH_ROWS-1:0][MAX_NUM_MSH_COLS-1:0]);
      for(int row_idx=0 ; row_idx<MAX_NUM_MSH_ROWS; row_idx++) begin
         for(int col_idx=0 ; col_idx<MAX_NUM_MSH_COLS; col_idx++) begin
            this.mesh_ptr[row_idx][col_idx] = mem_nodes_ptr[row_idx][col_idx];
         end
      end
   endfunction : set_mesh_ptr

   function void set_agent_ptr(smm_bfm_row_rd_req_agent rd_req_agent_ptr);
      this.rd_req_agent_ptr = rd_req_agent_ptr;
   endfunction : set_agent_ptr


   // ------------------------------------------------------------------------
   // FUNCTION: build_phase
   //
   // Gets the agent's configuration object and vif from the config db.
   // Creates monitor, sequencer and driver as specified in configuration object
   //
   // ------------------------------------------------------------------------
   function void build_phase(uvm_phase phase);
      super.build_phase(phase);
   endfunction : build_phase


   // Function: write
   //
   // Method that must be defined in each uvm_subscriber subclass. Access
   // to this method by outside components should be done via the
   // analysis_export.
   function void write(T_req ap_item);
//      mem_req = ap_item;
//      rreq_valid = 1;
      fork
         mrd_req_rsp(ap_item);
      join_none
   endfunction


//   virtual protected task automatic mrd_req();    // TODO: Change to task to make it time consuming for delay modeling
//        `uvm_info("mby_smm_bfm_mrd_req()::mrd_req(): Got into...","",     UVM_MEDIUM);
//     forever begin : issue_mrd_req 
//         wait(rreq_valid == 1);
//         req_id   = mem_req.data_pkt.mim_req_id;
//         addr     = mem_req.data_pkt.mim_seg_ptr[13:0];
//         node_row = mem_req.data_pkt.mim_seg_ptr[17:14];
//         node_col = {mem_req.data_pkt.mim_seg_ptr[1:0]^mem_req.data_pkt.mim_wd_sel,mem_req.data_pkt.mim_seg_ptr[18]};
//         rreq_delay = 833*(node_row+node_col);
//         //#(rreq_delay);
//         rd_data = mesh_ptr[node_row][node_col].mrd(addr);
//         `uvm_info("mby_smm_bfm_mrd_req()::mrd_req(): Address=", $sformatf("%014h", addr),     UVM_MEDIUM);
//         `uvm_info("mby_smm_bfm_mrd_req()::mrd_req(): NodeRow=", $sformatf("%04h",  node_row), UVM_MEDIUM);
//         `uvm_info("mby_smm_bfm_mrd_req()::mrd_req(): NodeCol=", $sformatf("%03h",  node_col), UVM_MEDIUM);
//         `uvm_info("mby_smm_bfm_mrd_req()::mrd_req(): rd_data=", $sformatf("%0128h",rd_data),  UVM_MEDIUM);
//         rrsp_valid = 1;
//         #100;
//         rreq_valid = 0;
//         rrsp_valid = 0;
//     end
//   endtask : mrd_req
//
//   
//   virtual protected task automatic mrd_rsp();
//      `uvm_info("mby_smm_bfm_mrd_req()::mrd_rsp(): Got into...","",     UVM_MEDIUM);
//      forever begin : collect_mem_rd
//         // Wait for a new transaction to appear
//         wait(rrsp_valid === 1);
//         
//         mem_rsp = T_req::type_id::create("mem_rsp", this);
//         mem_rsp.data_pkt.mim_rrsp_valid = 1;
//         mem_rsp.data_pkt.mim_rd_data = rd_data;
//         mem_rsp.data_pkt.mim_rrsp_req_id = req_id;
//         rrsp_delay = 833*(node_row+node_col+$urandom_range(0, 16));
//         //#(rrsp_delay);
//         `uvm_info("mby_smm_bfm_mrd_req()::mrd_rsp(): Delay=",rrsp_delay,     UVM_MEDIUM);
//         this.rd_req_agent_ptr.driver.vintf.drive_data(mem_rsp.data_pkt, 0, 0);
//         // Create the seq_item
//         `uvm_info("mby_smm_bfm_mrd_req()::mrd_rsp(): Creating the transaction for MemRd Response","", UVM_MEDIUM);
//         #100;
//         mem_rsp.data_pkt.mim_rrsp_valid = 0;
//         rrsp_valid = 0;
//      end
//   endtask : mrd_rsp

   virtual protected task automatic mrd_req_rsp(T_req mem_req);
      T_req          mem_rsp;
      
      logic [12:0]   req_id;
      logic [13:0]   addr;
      logic [3:0]    node_row;
      logic [2:0]    node_col;
      logic [511:0]  rd_data;
      
      time           rreq_delay;
      time           rrsp_delay;
      
      req_id      = mem_req.data_pkt.mim_req_id;
      addr        = mem_req.data_pkt.mim_seg_ptr[13:0];
      node_row    = mem_req.data_pkt.mim_seg_ptr[17:14];
      node_col    = {mem_req.data_pkt.mim_seg_ptr[1:0]^mem_req.data_pkt.mim_wd_sel,mem_req.data_pkt.mim_seg_ptr[18]};
      
      `uvm_info(get_type_name(), $sformatf("mby_smm_bfm_mrd_req::mrd_req_rsp() : Received a memory read request. ReqID = 0x%x", req_id),  UVM_MEDIUM);
      `uvm_info(get_type_name(), $sformatf("mby_smm_bfm_mrd_req::mrd_req_rsp() : Address = 0x%0x. ReqID = 0x%x", addr, req_id),           UVM_MEDIUM);
      `uvm_info(get_type_name(), $sformatf("mby_smm_bfm_mrd_req::mrd_req_rsp() : NodeRow = 0x%0x. ReqID = 0x%x", node_row, req_id),       UVM_MEDIUM);
      `uvm_info(get_type_name(), $sformatf("mby_smm_bfm_mrd_req::mrd_req_rsp() : NodeCol = 0x%0x. ReqID = 0x%x", node_col, req_id),       UVM_MEDIUM);
      
      // Process the input memory read request
      
      rreq_delay  = 833*(node_row+node_col);
      
      `uvm_info(get_type_name(), $sformatf("mby_smm_bfm_mrd_req::mrd_req_rsp() : Simulating read request delay of  = %0d. ReqID = 0x%x", rreq_delay, req_id),  UVM_MEDIUM);
      
      #(rreq_delay);
      
      rd_data = mesh_ptr[node_row][node_col].mrd(addr);
      
      `uvm_info(get_type_name(), $sformatf("mby_smm_bfm_mrd_req::mrd_req_rsp() :  RdData  = 0x%0x. ReqID = 0x%x", rd_data, req_id),  UVM_MEDIUM);
      
      //#100;
      
      // Now generate the memory read response
      
      mem_rsp = T_req::type_id::create("mem_rsp", this);
      mem_rsp.data_pkt.mim_rrsp_valid  = 1;
      mem_rsp.data_pkt.mim_rd_data     = rd_data;
      mem_rsp.data_pkt.mim_rrsp_req_id = req_id;
      
      rrsp_delay = 833*(node_row+node_col+$urandom_range(0, 16));
      
      `uvm_info(get_type_name(), $sformatf("mby_smm_bfm_mrd_req::mrd_req_rsp() : Simulating read response delay of  = %0d. ReqID = 0x%x", rrsp_delay, req_id),  UVM_MEDIUM);
      
      #(rrsp_delay);
      
      `uvm_info(get_type_name(), $sformatf("mby_smm_bfm_mrd_req::mrd_req_rsp() : Sending response to driver. ReqID = 0x%x", req_id),  UVM_MEDIUM);
      
      // Might want to convert this into a fifo specially on highly concurrent scenarios
      this.rd_req_agent_ptr.driver.vintf.drive_data(mem_rsp.data_pkt, 0, 0);
      
      //#100;
      
      mem_rsp.data_pkt.mim_rrsp_valid = 0;
      
      `uvm_info(get_type_name(), $sformatf("mby_smm_bfm_mrd_req::mrd_req_rsp() : Request done. ReqID = 0x%x", req_id),  UVM_MEDIUM);
   endtask : mrd_req_rsp

   // -------------------------------------------------------------------------
   // TASK: run_phase
   //
   // Main monitor thread starts: calls the monitor_if() virtual task
   //
   // -------------------------------------------------------------------------
  task run_phase (uvm_phase phase);
//      fork
//         begin : issue_req
//            mrd_req();
//         end
//         begin : issue_rsp
//            mrd_rsp();
//         end
//      join
   endtask : run_phase




endclass : mby_smm_bfm_mrd_req

`endif

