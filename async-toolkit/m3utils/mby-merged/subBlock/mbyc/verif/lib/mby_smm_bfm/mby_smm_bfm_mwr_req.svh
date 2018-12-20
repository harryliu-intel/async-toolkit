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
//-----------------------------------------------------------------------------

class mby_smm_bfm_mwr_req
   #(
      type T_req  = mby_base_sequence_item
   )
   extends uvm_pkg::uvm_subscriber #(T_req);

   T_req mem_req;
   
   smm_bfm_mem_node  mesh_ptr[MAX_NUM_MSH_ROWS-1:0][MAX_NUM_MSH_COLS-1:0];
    
   logic [13:0]  addr;
   logic [3:0]   node_row;
   logic [2:0]   node_col;
   logic [511:0] wr_data;
   time          wreq_delay;

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

   function void set_mesh_ptr(smm_bfm_mem_node  mem_nodes_ptr[MAX_NUM_MSH_ROWS-1:0][MAX_NUM_MSH_COLS-1:0]);
      for(int row_idx=0 ; row_idx<MAX_NUM_MSH_ROWS; row_idx++) begin
         for(int col_idx=0 ; col_idx<MAX_NUM_MSH_COLS; col_idx++) begin
            this.mesh_ptr[row_idx][col_idx] = mem_nodes_ptr[row_idx][col_idx];
         end
      end
   endfunction : set_mesh_ptr


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

   // -------------------------------------------------------------------------
   //
   // TODO : This is an empty shell for the address translation block
   //        - add analysis port declarations, build, connect and run phases.
   //
   // -------------------------------------------------------------------------

   

   function void mwr_req(); // TODO: Change to task to make it time consuming for delay modeling
      `uvm_info("mby_smm_bfm_mwr_req()::mwr_req(): ", mem_req.convert2string(), UVM_MEDIUM);
      addr     = mem_req.data_pkt.mim_wr_seg_ptr[13:0];
      node_row = mem_req.data_pkt.mim_wr_seg_ptr[17:14];
      node_col = {mem_req.data_pkt.mim_wr_seg_ptr[1:0]^mem_req.data_pkt.mim_wr_wd_sel,mem_req.data_pkt.mim_wr_seg_ptr[18]};
      wr_data  = mem_req.data_pkt.mim_wr_data;
      `uvm_info("mby_smm_bfm_mwr_req()::mwr_req(): Address=", $sformatf("%014h",addr), UVM_MEDIUM);
      `uvm_info("mby_smm_bfm_mwr_req()::mwr_req(): NodeRow=", $sformatf("%04h",node_row), UVM_MEDIUM);
      `uvm_info("mby_smm_bfm_mwr_req()::mwr_req(): NodeCol=", $sformatf("%03h",node_col), UVM_MEDIUM);
      `uvm_info("mby_smm_bfm_mwr_req()::mwr_req(): wr_data=",    $sformatf("%0128h",wr_data), UVM_MEDIUM);
      //wreq_delay = 833*(node_row+node_col);
      //#(wreq_delay);
      mesh_ptr[node_row][node_col].mwr(addr,wr_data);
         // Call desired functionality in parent.
   endfunction

   

   // Function: write
   //
   // Method that must be defined in each uvm_subscriber subclass. Access
   // to this method by outside components should be done via the
   // analysis_export.
   function void write(T_req ap_item);
      mem_req = ap_item;
      mwr_req();
   // Call desired functionality in parent.
   endfunction


//   virtual protected task delay();
//      forever begin : delay_thread
//         
//         //mwr_req();
//         //request_delay();
//      end
//   endtask : delay

   // -------------------------------------------------------------------------
   // TASK: run_phase
   //
   // Main monitor thread starts: calls the monitor_if() virtual task
   //
   // -------------------------------------------------------------------------
//   task run_phase (uvm_phase phase);
//      fork
//         begin : issue_req
//            delay();
//         end
//      join
//  endtask : run_phase





endclass : mby_smm_bfm_mwr_req

`endif

