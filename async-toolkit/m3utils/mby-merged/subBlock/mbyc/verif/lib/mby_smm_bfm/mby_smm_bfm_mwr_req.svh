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

   // VARIABLE: wr_req_cfg_obj
   //    The BFM's mwr request configuration objects
   mby_smm_bfm_cfg wr_req_cfg_obj;

   // VARIABLE: mesh_ptr
   //    Pointer to the SMM BFM nodes array.
   smm_bfm_mem_node  mesh_ptr[MAX_NUM_MSH_ROWS-1:0][MAX_NUM_MSH_COLS-1:0];

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
      
      wr_req_cfg_obj = new("wr_req_cfg_obj");
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
      mwr_req(ap_item);
   endfunction

   // ------------------------------------------------------------------------
   // FUNCTION: mwr_req
   //
   // Receives a memory write request, and stores the data into the SMM BFM memory.
   // Takes place in 0 simulation time as there are no requirements to model write
   // requests' delays.
   //
   // ARGUMENTS:
   //    T_req mem_req - memory write request issued to the SMM BFM.
   // ------------------------------------------------------------------------
   function void mwr_req(T_req mem_req);
      // TODO: parameterize these definitions
      bit [ADDR_WIDTH-1:0]  addr;     // Memory write address
      bit [MSH_DATA_WIDTH-1:0] wr_data;  // Memory write data
      
      bit [NUM_MSH_ROWS-1:0]   node_row; // SMM BFM node column
      bit [NUM_MSH_COLS-1:0]   node_col; // SMM BFM node row
      
      addr     = mem_req.data_pkt.mim_wr_seg_ptr[13:0];
      node_row = mem_req.data_pkt.mim_wr_seg_ptr[17:14];
      node_col = {mem_req.data_pkt.mim_wr_seg_ptr[1:0]^mem_req.data_pkt.mim_wr_wd_sel,mem_req.data_pkt.mim_wr_seg_ptr[18]};
      wr_data  = mem_req.data_pkt.mim_wr_data;
      
      `uvm_info(get_type_name(), $sformatf("mwr_req() : Received a memory write request for NodeRow = 0x%0x, NodeCol = 0x%0x, Address = 0x%0x. WrData = 0x%x", node_row, node_col, addr, wr_data),  UVM_MEDIUM)
      
      mesh_ptr[node_row][node_col].mwr(addr,wr_data);
   endfunction

   // -------------------------------------------------------------------------
   // TASK: run_phase
   //
   // Main monitor thread starts: calls the monitor_if() virtual task
   //
   // ARGUMENTS:
   //    uvm_phase phase - phase object.
   // -------------------------------------------------------------------------
   task run_phase (uvm_phase phase);
   endtask : run_phase
endclass : mby_smm_bfm_mwr_req

`endif

