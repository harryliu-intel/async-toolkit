//-----------------------------------------------------------------------------
// Title         : Madison Bay SMM Bus Functional Model
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : mby_smm_bfm.svh
// Author        : Roman Bernal <r.bernal@intel.com>
// Created       : 01.11.2018
//-----------------------------------------------------------------------------
// Description :
// This is the main smm_bfm class
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
`ifndef __MBY_SMM_BFM__
`define __MBY_SMM_BFM__
//-----------------------------------------------------------------------------
// CLASS: mby_smm_bfm
//
// Main class for the SMM BFM. It simulates the memory mesh behavior and latencies.
// This class contains the agents and subscribers used to handle all memory read and write
// requests coming from the outside.
// TODO : it will also provide coverage data on the use cases taking place, it will support
// basic/advanced traffic scenarios as well as resets.
//-----------------------------------------------------------------------------
class mby_smm_bfm extends uvm_component;
   // VARIABLE cfg_obj
   mby_smm_bfm_cfg cfg_obj;

   // VARIABLE: igr_wr_req_agent 
   // SMM BFM Agent Instance interfacing with the Ingress for Write Request Transactions.
   smm_bfm_wr_req_agent igr_wr_req_agent;
   
   // VARIABLE: egr_rd_req_agent 
   // SMM BFM Agent Instance interfacing with the Egress for Read Request Transactions.
   smm_bfm_rd_req_agent egr_rd_req_agent;

   // TODO : temporarily disabled, will be used for Global Memory? Manager and Multicast Engine
   // VARIABLE: gmm_wr_req_agent, mce_wr_req_agent 
   // These are the Mesh BFM Agent Instances interfacing with the GMM and MCE respectively for Write Request Transactions.
   //smm_bfm_col_wr_req_agent gmm_wr_req_agent;
   //smm_bfm_col_wr_req_agent mce_wr_req_agent;

   // VARIABLE: gmm_rd_req_agent, mce_rd_req_agent 
   // These are the Mesh BFM Agent Instances interfacing with the GMM and MCE respectively for Read Request/Responses Transactions.
   //smm_bfm_col_rd_req_agent gmm_rd_req_agent;
   //smm_bfm_col_rd_req_agent mce_rd_req_agent;

   // VARIABLE: mem_mesh 
   // Array of SMM BFM memory node instances representing the whole mesh.
   smm_bfm_mem_node  mem_mesh[MAX_NUM_MSH_ROWS-1:0][MAX_NUM_MSH_COLS-1:0];
   
   // VARIABLE: mwr_req_row 
   // Subscriber that handles incoming memory write requests from Ingress.
   smm_bfm_mwr_req   mwr_req_row;
   
   // VARIABLE: mrd_req_row
   // Subscriber that handles incoming memory read requests from Egress.
   smm_bfm_mrd_req   mrd_req_row;
   
   // -------------------------------------------------------------------------
   // Macro to register new class type
   // -------------------------------------------------------------------------
   `uvm_component_param_utils_begin(mby_smm_bfm)
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
   // The smm_agent is created and the configuration object is assigned to it.
   // At this level the mesh (array of mem node classes) is created as well.
   //
   // ------------------------------------------------------------------------
   function void build_phase(uvm_phase phase);
      super.build_phase(phase);
      
      // Set up config agents
      igr_wr_req_agent = smm_bfm_wr_req_agent::type_id::create("igr_wr_req_agent", this);
      egr_rd_req_agent = smm_bfm_rd_req_agent::type_id::create("egr_rd_req_agent", this);
      
      igr_wr_req_agent.cfg = this.cfg_obj.mwr_req_agent_cfg;
      egr_rd_req_agent.cfg = this.cfg_obj.mrd_req_agent_cfg;
      
      // Instantiate all of the SMM memory nodes into the mesh array
      for(int row_idx=0 ; row_idx<MAX_NUM_MSH_ROWS; row_idx++) begin
         for(int col_idx=0 ; col_idx<MAX_NUM_MSH_COLS; col_idx++) begin
            mem_mesh[row_idx][col_idx] = smm_bfm_mem_node::type_id::create($sformatf("mem_node%d%d",row_idx,col_idx), this);
            mem_mesh[row_idx][col_idx].set_row_col(row_idx, col_idx);
         end
      end

      // TODO : replicate mrd/mwr subscribers as per total DUT interfaces (6 of each)
      // Set up subscribers
      mwr_req_row = smm_bfm_mwr_req::type_id::create("mwr_req_row", this);
      mrd_req_row = smm_bfm_mrd_req::type_id::create("mrd_req_row", this);

      mwr_req_row.cfg_obj = this.cfg_obj;
      mwr_req_row.set_mesh_ptr(mem_mesh);
      mwr_req_row.set_agent_ptr(igr_wr_req_agent);
      mwr_req_row.set_operation_opts(INGRESS_MODE, WITHOUT_DELAY_SIMULATION);

      mrd_req_row.cfg_obj = this.cfg_obj;
      mrd_req_row.set_mesh_ptr(mem_mesh);
      mrd_req_row.set_agent_ptr(egr_rd_req_agent);
      mrd_req_row.set_operation_opts(EGRESS_MODE, WITH_DELAY_SIMULATION);

      // TODO : set up mrd_req_col for GPM_MODE

   endfunction : build_phase

   // ------------------------------------------------------------------------
   // FUNCTION: connect_phase
   //
   // Connecting subscribed component to monitor analysis port
   //
   // ------------------------------------------------------------------------
   function void connect_phase(uvm_phase phase);
      super.connect_phase(phase);
      igr_wr_req_agent.monitor.mon_ap.connect(mwr_req_row.analysis_export);
      egr_rd_req_agent.monitor.mon_ap.connect(mrd_req_row.analysis_export);
    endfunction : connect_phase

endclass : mby_smm_bfm
`endif
