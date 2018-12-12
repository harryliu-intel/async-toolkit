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
//-----------------------------------------------------------------------------
class mby_smm_bfm extends uvm_component;

   // VARIABLE: igr_wr_req_cfg_obj, igr_wr_req_cfg_obj
   // The bfm's mwr/mrd request configuration objects
   mby_smm_bfm_cfg igr_wr_req_cfg_obj;
   mby_smm_bfm_cfg egr_rd_req_cfg_obj;

   // VARIABLE: igr_wr_req_agent, egr_rd_req_agent 
   // These are the Mesh BFM Agent Instances interfacing with the Ingress and Egress respectively for Write/Read Request Transactions.
   smm_bfm_row_wr_req_agent igr_wr_req_agent;
   smm_bfm_row_rd_req_agent egr_rd_req_agent;


   // VARIABLE: gmm_wr_req_agent, mce_wr_req_agent 
   // These are the Mesh BFM Agent Instances interfacing with the GMM and MCE respectively for Write Request Transactions.
   //smm_bfm_col_wr_req_agent gmm_wr_req_agent;
   //smm_bfm_col_wr_req_agent mce_wr_req_agent;

   // VARIABLE: gmm_rd_req_agent, mce_rd_req_agent 
   // These are the Mesh BFM Agent Instances interfacing with the GMM and MCE respectively for Read Request/Responses Transactions.
   //smm_bfm_col_rd_req_agent gmm_rd_req_agent;
   //smm_bfm_col_rd_req_agent mce_rd_req_agent;

   smm_bfm_mem_node  mem_mesh[NUM_MSH_ROWS-1:0][NUM_MSH_COLS-1:0];
   smm_bfm_mwr_req   mwr_req;
   smm_bfm_mrd_req   mrd_req;
   
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
      igr_wr_req_agent = smm_bfm_row_wr_req_agent::type_id::create("igr_wr_req_agent", this);
      uvm_config_db#(mby_base_config)::set(this, "igr_wr_req_agent", "cfg_obj", igr_wr_req_cfg_obj);
      igr_wr_req_agent.cfg_obj = this.igr_wr_req_cfg_obj;
      igr_wr_req_agent.cfg_obj.monitor_active = UVM_ACTIVE;
      igr_wr_req_agent.cfg_obj.driver_active = UVM_ACTIVE;
      
      egr_rd_req_agent = smm_bfm_row_rd_req_agent::type_id::create("egr_rd_req_agent", this);
      uvm_config_db#(mby_base_config)::set(this, "egr_rd_req_agent", "cfg_obj", egr_rd_req_cfg_obj);
      egr_rd_req_agent.cfg_obj = this.egr_rd_req_cfg_obj;
      egr_rd_req_agent.cfg_obj.monitor_active = UVM_ACTIVE;
      egr_rd_req_agent.cfg_obj.driver_active = UVM_ACTIVE;
      
      for(int row_idx=0 ; row_idx<NUM_MSH_ROWS; row_idx++) begin
         for(int col_idx=0 ; col_idx<NUM_MSH_COLS; col_idx++) begin
            mem_mesh[row_idx][col_idx] = smm_bfm_mem_node::type_id::create($sformatf("mem_node%d%d",row_idx,col_idx), this);
         end
      end

      mwr_req         = smm_bfm_mwr_req::type_id::create("mwr_req", this);
      mwr_req.set_mesh_ptr(mem_mesh);       
      mrd_req         = smm_bfm_mrd_req::type_id::create("mrd_req", this);
      mrd_req.set_mesh_ptr(mem_mesh);       

      mrd_req.set_agent_ptr(egr_rd_req_agent);
   endfunction : build_phase

   // ------------------------------------------------------------------------
   // FUNCTION: connect_phase
   //
   // Connecting subscribed component to monitor analysis port
   //
   // ------------------------------------------------------------------------
   function void connect_phase(uvm_phase phase);
      super.connect_phase(phase);
      igr_wr_req_agent.monitor.mon_ap.connect(mwr_req.analysis_export);
      egr_rd_req_agent.monitor.mon_ap.connect(mrd_req.analysis_export);
    endfunction : connect_phase

endclass : mby_smm_bfm
`endif
