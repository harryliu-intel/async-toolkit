//-----------------------------------------------------------------------------
// Title         : Madison Bay GMM Bus Functional Model
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : mby_gmm_bfm.svh
// Author        : jose.j.godinez.carrillo  <jjgodine@ichips.intel.com>
// Created       : 01.11.2018
//-----------------------------------------------------------------------------
// Description :
// This is the main gmm_bfm class
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
`ifndef __MBY_GMM_BFM_PKG__
`error "Attempt to include file outside of mby_gmm_bfm_pkg."
`endif
`ifndef __MBY_GMM_BFM__
`define __MBY_GMM_BFM__
//-----------------------------------------------------------------------------
// CLASS: mby_gmm_bfm
//
// This is the main gmm_bfm class, it is just a container that instantiates and
// connects the pod pointer generator, the free pointer agent, the dirty pointer
// agent and the mesh agent.
//
//-----------------------------------------------------------------------------
class mby_gmm_bfm extends uvm_component;

   // VARIABLE: cfg_obj
   // The bfm's configuration object
   mby_gmm_bfm_cfg cfg_obj;

   // VARIBLE: pptr_gen
   // This is the pod pointer generator class.
   mby_gmm_bfm_pptr_gen pptr_gen;

   // VARIABLE: fpptr_agent
   // This is the free pointer agent that will be used in ingress and mesh modes
   // to send new pod pointers for ingress consumption.
   pod_agent fpptr_agent;

   // VARIABLE: dpptr_agent
   // This is the dirty pointer agent that will be used in egress and mesh modes
   // and will receive dirty pointer information from egress.
   pod_agent dpptr_agent;

   // VARIABLE: mesh_agent
   // This is the mesh agent that is used in mesh mode to access the mesh intf.
   // This agent is not used when the bfm is configured in ingress or egress
   // modes.
   msh_agent mesh_agent;

   // -------------------------------------------------------------------------
   // Macro to register new class type
   // -------------------------------------------------------------------------
   `uvm_component_param_utils_begin(mby_gmm_bfm)
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
   // The free, dirty and mesh agents are created and the configuration object
   // is assigned to them.
   //
   //
   // ------------------------------------------------------------------------
   function void build_phase(uvm_phase phase);
      super.build_phase(phase);
      pptr_gen = mby_gmm_bfm_pptr_gen::type_id::create("pptr_gen", this);
      pptr_gen.cfg_obj = this.cfg_obj;
      if(cfg_obj.bfm_mode == GMM_BFM_IGR_MODE) begin
         fpptr_agent = pod_agent::type_id::create("fpptr_agent", this);
         fpptr_agent.cfg_obj = this.cfg_obj.fpptr_cfg;
      end else if(cfg_obj.bfm_mode == GMM_BFM_EGR_MODE) begin
         dpptr_agent = pod_agent::type_id::create("dpptr_agent", this);
         dpptr_agent.cfg_obj = this.cfg_obj.dpptr_cfg;
      end else if(cfg_obj.bfm_mode == GMM_BFM_MSH_MODE) begin
         mesh_agent = msh_agent::type_id::create("mesh_agent", this);
         mesh_agent.cfg_obj = this.cfg_obj.msh_cfg;
      end
   endfunction

   // ------------------------------------------------------------------------
   // FUNCTION: connect_phase
   //
   // TODO: Connect the pptr_gen to the corresponding agent based on the bfm
   //       mode.
   //
   // ------------------------------------------------------------------------
   function void connect_phase(uvm_phase phase);
      super.connect_phase(phase);
      if(cfg_obj.bfm_mode == GMM_BFM_IGR_MODE) begin
         // TODO: connect the pptr_gen to the fpptr_agent
      end else if(cfg_obj.bfm_mode == GMM_BFM_EGR_MODE) begin
         // TODO: connect the pptr_gen to the dpptr_agent
      end else if(cfg_obj.bfm_mode == GMM_BFM_MSH_MODE) begin
         // TODO: connect the pptr_gen to fpptr_agent and dpptr_agent
      end
   endfunction

endclass : mby_gmm_bfm
`endif
