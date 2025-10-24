// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

//-----------------------------------------------------------------------------
// Title         : Madison Bay PBR Bus Functional Model
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : mby_pbr_bfm.svh
// Author        : ricardo.a.alfaro.gomez  <raalfaro@ichips.intel.com>
// 2ry contact   : jose.j.godinez.carrillo  <jjgodine@ichips.intel.com>
// Created       : 12.19.2018
//-----------------------------------------------------------------------------
// Description :
// This is the main pbr_bfm class
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
`ifndef __MBY_PBR_BFM_PKG__
`error "Attempt to include file outside of mby_pbr_bfm_pkg."
`endif
`ifndef __MBY_PBR_BFM__
`define __MBY_PBR_BFM__
//-----------------------------------------------------------------------------
// CLASS: mby_pbr_bfm
//
// This is the main pbr_bfm class, it is just a container that instantiates and
// connects the dirty pointer broker agent, the dirty pointer manager agent and
// the clean segment pointer agent.
//
//-----------------------------------------------------------------------------
class mby_pbr_bfm extends uvm_component;

   // VARIABLE: cfg_obj
   // The bfm's configuration object
   mby_pbr_bfm_cfg cfg_obj;

   // VARIABLE: dpb_agent
   // TODO: description
   mby_pbr_bfm_dpb_agent dpb_agent;

   // VARIABLE: dpm_agent
   // TODO: description
   mby_pbr_bfm_dpm_agent dpm_agent;

   // VARIABLE: csp_agent
   // TODO: description
   mby_pbr_bfm_csp_agent csp_agent;

   // VARIABLE: cpb_agent
   // TODO: description
   mby_pbr_bfm_cpb_agent cpb_agent;

   mby_pbr_bfm_cptr_sub cptr_req;
   mby_pbr_bfm_dptr_sub dptr_req;

   // -------------------------------------------------------------------------
   // Macro to register new class type
   // -------------------------------------------------------------------------
   `uvm_component_param_utils_begin(mby_pbr_bfm)
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
      cfg_obj = mby_pbr_bfm_cfg::type_id::create("cfg_obj", this);
   endfunction : new

   // ------------------------------------------------------------------------
   // FUNCTION: build_phase
   // The dpb, dpm and csp agents are created and the configuration object
   // is assigned to them.
   // When operating in ingress mode, it models the egress' dirty pointer
   // broker module.
   // When operating in egress mode it models the ingress clean segment
   // pointer cache that sends read requests to the egress' clean pointer
   // broker module, it also models the ingress' dirty pod manager.
   // ------------------------------------------------------------------------
   function void build_phase(uvm_phase phase);
      
      super.build_phase(phase);
      // ----------------------------------------------------------------------
      // Creating the proper pbr_bfm_agent based on the igr/egr mode and
      // assigning configuration object
      // ----------------------------------------------------------------------
      if(cfg_obj.bfm_mode == PBR_BFM_IGR_MODE) begin
         
         cpb_agent = mby_pbr_bfm_cpb_agent::type_id::create("cpb_agent", this);
         cpb_agent.cfg = new("cpb_agent_cfg");
         cpb_agent.cfg.monitor_enable = this.cfg_obj.cpb_monitor_is_active;
         cpb_agent.cfg.driver_enable = this.cfg_obj.cpb_driver_is_active;

         dpb_agent = mby_pbr_bfm_dpb_agent::type_id::create("dpb_agent", this);
         dpb_agent.cfg = new("dpb_agent_cfg");
         dpb_agent.cfg.monitor_enable = this.cfg_obj.dpb_monitor_is_active;
         dpb_agent.cfg.driver_enable = this.cfg_obj.dpb_driver_is_active;

         cptr_req = mby_pbr_bfm_cptr_sub::type_id::create("cptr_req", this);
         cptr_req.set_agent_ptr(cpb_agent);
         `uvm_info(get_name(), ("DBG_ALF: Done building pbr bfm: IGR MODE"), UVM_LOW);
      end else if(cfg_obj.bfm_mode == PBR_BFM_EGR_MODE) begin
         csp_agent = mby_pbr_bfm_csp_agent::type_id::create("csp_agent", this);
         csp_agent.cfg = new("csp_agent_cfg");
         csp_agent.cfg.monitor_enable = this.cfg_obj.csp_monitor_is_active;
         csp_agent.cfg.driver_enable = this.cfg_obj.csp_driver_is_active;

         dpm_agent = mby_pbr_bfm_dpm_agent::type_id::create("dpm_agent", this);
         dpm_agent.cfg = new("dpm_agent_cfg");
         dpm_agent.cfg.monitor_enable = this.cfg_obj.dpm_monitor_is_active;
         dpm_agent.cfg.driver_enable = this.cfg_obj.dpm_driver_is_active;

         dptr_req = mby_pbr_bfm_dptr_sub::type_id::create("dptr_req", this);
         dptr_req.set_agent_ptr(dpm_agent);
         `uvm_info(get_name(), ("DBG_ALF: Done building pbr bfm: EGR MODE"), UVM_LOW);

      end
   endfunction

   // ------------------------------------------------------------------------
   // FUNCTION: connect_phase
   // ------------------------------------------------------------------------
   function void connect_phase(uvm_phase phase);
      super.connect_phase(phase);
      if(cfg_obj.bfm_mode == PBR_BFM_IGR_MODE) begin
         cpb_agent.monitor.mon_ap.connect(cptr_req.analysis_export);
      end else if(cfg_obj.bfm_mode == PBR_BFM_EGR_MODE) begin
         dpm_agent.monitor.mon_ap.connect(dptr_req.analysis_export);
      end
   endfunction

endclass : mby_pbr_bfm
`endif
