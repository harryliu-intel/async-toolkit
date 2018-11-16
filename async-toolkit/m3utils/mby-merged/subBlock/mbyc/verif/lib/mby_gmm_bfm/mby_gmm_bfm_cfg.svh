//-----------------------------------------------------------------------------
// Title         : Madison Bay GMM Bus Functional Model Configuration Class
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : mby_gmm_bfm_cfg.svh
// Author        : jose.j.godinez.carrillo  <jjgodine@ichips.intel.com>
// Created       : 01.11.2018
//-----------------------------------------------------------------------------
// Description :
// This is the configuration class of the gmm_bfm
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
`ifndef __MBY_GMM_BFM_CFG__
`define __MBY_GMM_BFM_CFG__
//-----------------------------------------------------------------------------
// CLASS: mby_gmm_bfm_cfg
//
// This is the configuration class used by the gmm_bfm. It contains fields to
// control the gcm agent's driver/monitor behavior and also to control the
// frame generator capabilities.
//
//-----------------------------------------------------------------------------
class mby_gmm_bfm_cfg extends mby_base_config;

   // VARIABLE: msh_cfg
   // Basic configuration object for the mesh agent.
   rand mby_base_config msh_cfg;

   // VARIABLE: fpptr_cfg
   // Basic configuration object for the free pod pointer agent.
   rand mby_base_config fpptr_cfg;

   // VARIABLE: dpptr_cfg
   // Basic configuration object for the dirty pod pointer agent.
   rand mby_base_config dpptr_cfg;

   // VARIABLE: bfm_mode
   // This is the GMM bfm mode of operation (igr/egr/msh).
   rand mby_gmm_bfm_mode_t bfm_mode;

   // CONSTRAINT: gmm_mode_constraint
   // Sets the fpptr/dpptr/msh agent's configuration settings based
   // on the GMM's bfm_mode
   constraint gmm_mode_constraint {
      if(bfm_mode == GMM_BFM_IGR_MODE) {
         fpptr_cfg.driver_active  == UVM_ACTIVE;
         fpptr_cfg.monitor_active == UVM_ACTIVE;
         dpptr_cfg.driver_active  == UVM_PASSIVE;
         dpptr_cfg.monitor_active == UVM_PASSIVE;
         msh_cfg.driver_active    == UVM_PASSIVE;
         msh_cfg.monitor_active   == UVM_PASSIVE;
      } else if(bfm_mode == GMM_BFM_EGR_MODE) {
         fpptr_cfg.driver_active  == UVM_PASSIVE;
         fpptr_cfg.monitor_active == UVM_PASSIVE;
         dpptr_cfg.driver_active  == UVM_PASSIVE;
         dpptr_cfg.monitor_active == UVM_ACTIVE;
         msh_cfg.driver_active    == UVM_PASSIVE;
         msh_cfg.monitor_active   == UVM_PASSIVE;
      } else if(bfm_mode == GMM_BFM_MSH_MODE) {
         fpptr_cfg.driver_active  == UVM_ACTIVE;
         fpptr_cfg.monitor_active == UVM_ACTIVE;
         dpptr_cfg.driver_active  == UVM_PASSIVE;
         dpptr_cfg.monitor_active == UVM_ACTIVE;
         msh_cfg.driver_active    == UVM_ACTIVE;
         msh_cfg.monitor_active   == UVM_ACTIVE;
      }
   }

   // UVM object utils macro
   `uvm_object_utils(mby_gmm_bfm_cfg)

   // -------------------------------------------------------------------------
   // CONSTRUCTOR: new
   //
   // Constructor
   //
   // ARGUMENTS:
   //    string name - An identifier for this configuration object.
   // -------------------------------------------------------------------------
   function new(string name = "mby_gmm_bfm_cfg");
      super.new(name);
      this.fpptr_cfg = new("fpptr_cfg");
      this.dpptr_cfg = new("dpptr_cfg");
      this.msh_cfg   = new("msh_cfg");
   endfunction : new

endclass : mby_gmm_bfm_cfg
`endif
