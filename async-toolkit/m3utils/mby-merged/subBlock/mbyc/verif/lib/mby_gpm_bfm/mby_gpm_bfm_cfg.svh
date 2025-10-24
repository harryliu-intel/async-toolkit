// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

//-----------------------------------------------------------------------------
// Title         : Madison Bay GPM Bus Functional Model Configuration Class
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : mby_gpm_bfm_cfg.svh
// Author        : jose.j.godinez.carrillo  <jjgodine@ichips.intel.com>
// Created       : 01.11.2018
//-----------------------------------------------------------------------------
// Description :
// This is the configuration class of the gpm_bfm
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
`ifndef __MBY_GPM_BFM_PKG__
`error "Attempt to include file outside of mby_gpm_bfm_pkg."
`endif
`ifndef __MBY_GPM_BFM_CFG__
`define __MBY_GPM_BFM_CFG__
//-----------------------------------------------------------------------------
// CLASS: mby_gpm_bfm_cfg
//
// This is the configuration class used by the gpm_bfm. It contains fields to
// control the gcm agent's driver/monitor behavior and also to control the
// frame generator capabilities.
//
//-----------------------------------------------------------------------------
class mby_gpm_bfm_cfg extends shdv_base_config;


   // VARIABLE: fpptr_cfg
   // Basic configuration object for the free pod pointer agent.
   rand shdv_base_agent_config fpptr_cfg;

   // VARIABLE: dpptr_cfg
   // Basic configuration object for the dirty pod pointer agent.
   rand shdv_base_agent_config dpptr_cfg;

   // VARIABLE: bfm_mode
   // This is the GPM bfm mode of operation (igr/egr).
   rand mby_gpm_bfm_mode_t bfm_mode;

   // CONSTRAINT: gpm_mode_constraint
   // Sets the fpptr/dpptr agent's configuration settings based
   // on the GPM's bfm_mode
   constraint gpm_mode_constraint {
      if(bfm_mode == GPM_BFM_IGR_MODE) {
         fpptr_cfg.driver_enable  == UVM_ACTIVE;
         fpptr_cfg.monitor_enable == UVM_ACTIVE;
         dpptr_cfg.driver_enable  == UVM_PASSIVE;
         dpptr_cfg.monitor_enable == UVM_ACTIVE;
      } else if(bfm_mode == GPM_BFM_EGR_MODE) {
         fpptr_cfg.driver_enable  == UVM_PASSIVE;
         fpptr_cfg.monitor_enable == UVM_PASSIVE;
         dpptr_cfg.driver_enable  == UVM_PASSIVE;
         dpptr_cfg.monitor_enable == UVM_PASSIVE;
      } 
   }

   // UVM object utils macro
   `uvm_object_utils(mby_gpm_bfm_cfg)

   // -------------------------------------------------------------------------
   // CONSTRUCTOR: new
   //
   // Constructor
   //
   // ARGUMENTS:
   //    string name - An identifier for this configuration object.
   // -------------------------------------------------------------------------
   function new(string name = "mby_gpm_bfm_cfg");
      super.new(name);
      this.fpptr_cfg = new("fpptr_cfg");
      this.dpptr_cfg = new("dpptr_cfg");
   endfunction : new

endclass : mby_gpm_bfm_cfg
`endif
