//-----------------------------------------------------------------------------
// Title         : Madison Bay PBR Bus Functional Model Configuration Class
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : mby_pbr_bfm_cfg.svh
// Author        : ricardo.a.alfaro.gomez  <raalfaro@ichips.intel.com>
// 2ry contact   : jose.j.godinez.carrillo  <jjgodine@ichips.intel.com>
// Created       : 12.19.2018
//-----------------------------------------------------------------------------
// Description :
// This is the configuration class of the pbr_bfm
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
`ifndef __MBY_PBR_BFM_CFG__
`define __MBY_PBR_BFM_CFG__
//-----------------------------------------------------------------------------
// CLASS: mby_pbr_bfm_cfg
//
// This is the configuration class used by the pbr_bfm. It contains fields to
// control the gcm agent's driver/monitor behavior and also to control the
// frame generator capabilities.
//
//-----------------------------------------------------------------------------
class mby_pbr_bfm_cfg extends shdv_base_config;

   // VARIABLE: driver_is_active
   // Agent is configured to be active or passive
   rand uvm_active_passive_enum dpb_driver_is_active;
   rand uvm_active_passive_enum dpm_driver_is_active;
   rand uvm_active_passive_enum csp_driver_is_active;
   rand uvm_active_passive_enum cpb_driver_is_active;

   // VARIABLE: monitor_is_active
   // Agent is configured to be active or passive
   rand uvm_active_passive_enum dpb_monitor_is_active;
   rand uvm_active_passive_enum dpm_monitor_is_active;
   rand uvm_active_passive_enum csp_monitor_is_active;
   rand uvm_active_passive_enum cpb_monitor_is_active;

   // VARIABLE: bfm_mode
   // This is the PBR bfm mode of operation (igr/egr).
   rand mby_pbr_bfm_mode_t bfm_mode;

   // CONSTRAINT: pbr_mode_constraint
   // Sets the dpb/dpm/csp/cpb agent's configuration settings based
   // on the PBR's bfm_mode
   constraint pbr_mode_constraint {
      if(bfm_mode == PBR_BFM_IGR_MODE) {
         dpb_driver_is_active  == UVM_ACTIVE;
         dpb_monitor_is_active == UVM_ACTIVE;
         dpm_driver_is_active  == UVM_PASSIVE;
         dpm_monitor_is_active == UVM_PASSIVE;
         csp_driver_is_active  == UVM_PASSIVE;
         csp_monitor_is_active == UVM_PASSIVE;
         cpb_driver_is_active  == UVM_ACTIVE;
         cpb_monitor_is_active == UVM_ACTIVE;
      } else if(bfm_mode == PBR_BFM_EGR_MODE) {
         dpb_driver_is_active  == UVM_PASSIVE;
         dpb_monitor_is_active == UVM_PASSIVE;
         dpm_driver_is_active  == UVM_ACTIVE;
         dpm_monitor_is_active == UVM_ACTIVE;
         csp_driver_is_active  == UVM_ACTIVE;
         csp_monitor_is_active == UVM_ACTIVE;
         cpb_driver_is_active  == UVM_PASSIVE;
         cpb_monitor_is_active == UVM_PASSIVE;
      }
   }

   // UVM object utils macro
   `uvm_object_utils(mby_pbr_bfm_cfg)

   // -------------------------------------------------------------------------
   // CONSTRUCTOR: new
   //
   // Constructor
   //
   // ARGUMENTS:
   //    string name - An identifier for this configuration object.
   // -------------------------------------------------------------------------
   function new(string name = "mby_pbr_bfm_cfg");
      super.new(name);
   endfunction : new

endclass : mby_pbr_bfm_cfg
`endif
