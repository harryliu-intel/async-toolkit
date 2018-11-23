//-----------------------------------------------------------------------------
// Title         : Madison Bay GCM Bus Functional Model Configuration Class
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : mby_gcm_bfm_cfg.svh
// Author        : jose.j.godinez.carrillo  <jjgodine@ichips.intel.com>
// Created       : 01.11.2018
//-----------------------------------------------------------------------------
// Description :
// This is the configuration class of the gcm_bfm
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
`ifndef __MBY_GCM_BFM_PKG__
`error "Attempt to include file outside of mby_gcm_bfm_pkg."
`endif
`ifndef __MBY_GCM_BFM_CFG__
`define __MBY_GCM_BFM_CFG__
//-----------------------------------------------------------------------------
// CLASS: mby_gcm_bfm_cfg
//
// This is the configuration class used by the gcm_bfm. It contains fields to
// control the GCM agent's driver/monitor behavior.
//
//-----------------------------------------------------------------------------
class mby_gcm_bfm_cfg extends mby_base_config;

   // VARIABLE: bfm_mode
   // This is the GCM bfm mode of operation (igr/egr).
   rand mby_gcm_bfm_mode_t bfm_mode;

   // VARIABLE: queue_cfg
   // Basic configuration object for the queue agent.
   rand mby_base_config queue_cfg;

   // VARIABLE: rx_wm_cfg
   // Basic configuration object for the rx_wm agent.
   rand mby_base_config rx_wm_cfg;

   // VARIABLE: rx_sm_wm_cfg
   // Basic configuration object for the rx_sm_wm agent.
   rand mby_base_config rx_sm_wm_cfg;

   // VARIABLE: deque_cfg
   // Basic configuration object for the deque agent.
   rand mby_base_config deque_cfg;

   // VARIABLE: tx_sm_wm_cfg
   // Basic configuration object for the tx_sm_wm agent.
   rand mby_base_config tx_sm_wm_cfg;

   // CONSTRAINT: gmm_mode_constraint
   // Sets the agents' configuration settings based
   // on the GCM's bfm_mode
   constraint gcm_bfm_mode_constraint {
      if(bfm_mode == GCM_BFM_IGR_MODE) {
         queue_cfg.driver_active     == UVM_PASSIVE;
         queue_cfg.monitor_active    == UVM_ACTIVE;
         rx_wm_cfg.driver_active     == UVM_ACTIVE;
         rx_wm_cfg.monitor_active    == UVM_ACTIVE;
         rx_sm_wm_cfg.driver_active  == UVM_ACTIVE;
         rx_sm_wm_cfg.monitor_active == UVM_ACTIVE;
      } else if(bfm_mode == GCM_BFM_EGR_MODE) {
         deque_cfg.driver_active     == UVM_PASSIVE;
         deque_cfg.monitor_active    == UVM_ACTIVE;
         tx_sm_wm_cfg.driver_active  == UVM_ACTIVE;
         tx_sm_wm_cfg.monitor_active == UVM_ACTIVE;
      }
   }

   // UVM object utils macro
   `uvm_object_utils(mby_gcm_bfm_cfg)

   // -------------------------------------------------------------------------
   // CONSTRUCTOR: new
   //
   // Constructor
   //
   // ARGUMENTS:
   //    string name - An identifier for this configuration object.
   // -------------------------------------------------------------------------
   function new(string name = "mby_gcm_bfm_cfg");
      super.new(name);
      this.queue_cfg    = new("queue_cfg");
      this.deque_cfg    = new("deque_cfg");
      this.rx_wm_cfg    = new("rx_wm_cfg");
      this.rx_sm_wm_cfg = new("rx_sm_wm_cfg");
      this.tx_sm_wm_cfg = new("tx_sm_wm_cfg");
   endfunction : new

endclass : mby_gcm_bfm_cfg
`endif
