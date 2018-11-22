//-----------------------------------------------------------------------------
// Title         : Madison Bay REM_APP Bus Functional Model Configuration Class
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : mby_rem_app_cfg.svh
// Author        : jose.j.godinez.carrillo  <jjgodine@ichips.intel.com>
// Created       : 01.11.2018
//-----------------------------------------------------------------------------
// Description :
// This is the configuration class of the rem_app
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
`ifndef __MBY_REM_APP_PKG__
`error "Attempt to include file outside of mby_rem_app_pkg."
`endif
`ifndef __MBY_REM_APP_CFG__
`define __MBY_REM_APP_CFG__
//-----------------------------------------------------------------------------
// CLASS: mby_rem_app_cfg
//
// This is the configuration class used by the rem_app. It contains fields to
// control the gcm agent's driver/monitor behavior and also to control the
// frame generator capabilities.
//
//-----------------------------------------------------------------------------
class mby_rem_app_cfg extends mby_base_config;

   // VARIABLE: frame_gen_active
   // Agent is configured to be active or passive
   uvm_active_passive_enum frame_gen_active;

   // UVM object utils macro
   `uvm_object_utils(mby_rem_app_cfg)

   // -------------------------------------------------------------------------
   // CONSTRUCTOR: new
   //
   // Constructor
   //
   // ARGUMENTS:
   //    string name - An identifier for this configuration object.
   // -------------------------------------------------------------------------
   function new(string name = "mby_rem_app_cfg");
      super.new(name);
   endfunction : new

endclass : mby_rem_app_cfg
`endif
