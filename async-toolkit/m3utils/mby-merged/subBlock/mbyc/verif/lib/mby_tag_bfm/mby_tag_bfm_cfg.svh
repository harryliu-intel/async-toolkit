//-----------------------------------------------------------------------------
// Title         : Madison Bay Tag Bus Functional Model Configuration Class
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : mby_tag_bfm_cfg.svh
// Author        : jose.j.godinez.carrillo  <jjgodine@ichips.intel.com>
// Created       : 01.11.2018
//-----------------------------------------------------------------------------
// Description :
// This is the configuration class of the tag bfm
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
`ifndef __MBY_TAG_BFM_PKG__
`error "Attempt to include file outside of mby_tag_bfm_pkg."
`endif
`ifndef __MBY_TAG_BFM_CFG__
`define __MBY_TAG_BFM_CFG__
//-----------------------------------------------------------------------------
// CLASS: mby_tag_bfm_cfg
//
// This is the configuration class used by the tag bfm. It contains fields to
// control the tag agent's driver/monitor behavior and also to control the
// frame generator capabilities. This class needs to be randomized specifying
// the mode of operation (IGR or EGR), the rest of the fields will be
// constrained based off of that. E.g.
// (start code)
//    tag_bfm_cfg.randomize() with { bfm_mode == TAG_BFM_IGR_MODE; }
// (end)
//-----------------------------------------------------------------------------
class mby_tag_bfm_cfg extends mby_base_config;

   // VARIABLE: frame_gen_active
   // Agent is configured to be active or passive
   uvm_active_passive_enum frame_gen_active;

   // VARIABLE: bfm_mode
   // The tag bfm can be configured to be in ingress or egress modes.
   mby_tag_bfm_mode_t bfm_mode;

   // CONSTRAINT: ingress_constraint
   // Sets proper values for driver/monitor enables when operating in ingress mode
   constraint ingress_constraint {
      if (bfm_mode == TAG_BFM_IGR_MODE) {
         monitor_active   == UVM_ACTIVE;
         driver_active    == UVM_PASSIVE;
         frame_gen_active == UVM_PASSIVE;
      } else {
         monitor_active   == UVM_ACTIVE;
         driver_active    == UVM_ACTIVE;
         frame_gen_active == UVM_ACTIVE;
      }
   }

   // UVM object utils macro
   `uvm_object_utils(mby_tag_bfm_cfg)

   // -------------------------------------------------------------------------
   // CONSTRUCTOR: new
   //
   // Constructor
   //
   // ARGUMENTS:
   //    string name - An identifier for this configuration object.
   // -------------------------------------------------------------------------
   function new(string name = "mby_tag_bfm_cfg");
      super.new(name);
   endfunction : new

endclass : mby_tag_bfm_cfg
`endif
