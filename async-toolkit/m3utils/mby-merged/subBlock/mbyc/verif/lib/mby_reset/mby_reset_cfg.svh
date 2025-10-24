// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

//-----------------------------------------------------------------------------
// Title         : Madison Bay Tag Bus Functional Model Configuration Class
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : mby_tag_bfm_cfg.svh
// Author        : jesus.a.lopez.chin  <jesus.a.lopez.chin@intel.com>
// Created       : 01.17.2019
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
// When operating in egress mode, the traffic_mode must be defined too (it
// can be set to either uni-cast or multi-cast). E.g. for multi-cast:
// (start code)
//    tag_bfm_cfg.randomize() with {
//       bfm_mode     == TAG_BFM_EGR_MODE;
//       traffic_mode == TAG_BFM_MC_MODE;
//    }
// (end)
//
//-----------------------------------------------------------------------------
class mby_reset_cfg extends mby_base_pkg::mby_base_config;

   // VARIABLE: reset_mode
   // The reset_mode can be configured to activate/deactivate reset monitors and drivers
   rand mby_reset_modes reset_mode;

   // VARIABLE: driver_active
   // Agent is configured to be active or passive
   // TODO: add "_is_" e.g. driver_is_active
   rand uvm_active_passive_enum driver_active;

   // VARIABLE: monitor_active
   // Agent is configured to be active or passive
   rand uvm_active_passive_enum monitor_active;


   // CONSTRAINT: ingress_constraint
   // Sets proper values for driver/monitor enables
   // depending on the CTE or FC level
   constraint reset_constraint {
      if(reset_mode == RESET_EGR_MODE || reset_mode == RESET_IGR_MODE){
         monitor_active == UVM_ACTIVE;
         driver_active  == UVM_ACTIVE;
      }else {
         monitor_active == UVM_ACTIVE;
         driver_active  == UVM_PASSIVE;
      }
   }

   // UVM object utils macro
   `uvm_object_utils(mby_reset_cfg)

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

endclass : mby_reset_cfg
