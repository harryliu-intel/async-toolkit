//-----------------------------------------------------------------------------
// Title         : Madison Bay SMM Bus Functional Model Configuration Class
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : mby_smm_bfm_cfg.svh
// Author        : Roman Bernal  <r.bernal@intel.com>
// Created       : 01.11.2018
//-----------------------------------------------------------------------------
// Description :
// This is the configuration class of the smm_bfm
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
`ifndef __MBY_SMM_BFM_CFG__
`define __MBY_SMM_BFM_CFG__
//-----------------------------------------------------------------------------
// CLASS: mby_smm_bfm_cfg
//
// This is the configuration class used by the smm_bfm. It contains fields to
// control the smm agent's driver/monitor behavior and to set up congestion profiles
// for the smm_bfm.
//-----------------------------------------------------------------------------
class mby_smm_bfm_cfg extends mby_base_config;
   // TODO : once global config gets enabled the constraints for profile randomization will be applied, in the meantime
   //        only the default values will be used owing to no profile selection will take place.

   rand delay_type_e delay_profile;

   // VARIABLE: mrd_req_rsp_delay_min
   //    Defines the lower limit for memory read request/response delay randomization.
   int mrd_req_rsp_delay_min = 8;
   
   // VARIABLE: mrd_req_rsp_delay_max
   //    Defines the upper limit for memory read request/response delay randomization.
   int mrd_req_rsp_delay_max = 64;
   
   // CONSTRAINT: smm_bfm constraint
   // Sets different delay ranges based on the selected delay profile
   // TODO : Values were arbitrarily chosen, check spec to update accordingly.
   constraint delay_profiles_constraint {
      if (delay_profile == IDEAL_DELAY) {
         mrd_req_rsp_delay_min == 8;
         mrd_req_rsp_delay_max == 64;
      } else if (delay_profile == MEDIUM_DELAY) {
         mrd_req_rsp_delay_min == 32;
         mrd_req_rsp_delay_max == 128;
      } else if (delay_profile == HIGH_DELAY) {
         mrd_req_rsp_delay_min == 48;
         mrd_req_rsp_delay_max == 256;
      }
   }

   // CONSTRAINT: smm_bfm_constraint
   // Sets proper values for driver/monitor enables
   constraint egress_constraint {
      monitor_active   == UVM_ACTIVE;
      driver_active    == UVM_ACTIVE;
   }

   `uvm_object_utils(mby_smm_bfm_cfg)

   // -------------------------------------------------------------------------
   // CONSTRUCTOR: new
   //
   // Constructor
   //
   // ARGUMENTS:
   //    string name - An identifier for this configuration object.
   // -------------------------------------------------------------------------
   function new(string name = "mby_smm_bfm_cfg");
      super.new(name);
   endfunction : new

endclass : mby_smm_bfm_cfg
`endif
