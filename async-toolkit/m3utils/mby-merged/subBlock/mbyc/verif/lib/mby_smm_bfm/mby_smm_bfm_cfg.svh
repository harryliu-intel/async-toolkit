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
// control the smm agent's driver/monitor behavior.
//-----------------------------------------------------------------------------
class mby_smm_bfm_cfg extends mby_base_config;
   // TODO ;   WIP: Mesh Configuration Options - Profile based latencies.
   //          Different delay types: good condition, congestion levels (low, medium, high, super high), write lost,
   
   // TODO :   add plusargs to let the test select the profile.

   rand delay_type_t delay_profile;

   // VARIABLE: mrd_req_rsp_delay_min
   //    Defines the lower limit for memory read request/response delay randomization.
   int mrd_req_rsp_delay_min     = 8;
   
   // VARIABLE: mrd_req_rsp_delay_max
   //    Defines the upper limit for memory read request/response delay randomization.
   int mrd_req_rsp_delay_max     = 64;
   
   // VARIABLE: mrd_req_rsp_delay_extra
   //    Defines the extra clocks added to memory read request/response delay randomization.
   rand int mrd_req_rsp_delay_extra;
   
   // CONSTRAINT: smm_bfm constraint
   // Sets different delay profile values
   constraint delay_profiles_c {
      (delay_profile == IDEAL) -> (mrd_req_rsp_delay_extra == 0);
      (delay_profile == LOW_DELAY) -> (mrd_req_rsp_delay_extra inside {[1:10]});
      (delay_profile == MEDIUM_DELAY) -> (mrd_req_rsp_delay_extra inside {[11:100]});
      (delay_profile == HIGH_DELAY) -> (mrd_req_rsp_delay_extra inside {[101:1000]});
      (delay_profile == INSANE_DELAY) -> (mrd_req_rsp_delay_extra inside {[1001:10000]});
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
