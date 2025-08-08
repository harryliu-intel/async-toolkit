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
class mby_smm_bfm_cfg extends shdv_base_config;
   // TODO : once global config gets enabled the constraints for profile randomization will be applied, in the meantime
   //        only the default values will be used owing to no profile selection will take place.
   // TODO : Change delay modeling as a function of node_row/node_col coordinates, and traffic class (higher priority
   //        should take less time to get), correlate memory read delays to accesses associated to the a common segment
   //        (e. g. words within a segment are retrieved from different nodes from the same row, so memory reads should
   //        take a similar amount of time to get completed)
   // TODO : Consider back pressure modeling to reject memory write requests.
   // TODO : Model a separate plane for multicast accesses handling.

   // VARIABLE: mwr_req_agent_cfg
   // Basic configuration object for the memory write agent.
   rand shdv_base_agent_config mwr_req_agent_cfg;

   // VARIABLE: mrd_req_agent_cfg
   // Basic configuration object for the memory read agent.
   rand shdv_base_agent_config mrd_req_agent_cfg;

   // VARIABLE: delay_profile
   //    Profile selection for delay modeling on memory reads or writes.
   rand delay_type_e delay_profile;

   // VARIABLE: mrd_req_rsp_delay_min
   //    Defines the lower limit for memory read request/response delay randomization.
   int mrd_req_delay_min = 8;
   
   // VARIABLE: mrd_req_rsp_delay_max
   //    Defines the upper limit for memory read request/response delay randomization.
   int mrd_req_delay_max = 64;
   
   // VARIABLE: mwr_req_rsp_delay_min
   //    Defines the lower limit for memory write request delay randomization.
   int mwr_req_delay_min = 4;
   
   // VARIABLE: mrd_req_rsp_delay_max
   //    Defines the upper limit for memory write request delay randomization.
   int mwr_req_delay_max = 32;
   
   // CONSTRAINT: smm_bfm constraint
   // Sets different delay ranges based on the selected delay profile
   // TODO : Values were arbitrarily chosen, check spec to update accordingly.
   constraint delay_profiles_constraint {
      if (delay_profile == IDEAL_DELAY) {
         mrd_req_delay_min == 8;
         mrd_req_delay_max == 64;
         mwr_req_delay_min == 4;
         mwr_req_delay_max == 32;
      } else if (delay_profile == MEDIUM_DELAY) {
         mrd_req_delay_min == 32;
         mrd_req_delay_max == 128;
         mwr_req_delay_min == 16;
         mwr_req_delay_max == 64;
      } else if (delay_profile == HIGH_DELAY) {
         mrd_req_delay_min == 48;
         mrd_req_delay_max == 256;
         mwr_req_delay_min == 24;
         mwr_req_delay_max == 128;
      }
   }

   // CONSTRAINT: smm_bfm_constraint
   // Sets proper values for driver/monitor enables
   constraint egress_constraint {
      mwr_req_agent_cfg.monitor_enable == UVM_ACTIVE;
      mwr_req_agent_cfg.driver_enable  == UVM_PASSIVE;
      mrd_req_agent_cfg.monitor_enable == UVM_ACTIVE;
      mrd_req_agent_cfg.driver_enable  == UVM_ACTIVE;
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
      
      mwr_req_agent_cfg = new("smm_bfm_mwr_req_agent_cfg");
      mrd_req_agent_cfg = new("smm_bfm_mrd_req_agent_cfg");
   endfunction : new

endclass : mby_smm_bfm_cfg
`endif
