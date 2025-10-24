// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

// vim: noai : ts=3 : sw=3 : expandtab : ft=systemverilog

//------------------------------------------------------------------------------
//
// INTEL CONFIDENTIAL
//
// Copyright 2018 Intel Corporation All Rights Reserved.
//
// The source code contained or described herein and all documents related to
// the source code ("Material") are owned by Intel Corporation or its suppliers
// or licensors. Title to the Material remains with Intel Corporation or its
// suppliers and licensors. The Material contains trade secrets and proprietary
// and confidential information of Intel or its suppliers and licensors.  The
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
//   Author        : Kaleem Sheriff
//   Project       : Madison Bay
//------------------------------------------------------------------------------

// Class: mby_rx_ppe_dut_cfg
//
// This is the configuration object to control RX_PPE DUT configuration.
//

`ifndef __MBY_RX_PPE_DUT_CFG_GUARD
`define __MBY_RX_PPE_DUT_CFG_GUARD

`ifndef __INSIDE_MBY_RX_PPE_ENV_PKG
`error "Attempt to include file outside of mby_rx_ppe_env_pkg."
`endif

class mby_rx_ppe_dut_cfg extends shdv_base_config;

   // Variable: reset_type
   // Definition of the RESET type
   reset_type_e                     reset_type ;
   
   // Variable: parser_cfg
   rand mby_rx_ppe_parser_cfg       parser_cfg;
   
   // Variable: mapper_cfg
   rand mby_rx_ppe_mapper_cfg       mapper_cfg;
   
   // Variable: lpm_cfg
   rand mby_rx_ppe_lpm_cfg          lpm_cfg;
   
   // Variable: ema_cfg
   rand mby_rx_ppe_em_a_cfg         ema_cfg;
   
   // Variable: wcm_cfg
   rand mby_rx_ppe_wcm_cfg          wcm_cfg;

   // Variable: emb_cfg
   rand mby_rx_ppe_em_b_cfg         emb_cfg;
   
   // Variable: hash_cfg
   rand mby_rx_ppe_hash_cfg         hash_cfg;
   
   // Variable: policer_cfg
   rand mby_rx_ppe_policer_cfg      policer_cfg;
   
   // Variable: nexthop_cfg
   rand mby_rx_ppe_nexthop_cfg      nexthop_cfg;
   
   // Variable: maskgen_cfg
   rand mby_rx_ppe_maskgen_cfg      maskgen_cfg;
   
   // Variable: triggers_cfg
   rand mby_rx_ppe_triggers_cfg     triggers_cfg;
   


   `uvm_object_utils_begin(mby_rx_ppe_dut_cfg)
      `uvm_field_enum  (reset_type_e,                  reset_type,                    UVM_DEFAULT)
   `uvm_object_utils_end


   //---------------------------------------------------------------------------
   // Constructor: new
   //
   // Constructor.
   //
   // Arguments:
   //    string name - mby_rx_ppe_dut_cfg object name
   //---------------------------------------------------------------------------
   function new( string name = "mby_rx_ppe_dut_cfg");
      super.new(name);
      
      parser_cfg   = mby_rx_ppe_parser_cfg::type_id::create("parser_cfg");
      mapper_cfg   = mby_rx_ppe_mapper_cfg::type_id::create("mapper_cfg");
      lpm_cfg      = mby_rx_ppe_lpm_cfg::type_id::create("lpm_cfg");
      ema_cfg     = mby_rx_ppe_em_a_cfg::type_id::create("ema_cfg");
      wcm_cfg      = mby_rx_ppe_wcm_cfg::type_id::create("wcm_cfg");
      emb_cfg     = mby_rx_ppe_em_b_cfg::type_id::create("emb_cfg");
      
      hash_cfg     = mby_rx_ppe_hash_cfg::type_id::create("hash_cfg");
      policer_cfg  = mby_rx_ppe_policer_cfg::type_id::create("policer_cfg");
      nexthop_cfg  = mby_rx_ppe_nexthop_cfg::type_id::create("nexthop_cfg");
      maskgen_cfg  = mby_rx_ppe_maskgen_cfg::type_id::create("maskgen_cfg");
      triggers_cfg = mby_rx_ppe_triggers_cfg::type_id::create("triggers_cfg");
      

   endfunction: new

   //---------------------------------------------------------------------------
   // Function: pre_randomize
   //---------------------------------------------------------------------------
   function void pre_randomize();
      super.pre_randomize();
   endfunction: pre_randomize

   //---------------------------------------------------------------------------
   // Function: post_randomize
   // Collect Plusargs here, then push down cfg changes to any bfm/IP
   //---------------------------------------------------------------------------
   function void post_randomize();
      super.post_randomize();

   endfunction: post_randomize


endclass: mby_rx_ppe_dut_cfg

`endif // __MBY_RX_PPE_DUT_CFG_GUARD
