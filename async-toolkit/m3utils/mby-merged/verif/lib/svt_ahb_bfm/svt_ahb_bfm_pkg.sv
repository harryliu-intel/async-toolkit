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
//   Author        : Dhivya Sankar
//   Project       : Madison Bay
//------------------------------------------------------------------------------

//   Package:   svt_ahb_bfm_pkg
   package svt_ahb_bfm_pkg;

      import sla_pkg::*;
      `include "uvm_macros.svh"
      `include "slu_macros.svh"
      import svt_uvm_pkg::*;
      import uvm_pkg::*;
      import svt_ahb_uvm_pkg::*;
        
      // kl - added to fulfill the SVT macros consumed by svt_axi_bfm_pkg
      import svt_amba_common_uvm_pkg::*;
      `include "svt_ahb_common_defines.svi"

       `include "svt_ahb_bfm_defines.sv"
       `include "svt_uvm_util.svi"
      `include "cust_svt_ahb_system_configuration.sv"
      `include "cust_svt_master_transaction.sv"
      `include "cust_svt_ahb_slave_transaction.sv"
      `include "ahb_virtual_sequencer.sv"
      `include "ahb_slave_random_response_sequence.sv"
      `include "ahb_master_directed_sequence.sv"
      `include "ahb_bfm_env.sv"

   endpackage


