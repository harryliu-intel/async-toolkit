
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
//   Author        : Akshay Kotian
//   Project       : Madison Bay
//------------------------------------------------------------------------------

//   Package:    mby_rx_ppe_env_pkg
//
//   This is the main Env Package file. Holds all Environment files that will be included in RX_PPE env.


`ifndef __MBY_RX_PPE_ENV_PKG_GUARD
`define __MBY_RX_PPE_ENV_PKG_GUARD

package mby_rx_ppe_env_pkg;

   import uvm_pkg::*;

   import shdv_base_pkg::*;

   import mby_common_pkg::*;
   import mby_ec_bfm_pkg::*;
   //import ec_env_pkg::*;

   `include "uvm_macros.svh"

   `define __INSIDE_MBY_RX_PPE_ENV_PKG

   `include "mby_rx_ppe_defines.svh"
   `include "mby_rx_ppe_env_cfg.svh"
   `include "mby_rx_ppe_dut_cfg.svh"
   `include "mby_rx_ppe_tb_top_cfg.svh"
//PJP   `include "mby_rx_ppe_ral_env.svh"
   `include "mby_rx_ppe_env.svh"

    `undef  __INSIDE_MBY_RX_PPE_ENV_PKG

endpackage: mby_rx_ppe_env_pkg

`endif // __MBY_RX_PPE_ENV_PKG_GUARD

