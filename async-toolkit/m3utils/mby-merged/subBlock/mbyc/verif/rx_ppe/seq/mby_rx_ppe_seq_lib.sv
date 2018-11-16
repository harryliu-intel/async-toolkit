
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

//   Package:    mby_rx_ppe_seq_lib
//
//   Section: rx_ppe sequences library
//
//    include all rx_ppe sequences
//
//    <mby_rx_ppe_power_good_seq.sv>           - Power_Good sequence, is the initial sequence used to set Power_Good and clear Resets
//
//    <mby_rx_ppe_hard_reset_seq.sv>           - Hard_Reset sequence, drops the Hard Reset after a delay. (Ran second)
//
//    <mby_rx_ppe_warm_reset_seq.sv>           - Warm_Reset sequence, drops the Warm Reset after a delay. (Ran third)


`ifndef __MBY_RX_PPE_SEQ_LIB_GUARD
`define __MBY_RX_PPE_SEQ_LIB_GUARD

package mby_rx_ppe_seq_lib;

   import uvm_pkg::*;

   import shdv_base_pkg::*;

   `include "uvm_macros.svh"

   `define __INSIDE_MBY_RX_PPE_SEQ_LIB
   `include "mby_rx_ppe_env_base_seq.svh"
//PJP   `include "mby_rx_ppe_power_good_seq.svh"
//PJP   `include "mby_rx_ppe_hard_reset_seq.svh"
//PJP   `include "mby_rx_ppe_warm_reset_seq.svh"
//PJP   `include "mby_rx_ppe_env_cfg_seq.svh"

   `undef  __INSIDE_MBY_RX_PPE_SEQ_LIB

endpackage: mby_rx_ppe_seq_lib

`endif // __MBY_RX_PPE_SEQ_LIB_GUARD
