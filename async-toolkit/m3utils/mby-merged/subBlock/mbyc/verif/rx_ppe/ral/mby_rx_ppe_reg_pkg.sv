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
//   Author        :
//   Project       :
//------------------------------------------------------------------------------

package mby_rx_ppe_reg_pkg;

   import uvm_pkg::*;
   `include "uvm_macros.svh"

   `include "ral_parser.sv"
   `include "ral_mapper.sv"
   `include "ral_EM.sv"
   `include "ral_A.sv"     
   `include "ral_B.sv"     
//  `include "ral_cgrp_a.sv" // ral_EM and ral_A forms cgrp_a_map
//  `include "ral_cgrp_b.sv" // ral_EM and ral_B forms cgrp_b_map
   `include "ral_nexthop.sv"
   `include "ral_policers.sv"
   `include "ral_entropy.sv"
   `include "ral_cm_apply.sv"
   `include "ral_cm_usage.sv"
   `include "ral_fwd_misc.sv"
   `include "ral_mst_glort.sv"
   `include "ral_trig_apply.sv"
   `include "ral_trig_apply_misc.sv"
   `include "ral_trig_usage.sv"
   `include "ral_stats.sv"


   `include "mby_rx_ppe_reg_blk.sv"

endpackage: mby_rx_ppe_reg_pkg
