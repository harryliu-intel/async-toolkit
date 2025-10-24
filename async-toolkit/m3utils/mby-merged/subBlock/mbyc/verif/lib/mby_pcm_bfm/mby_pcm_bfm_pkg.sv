// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

//-----------------------------------------------------------------------------
// Title         : Madison Bay PCM Bus Functional Model Package
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : mby_pcm_bfm_pkg.sv
// Author        : jose.j.godinez.carrillo  <jjgodine@ichips.intel.com>
// Created       : 01.11.2018
//-----------------------------------------------------------------------------
// Description :
// This is the PCM BFM package file
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
`ifndef __MBY_PCM_BFM_PKG__
`define __MBY_PCM_BFM_PKG__
//------------------------------------------------------------------------------
//
// PACKAGE: mby_pcm_bfm_pkg
// This is the Madison Bay gcm (global congestion manager) bfm package,
// currently it depends on the uvm, shdv_base_pkg, mby_base_pkg verification
// packages and also depends on the gmm rtl package, which includes defines and
// typedefs for the interfaces and transaction classes of this agent.
//
//------------------------------------------------------------------------------
package mby_pcm_bfm_pkg;

   import uvm_pkg::*;
   import shdv_base_pkg::*;
   import mby_base_pkg::*;
   import mby_gmm_pkg::*;
   import shared_pkg::*;

   `include "uvm_macros.svh"
   `include "mby_pcm_bfm_defines.svh"
   `include "mby_pcm_bfm_types.svh"
   `include "mby_pcm_bfm_queue_xaction.svh"
   `include "mby_pcm_bfm_deque_xaction.svh"
   `include "mby_pcm_bfm_wm_xaction.svh"
   `include "mby_pcm_bfm_sm_wm_xaction.svh"
   `include "mby_pcm_bfm_plcr_xaction.svh"
   `include "mby_pcm_bfm_cfg.svh"
   `include "mby_pcm_bfm.svh"


endpackage : mby_pcm_bfm_pkg


`endif


