// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

//-----------------------------------------------------------------------------
// Title         : Madison Bay IGR E2E Scoreboard Package
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : mby_igr_scbd_pkg.sv
// Author        : ricardo.a.alfaro.gomez  <raalfaro@ichips.intel.com>
// 2ry contact   : jose.j.godinez.carrillo  <jjgodine@ichips.intel.com>
// Created       : 12.12.2018
//-----------------------------------------------------------------------------
// Description :
// This is the IGR E2E Scoreboard package file
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
`ifndef __MBY_IGR_SCBD_PKG__
`define __MBY_IGR_SCBD_PKG__
//------------------------------------------------------------------------------
//
// PACKAGE: mby_igr_scbd_pkg
// This is the Madison Bay igr scbd package, currently it depends on the uvm,
// shdv_base_pkg and mby_base_pkg verification packages.
//
//------------------------------------------------------------------------------
package mby_igr_scbd_pkg;

    import uvm_pkg::*;
    import shdv_base_pkg::*;
    import mby_base_pkg::*;

    `include "uvm_macros.svh"
//TODO:    `include "mby_igr_base_scbd_xaction.svh"
    `include "mby_igr_base_predictor.svh"
    `include "mby_igr_base_comparator.svh"
    `include "mby_igr_base_scbd.svh"

    `include "mby_igr_e2e_scbd.svh"

endpackage : mby_igr_scbd_pkg

`endif


