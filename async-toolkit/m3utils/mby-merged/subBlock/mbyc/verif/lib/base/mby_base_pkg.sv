//-----------------------------------------------------------------------------
// Title         : Madison Bay Base Package
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : mby_base_pkg.svh
// Author        : jose.j.godinez.carrillo  <jjgodine@ichips.intel.com>
// Created       : 29.10.2018
// Last modified : 29.10.2018
//-----------------------------------------------------------------------------
// Description :
// This is the base sequencer class for Madison Bay
//-----------------------------------------------------------------------------
// Copyright (c) 2018 by Intel Corporation This model is the confidential and
// proprietary property of Intel Corporation and the possession or use of this
// file requires a written license from Intel Corporation.
//------------------------------------------------------------------------------
// Modification history :
// 29.10.2018 : created
//-----------------------------------------------------------------------------
`ifndef __MBY_BASE_PKG__
`define __MBY_BASE_PKG__

package mby_base_pkg;

   import uvm_pkg::*;
   import shdv_base_pkg::*;

   `include "uvm_macros.svh"
   `include "mby_base_config.svh"
   `include "mby_base_sequence_item.svh"
   `include "mby_base_sequencer.svh"
   `include "mby_base_monitor.svh"
   `include "mby_base_driver.svh"
   `include "mby_base_agent.svh"

endpackage

`endif
