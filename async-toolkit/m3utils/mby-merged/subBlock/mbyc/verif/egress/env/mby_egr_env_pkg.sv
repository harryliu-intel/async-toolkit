//-----------------------------------------------------------------------------
// Title         : Egress env pkg
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : mby_egr_env_pkg.sv
// Author        : jose.j.godinez.carrillo  <jjgodine@ichips.intel.com>
// Created       : 21.08.2018
// Last modified : 21.08.2018
//-----------------------------------------------------------------------------
// Description :
// Egress env pkg definition
//-----------------------------------------------------------------------------
// Copyright (c) 2018 by Intel Corporation This model is the confidential and
// proprietary property of Intel Corporation and the possession or use of this
// file requires a written license from Intel Corporation.
//------------------------------------------------------------------------------
// Modification history :
// 21.08.2018 : created
//-----------------------------------------------------------------------------

`ifndef __MBY_IGR_ENV_PKG_GUARD
`define __MBY_IGR_ENV_PKG_GUARD

package mby_egr_env_pkg;

   import uvm_pkg::*;

   import shdv_base_pkg::*;
   //import mby_wm_dpi_pkg::*;
   import mby_common_pkg::*;
   import mby_ec_bfm_pkg::*;

   `include "uvm_macros.svh"

   `include "mby_egr_types.svh"
   `include "mby_egr_ti_cfg.svh"
   `include "mby_egr_dut_cfg.svh"
   `include "mby_egr_env_cfg.svh"
   `include "mby_egr_tb_cfg.svh"
   //PJP`include "mby_egr_ral_env.svh"
   `include "mby_egr_base_env.svh"
   `include "mby_egr_env_monitor.svh"
   `include "mby_egr_env.svh"
   `include "mby_egr_seqlib.sv"

endpackage // mby_egr_env_pkg

`endif // __MBY_IGR_ENV_PKG_GUARD
