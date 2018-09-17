//-----------------------------------------------------------------------------
// Title         : Egress env pkg
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : egress_env_pkg.sv
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

package egress_env_pkg;

`ifdef XVM
  import ovm_pkg::*;
  import xvm_pkg::*;
`include "ovm_macros.svh"
`include "sla_macros.svh"
`endif

  import sla_pkg::*;
  import uvm_pkg::*;

  import shdv_base_pkg::*;
  //import mby_wm_dpi_pkg::*;
  import mby_common_pkg::*;

  import mby_ec_bfm_pkg::*;
  import ec_env_pkg::*;

`include "uvm_macros.svh"
`include "slu_macros.svh"

`include "egress_types.svh"
`include "egress_ti_config.svh"
`include "egress_config.svh"
`include "egress_ral_env.svh"
`include "egress_env_monitor.svh"
`include "egress_base_env.svh"
`include "egress_env.svh"
`include "egress_seqlib.sv"

endpackage // egress_env_pkg
