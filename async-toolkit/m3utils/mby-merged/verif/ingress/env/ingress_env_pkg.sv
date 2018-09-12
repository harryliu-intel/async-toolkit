//-----------------------------------------------------------------------------
// Title         : Ingress env pkg
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : ingress_env_pkg.sv
// Author        : jose.j.godinez.carrillo  <jjgodine@ichips.intel.com>
// Created       : 21.08.2018
// Last modified : 21.08.2018
//-----------------------------------------------------------------------------
// Description :
// Ingress env pkg definition
//-----------------------------------------------------------------------------
// Copyright (c) 2018 by Intel Corporation This model is the confidential and
// proprietary property of Intel Corporation and the possession or use of this
// file requires a written license from Intel Corporation.
//------------------------------------------------------------------------------
// Modification history :
// 21.08.2018 : created
//-----------------------------------------------------------------------------

package ingress_env_pkg;

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
    
  import mby_ec_bfm_pkg::*;
  import ec_env_pkg::*;

`include "uvm_macros.svh"
`include "slu_macros.svh"
  
`include "ingress_types.svh"
`include "ingress_ti_config.svh"
`include "ingress_config.svh"
`include "ingress_ral_env.svh"
`include "ingress_env_monitor.svh"
`include "ingress_base_env.svh"
`include "ingress_env.svh"
`include "ingress_seqlib.sv"

endpackage // ingress_env_pkg
