//-----------------------------------------------------------------------------
// Title         : Ingress registers ral env class
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : ingress_regs_ral_env.svh
// Author        : jose.j.godinez.carrillo  <jjgodine@ichips.intel.com>
// Created       : 21.08.2018
// Last modified : 21.08.2018
//-----------------------------------------------------------------------------
// Description :
// 
//-----------------------------------------------------------------------------
// Copyright (c) 2018 by Intel Corporation This model is the confidential and
// proprietary property of Intel Corporation and the possession or use of this
// file requires a written license from Intel Corporation.
//------------------------------------------------------------------------------
// Modification history :
// 21.08.2018 : created
//-----------------------------------------------------------------------------

`ifndef SLA_PATH_TO_STRING
`define SLA_PATH_TO_STRING(SIG_PATH) `"SIG_PATH`"
`endif

`include "ingress_regs_regs.svh"

class ingress_regs_ral_env extends sla_ral_env;

  `ovm_component_utils(ingress_regs_ral_env);

  rand ingress_regs_file ingress_regs;

  // --------------------------
  function new( string n="ingress_regs_ral_env", ovm_component p = null, string hdl_path = "");
    super.new( n, p, hdl_path);
  endfunction : new

  // --------------------------
  virtual function void build();
    super.build();
    ingress_regs = ingress_regs_file::type_id::create("ingress_regs", this);
    add_file("ingress_regs", ingress_regs);
  endfunction : build

  // --------------------------
  function void connect();
    ingress_regs.set_base(4'h0);
    set_bit_blasting(1);
  endfunction : connect

  // --------------------------
  virtual function string sprint_sv_ral_env();  
     return({"Generated SV RAL Env ---> ", `__FILE__}); 
  endfunction  

endclass : ingress_regs_ral_env
