//-----------------------------------------------------------------------------
// Title         : Egress registers ral env class
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : mby_egr_regs_ral_env.svh
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

`include "mby_egr_regs_regs.svh"

class mby_egr_regs_ral_env extends sla_ral_env;

  `ovm_component_utils(mby_egr_regs_ral_env);

  rand mby_egr_regs_file mby_egr_regs;

  // --------------------------
  function new( string n="mby_egr_regs_ral_env", ovm_component p = null, string hdl_path = "");
    super.new( n, p, hdl_path);
  endfunction : new

  // --------------------------
  virtual function void build();
    super.build();
    mby_egr_regs = mby_egr_regs_file::type_id::create("mby_egr_regs", this);
    add_file("mby_egr_regs", mby_egr_regs);
  endfunction : build

  // --------------------------
  function void connect();
    mby_egr_regs.set_base(4'h0);
    set_bit_blasting(1);
  endfunction : connect

  // --------------------------
  virtual function string sprint_sv_ral_env();  
     return({"Generated SV RAL Env ---> ", `__FILE__}); 
  endfunction  

endclass : mby_egr_regs_ral_env
