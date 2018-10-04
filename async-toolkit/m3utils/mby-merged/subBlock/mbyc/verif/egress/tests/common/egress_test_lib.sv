// vim: noai : ts=3 : sw=3 : expandtab : ft=systemverilog
//-----------------------------------------------------------------------------
// Title         : Egress test library
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : egress_test_lib.sv
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

program egress_test_lib;

`ifdef XVM
  import ovm_pkg::*;
  import xvm_pkg::*;
`include "ovm_macros.svh"
`include "sla_macros.svh"
`endif

  import sla_pkg::*;
  import uvm_pkg::*;
  import egress_env_pkg::*;
  import eth_bfm_pkg::*;
  import mby_ec_bfm_pkg::*;

`include "uvm_macros.svh"
`include "slu_macros.svh"
`include "egress_base_test.svh"
`include "egress_rand_test.svh"

  // UVM Start test
  initial begin
    string testname;
    if ($value$plusargs("UVM_TESTNAME=%s", testname  )) begin
`ifndef XVM
      $display ("EGRESS_tb Started Running %s in UVM mode!\n",testname);
    end
    uvm_pkg::run_test(testname);
`else
    $display ("EGRESS_tb Started Running %s in XVM mode!\n",testname);
    end
    xvm_pkg::run_test("", testname,   xvm::EOP_UVM);
`endif
  end

endprogram
