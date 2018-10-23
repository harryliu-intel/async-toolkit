// vim: noai : ts=3 : sw=3 : expandtab : ft=systemverilog
//-----------------------------------------------------------------------------
// Title         : Ingress test library
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : mby_igr_test_lib.sv
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

program mby_igr_test_lib;

   import uvm_pkg::*;
   import mby_igr_env_pkg::*;
   import eth_bfm_pkg::*;
   import mby_ec_bfm_pkg::*;

   `include "uvm_macros.svh"
   `include "mby_igr_base_test.svh"
   `include "mby_igr_rand_test.svh"

   // UVM Start test
   initial begin
      string testname;
      if ($value$plusargs("UVM_TESTNAME=%s", testname  )) begin
         $display ("INGRESS_tb Started Running %s in UVM mode!\n",testname);
      end
      uvm_pkg::run_test(testname);
   end

endprogram
