//-----------------------------------------------------------------------------
// Title         : Ingress environment interface
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : mby_igr_env_if.sv
// Author        : jose.j.godinez.carrillo  <jjgodine@ichips.intel.com>
// Created       : 21.08.2018
// Last modified : 21.08.2018
//-----------------------------------------------------------------------------
// Description :
// interface to connect between the ingress DUT and environment.
// can be used for OOO signals. E.g. interrupts, power, clocks and resets
//-----------------------------------------------------------------------------
// Copyright (c) 2018 by Intel Corporation This model is the confidential and
// proprietary property of Intel Corporation and the possession or use of this
// file requires a written license from Intel Corporation.
//------------------------------------------------------------------------------
// Modification history :
// 21.08.2018 : created
//-----------------------------------------------------------------------------

interface mby_igr_env_if();
   // Power good
   logic power_good_reset;

   // Primary interface clk & reset
  logic reset;
  logic clock;
	
   // Dummy interrupt wire for monitoring.
   logic ingress_int_wire;

   initial begin
    ingress_int_wire = 0;
      power_good_reset = 1;
    reset = 1;
      #8_000ps;
      power_good_reset = 0;
    reset = 0;
   end

endinterface
