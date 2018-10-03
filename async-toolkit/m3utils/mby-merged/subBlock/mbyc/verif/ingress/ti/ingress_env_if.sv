//-----------------------------------------------------------------------------
// Title         : Ingress environment interface
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : ingress_env_if.sv
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

interface ingress_env_if();
  // Power good
  logic power_good_reset;

  // Primary interface clk & reset
  logic primary_reset;
  logic primary_clock;
  logic enable_primary_clock;

  //  Secondary interface clk & reset
  logic secondary_reset;
  logic secondary_clock;
  logic enable_secondary_clock;
	
  // Dummy interrupt wire for monitoring.
  logic ingress_int_wire;

  initial begin
    power_good_reset = 1;
    primary_reset = 1;
    secondary_reset = 1;
    enable_primary_clock   = 0;
    enable_secondary_clock = 0;
    #5ps;
    enable_primary_clock   = 1;
    enable_secondary_clock = 1;
    #8_000ps;
    power_good_reset = 0;
    primary_reset    = 0;
    secondary_reset  = 0;
  end
  
endinterface
