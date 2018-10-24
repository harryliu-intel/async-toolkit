//-----------------------------------------------------------------------------
// Title         : Ingress DUT config object
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : mby_igr_dut_cfg.svh
// Author        : paul.j.pladsen <paul.j.pladsen@intel.com>
// Created       : 21.08.2018
// Last modified : 21.08.2018
//-----------------------------------------------------------------------------
// Description :
// This is the configuration object contains all necessary switches, registers,
// and parameters in order to configure the Ingress DUT.
// 
// NOTE: This configuration object will be included in a small sandbox
// testbench used in the emulation environment in order to enable random
// configuration of the DUT when running simulations on the emulation platform.
// In order to reducd the amound of testbench collateral needed the DUT config
// will not extend the shared dv config object.
//-----------------------------------------------------------------------------
// Copyright (c) 2018 by Intel Corporation This model is the confidential and
// proprietary property of Intel Corporation and the possession or use of this
// file requires a written license from Intel Corporation.
//-----------------------------------------------------------------------------
// Modification history :
// 22.10.2018 : created
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// Class: mby_igr_dut_cfg
//-----------------------------------------------------------------------------
class mby_igr_dut_cfg extends uvm_object;

   `uvm_object_utils_begin(mby_igr_dut_cfg)
   `uvm_object_utils_end

   //--------------------------------------------------------------------------
   // Function: new
   //--------------------------------------------------------------------------
   function new (string name = "mby_igr_dut_cfg");
      super.new(name);
   endfunction : new

endclass : mby_igr_dut_cfg
