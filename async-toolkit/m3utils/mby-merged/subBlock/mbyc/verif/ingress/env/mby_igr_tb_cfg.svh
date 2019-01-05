//-----------------------------------------------------------------------------
// Title         : Ingress Testbench Config Object
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : mby_igr_tb_cfg.svh
// Author        : paul.j.pladsen <paul.j.pladsen@intel.com>
// Created       : 22.10.2018
// Last modified : 22.10.2018
//-----------------------------------------------------------------------------
// Description :
// This is the testbench configuration object which contains an instance of the dut configuration object and the env configuration object.  The dut configuration object holds all variable necessary to configure the design.  The env configuration object holds all variables and switches necessary to configure the simulation environment including BFMs, scoreboards, and test command line arguments.
//
//-----------------------------------------------------------------------------
// Copyright (c) 2018 by Intel Corporation This model is the confidential and
// proprietary property of Intel Corporation and the possession or use of this
// file requires a written license from Intel Corporation.
//-----------------------------------------------------------------------------
// Modification history :
// 22.10.2018 : created
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// Class: mby_igr_tb_cfg
//-----------------------------------------------------------------------------
class mby_igr_tb_cfg extends shdv_base_config;

   mby_igr_env_cfg env_cfg;
   mby_igr_dut_cfg dut_cfg;

   `uvm_object_utils_begin(mby_igr_tb_cfg)
      `uvm_field_object( env_cfg, UVM_ALL_ON )
      `uvm_field_object( dut_cfg, UVM_ALL_ON )
   `uvm_object_utils_end

   //--------------------------------------------------------------------------
   // Function: new
   //--------------------------------------------------------------------------
   function new (string name = "mby_igr_tb_cfg");
      super.new(name);
   endfunction : new

endclass : mby_igr_tb_cfg
