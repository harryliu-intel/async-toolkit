//-----------------------------------------------------------------------------
// Title         : Madison Bay Base Config Class
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : mby_base_config.svh
// Author        : jose.j.godinez.carrillo  <jjgodine@ichips.intel.com>
// Created       : 30.10.2018
// Last modified : 30.10.2018
//-----------------------------------------------------------------------------
// Description :
// Base configuration object for Madison Bay.
//-----------------------------------------------------------------------------
// Copyright (c) 2018 by Intel Corporation This model is the confidential and
// proprietary property of Intel Corporation and the possession or use of this
// file requires a written license from Intel Corporation.
//------------------------------------------------------------------------------
// Modification history :
// 30.10.2018 : created
//-----------------------------------------------------------------------------
`ifndef __MBY_BASE_PKG__
`error "Attempt to include file outside of mby_igr_env_pkg."
`endif
`ifndef __MBY_BASE_CONFIG__
`define __MBY_BASE_CONFIG__
//-----------------------------------------------------------------------------
// CLASS: mby_base_config
//
//-----------------------------------------------------------------------------
class mby_base_config extends shdv_base_config;

   // VARIABLE: is_active
   // Agent is configured to be active or passive
   uvm_active_passive_enum is_active;
   
   // VARIABLE: mon_active
   // Agent is configured to be active or passive
   uvm_active_passive_enum mon_active;

   // UVM object utils macro
   `uvm_object_param_utils_begin(mby_base_config)
      `uvm_field_enum(uvm_active_passive_enum, is_active,  UVM_DEFAULT)
      `uvm_field_enum(uvm_active_passive_enum, mon_active, UVM_DEFAULT)
   `uvm_object_utils_end

   // -------------------------------------------------------------------------
   // CONSTRUCTOR: new
   //
   // Constructor
   //
   // ARGUMENTS:
   //    string name - An identifier for this configuration object.
   // -------------------------------------------------------------------------
   function new(string name = "mby_base_config");
      super.new(name);
   endfunction : new

endclass : mby_base_config
`endif

