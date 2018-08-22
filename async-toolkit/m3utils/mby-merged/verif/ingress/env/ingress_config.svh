//-----------------------------------------------------------------------------
// Title         : Ingress config object
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : ingress_config.svh
// Author        : jose.j.godinez.carrillo  <jjgodine@ichips.intel.com>
// Created       : 21.08.2018
// Last modified : 21.08.2018
//-----------------------------------------------------------------------------
// Description :
// This is the configuration object to control the ingress env.
// This Class contain all the switches to control the ENV setting.
// 
// By default it contains Saola config object:
// 
// checkers_enabled - default 1
// monitors_enabled - default 1
// trackers_enabled - default 1
// coverage_enabled - default 1
// 
// By default Saola will be build this object unless Upper env 
// or a test override it using set_config ...
//
//-----------------------------------------------------------------------------
// Copyright (c) 2018 by Intel Corporation This model is the confidential and
// proprietary property of Intel Corporation and the possession or use of this
// file requires a written license from Intel Corporation.
//------------------------------------------------------------------------------
// Modification history :
// 21.08.2018 : created
//-----------------------------------------------------------------------------

class ingress_config extends slu_config_object;

    string ingress_primary_access  = "primary";
    string ingress_sideband_access = "sideband";
    bit    ingress_chassis_rst_verbose_dbg;
    bit    ingress_has_reset_pkg = 0;

  `uvm_object_utils_begin(ingress_config)
     `uvm_field_string(ingress_primary_access,          UVM_ALL_ON)
     `uvm_field_string(ingress_sideband_access,         UVM_ALL_ON)
     `uvm_field_string(ingress_chassis_rst_verbose_dbg, UVM_ALL_ON)
   `uvm_object_utils_end

  //function: new 
  function new (string name = "ingress_config");
    super.new(name);
    set_parent_name(name);
  endfunction
  
endclass
