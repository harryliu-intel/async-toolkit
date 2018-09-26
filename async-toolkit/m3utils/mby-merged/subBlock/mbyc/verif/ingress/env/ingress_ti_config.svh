//-----------------------------------------------------------------------------
// Title         : Ingress Test island config
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : ingress_ti_config.svh
// Author        : jose.j.godinez.carrillo  <jjgodine@ichips.intel.com>
// Created       : 21.08.2018
// Last modified : 21.08.2018
//-----------------------------------------------------------------------------
// Description :
// This is the configuration object to control the MBY rtl env.
//-----------------------------------------------------------------------------
// Copyright (c) 2018 by Intel Corporation This model is the confidential and
// proprietary property of Intel Corporation and the possession or use of this
// file requires a written license from Intel Corporation.
//------------------------------------------------------------------------------
// Modification history :
// 21.08.2018 : created
//-----------------------------------------------------------------------------

class ingress_ti_config extends uvm_object;

 string ingress_ti_low_path;
 int ingress_has_reset_pkg;

  `uvm_object_utils_begin(ingress_ti_config)
     `uvm_field_string(ingress_ti_low_path, UVM_ALL_ON)
     `uvm_field_int(ingress_has_reset_pkg, UVM_ALL_ON)
   `uvm_object_utils_end

  //function: new 
  function new( string         name = "");
    super.new(name);
  endfunction
  
endclass
