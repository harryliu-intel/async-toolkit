//-----------------------------------------------------------------------------
// Title         : Egress Test island config
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : mby_egr_ti_config.svh
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

//-------------------------------------------------------------------------------
// Class: mby_igr_ti_cfg
//-------------------------------------------------------------------------------
class mby_egr_ti_cfg extends uvm_object;

  string egress_ti_low_path;
  int    egress_has_reset_pkg;

  `uvm_object_utils_begin(mby_egr_ti_cfg)
     `uvm_field_string(egress_ti_low_path, UVM_ALL_ON)
     `uvm_field_int(egress_has_reset_pkg, UVM_ALL_ON)
   `uvm_object_utils_end

   //---------------------------------------------------------------------------
   // Function: new
   //---------------------------------------------------------------------------
   function new(string name = "");
      super.new(name);
   endfunction : new
  
endclass : mby_egr_ti_cfg
