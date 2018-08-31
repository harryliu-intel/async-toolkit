//-----------------------------------------------------------------------------
// Title         : Egress base environment class
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : egress_base_env.svh
// Author        : jose.j.godinez.carrillo  <jjgodine@ichips.intel.com>
// Created       : 21.08.2018
// Last modified : 21.08.2018
//-----------------------------------------------------------------------------
// Description :
// This is the main ingres env file
//-----------------------------------------------------------------------------
// Copyright (c) 2018 by Intel Corporation This model is the confidential and
// proprietary property of Intel Corporation and the possession or use of this
// file requires a written license from Intel Corporation.
//------------------------------------------------------------------------------
// Modification history :
// 21.08.2018 : created
//-----------------------------------------------------------------------------
class egress_base_env extends slu_tb_env;

  // Variable: cfg
  // The egress config object
  protected egress_config cfg;

  `uvm_component_utils_begin(egress_base_env)
    `uvm_field_object(cfg, UVM_ALL_ON)
  `uvm_component_utils_end
      
  function new (string name="egress_base_env", uvm_component parent = null);
    super.new(name, parent);
    // Set SLA CFG object type
    // Refer to Saola user guide for info about Saola CFG obj
    config_type = "egress_config";
    //Set RAL env type
    ral_type = "egress_ral_env";
  endfunction: new

  //////////////////////////////////////////////////////////////////////////////
  // Function: egress_base_env build
  // Build phase of egress_base_env.
  virtual function void build_phase(uvm_phase phase);
    if (_level == SLA_TOP) begin
      // In this section all the IP specific stuff that are
      // relevant only when the IP is stand alone should be set
      // IP SM env - will not be reuse in integration
      //    sm_type = "egress_sm_env";
      //    // IP IM env - will not be reuse in integration
      //    im_type = "egress_im_env";
      //    //Set Fuse env type
      //    fuse_type= "egress_fuse_env";
      // Saola timeouts
      // max_run_clocks = 20000000;
      // UVM timeout
      // uvm_pkg::set_global_timeout (2000us);
    end // if (_level == SLA_TOP)
    super.build_phase(phase);
    assert ($cast(cfg, config_obj));
  endfunction // void

  //////////////////////////////////////////////////////////////////////////////
  // Function: egress_base_env connect 
  // connect phase of egress_base_env
  function void connect_phase(uvm_phase phase);
  endfunction // void

  //////////////////////////////////////////////////////////////////////////////
  // Function: egress_base_env end_of_elaboration 
  // end_of_elaboration  phase of egress_base_env
  virtual function void end_of_elaboration_phase (uvm_phase phase);
    super.end_of_elaboration_phase(phase);
    if (_level == SLA_TOP) begin
      // Randomize the fuses
      `slu_assert(fuse.randomize(),("Unable to randomize fuses"));
    end
  endfunction

  //////////////////////////////////////////////////////////////////////////////
  // Function: egress_base_env start_of_simulation 
  // start_of_simulation  phase of egress_base_env
  virtual function void start_of_simulation_phase (uvm_phase phase);
    super.start_of_simulation_phase(phase);
  endfunction
  
endclass // egress_base_env

