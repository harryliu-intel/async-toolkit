//-----------------------------------------------------------------------------
// Title         : Ingress base environment class
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : ingress_base_env.svh
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

class ingress_base_env extends shdv_base_env;

  protected ingress_config cfg;

  `uvm_component_utils_begin(ingress_base_env)
    `uvm_field_object(cfg, UVM_ALL_ON)
  `uvm_component_utils_end
      
  function new (string name="ingress_base_env", uvm_component parent = null);
    super.new(name, parent);
    // Set SLA CFG object type
    // Refer to Saola user guide for info about Saola CFG obj
    config_type = "ingress_config";
    //Set RAL env type
    ral_type = "ingress_ral_env";
  endfunction: new

  //////////////////////////////////////////////////////////////////////////////
  // Function: ingress_base_env build
  // build phase of ingress_base_env
  // All VC's and Agent should be build in this pahse.
  // For each new VC's/Agnet it is recommand to add it an a specific function
  virtual function void build_phase(uvm_phase phase);
    if (_level == SLA_TOP) begin
      // In this section all the IP specific stuff that are
      // relevant only when the IP is stand alone should be set
      // IP SM env - will not be reuse in integration
      //    sm_type = "ingress_sm_env";
      //    // IP IM env - will not be reuse in integration
      //    im_type = "ingress_im_env";
      //    //Set Fuse env type
      //    fuse_type= "ingress_fuse_env";
      // Saola timeouts
      // max_run_clocks = 20000000;
      // UVM timeout
      // uvm_pkg::set_global_timeout (2000us);
    end // if (_level == SLA_TOP)
    super.build_phase(phase);
    assert ($cast(cfg, config_obj));
  endfunction // void

  //////////////////////////////////////////////////////////////////////////////
  // Function: ingress_base_env connect 
  // connect phase of ingress_base_env
  function void connect_phase(uvm_phase phase);
  endfunction // void

  //////////////////////////////////////////////////////////////////////////////
  // Function: ingress_base_env end_of_elaboration 
  // end_of_elaboration  phase of ingress_base_env
  // In this pahse we randomize the fuse env
  virtual function void end_of_elaboration_phase (uvm_phase phase);
    super.end_of_elaboration_phase(phase);
    if (_level == SLA_TOP) begin
      // Randomize the fuses
      `slu_assert(fuse.randomize(),("Unable to randomize fuses"));
    end
  endfunction

  //////////////////////////////////////////////////////////////////////////////
  // Function: ingress_base_env start_of_simulation 
  // start_of_simulation  phase of ingress_base_env
  virtual function void start_of_simulation_phase (uvm_phase phase);
    super.start_of_simulation_phase(phase);
  endfunction
  
endclass // ingress_base_env

