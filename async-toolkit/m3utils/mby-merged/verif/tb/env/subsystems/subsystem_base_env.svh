// -----------------------------------------------------------------------------
// INTEL CONFIDENTIAL
// copyright(c)// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :  Raghu P Gudla
// Created On   :  09/22/2018
// Description  :  subsystem integ base environment for derived classes
// -----------------------------------------------------------------------------

`ifndef SUBSYSTEM_BASE_ENV__SVH_
`define SUBSYSTEM_BASE_ENV__SVH_

typedef class fc_tb_env;

class subsystem_base_env extends slu_tb_env;

  // factory registration
  `uvm_component_utils(subsystem_base_env)
  
  // FC TB handle
  fc_tb_env  fc_tb_env_handle;

  // FC TB cfg handle
  fc_cfg_obj fc_cfg_obj_handle;

  //
  // constructor
  //
  function new( string name ="subsystem_base_env", uvm_component parent = null);
    super.new(name, parent);
  endfunction : new

  //
  // build_phase method
  //
  function void build_phase (uvm_phase phase);
    `uvm_info(get_type_name(), "start subsystem_base_env build phase", UVM_MEDIUM)
    super.build_phase(phase);

    // getting the object's into tb_env and cfg handles
    fc_tb_env_handle = fc_tb_env::get_tb_env();
    fc_cfg_obj_handle = fc_tb_env_handle.get_cfg_obj_handle();
    `uvm_info(get_type_name(), "end of subsystem_base_env build phase", UVM_MEDIUM)
  endfunction : build_phase

  //
  // connect_phase method
  //
  function void connect_phase (uvm_phase phase);
    `uvm_info(get_type_name(), "start subsystem_base_env connect phase", UVM_MEDIUM)
    super.connect_phase(phase);
    `uvm_info(get_type_name(), "end of subsystem_base_env connect phase", UVM_MEDIUM)
  endfunction : connect_phase

  //
  // end_of_elaboration_phase method
  //
  function void end_of_elaboration_phase (uvm_phase phase);
    `uvm_info(get_type_name(), "start subsystem_base_env end_of_elaboration phase", UVM_MEDIUM)
    super.end_of_elaboration_phase(phase);
    `uvm_info(get_type_name(), "end of subsystem_base_env end_of_elaboration phase", UVM_MEDIUM)
  endfunction : end_of_elaboration_phase

endclass : subsystem_base_env
 
`endif // SUBSYSTEM_BASE_ENV__SVH_

