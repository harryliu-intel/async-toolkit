// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :  Raghu P Gudla
// Created On   :  09/22/2018
// Description  :  UVM IP3 integration environment.
// -----------------------------------------------------------------------------

`ifndef _vte_ip3Uvm_integ_env__svh_
 `define _vte_ip3Uvm_integ_env__svh_
 
 // implementation only if ENV enable defined
 `ifdef VTE_IP3_UVM_ENV_ENABLE
 
class vte_ip3Uvm_integ_env extends subsystem_base_env;
 
   // factory registration
   `uvm_component_utils(vte_ip3Uvm_integ_env)
   
   // IP3 UVM env
   ip3_uvm_env       ip3_uvm_env_inst;
 
   // IP3 UVM env cfg
   ip3_uvm_env_cfg   ip3_uvm_env_cfg_inst;
 
   //
   // constructor
   //
   function new( string name ="vte_ip3Uvm_integ_env", uvm_component parent = null);
     super.new(name, parent);
   endfunction : new
 
   //
   // build_phase method
   //
   function void build_phase (uvm_phase phase);
     `uvm_info(get_type_name(), "start IP3 UVM ENV build phase", UVM_MEDIUM)
     super.build_phase(phase);
 
     build_ip3UVMEnv();
 
     `uvm_info(get_type_name(), "end of IP3 UVM ENV build phase", UVM_MEDIUM)
   endfunction : build_phase
 
   //
   // connect_phase method
   //
   function void connect_phase (uvm_phase phase);
     `uvm_info(get_type_name(), "start IP3 UVM ENV connect phase", UVM_MEDIUM)
     super.connect_phase(phase);
     `uvm_info(get_type_name(), "end of IP3 UVM ENV connect phase", UVM_MEDIUM)
   endfunction : connect_phase
 
   //
   // end_of_elaboration_phase method
   //
   function void end_of_elaboration_phase (uvm_phase phase);
     `uvm_info(get_type_name(), "start IP3 UVM ENV end_of_elaboration phase", UVM_MEDIUM)
     super.end_of_elaboration_phase(phase);
     `uvm_info(get_type_name(), "end of IP3 UVM ENV end_of_elaboration phase", UVM_MEDIUM)
   endfunction : end_of_elaboration_phase
 
   //
   // build_ip3UVMEnv method
   //
   function void build_ip3UVMEnv ();
 
     // creating both CFG and env's 
     ip3_uvm_env_cfg_inst = ip3_uvm_env_cfg::type_id::create("ip3_uvm_env_cfg_inst");
     ip3_uvm_env_inst     = ip3_uvm_env::type_id::create("ip3_uvm_env_inst", this);

     // set the ag't to passive mode
     ip3_uvm_env_cfg_inst.is_h1_agt_active = UVM_PASSIVE;
     ip3_uvm_env_cfg_inst.is_h2_agt_active = UVM_PASSIVE;

     // pass-in the IP3 reg_model from FC regmodel
     ip3_uvm_env_cfg_inst.regModel  = fc_cfg_obj_handle.reg_model._ip3_uvm_reg_blk;

     // set the IP3 ENV cfg down ip3 subenv
     uvm_config_db#(ip3_uvm_env_cfg)::set(null, "*.ip3_uvm_env_inst", "ip3_uvm_env_cfg", ip3_uvm_env_cfg_inst);

   endfunction : build_ip3UVMEnv
 
 endclass : vte_ip3Uvm_integ_env
 
 `endif // VTE_IP3_UVM_ENV_ENABLE
`endif // _vte_ip3Uvm_integ_env__svh_

