// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :  Subodh Nanal 
// Created On   :  09/22/2018
// Description  :  IGR integration environment.
// -----------------------------------------------------------------------------

`ifndef _igr_integ_env__svh_
 `define _igr_integ_env__svh_
 
`ifdef IGR_ENV_ENABLE
 
class igr_integ_env extends subsystem_base_env;
 
   // factory registration
   `uvm_component_utils(igr_integ_env)
   
   // igr env
   ingress_env       ingress_env_inst[`NUM_IGR];
 
   //
   // constructor
   //
   function new( string name ="igr_integ_env", uvm_component parent = null);
     super.new(name, parent);
   endfunction : new
 
   //
   // build_phase method
   //
   function void build_phase (uvm_phase phase);
     `uvm_info(get_type_name(), "start IGR ENV build phase", UVM_MEDIUM)
     super.build_phase(phase);
 
     build_igr_env();
 
     `uvm_info(get_type_name(), "end of IGR ENV build phase", UVM_MEDIUM)
   endfunction : build_phase
 
   //
   // connect_phase method
   //
   function void connect_phase (uvm_phase phase);
     `uvm_info(get_type_name(), "start IGR ENV connect phase", UVM_MEDIUM)
     super.connect_phase(phase);
     `uvm_info(get_type_name(), "end of IGR ENV connect phase", UVM_MEDIUM)
   endfunction : connect_phase
 
   //
   // end_of_elaboration_phase method
   //
   function void end_of_elaboration_phase (uvm_phase phase);
     `uvm_info(get_type_name(), "start IGR ENV end_of_elaboration phase", UVM_MEDIUM)
     super.end_of_elaboration_phase(phase);
     `uvm_info(get_type_name(), "end of IGR ENV end_of_elaboration phase", UVM_MEDIUM)
   endfunction : end_of_elaboration_phase
 
   //
   // build_igr_env method
   //
   function void build_igr_env ();
 
     // creating both CFG and env's 
     for (int i=0; i<`NUM_IGR; i++) begin
         ingress_env_inst[i] = ingress_env::type_id::create($sformatf("ingress_env_inst_%0d", i), this);
         ingress_env_inst[i].set_level(SLA_SUB);
         `uvm_info(get_name(),  $sformatf("build_igr_env: ingress_env_inst[%0d] created",i),UVM_MEDIUM)
     end

     // pass-in the IP3 reg_model from FC regmodel
     //ingress_env_cfg_inst.regModel  = fc_cfg_obj_handle.reg_model._ingress_env_reg_blk;

     // set the IP3 ENV cfg down ip3 subenv
     //uvm_config_db#(ingress_env_env_cfg)::set(null, "*.ingress_env_env_inst", "ingress_env_env_cfg", ingress_env_env_cfg_inst);

   endfunction : build_igr_env
 
 endclass : igr_integ_env
 
 `endif // IGR_ENV_ENABLE
`endif // _igr_integ_env__svh_

