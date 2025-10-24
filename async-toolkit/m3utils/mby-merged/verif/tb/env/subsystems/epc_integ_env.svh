// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :  Subodh Nanal 
// Created On   :  10/15/2018
// Description  :  EPC integration environment.
// -----------------------------------------------------------------------------

`ifndef _epc_integ_env__svh_
 `define _epc_integ_env__svh_
 
`ifdef EPC_ENV_ENABLE
 
class epc_integ_env extends subsystem_base_env;
 
   // factory registration
   `uvm_component_utils(epc_integ_env)
   
   // epc env
   mby_ec_top_env       epc_env_inst[`NUM_EPC];

   // epc cfg
   mby_ec_top_env_cfg   epc_env_cfg_inst[`NUM_EPC];
 
   //
   // constructor
   //
   function new( string name ="epc_integ_env", uvm_component parent = null);
     super.new(name, parent);
   endfunction : new
 
   //
   // build_phase method
   //
   function void build_phase (uvm_phase phase);
     `uvm_info(get_type_name(), "start EPC ENV build phase", UVM_MEDIUM)
     super.build_phase(phase);
 
     build_epc_env();
 
     `uvm_info(get_type_name(), "end of EPC ENV build phase", UVM_MEDIUM)
   endfunction : build_phase
 
   //
   // connect_phase method
   //
   function void connect_phase (uvm_phase phase);
     `uvm_info(get_type_name(), "start EPC ENV connect phase", UVM_MEDIUM)
     super.connect_phase(phase);
     `uvm_info(get_type_name(), "end of EPC ENV connect phase", UVM_MEDIUM)
   endfunction : connect_phase
 
   //
   // end_of_elaboration_phase method
   //
   function void end_of_elaboration_phase (uvm_phase phase);
     `uvm_info(get_type_name(), "start EPC ENV end_of_elaboration phase", UVM_MEDIUM)
     super.end_of_elaboration_phase(phase);
     `uvm_info(get_type_name(), "end of EPC ENV end_of_elaboration phase", UVM_MEDIUM)
   endfunction : end_of_elaboration_phase
 
   //
   // build_epc_env method
   //
   function void build_epc_env ();
 
     // creating both CFG and env's 
     //for (int i=0; i<`NUM_EPC; i++) begin
     for (int i=0; i<1; i++) begin
         epc_env_inst[i] = mby_ec_top_env::type_id::create($sformatf("epc_env_inst_%0d", i), this);
         `uvm_info(get_name(),  $sformatf("build_epc_env: epc_env_inst[%0d] created",i),UVM_MEDIUM)

         epc_env_cfg_inst[i] = mby_ec_top_env_cfg#()::type_id::create($sformatf("epc_env_cfg_inst_%0d", i), this);
         `uvm_info(get_name(),  $sformatf("build_epc_env: epc_env_cfg_inst[%0d] created",i),UVM_MEDIUM)

         //Not required once 51e is available
         //epc_env_cfg_inst[i].endpoint_type = "dut";
         //epc_env_cfg_inst[i].local_lb_mode = eth_port_env_pkg::NO_LOCAL_LB;
         //epc_env_cfg_inst[i].remote_lb_mode = eth_port_env_pkg::NO_REMOTE_LB;

         //epc_env_cfg_inst[i].is_bfm_enabled = 1; //drive pkt from CDI BFM
         //epc_env_cfg_inst[i].dut_mode = eth_port_env_pkg::ENABLE_MAC;

         //Randomize the cfg and pass it to the env
         epc_env_cfg_inst[i].randomize();

         epc_env_inst[i].set_env_cfg(epc_env_cfg_inst[i]);
     end

     // pass-in the IP3 reg_model from FC regmodel
     //ingress_env_cfg_inst.regModel  = fc_cfg_obj_handle.reg_model._ingress_env_reg_blk;

     // set the IP3 ENV cfg down ip3 subenv
     //uvm_config_db#(ingress_env_env_cfg)::set(null, "*.ingress_env_env_inst", "ingress_env_env_cfg", ingress_env_env_cfg_inst);

   endfunction : build_epc_env
 
 endclass : epc_integ_env
 
 `endif // EPC_ENV_ENABLE
`endif // _epc_integ_env__svh_

