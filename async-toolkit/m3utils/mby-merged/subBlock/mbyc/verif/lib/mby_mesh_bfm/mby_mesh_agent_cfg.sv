//----------------------------------------------------------------------------------------
// Copyright(C) 2014 Intel Corporation, Confidential Information
//----------------------------------------------------------------------------------------
// Author: Dhivya Sankar
// Project: Madison Bay 
// Description: The configuration object for the Mesh Agent.
//----------------------------------------------------------------------------------------
typedef class mby_mesh_req_agent_cfg;
   
//----------------------------------------------------------------------------------------
// Class: mby_mesh_agent_cfg
//----------------------------------------------------------------------------------------
class mby_mesh_agent_cfg extends uvm_object;

   `uvm_object_utils(mby_mesh_agent_cfg)
   typedef uvm_active_passive_enum act_psv_e;
   uvm_active_passive_enum is_active = UVM_ACTIVE;

   rand mby_mesh_req_agent_cfg req_agent_cfg;
   
   
   extern function new(string name = "");
   

endclass 

//----------------------------------------------------------------------------------------
// Constructor
//----------------------------------------------------------------------------------------
function mby_mesh_agent_cfg::new (string name = "");

   super.new(name);
   req_agent_cfg = mby_mesh_req_agent_cfg::type_id::create("req_agent_cfg");
   
endfunction 

