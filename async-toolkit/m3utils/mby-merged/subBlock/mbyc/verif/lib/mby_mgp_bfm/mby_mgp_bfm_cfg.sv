//----------------------------------------------------------------------------------------
// Copyright(C) 2014 Intel Corporation, Confidential Information
//----------------------------------------------------------------------------------------
// Author: Dhivya Sankar
// Project: Madison Bay 
// Description: The configuration object for the Mesh Agent.
//----------------------------------------------------------------------------------------
typedef class mby_mgp_req_agent_cfg;
   
//----------------------------------------------------------------------------------------
// Class: mby_mgp_bfm_cfg
// mby_mgp_bfm configuration class
//----------------------------------------------------------------------------------------
class mby_mgp_bfm_cfg extends uvm_object;

   `uvm_object_utils(mby_mgp_bfm_cfg)

   bit driver_enable = 1;
   bit monitor_enable = 1;

   bus_type_e bus_type;
   req_type_e req_type;

   rand mby_mgp_req_agent_cfg req_agent_cfg;
   
   extern function new(string name = "");


endclass 

//----------------------------------------------------------------------------------------
// Constructor: new
// Create new configuration
//----------------------------------------------------------------------------------------
function mby_mgp_bfm_cfg::new (string name = "");

   super.new(name);

   req_agent_cfg = mby_mgp_req_agent_cfg::type_id::create("req_agent_cfg");
   
endfunction : new

