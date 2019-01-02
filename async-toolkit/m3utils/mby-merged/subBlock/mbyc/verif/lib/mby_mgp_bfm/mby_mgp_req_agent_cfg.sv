//----------------------------------------------------------------------------------------
// Copyright(C) 2014 Intel Corporation, Confidential Information
//----------------------------------------------------------------------------------------
// Author: Dhivya Sankar
// Project: Madison Bay 
// Description: The configuration object for the read/write req/rsp agent.
//----------------------------------------------------------------------------------------
typedef enum bus_type_e;
//----------------------------------------------------------------------------------------
// Class: mby_mgp_req_agent_cfg
// Configuration class for req_agent
//----------------------------------------------------------------------------------------
class mby_mgp_req_agent_cfg extends uvm_object;

   `uvm_object_utils(mby_mgp_req_agent_cfg)
   bit driver_enable = 1;
   bit monitor_enable = 1;

   bus_type_e bus_type;
   
   extern function new(string name = "");
   

endclass 

//----------------------------------------------------------------------------------------
// Constructor
// Creating new req agent
//----------------------------------------------------------------------------------------
function mby_mgp_req_agent_cfg::new (string name = "");

   super.new(name);
   
endfunction : new
