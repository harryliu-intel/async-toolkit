//----------------------------------------------------------------------------------------
// Copyright(C) 2016 Intel Corporation, Confidential Information
//----------------------------------------------------------------------------------------
// Author:  Dhivya Sankar
// Project: Madison Bay
// Description: Mesh flow control policy.
// The flow control policy enables the driver to send more packets based on credit 
// availability. It also generates credits that need to be returned to the DUT for all the 
// monitored requests/responses. 
//----------------------------------------------------------------------------------------

//----------------------------------------------------------------------------------------
// Class: mby_mgp_flow_ctrl
//----------------------------------------------------------------------------------------
class mby_mgp_flow_ctrl  extends uvm_component;
   `uvm_component_utils(mby_mgp_flow_ctrl)
 

   extern function new(string name = "", uvm_component parent = null);
   extern virtual function void build_phase(uvm_phase phase);
   
endclass 

//----------------------------------------------------------------------------------------
// Constructor
//----------------------------------------------------------------------------------------
function mby_mgp_flow_ctrl::new(string name = "", uvm_component parent = null);
   super.new(name, parent);
endfunction : new

//----------------------------------------------------------------------------------------
// Method: build
//----------------------------------------------------------------------------------------
function void mby_mgp_flow_ctrl::build_phase(uvm_phase phase);
   super.build_phase(phase);
endfunction : build_phase