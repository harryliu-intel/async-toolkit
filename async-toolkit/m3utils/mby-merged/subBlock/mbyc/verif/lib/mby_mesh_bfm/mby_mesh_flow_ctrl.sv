//----------------------------------------------------------------------------------------
// Copyright(C) 2016 Intel Corporation, Confidential Information
//----------------------------------------------------------------------------------------
// Author:  Dhivya Sankar
// Project: Madison Bay
// Description: Mesh flow control policy.
// The flow control policy enables to driver to send more packets based on credit 
// availability. It also generates credits that need to be returned for all the requests/
// responses sent by the DUT. 
//----------------------------------------------------------------------------------------

//----------------------------------------------------------------------------------------
// Class: mby_mesh_flow_ctrl
//----------------------------------------------------------------------------------------
class mby_mesh_flow_ctrl  extends uvm_component;
   `uvm_component_utils_begin(mby_mesh_flow_ctrl)
   `uvm_component_utils_end

   extern function new(string name = "", uvm_component parent = null);
   extern virtual function void build_phase(uvm_phase phase);
   
endclass 

//----------------------------------------------------------------------------------------
// Constructor
//----------------------------------------------------------------------------------------
function mby_mesh_flow_ctrl::new(string name = "", uvm_component parent = null);
   super.new(name, parent);
endfunction

//----------------------------------------------------------------------------------------
// Method: build
//----------------------------------------------------------------------------------------
function void mby_mesh_flow_ctrl::build_phase(uvm_phase phase);
   super.build_phase(phase);
endfunction