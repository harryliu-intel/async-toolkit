//----------------------------------------------------------------------------------------
// Copyright(C) 2014 Intel Corporation, Confidential Information
//----------------------------------------------------------------------------------------
// Author:  Dhivya Sankar
// Project: Madison Bay
// Description: The mgp bfm is comprised of the 2 mgp agents, one set up in master mode
// and the other in slave mode.  
//----------------------------------------------------------------------------------------

//----------------------------------------------------------------------------------------
// Class: mby_mgp_bfm
//----------------------------------------------------------------------------------------
class mby_mgp_bfm extends uvm_component;

    `uvm_component_utils(mby_mgp_bfm)

   mby_mgp_agent mgp_mst_agent;
   mby_mgp_agent mgp_slv_agent;

   extern function new(string name = "", uvm_component parent);
   extern virtual function void build_phase(uvm_phase phase);
   extern virtual function void connect_phase(uvm_phase phase);
   extern virtual function void end_of_elaboration_phase(uvm_phase phase);
   extern virtual task run_phase(uvm_phase phase);
endclass 

//----------------------------------------------------------------------------------------
// Constructor
//----------------------------------------------------------------------------------------
function mby_mgp_bfm::new(string name = "", uvm_component parent);
   super.new(name, parent);
 
endfunction : new

//----------------------------------------------------------------------------------------
// Method: build
//----------------------------------------------------------------------------------------
function void mby_mgp_bfm::build_phase(uvm_phase phase);
   super.build_phase(phase);

   if (bfm_cfg == null) begin
      `uvm_fatal(get_name(), "build: mesh bfm configuration handle is null.")
   end

   mgp_mst_agent = mby_mgp_agent::type_id::create("mgp_mst_agent", this);
   mgp_slv_agent = mby_mgp_agent::type_id::create("mgp_slv_agent", this);

endfunction : build_phase

//----------------------------------------------------------------------------------------
// Method: connect
//----------------------------------------------------------------------------------------
function void mby_mgp_bfm::connect_phase(uvm_phase phase);
   
   super.connect_phase(phase);
   
endfunction : connect_phase

//----------------------------------------------------------------------------------------
// Method: end_of_elaboration
//----------------------------------------------------------------------------------------
function void mby_mgp_bfm::end_of_elaboration_phase(uvm_phase phase);
   
   super.end_of_elaboration_phase(phase);
   
endfunction : end_of_elaboration_phase
//----------------------------------------------------------------------------------------
// Method: run
//----------------------------------------------------------------------------------------
task mby_mgp_bfm::run_phase(uvm_phase phase);
   
endtask : run_phase