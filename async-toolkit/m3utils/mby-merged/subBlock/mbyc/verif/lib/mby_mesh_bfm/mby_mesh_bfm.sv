//----------------------------------------------------------------------------------------
// Copyright(C) 2014 Intel Corporation, Confidential Information
//----------------------------------------------------------------------------------------
// Author:  Dhivya Sankar
// Project: Madison Bay
// Description: The mesh bfm comprises of all the 2 mesh agents, one set up in master mode
// and the other in slave mode.  
//----------------------------------------------------------------------------------------

//----------------------------------------------------------------------------------------
// Class: mby_mesh_bfm
//----------------------------------------------------------------------------------------
class mby_mesh_bfm extends uvm_component;

    `uvm_component_utils(mby_mesh_bfm)

   mby_mesh_agent mesh_mst_agent;
   mby_mesh_agent mesh_slv_agent;

   extern function new(string name = "", uvm_component parent);
   extern virtual function void build_phase(uvm_phase phase);
   extern virtual function void connect_phase(uvm_phase phase);
   extern virtual function void end_of_elaboration_phase(uvm_phase phase);
   extern virtual task run_phase(uvm_phase phase);
endclass 

//----------------------------------------------------------------------------------------
// Constructor
//----------------------------------------------------------------------------------------
function mby_mesh_bfm::new(string name = "", uvm_component parent);
   super.new(name, parent);
 
endfunction 

//----------------------------------------------------------------------------------------
// Method: build
//----------------------------------------------------------------------------------------
function void mby_mesh_bfm::build_phase(uvm_phase phase);
   super.build_phase(phase);

   if (bfm_cfg == null) begin
      `uvm_fatal(get_name(), "build: mesh bfm configuration handle is null.")
   end

   mesh_mst_agent = mby_mesh_agent::type_id::create("mesh_mst_agent");
   mesh_slv_agent = mby_mesh_agent::type_id::create("mesh_slv_agent");

   endfunction 

//----------------------------------------------------------------------------------------
// Method: connect
//----------------------------------------------------------------------------------------
function void mby_mesh_bfm::connect_phase(uvm_phase phase);
   
   super.connect_phase(phase);
   
endfunction 

//----------------------------------------------------------------------------------------
// Method: end_of_elaboration
//----------------------------------------------------------------------------------------
function void mby_mesh_bfm::end_of_elaboration_phase(uvm_phase phase);
   
   super.end_of_elaboration_phase(phase);
   
endfunction
//----------------------------------------------------------------------------------------
// Method: run
//----------------------------------------------------------------------------------------
task mby_mesh_bfm::run_phase(uvm_phase phase);
   
endtask 