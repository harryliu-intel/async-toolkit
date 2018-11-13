//----------------------------------------------------------------------------------------
// Copyright(C) 2016 Intel Corporation, Confidential Information
//----------------------------------------------------------------------------------------
// Author:  Dhivya Sankar
// Project: Madison Bay
// Description: Mesh IO policy.
// The IO policy is a class that drives/monitors the signals on the respective interfaces.
//----------------------------------------------------------------------------------------
typedef class mby_mgp_req_seq_item;
   
//----------------------------------------------------------------------------------------
// Class: mby_mgp_mem_crdt_io
//----------------------------------------------------------------------------------------
class mby_mgp_mem_crdt_io  extends uvm_component;
   `uvm_component_utils(mby_mgp_mem_crdt_io)
  

   virtual mby_mgp_op_if op_vif;
   virtual mby_mgp_data_if data_vif;

   uvm_analysis_port#(uvm_object) mon_op_port;
   uvm_analysis_port#(uvm_object) mon_data_port;

   mby_mgp_req_seq_item rdreq_q[$];
   
   
   extern function new(string name = "", uvm_component parent = null);
   extern virtual function void build_phase(uvm_phase phase);
 
endclass 

//----------------------------------------------------------------------------------------
// Constructor
//----------------------------------------------------------------------------------------
function mby_mgp_mem_crdt_io::new(string name = "", uvm_component parent = null);
   super.new(name, parent);
endfunction : new

//----------------------------------------------------------------------------------------
// Method: build
//----------------------------------------------------------------------------------------
function void mby_mgp_mem_crdt_io::build_phase(uvm_phase phase);
   super.build_phase(phase);
endfunction : build_phase

