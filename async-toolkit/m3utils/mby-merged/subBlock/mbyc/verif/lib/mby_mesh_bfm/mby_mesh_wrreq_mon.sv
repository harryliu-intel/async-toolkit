   
//----------------------------------------------------------------------------------------
// Copyright(C) 2016 Intel Corporation, Confidential Information
//----------------------------------------------------------------------------------------
// Author:  Dhivya Sankar
// Project: Madison Bay
// Description: Mesh read request monitor.
// The read req monitor monitors read requests on the interface and returns credits. 
// The requests comprise of opcode and data. 
//----------------------------------------------------------------------------------------

//----------------------------------------------------------------------------------------
// Class: mby_mesh_wrreq_mon
//----------------------------------------------------------------------------------------
class mby_mesh_wrreq_mon  extends uvm_monitor;
   `uvm_component_utils_begin(mby_mesh_wrreq_mon)
   `uvm_component_utils_end
     
   mby_mesh_req_agent_cfg req_agent_cfg;

   mby_mesh_mem_crdt_io  mem_crdt_io;
   mby_mesh_flow_ctrl    flow_ctrl;
   
   extern function new(string name = "", uvm_component parent = null);
   extern virtual function void build_phase(uvm_phase phase);
   extern virtual function void reset();
   extern virtual function void start();
   extern virtual task run_phase(uvm_phase phase);
   extern virtual task sample_wrreq();
   extern virtual task prepare_wqcrdt();
   extern virtual task drive_wqcrdt();
   
endclass 

//----------------------------------------------------------------------------------------
// Constructor
//----------------------------------------------------------------------------------------
function mby_mesh_wrreq_mon::new(string name = "", uvm_component parent = null);
   super.new(name, parent);
endfunction

//----------------------------------------------------------------------------------------
// Method: build
//----------------------------------------------------------------------------------------
function void mby_mesh_wrreq_mon::build_phase(uvm_phase phase);
   super.build_phase(phase);
endfunction
   
//----------------------------------------------------------------------------------------
// Method: reset
//----------------------------------------------------------------------------------------
function void mby_mesh_wrreq_mon::reset();
endfunction 

//----------------------------------------------------------------------------------------
// Method: start
//----------------------------------------------------------------------------------------
function void mby_mesh_wrreq_mon::start();
endfunction 

//----------------------------------------------------------------------------------------
// Method: run
//----------------------------------------------------------------------------------------
task mby_mesh_wrreq_mon::run_phase(uvm_phase phase);

   if (req_agent_cfg.is_active == UVM_ACTIVE) begin
      fork
         forever @(mem_crdt_io.vif.mst_cb) begin
            if(!mem_crdt_io.vif.rst) begin
               sample_wrreq();
               prepare_wqcrdt();
               drive_wqcrdt();
            end
         end
      join_none
   end 
   
endtask 

//----------------------------------------------------------------------------------------
// Method: sample_wrreq
//----------------------------------------------------------------------------------------
task mby_mesh_wrreq_mon::sample_wrreq();

endtask 

//----------------------------------------------------------------------------------------
// Method: prepare_wrreq
//----------------------------------------------------------------------------------------
task mby_mesh_wrreq_mon::prepare_wqcrdt();

endtask 


//----------------------------------------------------------------------------------------
// Method: drive_wqcrdt
//----------------------------------------------------------------------------------------
task mby_mesh_wrreq_mon::drive_wqcrdt();

endtask