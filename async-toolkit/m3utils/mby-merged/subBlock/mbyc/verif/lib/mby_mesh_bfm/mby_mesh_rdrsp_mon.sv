   
//----------------------------------------------------------------------------------------
// Copyright(C) 2016 Intel Corporation, Confidential Information
//----------------------------------------------------------------------------------------
// Author:  Dhivya Sankar
// Project: Madison Bay
// Description: Mesh read response monitor.
// The read rsp monitor monitors read responses on the interface and returns credits. 
// The response comprise of opcode and data. 
//----------------------------------------------------------------------------------------

//----------------------------------------------------------------------------------------
// Class: mby_mesh_rdrsp_mon
//----------------------------------------------------------------------------------------
class mby_mesh_rdrsp_mon  extends uvm_monitor;
   `uvm_component_utils_begin(mby_mesh_rdrsp_mon)
   `uvm_component_utils_end
     
   mby_mesh_req_agent_cfg req_agent_cfg;

   mby_mesh_mem_crdt_io  mem_crdt_io;
   mby_mesh_flow_ctrl    flow_ctrl;
   
   extern function new(string name = "", uvm_component parent = null);
   extern virtual function void build_phase(uvm_phase phase);
   extern virtual function void reset();
   extern virtual function void start();
   extern virtual task run_phase(uvm_phase phase);
   extern virtual task sample_rdrsp();
   extern virtual task prepare_rpcrdt();
   extern virtual task drive_rpcrdt();
   
endclass 

//----------------------------------------------------------------------------------------
// Constructor
//----------------------------------------------------------------------------------------
function mby_mesh_rdrsp_mon::new(string name = "", uvm_component parent = null);
   super.new(name, parent);
endfunction

//----------------------------------------------------------------------------------------
// Method: build
//----------------------------------------------------------------------------------------
function void mby_mesh_rdrsp_mon::build_phase(uvm_phase phase);
   super.build_phase(phase);
endfunction
   
//----------------------------------------------------------------------------------------
// Method: reset
//----------------------------------------------------------------------------------------
function void mby_mesh_rdrsp_mon::reset();
endfunction 

//----------------------------------------------------------------------------------------
// Method: start
//----------------------------------------------------------------------------------------
function void mby_mesh_rdrsp_mon::start();
endfunction 

//----------------------------------------------------------------------------------------
// Method: run
//----------------------------------------------------------------------------------------
task mby_mesh_rdrsp_mon::run_phase(uvm_phase phase);

   if (req_agent_cfg.is_active == UVM_ACTIVE) begin
      fork
         forever @(mem_crdt_io.vif.mst_cb) begin
            if(!mem_crdt_io.vif.rst) begin
               sample_rdrsp();
               prepare_rpcrdt();
               drive_rpcrdt();
            end
         end
      join_none
   end 
   
endtask 

//----------------------------------------------------------------------------------------
// Method: sample_rdrsp
//----------------------------------------------------------------------------------------
task mby_mesh_rdrsp_mon::sample_rdrsp();

endtask 

//----------------------------------------------------------------------------------------
// Method: prepare_rpcrdt
//----------------------------------------------------------------------------------------
task mby_mesh_rdrsp_mon::prepare_rpcrdt();

endtask 


//----------------------------------------------------------------------------------------
// Method: drive_rpcrdt
//----------------------------------------------------------------------------------------
task mby_mesh_rdrsp_mon::drive_rpcrdt();

endtask