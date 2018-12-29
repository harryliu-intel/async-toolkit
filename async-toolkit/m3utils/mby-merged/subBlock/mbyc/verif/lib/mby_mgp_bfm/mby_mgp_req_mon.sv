   
//----------------------------------------------------------------------------------------
// Copyright(C) 2016 Intel Corporation, Confidential Information
//----------------------------------------------------------------------------------------
// Author:  Dhivya Sankar
// Project: Madison Bay
// Description: Mesh request monitor.
// The req monitor monitors  requests/responses on the interface and returns credits. 
//----------------------------------------------------------------------------------------

//----------------------------------------------------------------------------------------
// Class: mby_mgp_req_mon
// Monitor class
//----------------------------------------------------------------------------------------
class mby_mgp_req_mon  extends uvm_monitor;
   `uvm_component_utils(mby_mgp_req_mon)
    
   mby_mgp_req_agent_cfg req_agent_cfg;

   mby_mgp_mem_crdt_io   mem_crdt_io;
   mby_mgp_flow_ctrl     flow_ctrl;
   
   extern function new(string name = "", uvm_component parent = null);
   extern virtual function void build_phase(uvm_phase phase);
   extern virtual function void reset();
   extern virtual function void start();
   extern virtual task run_phase(uvm_phase phase);
   extern virtual task sample_req();
   extern virtual task prepare_rqcrdt();
   extern virtual task drive_rqcrdt();
   
endclass 

//----------------------------------------------------------------------------------------
// Constructor
//----------------------------------------------------------------------------------------
function mby_mgp_req_mon::new(string name = "", uvm_component parent = null);
   super.new(name, parent);
endfunction : new

//----------------------------------------------------------------------------------------
// Method: build
//----------------------------------------------------------------------------------------
function void mby_mgp_req_mon::build_phase(uvm_phase phase);
   super.build_phase(phase);
endfunction : build_phase
   
//----------------------------------------------------------------------------------------
// Method: reset
//----------------------------------------------------------------------------------------
function void mby_mgp_req_mon::reset();
endfunction : reset

//----------------------------------------------------------------------------------------
// Method: start
//----------------------------------------------------------------------------------------
function void mby_mgp_req_mon::start();
endfunction : start

//----------------------------------------------------------------------------------------
// Method: run
//----------------------------------------------------------------------------------------
task mby_mgp_req_mon::run_phase(uvm_phase phase);

   if (req_agent_cfg.monitor_enable) begin
/*      fork
         forever @(mem_crdt_io.op_vif.op_mst_cb) begin
            if(!mem_crdt_io.op_vif.rst) begin
               sample_req();
               prepare_rqcrdt();
               drive_rqcrdt();
            end
         end
      join_none*/
   end 
   
endtask : run_phase

//----------------------------------------------------------------------------------------
// Method: sample_req
//----------------------------------------------------------------------------------------
task mby_mgp_req_mon::sample_req();

endtask : sample_req

//----------------------------------------------------------------------------------------
// Method: prepare_rqcrdt
//----------------------------------------------------------------------------------------
task mby_mgp_req_mon::prepare_rqcrdt();

endtask : prepare_rqcrdt


//----------------------------------------------------------------------------------------
// Method: drive_rqcrdt
//----------------------------------------------------------------------------------------
task mby_mgp_req_mon::drive_rqcrdt();

endtask : drive_rqcrdt
