   
//----------------------------------------------------------------------------------------
// Copyright(C) 2016 Intel Corporation, Confidential Information
//----------------------------------------------------------------------------------------
// Author:  Dhivya Sankar
// Project: Madison Bay
// Description: Mesh read request monitor.
// The read req monitor monitors read requests on the interface and returns credits. 
//----------------------------------------------------------------------------------------

//----------------------------------------------------------------------------------------
// Class: mby_mgp_rdreq_mon
//----------------------------------------------------------------------------------------
class mby_mgp_rdreq_mon  extends uvm_monitor;
   `uvm_component_utils(mby_mgp_rdreq_mon)
    
   mby_mgp_req_agent_cfg req_agent_cfg;

   mby_mgp_mem_crdt_io  mem_crdt_io;
   mby_mgp_flow_ctrl    flow_ctrl;
   
   extern function new(string name = "", uvm_component parent = null);
   extern virtual function void build_phase(uvm_phase phase);
   extern virtual function void reset();
   extern virtual function void start();
   extern virtual task run_phase(uvm_phase phase);
   extern virtual task sample_rdreq();
   extern virtual task prepare_rqcrdt();
   extern virtual task drive_rqcrdt();
   
endclass 

//----------------------------------------------------------------------------------------
// Constructor
//----------------------------------------------------------------------------------------
function mby_mgp_rdreq_mon::new(string name = "", uvm_component parent = null);
   super.new(name, parent);
endfunction : new

//----------------------------------------------------------------------------------------
// Method: build
//----------------------------------------------------------------------------------------
function void mby_mgp_rdreq_mon::build_phase(uvm_phase phase);
   super.build_phase(phase);
endfunction : build_phase
   
//----------------------------------------------------------------------------------------
// Method: reset
//----------------------------------------------------------------------------------------
function void mby_mgp_rdreq_mon::reset();
endfunction : reset

//----------------------------------------------------------------------------------------
// Method: start
//----------------------------------------------------------------------------------------
function void mby_mgp_rdreq_mon::start();
endfunction : start

//----------------------------------------------------------------------------------------
// Method: run
//----------------------------------------------------------------------------------------
task mby_mgp_rdreq_mon::run_phase(uvm_phase phase);

   if (req_agent_cfg.is_active == UVM_ACTIVE) begin
      fork
         forever @(mem_crdt_io.op_vif.op_mst_cb) begin
            if(!mem_crdt_io.op_vif.rst) begin
               sample_rdreq();
               prepare_rqcrdt();
               drive_rqcrdt();
            end
         end
      join_none
   end 
   
endtask : run_phase

//----------------------------------------------------------------------------------------
// Method: sample_rdreq
//----------------------------------------------------------------------------------------
task mby_mgp_rdreq_mon::sample_rdreq();

endtask : sample_rdreq

//----------------------------------------------------------------------------------------
// Method: prepare_rdreq
//----------------------------------------------------------------------------------------
task mby_mgp_rdreq_mon::prepare_rqcrdt();

endtask : prepare_rqcrdt


//----------------------------------------------------------------------------------------
// Method: drive_rqcrdt
//----------------------------------------------------------------------------------------
task mby_mgp_rdreq_mon::drive_rqcrdt();

endtask : drive_rqcrdt