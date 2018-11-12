
//----------------------------------------------------------------------------------------
// Copyright(C) 2016 Intel Corporation, Confidential Information
//----------------------------------------------------------------------------------------
// Author:  Dhivya Sankar
// Project: Madison Bay
// Description: Mesh write request driver.
// The write req driver drives write requests on the interface. 
//----------------------------------------------------------------------------------------

//----------------------------------------------------------------------------------------
// Class: mby_mgp_wrreq_drv
//----------------------------------------------------------------------------------------
class mby_mgp_wrreq_drv  extends uvm_driver#(mby_mgp_req_seq_item);
   `uvm_component_utils(mby_mgp_wrreq_drv)
     
   mby_mgp_req_agent_cfg req_agent_cfg;

   mby_mgp_mem_crdt_io  mem_crdt_io;
   mby_mgp_flow_ctrl    flow_ctrl;
   
   extern function new(string name = "", uvm_component parent = null);
   extern virtual function void build_phase(uvm_phase phase);
   extern virtual function void reset();
   extern virtual function void start();
   extern virtual task run_phase(uvm_phase phase);
   extern virtual task sample_wqcrdt();
   extern virtual task prepare_wrreq();
   extern virtual task drive_wrreq();
   
endclass 

//----------------------------------------------------------------------------------------
// Constructor
//----------------------------------------------------------------------------------------
function mby_mgp_wrreq_drv::new(string name = "", uvm_component parent = null);
   super.new(name, parent);
endfunction : new

//----------------------------------------------------------------------------------------
// Method: build
//----------------------------------------------------------------------------------------
function void mby_mgp_wrreq_drv::build_phase(uvm_phase phase);
   super.build_phase(phase);
endfunction : build_phase
   
//----------------------------------------------------------------------------------------
// Method: reset
//----------------------------------------------------------------------------------------
function void mby_mgp_wrreq_drv::reset();
endfunction : reset

//----------------------------------------------------------------------------------------
// Method: start
//----------------------------------------------------------------------------------------
function void mby_mgp_wrreq_drv::start();
endfunction : start

//----------------------------------------------------------------------------------------
// Method: run
//----------------------------------------------------------------------------------------
task mby_mgp_wrreq_drv::run_phase(uvm_phase phase);

   if (req_agent_cfg.is_active == UVM_ACTIVE) begin
      fork
         forever @(mem_crdt_io.op_vif.op_mst_cb) begin
            if(!mem_crdt_io.op_vif.rst) begin
               sample_wqcrdt();
               prepare_wrreq();
               drive_wrreq();
            end
         end
      join_none
   end 
   
endtask : run_phase

//----------------------------------------------------------------------------------------
// Method: sample_wqcrdt
//----------------------------------------------------------------------------------------
task mby_mgp_wrreq_drv::sample_wqcrdt();

endtask : sample_wqcrdt

//----------------------------------------------------------------------------------------
// Method: prepare_wrreq
//----------------------------------------------------------------------------------------
task mby_mgp_wrreq_drv::prepare_wrreq();

endtask : prepare_wrreq


//----------------------------------------------------------------------------------------
// Method: drive_wrreq
//----------------------------------------------------------------------------------------
task mby_mgp_wrreq_drv::drive_wrreq();

endtask : drive_wrreq