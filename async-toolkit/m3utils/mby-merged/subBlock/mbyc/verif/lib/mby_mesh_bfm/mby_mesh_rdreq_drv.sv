//----------------------------------------------------------------------------------------
// Copyright(C) 2016 Intel Corporation, Confidential Information
//----------------------------------------------------------------------------------------
// Author:  Dhivya Sankar
// Project: Madison Bay
// Description: Mesh read request driver.
// The read req driver drives read requests on the interface. They comprise of opcode and
// data. 
//----------------------------------------------------------------------------------------

//----------------------------------------------------------------------------------------
// Class: mby_mesh_rdreq_drv
//----------------------------------------------------------------------------------------
class mby_mesh_rdreq_drv  extends uvm_driver#(mby_mesh_req_seq_item);
   `uvm_component_utils_begin(mby_mesh_rdreq_drv)
   `uvm_component_utils_end
     
   mby_mesh_req_agent_cfg req_agent_cfg;

   mby_mesh_mem_crdt_io  mem_crdt_io;
   mby_mesh_flow_ctrl    flow_ctrl;
   
   extern function new(string name = "", uvm_component parent = null);
   extern virtual function void build_phase(uvm_phase phase);
   extern virtual function void reset();
   extern virtual function void start();
   extern virtual task run_phase(uvm_phase phase);
   extern virtual task sample_rqcrdt();
   extern virtual task prepare_rdreq();
   extern virtual task drive_rdreq();
   
endclass 

//----------------------------------------------------------------------------------------
// Constructor
//----------------------------------------------------------------------------------------
function mby_mesh_rdreq_drv::new(string name = "", uvm_component parent = null);
   super.new(name, parent);
endfunction

//----------------------------------------------------------------------------------------
// Method: build
//----------------------------------------------------------------------------------------
function void mby_mesh_rdreq_drv::build_phase(uvm_phase phase);
   super.build_phase(phase);
endfunction
   
//----------------------------------------------------------------------------------------
// Method: reset
//----------------------------------------------------------------------------------------
function void mby_mesh_rdreq_drv::reset();
endfunction 

//----------------------------------------------------------------------------------------
// Method: start
//----------------------------------------------------------------------------------------
function void mby_mesh_rdreq_drv::start();
endfunction 

//----------------------------------------------------------------------------------------
// Method: run
//----------------------------------------------------------------------------------------
task mby_mesh_rdreq_drv::run_phase(uvm_phase phase);

   if (req_agent_cfg.is_active == UVM_ACTIVE) begin
      fork
         forever @(mem_crdt_io.vif.mst_cb) begin
            if(!mem_crdt_io.vif.rst) begin
               sample_rqcrdt();
               prepare_rdreq();
               drive_rdreq();
            end
         end
      join_none
   end 
   
endtask 

//----------------------------------------------------------------------------------------
// Method: sample_rqcrdt
//----------------------------------------------------------------------------------------
task mby_mesh_rdreq_drv::sample_rqcrdt();

endtask 

//----------------------------------------------------------------------------------------
// Method: prepare_rdreq
//----------------------------------------------------------------------------------------
task mby_mesh_rdreq_drv::prepare_rdreq();

endtask 


//----------------------------------------------------------------------------------------
// Method: drive_rdreq
//----------------------------------------------------------------------------------------
task mby_mesh_rdreq_drv::drive_rdreq();

endtask