//----------------------------------------------------------------------------------------
// Copyright(C) 2016 Intel Corporation, Confidential Information
//----------------------------------------------------------------------------------------
// Author:  Dhivya Sankar
// Project: Madison Bay
// Description: Mesh read response driver.
// The read rsp driver drives read responses on the interface. 
//----------------------------------------------------------------------------------------

//----------------------------------------------------------------------------------------
// Class: mby_mgp_rdrsp_drv
//----------------------------------------------------------------------------------------
class mby_mgp_rdrsp_drv  extends uvm_driver#(mby_mgp_req_seq_item);
   `uvm_component_utils(mby_mgp_rdrsp_drv)
        
   mby_mgp_req_agent_cfg req_agent_cfg;

   mby_mgp_mem_crdt_io  mem_crdt_io;
   mby_mgp_flow_ctrl    flow_ctrl;
   
   extern function new(string name = "", uvm_component parent = null);
   extern virtual function void build_phase(uvm_phase phase);
   extern virtual function void reset();
   extern virtual function void start();
   extern virtual task run_phase(uvm_phase phase);
   extern virtual task sample_rpcrdt();
   extern virtual task prepare_rdrsp();
   extern virtual task drive_rdrsp();
   
endclass 

//----------------------------------------------------------------------------------------
// Constructor
//----------------------------------------------------------------------------------------
function mby_mgp_rdrsp_drv::new(string name = "", uvm_component parent = null);
   super.new(name, parent);
endfunction : new

//----------------------------------------------------------------------------------------
// Method: build
//----------------------------------------------------------------------------------------
function void mby_mgp_rdrsp_drv::build_phase(uvm_phase phase);
   super.build_phase(phase);
endfunction : build_phase
   
//----------------------------------------------------------------------------------------
// Method: reset
//----------------------------------------------------------------------------------------
function void mby_mgp_rdrsp_drv::reset();
endfunction : reset

//----------------------------------------------------------------------------------------
// Method: start
//----------------------------------------------------------------------------------------
function void mby_mgp_rdrsp_drv::start();
endfunction : start

//----------------------------------------------------------------------------------------
// Method: run
//----------------------------------------------------------------------------------------
task mby_mgp_rdrsp_drv::run_phase(uvm_phase phase);

   if (req_agent_cfg.is_active == UVM_ACTIVE) begin
      fork
         forever @(mem_crdt_io.op_vif.op_mst_cb) begin
            if(!mem_crdt_io.op_vif.rst) begin
               sample_rpcrdt();
               prepare_rdrsp();
               drive_rdrsp();
            end
         end
      join_none
   end 
   
endtask : run_phase

//----------------------------------------------------------------------------------------
// Method: sample_rpcrdt
//----------------------------------------------------------------------------------------
task mby_mgp_rdrsp_drv::sample_rpcrdt();

endtask : sample_rpcrdt

//----------------------------------------------------------------------------------------
// Method: prepare_rdrsp
//----------------------------------------------------------------------------------------
task mby_mgp_rdrsp_drv::prepare_rdrsp();

endtask : prepare_rdrsp


//----------------------------------------------------------------------------------------
// Method: drive_rdrsp
//----------------------------------------------------------------------------------------
task mby_mgp_rdrsp_drv::drive_rdrsp();

endtask : drive_rdrsp