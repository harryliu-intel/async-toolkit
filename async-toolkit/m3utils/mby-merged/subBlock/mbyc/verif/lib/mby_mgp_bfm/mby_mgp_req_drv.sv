// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

//----------------------------------------------------------------------------------------
// Copyright(C) 2016 Intel Corporation, Confidential Information
//----------------------------------------------------------------------------------------
// Author:  Dhivya Sankar
// Project: Madison Bay
// Description: Mesh request driver.
// The req driver drives read/write requests on the interface. 
//----------------------------------------------------------------------------------------

//----------------------------------------------------------------------------------------
// Class: mby_mgp_req_drv
// Class to send rd/wr req to mesh
//----------------------------------------------------------------------------------------
class mby_mgp_req_drv  extends uvm_driver#(mby_mgp_req_seq_item);
   `uvm_component_utils_begin(mby_mgp_req_drv)
   `uvm_component_utils_end
     
   mby_mgp_req_agent_cfg req_agent_cfg;

   mby_mgp_mem_crdt_io   mem_crdt_io;
   mby_mgp_flow_ctrl     flow_ctrl;
   int port_num;
   
   extern function new(string name = "", uvm_component parent = null);
   extern virtual function void build_phase(uvm_phase phase);
   extern virtual function void reset();
   extern virtual function void start();
   extern virtual task run_phase(uvm_phase phase);
   
endclass 

//----------------------------------------------------------------------------------------
// Constructor
//----------------------------------------------------------------------------------------
function mby_mgp_req_drv::new(string name = "", uvm_component parent = null);
   super.new(name, parent);
endfunction : new

//----------------------------------------------------------------------------------------
// Method: build
//----------------------------------------------------------------------------------------
function void mby_mgp_req_drv::build_phase(uvm_phase phase);
   super.build_phase(phase);
endfunction : build_phase
   
//----------------------------------------------------------------------------------------
// Method: reset
//----------------------------------------------------------------------------------------
function void mby_mgp_req_drv::reset();
endfunction : reset

//----------------------------------------------------------------------------------------
// Method: start
//----------------------------------------------------------------------------------------
function void mby_mgp_req_drv::start();
endfunction : start

//----------------------------------------------------------------------------------------
// Method: run
//----------------------------------------------------------------------------------------
task mby_mgp_req_drv::run_phase(uvm_phase phase);
   mby_mgp_req_seq_item  req, req_c;
   int cnt;
   
   if (req_agent_cfg.driver_enable && !mem_crdt_io.rdreq_vif.reset) begin
      fork
         forever begin
	    mem_crdt_io.step();
	    seq_item_port.try_next_item(req);
	    mem_crdt_io.fill_idle();

	    if (req == null) begin
               req = mem_crdt_io.idle_req();
	       mem_crdt_io.fill_req(req);
	    end
	       
	    else begin
	       mem_crdt_io.fill_req(req);
               seq_item_port.item_done();
            end

            mem_crdt_io.drive_item();

         end 
      join_none
   end
   
   
endtask : run_phase


