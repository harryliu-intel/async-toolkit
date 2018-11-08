//----------------------------------------------------------------------------------------
// Copyright(C) 2016 Intel Corporation, Confidential Information
//----------------------------------------------------------------------------------------
// Author:  Dhivya Sankar
// Project: Madison Bay
// Description: The rdreq_agent class.
// The agent is always active, drives read requests and monitors requests/credits coming
// out from the dut.
//----------------------------------------------------------------------------------------

//----------------------------------------------------------------------------------------
// Class: mby_mesh_rdreq_agent
//----------------------------------------------------------------------------------------
class mby_mesh_rdreq_agent extends uvm_agent;
   
   mby_mesh_rdreq_drv  rdreq_drv;
   mby_mesh_rdreq_mon  rdreq_mon;
   uvm_sequencer#(rdreq_item) rdreq_seqr;

   mby_mesh_req_agent_cfg req_agent_cfg;
   mby_mesh_mem_crdt_io     mem_crdt_io;
   mby_mesh_flow_ctrl       flow_ctrl;
   
   `uvm_component_utils(mby_mesh_rdreq_agent)
   extern function new(string name = "", uvm_component parent = null);
   extern virtual function void build_phase(uvm_phase phase);
   extern virtual function void connect_phase(uvm_phase phase);
   extern virtual function void reset();

endclass 

//----------------------------------------------------------------------------------------
// Constructor
//----------------------------------------------------------------------------------------
function mby_mesh_rdreq_agent::new(string name = "", uvm_component parent = null);
   super.new(name, parent);
endfunction

//----------------------------------------------------------------------------------------
// Method: build
//----------------------------------------------------------------------------------------
function void mby_mesh_rdreq_agent::build_phase(uvm_phase phase);
   super.build_phase(phase);

   if (req_agent_cfg == null) begin
      `uvm_fatal(get_name(), "build: req_agent_cfg handle is null.")
   end
                           

   if (req_agent_cfg.is_active == UVM_ACTIVE) begin
      rdreq_seqr = new("rdreq_seqr", this);
   end
   rdreq_drv              = mby_mesh_rdreq_drv::type_id::create("rdreq_drv", this);
   rdreq_drv.req_agent_cfg = req_agent_cfg;
   rdreq_drv.flow_ctrl    = flow_ctrl;
   rdreq_drv.mem_crdt_io  = mem_crdt_io;

   rdreq_mon              = mby_mesh_rdreq_mon::type_id::create("rdreq_mon", this);
   rdreq_mon.req_agent_cfg = req_agent_cfg;
   rdreq_mon.mem_crdt_io  = mem_crdt_io;
   
endfunction

//----------------------------------------------------------------------------------------
// Method: connect
//----------------------------------------------------------------------------------------
function void mby_mesh_rdreq_agent::connect_phase(uvm_phase phase);
   super.connect_phase(phase);
   if (req_agent_cfg.is_active == UVM_ACTIVE)
      rdreq_drv.seq_item_port.connect(rdreq_seqr.seq_item_export);
endfunction

//----------------------------------------------------------------------------------------
// Method: reset
//----------------------------------------------------------------------------------------
function void mby_mesh_rdreq_agent::reset();
   rdreq_mon.reset();
endfunction


