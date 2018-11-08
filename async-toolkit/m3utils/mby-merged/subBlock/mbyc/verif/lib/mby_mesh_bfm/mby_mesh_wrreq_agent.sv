//----------------------------------------------------------------------------------------
// Copyright(C) 2016 Intel Corporation, Confidential Information
//----------------------------------------------------------------------------------------
// Author:  Dhivya Sankar
// Project: Madison Bay
// Description: The wrreq_agent class.
// The agent drives write requests and monitors requests/credits coming
// out from the dut.
//----------------------------------------------------------------------------------------

//----------------------------------------------------------------------------------------
// Class: mby_mesh_wrreq_agent
//----------------------------------------------------------------------------------------
class mby_mesh_wrreq_agent extends uvm_agent;
   
   mby_mesh_wrreq_drv  wrreq_drv;
   mby_mesh_wrreq_mon  wrreq_mon;
   uvm_sequencer#(mby_mesh_wq_seq_item) wrreq_seqr;

   mby_mesh_req_agent_cfg   req_agent_cfg;
   mby_mesh_mem_crdt_io     mem_crdt_io;
   mby_mesh_flow_ctrl       flow_ctrl;
   
   `uvm_component_utils(mby_mesh_wrreq_agent)
   extern function new(string name = "", uvm_component parent = null);
   extern virtual function void build_phase(uvm_phase phase);
   extern virtual function void connect_phase(uvm_phase phase);
   extern virtual function void reset();

endclass 

//----------------------------------------------------------------------------------------
// Constructor
//----------------------------------------------------------------------------------------
function mby_mesh_wrreq_agent::new(string name = "", uvm_component parent = null);
   super.new(name, parent);
endfunction

//----------------------------------------------------------------------------------------
// Method: build
//----------------------------------------------------------------------------------------
function void mby_mesh_wrreq_agent::build_phase(uvm_phase phase);
   super.build_phase(phase);

   if (req_agent_cfg == null) begin
      `uvm_fatal(get_name(), "build: req_agent_cfg handle is null.")
   end
                           

   if (req_agent_cfg.is_active == UVM_ACTIVE) begin
      wrreq_seqr = new("wrreq_seqr", this);
   end
   wrreq_drv              = mby_mesh_wrreq_drv::type_id::create("wrreq_drv", this);
   wrreq_drv.req_agent_cfg = req_agent_cfg;
   wrreq_drv.flow_ctrl    = flow_ctrl;
   wrreq_drv.mem_crdt_io  = mem_crdt_io;

   wrreq_mon              = mby_mesh_wrreq_mon::type_id::create("wrreq_mon", this);
   wrreq_mon.req_agent_cfg = req_agent_cfg;
   wrreq_mon.mem_crdt_io  = mem_crdt_io;
   
endfunction

//----------------------------------------------------------------------------------------
// Method: connect
//----------------------------------------------------------------------------------------
function void mby_mesh_wrreq_agent::connect_phase(uvm_phase phase);
   super.connect_phase(phase);
   if (req_agent_cfg.is_active == UVM_ACTIVE)
      wrreq_drv.seq_item_port.connect(wrreq_seqr.seq_item_export);
endfunction

//----------------------------------------------------------------------------------------
// Method: reset
//----------------------------------------------------------------------------------------
function void mby_mesh_wrreq_agent::reset();
   wrreq_mon.reset();
endfunction


