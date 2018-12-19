//----------------------------------------------------------------------------------------
// Copyright(C) 2016 Intel Corporation, Confidential Information
//----------------------------------------------------------------------------------------
// Author:  Dhivya Sankar
// Project: Madison Bay
// Description: The rdreq_agent class.
// The agent drives and monitors read responses.
//----------------------------------------------------------------------------------------

//----------------------------------------------------------------------------------------
// Class: mby_mgp_rdrsp_agent
//----------------------------------------------------------------------------------------
class mby_mgp_rdrsp_agent extends uvm_agent;
   
   mby_mgp_rdrsp_drv  rdrsp_drv;
   mby_mgp_rdrsp_mon  rdrsp_mon;
   uvm_sequencer#(mby_mgp_req_seq_item) rdrsp_seqr;

   mby_mgp_req_agent_cfg req_agent_cfg;
   mby_mgp_mem_crdt_io     mem_crdt_io;
   mby_mgp_flow_ctrl       flow_ctrl;
   
   `uvm_component_utils(mby_mgp_rdrsp_agent)
   extern function new(string name = "", uvm_component parent = null);
   extern virtual function void build_phase(uvm_phase phase);
   extern virtual function void connect_phase(uvm_phase phase);
   extern virtual function void reset();

endclass 

//----------------------------------------------------------------------------------------
// Constructor
//----------------------------------------------------------------------------------------
function mby_mgp_rdrsp_agent::new(string name = "", uvm_component parent = null);
   super.new(name, parent);
endfunction : new

//----------------------------------------------------------------------------------------
// Method: build
//----------------------------------------------------------------------------------------
function void mby_mgp_rdrsp_agent::build_phase(uvm_phase phase);
   super.build_phase(phase);

   if (req_agent_cfg == null) begin
      `uvm_fatal(get_name(), "build: req_agent_cfg handle is null.")
   end
                           

   if (req_agent_cfg.driver_enable) begin
      rdrsp_seqr = new("rdreq_seqr", this);
   
      rdrsp_drv              = mby_mgp_rdrsp_drv::type_id::create("rdreq_drv", this);
      rdrsp_drv.req_agent_cfg = req_agent_cfg;
      rdrsp_drv.flow_ctrl    = flow_ctrl;
      rdrsp_drv.mem_crdt_io  = mem_crdt_io;
   end
   if (req_agent_cfg.monitor_enable) begin
      rdrsp_mon              = mby_mgp_rdrsp_mon::type_id::create("rdreq_mon", this);
      rdrsp_mon.req_agent_cfg = req_agent_cfg;
      rdrsp_mon.mem_crdt_io  = mem_crdt_io;
   end
endfunction : build_phase

//----------------------------------------------------------------------------------------
// Method: connect
//----------------------------------------------------------------------------------------
function void mby_mgp_rdrsp_agent::connect_phase(uvm_phase phase);
   super.connect_phase(phase);
   if (req_agent_cfg.driver_enable) begin
      rdrsp_drv.seq_item_port.connect(rdrsp_seqr.seq_item_export);
   end
endfunction : connect_phase

//----------------------------------------------------------------------------------------
// Method: reset
//----------------------------------------------------------------------------------------
function void mby_mgp_rdrsp_agent::reset();
   if (req_agent_cfg.driver_enable) begin
      rdrsp_drv.reset();
   end
   if (req_agent_cfg.monitor_enable) begin
      rdrsp_mon.reset();
   end
endfunction : reset 


