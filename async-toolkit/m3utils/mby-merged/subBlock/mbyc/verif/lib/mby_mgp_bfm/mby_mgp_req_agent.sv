//----------------------------------------------------------------------------------------
// Copyright(C) 2016 Intel Corporation, Confidential Information
//----------------------------------------------------------------------------------------
// Author:  Dhivya Sankar
// Project: Madison Bay
// Description: The mesh req_agent class.
// The agent drives read requests and monitors requests/credits coming
// out from the dut.
//----------------------------------------------------------------------------------------

//----------------------------------------------------------------------------------------
// Class: mby_mgp_req_agent
// Agent class 
//----------------------------------------------------------------------------------------
class mby_mgp_req_agent extends uvm_agent;
   
   mby_mgp_req_drv  req_drv;
   mby_mgp_req_mon  req_mon;
   mby_mgp_req_seqr req_seqr;

   mby_mgp_req_agent_cfg   req_agent_cfg;
   mby_mgp_mem_crdt_io     mem_crdt_io;
   mby_mgp_flow_ctrl       flow_ctrl;
   int port_num;
   
   `uvm_component_utils(mby_mgp_req_agent)
   extern function new(string name = "", uvm_component parent = null);
   extern virtual function void build_phase(uvm_phase phase);
   extern virtual function void connect_phase(uvm_phase phase);
   extern virtual function void reset();

endclass 

//----------------------------------------------------------------------------------------
// Constructor
// Creating new object for the agent
//----------------------------------------------------------------------------------------
function mby_mgp_req_agent::new(string name = "", uvm_component parent = null);
   super.new(name, parent);
endfunction : new

//----------------------------------------------------------------------------------------
// Method: build
// Building agent components based on enable
//----------------------------------------------------------------------------------------
function void mby_mgp_req_agent::build_phase(uvm_phase phase);
   super.build_phase(phase);

   if (req_agent_cfg == null) begin
      `uvm_fatal(get_name(), "build: req_agent_cfg handle is null.")
   end
                           

   if (req_agent_cfg.driver_enable) begin
      req_seqr = mby_mgp_req_seqr::type_id::create("req_seqr", this);
   
      req_drv                 = mby_mgp_req_drv::type_id::create("req_drv", this);
      req_drv.req_agent_cfg   = req_agent_cfg;
      req_drv.flow_ctrl       = flow_ctrl;
      req_drv.mem_crdt_io     = mem_crdt_io;
      req_drv.port_num        = port_num;
   end
   if (req_agent_cfg.monitor_enable) begin
      req_mon                 = mby_mgp_req_mon::type_id::create("req_mon", this);
      req_mon.req_agent_cfg   = req_agent_cfg;
      req_mon.mem_crdt_io     = mem_crdt_io;
      req_mon.port_num        = port_num;
   end
endfunction : build_phase

//----------------------------------------------------------------------------------------
// Method: connect
// Function connecting driver to sequencer
//----------------------------------------------------------------------------------------
function void mby_mgp_req_agent::connect_phase(uvm_phase phase);
   super.connect_phase(phase);
   if (req_agent_cfg.driver_enable) begin
      req_drv.seq_item_port.connect(req_seqr.seq_item_export);
   end
endfunction : connect_phase

//----------------------------------------------------------------------------------------
// Method: reset
//----------------------------------------------------------------------------------------
function void mby_mgp_req_agent::reset();
   if (req_agent_cfg.driver_enable) begin
      req_drv.reset();
   end
   if (req_agent_cfg.monitor_enable) begin
      req_mon.reset();
   end
endfunction : reset


