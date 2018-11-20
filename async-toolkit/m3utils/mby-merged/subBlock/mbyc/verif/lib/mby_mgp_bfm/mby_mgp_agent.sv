//----------------------------------------------------------------------------------------
// Copyright(C) 2014 Intel Corporation, Confidential Information
//----------------------------------------------------------------------------------------
// Author:  Dhivya Sankar
// Project: Madison Bay
// Description: The mgp agent comprises of all the agents for read req, write req and 
// read rsp. The mgp agent can be configured in master mode to drive requests and slave
// mode to monitor the requests from the DUT.   
//----------------------------------------------------------------------------------------
typedef class mby_mgp_agent_cfg;
   
//----------------------------------------------------------------------------------------
// Class: mby_mgp_agent
//----------------------------------------------------------------------------------------
class mby_mgp_agent extends uvm_component;

   `uvm_component_utils(mby_mgp_agent)

   mby_mgp_rdreq_agent rdreq_agent[NUM_PLANES];
   mby_mgp_wrreq_agent wrreq_agent[NUM_PLANES];
   mby_mgp_rdrsp_agent rdrsp_agent[NUM_PLANES];

   mby_mgp_flow_ctrl   flow_ctrl;
   mby_mgp_mem_crdt_io mem_crdt_io;

   mby_mgp_agent_cfg  mgp_agent_cfg;

   virtual mby_mgp_op_if rdreq_opif[NUM_PLANES];
   virtual mby_mgp_op_if wrreq_opif[NUM_PLANES];
   virtual mby_mgp_op_if rdrsp_opif[NUM_PLANES];

   virtual mby_mgp_data_if rdrsp_dataif[NUM_PLANES];
   virtual mby_mgp_data_if wrreq_dataif[NUM_PLANES];

   extern function new(string name = "", uvm_component parent);
   extern virtual function void build_phase(uvm_phase phase);
   extern virtual function void connect_phase(uvm_phase phase);
   extern virtual function void end_of_elaboration_phase(uvm_phase phase);
   extern virtual function void reset();
   extern virtual function void start();
   extern virtual task run_phase(uvm_phase phase);
   extern virtual function void assign_cfg(mby_mgp_agent_cfg mgp_cfg);
   extern virtual function void assign_vi(virtual mby_mgp_op_if rdreq_opvif[NUM_PLANES], 
                                          virtual mby_mgp_op_if rdrsp_opvif[NUM_PLANES], 
                                          virtual mby_mgp_op_if wrreq_opvif[NUM_PLANES], 
                                          virtual mby_mgp_data_if rdrsp_datavif[NUM_PLANES], 
                                          virtual mby_mgp_data_if wrreq_datavif[NUM_PLANES]);
   

endclass 


//----------------------------------------------------------------------------------------
// Constructor
//----------------------------------------------------------------------------------------
function mby_mgp_agent::new(string name = "", uvm_component parent);
   super.new(name, parent);
 
endfunction : new

//----------------------------------------------------------------------------------------
// Method: reset
//----------------------------------------------------------------------------------------
function void mby_mgp_agent::reset();

endfunction : reset

//----------------------------------------------------------------------------------------
// Method: start
//----------------------------------------------------------------------------------------
function void mby_mgp_agent::start();

endfunction : start

//----------------------------------------------------------------------------------------
// Method: build
//----------------------------------------------------------------------------------------
function void mby_mgp_agent::build_phase(uvm_phase phase);
   super.build_phase(phase);

   if (mgp_agent_cfg == null) begin
      `uvm_fatal(get_name(), "build: mgp agent configuration handle is null.")
   end

   flow_ctrl     = mby_mgp_flow_ctrl::type_id::create("flow_ctrl", this);
   mem_crdt_io   = mby_mgp_mem_crdt_io::type_id::create("mem_crdt_io", this);
   
   for (int idx = 0; idx < NUM_PLANES; idx++) begin
      rdreq_agent[idx] = mby_mgp_rdreq_agent::type_id::create($sformatf("rdreq%0d_agent", idx), this);
      rdreq_agent[idx].req_agent_cfg  = mgp_agent_cfg.req_agent_cfg;
      rdreq_agent[idx].flow_ctrl     = flow_ctrl;
      rdreq_agent[idx].mem_crdt_io   = mem_crdt_io;
      
      wrreq_agent[idx] = mby_mgp_wrreq_agent::type_id::create($sformatf("wrreq%0d_agent", idx), this);
      wrreq_agent[idx].req_agent_cfg  = mgp_agent_cfg.req_agent_cfg;
      wrreq_agent[idx].flow_ctrl     = flow_ctrl;
      wrreq_agent[idx].mem_crdt_io   = mem_crdt_io;
      
      rdrsp_agent[idx] = mby_mgp_rdrsp_agent::type_id::create($sformatf("rdrsp%0d_agent", idx), this);
      rdrsp_agent[idx].req_agent_cfg  = mgp_agent_cfg.req_agent_cfg;
      rdrsp_agent[idx].flow_ctrl     = flow_ctrl;
      rdrsp_agent[idx].mem_crdt_io   = mem_crdt_io;
   end
   
      
endfunction : build_phase

//----------------------------------------------------------------------------------------
// Method: connect
//----------------------------------------------------------------------------------------
function void mby_mgp_agent::connect_phase(uvm_phase phase);
   
   super.connect_phase(phase);
   
endfunction : connect_phase

//----------------------------------------------------------------------------------------
// Method: end_of_elaboration
//----------------------------------------------------------------------------------------
function void mby_mgp_agent::end_of_elaboration_phase(uvm_phase phase);
   
   super.end_of_elaboration_phase(phase);
   
endfunction : end_of_elaboration_phase
//----------------------------------------------------------------------------------------
// Method: run
//----------------------------------------------------------------------------------------
task mby_mgp_agent::run_phase(uvm_phase phase);
   
endtask : run_phase

//----------------------------------------------------------------------------------------
// Method: assign_cfg
//----------------------------------------------------------------------------------------
function void mby_mgp_agent::assign_cfg(mby_mgp_agent_cfg mgp_cfg);
   mgp_agent_cfg = mgp_cfg;
endfunction

//----------------------------------------------------------------------------------------
// Method: assign_vi
//----------------------------------------------------------------------------------------
function void mby_mgp_agent::assign_vi(virtual mby_mgp_op_if rdreq_opvif[NUM_PLANES], 
                                        virtual mby_mgp_op_if rdrsp_opvif[NUM_PLANES], 
                                        virtual mby_mgp_op_if wrreq_opvif[NUM_PLANES], 
                                        virtual mby_mgp_data_if rdrsp_datavif[NUM_PLANES], 
                                        virtual mby_mgp_data_if wrreq_datavif[NUM_PLANES]);

   for (int idx = 0; idx < NUM_PLANES; idx++) begin
      rdreq_opif[idx] = rdreq_opvif[idx];
      wrreq_opif[idx] = wrreq_opvif[idx];
      rdrsp_opif[idx] = rdrsp_opvif[idx];

      wrreq_dataif[idx] = wrreq_datavif[idx];
      rdrsp_dataif[idx] = rdrsp_datavif[idx];
   end

endfunction : assign_vi
