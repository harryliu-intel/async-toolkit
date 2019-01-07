//----------------------------------------------------------------------------------------
// Copyright(C) 2014 Intel Corporation, Confidential Information
//----------------------------------------------------------------------------------------
// Author:  Dhivya Sankar
// Project: Madison Bay
// Description: The mgp agent comprises of all the agents for read req, write req and 
// read rsp. The mgp agent can be configured in master mode to drive requests and slave
// mode to monitor the requests from the DUT.   
//----------------------------------------------------------------------------------------
typedef class mby_mgp_bfm_cfg;
   
//----------------------------------------------------------------------------------------
// Class: mby_mgp_bfm
// mby_mgp_bfm class
//----------------------------------------------------------------------------------------
class mby_mgp_bfm extends uvm_component;

   `uvm_component_utils(mby_mgp_bfm)

   mby_mgp_req_agent rdreq_agent[NUM_MSH_ROW_PORTS];
   mby_mgp_req_agent wrreq_agent[NUM_MSH_ROW_PORTS];
   mby_mgp_req_agent rdrsp_agent[NUM_MSH_ROW_PORTS];

   mby_mgp_flow_ctrl   flow_ctrl;
   mby_mgp_mem_crdt_io rdreq_io[NUM_MSH_ROW_PORTS];
   mby_mgp_mem_crdt_io rdrsp_io[NUM_MSH_ROW_PORTS];
   mby_mgp_mem_crdt_io wrreq_io[NUM_MSH_ROW_PORTS];
   
   mby_mgp_bfm_cfg  bfm_cfg;

   virtual mby_mgp_mim_req_if rdreq_vif;
   virtual mby_mgp_mim_req_if wrreq_vif;
   virtual mby_mgp_mim_rsp_if rsp_vif;

   int 	   port_idx;
   
   extern function new(string name = "", uvm_component parent);
   extern virtual function void build_phase(uvm_phase phase);
   extern virtual function void connect_phase(uvm_phase phase);
   extern virtual function void end_of_elaboration_phase(uvm_phase phase);
   extern virtual function void reset();
   extern virtual function void start();
   extern virtual task run_phase(uvm_phase phase);
   extern virtual function void assign_cfg(mby_mgp_bfm_cfg bfm_cfg);
   extern virtual function void assign_vi(virtual mby_mgp_mim_req_if rreq_if, 
                                          virtual mby_mgp_mim_req_if wreq_if, 
                                          virtual mby_mgp_mim_rsp_if rsp_if );
   

endclass 


//----------------------------------------------------------------------------------------
// Constructor
//----------------------------------------------------------------------------------------
function mby_mgp_bfm::new(string name = "", uvm_component parent);
   super.new(name, parent);
 
endfunction : new

//----------------------------------------------------------------------------------------
// Method: reset
//----------------------------------------------------------------------------------------
function void mby_mgp_bfm::reset();

endfunction : reset

//----------------------------------------------------------------------------------------
// Method: start
//----------------------------------------------------------------------------------------
function void mby_mgp_bfm::start();

endfunction : start

//----------------------------------------------------------------------------------------
// Method: build
//----------------------------------------------------------------------------------------
function void mby_mgp_bfm::build_phase(uvm_phase phase);
   super.build_phase(phase);

   if (bfm_cfg == null) begin
      `uvm_fatal(get_name(), "build: mgp bfm configuration handle is null.")
   end

   //
   // Build flow control and IO policy
   //
   
   flow_ctrl             = mby_mgp_flow_ctrl::type_id::create("flow_ctrl", this);
   for (int idx = 0; idx < NUM_MSH_ROW_PORTS; idx++) begin
      rdreq_io[idx]      = mby_mgp_mem_crdt_io::type_id::create($sformatf("rdreq_io%0d", idx), this);
      rdrsp_io[idx]      = mby_mgp_mem_crdt_io::type_id::create($sformatf("rdrsp_io%0d", idx), this);
      wrreq_io[idx]      = mby_mgp_mem_crdt_io::type_id::create($sformatf("wrreq_io%0d", idx), this);
      rdreq_io[idx].port_idx  = port_idx;
      rdreq_io[idx].req_type  = RDREQ;
      rdreq_io[idx].port_num  = idx;
   
      rdreq_io[idx].rdreq_vif = rdreq_vif;
      rdreq_io[idx].wrreq_vif = wrreq_vif;
      rdreq_io[idx].rsp_vif   = rsp_vif;

      wrreq_io[idx].req_type  = WRREQ;
   
      wrreq_io[idx].port_idx  = port_idx;
      wrreq_io[idx].port_num  = idx;
      wrreq_io[idx].rdreq_vif = rdreq_vif;
      wrreq_io[idx].wrreq_vif = wrreq_vif;
      wrreq_io[idx].rsp_vif   = rsp_vif;


      rdrsp_io[idx].req_type  = RDRSP;
      rdrsp_io[idx].port_num  = idx;
      rdrsp_io[idx].port_idx  = port_idx;
      rdrsp_io[idx].rdreq_vif = rdreq_vif;
      rdrsp_io[idx].rsp_vif   = rsp_vif;
      rdrsp_io[idx].wrreq_vif = wrreq_vif;
   end
   //
   // Build read, write and response agents and assign configs.
   //
   for (int idx = 0; idx < NUM_MSH_ROW_PORTS; idx++) begin
      rdreq_agent[idx] = mby_mgp_req_agent::type_id::create($sformatf("rdreq%0d_agent", idx), this);
      rdreq_agent[idx].req_agent_cfg  = bfm_cfg.req_agent_cfg;
      rdreq_agent[idx].flow_ctrl      = flow_ctrl;
      rdreq_agent[idx].mem_crdt_io    = rdreq_io[idx];
      rdreq_agent[idx].port_num       = idx;
      
      wrreq_agent[idx] = mby_mgp_req_agent::type_id::create($sformatf("wrreq%0d_agent", idx), this);
      wrreq_agent[idx].req_agent_cfg  = bfm_cfg.req_agent_cfg;
      wrreq_agent[idx].flow_ctrl      = flow_ctrl;
      wrreq_agent[idx].mem_crdt_io    = wrreq_io[idx];
      wrreq_agent[idx].port_num       = idx;
      
      rdrsp_agent[idx] = mby_mgp_req_agent::type_id::create($sformatf("rdrsp%0d_agent", idx), this);
      rdrsp_agent[idx].req_agent_cfg  = bfm_cfg.req_agent_cfg;
      rdrsp_agent[idx].flow_ctrl      = flow_ctrl;
      rdrsp_agent[idx].mem_crdt_io    = rdrsp_io[idx];
      rdrsp_agent[idx].port_num       = idx;
      
   end
   
      
endfunction : build_phase

//----------------------------------------------------------------------------------------
// Method: connect
//----------------------------------------------------------------------------------------
function void mby_mgp_bfm::connect_phase(uvm_phase phase);
   
   super.connect_phase(phase);
   
endfunction : connect_phase

//----------------------------------------------------------------------------------------
// Method: end_of_elaboration
//----------------------------------------------------------------------------------------
function void mby_mgp_bfm::end_of_elaboration_phase(uvm_phase phase);
   
   super.end_of_elaboration_phase(phase);
   
endfunction : end_of_elaboration_phase
//----------------------------------------------------------------------------------------
// Method: run
//----------------------------------------------------------------------------------------
task mby_mgp_bfm::run_phase(uvm_phase phase);
   
endtask : run_phase

//----------------------------------------------------------------------------------------
// Method: assign_cfg
//----------------------------------------------------------------------------------------
function void mby_mgp_bfm::assign_cfg(mby_mgp_bfm_cfg bfm_cfg);
   this.bfm_cfg        = bfm_cfg;
  
   
endfunction

//----------------------------------------------------------------------------------------
// Method: assign_vi
//----------------------------------------------------------------------------------------
function void mby_mgp_bfm::assign_vi(virtual mby_mgp_mim_req_if rreq_if, virtual mby_mgp_mim_req_if wreq_if, virtual mby_mgp_mim_rsp_if rsp_if);
      
   rdreq_vif = rreq_if;
   wrreq_vif = wreq_if;
   rsp_vif   = rsp_if;
 
   
endfunction : assign_vi

