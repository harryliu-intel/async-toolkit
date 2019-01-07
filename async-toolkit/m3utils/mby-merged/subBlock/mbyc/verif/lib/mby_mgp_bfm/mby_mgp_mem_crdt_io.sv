//----------------------------------------------------------------------------------------
// Copyright(C) 2016 Intel Corporation, Confidential Information
//----------------------------------------------------------------------------------------
// Author:  Dhivya Sankar
// Project: Madison Bay
// Description: Mesh IO policy.
// The IO policy is a class that drives/monitors the signals on the respective interfaces.
//----------------------------------------------------------------------------------------
typedef class mby_mgp_req_seq_item;
   
//----------------------------------------------------------------------------------------
// Class: mby_mgp_mem_crdt_io
// Description: Includes credit IO policy
//----------------------------------------------------------------------------------------
class mby_mgp_mem_crdt_io  extends uvm_component;
   `uvm_component_utils(mby_mgp_mem_crdt_io)
  

   virtual mby_mgp_mim_req_if rdreq_vif;
   virtual mby_mgp_mim_req_if wrreq_vif;
   virtual mby_mgp_mim_rsp_if rsp_vif;

   int 	                port_idx;
   int 			port_num;
   
   mby_mgp_req_seq_item::physical_t pline_phys;
   req_type_e req_type;
   
   extern function new(string name = "", uvm_component parent = null);
   extern virtual function void build_phase(uvm_phase phase);
   extern virtual function void fill_idle();
   extern virtual function void fill_req(mby_mgp_req_seq_item req);
   extern virtual function mby_mgp_req_seq_item sample_item();
   extern virtual task drive_item();
   extern virtual function mby_mgp_req_seq_item idle_req();
   extern virtual task step();
   
endclass 

//----------------------------------------------------------------------------------------
// Constructor
//----------------------------------------------------------------------------------------
function mby_mgp_mem_crdt_io::new(string name = "", uvm_component parent = null);
   super.new(name, parent);
   
endfunction : new

//----------------------------------------------------------------------------------------
// Method: build
//----------------------------------------------------------------------------------------
function void mby_mgp_mem_crdt_io::build_phase(uvm_phase phase);
   super.build_phase(phase);
endfunction : build_phase


//----------------------------------------------------------------------------------------
// Method: step
// Track posedge of clock
//----------------------------------------------------------------------------------------
task mby_mgp_mem_crdt_io::step();
   @rdreq_vif.req_mst_cb;
endtask

//----------------------------------------------------------------------------------------
// Method: idle_req
// Generate an idle request
//----------------------------------------------------------------------------------------
function mby_mgp_req_seq_item mby_mgp_mem_crdt_io::idle_req();
   idle_req = mby_mgp_req_seq_item::type_id::create("idle_req");
   idle_req.req_type = req_type;

endfunction      

//----------------------------------------------------------------------------------------
// Method: fill_idle
// Fill the bus with idle requests when there isn't a valid request.
//----------------------------------------------------------------------------------------
function void mby_mgp_mem_crdt_io::fill_idle();
   idle_req().pack(pline_phys);
   
endfunction

//----------------------------------------------------------------------------------------
// Method: fill_req
// Fill the bus with valid requests.
//----------------------------------------------------------------------------------------
function void mby_mgp_mem_crdt_io::fill_req(mby_mgp_req_seq_item req);
   mby_mgp_req_seq_item::physical_t req_phys;
   req.req_type = req_type;
   req.pack(req_phys);
   pline_phys = req_phys;
   `uvm_info(this.get_name(), $sformatf("Filled pline req data : 0x%0x, req_type : %s", req.data, req_type.name), UVM_NONE)
endfunction


//----------------------------------------------------------------------------------------
// Method: drive_item
// Drive the valid requests on the interface
//----------------------------------------------------------------------------------------
task mby_mgp_mem_crdt_io::drive_item();
   mby_mgp_req_seq_item::physical_t req_phys_out;
   mby_mgp_req_seq_item req;


   //
   // At the posedge of clk, drive the transactions on rd/wr interfaces based
   // on port_idx and port_num.
   //

   req_phys_out = pline_phys;
   if (req_type == RDREQ) begin
      rdreq_vif.req_mst_cb.req_id[port_idx][port_num]  <= req_phys_out.req_bus.req_id;
      rdreq_vif.req_mst_cb.valid[port_idx][port_num]   <= req_phys_out.req_bus.valid;
      rdreq_vif.req_mst_cb.seg_ptr[port_idx][port_num] <= req_phys_out.req_bus.seg_ptr;
      rdreq_vif.req_mst_cb.sema[port_idx][port_num]    <= req_phys_out.req_bus.sema;
      rdreq_vif.req_mst_cb.wd_sel[port_idx][port_num]  <= req_phys_out.req_bus.word_sel;
      rdreq_vif.req_mst_cb.data[port_idx][port_num]    <= 0;
   end
   else if (req_type == WRREQ) begin
      wrreq_vif.req_mst_cb.req_id[port_idx][port_num]  <= req_phys_out.req_bus.req_id;
      wrreq_vif.req_mst_cb.valid[port_idx][port_num]   <= req_phys_out.req_bus.valid;
      wrreq_vif.req_mst_cb.seg_ptr[port_idx][port_num] <= req_phys_out.req_bus.seg_ptr;
      wrreq_vif.req_mst_cb.sema[port_idx][port_num]    <= req_phys_out.req_bus.sema;
      wrreq_vif.req_mst_cb.wd_sel[port_idx][port_num]  <= req_phys_out.req_bus.word_sel;
      wrreq_vif.req_mst_cb.data[port_idx][port_num]    <= req_phys_out.req_bus.data;
   end 
   if (rdreq_vif.reset == 1'b1) begin
      for (int idx = 0; idx < NUM_MSH_ROWS; idx++) begin
	 for (int jdx = 0; jdx < NUM_MSH_ROW_PORTS; jdx++) begin
	    rdreq_vif.req_mst_cb.req_id[idx][jdx]  <= 0;
            rdreq_vif.req_mst_cb.valid[idx][jdx]   <= 0;
            rdreq_vif.req_mst_cb.seg_ptr[idx][jdx] <= 0;
            rdreq_vif.req_mst_cb.sema[idx][jdx]    <= 0;
            rdreq_vif.req_mst_cb.wd_sel[idx][jdx]  <= 0;
            rdreq_vif.req_mst_cb.data[idx][jdx]    <= 0;
	    wrreq_vif.req_mst_cb.req_id[idx][jdx]  <= 0;
            wrreq_vif.req_mst_cb.valid[idx][jdx]   <= 0;
            wrreq_vif.req_mst_cb.seg_ptr[idx][jdx] <= 0;
            wrreq_vif.req_mst_cb.sema[idx][jdx]    <= 0;
            wrreq_vif.req_mst_cb.wd_sel[idx][jdx]  <= 0;
            wrreq_vif.req_mst_cb.data[idx][jdx]    <= 0;
	 end 
      end
   end
endtask : drive_item

//----------------------------------------------------------------------------------------
// Method: sample_item
//----------------------------------------------------------------------------------------
function mby_mgp_req_seq_item mby_mgp_mem_crdt_io::sample_item();
   mby_mgp_req_seq_item::physical_t phys;
   mby_mgp_req_seq_item req;

   req  = mby_mgp_req_seq_item::type_id::create("req");
   req.req_type = req_type;
   
   //
   // Monitor the response interface and collect all the valid responses.
   //
   if (req_type == RDREQ) begin
      phys.req_bus.valid    <= rdreq_vif.req_slv_cb.valid[port_idx][port_num];
      phys.req_bus.seg_ptr  <= rdreq_vif.req_slv_cb.seg_ptr[port_idx][port_num];
      phys.req_bus.sema     <= rdreq_vif.req_slv_cb.sema[port_idx][port_num];
      phys.req_bus.word_sel <= rdreq_vif.req_slv_cb.wd_sel[port_idx][port_num];
   end
   else if (req_type == WRREQ) begin
      phys.req_bus.valid    <= wrreq_vif.req_slv_cb.valid[port_idx][port_num];
      phys.req_bus.seg_ptr  <= wrreq_vif.req_slv_cb.seg_ptr[port_idx][port_num];
      phys.req_bus.sema     <= wrreq_vif.req_slv_cb.sema[port_idx][port_num];
      phys.req_bus.word_sel <= wrreq_vif.req_slv_cb.wd_sel[port_idx][port_num];
      phys.req_bus.data     <= wrreq_vif.req_slv_cb.data[port_idx][port_num];
   end
   else if (req_type == RDRSP) begin
      phys.rsp_bus.valid    <= rsp_vif.rrsp_slv_cb.valid[port_idx][port_num];
      phys.rsp_bus.dest_blk <= rsp_vif.rrsp_slv_cb.rrsp_dest_blk[port_idx][port_num];
      phys.rsp_bus.req_id   <= rsp_vif.rrsp_slv_cb.req_id[port_idx][port_num];
      phys.rsp_bus.data     <= rsp_vif.rrsp_slv_cb.data[port_idx][port_num];
   end
      
   req.unpack(phys);

   if (req.valid) begin
      return req;
   end
endfunction : sample_item


