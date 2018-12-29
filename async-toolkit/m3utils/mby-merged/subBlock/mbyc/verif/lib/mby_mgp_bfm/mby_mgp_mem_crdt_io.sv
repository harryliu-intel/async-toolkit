//----------------------------------------------------------------------------------------
// Copyright(C) 2016 Intel Corporation, Confidential Information
//----------------------------------------------------------------------------------------
// Author:  Dhivya Sankar
// Project: Madison Bay
// Description: Mesh IO policy.
// The IO policy is a class that drives/monitors the signals on the respective interfaces.
//----------------------------------------------------------------------------------------
typedef class mby_mgp_req_seq_item;
typedef class mby_mgp_bfm_cfg;
   
//----------------------------------------------------------------------------------------
// Class: mby_mgp_mem_crdt_io
//----------------------------------------------------------------------------------------
class mby_mgp_mem_crdt_io  extends uvm_component;
   `uvm_component_utils(mby_mgp_mem_crdt_io)
  

   virtual mby_mgp_mim_req_if rdreq_vif;
   virtual mby_mgp_mim_req_if wrreq_vif;
   virtual mby_mgp_mim_rsp_if rsp_vif;

   mby_mgp_req_seq_item req_q[$];
   mby_mgp_bfm_cfg      bfm_cfg;

   int 	                port_idx;
   
   extern function new(string name = "", uvm_component parent = null);
   extern virtual function void build_phase(uvm_phase phase);
   extern virtual function mby_mgp_req_seq_item sample_item(int port_num);
   extern virtual task drive_item(mby_mgp_req_seq_item item, int port_num);
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
//----------------------------------------------------------------------------------------
task mby_mgp_mem_crdt_io::step();
   @rdreq_vif.req_mst_cb;
endtask
   
//----------------------------------------------------------------------------------------
// Method: drive_item
//----------------------------------------------------------------------------------------
task mby_mgp_mem_crdt_io::drive_item(mby_mgp_req_seq_item item, int port_num);
   mby_mgp_req_seq_item::physical_t req_phys;
   mby_mgp_req_seq_item req;


   //
   // At the posedge of clk, drive the transactions on rd/wr interfaces based
   // on port_idx and port_num.
   //
   if ($cast (req, item)) begin
      req.pack(req_phys);
      step();
      for (int idx = 0; idx < NUM_MSH_ROWS; idx++) begin
	 for (int jdx = 0; jdx < NUM_MSH_ROW_PORTS; jdx++) begin
	    if (port_idx == idx && port_num == jdx) begin
               if (req.req_type == RDREQ) begin
                  rdreq_vif.req_mst_cb.valid[idx][jdx]   <= req_phys.req_bus.valid;
                  rdreq_vif.req_mst_cb.seg_ptr[idx][jdx] <= req_phys.req_bus.seg_ptr;
                  rdreq_vif.req_mst_cb.sema[idx][jdx]    <= req_phys.req_bus.sema;
                  rdreq_vif.req_mst_cb.wd_sel[idx][jdx]  <= req_phys.req_bus.word_sel;
                  rdreq_vif.req_mst_cb.data[idx][jdx]    <= 0;
               end
               else if (req.req_type == WRREQ) begin
                  wrreq_vif.req_mst_cb.valid[idx][jdx]   <= req_phys.req_bus.valid;
                  wrreq_vif.req_mst_cb.seg_ptr[idx][jdx] <= req_phys.req_bus.seg_ptr;
                  wrreq_vif.req_mst_cb.sema[idx][jdx]    <= req_phys.req_bus.sema;
                  wrreq_vif.req_mst_cb.wd_sel[idx][jdx]  <= req_phys.req_bus.word_sel;
                  repeat (2) @(wrreq_vif.req_mst_cb);
                  wrreq_vif.req_mst_cb.data[idx][jdx]    <= req_phys.req_bus.data;
               end
	    end
            else begin
	       rdreq_vif.req_mst_cb.valid[idx][jdx]   <= 0;
               rdreq_vif.req_mst_cb.seg_ptr[idx][jdx] <= 0;
               rdreq_vif.req_mst_cb.sema[idx][jdx]    <= 0;
               rdreq_vif.req_mst_cb.wd_sel[idx][jdx]  <= 0;
               rdreq_vif.req_mst_cb.data[idx][jdx]    <= 0; 
               wrreq_vif.req_mst_cb.valid[idx][jdx]   <= 0;
               wrreq_vif.req_mst_cb.seg_ptr[idx][jdx] <= 0;
               wrreq_vif.req_mst_cb.sema[idx][jdx]    <= 0;
               wrreq_vif.req_mst_cb.wd_sel[idx][jdx]  <= 0;
               wrreq_vif.req_mst_cb.data[idx][jdx]    <= 0;
	    end
         end 
      end
   end
   
endtask : drive_item

//----------------------------------------------------------------------------------------
// Method: sample_item
//----------------------------------------------------------------------------------------
function mby_mgp_req_seq_item mby_mgp_mem_crdt_io::sample_item(int port_num);
   mby_mgp_req_seq_item::physical_t phys;
   mby_mgp_req_seq_item req;

   req  = mby_mgp_req_seq_item::type_id::create("req");

   //
   // Monitor the response interface and collect all the valid responses.
   //
   step();
   if (bfm_cfg.req_type == RDREQ) begin
      phys.req_bus.valid    <= rdreq_vif.req_slv_cb.valid[port_idx][port_num];
      phys.req_bus.seg_ptr  <= rdreq_vif.req_slv_cb.seg_ptr[port_idx][port_num];
      phys.req_bus.sema     <= rdreq_vif.req_slv_cb.sema[port_idx][port_num];
      phys.req_bus.word_sel <= rdreq_vif.req_slv_cb.wd_sel[port_idx][port_num];
   end
   else if (bfm_cfg.req_type == WRREQ) begin
      phys.req_bus.valid    <= wrreq_vif.req_slv_cb.valid[port_idx][port_num];
      phys.req_bus.seg_ptr  <= wrreq_vif.req_slv_cb.seg_ptr[port_idx][port_num];
      phys.req_bus.sema     <= wrreq_vif.req_slv_cb.sema[port_idx][port_num];
      phys.req_bus.word_sel <= wrreq_vif.req_slv_cb.wd_sel[port_idx][port_num];
      phys.req_bus.data     <= wrreq_vif.req_slv_cb.data[port_idx][port_num];
   end
   else if (bfm_cfg.req_type == RDRSP) begin
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


