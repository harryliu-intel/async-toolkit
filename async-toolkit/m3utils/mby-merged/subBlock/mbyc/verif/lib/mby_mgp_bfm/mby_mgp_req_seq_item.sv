//----------------------------------------------------------------------------------------
// Copyright(C) 2014 Intel Corporation, Confidential Information
//----------------------------------------------------------------------------------------
// Author:  Dhivya Sankar
// Project: Madison Bay
// Description: The read/write request/response item. The class represents an op/data structure.
//----------------------------------------------------------------------------------------
typedef enum bus_type_e;
import mby_msh_pkg::*;

//----------------------------------------------------------------------------------------
// Class: mby_mgp_req_seq_item
// rd, wr req seq item
//----------------------------------------------------------------------------------------
class mby_mgp_req_seq_item extends uvm_sequence_item;

   `uvm_object_utils(mby_mgp_req_seq_item)

   typedef union packed {
      struct packed {
         logic [MSH_ID_WIDTH-1:0]       req_id;
         logic [MSH_SEG_PTR_WIDTH-1:0]  seg_ptr;
         logic                          valid;        
         logic                          sema;
         logic [MSH_WD_SEL_WIDTH-1:0]   word_sel;
         logic [MSH_PCLASS_WIDTH-1:0]   pclass;
         logic 	                        mcast;
         logic 	                        csr;
         logic [515:0]                  unused;
      } rreq_bus;
      struct packed {
         logic [MSH_SEG_PTR_WIDTH-1:0]  seg_ptr;
         logic                          valid;        
         logic                          sema;
         logic [MSH_WD_SEL_WIDTH-1:0]   word_sel;
         logic [MSH_PCLASS_WIDTH-1:0]   pclass;
         logic 	                        mcast;
         logic 	                        csr;
         logic [MSH_DATA_WIDTH-1:0]     data;
         logic [MSH_ECC_WIDTH-1:0]      ecc; 
      } wreq_bus;
      struct packed {
         logic [MSH_ID_WIDTH-1:0]       req_id;
         logic                          valid;  
         logic [MSH_DATA_WIDTH-1:0]     data;
         logic [MSH_ECC_WIDTH-1:0]      ecc; 
         logic [9:0] 			unused;     
      } rsp_bus;
   }physical_t;

   rand bit [MSH_ID_WIDTH-1:0]       req_id;
   rand bit                          sema;
   rand bit                          valid;
   rand bit [MSH_SEG_PTR_WIDTH-1:0]  seg_ptr;
   rand bit [MSH_WD_SEL_WIDTH-1:0]   word_sel;
   rand bit [MSH_PCLASS_WIDTH-1:0]   pclass;
   rand bit                          mcast;
   rand bit                          csr;
   rand bit [MSH_DATA_WIDTH-1:0]     data;
   rand bit [MSH_ECC_WIDTH-1:0]      ecc;

   bus_type_e bus_type;
   req_type_e req_type;

   extern constraint c_sema;
   extern constraint c_valid;
   extern constraint c_req_id;
   extern constraint c_mcast;
   extern constraint c_pclass;
   extern constraint c_csr;
   extern function new(string name = "");
   extern virtual function void pack(ref physical_t phys);
   extern virtual function void unpack(physical_t phys);

endclass 

constraint mby_mgp_req_seq_item::c_sema {
   sema == 0;
}

constraint mby_mgp_req_seq_item::c_valid {
   valid == 1;
}

constraint mby_mgp_req_seq_item::c_req_id {
   soft req_id == 0;
}
constraint mby_mgp_req_seq_item::c_mcast {
   soft mcast == 0;
}
constraint mby_mgp_req_seq_item::c_csr {
   soft csr == 0;
}
constraint mby_mgp_req_seq_item::c_pclass {
   soft pclass == 0;
}
//----------------------------------------------------------------------------------------
// Constructor
//----------------------------------------------------------------------------------------
function mby_mgp_req_seq_item::new(string name = "");
   super.new(name);
endfunction : new


//----------------------------------------------------------------------------------------
// Method: pack
// Convert the req class fields into a physical object.
//----------------------------------------------------------------------------------------
function void mby_mgp_req_seq_item::pack(ref physical_t phys);

   if (req_type == RDREQ) begin
      phys.rreq_bus.req_id   = req_id;
      phys.rreq_bus.valid    = valid;
      phys.rreq_bus.sema     = sema;
      phys.rreq_bus.word_sel = word_sel;
      phys.rreq_bus.seg_ptr  = seg_ptr;
      phys.rreq_bus.pclass   = pclass;
      phys.rreq_bus.mcast    = mcast;
      phys.rreq_bus.csr      = csr;
      phys.rreq_bus.unused   = 515'b0;
      
   end
   else if (req_type == WRREQ) begin
      phys.wreq_bus.valid    = valid;
      phys.wreq_bus.sema     = sema;
      phys.wreq_bus.word_sel = word_sel;
      phys.wreq_bus.seg_ptr  = seg_ptr;
      phys.wreq_bus.pclass   = pclass;
      phys.wreq_bus.mcast    = mcast;
      phys.wreq_bus.csr      = csr;
      phys.wreq_bus.data     = data;
      phys.wreq_bus.ecc      = ecc;
      
   end
   else if (req_type == RDRSP) begin
      phys.rsp_bus.req_id   = req_id;
      phys.rsp_bus.valid    = valid;
      phys.rsp_bus.unused   = 10'b0;
      phys.rsp_bus.data    = data;
      phys.rsp_bus.ecc     = ecc;
   end

endfunction : pack

//----------------------------------------------------------------------------------------
// Method: unpack
// Convert the physical req into an instance of this class.
//----------------------------------------------------------------------------------------
function void mby_mgp_req_seq_item::unpack(physical_t phys);

   if (req_type == RDREQ) begin
      req_id   = phys.rreq_bus.req_id;
      valid    = phys.rreq_bus.valid;
      sema     = phys.rreq_bus.sema;
      word_sel = phys.rreq_bus.word_sel;
      seg_ptr  = phys.rreq_bus.seg_ptr;
      pclass   = phys.rreq_bus.pclass;
      mcast    = phys.rreq_bus.mcast;
      csr      = phys.rreq_bus.csr;
      
   end
   else if (req_type == WRREQ) begin
      valid    = phys.wreq_bus.valid;
      sema     = phys.wreq_bus.sema;
      word_sel = phys.wreq_bus.word_sel;
      seg_ptr  = phys.wreq_bus.seg_ptr;
      pclass   = phys.wreq_bus.pclass;
      mcast    = phys.wreq_bus.mcast;
      csr      = phys.wreq_bus.csr;
      data     = phys.wreq_bus.data;
      ecc      = phys.wreq_bus.ecc;
   end
   else if (req_type == RDRSP) begin
      req_id   = phys.rsp_bus.req_id;
      valid    = phys.rsp_bus.valid;
      data     = phys.rsp_bus.data;
      ecc      = phys.rsp_bus.ecc;
   end

endfunction : unpack
