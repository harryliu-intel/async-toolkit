//----------------------------------------------------------------------------------------
// Copyright(C) 2014 Intel Corporation, Confidential Information
//----------------------------------------------------------------------------------------
// Author:  Dhivya Sankar
// Project: Madison Bay
// Description: The read/write request/response item. The class represents an op/data structure.
//----------------------------------------------------------------------------------------
typedef enum bus_type_e;

//----------------------------------------------------------------------------------------
// Class: mby_mgp_req_seq_item
// rd, wr req seq item
//----------------------------------------------------------------------------------------
class mby_mgp_req_seq_item extends uvm_sequence_item;

   `uvm_object_utils(mby_mgp_req_seq_item)

   typedef union packed {
      struct packed {
         logic [12:0]  req_id;
         logic [19:0]  seg_ptr;
         logic         valid;        
         logic [3:0]   sema;
         logic [1:0]   word_sel;
         logic [511:0] data;
      } req_bus;

      struct packed {
         logic [12:0]      req_id;
         logic [2:0]       dest_blk;
         logic 	           valid; 
         logic [511:0]     data;
         logic [22:0]      dummy_rsp;
      } rsp_bus;
       

   }physical_t;

   rand bit [12:0]  req_id;
   rand bit [3:0]   sema;
   rand bit         valid;
   rand bit [19:0]  seg_ptr;
   rand bit [511:0] data;
   rand bit [1:0]   word_sel;
   rand bit [2:0]   dest_blk;
   rand bit [471:0] dummy_req;
   rand bit [495:0] dummy_rsp;
   //rand bit [63:0] ecc;

   rand bus_type_e bus_type;
   rand req_type_e req_type;

   extern constraint c_sema;
   extern constraint c_valid;
   extern constraint c_req_id;
   

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
   req_id == 0;
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
      phys.req_bus.req_id   = req_id;
      phys.req_bus.valid    = valid;
      phys.req_bus.sema     = sema;
      phys.req_bus.word_sel = word_sel;
      phys.req_bus.seg_ptr  = seg_ptr;
      phys.req_bus.data     = 0;
   end
   else if (req_type == WRREQ) begin
      phys.req_bus.req_id   = req_id;
      phys.req_bus.valid    = valid;
      phys.req_bus.sema     = sema;
      phys.req_bus.word_sel = word_sel;
      phys.req_bus.seg_ptr  = seg_ptr;
      phys.req_bus.data     = data;
   end
   else if (req_type == RDRSP) begin
      phys.rsp_bus.req_id   = req_id;
      phys.rsp_bus.valid    = valid;
      phys.rsp_bus.dest_blk = dest_blk;
      phys.rsp_bus.data     = data;
   end
endfunction : pack

//----------------------------------------------------------------------------------------
// Method: unpack
// Convert the physical req into an instance of this class.
//----------------------------------------------------------------------------------------
function void mby_mgp_req_seq_item::unpack(physical_t phys);

   if (req_type == RDREQ) begin
      req_id   = phys.req_bus.req_id;
      valid    = phys.req_bus.valid;
      sema     = phys.req_bus.sema;
      word_sel = phys.req_bus.word_sel;
      seg_ptr  = phys.req_bus.seg_ptr;
      data     = 0;
   end
   else if (req_type == WRREQ) begin
      req_id   = phys.req_bus.req_id;
      valid    = phys.req_bus.valid;
      sema     = phys.req_bus.sema;
      word_sel = phys.req_bus.word_sel;
      seg_ptr  = phys.req_bus.seg_ptr;
      data     = phys.req_bus.data;
   end
   else if (req_type == RDRSP) begin
      req_id   = phys.rsp_bus.req_id;
      valid    = phys.rsp_bus.valid;
      dest_blk = phys.rsp_bus.dest_blk;
      data     = phys.rsp_bus.data;
   end

endfunction : unpack
