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
         logic [12:0]  op_id;
         logic [19:0]  seg_ptr;
         logic         valid;        
         logic [3:0]   sema;
         logic [1:0]   word_sel;  
         logic [471:0] dummy_req;
      } req_opbus;

      struct packed {
         logic [12:0]      op_id;
         logic [2:0]       dest_blk;  
         logic [495:0]     dummy_rsp;
      } rsp_opbus;
       
      struct packed {
         logic [511:0] data;
         //logic [63:0] ecc;
      } databus;

   }physical_t;

   rand bit [12:0]  op_id;
   rand bit [3:0]  sema;
   rand bit        valid;
   rand bit [19:0] seg_ptr;
   rand bit [511:0] data;
   rand bit [1:0]   word_sel;
   rand bit [2:0]   dest_blk;
   rand bit [471:0] dummy_req;
   rand bit [495:0] dummy_rsp;
   //rand bit [63:0] ecc;

   extern function new(string name = "");
   extern virtual function void pack(ref physical_t phys);
   extern virtual function void unpack(physical_t phys);

endclass 

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
/*
   if (bus_type == OP) begin
      phys.opbus.op_id = op_id;
      phys.opbus.ps    = ps;
      phys.opbus.py    = py;
      phys.opbus.mx    = mx;
      phys.opbus.my    = my;
      phys.opbus.addr  = addr;
      phys.opbus.sema  = sema;
      phys.opbus.age   = age;
      phys.opbus.dummy = 0;
   end
   else if (bus_type == DATA) begin
      phys.databus.data = data;
   end
*/
endfunction : pack

//----------------------------------------------------------------------------------------
// Method: unpack
// Convert the physical req into an instance of this class.
//----------------------------------------------------------------------------------------
function void mby_mgp_req_seq_item::unpack(physical_t phys);
/*
   if (bus_type == OP) begin
      op_id = phys.opbus.op_id;
      ps    = phys.opbus.ps;
      py    = phys.opbus.py;
      mx    = phys.opbus.mx;
      my    = phys.opbus.my;
      addr  = phys.opbus.addr;
      sema  = phys.opbus.sema;
      age   = phys.opbus.age;
      dummy = 0;
   end
   else if (bus_type == DATA) begin
      data = phys.databus.data;
   end
*/
endfunction : unpack
