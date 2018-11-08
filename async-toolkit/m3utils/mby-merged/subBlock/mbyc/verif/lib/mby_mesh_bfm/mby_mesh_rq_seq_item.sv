//----------------------------------------------------------------------------------------
// Copyright(C) 2014 Intel Corporation, Confidential Information
//----------------------------------------------------------------------------------------
// Author:  Dhivya Sankar
// Project: Madison Bay
// Description: The read/write request/response item. The class represents a read/write
// request/response that comprises of op structure.
//----------------------------------------------------------------------------------------

//----------------------------------------------------------------------------------------
// Class: mby_mesh_req_seq_item
//----------------------------------------------------------------------------------------
class mby_mesh_req_seq_item extends uvm_sequence_item;

   `uvm_component_utils(mby_mesh_req_seq_item)

   typedef union packed {
      struct packed {
         logic [15:0] op_id;
         logic        ps;
         logic [3:0]  py;
         logic [2:0]  mx;
         logic [3:0]  my;
         logic [13:0] addr;
         logic [1:0]  sema;
         logic [7:0]  age;
      } opbus;

      struct packed {
         logic [511:0] data;
      } databus;

   }physical_t;

   rand bit [15:0] op_id;
   rand bit        ps;
   rand bit [3:0]  py;
   rand bit [2:0]  mx;
   rand bit [3:0]  my;
   rand bit [13:0] addr;
   rand bit [1:0]  sema;
   rand bit [7:0]  age;
   rand bit [511:0] data;

   extern function new(string name = "");
   extern virtual function void pack(ref physical_t phys);
   extern virtual function void unpack(physical_t phys);

endclass 

//----------------------------------------------------------------------------------------
// Constructor
//----------------------------------------------------------------------------------------
function mby_mesh_req_seq_item::new(string name = "");
   super.new(name);
endfunction 


//----------------------------------------------------------------------------------------
// Method: pack
// Convert the req class fields into a physical object.
//----------------------------------------------------------------------------------------
function void mby_mesh_req_seq_item::pack(ref physical_t phys);

   if (bus_type == OP) begin
      phys.opbus.op_id = op_id;
      phys.opbus.ps    = ps;
      phys.opbus.py    = py;
      phys.opbus.mx    = mx;
      phys.opbus.my    = my;
      phys.opbus.addr  = addr;
      phys.opbus.sema  = sema;
      phys.opbus.age   = age;
   end
   else if (bus_type == DATA) begin
      phys.databus.data = data;
   end

endfunction

//----------------------------------------------------------------------------------------
// Method: unpack
// Convert the physical req into an instance of this class.
//----------------------------------------------------------------------------------------
function void mby_mesh_req_seq_item::unpack(physical_t phys);

   if (bus_type == OP) begin
      op_id = phys.opbus.op_id;
      ps    = phys.opbus.ps;
      py    = phys.opbus.py;
      mx    = phys.opbus.mx;
      my    = phys.opbus.my;
      addr  = phys.opbus.addr;
      sema  = phys.opbus.sema;
      age   = phys.opbus.age;
   end
   else if (bus_type == DATA) begin
      data = phys.databus.data;
   end

endfunction