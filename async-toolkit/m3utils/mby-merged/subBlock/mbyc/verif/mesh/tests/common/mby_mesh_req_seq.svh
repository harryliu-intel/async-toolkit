//----------------------------------------------------------------------------------------
// Copyright(C) 2018 Intel Corporation, Confidential Information
//----------------------------------------------------------------------------------------
// Author:  Dhivya Sankar
// Project: Madison Bay mesh
// Description: Mesh request Sequence.
//
// Generate the stream of requests on Mesh interface.  One of these sequences is created 
// and run on each of the MGP bfm's in the testbench.
//----------------------------------------------------------------------------------------


//----------------------------------------------------------------------------------------
// Class: mby_mesh_req_seq
//----------------------------------------------------------------------------------------
class mby_mesh_req_seq extends uvm_sequence#(mby_mgp_bfm_pkg::mby_mgp_req_seq_item);

   `uvm_object_utils(mby_mesh_req_seq)

   int cnt = 10;
   extern function new(string name = "");
   extern virtual task pre_body();
   extern virtual task body();

endclass

//----------------------------------------------------------------------------------------
// Constructor
//----------------------------------------------------------------------------------------
function mby_mesh_req_seq::new(string name = "");

   super.new(name);
   
endfunction

//----------------------------------------------------------------------------------------
// Method: pre_body
//----------------------------------------------------------------------------------------
task mby_mesh_req_seq::pre_body();
   super.pre_body();

endtask

//----------------------------------------------------------------------------------------
// Method: body
//----------------------------------------------------------------------------------------
task mby_mesh_req_seq::body();

   mby_mgp_bfm_pkg::mby_mgp_req_seq_item item;
   mby_mgp_bfm_pkg::mby_mgp_req_seqr seqr;
   
   int loop_cnt = 1;
   `uvm_info(get_name(), "body: Starting.", UVM_LOW)

   $cast(seqr, shdv_base_pkg::shdv_base_tb_sequencer::pick_sequencer("req_seqr"));
   
   for (int idx = 0; idx < loop_cnt; idx++) begin
      int addr;
      item = mby_mgp_bfm_pkg::mby_mgp_req_seq_item::type_id::create("item");
      item.set_item_context(this, seqr);
      addr = (cnt % 2 == 0) ? cnt : cnt - 1;
      item.randomize() with {
// FIXME:          req_id  == cnt;
         req_id  == 0;
         seg_ptr == addr;
         valid   == 1;
         sema    == 0;
      };
      `uvm_send(item)
      
      cnt++;
      #10ns;
   end

endtask 
