//-----------------------------------------------------------------------------
// Title         : Egress First Packet test
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : mby_egr_first_packet_test.svh
// Author        : Oscar Garcia  <oscar.garcia@intel.com>
// Created       : 14.12.2018
// Last modified : 14.12.2018
//-----------------------------------------------------------------------------
// Description :
//
//-----------------------------------------------------------------------------
// Copyright (c) 2018 by Intel Corporation This model is the confidential and
// proprietary property of Intel Corporation and the possession or use of this
// file requires a written license from Intel Corporation.
//------------------------------------------------------------------------------
// Modification history :
// 14.12.2018 : created
//-----------------------------------------------------------------------------
import shdv_reset_pkg::*;


class mby_egr_tag_seq extends mby_egr_extended_base_seq;

   `uvm_object_utils (mby_egr_tag_seq)

   mby_tag_bfm_pkg::mby_tag_bfm_uc_xaction uc_tag;

   shdv_base_pkg::shdv_base_sequencer#(.T_req(mby_tag_bfm_uc_xaction)) tag_sequencer;

   //---------------------------------------------------------------------------
   // Function: new
   //---------------------------------------------------------------------------
   function new(string name = "mby_egr_tag_seq", uvm_component parent = null);
      super.new(name);
      // raise objection by default
      this.set_automatic_phase_objection(1);
      set_process_name(name);

   endfunction : new

   virtual protected task body_thread();
      this.set_name("mby_egr_tag_seq");
      wait_n(22);

      assert($cast(tag_sequencer, shdv_base_pkg::shdv_base_tb_sequencer::pick_sequencer("tag_bfm_uc_0")))

      else begin
         `uvm_error(get_name(), "Could not get a pointer to the tag sequencer");
      end
      for (int i = 0; i < 10; i++) begin
         uc_tag = mby_tag_bfm_pkg::mby_tag_bfm_uc_xaction::type_id::create("uc_tag");
         uc_tag.set_item_context(this, tag_sequencer);

         uc_tag.randomize();

         uc_tag.data = 0;
         uc_tag.data.valid = 1;
         uc_tag.data.dst_tc = 0;
         uc_tag.data.dst_port = 0;
         uc_tag.data.src_port = 0;
         uc_tag.data.src_tc = 0;
         uc_tag.data.sll = 0;
         uc_tag.data.eop = 1'b1;
         uc_tag.data.length = 64;
         uc_tag.data.ptr_toggle = 4'b1;
         //uc_tag.data.ptr = $urandom_range(0, 2 ** 19 - 1);
         uc_tag.data.ptr = 20 + i;

         `uvm_info(get_name(),uc_tag.convert2string(),UVM_LOW)

         `uvm_send(uc_tag)
         uc_tag.data = 0;
         `uvm_send(uc_tag)
      //wait_n(1);
      end
      `uvm_info(get_name(), "Finished mby_egr_tag_seq", UVM_LOW)
      wait_n(100);
   endtask

   virtual task body();
      super.body();
   endtask




endclass : mby_egr_tag_seq

