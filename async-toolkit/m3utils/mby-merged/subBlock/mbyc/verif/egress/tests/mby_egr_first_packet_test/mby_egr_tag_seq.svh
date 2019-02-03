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
   mby_smm_bfm_pkg::mby_smm_bfm_mwr_req_xaction smm_wr;

   shdv_base_pkg::shdv_base_sequencer#(.T_req(mby_tag_bfm_uc_xaction)) tag_sequencer;
   shdv_base_pkg::shdv_base_sequencer#(.T_req(mby_smm_bfm_pkg::mby_smm_bfm_mwr_req_xaction)) smm_sequencer;

   //---------------------------------------------------------------------------
   // Function: new
   //---------------------------------------------------------------------------
   function new(string name = "mby_egr_tag_seq", uvm_component parent = null);
      super.new(name);
      // raise objection by default
      this.set_automatic_phase_objection(1);

   endfunction : new

   virtual protected task body_thread();
      uvm_status_e status;
      uvm_path_e access_type = UVM_BACKDOOR;
      uvm_reg_data_t read_data;
      mby_egr_reg_pkg::mby_egr_reg_blk ral;
      ral = env.get_tb_ral;
      this.set_name("mby_egr_tag_seq");



      smm_wr = mby_smm_bfm_pkg::mby_smm_bfm_mwr_req_xaction::type_id::create("smm_wr");
      assert($cast(smm_sequencer, shdv_base_pkg::shdv_base_tb_sequencer::pick_sequencer("smm_bfm_wr_req")))

      else begin
         `uvm_error(get_name(), "Could not get a pointer to the smm sequencer");
      end

      smm_wr.set_item_context(this, smm_sequencer);
      smm_wr.data = 0;
      smm_wr.data.mim_wreq_valid = 1;
      smm_wr.data.mim_wr_seg_ptr = 23;
      smm_wr.data.mim_wr_data = 'h00000CAFE;
      `uvm_send(smm_wr);
      smm_wr.data =0;
      `uvm_send(smm_wr);


      wait_n(22);
/*
      // Waiting for register to be implemented on RTL block
      `uvm_info(get_name(), "CSR backdoor access", UVM_LOW);
      ral.tx_pb_block.TX_PB_PORT_CFG.read(status, read_data, access_type);
      if (read_data != '0)
      begin
         `uvm_warning(get_name(), "TX_PB_PORT_CFG: Read data is different from zero");
      end else
      begin
         `uvm_info(get_name(), "TX_PB_PORT_CFG: Read data is zero", UVM_LOW);
      end
      wait_n(10);
      ral.tx_pb_block.TX_PB_PORT_CFG.write(status, '1, access_type);
      `uvm_info(get_name(), "TX_PB_PORT_CFG: Written data is 0xFFFFFFFF", UVM_LOW);
      wait_n(10);
      ral.tx_pb_block.TX_PB_PORT_CFG.read(status, read_data, access_type);
      if (read_data != 'hffffffff)
      begin
         `uvm_warning(get_name(), "TX_PB_PORT_CFG: Read data is different written data");
      end else
      begin
         `uvm_info(get_name(), "TX_PB_PORT_CFG: Read data is equal to written data", UVM_LOW);
      end
      wait_n(10);
*/
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
         uc_tag.data.ptr = $urandom_range(0, 2 ** 19 - 1);
         
         `uvm_info(get_name(),uc_tag.convert2string(),UVM_LOW)

         `uvm_send(uc_tag)
         uc_tag.data = 0;

         `uvm_send(uc_tag)

         traffic_manager.wait_if_paused(process_name);

      end

      `uvm_info(get_name(), "Finished mby_egr_tag_seq", UVM_LOW)
      wait_n(100);
   endtask

   virtual task body();
      super.body();
   endtask




endclass : mby_egr_tag_seq

