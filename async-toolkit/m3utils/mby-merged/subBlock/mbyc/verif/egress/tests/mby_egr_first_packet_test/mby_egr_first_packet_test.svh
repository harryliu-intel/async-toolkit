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

`ifndef MBY_EGR_FIRST_PACKET_TEST__SVH
`define MBY_EGR_FIRST_PACKET_TEST__SVH

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

   //---------------------------------------------------------------------------
   // Task: body()
   //---------------------------------------------------------------------------
   virtual task body();
       
      uvm_status_e status;
      uvm_path_e access_type = UVM_BACKDOOR;
      uvm_reg_data_t read_data;
      mby_egr_reg_pkg::mby_egr_reg_blk ral;
      ral = env.tb_ral;
       
      this.set_name("mby_egr_tag_seq");
      
      
      
      
      smm_wr = mby_smm_bfm_pkg::mby_smm_bfm_mwr_req_xaction::type_id::create("smm_wr");
      assert($cast(smm_sequencer, env.get_tb_seqr().pick_sequencer("smm_bfm_wr_req")))

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

      assert($cast(tag_sequencer, env.get_tb_seqr().pick_sequencer("tag_bfm_uc_0")))

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
         //wait_n(1);
       end
      `uvm_info(get_name(), "Finished mby_egr_tag_seq", UVM_LOW)
      wait_n(100);
   endtask : body



endclass : mby_egr_tag_seq


//-----------------------------------------------------------------------------
// Class: mby_egr_first_packet_test
//-----------------------------------------------------------------------------
class mby_egr_first_packet_test extends mby_egr_base_test;

   `uvm_component_utils(mby_egr_first_packet_test)

   //---------------------------------------------------------------------------
   // Function: new()
   //---------------------------------------------------------------------------
   function new (string name="mby_egr_first_packet_test", uvm_component parent=null);
      super.new (name, parent);
   endfunction : new

   //---------------------------------------------------------------------------
   // Function: build_phase()
   //---------------------------------------------------------------------------
   function void build_phase(uvm_phase phase);
      super.build_phase(phase);
      `uvm_info("build_phase()", "Exiting build_phase", UVM_NONE)
   endfunction : build_phase

   //---------------------------------------------------------------------------
   // Function: connect_phase()
   // sequences are defined for each of the run sub-phases.
   // user data phase sequence is set to egress_eth_simple_seq.
   //---------------------------------------------------------------------------
   function void connect_phase(uvm_phase phase);
      super.connect_phase(phase);
      `uvm_info("connect_phase()", "Exiting connect_phase", UVM_NONE)
   endfunction : connect_phase

   //---------------------------------------------------------------------------
   // Function: set_default_sequences()
   //---------------------------------------------------------------------------
   function void set_default_sequences();
      super.set_default_sequences();
      `uvm_info("::set_default_sequences", "Setting phase sequences", UVM_NONE)
/*
      // Specifying reset phase sequence
      uvm_config_db#(uvm_object_wrapper)::set(this,
         "env.mby_egr_tb_sequencer.reset_phase",
         "default_sequence",
         mby_egr_dummy_seq::type_id::get());

      // Specifying post_reset phase sequence
      uvm_config_db#(uvm_object_wrapper)::set(this,
         "env.mby_egr_tb_sequencer.post_reset_phase",
         "default_sequence",
         mby_egr_dummy_seq::type_id::get());

      // Specifying configure phase sequence
      uvm_config_db#(uvm_object_wrapper)::set(this,
         "env.mby_egr_tb_sequencer.configure_phase",
         "default_sequence",
         mby_egr_dummy_seq::type_id::get());

      // Specifying shutdown phase sequence
      uvm_config_db#(uvm_object_wrapper)::set(this,
         "env.mby_egr_tb_sequencer.shutdown_phase",
         "default_sequence",
         mby_egr_dummy_seq::type_id::get());

      // Specifying main phase sequence
      uvm_config_db#(uvm_object_wrapper)::set(this,
         "env.mby_egr_tb_sequencer.main_phase",
         "default_sequence",
         mby_egr_tag_seq::type_id::get());
      
     */ 
      
           env.set_reset_sequence("mby_egr_dummy_seq");

      // Specifying post_reset phase sequence
     /*
      uvm_config_db#(uvm_object_wrapper)::set(this,
         "env.tb_seqr.post_reset_phase",
         "default_sequence",
         mby_igr_dummy_seq::type_id::get()); */

      // Specifying configure phase sequence
      env.set_configure_sequence("mby_egr_dummy_seq");

      // Specifying shutdown phase sequence
      env.set_shutdown_sequence("mby_egr_dummy_seq");

      // Specifying main phase sequence
      env.set_main_sequence("mby_egr_tag_seq");
   endfunction : set_default_sequences

   //---------------------------------------------------------------------------
   // Function: start_of_simulation_phase()
   //
   // Updates the uvm report server to use MBY's report server
   //
   //---------------------------------------------------------------------------
   function void start_of_simulation_phase( uvm_phase phase );
      mby_report_server mby_server = new;
      super.start_of_simulation_phase( phase );
      //uvm_report_server::set_server( mby_server );
      `uvm_info("start_of_simulation_phase()", "Exiting start_of_sim phase", UVM_NONE)
   endfunction: start_of_simulation_phase

   //---------------------------------------------------------------------------
   // Task: run_phase()
   //
   // Prints out a message to identify start of run phase
   //---------------------------------------------------------------------------
   task run_phase(uvm_phase phase);
      phase.raise_objection(this);
      `uvm_info("::run_phase()", "Starting run phase", UVM_NONE)
      phase.drop_objection(this);
   endtask : run_phase

endclass : mby_egr_first_packet_test


`endif
