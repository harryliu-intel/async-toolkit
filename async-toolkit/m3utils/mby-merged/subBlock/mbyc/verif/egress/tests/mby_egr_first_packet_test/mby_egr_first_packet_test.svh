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
   mby_base_pkg::mby_base_sequencer#(.T_req(mby_tag_bfm_pkg::mby_tag_bfm_uc_xaction)) tag_sequencer;

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
      this.set_name("mby_egr_tag_seq");
      wait_n(22);
      
    


      assert($cast(tag_sequencer, mby_egr_env_pkg::shdv_base_tb_sequencer::pick_sequencer("tag_bfm_uc_0")))

      else begin
         `uvm_error(get_name(), "Could not get a pointer to the tag sequencer");
      end
      uc_tag = mby_tag_bfm_pkg::mby_tag_bfm_uc_xaction::type_id::create("uc_tag");
      uc_tag.set_item_context(this, tag_sequencer);
      uc_tag.randomize();
      
      
      uc_tag.data_pkt = 0;
      uc_tag.data_pkt.valid = 1;
      uc_tag.data_pkt.dst_tc = 0;
      uc_tag.data_pkt.dst_port = 0;
      uc_tag.data_pkt.src_port = 0;
      uc_tag.data_pkt.src_tc = 0;
      uc_tag.data_pkt.sll = 0;
      uc_tag.data_pkt.eop = 1'b1;
      uc_tag.data_pkt.length = 64;
      uc_tag.data_pkt.ptr_toggle = 4'b1;
      uc_tag.data_pkt.ptr = 23;
      

      
      
      

      `uvm_info(get_name(),uc_tag.convert2string(),UVM_LOW)

      `uvm_send(uc_tag)
      
      `uvm_info(get_name(), "Finished mby_egr_tag_seq", UVM_LOW)
      wait_n(20);
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
