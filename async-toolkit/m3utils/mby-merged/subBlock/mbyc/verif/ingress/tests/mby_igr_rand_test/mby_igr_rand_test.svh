//-----------------------------------------------------------------------------
// Title         : Ingress Random test
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : mby_igr_rand_test.svh
// Author        : jose.j.godinez.carrillo  <jjgodine@ichips.intel.com>
// Created       : 21.08.2018
// Last modified : 21.08.2018
//-----------------------------------------------------------------------------
// Description :
//
//-----------------------------------------------------------------------------
// Copyright (c) 2018 by Intel Corporation This model is the confidential and
// proprietary property of Intel Corporation and the possession or use of this
// file requires a written license from Intel Corporation.
//------------------------------------------------------------------------------
// Modification history :
// 21.08.2018 : created
//-----------------------------------------------------------------------------
`ifndef MBY_IGR_RAND_TEST__SVH
`define MBY_IGR_RAND_TEST__SVH

//-----------------------------------------------------------------------------
// Class: mby_igr_eth_simple_seq
//-----------------------------------------------------------------------------
class mby_igr_dummy_seq extends mby_igr_extended_base_seq;

   `uvm_object_utils(mby_igr_dummy_seq)

   //---------------------------------------------------------------------------
   // Function: new
   //---------------------------------------------------------------------------
   function new(string name = "mby_igr_dummy_seq", uvm_component parent = null);
      super.new(name);
      // raise objection by default
      this.set_automatic_phase_objection(1);
   endfunction : new

   //---------------------------------------------------------------------------
   // Task: body()
   //---------------------------------------------------------------------------
   virtual task body();
      #100ps;
      `uvm_info(get_name(), "Finished mby_igr_dummy_seq", UVM_LOW)
   endtask : body

endclass : mby_igr_dummy_seq

//-----------------------------------------------------------------------------
// Class: mby_igr_eth_simple_seq
//-----------------------------------------------------------------------------
class mby_igr_eth_simple_seq extends mby_igr_extended_base_seq;

   `uvm_object_utils(mby_igr_eth_simple_seq)

   eth_frame      los_frames[4];
   eth_sequencer  los_sequencers[4];

   //---------------------------------------------------------------------------
   // Function: new()
   //---------------------------------------------------------------------------
   function new (string name="mby_igr_eth_simple_seq");
      super.new (name);
      // raise objection by default
      this.set_automatic_phase_objection(1);
      `uvm_info(this.get_name(), ("mby_igr_eth_simple_seq::new"), UVM_LOW)
   endfunction : new

   //---------------------------------------------------------------------------
   // Task: body()
   //---------------------------------------------------------------------------
   virtual task body();
      int count[4] = {0,0,0,0};
      this.set_name("mby_igr_eth_simple_seq");

      `uvm_info("TST", ("Starting eth simple sequence..."), UVM_LOW)
      foreach(los_sequencers[i]) begin
         assert($cast(los_sequencers[i], 
            shdv_base_pkg::shdv_base_tb_sequencer::pick_sequencer($sformatf("eth_bfm_%0d_rx0", i))))
         else begin
            `uvm_error(get_name(), $sformatf("Could not get a pointer to the sequencer%0d", i));
         end
      end
      foreach(los_frames[i]) begin
         los_frames[i] = eth_frame::type_id::create($sformatf("los_frames_%0d", i));
         los_frames[i].set_item_context(this, los_sequencers[i]);
      end
      foreach(los_frames[i]) begin
         automatic int auto_i = i;
         fork
           begin
             repeat (20) begin
               assert(los_frames[auto_i].randomize() with {
                 bubble         == 0;
                 kind           inside {BASIC_FRAME,
                                        IPV4_FRAME,
                                        IPV6_FRAME};
                 payload.size() inside {[64:66]};
                 dmac            == 'h000102030405 + count[auto_i];
                 smac            == 'h060708090a0b + count[auto_i];
                 tc              == count[auto_i][3:0];
                 (kind == BASIC_FRAME) ->
                    foreach (payload[idx])
                      payload[idx] == idx;
                 })
               else begin
                     `uvm_error(get_name(), "Unable to randomize eth_pkt");
               end
               count[auto_i]++;
               `uvm_info("TST", $sformatf("Started eth_frame %0d %0d", auto_i, count[auto_i]), UVM_HIGH)
               `uvm_send(los_frames[auto_i])
               `uvm_info("TST", $sformatf("Sent eth_frame %0d %0d", auto_i, count[auto_i]), UVM_LOW)
             end
           end
         join_none
      end

      wait fork;
   endtask

endclass : mby_igr_eth_simple_seq

class mby_report_server extends uvm_default_report_server;
   virtual function string compose_report_message( uvm_report_message report_message,
                                                   string report_object_name = "" );
      uvm_severity severity  = report_message.get_severity();
      string       name      = report_message.get_report_object().get_full_name();
      string       id        = report_message.get_id();
      string       message   = report_message.get_message();
      string       filename  = report_message.get_filename();
      int          line      = report_message.get_line();
      string       filename_ = "";

      foreach(filename[i]) begin
         if(filename[i] == "/")
            filename_ = "";
         else
            filename_ = {filename_, filename[i]};
      end

      return $sformatf( "%-14s | %24s(%05d) @ %0t | %-36s | %-7s | %s",
                        severity.name(), filename_, line, $time, name, id, message );
   endfunction: compose_report_message
endclass : mby_report_server

//-----------------------------------------------------------------------------
// Class: mby_igr_rand_test
//-----------------------------------------------------------------------------
class mby_igr_rand_test extends mby_igr_base_test;

   `uvm_component_utils(mby_igr_rand_test)

   //---------------------------------------------------------------------------
   // Function: new()
   //---------------------------------------------------------------------------
   function new (string name="mby_igr_rand_test", uvm_component parent=null);
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
         "env.mby_igr_tb_sequencer.reset_phase",
         "default_sequence",
         mby_igr_dummy_seq::type_id::get());

      // Specifying post_reset phase sequence
      uvm_config_db#(uvm_object_wrapper)::set(this,
         "env.mby_igr_tb_sequencer.post_reset_phase",
         "default_sequence",
         mby_igr_dummy_seq::type_id::get());

      // Specifying configure phase sequence
      uvm_config_db#(uvm_object_wrapper)::set(this,
         "env.mby_igr_tb_sequencer.configure_phase",
         "default_sequence",
         mby_igr_dummy_seq::type_id::get());

      // Specifying shutdown phase sequence
      uvm_config_db#(uvm_object_wrapper)::set(this,
         "env.mby_igr_tb_sequencer.shutdown_phase",
         "default_sequence",
         mby_igr_dummy_seq::type_id::get());

      // Specifying main phase sequence
      uvm_config_db#(uvm_object_wrapper)::set(this,
         "env.mby_igr_tb_sequencer.main_phase",
         "default_sequence",
         mby_igr_eth_simple_seq::type_id::get());
   endfunction : set_default_sequences

      uvm_config_db#(uvm_object_wrapper)::set(this, "env.tb_seqr.reset_phase",      "default_sequence", mby_igr_dummy_seq::type_id::get());
      uvm_config_db#(uvm_object_wrapper)::set(this, "env.tb_seqr.post_reset_phase", "default_sequence", mby_igr_dummy_seq::type_id::get());
      uvm_config_db#(uvm_object_wrapper)::set(this, "env.tb_seqr.configure_phase",  "default_sequence", mby_igr_dummy_seq::type_id::get());
      uvm_config_db#(uvm_object_wrapper)::set(this, "env.tb_seqr.shutdown_phase",   "default_sequence", mby_igr_dummy_seq::type_id::get());
      uvm_config_db#(uvm_object_wrapper)::set(this, "env.tb_seqr.main_phase",       "default_sequence", mby_igr_eth_simple_seq::type_id::get());

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

endclass : mby_igr_rand_test

`endif
