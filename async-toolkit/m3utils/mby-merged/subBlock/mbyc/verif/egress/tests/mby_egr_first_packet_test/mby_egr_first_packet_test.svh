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
// TODO: Test development stopped while working on RESET testing. This test 
// needs to be enhanced. 
//-----------------------------------------------------------------------------
// Copyright (c) 2018 by Intel Corporation This model is the confidential and
// proprietary property of Intel Corporation and the possession or use of this
// file requires a written license from Intel Corporation.
//------------------------------------------------------------------------------
// Modification history :
// 14.12.2018 : created
//-----------------------------------------------------------------------------
import shdv_reset_pkg::*;

`ifndef MBY_EGR_FIRST_PACKET_TEST__SVH
`define MBY_EGR_FIRST_PACKET_TEST__SVH

class mby_first_packet_seq extends mby_egr_env_base_seq;
   `uvm_object_utils (mby_first_packet_seq)

   // ============= TEST SEQUENCES =============
   mby_egr_tag_seq   egr_tag_seq;
   // ==========================================

   //TODO: Create another sequence/class to contain the traffic managers
   shdv_synchronization_pkg::shdv_process_manager  traffic_managers[$];
   mby_egr_extended_base_seq                       test_seqs[$];
   mby_egr_mid_reset_seq                           reset_seq;

   //---------------------------------------------------------------------------
   // Function: new
   //---------------------------------------------------------------------------
   function new(string name = "mby_first_packet_seq", uvm_component parent = null);
      super.new(name);
      // raise objection by default
      this.set_automatic_phase_objection(1);
   endfunction : new

   virtual task body();

      create_sequences();
      update_traffic_manager();


      if ($test$plusargs("RESET"))begin
         traffic_rst_traffic();
      end else begin
         send_traffic();
      end


      `uvm_info(get_name(), "Finished mby_egr_first_packet_seq", UVM_NONE)

   endtask : body

   extern protected task create_sequences();
   extern protected task update_traffic_manager();
   extern protected task send_traffic();
   extern protected task pull_reset();
   extern protected task traffic_rst_traffic();
   extern protected task config_phase();
   extern protected task pause_all_traffic();
   extern protected task resume_all_traffic();
   extern protected task kill_all_traffic();

endclass : mby_first_packet_seq

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

      env.set_reset_sequence("mby_egr_hard_reset_seq");

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
      env.set_main_sequence("mby_first_packet_seq");
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



//=================================================
//--------------EXTERN METHODS/TASKS---------------
//=================================================

//---------------------------------------------------------------------------
// Task: pause_all_traffic
//
// This task will pause the traffic being sent. Blocked until the
// process was correctly paused.
//---------------------------------------------------------------------------
task mby_first_packet_seq::pause_all_traffic();
   foreach (traffic_managers[idx]) begin
      if(traffic_managers[idx].is_started(traffic_managers[idx].name))begin
         `uvm_info(get_name(), $sformatf("[RST_DBG]: Pausing traffic for TRAFFIC_MANAGER(%s)",traffic_managers[idx].name), UVM_LOW)
         traffic_managers[idx].pause_process(traffic_managers[idx].name);

         if(!traffic_managers[idx].is_paused(traffic_managers[idx].name))begin
            `uvm_fatal(get_name(), "[RST_DBG]: Process was not paused correctly.")
         end

      end else begin
         `uvm_fatal(get_name(), $sformatf("[RST_DBG]: Traffic for process = %s can't be paused because the process hasn't started yet.",traffic_managers[idx].name))
      end
   end
endtask

//---------------------------------------------------------------------------
// Task: resume_all_traffic
//
// This task will force the process to resume the traffic.
//---------------------------------------------------------------------------
task mby_first_packet_seq::resume_all_traffic();
   foreach (traffic_managers[idx]) begin
      begin
         `uvm_info(get_name(), $sformatf("[RST_DBG]: Resuming traffic for TRAFFIC_MANAGER(%s)",traffic_managers[idx].name), UVM_LOW)
         traffic_managers[idx].resume_process(traffic_managers[idx].name);
         traffic_managers[idx].wait_until_finished(traffic_managers[idx].name);
      end
   end
endtask

//---------------------------------------------------------------------------
// Task: kill_all_traffic
//
// This task will force the process to stop sending traffic. This allows the
// test to finish.
//---------------------------------------------------------------------------
task mby_first_packet_seq::kill_all_traffic();
   foreach (traffic_managers[idx]) begin
      traffic_managers[idx].kill_process(traffic_managers[idx].name);
   end
endtask

task mby_first_packet_seq::create_sequences();
   egr_tag_seq = mby_egr_tag_seq::type_id::create("egr_tag_seq");
   test_seqs.push_back(egr_tag_seq);

   if($test$plusargs("RESET"))begin
      reset_seq = mby_egr_mid_reset_seq::type_id::create("reset_seq");
   end
endtask

task mby_first_packet_seq:: update_traffic_manager();
   foreach(test_seqs[idx])begin
      traffic_managers.push_back(test_seqs[idx].traffic_manager);
   end
endtask : update_traffic_manager

task mby_first_packet_seq:: send_traffic();

   foreach(test_seqs[idx])begin
      `uvm_info(get_name(), $sformatf("[RST_DBG]: Sending sequence = %s", test_seqs[idx].get_name()), UVM_NONE)
      fork
         begin
            `uvm_send(test_seqs.pop_back());
         end
      join_none
   end
   wait fork;
endtask : send_traffic

task mby_first_packet_seq:: pull_reset();

   reset_seq.randomize() with {
      reset_seq.rst_type == HARD_RESET;
      reset_seq.duration inside {[400:500]};
   };
   `uvm_info(get_type_name(), $sformatf("[RST_DBG]: Pulling %s", reset_seq.rst_type.name()), UVM_NONE)
   `uvm_send(reset_seq)

endtask : pull_reset

task mby_first_packet_seq:: traffic_rst_traffic();


   fork
      send_traffic();
   join_none

   wait_n(50);
   if($test$plusargs("CLEAN_RESET"))begin
      `uvm_info(get_name(), "[RST_DBG]: Clean Reset flow", UVM_NONE)
      pause_all_traffic();
      
      //TODO: Replace with shutdown_seq();  
      wait_n(200);
      
      pull_reset();
      config_phase();
      resume_all_traffic();
   end else if($test$plusargs("DIRTY_RESET"))begin
      `uvm_info(get_name(), "[RST_DBG]: Dirty Reset flow", UVM_NONE)
      fork
         kill_all_traffic();
         pull_reset();
      join
      config_phase();
      create_sequences();
      update_traffic_manager();
      send_traffic();
   end

endtask : traffic_rst_traffic
task mby_first_packet_seq:: config_phase();
   //TODO: Replace with CONFIG_PHASE
   `uvm_warning(get_type_name(), "Waiting for 500 cycles. Simulating the CONFIG_PHASE. Replace the wait for the actual config sequence once it's available.")
   wait_n(200);
endtask : config_phase
`endif

