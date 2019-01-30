//-----------------------------------------------------------------------------
// Title         : Egress extended base sequence
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : mby_egr_extended_base_seq.sv
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

//-----------------------------------------------------------------------------
// Class: mby_egr_extended_base_seq
//-----------------------------------------------------------------------------

class mby_egr_extended_base_seq extends mby_egr_env_base_seq;

   `uvm_object_utils(mby_egr_extended_base_seq)

   string process_name;
   shdv_synchronization_pkg::shdv_process_manager    traffic_manager;
   mby_egr_env    env;

//PJP  `uvm_declare_p_sequencer(slu_sequencer)

   //---------------------------------------------------------------------------
   // Function: new
   //---------------------------------------------------------------------------
   function new(input string name = "mby_egr_extended_base_seq",
         uvm_sequencer_base sequencer=null, uvm_sequence parent_seq=null);
      super.new(name /*, sequencer, parent_seq*/);
      env = mby_egr_env::get_egr_env();

   endfunction

   function void build_phase(uvm_phase phase);
   endfunction : build_phase

   //---------------------------------------------------------------------------
   // Function: wait_n
   //
   // This function will cause a delay of n cycles
   //
   // ARGUMENTS:
   //   int n - number of cycles to wait for
   //---------------------------------------------------------------------------
   task wait_n(int n);
      repeat(n) @(posedge this.env.egress_if.clock);
   endtask : wait_n

   //---------------------------------------------------------------------------
   // Function: set_name
   //
   // User should define the name of the sequence in the test sequence inside
   // new method. For print purposes.
   //---------------------------------------------------------------------------
   virtual task set_process_name(string s);
      this.process_name = s;
      traffic_manager = new(process_name);
      `uvm_info(get_name(), $sformatf("[RST_DBG]: process_name = %s ", process_name), UVM_NONE)
   endtask

   //---------------------------------------------------------------------------
   // Task: body_thread
   //
   // This task should be overridden by the user. This task should contain
   // the randomization of the seq_item and the `uvm_send/do call.
   //---------------------------------------------------------------------------
   virtual protected task body_thread();
   endtask

   //---------------------------------------------------------------------------
   // Task: body
   //
   // This task registers the traffic process into the process manager.
   // This task is in charge of pausing the traffic when notified by the
   // process manager. User can modify when to pause the traffic. Randomized
   // delay by default.
   //---------------------------------------------------------------------------
   virtual task body();
      if($test$plusargs("RESET"))begin
         `uvm_info(get_name(), $sformatf("[RST_DBG]: process name = %s", process_name), UVM_NONE)
         traffic_manager.process_started(process_name);

         while(!traffic_manager.is_killed(process_name))begin
            traffic_manager.wait_if_paused(process_name);
            body_thread();
         end

         traffic_manager.process_finished(process_name);
      end else begin
         body_thread();
      end
   endtask

endclass : mby_egr_extended_base_seq
