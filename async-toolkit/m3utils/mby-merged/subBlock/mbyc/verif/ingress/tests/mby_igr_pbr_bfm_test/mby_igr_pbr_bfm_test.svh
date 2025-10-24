// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

//-----------------------------------------------------------------------------
// Title         : Madison Bay PBR BFM Test
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : mby_igr_pbr_bfm_test.svh
// Author        : ricardo.a.alfaro.gomez  <raalfaro@ichips.intel.com>
// 2ry contact   : jose.j.godinez.carrillo  <jjgodine@ichips.intel.com>
// Created       : 01.28.2019
//-----------------------------------------------------------------------------
// Description : Temporal test created for debugging pbr bfm interfaces 
// FIXME: Separate igr/egr
//-----------------------------------------------------------------------------
// Copyright (c) 2018 by Intel Corporation This model is the confidential and
// proprietary property of Intel Corporation and the possession or use of this
// file requires a written license from Intel Corporation.
//------------------------------------------------------------------------------
// Modification history :
// 21.08.2018 : created
//-----------------------------------------------------------------------------
`ifndef MBY_IGR_PBR_BFM_TEST__SVH
`define MBY_IGR_PBR_BFM_TEST__SVH

typedef class mby_igr_cptr_debug_seq;
typedef class mby_igr_dptr_debug_seq;

class mby_ptr_debug_seq extends mby_igr_extended_base_seq;
   `uvm_object_utils(mby_ptr_debug_seq)

   mby_pbr_bfm_pkg::mby_pbr_bfm_dptr_xaction dptr_xaction;
   mby_base_pkg::mby_base_sequencer#(.T_req(mby_pbr_bfm_pkg::mby_pbr_bfm_dptr_xaction)) dptr_sequencer;

   function new (string name="mby_igr_dptr_debug_seq");
      super.new (name);
      this.set_automatic_phase_objection(1);
   endfunction : new

   virtual task body();
      mby_igr_cptr_debug_seq cptr_ss;
      mby_igr_dptr_debug_seq dptr_ss;
      fork
         //`uvm_do(cptr_ss)
         //`uvm_do(dptr_ss)
      join
   endtask: body
endclass: mby_ptr_debug_seq

class mby_igr_dptr_debug_seq extends mby_igr_extended_base_seq;

   `uvm_object_utils(mby_igr_dptr_debug_seq)

   mby_pbr_bfm_pkg::mby_pbr_bfm_dptr_xaction dptr_xaction;
   shdv_base_pkg::shdv_base_sequencer#(.T_req(mby_pbr_bfm_pkg::mby_pbr_bfm_dptr_xaction)) dptr_sequencer;

   function new (string name="mby_igr_dptr_debug_seq");
      super.new (name);
      // raise objection by default
      this.set_automatic_phase_objection(1);
   endfunction : new

   virtual task body();
      automatic int auto_i = 0;

      this.set_name("mby_igr_dptr_debug_seq");
      wait_n(10);

      if(!$cast(dptr_sequencer, env.get_tb_seqr().pick_sequencer("igr_pbr_bfm_dpb_sequencer")))
      begin
         `uvm_fatal(get_name(), "DBG_ALF: Could not get a pointer to the sequencer igr_pbr_bfm_dpb_sequencer");
      end

      dptr_xaction = mby_pbr_bfm_pkg::mby_pbr_bfm_dptr_xaction::type_id::create("dptr_xaction");
      dptr_xaction.set_item_context(this, dptr_sequencer);
      `uvm_info(get_name(), ("DBG_ALF: Done getting dpb sequencer pointer..."), UVM_DEBUG)

      repeat (2) begin
         auto_i++;
         if(!dptr_xaction.randomize() with {
                  data.data_dirty_ptr == 9 + auto_i;
                  data.pod_put_req == 1;
                  data.pod_put_type == 0;
                  data.pod_put_ack == 0;
                  data.schedule_stall == 0;
               })begin
            `uvm_error(get_name(), "Unable to randomize dptr_xaction");
         end
         `uvm_info(get_name(), $sformatf("DBG_ALF: Start mby_igr_dptr_debug_seq auto_i=0x%0x:: %s",auto_i,dptr_xaction.convert2string()), UVM_DEBUG)
         `uvm_send(dptr_xaction)
         `uvm_info(get_name(), $sformatf("DBG_ALF: End mby_igr_dptr_debug_seq auto_i=0x%0x",auto_i), UVM_DEBUG)
         wait_n(5);
      end
      wait_n(30);
   endtask
endclass : mby_igr_dptr_debug_seq

class mby_igr_cptr_debug_seq extends mby_igr_extended_base_seq;

   `uvm_object_utils(mby_igr_cptr_debug_seq)

   mby_pbr_bfm_pkg::mby_pbr_bfm_cptr_xaction cptr_xaction;
   shdv_base_pkg::shdv_base_sequencer#(.T_req(mby_pbr_bfm_pkg::mby_pbr_bfm_cptr_xaction)) cptr_sequencer;

   function new (string name="mby_igr_cptr_debug_seq");
      super.new (name);
      // raise objection by default
      this.set_automatic_phase_objection(1);
   endfunction : new

   virtual task body();
      automatic int auto_i = 5;

      this.set_name("mby_igr_cptr_debug_seq");
      wait_n(10);

      if(!$cast(cptr_sequencer, env.get_tb_seqr().pick_sequencer("egr_pbr_bfm_csp_sequencer")))
      begin
         `uvm_error(get_name(), "Could not get a pointer to the sequencer egr_pbr_bfm_csp_sequencer");
      end

      cptr_xaction = mby_pbr_bfm_pkg::mby_pbr_bfm_cptr_xaction::type_id::create("cptr_xaction");
      cptr_xaction.set_item_context(this, cptr_sequencer);
      `uvm_info(get_name(), ("DBG_ALF: Done getting csp sequencer pointer..."), UVM_DEBUG)

      repeat (3) begin
         auto_i++;
         if(!cptr_xaction.randomize() with {
                  data.req_seg_ptr == 9;
                  data.req_valid == 1;
                  data.req_id == auto_i;
                  data.fifo_ack == 0;
                  data.data_valid == 0;
                  data.data_clean_ptr == 0;
                  data.id_ack_id == 0;
                  data.id_ack_valid == 0;
               })begin
            `uvm_error(get_name(), "Unable to randomize cptr_xaction");
         end
         `uvm_info(get_name(), $sformatf("DBG_ALF: Start mby_igr_cptr_debug_seq auto_i=0x%0x:: %s",auto_i,cptr_xaction.convert2string()), UVM_DEBUG)
         `uvm_send(cptr_xaction)
         `uvm_info(get_name(), $sformatf("DBG_ALF: End mby_igr_cptr_debug_seq auto_i=0x%0x",auto_i), UVM_DEBUG)
         wait_n(10);
      end
      wait_n(10);
   endtask
endclass : mby_igr_cptr_debug_seq


//-----------------------------------------------------------------------------
// Class: mby_igr_pbr_bfm_test
//-----------------------------------------------------------------------------
class mby_igr_pbr_bfm_test extends mby_igr_base_test;

   `uvm_component_utils(mby_igr_pbr_bfm_test)

   //---------------------------------------------------------------------------
   // Function: new()
   //---------------------------------------------------------------------------
   function new (string name="mby_igr_pbr_bfm_test", uvm_component parent=null);
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
      env.set_reset_sequence("mby_igr_dummy_seq");

      // Specifying post_reset phase sequence
      /*
       uvm_config_db#(uvm_object_wrapper)::set(this,
       "env.tb_seqr.post_reset_phase",
       "default_sequence",
       mby_igr_dummy_seq::type_id::get()); */

      // Specifying configure phase sequence
      env.set_configure_sequence("mby_igr_dummy_seq");

      // Specifying shutdown phase sequence
      env.set_shutdown_sequence("mby_igr_dummy_seq");

      // Specifying main phase sequence
      env.set_main_sequence("mby_ptr_debug_seq");

   endfunction: set_default_sequences

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
   endtask: run_phase

endclass: mby_igr_pbr_bfm_test

`endif
