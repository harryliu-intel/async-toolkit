//-----------------------------------------------------------------------------
// Title         : Ingress base test
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : mby_igr_base_test.svh
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
`ifndef MBY_IGR_BASE_TEST__SVH
`define MBY_IGR_BASE_TEST__SVH

//-----------------------------------------------------------------------------
// Class: mby_igr_base_test
//-----------------------------------------------------------------------------
class mby_igr_base_test extends uvm_test;

   `uvm_component_utils(mby_igr_base_test)

   mby_igr_env env;

   //-----------------------------------------------------------------------------
   // Function: new()
   //-----------------------------------------------------------------------------
   function new (string name="mby_igr_base_test", uvm_component parent=null);
      super.new (name, parent);
      $timeformat(-9, 0, "ns", 10);
   endfunction : new

   extern function void build_phase(uvm_phase phase);
   extern function void connect_phase(uvm_phase phase);
   extern virtual function void report_phase(uvm_phase phase);
   extern virtual function void set_type_overrides();
   extern virtual function void set_default_sequences();

endclass : mby_igr_base_test

//-----------------------------------------------------------------------------
// Function: report_phase()
//-----------------------------------------------------------------------------
function void mby_igr_base_test::report_phase(uvm_phase phase);
   uvm_report_server foo;
   int err_cnt;
   super.report_phase(phase);
   foo = uvm_top.get_report_server();

   // sum num of errors from sequences (use global report server)
   err_cnt = foo.get_severity_count(UVM_ERROR) + foo.get_severity_count(UVM_FATAL);

   // sum num of errors from this test.
   foo = get_report_server();
   err_cnt += (foo.get_severity_count(UVM_ERROR) + foo.get_severity_count(UVM_FATAL));

   if (err_cnt == 0) begin
      uvm_report_info(get_full_name(),  "                                                    ", UVM_LOG);
      uvm_report_info(get_full_name(),  "         _______  _______  _______  _______         ", UVM_LOG);
      uvm_report_info(get_full_name(),  "        |       ||   _   ||       ||       |        ", UVM_LOG);
      uvm_report_info(get_full_name(),  "        |    _  ||  |_|  ||  _____||  _____|        ", UVM_LOG);
      uvm_report_info(get_full_name(),  "        |   |_| ||       || |_____ | |_____         ", UVM_LOG);
      uvm_report_info(get_full_name(),  "        |    ___||       ||_____  ||_____  |        ", UVM_LOG);
      uvm_report_info(get_full_name(),  "        |   |    |   _   | _____| | _____| |        ", UVM_LOG);
      uvm_report_info(get_full_name(),  "        |___|    |__| |__||_______||_______|        ", UVM_LOG);
      uvm_report_info(get_full_name(),  "                                                    ", UVM_LOG);
   end
   else begin
      uvm_report_info(get_full_name(),  "                                                    ", UVM_LOG);
      uvm_report_info(get_full_name(),  "         _______  _______  ___   ___                ", UVM_LOG);
      uvm_report_info(get_full_name(),  "        |       ||   _   ||   | |   |               ", UVM_LOG);
      uvm_report_info(get_full_name(),  "        |    ___||  |_|  ||   | |   |               ", UVM_LOG);
      uvm_report_info(get_full_name(),  "        |   |___ |       ||   | |   |               ", UVM_LOG);
      uvm_report_info(get_full_name(),  "        |    ___||       ||   | |   |___            ", UVM_LOG);
      uvm_report_info(get_full_name(),  "        |   |    |   _   ||   | |       |           ", UVM_LOG);
      uvm_report_info(get_full_name(),  "        |___|    |__| |__||___| |_______|           ", UVM_LOG);
      uvm_report_info(get_full_name(),  "                                                    ", UVM_LOG);
      uvm_report_info(get_full_name(),  "                                                    ", UVM_LOG);
   end

endfunction : report_phase

//-----------------------------------------------------------------------------
// Function: build_phase()
//-----------------------------------------------------------------------------
function void mby_igr_base_test::build_phase(uvm_phase phase);
   uvm_report_info(get_full_name(),"Build", UVM_LOG);
   env = mby_igr_env::type_id::create("env",this);

   set_type_overrides();
endfunction : build_phase

//-----------------------------------------------------------------------------
// Function: connect_phase()
//-----------------------------------------------------------------------------
function void mby_igr_base_test::connect_phase(uvm_phase phase);
   super.connect_phase(phase);
   set_default_sequences();
endfunction : connect_phase

//-----------------------------------------------------------------------------
// Function: set_type_overrides()
//-----------------------------------------------------------------------------
function void mby_igr_base_test::set_type_overrides();
endfunction : set_type_overrides

//-----------------------------------------------------------------------------
// Function: set_default_sequences()
//-----------------------------------------------------------------------------
function void mby_igr_base_test::set_default_sequences();

   // Set up default cold reset sequence
//PJP: TODO Uncomment the following code once reset and configuration sequences are devloped.
/*
   uvm_config_db#(uvm_object_wrapper)::set(this, "env.mby_igr_tb_sequencer.reset_phase", "default_sequence", igr_env_pkg::mby_igr_hard_reset_seq::type_id::get());
   uvm_config_db#(int unsigned)::set(this, "env.mby_igr_tb_sequencer.reset_phase", "default_sequence.min_random_count", 1);
   uvm_config_db#(int unsigned)::set(this, "env.mby_igr_tb_sequencer.reset_phase", "default_sequence.max_random_count", 1);

   // Set up default CSR post reset sequence
   uvm_config_db#(uvm_object_wrapper)::set(this, "env.mby_igr_tb_sequencer.post_reset_phase", "default_sequence", igr_env_pkg::mby_igr_post_reset_seq::type_id::get());
   uvm_config_db#(int unsigned)::set(this, "env.mby_igr_tb_sequencer.post_reset_phase", "default_sequence.min_random_count", 1);
   uvm_config_db#(int unsigned)::set(this, "env.mby_igr_tb_sequencer.post_reset_phase", "default_sequence.max_random_count", 1);

   // Set up default CSR configuration sequence
   uvm_config_db#(uvm_object_wrapper)::set(this, "env.mby_igr_tb_sequencer.configure_phase", "default_sequence", igr_env_pkg::mby_igr_config_seq::type_id::get());
   uvm_config_db#(int unsigned)::set(this, "env.mby_igr_tb_sequencer.configure_phase", "default_sequence.min_random_count", 1);
   uvm_config_db#(int unsigned)::set(this, "env.mby_igr_tb_sequencer.configure_phase", "default_sequence.max_random_count", 1);

   // Set up default shutdown sequence
   uvm_config_db#(uvm_object_wrapper)::set(this, "env.mby_igr_tb_sequencer.shutdown_phase", "default_sequence", igr_env_pkg::mby_igr_shutdown_seq::type_id::get());
   uvm_config_db#(int unsigned)::set(this, "env.mby_igr_tb_sequencer.shutdown_phase", "default_sequence.min_random_count", 1);
   uvm_config_db#(int unsigned)::set(this, "env.mby_igr_tb_sequencer.shutdown_phase", "default_sequence.max_random_count", 1);
*/

   uvm_config_db#(int unsigned)::set(this, "env.mby_igr_tb_sequencer.main_phase", "default_sequence.min_random_count", 1);
   uvm_config_db#(int unsigned)::set(this, "env.mby_igr_tb_sequencer.main_phase", "default_sequence.max_random_count", 1);

endfunction : set_default_sequences

`endif
