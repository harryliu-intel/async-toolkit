//-----------------------------------------------------------------------------
// Title         : Ingress base test
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : ingress_base_test.svh
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
`ifndef INGRESS_BASE_TEST__SVH
`define INGRESS_BASE_TEST__SVH

class ingress_base_test extends uvm_test;

  `uvm_component_utils(ingress_base_test)

  // Variable: env
  // The egress environment class
  ingress_env env;

  // Function: new
  // Constructor
  function new (string name="ingress_base_test", uvm_component parent=null);
    super.new (name, parent);
    $timeformat(-9, 0, "ns", 10);
  endfunction : new

  extern function void build_phase(uvm_phase phase);
  extern function void connect_phase(uvm_phase phase);
  extern virtual function void report_phase(uvm_phase phase);

endclass : ingress_base_test

function void ingress_base_test::report_phase(uvm_phase phase);
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

endfunction

function void ingress_base_test::build_phase(uvm_phase phase);
  uvm_report_info(get_full_name(),"Build", UVM_LOG);
  env = ingress_env::type_id::create("env",this);
endfunction : build_phase

function void ingress_base_test::connect_phase(uvm_phase phase);
  super.connect_phase(phase);
  // START IOSF_NOT_PRESENT
  //env.set_test_phase_type("env", "HARD_RESET_PHASE", "ingress_hard_reset_seq");
  //env.set_test_phase_type("env", "CONFIG_PHASE", "ingress_pci_config_seq");
  // END IOSF_NOT_PRESENT
endfunction : connect_phase

`endif
