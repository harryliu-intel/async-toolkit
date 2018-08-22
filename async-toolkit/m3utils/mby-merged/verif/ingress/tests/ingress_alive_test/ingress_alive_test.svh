//-----------------------------------------------------------------------------
// Title         : Ingress alive test
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : ingress_alive_test.svh
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
`ifndef INGRESS_ALIVE_TEST__SVH
`define INGRESS_ALIVE_TEST__SVH

class ingress_dummy_seq extends ingress_extended_base_seq;

  `uvm_object_utils(ingress_dummy_seq)

  function new(string name = "ingress_dummy_seq", uvm_component parent = null);
    super.new(name);
  endfunction

  // Task: pre_body()
  // Raise an objection before main body.
  virtual task pre_body();
    uvm_phase phase;
    super.pre_body();
    phase = starting_phase;
    if (phase != null) begin
      phase.raise_objection(this);
    end
  endtask: pre_body

  // Task: post_body()
  // Drop objection after main body.
  virtual task post_body();
    uvm_phase phase;
    super.post_body();
    phase = starting_phase;
    if (phase != null) begin
      phase.drop_objection(this);
    end
  endtask: post_body

  virtual task body();
    #10_000ps;
    `uvm_info(get_name(), "Finished ingress_dummy_seq", UVM_LOW)
  endtask: body

endclass

class ingress_eth_simple_seq extends ingress_extended_base_seq;

  `uvm_object_utils(ingress_eth_simple_seq)
  `uvm_declare_p_sequencer(slu_sequencer)

  ingress_env        el_ambiente;
  eth_frame      el_frame;
  eth_sequencer  el_sequencer;

  function new (string name="ingress_eth_simple_seq");
    super.new (name);
    `slu_assert($cast(el_ambiente, slu_utils::get_comp_by_name("env")),
                ("Could not get mby env pointer"))
  endfunction :  new

  virtual task body();
    int count = 0;
    `slu_info(this.get_name(), ("Starting eth simple sequence..."))
    `slu_assert($cast(el_sequencer, el_ambiente.get_slu_sqcr().pick_sequencer("tx0")),
                ("Could not get a pointer to the sequencer"))
    el_frame = eth_frame::type_id::create("el_frame");
    el_frame.set_item_context(this, el_sequencer);
    repeat (20) begin
      `slu_assert(el_frame.randomize() with {
        kind           inside {BASIC_FRAME,
                               IPV4_FRAME,
                               IPV6_FRAME};
        payload.size() inside {[256:512]};
        dmac            == 'h000102030405 + count;
        smac            == 'h060708090a0b + count;
        tc              == count[3:0];
        (kind == BASIC_FRAME) ->
          foreach (payload[idx])
            payload[idx] == idx;
       }, ("Unable to randomize eth_pkt"))
    count++;
    `slu_info(this.get_name(), ("Started eth_frame %d", count))
    `uvm_send(el_frame)
    #200_000ps;
    `slu_info(this.get_name(), ("Sent eth_frame %d", count))
  end
  endtask

endclass

class ingress_alive_test extends ingress_base_test;

  `uvm_component_utils(ingress_alive_test)

  function new (string name="ingress_alive_test", uvm_component parent=null);
    super.new (name, parent);
  endfunction :  new

  function void build_phase(uvm_phase phase);
    super.build_phase(phase);
    // TODO: Create config obj here.
  endfunction : build_phase

  function void connect_phase(uvm_phase phase);
    super.connect_phase(phase);
    // TODO: Set data phase seq here.
    env.set_test_phase_type("env", "POWER_GOOD_PHASE", "ingress_dummy_seq");
    env.set_test_phase_type("env", "HARD_RESET_PHASE", "ingress_dummy_seq");
    env.set_test_phase_type("env", "WARM_RESET_PHASE", "ingress_dummy_seq");
    env.set_test_phase_type("env", "TRAINING_PHASE",   "ingress_dummy_seq");
    env.set_test_phase_type("env", "CONFIG_PHASE",     "ingress_dummy_seq");
    env.set_test_phase_type("env", "USER_DATA_PHASE",  "ingress_eth_simple_seq");
  endfunction : connect_phase

endclass : ingress_alive_test

`endif
