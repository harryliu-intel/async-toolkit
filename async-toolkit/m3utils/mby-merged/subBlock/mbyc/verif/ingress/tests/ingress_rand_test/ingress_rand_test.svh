//-----------------------------------------------------------------------------
// Title         : Ingress alive test
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : ingress_rand_test.svh
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
`ifndef INGRESS_RAND_TEST__SVH
`define INGRESS_RAND_TEST__SVH

class ingress_dummy_seq extends ingress_extended_base_seq;

  `uvm_object_utils(ingress_dummy_seq)

  // Function: new
  // Constructor
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

endclass // ingress_dummy_seq

class ingress_eth_simple_seq extends ingress_extended_base_seq;

  `uvm_object_utils(ingress_eth_simple_seq)
  `uvm_declare_p_sequencer(slu_sequencer)

  ingress_env    el_ambiente;
  eth_frame      los_frames[4];
  eth_sequencer  los_sequencers[4];

  // Function: new
  // Constructor
  function new (string name="ingress_eth_simple_seq");
    super.new (name);
    `slu_assert($cast(el_ambiente, slu_utils::get_comp_by_name("env")),
                ("Could not get mby env pointer"))
  endfunction :  new

  // Task: body()
  // Main task.
  virtual task body();
    int count[4] = {0,0,0,0};
    `slu_info(this.get_name(), ("Starting eth simple sequence..."))
    foreach(los_sequencers[i]) begin
      `slu_assert($cast(los_sequencers[i], 
        el_ambiente.get_slu_sqcr().pick_sequencer($sformatf("eth_bfm_%0d_tx0", i))),
        ("Could not get a pointer to the sequencer%0d", i))
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
            `slu_assert(los_frames[auto_i].randomize() with {
              bubble          == 0;
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
              }, ("Unable to randomize eth_pkt"))
            count[auto_i]++;
            `slu_info(this.get_name(), ("Started eth_frame %0d in EPL%0d, port0", count[auto_i], auto_i))
            los_frames[auto_i].print();
            `uvm_send(los_frames[auto_i])
            //#200_000ps;
            `slu_info(this.get_name(), ("Sent eth_frame %0d in EPL%0d, port0", count[auto_i], auto_i))
          end
        end
      join_none
    end
    wait fork;
  endtask

endclass // ingress_eth_simple_seq

class ingress_rand_test extends ingress_base_test;

  `uvm_component_utils(ingress_rand_test)

  // Function: new
  // Constructor
  function new (string name="ingress_rand_test", uvm_component parent=null);
    super.new (name, parent);
  endfunction :  new

  function void build_phase(uvm_phase phase);
    super.build_phase(phase);
    // TODO: Create config obj here.
  endfunction : build_phase

  // Function: connect_phase
  // sequences are defined for each of the run sub-phases.
  // user data phase sequence is set to egress_eth_simple_seq.
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

endclass : ingress_rand_test

`endif
