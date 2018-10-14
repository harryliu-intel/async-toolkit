//-----------------------------------------------------------------------------
// Title         : Ingress hard reset sequence
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : ingress_hard_reset_seq.sv
// Author        : jose.j.godinez.carrillo  <jjgodine@ichips.intel.com>
// Created       : 22.08.2018
// Last modified : 22.08.2018
//-----------------------------------------------------------------------------
// Description :
//
//-----------------------------------------------------------------------------
// Copyright (c) 2018 by Intel Corporation This model is the confidential and
// proprietary property of Intel Corporation and the possession or use of this
// file requires a written license from Intel Corporation.
//------------------------------------------------------------------------------
// Modification history :
// 22.08.2018 : created
//-----------------------------------------------------------------------------

class ingress_hard_reset_seq extends ingress_extended_base_seq;

  `uvm_object_utils(ingress_hard_reset_seq)
  `uvm_declare_p_sequencer(slu_sequencer)

  // Function: body
  //  Asserts and de-asserts reset signals
  task body();
    virtual ingress_env_if env_if;
    ingress_env ingress_env_ptr;
    uvm_event ingress_fusepull_comp_e;
    uvm_event_pool    ingress_epool;

    uvm_report_warning (get_name(), "INTEG - INGRESS_hard_reset should be IMP ");

    `slu_assert($cast(ingress_env_ptr, ingress_env::get_ingress_env()), ("Unable to get handle to ingress_env."))

    //Pointer to env IF
    env_if = ingress_env_ptr.ingress_if;
    //pointer to event pool and fuse event
    ingress_epool = ingress_epool.get_global_pool();
    ingress_fusepull_comp_e = ingress_epool.get("INGRESS_DETECT_FUSEPULL_COMP_SB_MSG");

    //Reset and power up flow
    env_if.power_good_reset = 0;
    env_if.reset = 0;

    //power good
    #1;
    env_if.power_good_reset = 1;

    ingress_fusepull_comp_e.wait_trigger();

    //Primary interface
    repeat (10) begin
      @(posedge env_if.clock);
    end
    env_if.reset = 1;

  endtask // body

endclass // ingress_hard_reset_seq
