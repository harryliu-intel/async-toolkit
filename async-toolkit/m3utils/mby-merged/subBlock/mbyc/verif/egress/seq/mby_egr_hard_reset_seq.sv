//-----------------------------------------------------------------------------
// Title         : Egress hard reset sequence
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : mby_egr_hard_reset_seq.sv
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
//-----------------------------------------------------------------------------
// Modification history :
// 22.08.2018 : created
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// Class: mby_egr_hard_reset_seq
//-----------------------------------------------------------------------------
class mby_egr_hard_reset_seq extends mby_egr_extended_base_seq;

  `uvm_object_utils(mby_egr_hard_reset_seq)

   //--------------------------------------------------------------------------
   // Task: body()
   //--------------------------------------------------------------------------
   task body();
      virtual mby_egr_env_if env_if;
      mby_egr_env egress_env_ptr;
      uvm_event egress_fusepull_comp_e;
      uvm_event_pool    egress_epool;

      uvm_report_warning (get_name(), "INTEG - EGRESS_hard_reset should be IMP ");

      assert($cast(egress_env_ptr, mby_egr_env::get_egr_env()))
      else begin
         `uvm_error(get_name(), $sformatf("Unable to get handle to mby_egr_env."));
      end

      //Pointer to env IF
      env_if = egress_env_ptr.egress_if;
      //pointer to event pool and fuse event
      egress_epool = egress_epool.get_global_pool();
      egress_fusepull_comp_e = egress_epool.get("EGRESS_DETECT_FUSEPULL_COMP_SB_MSG");

      //Reset and power up flow
      env_if.power_good_reset = 0;
      env_if.reset = 0;

      //power good
      #1;
      env_if.power_good_reset = 1;

      egress_fusepull_comp_e.wait_trigger();

      //Primary interface
      repeat (10) begin
         @(posedge env_if.clock);
      end
      env_if.reset = 1;

   endtask : body

endclass : mby_egr_hard_reset_seq
