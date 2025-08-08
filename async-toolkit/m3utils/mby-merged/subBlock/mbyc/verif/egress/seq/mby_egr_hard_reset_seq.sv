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
import shdv_reset_pkg::*;
class mby_egr_hard_reset_seq extends mby_egr_extended_base_seq;

   shdv_reset_event        reset_event;
   shdv_reset_manager      reset_manager;
   shdv_reset_data         reset_data;

   `uvm_object_utils(mby_egr_hard_reset_seq)
   rand int delay, duration;
   
   constraint reset_select{
      soft duration  inside {[80:100]};
      soft delay     inside {[0:5]};
   }
   //--------------------------------------------------------------------------
   // Task: body()
   //--------------------------------------------------------------------------
   task body();


      randomize();
      reset_data              = shdv_reset_data::type_id::create("reset_data");
      reset_data.rst_domain   = "egr";

      reset_data.rst_type  = "power_good";
      reset_event          = reset_manager.get_global("egr_power_good");

      wait_n(delay);
      reset_event.set(reset_data);

      wait_n(duration);
      reset_event.clear();


   endtask : body

endclass : mby_egr_hard_reset_seq
