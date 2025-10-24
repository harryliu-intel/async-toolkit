// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

//-----------------------------------------------------------------------------
// Title         : Egress env base sequence
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : mby_egr_env_base_seq.sv
// Author        : jose.j.godinez.carrillo  <jjgodine@ichips.intel.com>
// Created       : 21.08.2018
// Last modified : 21.08.2018
//-----------------------------------------------------------------------------
// Description :
// This file contain all of the MBY base sequences.
// Class: mby_egr_env_base_seq
// Base sequence for all the sequences.
// This base sequence setup the MBY RAL reg file pointer in the sequence which
// will be used by all the config seq to access MBY registers
// TODO: re-enable wm & sm
//-----------------------------------------------------------------------------
// Copyright (c) 2018 by Intel Corporation This model is the confidential and
// proprietary property of Intel Corporation and the possession or use of this
// file requires a written license from Intel Corporation.
//-----------------------------------------------------------------------------
// Modification history :
// 21.08.2018 : created
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// Class: mby_egr_env_base_seq
//-----------------------------------------------------------------------------
//class mby_egr_env_base_seq extends mby_base_seq;
class mby_egr_env_base_seq extends shdv_base_sequence;
  `uvm_object_utils(mby_egr_env_base_seq)
//PJP  `uvm_declare_p_sequencer(slu_sequencer)
  mby_egr_env env;
   
//PJP  sla_ral_env ral;

   //-----------------------------------------------------------------------------
   // Function: new()
   //-----------------------------------------------------------------------------
   function new(input string name = "mby_egr_env_base_seq");
      super.new(name);
//PJP    `slu_assert($cast(ral, sla_ral_env::get_ptr()),
//PJP                ("Unable to get handle to RAL."))
      env = mby_egr_env::get_egr_env();
   endfunction : new



   
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
   

endclass // mby_egr_env_base_seq
