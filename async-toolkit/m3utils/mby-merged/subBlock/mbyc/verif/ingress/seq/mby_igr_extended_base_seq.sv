//-----------------------------------------------------------------------------
// Title         : Ingress extended base sequence
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : mby_igr_extended_base_seq.sv
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
// Class: mby_igr_extended_base_seq
//-----------------------------------------------------------------------------
class mby_igr_extended_base_seq extends mby_igr_env_base_seq;

   `uvm_object_utils(mby_igr_extended_base_seq)

   //---------------------------------------------------------------------------
   // Function: new
   //---------------------------------------------------------------------------
   function new(input string name = "mby_igr_extended_base_seq",
               uvm_sequencer_base sequencer=null, uvm_sequence parent_seq=null);
      super.new(name /*, sequencer, parent_seq*/);
   endfunction

endclass : mby_igr_extended_base_seq
