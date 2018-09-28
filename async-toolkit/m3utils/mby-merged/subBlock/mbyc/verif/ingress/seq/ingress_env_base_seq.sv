//-----------------------------------------------------------------------------
// Title         : Ingress env base sequence
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : ingress_env_base_seq.sv
// Author        : jose.j.godinez.carrillo  <jjgodine@ichips.intel.com>
// Created       : 21.08.2018
// Last modified : 21.08.2018
//-----------------------------------------------------------------------------
// Description :
// This file contain all of the MBY base sequences.
// Class: ingress_env_base_seq
// Base sequence for all the sequences.
// This base sequence setup the MBY RAL reg file pointer in the sequence which
// will be used by all the config seq to access MBY registers
// TODO: re-enable wm & sm
//-----------------------------------------------------------------------------
// Copyright (c) 2018 by Intel Corporation This model is the confidential and
// proprietary property of Intel Corporation and the possession or use of this
// file requires a written license from Intel Corporation.
//------------------------------------------------------------------------------
// Modification history :
// 21.08.2018 : created
//-----------------------------------------------------------------------------

class ingress_env_base_seq extends mby_base_seq;

  `uvm_object_utils(ingress_env_base_seq)
  `uvm_declare_p_sequencer(slu_sequencer)

  sla_ral_env ral;

  function new(input string name = "ingress_env_base_seq");
    super.new(name);
    `slu_assert($cast(ral, sla_ral_env::get_ptr()),
                ("Unable to get handle to RAL."))
  endfunction

endclass
