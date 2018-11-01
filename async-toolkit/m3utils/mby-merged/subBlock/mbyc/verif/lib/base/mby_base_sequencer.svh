//-----------------------------------------------------------------------------
// Title         : Madison Bay Base Sequencer Class
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : mby_base_sequencer.svh
// Author        : jose.j.godinez.carrillo  <jjgodine@ichips.intel.com>
// Created       : 29.10.2018
// Last modified : 29.10.2018
//-----------------------------------------------------------------------------
// Description :
// This is the base sequencer class for Madison Bay
//-----------------------------------------------------------------------------
// Copyright (c) 2018 by Intel Corporation This model is the confidential and
// proprietary property of Intel Corporation and the possession or use of this
// file requires a written license from Intel Corporation.
//------------------------------------------------------------------------------
// Modification history :
// 29.10.2018 : created
//-----------------------------------------------------------------------------
`ifndef __MBY_BASE_PKG__
`error "Attempt to include file outside of mby_igr_env_pkg."
`endif
`ifndef __MBY_BASE_SEQUENCER__
`define __MBY_BASE_SEQUENCER__
//-----------------------------------------------------------------------------
// CLASS: mby_base_sequencer
//
// This is a parameterized class used by mby_base_agent.
//
// PARAMETERS:
//     T_req - request sequence item type to be used (defaults to uvm_sequence_item)
//     T_rsp - response sequence item type to be used (defaults to T_req)
//
//-----------------------------------------------------------------------------
class mby_base_sequencer
   #(
      type T_req = uvm_sequence_item,
      type T_rsp = T_req
   )
   extends uvm_sequencer
   #(
      T_req,
      T_rsp
   );
   // -------------------------------------------------------------------------
   // CONSTRUCTOR: new
   //
   // Constructor
   //
   // ARGUMENTS:
   //    string name          - An instance name of the sequencer.
   //    uvm_component parent - The sequencer's parent component.
   // -------------------------------------------------------------------------
   function new(string name, uvm_component parent);
      super.new(name, parent);
   endfunction : new
endclass : mby_base_sequencer
`endif
