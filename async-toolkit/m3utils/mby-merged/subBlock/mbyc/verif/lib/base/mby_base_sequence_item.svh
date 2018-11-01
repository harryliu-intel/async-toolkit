//-----------------------------------------------------------------------------
// Title         : Madison Bay Base Sequence Item Class
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : mby_base_sequence_item.svh
// Author        : jose.j.godinez.carrillo  <jjgodine@ichips.intel.com>
// Created       : 30.10.2018
// Last modified : 30.10.2018
//-----------------------------------------------------------------------------
// Description :
// This is the base sequence item class for Madison Bay.
//-----------------------------------------------------------------------------
// Copyright (c) 2018 by Intel Corporation This model is the confidential and
// proprietary property of Intel Corporation and the possession or use of this
// file requires a written license from Intel Corporation.
//------------------------------------------------------------------------------
// Modification history :
// 30.10.2018 : created
//-----------------------------------------------------------------------------
`ifndef __MBY_BASE_PKG__
`error "Attempt to include file outside of mby_igr_env_pkg."
`endif
`ifndef __MBY_BASE_SEQUENCE_ITEM__
`define __MBY_BASE_SEQUENCE_ITEM__
//-----------------------------------------------------------------------------
// CLASS: mby_base_sequence_item
//
// This is a parameterized class used by mby_base_agent.
//
// PARAMETERS:
//     T_data     - data type (expecting to be a struct)
//     T_data_rsp - data to respond (defaults to the same as T_data)
//     T_debug    - debug data type (optional, used for debug)
//
//-----------------------------------------------------------------------------
class mby_base_sequence_item
   #(
      type T_data     = logic[31:0],
      type T_data_rsp = T_data,
      type T_debug    = logic[31:0]
    )
   extends uvm_sequence_item;
   
   // VARIABLE: data_pkt
   // Struct contains all the data items of this sequence item.
   rand T_data data_pkt;
   
   // VARIABLE: resp_pkt
   // Struct that contains all the data items of the response.
   T_data_rsp resp_pkt;
   
   // VARIABLE: debg_pkt
   // Struct that contains debug data of this transaction.
   rand T_debug debg_pkt;
   
   // VARIABLE: rsp_req
   // Response is required for this request.
   bit rsp_req = 0;
   
   // VARIABLE: delay
   // Transaction delay used by the driver
   rand int delay;
   
   // CONSTRAINT: delay_constraint
   // sets initial delay parameters
   constraint delay_constraint {
      delay >= 0;
      delay <= 15;
   }
   
   // -------------------------------------------------------------------------
   // Macro for factory registration
   // -------------------------------------------------------------------------
  `uvm_object_param_utils_begin (mby_base_sequence_item#(T_data, T_data_rsp, T_debug))
    `uvm_field_int(data_pkt, UVM_DEFAULT)
    `uvm_field_int(resp_pkt, UVM_DEFAULT)
    `uvm_field_int(debg_pkt, UVM_DEFAULT)
    `uvm_field_int(delay,    UVM_DEFAULT)
    `uvm_field_int(rsp_req,  UVM_DEFAULT)
  `uvm_object_utils_end

   // -------------------------------------------------------------------------
   // CONSTRUCTOR: new
   //
   // Constructor
   //
   // ARGUMENTS:
   //     string name - The sequence item name
   // -------------------------------------------------------------------------
   function new (string name = "mby_base_sequence_item");
      super.new(name);
   endfunction
   
endclass : mby_base_sequence_item

`endif
