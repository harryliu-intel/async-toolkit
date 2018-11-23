//-----------------------------------------------------------------------------
// Title         : Madison Bay Base Sequence Item Class
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : mby_base_sequence_item.svh
// Author        : jose.j.godinez.carrillo  <jjgodine@ichips.intel.com>
// Created       : 30.10.2018
//-----------------------------------------------------------------------------
// Description :
// This is the base sequence item class for Madison Bay.
//-----------------------------------------------------------------------------
// Copyright (c) 2018 by Intel Corporation This model is the confidential and
// proprietary property of Intel Corporation and the possession or use of this
// file requires a written license from Intel Corporation.
//
// The source code contained or described herein and all documents related to
// the source code ("Material") are owned by Intel Corporation or its suppliers
// or licensors. Title to the Material remains with Intel Corporation or its
// suppliers and licensors. The Material contains trade secrets and proprietary
// and confidential information of Intel or its suppliers and licensors. The
// Material is protected by worldwide copyright and trade secret laws and
// treaty provisions. No part of the Material may be used, copied, reproduced,
// modified, published, uploaded, posted, transmitted, distributed, or
// disclosed in any way without Intel's prior express written permission.
//
// No license under any patent, copyright, trade secret or other intellectual
// property right is granted to or conferred upon you by disclosure or delivery
// of the Materials, either expressly, by implication, inducement, estoppel or
// otherwise. Any license under such intellectual property rights must be
// express and approved by Intel in writing.
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
   extends uvm_sequence_item; // TODO: replace with shdv_base_transaction

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

   // TODO: add unique identifier field
   // start/end times (this is included in the shdv_base_transaction

   // CONSTRAINT: delay_constraint
   // sets initial delay parameters
   constraint delay_constraint {
      delay >= 0;
      delay <= 15;
   }

   // -------------------------------------------------------------------------
   // Macro for factory registration
   // TODO:
   //   better to have manual copy/print/compare methods
   //   convert2string (small)
   // -------------------------------------------------------------------------
  `uvm_object_param_utils_begin (mby_base_sequence_item#(T_data, T_data_rsp, T_debug))
    //`uvm_field_int(data_pkt, UVM_DEFAULT)
    //`uvm_field_int(resp_pkt, UVM_DEFAULT)
    //`uvm_field_int(debg_pkt, UVM_DEFAULT)
    //`uvm_field_int(delay,    UVM_DEFAULT)
    //`uvm_field_int(rsp_req,  UVM_DEFAULT)
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

   // -------------------------------------------------------------------------
   // FUNCTION: do_print
   //
   // print and sprint functions use the do_print function to print out the
   // class, here the driver/monitor active are printed out.
   //
   // ARGUMENTS:
   //    uvm_printer printer - APIs of the uvm_printer class are used to print
   //    the class information.
   // -------------------------------------------------------------------------
   virtual function void do_print(uvm_printer printer);
      super.do_print(printer);
      // pretty print the configuration object
   endfunction : do_print


endclass : mby_base_sequence_item

`endif
