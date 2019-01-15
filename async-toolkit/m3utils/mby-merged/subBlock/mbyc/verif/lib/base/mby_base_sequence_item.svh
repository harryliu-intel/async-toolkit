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
`error "Attempt to include file outside of mby_base_pkg."
`endif
`ifndef __MBY_BASE_SEQUENCE_ITEM__
`define __MBY_BASE_SEQUENCE_ITEM__
//-----------------------------------------------------------------------------
// CLASS: mby_base_sequence_item
//
// This is a basic sequence item class used by mby_base_agent.
//
//-----------------------------------------------------------------------------
class mby_base_sequence_item extends uvm_sequence_item; // TODO: replace with shdv_base_transaction

   // VARIABLE: global_transaction_count
   // This is a static variable that provides a unique number for each transaction
   // created.
   static int unsigned global_transaction_count;

   // VARIABLE: unique_transaction_id
   // This is initialized to this objects unique ID when the object is constructed.
   // It is used in conjunctions with the ID field of the UVM reporting to group all
   // messages related to this transaction regardless of where the report message was
   // generated.
   int unsigned unique_transaction_id;

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
  `uvm_object_utils(mby_base_sequence_item)

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
      unique_transaction_id = global_transaction_count++;
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
      // pretty print the sequence object
   endfunction : do_print

   //------------------------------------------------------------------------------
   // FUNCTION: convert2string()
   //
   // Has <unique_transaction_id>, but will need to be derived. It returns a string
   // that can be used to print the transaction.
   //------------------------------------------------------------------------------
   virtual function string convert2string ();
      return $psprintf("unique_transaction_id=0x%0h", unique_transaction_id);
   endfunction : convert2string

endclass : mby_base_sequence_item

`endif
