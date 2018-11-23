//-----------------------------------------------------------------------------
// Title         : Madison Bay SMM BFM Transaction item
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : mby_smm_bfm_xaction.svh
// Author        : jose.j.godinez.carrillo  <jjgodine@ichips.intel.com>
// Created       : 01.11.2018
//-----------------------------------------------------------------------------
// Description :
// This is the transaction item used by the gcm bfm
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
//
//------------------------------------------------------------------------------
`ifndef __MBY_SMM_BFM_PKG__
`error "Attempt to include file outside of mby_smm_bfm_pkg."
`endif
`ifndef __MBY_SMM_BFM_XACTION__
`define __MBY_SMM_BFM_XACTION__
//-----------------------------------------------------------------------------
// CLASS: mby_smm_bfm_xaction
//
// This is a parameterized class used by mby_base_agent.
//
// PARAMETERS:
//     T_data     - data type (expecting to be a struct)
//     T_debug    - set to logic for now
//
//-----------------------------------------------------------------------------
class mby_smm_bfm_xaction extends mby_base_sequence_item
#(
   .T_data (),
   .T_debug()
);

   // -------------------------------------------------------------------------
   // Macro for factory registration
   // -------------------------------------------------------------------------
  `uvm_object_utils(mby_smm_bfm_xaction#(T_data, T_data_rsp, T_debug))

   // -------------------------------------------------------------------------
   // CONSTRUCTOR: new
   //
   // Constructor
   //
   // ARGUMENTS:
   //     string name - The sequence item name
   //
   // -------------------------------------------------------------------------
   function new (string name = "mby_smm_bfm_xaction");
      super.new(name);
   endfunction

   // -------------------------------------------------------------------------
   // FUNCTION: convert2string
   //
   // Provides a simple way to print out the transaction's basic information
   //
   // -------------------------------------------------------------------------
   virtual function string convert2string();
   endfunction : convert2string

   // -------------------------------------------------------------------------
   // FUNCTION: do_print
   //
   // Print implementation: print and sprint functions use the do_print
   // function to print out the class, here's where the entire transaction
   // is written to the std output.
   //
   // ARGUMENTS:
   //    uvm_printer printer - APIs of the uvm_printer class are used to print
   //    the class information.
   //
   // -------------------------------------------------------------------------
   virtual function void do_print(uvm_printer printer);
      super.do_print(printer);
      // pretty print
   endfunction : do_print

endclass : mby_smm_bfm_xaction
`endif
