//-----------------------------------------------------------------------------
// Title         : Madison Bay Tag BFM Transaction item
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : mby_tag_bfm_uc_xaction.svh
// Author        : jose.j.godinez.carrillo  <jjgodine@ichips.intel.com>
// Created       : 01.11.2018
//-----------------------------------------------------------------------------
// Description :
// This is the transaction item used by the tag bfm
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
`ifndef __MBY_TAG_BFM_PKG__
`error "Attempt to include file outside of mby_tag_bfm_pkg."
`endif
`ifndef __MBY_TAG_BFM_UC_XACTION__
`define __MBY_TAG_BFM_UC_XACTION__
//-----------------------------------------------------------------------------
// CLASS: mby_tag_bfm_uc_xaction
//
// This is a parameterized class used by the uni-cast tag_agent.
//
// PARAMETERS:
//     T_data     - mby_tag_bfm_uc_data_t
//     T_debug    - mby_tag_bfm_uc_debg_t
//
//-----------------------------------------------------------------------------
class mby_tag_bfm_uc_xaction extends shdv_base_sequence_item_param
#(
   .T_data (mby_tag_bfm_uc_data_t),
   .T_debug(mby_tag_bfm_uc_debg_t)
);

   // -------------------------------------------------------------------------
   // Macro for factory registration
   // -------------------------------------------------------------------------
   `uvm_object_utils(mby_tag_bfm_uc_xaction)

   // -------------------------------------------------------------------------
   // CONSTRUCTOR: new
   //
   // Constructor
   //
   // ARGUMENTS:
   //     string name - The sequence item name
   //
   // -------------------------------------------------------------------------
   function new (string name = "mby_tag_bfm_uc_xaction");
      super.new(name);
   endfunction

   // -------------------------------------------------------------------------
   // FUNCTION: convert2string
   //
   // Provides a simple way to print out the transaction's basic information
   //
   // -------------------------------------------------------------------------
   virtual function string convert2string();
      string msg_str = "";
      string lns_str = { {8{" -------- "}}, "\n" };
      msg_str = super.convert2string();
      msg_str = { msg_str, $sformatf("tag_uc_xaction::seg_ptr = %020h\n",
         this.data.ptr) };
      msg_str = { msg_str, lns_str };
      return msg_str;
   endfunction : convert2string

   // -------------------------------------------------------------------------
   // FUNCTION: do_copy
   //
   // replaces the default copy, because we are not using the uvm field macros
   // for performance reasons
   //
   // ARGUMENTS:
   //    uvm_object p - object to be copied, if null it will error
   //
   // -------------------------------------------------------------------------

   virtual function void do_copy(uvm_object p);

      mby_tag_bfm_uc_xaction temp;

      if(p == null) begin
         `uvm_error("TAG_BFM", "Error making a copy of tag_bfm_uc_xaction");
      end
      else begin
         $cast(temp, p);
         this.data = temp.data;
         this.debug = temp.debug;
         this.rsp = temp.rsp;
         this.delay = temp.delay;
      end

   endfunction: do_copy



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

endclass : mby_tag_bfm_uc_xaction
`endif
