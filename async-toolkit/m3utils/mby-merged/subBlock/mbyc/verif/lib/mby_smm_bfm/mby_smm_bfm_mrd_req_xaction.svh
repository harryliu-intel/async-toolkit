//-----------------------------------------------------------------------------
// Title         : Madison Bay SMM BFM Transaction item
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : mby_smm_bfm_mrd_req_xaction.svh
// Author        : Roman Bernal <r.bernal@intel.com>
// Created       : 01.11.2018
//-----------------------------------------------------------------------------
// Description :
// This is the transaction item used by the smm bfm
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
`ifndef __MBY_SMM_BFM_MRD_REQ_XACTION__
`define __MBY_SMM_BFM_MRD_REQ_XACTION__
//-----------------------------------------------------------------------------
// CLASS: mby_smm_bfm_mrd_req_xaction
//
// This is a parameterized class used by mby_base_agent.
//
// PARAMETERS:
//     T_data     - data type (expecting to be a struct)
//
//-----------------------------------------------------------------------------
class mby_smm_bfm_mrd_req_xaction extends shdv_base_sequence_item_param
#(
   .T_data (mby_smm_bfm_row_rd_req_t)
);

   // -------------------------------------------------------------------------
   // Macro for factory registration
   // -------------------------------------------------------------------------
  `uvm_object_utils(mby_smm_bfm_mrd_req_xaction)

   // -------------------------------------------------------------------------
   // CONSTRUCTOR: new
   //
   // Constructor
   //
   // ARGUMENTS:
   //     string name - The sequence item name
   //
   // -------------------------------------------------------------------------
   function new (string name = "mby_smm_bfm_mrd_req_xaction");
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
      msg_str = { msg_str, $sformatf("row_rd_req_xaction::seg_ptr=%05h -:- rd_data=%0128h\n",
         this.data.mim_seg_ptr, this.data.mim_rd_data) };
      msg_str = { msg_str, lns_str };
      return msg_str;
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
   endfunction : do_print

endclass : mby_smm_bfm_mrd_req_xaction
`endif
