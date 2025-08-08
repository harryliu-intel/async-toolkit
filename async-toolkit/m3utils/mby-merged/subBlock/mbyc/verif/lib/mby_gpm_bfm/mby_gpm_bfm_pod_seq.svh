//-----------------------------------------------------------------------------
// Title         : Madison Bay GPM BFM Free Pod/Response Sequence Class
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : mby_gpm_bfm_pod_seq.svh
// Author        : jose.gerardo.soria.garcia  <jose.gerardo.soria.garcia@intel.com>
// Created       : 21.01.2019
//-----------------------------------------------------------------------------
// Description :
// This is the sequence item class for the Global Pointer Manager BFM.
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
`ifndef __MBY_GPM_BFM_POD_SEQ__
`define __MBY_GPM_BFM_POD_SEQ__

//-----------------------------------------------------------------------------
// CLASS: mby_gpm_bfm_pod_seq
//
// This is a parameterized class used by gpm_bfm.
//
// PARAMETERS:
//    T_rsp       sequence item type to be handled
//
//-----------------------------------------------------------------------------
class mby_gpm_bfm_pod_seq#(
      type REQ = shdv_base_pkg::shdv_base_sequence_item,
      type RSP = REQ
   ) extends shdv_base_pkg::shdv_base_sequence#(.REQ(REQ), .RSP(RSP));

   // -------------------------------------------------------------------------
   // Macro for factory registration
   // TODO:
   //   better to have manual copy/print/compare methods
   //   convert2string (small)
   // -------------------------------------------------------------------------
   //`uvm_object_param_utils_begin (mby_gpm_bfm_pod_seq#(REQ, RSP))
   //`uvm_object_utils_end
   `uvm_object_param_utils(mby_gpm_bfm_pod_seq#(REQ,RSP))
   
   // -------------------------------------------------------------------------
   // CONSTRUCTOR: new
   //
   // Constructor
   //
   // ARGUMENTS:
   //    string name - The sequence item name
   // -------------------------------------------------------------------------
   function new (string name = "mby_gpm_bfm_pod_seq");
      super.new(name);
   endfunction : new
   
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
   
   // -------------------------------------------------------------------------
   // TASK: body
   //
   // The body task is used to send the sequence item to the sequencer
   // -------------------------------------------------------------------------
   virtual task body();
      `uvm_send(req)
   endtask : body
   
endclass : mby_gpm_bfm_pod_seq

`endif
