//-----------------------------------------------------------------------------
// Title         : Madison Bay SMM BFM Read/Response Sequence Class
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : mby_smm_bfm_mrd_seq.svh
// Author        : jose.j.godinez.carrillo  <jjgodine@ichips.intel.com>
// Created       : 18.12.2018
//-----------------------------------------------------------------------------
// Description :
// This is the sequence item class for the Shared Memory Model BFM.
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
`ifndef __MBY_SMM_BFM_MRD_SEQ__
`define __MBY_SMM_BFM_MRD_SEQ__

//-----------------------------------------------------------------------------
// CLASS: mby_smm_bfm_mrd_seq
//
// This is a parameterized class used by smm_bfm to generate memory read responses
// for Egress..
//
// PARAMETERS:
//     T_req      - sequence item type to be handled
//
//-----------------------------------------------------------------------------
class mby_smm_bfm_mrd_seq
   #(
      type T_req = mby_smm_bfm_mrd_req_xaction
    )
   extends shdv_base_sequence;

   // VARIABLE: mem_rsp
   //    Struct contains all the data items of this sequence item.
   rand T_req mem_rsp;

   // -------------------------------------------------------------------------
   // Macro for factory registration
   // TODO:
   //   better to have manual copy/print/compare methods
   //   convert2string (small)
   // -------------------------------------------------------------------------
  `uvm_object_param_utils_begin (mby_smm_bfm_mrd_seq#(T_req))
    `uvm_field_object(mem_rsp, UVM_DEFAULT)
  `uvm_object_utils_end

   // -------------------------------------------------------------------------
   // CONSTRUCTOR: new
   //
   // Constructor
   //
   // ARGUMENTS:
   //     string name - The sequence item name
   // -------------------------------------------------------------------------
   function new (string name = "mby_smm_bfm_mrd_seq");
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
      // pretty print the sequence object
   endfunction : do_print

   // -------------------------------------------------------------------------
   // TASK: body
   //
   // print and sprint functions use the do_print function to print out the
   // class, here the driver/monitor active are printed out.
   // -------------------------------------------------------------------------
   virtual task body();
      `uvm_send(mem_rsp)
   endtask
endclass : mby_smm_bfm_mrd_seq

`endif
