// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

//-----------------------------------------------------------------------------
// Title         : Madison Bay PBR CPTR response sequence
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : mby_pbr_bfm_cptr_req.svh
// Author        : ricardo.a.alfaro.gomez  <raalfaro@ichips.intel.com>
// 2ry contact   : jose.j.godinez.carrillo  <jjgodine@ichips.intel.com>
// Created       : 01.14.2018
//-----------------------------------------------------------------------------
// Description :
// This is the sequence item class for the PBR BFM.
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
`ifndef __MBY_PBR_BFM_PKG__
`error "Attempt to include file outside of mby_pbr_bfm_pkg."
`endif
`ifndef __MBY_PBR_BFM_CPTR_RSP_SEQ__
`define __MBY_PBR_BFM_CPTR_RSP_SEQ__

//-----------------------------------------------------------------------------
// CLASS: mby_pbr_bfm_cptr_rsp_seq
//
// This is a parameterized class used by smm_bfm.
//
// PARAMETERS:
//     T_req      - sequence item type to be handled
//
//-----------------------------------------------------------------------------
class mby_pbr_bfm_cptr_rsp_seq
   #(
      type T_req = mby_pbr_bfm_cptr_xaction
    )
   extends shdv_base_sequence;

   // VARIABLE: data_pkt
   // Struct contains all the data items of this sequence item.
   rand T_req cptr_rsp;

   // -------------------------------------------------------------------------
   // Macro for factory registration
   // TODO:
   //   better to have manual copy/print/compare methods
   //   convert2string (small)
   // -------------------------------------------------------------------------
  `uvm_object_param_utils_begin (mby_pbr_bfm_cptr_rsp_seq#(T_req))
    `uvm_field_object(cptr_rsp, UVM_DEFAULT)
  `uvm_object_utils_end

   // -------------------------------------------------------------------------
   // CONSTRUCTOR: new
   //
   // Constructor
   //
   // ARGUMENTS:
   //     string name - The sequence item name
   // -------------------------------------------------------------------------
   function new (string name = "mby_pbr_bfm_cptr_rsp_seq");
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
      //`uvm_info(get_type_name(), "DBG_ALF: cptr_rsp_seq started", UVM_LOW)
      `uvm_send(cptr_rsp)
      //`uvm_info(get_type_name(), "DBG_ALF: cptr_rsp_seq sent", UVM_LOW)
   endtask
endclass : mby_pbr_bfm_cptr_rsp_seq

`endif
