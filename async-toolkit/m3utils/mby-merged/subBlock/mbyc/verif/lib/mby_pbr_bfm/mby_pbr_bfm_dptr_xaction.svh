//-----------------------------------------------------------------------------
// Title         : Madison Bay PBR BFM Transaction item
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : mby_pbr_bfm_dptr_xaction.svh
// Author        : ricardo.a.alfaro.gomez  <raalfaro@ichips.intel.com>
// 2ry contact   : jose.j.godinez.carrillo  <jjgodine@ichips.intel.com>
// Created       : 12.19.2018
//-----------------------------------------------------------------------------
// Description :
// This is the transaction item used by the pbr's dptr bfm
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
`ifndef __MBY_PBR_BFM_PKG__
`error "Attempt to include file outside of mby_pbr_bfm_pkg."
`endif
`ifndef __MBY_PBR_BFM_DPTR_XACTION__
`define __MBY_PBR_BFM_DPTR_XACTION__
//-----------------------------------------------------------------------------
// CLASS: mby_pbr_bfm_dptr_xaction
//
// This is a parameterized class used by mby_base_agent.
//
// PARAMETERS:
//     T_data     - mby_pbr_bfm_dptr_data_t
//     T_debug    - mby_pbr_bfm_dptr_debg_t
//
//-----------------------------------------------------------------------------
class mby_pbr_bfm_dptr_xaction extends shdv_base_sequence_item_param
#(
   .T_data (mby_pbr_bfm_dptr_data_t),
   .T_debug(mby_pbr_bfm_dptr_debg_t)
);

   // -------------------------------------------------------------------------
   // Macro for factory registration
   // -------------------------------------------------------------------------
  `uvm_object_utils(mby_pbr_bfm_dptr_xaction)

   // -------------------------------------------------------------------------
   // CONSTRUCTOR: new
   //
   // Constructor
   //
   // ARGUMENTS:
   //     string name - The sequence item name
   //
   // -------------------------------------------------------------------------
   function new (string name = "mby_pbr_bfm_dptr_xaction");
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
      string lns_str = { {8{"--------"}}, "\n" };
      //msg_str = super.convert2string();
      msg_str = { msg_str, $sformatf("dptr_xaction::\t") };
      msg_str = { msg_str, $sformatf("pod_put_req      = %0x\t", this.data.pod_put_req) };
      msg_str = { msg_str, $sformatf("pod_put_type         = 0x%0x\t", this.data.pod_put_type) };
      msg_str = { msg_str, $sformatf("pod_put_ack   = %0x\t", this.data.pod_put_ack) };
      msg_str = { msg_str, $sformatf("schedule_stall   = %0x\t", this.data.schedule_stall) };
      msg_str = { msg_str, $sformatf("data_dirty_ptr = 0x%0x\t", this.data.data_dirty_ptr) };
      //msg_str = { msg_str, lns_str };
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
      // pretty print
   endfunction : do_print

endclass : mby_pbr_bfm_dptr_xaction
`endif
