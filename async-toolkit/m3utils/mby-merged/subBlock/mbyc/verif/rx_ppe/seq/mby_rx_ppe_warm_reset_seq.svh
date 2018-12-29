// vim: noai : ts=3 : sw=3 : expandtab : ft=systemverilog

//------------------------------------------------------------------------------
//
// INTEL CONFIDENTIAL
//
// Copyright 2018 Intel Corporation All Rights Reserved.
//
// The source code contained or described herein and all documents related to
// the source code ("Material") are owned by Intel Corporation or its suppliers
// or licensors. Title to the Material remains with Intel Corporation or its
// suppliers and licensors. The Material contains trade secrets and proprietary
// and confidential information of Intel or its suppliers and licensors.  The
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
//   Author        : Akshay Kotian
//   Project       : Madison Bay
//------------------------------------------------------------------------------

//   Class:  mby_rx_ppe_warm_reset_seq
//
//   This is the main IP Warm Reset Sequence. execute in Warm_Reset_Phase
//
//   Delays for some time and drops warm_reset.

`ifndef __MBY_RX_PPE_WARM_RESET_SEQ_GUARD
`define __MBY_RX_PPE_WARM_RESET_SEQ_GUARD

`ifndef __INSIDE_MBY_RX_PPE_SEQ_LIB
`error "Attempt to include file outside of mby_rx_ppe_seq_lib."
`endif

class mby_rx_ppe_warm_reset_seq extends mby_rx_ppe_env_base_seq;

   `uvm_object_utils(mby_rx_ppe_warm_reset_seq)

   //------------------------------------------------------------------------------
   //  Constructor: new
   //  New rx_ppe Warm Reset Sequence Object.
   //  Gets handle to the rx_ppe ENV.
   //
   //  Arguments:
   //  string name  - rx_ppe WarmReset sequence object name.
   //------------------------------------------------------------------------------
   function new(input string name = "mby_rx_ppe_warm_reset_seq");
      super.new(name);
   endfunction: new

   //------------------------------------------------------------------------------
   //  Task: body
   //  Sequence body is used to control Warm_Reset (Delay -> Clear).
   //------------------------------------------------------------------------------
   task body();

      repeat (100) @(posedge vif.fab_clk);

      `uvm_info(get_name(), $sformatf("Warm_Reset Cleared"), UVM_NONE);
      vif.warm_reset                 = 0;

   endtask: body

endclass: mby_rx_ppe_warm_reset_seq

`endif // __MBY_RX_PPE_WARM_RESET_SEQ_GUARD
