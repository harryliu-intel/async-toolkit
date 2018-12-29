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

//   Class:  mby_rx_ppe_hard_reset_seq
//
//   This is the main IP Hard Reset Sequence. execute in Hard_Reset_Phase
//
//   Sets both Hard and Warm Resets.   Delays for some time and drops Hard Reset.

`ifndef __MBY_RX_PPE_HARD_RESET_SEQ_GUARD
`define __MBY_RX_PPE_HARD_RESET_SEQ_GUARD

`ifndef __INSIDE_MBY_RX_PPE_SEQ_LIB
`error "Attempt to include file outside of mby_rx_ppe_env_base_seq."
`endif


class mby_rx_ppe_hard_reset_seq extends mby_rx_ppe_env_base_seq;

   `uvm_object_utils(mby_rx_ppe_hard_reset_seq)

   //------------------------------------------------------------------------------
   //  Constructor: new
   //  New rx_ppe Hard Reset Sequence Object.
   //  Gets handle to the rx_ppe ENV.
   //
   //  Arguments:
   //  string name  - rx_ppe Hard Reset sequence object name.
   //------------------------------------------------------------------------------
   function new(input string name = "mby_rx_ppe_hard_reset_seq");
      super.new(name);
   endfunction: new


   //------------------------------------------------------------------------------
   //  Task: body
   //  Sequence body is used to control Hard_Reset (Set -> Delay -> Clear),
   //  as well as Warm_Reset (Set)
   //------------------------------------------------------------------------------
   task body();
      
      `uvm_info(this.get_name(), ("Phase::reset_phase:mby_rx_ppe_hard_reset_seq::Starting"), UVM_LOW)    
      `uvm_info(get_name(), $sformatf("Hard_Reset  & warm_reset Set"), UVM_NONE);
      vif.hard_reset                 = 1;
      vif.warm_reset                 = 1;

      repeat (200) @(posedge vif.fab_clk);

      `uvm_info(get_name(), $sformatf("Hard_Reset & warm_reset Cleared"), UVM_NONE);
      vif.hard_reset                 = 0;
      
      repeat (20) @(posedge vif.fab_clk);
      vif.warm_reset                 = 0;

   endtask: body

endclass: mby_rx_ppe_hard_reset_seq

`endif // __MBY_RX_PPE_HARD_RESET_SEQ_GUARD
