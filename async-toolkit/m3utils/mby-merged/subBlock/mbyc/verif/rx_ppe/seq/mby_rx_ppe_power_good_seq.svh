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

//   Class:  mby_rx_ppe_power_good_seq
//
//   This is the main IP Power_Good Sequence. execute in Power_Good_Phase
//
//   Clears all Power_Good, Hard_Reset, and Warm_Reset.
//
//   Delay for short time and Set Power_Good.

`ifndef __MBY_RX_PPE_POWER_GOOD_SEQ_GUARD
`define __MBY_RX_PPE_POWER_GOOD_SEQ_GUARD

`ifndef __INSIDE_MBY_RX_PPE_SEQ_LIB
`error "Attempt to include file outside of mby_rx_ppe_seq_lib."
`endif


class mby_rx_ppe_power_good_seq extends shdv_base_reset_sequence;

   `uvm_object_utils(mby_rx_ppe_power_good_seq)

   //------------------------------------------------------------------------------
   //  Constructor: new
   //  New Power_Good_Seq Object.
   //  Gets handle to the rx_ppe ENV.
   //
   //  Arguments:
   //  string name  - rx_ppe TOP power good sequence object name.
   //------------------------------------------------------------------------------
   function new(input string name = "mby_rx_ppe_power_good_seq");
      super.new(name);
   endfunction: new

   //------------------------------------------------------------------------------
   //  Task: body
   //  Sequence body is used to control Power_Good (Initial Clear -> Delay -> Set),
   // as well as Hard_Reset/Warm_Reset (Initial Clear)
   //------------------------------------------------------------------------------
   task body();

      vif.power_good_reset           = 0;
      vif.hard_reset                 = 0;
      vif.warm_reset                 = 0;

      #10;
      `uvm_info(get_name(), $sformatf("Power_Good_Reset Set"), UVM_NONE);
      vif.power_good_reset           = 1;

   endtask: body

endclass: mby_rx_ppe_power_good_seq

`endif // __MBY_RX_PPE_POWER_GOOD_SEQ_GUARD


