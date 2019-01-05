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
//   Author        : Kaleem Sheriff
//   Project       : Madison Bay
//------------------------------------------------------------------------------

//   Class:    mby_rx_ppe_eot_seq
//
//   This is the rx_ppe Config sequence file.

`ifndef __MBY_RX_PPE_EOT_SEQ_GUARD
`define __MBY_RX_PPE_EOT_SEQ_GUARD

`ifndef __INSIDE_MBY_RX_PPE_SEQ_LIB
`error "Attempt to include file outside of mby_rx_ppe_seq_lib."
`endif


class mby_rx_ppe_eot_seq extends uvm_sequence;

   `uvm_object_utils(mby_rx_ppe_eot_seq)

   // Variable: access_type
   // RAL ENV Access Type
   string access_type = "BACKDOOR";
   

   //------------------------------------------------------------------------------
   //  Task: new
   //  Confiure rx ppe
   //------------------------------------------------------------------------------
    function new(string name = "mby_rx_ppe_eot_seq");
       super.new(name);
    endfunction

   //------------------------------------------------------------------------------
   //  Task: pre_body
   //  Confiure rx ppe
   //------------------------------------------------------------------------------
    virtual task pre_body();
      
    endtask

   //------------------------------------------------------------------------------
   //  Task: body
   //  Confiure rx ppe
   //------------------------------------------------------------------------------
    virtual task body();
      `uvm_info(get_name(), "Eot Sequence", UVM_MEDIUM);
	 
    endtask : body

endclass : mby_rx_ppe_eot_seq

`endif // __MBY_RX_PPE_EOT_SEQ_GUARD
