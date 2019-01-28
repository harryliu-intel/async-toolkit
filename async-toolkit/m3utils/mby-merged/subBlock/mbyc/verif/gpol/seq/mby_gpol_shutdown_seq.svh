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
// Author   : Akshay Kotian
// Project  : Madison bay
//------------------------------------------------------------------------------

`ifndef __MBY_GPOL_SHUTDOWN_SEQ_GUARD
`define __MBY_GPOL_SHUTDOWN_SEQ_GUARD

`ifndef __INSIDE_MBY_GPOL_SEQ_LIB
`error "Attempt to include file outside of mby_gpol_seq_lib."
`endif
//------------------------------------------------------------------------------
// Class: shdv_base_shutdown_sequence
//
// Sequence associated to the UVM shutdown run-time phase.  This sequence waits
// until all scoreboards and BFMs in the environment have completed their
// analysis and flushed all transactions from their queues.
//------------------------------------------------------------------------------
class mby_gpol_shutdown_seq extends shdv_base_shutdown_sequence;

   `uvm_object_utils_begin(mby_gpol_shutdown_seq)
   `uvm_object_utils_end

   //---------------------------------------------------------------------------
   // Function: new
   //
   // Constructor.
   //
   // Arguments:
   //    string name - sequence name
   //---------------------------------------------------------------------------
   function new(string name = "mby_gpol_shutdown_sequence");
      super.new(name);
   endfunction

   //---------------------------------------------------------------------------
   // Task: body
   //
   // Main task of shutdown sequence.  
   //---------------------------------------------------------------------------
   virtual task body();
      `uvm_info(get_name(), "shutdown sequence started", UVM_LOW)
   endtask

endclass 
`endif
