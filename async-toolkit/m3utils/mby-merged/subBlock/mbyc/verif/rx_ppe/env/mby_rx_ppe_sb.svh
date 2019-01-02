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
//   Author        : Lewis Sternberg
//   Project       : Madison Bay
//
// IT IS ASSUMED that the expected values will be written to the scoreboard before the actual values.
//
//TODO: replace with shdv_scoreboard when available
//------------------------------------------------------------------------------

// for a reference to the use of this macro, see uvm/1.2/docs/html/files/macros/uvm_tlm_defines-svh.html#`uvm_analysis_imp_decl
// To appear in shdv_package::definitions
`uvm_analysis_imp_decl(_expected)
`uvm_analysis_imp_decl(_actual)

// very short-term, this is the RxPPE scoreboard which will be instantiated after each RxPPE stage
// long-term, this will hopefully become the shdv_sb

// Parameterized class.  The parameter, T, is the type of the packet to be passed to the scoreboard

//FIXME: inherit T from a proper class
class mby_rx_ppe_sb # (type T=mby_rx_ppe_sb_obj) extends uvm_scoreboard;
   `uvm_component_param_utils(mby_rx_ppe_sb#(T))
   // declare expected and actual analyis ports as base-type
   uvm_analysis_imp_expected  #(mby_rx_ppe_sb_obj, mby_rx_ppe_sb #(T)) expected_imp;
   uvm_analysis_imp_actual    #(mby_rx_ppe_sb_obj, mby_rx_ppe_sb #(T)) actual_imp;

   // scoreboard queue.  It is assumed that expected object will be supplied before the actual object
   // TODO: add capability for out-of-order
   // TODO: add capability for multiple traffic classes
   T sb[$];

   function new(string name, uvm_component parent);
      super.new(name, parent);
      expected_imp = new("expected_imp", this);
      actual_imp   = new("actual_imp"  , this);
   endfunction : new

   function void build_phase(uvm_phase phase);
      // make sure that the type has been overriden
      T test_obj;
      super.build_phase(phase);
      `uvm_info(get_name(), $sformatf("build_phase(): confirming T returns type: %s", test_obj.my_type()), UVM_DEBUG)
   endfunction : build_phase

   //----------------------------------------------------------------------------
   // Function: write_expected
   //
   // write to the expected_imp are automatically forwarded by the `uvm_analysis_imp_decl(_expected) to this function
   // write_expected() casts the expected_pkt_ argument and adds it to the sb queue
   //----------------------------------------------------------------------------
   function void write_expected(mby_rx_ppe_sb_obj expected_pkt_);
      T expected_pkt;
      if (!$cast(expected_pkt, expected_pkt_)) begin
         `uvm_fatal(get_name(), "write_expected(): $cast() failed")
      end
      sb.push_back(expected_pkt);
   endfunction : write_expected

   //----------------------------------------------------------------------------
   // Function: write_actual
   //
   // write to the actual_imp are automatically forwarded by the `uvm_analysis_imp_decl(_actual) to this function
   // write_actual() casts the actual_pkt_ argument and calls its compare() function with the top element of the scoreboard
   //----------------------------------------------------------------------------
   function void write_actual(mby_rx_ppe_sb_obj actual_pkt_);
      T actual_pkt;
      if (!$cast(actual_pkt, actual_pkt_)) begin
         `uvm_fatal(get_name(), "write_actual(): $cast() failed")
      end
      if (sb.size() == 0) begin
         `uvm_error(get_name(), $sformatf("recieved 'actual' packet before any 'expected' packets.  Actual packet:\n%s\n", actual_pkt.sprint()))
         return;
      end
      if(actual_pkt.compare(sb[0]))
         `uvm_info(get_name(), $sformatf("actual pkt matches expected pkt.   Actual packet:\n%s\nExpected packet:\n%s\n", actual_pkt.sprint(), sb[0].sprint()), UVM_DEBUG)
      else 
         `uvm_error(get_name(), $sformatf("actual pkt does not match expected pkt.   Actual packet:\n%s\nExpected packet:\n%s\n", actual_pkt.sprint(), sb[0].sprint()))
      sb.pop_front();
   endfunction : write_actual

endclass : mby_rx_ppe_sb

