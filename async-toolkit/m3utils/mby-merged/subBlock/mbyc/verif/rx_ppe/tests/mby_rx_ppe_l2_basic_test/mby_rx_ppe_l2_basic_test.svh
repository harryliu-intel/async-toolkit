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

//   Class:    mby_rx_ppe_l2_basic_test
//

`ifndef MBY_RX_PPE_L2_BASIC_TEST_SVH
`define MBY_RX_PPE_L2_BASIC_TEST_SVH

`ifndef __INSIDE_MBY_RX_PPE_TEST_LIB
`error "Attempt to include file outside of mby_rx_ppe_test_lib."
`endif

class mby_rx_ppe_l2_basic_seq extends mby_rx_ppe_seq_lib::mby_rx_ppe_env_base_seq;

   `uvm_object_utils(mby_rx_ppe_l2_basic_seq)

   //------------------------------------------------------------------------------
   //  Constructor: new
   //  Arguments:
   //  name   - rx_ppe l2 basic seq object name.
   //------------------------------------------------------------------------------
   function new (string name="mby_rx_ppe_l2_basic_seq");
      super.new (name);
   endfunction :  new

   //------------------------------------------------------------------------------
   //  Task:  body
   //  
   //------------------------------------------------------------------------------
   virtual task body();
      int count = 0;
      pktlib_pkg::pktlib_class p,pkt2;
      bit [7:0] bytes [], ubytes [];
      
      `uvm_info(this.get_name(), ("Phase::main_phase:mby_rx_ppe_l2_basic_seq::Starting"), UVM_LOW) 
      p = pktlib_class::type_id::create("p");
      
      // configure headers
      p.cfg_hdr('{p.eth[0],p.data[0]});
      
      p.toh.min_plen = 64;
      p.toh.max_plen = 64;
      
      p.randomize() with {
                eth[0].da == 48'hbbaadeadbeef;
		eth[0].sa == 48'hccdd55555555;
                data[0].data_len == 46;
			};      
      
      p.pack_hdr(bytes);
      
      `uvm_info(this.get_name(), ("Phase::main_phase:mby_rx_ppe_l2_basic_seq::pack"), UVM_LOW)
      p.display_cfg_hdr();
      p.display_hdr();
      p.display_pkt(p.pkt);
      
      `uvm_info(this.get_name(), ("Phase::main_phase:mby_rx_ppe_l2_basic_seq::unpack"), UVM_LOW)
      
      ubytes = p.pkt;
      pkt2 = pktlib_class::type_id::create("pkt2");
      pkt2.unpack_hdr(ubytes, SMART_UNPACK);
      
      pkt2.display_cfg_hdr();
      pkt2.display_hdr();
      pkt2.display_pkt(p.pkt);
            
   endtask
   
endclass : mby_rx_ppe_l2_basic_seq

/////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////

class mby_rx_ppe_l2_basic_test extends mby_rx_ppe_base_test;

   `uvm_component_utils(mby_rx_ppe_l2_basic_test)
   //------------------------------------------------------------------------------
   // Constructor: new
   //  Arguments:
   //  name   - rx_ppe alive test object name.
   //  parent - Component parent object.
   //------------------------------------------------------------------------------
   function new (string name="mby_rx_ppe_l2_basic_test", uvm_component parent=null);
      super.new (name, parent);
   endfunction :  new

   //------------------------------------------------------------------------------
   // Function: build_phase
   //  Arguments:
   //  phase - uvm_phase object.
   //------------------------------------------------------------------------------
   function void build_phase(uvm_phase phase);
      super.build_phase(phase);
   endfunction : build_phase

   //------------------------------------------------------------------------------
   // Function: connect_phase
   // Sets USER_DATA_PHASE sequence.
   //
   //  Arguments:
   //  phase - uvm_phase object.
   //------------------------------------------------------------------------------
   function void connect_phase(uvm_phase phase);
      super.connect_phase(phase);
      set_default_sequences();

   endfunction : connect_phase
   
   //---------------------------------------------------------------------------
   // Function: set_default_sequences()
   //---------------------------------------------------------------------------
   function void set_default_sequences();
      super.set_default_sequences();
      `uvm_info("::set_default_sequences", "Setting phase sequences", UVM_NONE)

      // Specifying main phase sequence
      uvm_config_db#(uvm_object_wrapper)::set(this,
         "env.tb_seqr.main_phase",
         "default_sequence",
         mby_rx_ppe_l2_basic_seq::type_id::get());
   endfunction : set_default_sequences   

endclass : mby_rx_ppe_l2_basic_test


`endif // MBY_RX_PPE_L2_BASIC_TEST_SVH
