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
//   Run Cmd       : trex mby_rx_ppe_l2_basic_test -ace_args -simv_args '"'+UVM_VERBOSITY=UVM_HIGH +fsdb '"' -ace_args- -seed 1 -dut mby -model rx_ppe
//------------------------------------------------------------------------------

//   Class:    mby_rx_ppe_l2_basic_test
//

`ifndef MBY_RX_PPE_L2_BASIC_TEST_SVH
`define MBY_RX_PPE_L2_BASIC_TEST_SVH

`ifndef __INSIDE_MBY_RX_PPE_TEST_LIB
`error "Attempt to include file outside of mby_rx_ppe_test_lib."
`endif

class mby_vars;

   rand int num_pkts;
   rand int max_pkt_len;
   rand int min_pkt_len;
    
   //------------------------------------------------------------------------------
   //  Constructor: new
   //  Arguments:
   //  name   - mby_rx_vars object name.
   //------------------------------------------------------------------------------
   function new (string name="mby_rx_vars");     
   endfunction :  new
   
   constraint num_pkts_c {
      num_pkts == 1;
   } 
   constraint max_pkt_len_c {
      max_pkt_len == 64;
   } 
   constraint min_pkt_len_c {
      min_pkt_len == 64;
   }
   
   function void post_randomize();
      begin
         `uvm_info("mby_vars:",$psprintf("num_pkts=%0d max_pkt_len=%0d min_pkt_len=%0d", num_pkts,max_pkt_len,min_pkt_len), UVM_NONE)
      end
   endfunction :  post_randomize
   
endclass : mby_vars   

class mby_rx_ppe_l2_basic_seq extends mby_rx_ppe_seq_lib::mby_rx_ppe_env_base_seq;

   `uvm_object_utils(mby_rx_ppe_l2_basic_seq)
   
   int num_pkts;
   int min_pkt_len;
   int max_pkt_len;
   mby_vars my_vars;
   
   
   //------------------------------------------------------------------------------
   //  Constructor: new
   //  Arguments:
   //  name   - rx_ppe l2 basic seq object name.
   //------------------------------------------------------------------------------
   function new (string name="mby_rx_ppe_l2_basic_seq");
      super.new (name);
      
      my_vars = new();
      my_vars.randomize();
  
   endfunction :  new

   //------------------------------------------------------------------------------
   //  Task:  body
   //  
   //------------------------------------------------------------------------------
   virtual task body();
      pktlib_pkg::pktlib_class p,pkt2;
      bit [7:0] bytes [], ubytes [];
      bit[47:0] mac_da = 48'hbbaadeadbeee;
      bit[47:0] mac_sa = 48'hccdd55555555;      
            
      repeat(my_vars.num_pkts) begin //{
         `uvm_info(this.get_name(), $psprintf("Phase::main_phase:mby_rx_ppe_l2_basic_seq::Starting mac_da=%0h mac_sa=%0h", mac_da,mac_sa), UVM_LOW) 
         p = pktlib_class::type_id::create("p");
      
         // configure headers
         p.cfg_hdr('{p.eth[0],p.data[0]});
      
         p.toh.min_plen = my_vars.min_pkt_len;
         p.toh.max_plen = my_vars.max_pkt_len;

         p.randomize() with {
            eth[0].da == mac_da;
	    eth[0].sa == mac_sa;
	    eth[0].etype_encap == 1;
            data[0].data_len == 46;   // Remove this constraint for larger packts.
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
	 
	 mac_da = mac_da + 48'h1;
	 mac_sa = mac_sa + 48'h1;
      end //}      
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
