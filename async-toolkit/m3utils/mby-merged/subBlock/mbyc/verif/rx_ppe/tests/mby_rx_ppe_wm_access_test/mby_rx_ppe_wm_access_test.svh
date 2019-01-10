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
// Author      :
// Project     :
// Description : This test performs basic access to WM.
//------------------------------------------------------------------------------


`ifndef MBY_RX_PPE_WM_ACCESS__SVH
`define MBY_RX_PPE_WM_ACCESS__SVH

class mby_rx_ppe_wm_access_seq extends mby_rx_ppe_seq_lib::mby_rx_ppe_env_base_seq;

   `uvm_object_utils(mby_rx_ppe_wm_access_seq)

   function new (string name="mby_rx_ppe_wm_access_seq");
      super.new (name);
      this.set_automatic_phase_objection(1);
      `uvm_info(this.get_name(), ("mby_rx_ppe_wm_access_seq::new"), UVM_NONE)

   endfunction :  new

   task body();

      int err_info;
      byte rx_data[];
      mbyRxMacToParser igr_pkt;
      mbyParserToMapper parser_pkt;
      wm_pkt_t push_pkt;

      this.set_name("mby_rx_ppe_wm_access_seq");

      `uvm_info(get_name(), "mby_rx_ppe_wm_access_seq is running!", UVM_NONE);

      igr_pkt.RX_PORT = 1;    //$urandom_range(16, 1);
      igr_pkt.RX_LENGHT = 64; //$urandom_range(64, 1);
      rx_data = new[igr_pkt.RX_LENGHT];

      rx_data  =
      {
         8'h00, 8'h01, 8'h02, 8'h03, 8'h04, 8'h05, 8'h06, 8'h07, 8'h08, 8'h09, 8'h0a, 8'h0b,
         8'h0c, 8'h0d, 8'h0e, 8'h0f, 8'h10, 8'h11, 8'h12, 8'h13, 8'h14, 8'h15, 8'h16, 8'h17,
         8'h18, 8'h19, 8'h1a, 8'h1b, 8'h1c, 8'h1d, 8'h1e, 8'h1f, 8'h20, 8'h21, 8'h22, 8'h23,
         8'h24, 8'h25, 8'h26, 8'h27, 8'h28, 8'h29, 8'h2a, 8'h2b, 8'h2c, 8'h2d, 8'h2e, 8'h2f,
         8'h30, 8'h31, 8'h32, 8'h33, 8'h34, 8'h35, 8'h36, 8'h37, 8'h38, 8'h39, 8'h3a, 8'h3b,
         8'hee, 8'h7f, 8'hec, 8'hb0
      };

      foreach(rx_data[idx]) begin
         igr_pkt.RX_DATA[idx] = rx_data[idx];
         push_pkt.data[idx] = rx_data[idx];;
      end

      `uvm_info(get_name(), "Sending pkt to WM Parser", UVM_NONE)
      `uvm_info(get_name(),  $sformatf("PKT send: Port:%0d, length:%0d ",igr_pkt.RX_PORT ,igr_pkt.RX_LENGHT),UVM_NONE)

      err_info = wm_parser(igr_pkt, parser_pkt);

      `uvm_info(get_name(),  $sformatf("Received Parser pkt:Err_info: %d\n Port:%0d, length:%0d, PA_ADJ_SEG_LEN: %d",err_info, parser_pkt.RX_PORT, parser_pkt.RX_LENGTH, parser_pkt.PA_ADJ_SEG_LEN),UVM_NONE)
  
//      for(int idx = 0; idx < parser_pkt.RX_LENGTH ; idx++)
//         `uvm_info(get_name(),  $sformatf("Received Parser pkt Data[%0d]: %0x",idx,parser_pkt.RX_DATA[idx]),UVM_NONE)
//      push_pkt.port = 1;
//      push_pkt.len = 64;
//      `uvm_info(get_name(), "Sending pkt to WM Push Pkt", UVM_NONE)
//      err_info = wm_pkt_push(push_pkt);

   endtask

endclass

class mby_rx_ppe_wm_access_test extends mby_rx_ppe_base_test;

   `uvm_component_utils(mby_rx_ppe_wm_access_test)

   function new (string name="mby_rx_ppe_wm_access_test", uvm_component parent=null);
      super.new (name, parent);
   endfunction :  new

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

      `uvm_info("::set_default_sequences", "Setting phase sequences", UVM_NONE)

      // main phase sequence
      uvm_config_db#(uvm_object_wrapper)::set(this,
         "env.tb_seqr.main_phase",
         "default_sequence",
         mby_rx_ppe_wm_access_seq::type_id::get());
   endfunction : set_default_sequences

   //---------------------------------------------------------------------------
   // Function: start_of_simulation_phase()
   //
   // Updates the uvm report server to use MBY's report server
   //
   //---------------------------------------------------------------------------
   function void start_of_simulation_phase( uvm_phase phase );
      super.start_of_simulation_phase( phase );

      `uvm_info("start_of_simulation_phase()", "Exiting start_of_sim phase", UVM_NONE)
   endfunction: start_of_simulation_phase

   //---------------------------------------------------------------------------
   // Task: run_phase()
   //
   // Prints out a message to identify start of run phase
   //---------------------------------------------------------------------------
   task run_phase(uvm_phase phase);
      phase.raise_objection(this);
      `uvm_info("::run_phase()", "Starting run phase", UVM_NONE)
      phase.drop_objection(this);
   endtask : run_phase


endclass : mby_rx_ppe_wm_access_test


`endif

