// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

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
      mbyRxMacToParser igr_pkt;
      mbyParserToMapper parser_pkt;

      this.set_name("mby_rx_ppe_wm_access_seq");

      `uvm_info(get_name(), "mby_rx_ppe_wm_access_seq is running!", UVM_NONE);

      igr_pkt.RX_PORT = 4;    //$urandom_range(16, 1);
      igr_pkt.RX_LENGTH = 64; //$urandom_range(128, 1);

      for( int idx = 0; idx < igr_pkt.RX_LENGTH; idx++ ) begin
         igr_pkt.RX_DATA[idx] = idx;
      end

      `uvm_info(get_name(), "Sending pkt to WM Parser", UVM_MEDIUM)
      `uvm_info(get_name(),  $sformatf("PKT send: Port:%0d, length:%0d ",igr_pkt.RX_PORT ,igr_pkt.RX_LENGTH),UVM_HIGH)

      err_info = wm_Parser(igr_pkt, parser_pkt);

      `uvm_info(get_name(),  $sformatf("Received Parser pkt:Err_info: %d\n Port:%0d, length:%0d, \n\
            PA_ADJ_SEG_LEN: %d\n PA_PACKET_TYPE : %0d",err_info, parser_pkt.RX_PORT,
            parser_pkt.RX_LENGTH, parser_pkt.PA_ADJ_SEG_LEN, parser_pkt.PA_PACKET_TYPE),UVM_HIGH)

    //  for( int idx = 0; idx < parser_pkt.RX_LENGTH; idx++ ) begin
    //     `uvm_info(get_name(),  $sformatf("Parser pkt data[%0d]  :%0d ",idx, parser_pkt.RX_DATA[idx]),UVM_HIGH)
    //  end

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
      super.set_default_sequences();

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

