// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

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

//   Class:    mby_rx_ppe_env_shutdown_seq
//
//   This is the rx_ppe env Shutdown sequence file.

`ifndef __MBY_RX_PPE_ENV_SHUTDOWN_SEQ_GUARD
`define __MBY_RX_PPE_ENV_SHUTDOWN_SEQ_GUARD

`ifndef __INSIDE_MBY_RX_PPE_SEQ_LIB
`error "Attempt to include file outside of mby_rx_ppe_seq_lib."
`endif

class mby_rx_ppe_env_shutdown_seq extends shdv_base_shutdown_sequence;

   `uvm_object_utils(mby_rx_ppe_env_shutdown_seq)

   // Variable: env
   // rx_ppe Top Level Env.
   mby_rx_ppe_env_pkg::mby_rx_ppe_env         env;

   // Variable: tb_cfg
   // rx_ppe tb cfg.
   mby_rx_ppe_env_pkg::mby_rx_ppe_tb_top_cfg  tb_cfg;
   
   // Variable:  tb_ral
   // Handle to RX PPE RAL.
   mby_rx_ppe_reg_pkg::mby_rx_ppe_reg_blk     ral;   

   // Variable: vif
   // Handle to rx_ppe Tb interface.
   virtual mby_rx_ppe_tb_if                   tb_vif;   

   // Variable: rx_ppe_eot_seq
   // rx_ppe_eot_seq
   mby_rx_ppe_seq_lib::mby_rx_ppe_eot_seq        rx_ppe_eot_seq;

   //------------------------------------------------------------------------------
   //  Constructor: new
   //  
   //  Arguments:
   //  string name  - rx_ppe env shutdown sequence object name.
   //------------------------------------------------------------------------------
   function new(input string name = "mby_rx_ppe_env_shutdown_seq");
      super.new(name);
      set_env (shdv_base_env::get_top_tb_env());
   endfunction: new

   // ------------------------------------------------------------------------
   //  Function: set_env
   //  Arguments: shdv_base_env 
   // ------------------------------------------------------------------------
   virtual function void set_env(shdv_base_env tb_env);
      mby_rx_ppe_env_pkg::mby_rx_ppe_env temp_env;
      bit stat;

      stat = $cast(temp_env,tb_env);
      if(!stat) begin
         `uvm_fatal(get_name(), "Cast of sla_tb_env failed");
      end
      if(temp_env == null) begin
         `uvm_fatal(get_name(), "Could not fetch sla_tb_env handle!!!");
      end

      this.env = temp_env;
      this.ral = temp_env.get_tb_ral();
      this.tb_cfg = temp_env.get_tb_cfg();      
      this.tb_vif = temp_env.get_tb_vif();

   endfunction : set_env


   //------------------------------------------------------------------------------
   //  Task: body
   //  Check rx_ppe DUT.
   //------------------------------------------------------------------------------
   virtual task     body();

      `uvm_info(this.get_name(), ("Phase::shutdown_phase:mby_rx_ppe_env_shutdown_seq::Starting"), UVM_LOW)

      rx_ppe_eot_seq = mby_rx_ppe_seq_lib::mby_rx_ppe_eot_seq::type_id::create("rx_ppe_eot_seq");
//    rx_ppe_eot_seq.env       = this.env;
//    rx_ppe_eot_seq.ral_env   = this.ral;
//    rx_ppe_eot_seq.dut_cfg       = this.tb_cfg.dut_cfg;
      rx_ppe_eot_seq.access_type  = "FRONTDOOR";
	
      `uvm_info(get_name(), "********** Starting rx_ppe_eot_seq **********", UVM_MEDIUM);
//    rx_ppe_eot_seq.start(sla_sequencer::pick_sequencer("ral_sequencer"));
       
   endtask : body

endclass : mby_rx_ppe_env_shutdown_seq

`endif // __MBY_RX_PPE_ENV_SHUTDOWN_SEQ_GUARD
