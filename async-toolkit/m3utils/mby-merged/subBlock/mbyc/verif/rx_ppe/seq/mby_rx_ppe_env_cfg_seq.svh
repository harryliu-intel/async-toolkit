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

//   Class:    mby_rx_ppe_env_cfg_seq
//
//   This is the rx_ppe ENV Config sequence file.

`ifndef __MBY_RX_PPE_ENV_CFG_SEQ_GUARD
`define __MBY_RX_PPE_ENV_CFG_SEQ_GUARD

`ifndef __INSIDE_MBY_RX_PPE_SEQ_LIB
`error "Attempt to include file outside of mby_rx_ppe_seq_lib."
`endif

class mby_rx_ppe_env_cfg_seq extends mby_rx_ppe_env_base_seq;

   // Variable: rx_ppe_cfg_seq
   // rx_ppe_cfg_seq
   mby_rx_ppe_seq_lib::mby_rx_ppe_cfg_seq        rx_ppe_cfg_seq;

   `uvm_object_utils(mby_rx_ppe_env_cfg_seq)

   //------------------------------------------------------------------------------
   //  Constructor: new
   //  Arguments:
   //  string name  - rx_ppe env config sequence object name.
   //------------------------------------------------------------------------------
   function new(input string name = "mby_rx_ppe_env_cfg_seq");
      super.new(name);
   endfunction: new

   //------------------------------------------------------------------------------
   //  Function: sm_config
   //  This finction calls the "allocate_mem" function which allocates memory
   //  address space for exclusive use.
   //------------------------------------------------------------------------------
   virtual function void sm_config();
//      sm.ag.allocate_mem(ag_result, "MMIO_LOW", 32'h2_0000, "GBE_MEM_LOW",32'h1_FFFF);
   endfunction

   //------------------------------------------------------------------------------
   //  Task: body
   //  Configures rx_ppe DUT.
   //------------------------------------------------------------------------------
   virtual task     body();
      `uvm_info(this.get_name(), ("Phase::config_phase:mby_rx_ppe_env_cfg_seq::Starting"), UVM_LOW)    

//    rx_ppe_cfg_seq = mby_rx_ppe_seq_lib::mby_rx_ppe_cfg_seq::type_id::create("rx_ppe_cfg_seq");
//    rx_ppe_cfg_seq.env       = env;
//    rx_ppe_cfg_seq.ral_env   = env.tp_env.get_tb_ral();
//    rx_ppe_cfg_seq.dut_cfg       = env.tb_cfg.dut_cfg;
//    rx_ppe_cfg_seq.access_type  = "FRONTDOOR";
	
      `uvm_info(get_name(), "********** Starting mby_rx_ppe_config_seq **********", UVM_MEDIUM);
//    rx_ppe_cfg_seq.start(sla_sequencer::pick_sequencer("ral_sequencer"));
       
   endtask : body


endclass : mby_rx_ppe_env_cfg_seq

`endif // __MBY_RX_PPE_ENV_CFG_SEQ_GUARD



/*

   // Variable: dut_cfg
   // Mesh dut cfg.
//k   mby_rx_ppe_env_pkg::mby_rx_ppe_dut_cfg     dut_cfg;
    
   // Variable: env
   // Protected rx_ppe Top Level Env
//k   protected mby_rx_ppe_env_pkg::mby_rx_ppe_env   env;

   // Variable: status
   // RAL Status
//k   sla_status_t status;

   // Variable: rd_val
   // Value for RAL Read
//k   sla_ral_data_t rd_val;

   // Variable: wr_val
   // Value for RAL Write
//k   sla_ral_data_t wr_val;

   // Variable: ral
   // RAL ENV
//k   sla_ral_env ral;

   // Variable: access_type
   // RAL ENV Access Type
//k   string access_type = "BACKDOOR";

*/
