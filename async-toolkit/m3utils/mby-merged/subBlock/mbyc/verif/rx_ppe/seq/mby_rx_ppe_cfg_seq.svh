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

//   Class:    mby_rx_ppe_cfg_seq
//
//   This is the rx_ppe Config sequence file.

`ifndef __MBY_RX_PPE_CFG_SEQ_GUARD
`define __MBY_RX_PPE_CFG_SEQ_GUARD

`ifndef __INSIDE_MBY_RX_PPE_SEQ_LIB
`error "Attempt to include file outside of mby_rx_ppe_seq_lib."
`endif


class mby_rx_ppe_cfg_seq extends uvm_sequence;

   `uvm_object_utils(mby_rx_ppe_cfg_seq)
   
   // Variable: access_type
   // RAL ENV Access Type
   string access_type = "BACKDOOR";
   
   // Variable: dut_cfg
   // dut cfg
   mby_rx_ppe_env_pkg::mby_rx_ppe_dut_cfg     dut_cfg;
   
   
   //------------------------------------------------------------------------------
   //  Task: new
   //  Confiure rx ppe
   //------------------------------------------------------------------------------
    function new(string name = "mby_rx_ppe_cfg_seq");
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
      `uvm_info(get_name(), "Top Env Configuration Sequence", UVM_MEDIUM);
      
      //Configure DUT here..

      config_parser();
      config_mapper();
      config_lpm();
      config_em();
      config_wcm();
      
      config_hash();
      config_policer();
      config_nexthop();
      config_maskgen();
      config_triggers();
	 
    endtask : body

   //------------------------------------------------------------------------------
   //  Task: config_parser
   //  Configures PARSER DUT.
   //------------------------------------------------------------------------------
   task config_parser();
      `uvm_info(get_name(), "config_parser", UVM_MEDIUM);

   endtask : config_parser

   //------------------------------------------------------------------------------
   //  Task: config_mapper
   //  Configures MAPPER DUT.
   //------------------------------------------------------------------------------
   task config_mapper();
      `uvm_info(get_name(), "config_mapper", UVM_MEDIUM);

   endtask : config_mapper

   //------------------------------------------------------------------------------
   //  Task: config_lpm
   //  Configures LPM DUT.
   //------------------------------------------------------------------------------
   task config_lpm();
      `uvm_info(get_name(), "config_lpm", UVM_MEDIUM);

   endtask : config_lpm

   //------------------------------------------------------------------------------
   //  Task: config_em
   //  Configures EM DUT.
   //------------------------------------------------------------------------------
   task config_em();
      `uvm_info(get_name(), "config_em", UVM_MEDIUM);

   endtask : config_em
   
   //------------------------------------------------------------------------------
   //  Task: config_wcm
   //  Configures WCM DUT.
   //------------------------------------------------------------------------------
   task config_wcm();
      `uvm_info(get_name(), "config_wcm", UVM_MEDIUM);

   endtask : config_wcm

   //------------------------------------------------------------------------------
   //  Task: config_hash
   //  Configures HASH DUT.
   //------------------------------------------------------------------------------
   task config_hash();
      `uvm_info(get_name(), "config_hash", UVM_MEDIUM);

   endtask : config_hash

   //------------------------------------------------------------------------------
   //  Task: config_policer
   //  Configures POLICER DUT.
   //------------------------------------------------------------------------------
   task config_policer();
      `uvm_info(get_name(), "config_policer", UVM_MEDIUM);

   endtask : config_policer

   //------------------------------------------------------------------------------
   //  Task: config_nexthop
   //  Configures NEXTHOP DUT.
   //------------------------------------------------------------------------------
   task config_nexthop();
      `uvm_info(get_name(), "config_nexthop", UVM_MEDIUM);

   endtask : config_nexthop

   //------------------------------------------------------------------------------
   //  Task: config_maskgen
   //  Configures MASKGEN DUT.
   //------------------------------------------------------------------------------
   task config_maskgen();
      `uvm_info(get_name(), "config_maskgen", UVM_MEDIUM);

   endtask : config_maskgen
   
   //------------------------------------------------------------------------------
   //  Task: config_triggers
   //  Configures TRIGGERS DUT.
   //------------------------------------------------------------------------------
   task config_triggers();
      `uvm_info(get_name(), "config_triggers", UVM_MEDIUM);

   endtask : config_triggers

endclass : mby_rx_ppe_cfg_seq

`endif // __MBY_RX_PPE_CFG_SEQ_GUARD
