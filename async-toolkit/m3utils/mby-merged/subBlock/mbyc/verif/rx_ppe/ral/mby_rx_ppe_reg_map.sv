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
//   Author        :
//   Project       :
//------------------------------------------------------------------------------

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
//   Author        :
//   Project       :
//------------------------------------------------------------------------------

class mby_rx_ppe_reg_blk extends uvm_reg_block;

   `uvm_object_utils(mby_rx_ppe_reg_blk)


   // Register blocks
   ral_block_parser rx_ppe_parser;
   ral_block_mapper rx_ppe_mapper;
   //Add rest of the Rx PPE blocks

   // Maps
   uvm_reg_map rx_ppe_reg_map;


   // Constructor
   function new (string name = "mby_rx_ppe_reg_blk");
      super.new(name);
   endfunction : new

   virtual function void build ();

      rx_ppe_parser = ral_block_parser::type_id::create("rx_ppe_parser");
      rx_ppe_parser.configure(this);
      rx_ppe_parser.build();

      rx_ppe_mapper = ral_block_mapper::type_id::create("rx_ppe_mapper");
      rx_ppe_mapper.configure(this);
      rx_ppe_mapper.build();

      // create_map( string name, uvm_reg_addr_t base_addr, int unsigned n_bytes, uvm_endianness_e endian, bit byte_addressing = 1)
      rx_ppe_reg_map     = create_map("rx_ppe_reg_map",'h0, 8, UVM_LITTLE_ENDIAN);
      //virtual function void add_submap ( uvm_reg_map child_map, uvm_reg_addr_t offset)
      //TODO: Need to clean the addr values -- Picked random addresses for initial setup.
      rx_ppe_reg_map.add_submap(rx_ppe_parser.default_map, 32'h0000_0000);
      rx_ppe_reg_map.add_submap(rx_ppe_mapper.default_map, 32'h0004_0000);


      default_map = rx_ppe_reg_map;

      //TODO: Should this be moved to TB where the base addr of the reg_map is set?
      //lock_model();
   endfunction : build

endclass : mby_rx_ppe_reg_blk
