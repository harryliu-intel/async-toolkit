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

// Class: mby_mesh_reg_blk
// Description

class mby_mesh_reg_blk extends uvm_reg_block;

   `uvm_object_utils(mby_mesh_reg_blk)


   // Register blocks
   ral_block_mby_mesh_row_map mesh_row;


   //Variable: mesh_reg_map
   // Maps
   uvm_reg_map mesh_reg_map;


   // Constructor
   function new (string name = "mby_mesh_reg_blk");
      super.new(name);
   endfunction : new

   virtual function void build ();

      mesh_row = ral_block_mby_mesh_row_map::type_id::create("mesh_row");
      mesh_row.configure(this);
      mesh_row.build();

      // create_map( string name, uvm_reg_addr_t base_addr, int unsigned n_bytes, uvm_endianness_e endian, bit byte_addressing = 1)
      mesh_reg_map     = create_map("mesh_reg_map",'h0, 8, UVM_LITTLE_ENDIAN);
      //virtual function void add_submap ( uvm_reg_map child_map, uvm_reg_addr_t offset)
      //TODO: Need to clean the addr values -- Picked random addresses for initial setup.
      mesh_reg_map.add_submap(mesh_row.default_map, 32'h0000_0000);

      default_map = mesh_reg_map;


   endfunction : build

endclass : mby_mesh_reg_blk
