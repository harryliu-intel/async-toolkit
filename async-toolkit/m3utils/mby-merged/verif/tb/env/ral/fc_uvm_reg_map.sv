// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :  Raghu P Gudla
// Created On   :  09/22/2018
// Description  :  FC UVM reg model/block
// -----------------------------------------------------------------------------

`ifndef _fc_uvm_reg_map_sv_
`define _fc_uvm_reg_map_sv_

class fc_uvm_reg_map extends uvm_reg_block;

  // factory registration
  `uvm_object_utils(fc_uvm_reg_map)

  // reg blk's
  ip3_uvm_reg_blk     _ip3_uvm_reg_blk;

  // map's
  uvm_reg_map axi_reg_map;
  uvm_reg_map apb_reg_map;

  // constructor
  function new (string name = "fc_uvm_reg_map");
     super.new(name);
  endfunction : new

  virtual function void build ();

     // ip3_uvm_cfg_reg_blk
     _ip3_uvm_reg_blk = ip3_uvm_reg_blk::type_id::create("_ip3_uvm_reg_blk");
     _ip3_uvm_reg_blk.configure(this); 
     _ip3_uvm_reg_blk.build();

     // map's
     axi_reg_map     = create_map("axi_reg_map",     'h0, 4, UVM_LITTLE_ENDIAN);
     apb_reg_map     = create_map("apb_reg_map",     'h0, 4, UVM_LITTLE_ENDIAN);

     // adding reg blk's to the map
     // --------------------------------
     // AXI :: continuos addressing
     // --------------------------------
     axi_reg_map.add_submap(_ip3_uvm_reg_blk._h1_reg_map, 32'h0000_4000);

     // --------------------------------
     // APB :: continuos addressing
     // --------------------------------
     apb_reg_map.add_submap(_ip3_uvm_reg_blk._h2_cfg_reg_map, 32'h0000_0800);
     apb_reg_map.add_submap(_ip3_uvm_reg_blk._h2_mem_reg_map, 32'h0008_8000);
     apb_reg_map.add_submap(_ip3_uvm_reg_blk._h2_io_reg_map,  32'h0808_8000);

     // add one as default
     default_map = apb_reg_map;

     lock_model();
  endfunction : build

endclass : fc_uvm_reg_map

`endif // _fc_uvm_reg_map_sv_
