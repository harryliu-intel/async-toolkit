// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

`ifndef RAL_MBY_TX_PB_MAP
`define RAL_MBY_TX_PB_MAP

import uvm_pkg::*;

class ral_reg_mby_tx_pb_map_TX_PB_PORT_CFG extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field PORT;

	function new(string name = "mby_tx_pb_map_TX_PB_PORT_CFG");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 32, 32, "RO", 0, 32'h0, 1, 0, 1);
      this.PORT = uvm_reg_field::type_id::create("PORT",,get_full_name());
      this.PORT.configure(this, 32, 0, "RW", 0, 32'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_mby_tx_pb_map_TX_PB_PORT_CFG)

endclass : ral_reg_mby_tx_pb_map_TX_PB_PORT_CFG


class ral_reg_mby_tx_pb_map_TX_PB_PREFETCH extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field BUF_SIZE;

	function new(string name = "mby_tx_pb_map_TX_PB_PREFETCH");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 56, 8, "RO", 0, 56'h0, 1, 0, 1);
      this.BUF_SIZE = uvm_reg_field::type_id::create("BUF_SIZE",,get_full_name());
      this.BUF_SIZE.configure(this, 8, 0, "RW", 0, 8'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_mby_tx_pb_map_TX_PB_PREFETCH)

endclass : ral_reg_mby_tx_pb_map_TX_PB_PREFETCH


class ral_block_mby_tx_pb_map extends uvm_reg_block;
	rand ral_reg_mby_tx_pb_map_TX_PB_PORT_CFG TX_PB_PORT_CFG;
	rand ral_reg_mby_tx_pb_map_TX_PB_PREFETCH TX_PB_PREFETCH[16];
	uvm_reg_field TX_PB_PORT_CFG_RSVD0;
	rand uvm_reg_field TX_PB_PORT_CFG_PORT;
	rand uvm_reg_field PORT;
	uvm_reg_field TX_PB_PREFETCH_RSVD0[16];
	rand uvm_reg_field TX_PB_PREFETCH_BUF_SIZE[16];
	rand uvm_reg_field BUF_SIZE[16];

	function new(string name = "mby_tx_pb_map");
		super.new(name, build_coverage(UVM_NO_COVERAGE));
	endfunction: new

   virtual function void build();
      this.default_map = create_map("", 0, 8, UVM_LITTLE_ENDIAN, 0);
      this.TX_PB_PORT_CFG = ral_reg_mby_tx_pb_map_TX_PB_PORT_CFG::type_id::create("TX_PB_PORT_CFG",,get_full_name());
      this.TX_PB_PORT_CFG.configure(this, null, "");
      this.TX_PB_PORT_CFG.build();
         this.TX_PB_PORT_CFG.add_hdl_path('{

            '{"TX_PB_PORT_CFG", -1, -1}
         });
      this.default_map.add_reg(this.TX_PB_PORT_CFG, `UVM_REG_ADDR_WIDTH'h0, "RW", 0);
		this.TX_PB_PORT_CFG_RSVD0 = this.TX_PB_PORT_CFG.RSVD0;
		this.TX_PB_PORT_CFG_PORT = this.TX_PB_PORT_CFG.PORT;
		this.PORT = this.TX_PB_PORT_CFG.PORT;
      foreach (this.TX_PB_PREFETCH[i]) begin
         int J = i;
         this.TX_PB_PREFETCH[J] = ral_reg_mby_tx_pb_map_TX_PB_PREFETCH::type_id::create($psprintf("TX_PB_PREFETCH[%0d]",J),,get_full_name());
         this.TX_PB_PREFETCH[J].configure(this, null, "");
         this.TX_PB_PREFETCH[J].build();
         this.TX_PB_PREFETCH[J].add_hdl_path('{

            '{$psprintf("TX_PB_PREFETCH[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.TX_PB_PREFETCH[J], `UVM_REG_ADDR_WIDTH'h80+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.TX_PB_PREFETCH_RSVD0[J] = this.TX_PB_PREFETCH[J].RSVD0;
			this.TX_PB_PREFETCH_BUF_SIZE[J] = this.TX_PB_PREFETCH[J].BUF_SIZE;
			this.BUF_SIZE[J] = this.TX_PB_PREFETCH[J].BUF_SIZE;
      end
   endfunction : build

	`uvm_object_utils(ral_block_mby_tx_pb_map)

endclass : ral_block_mby_tx_pb_map



`endif
