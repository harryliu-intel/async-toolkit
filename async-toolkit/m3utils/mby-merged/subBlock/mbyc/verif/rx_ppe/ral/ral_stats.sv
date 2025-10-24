// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

`ifndef RAL_STATS
`define RAL_STATS

import uvm_pkg::*;

class ral_reg_stats_RX_STATS_BANK_FRAME extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field FRAME_COUNTER;

	function new(string name = "stats_RX_STATS_BANK_FRAME");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 16, 48, "RO", 0, 16'h0, 1, 0, 1);
      this.FRAME_COUNTER = uvm_reg_field::type_id::create("FRAME_COUNTER",,get_full_name());
      this.FRAME_COUNTER.configure(this, 48, 0, "RW", 1, 48'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_stats_RX_STATS_BANK_FRAME)

endclass : ral_reg_stats_RX_STATS_BANK_FRAME


class ral_reg_stats_RX_STATS_BANK_BYTE extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field BYTE_COUNTER;

	function new(string name = "stats_RX_STATS_BANK_BYTE");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 8, 56, "RO", 0, 8'h0, 1, 0, 1);
      this.BYTE_COUNTER = uvm_reg_field::type_id::create("BYTE_COUNTER",,get_full_name());
      this.BYTE_COUNTER.configure(this, 56, 0, "RW", 1, 56'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_stats_RX_STATS_BANK_BYTE)

endclass : ral_reg_stats_RX_STATS_BANK_BYTE


class ral_reg_stats_RX_STATS_VLAN_FRAME extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field FRAME_COUNTER;

	function new(string name = "stats_RX_STATS_VLAN_FRAME");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 28, 36, "RO", 0, 28'h0, 1, 0, 0);
      this.FRAME_COUNTER = uvm_reg_field::type_id::create("FRAME_COUNTER",,get_full_name());
      this.FRAME_COUNTER.configure(this, 36, 0, "RW", 1, 36'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_stats_RX_STATS_VLAN_FRAME)

endclass : ral_reg_stats_RX_STATS_VLAN_FRAME


class ral_reg_stats_RX_STATS_VLAN_BYTE extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field BYTE_COUNTER;

	function new(string name = "stats_RX_STATS_VLAN_BYTE");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 20, 44, "RO", 0, 20'h0, 1, 0, 0);
      this.BYTE_COUNTER = uvm_reg_field::type_id::create("BYTE_COUNTER",,get_full_name());
      this.BYTE_COUNTER.configure(this, 44, 0, "RW", 1, 44'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_stats_RX_STATS_VLAN_BYTE)

endclass : ral_reg_stats_RX_STATS_VLAN_BYTE


class ral_block_stats extends uvm_reg_block;
	rand ral_reg_stats_RX_STATS_BANK_FRAME RX_STATS_BANK_FRAME[4][0:143];
	rand ral_reg_stats_RX_STATS_BANK_BYTE RX_STATS_BANK_BYTE[4][0:143];
	rand ral_reg_stats_RX_STATS_VLAN_FRAME RX_STATS_VLAN_FRAME[16128];
	rand ral_reg_stats_RX_STATS_VLAN_BYTE RX_STATS_VLAN_BYTE[16128];
	uvm_reg_field RX_STATS_BANK_FRAME_RSVD0[4][0:143];
	rand uvm_reg_field RX_STATS_BANK_FRAME_FRAME_COUNTER[4][0:143];
	uvm_reg_field RX_STATS_BANK_BYTE_RSVD0[4][0:143];
	rand uvm_reg_field RX_STATS_BANK_BYTE_BYTE_COUNTER[4][0:143];
	uvm_reg_field RX_STATS_VLAN_FRAME_RSVD0[16128];
	rand uvm_reg_field RX_STATS_VLAN_FRAME_FRAME_COUNTER[16128];
	uvm_reg_field RX_STATS_VLAN_BYTE_RSVD0[16128];
	rand uvm_reg_field RX_STATS_VLAN_BYTE_BYTE_COUNTER[16128];

	function new(string name = "stats");
		super.new(name, build_coverage(UVM_NO_COVERAGE));
	endfunction: new

   virtual function void build();
      this.default_map = create_map("", 0, 8, UVM_LITTLE_ENDIAN, 0);
      foreach (this.RX_STATS_BANK_FRAME[i,j]) begin
         int J = i;
         int K = j;
         this.RX_STATS_BANK_FRAME[J][K] = ral_reg_stats_RX_STATS_BANK_FRAME::type_id::create($psprintf("RX_STATS_BANK_FRAME[%0d][%0d]",J,K),,get_full_name());
         this.RX_STATS_BANK_FRAME[J][K].configure(this, null, "");
         this.RX_STATS_BANK_FRAME[J][K].build();
         this.RX_STATS_BANK_FRAME[J][K].add_hdl_path('{

            '{$psprintf("RX_STATS_BANK_FRAME[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.RX_STATS_BANK_FRAME[J][K], `UVM_REG_ADDR_WIDTH'h0+J*`UVM_REG_ADDR_WIDTH'h800+K*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.RX_STATS_BANK_FRAME_RSVD0[J][K] = this.RX_STATS_BANK_FRAME[J][K].RSVD0;
			this.RX_STATS_BANK_FRAME_FRAME_COUNTER[J][K] = this.RX_STATS_BANK_FRAME[J][K].FRAME_COUNTER;
      end
      foreach (this.RX_STATS_BANK_BYTE[i,j]) begin
         int J = i;
         int K = j;
         this.RX_STATS_BANK_BYTE[J][K] = ral_reg_stats_RX_STATS_BANK_BYTE::type_id::create($psprintf("RX_STATS_BANK_BYTE[%0d][%0d]",J,K),,get_full_name());
         this.RX_STATS_BANK_BYTE[J][K].configure(this, null, "");
         this.RX_STATS_BANK_BYTE[J][K].build();
         this.RX_STATS_BANK_BYTE[J][K].add_hdl_path('{

            '{$psprintf("RX_STATS_BANK_BYTE[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.RX_STATS_BANK_BYTE[J][K], `UVM_REG_ADDR_WIDTH'h4000+J*`UVM_REG_ADDR_WIDTH'h800+K*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.RX_STATS_BANK_BYTE_RSVD0[J][K] = this.RX_STATS_BANK_BYTE[J][K].RSVD0;
			this.RX_STATS_BANK_BYTE_BYTE_COUNTER[J][K] = this.RX_STATS_BANK_BYTE[J][K].BYTE_COUNTER;
      end
      foreach (this.RX_STATS_VLAN_FRAME[i]) begin
         int J = i;
         this.RX_STATS_VLAN_FRAME[J] = ral_reg_stats_RX_STATS_VLAN_FRAME::type_id::create($psprintf("RX_STATS_VLAN_FRAME[%0d]",J),,get_full_name());
         this.RX_STATS_VLAN_FRAME[J].configure(this, null, "");
         this.RX_STATS_VLAN_FRAME[J].build();
         this.RX_STATS_VLAN_FRAME[J].add_hdl_path('{

            '{$psprintf("RX_STATS_VLAN_FRAME[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.RX_STATS_VLAN_FRAME[J], `UVM_REG_ADDR_WIDTH'h8000+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.RX_STATS_VLAN_FRAME_RSVD0[J] = this.RX_STATS_VLAN_FRAME[J].RSVD0;
			this.RX_STATS_VLAN_FRAME_FRAME_COUNTER[J] = this.RX_STATS_VLAN_FRAME[J].FRAME_COUNTER;
      end
      foreach (this.RX_STATS_VLAN_BYTE[i]) begin
         int J = i;
         this.RX_STATS_VLAN_BYTE[J] = ral_reg_stats_RX_STATS_VLAN_BYTE::type_id::create($psprintf("RX_STATS_VLAN_BYTE[%0d]",J),,get_full_name());
         this.RX_STATS_VLAN_BYTE[J].configure(this, null, "");
         this.RX_STATS_VLAN_BYTE[J].build();
         this.RX_STATS_VLAN_BYTE[J].add_hdl_path('{

            '{$psprintf("RX_STATS_VLAN_BYTE[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.RX_STATS_VLAN_BYTE[J], `UVM_REG_ADDR_WIDTH'h28000+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.RX_STATS_VLAN_BYTE_RSVD0[J] = this.RX_STATS_VLAN_BYTE[J].RSVD0;
			this.RX_STATS_VLAN_BYTE_BYTE_COUNTER[J] = this.RX_STATS_VLAN_BYTE[J].BYTE_COUNTER;
      end
   endfunction : build

	`uvm_object_utils(ral_block_stats)

endclass : ral_block_stats



`endif
