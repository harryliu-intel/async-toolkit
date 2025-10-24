// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

`ifndef RAL_TRIG_APPLY_MISC
`define RAL_TRIG_APPLY_MISC

import uvm_pkg::*;

class ral_reg_trig_apply_misc_TRIGGER_RATE_LIM_CFG_2 extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field DROP_MASK;

	function new(string name = "trig_apply_misc_TRIGGER_RATE_LIM_CFG_2");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 40, 24, "RO", 0, 40'h0, 1, 0, 1);
      this.DROP_MASK = uvm_reg_field::type_id::create("DROP_MASK",,get_full_name());
      this.DROP_MASK.configure(this, 24, 0, "RW", 0, 24'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_trig_apply_misc_TRIGGER_RATE_LIM_CFG_2)

endclass : ral_reg_trig_apply_misc_TRIGGER_RATE_LIM_CFG_2


class ral_reg_trig_apply_misc_TRIGGER_ACTION_METADATA_MASK extends uvm_reg;
	rand uvm_reg_field METADATA_MASK;

	function new(string name = "trig_apply_misc_TRIGGER_ACTION_METADATA_MASK");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.METADATA_MASK = uvm_reg_field::type_id::create("METADATA_MASK",,get_full_name());
      this.METADATA_MASK.configure(this, 64, 0, "RW", 0, 64'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_trig_apply_misc_TRIGGER_ACTION_METADATA_MASK)

endclass : ral_reg_trig_apply_misc_TRIGGER_ACTION_METADATA_MASK


class ral_reg_trig_apply_misc_TRIGGER_IP extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field PENDING;

	function new(string name = "trig_apply_misc_TRIGGER_IP");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 16, 48, "RO", 0, 16'h0, 1, 0, 1);
      this.PENDING = uvm_reg_field::type_id::create("PENDING",,get_full_name());
      this.PENDING.configure(this, 48, 0, "W1C", 1, 48'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_trig_apply_misc_TRIGGER_IP)

endclass : ral_reg_trig_apply_misc_TRIGGER_IP


class ral_reg_trig_apply_misc_TRIGGER_IM extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field MASK;

	function new(string name = "trig_apply_misc_TRIGGER_IM");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 16, 48, "RO", 0, 16'h0, 1, 0, 1);
      this.MASK = uvm_reg_field::type_id::create("MASK",,get_full_name());
      this.MASK.configure(this, 48, 0, "RW", 0, 48'hffffffffffff, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_trig_apply_misc_TRIGGER_IM)

endclass : ral_reg_trig_apply_misc_TRIGGER_IM


class ral_reg_trig_apply_misc_TRIGGER_RATE_LIM_EMPTY extends uvm_reg;
	uvm_reg_field RSVD0;
	uvm_reg_field EMPTY;

	function new(string name = "trig_apply_misc_TRIGGER_RATE_LIM_EMPTY");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 48, 16, "RO", 0, 48'h0, 1, 0, 1);
      this.EMPTY = uvm_reg_field::type_id::create("EMPTY",,get_full_name());
      this.EMPTY.configure(this, 16, 0, "RO", 1, 16'hffff, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_trig_apply_misc_TRIGGER_RATE_LIM_EMPTY)

endclass : ral_reg_trig_apply_misc_TRIGGER_RATE_LIM_EMPTY


class ral_reg_trig_apply_misc_MA_TCN_FIFO_0 extends uvm_reg;
	uvm_reg_field RSVD0;
	uvm_reg_field PORT;
	uvm_reg_field MAC_ADDRESS;

	function new(string name = "trig_apply_misc_MA_TCN_FIFO_0");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 11, 53, "RO", 0, 11'h0, 1, 0, 0);
      this.PORT = uvm_reg_field::type_id::create("PORT",,get_full_name());
      this.PORT.configure(this, 5, 48, "RO", 1, 5'h0, 1, 0, 0);
      this.MAC_ADDRESS = uvm_reg_field::type_id::create("MAC_ADDRESS",,get_full_name());
      this.MAC_ADDRESS.configure(this, 48, 0, "RO", 1, 48'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_trig_apply_misc_MA_TCN_FIFO_0)

endclass : ral_reg_trig_apply_misc_MA_TCN_FIFO_0


class ral_reg_trig_apply_misc_MA_TCN_FIFO_1 extends uvm_reg;
	uvm_reg_field RSVD0;
	uvm_reg_field L2_DOMAIN;
	uvm_reg_field VID;

	function new(string name = "trig_apply_misc_MA_TCN_FIFO_1");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 44, 20, "RO", 0, 44'h0, 1, 0, 0);
      this.L2_DOMAIN = uvm_reg_field::type_id::create("L2_DOMAIN",,get_full_name());
      this.L2_DOMAIN.configure(this, 8, 12, "RO", 1, 8'h0, 1, 0, 0);
      this.VID = uvm_reg_field::type_id::create("VID",,get_full_name());
      this.VID.configure(this, 12, 0, "RO", 1, 12'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_trig_apply_misc_MA_TCN_FIFO_1)

endclass : ral_reg_trig_apply_misc_MA_TCN_FIFO_1


class ral_reg_trig_apply_misc_MA_TCN_DEQUEUE extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field READY;

	function new(string name = "trig_apply_misc_MA_TCN_DEQUEUE");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 63, 1, "RO", 0, 63'h0, 1, 0, 0);
      this.READY = uvm_reg_field::type_id::create("READY",,get_full_name());
      this.READY.configure(this, 1, 0, "RW", 1, 1'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_trig_apply_misc_MA_TCN_DEQUEUE)

endclass : ral_reg_trig_apply_misc_MA_TCN_DEQUEUE


class ral_reg_trig_apply_misc_MA_TCN_DATA_0 extends uvm_reg;
	uvm_reg_field RSVD0;
	uvm_reg_field VALID;
	uvm_reg_field PORT;
	uvm_reg_field MAC_ADDRESS;

	function new(string name = "trig_apply_misc_MA_TCN_DATA_0");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 10, 54, "RO", 0, 10'h0, 1, 0, 0);
      this.VALID = uvm_reg_field::type_id::create("VALID",,get_full_name());
      this.VALID.configure(this, 1, 53, "RO", 1, 1'h0, 1, 0, 0);
      this.PORT = uvm_reg_field::type_id::create("PORT",,get_full_name());
      this.PORT.configure(this, 5, 48, "RO", 1, 5'h0, 1, 0, 0);
      this.MAC_ADDRESS = uvm_reg_field::type_id::create("MAC_ADDRESS",,get_full_name());
      this.MAC_ADDRESS.configure(this, 48, 0, "RO", 1, 48'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_trig_apply_misc_MA_TCN_DATA_0)

endclass : ral_reg_trig_apply_misc_MA_TCN_DATA_0


class ral_reg_trig_apply_misc_MA_TCN_DATA_1 extends uvm_reg;
	uvm_reg_field RSVD0;
	uvm_reg_field L2_DOMAIN;
	uvm_reg_field VID;

	function new(string name = "trig_apply_misc_MA_TCN_DATA_1");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 44, 20, "RO", 0, 44'h0, 1, 0, 0);
      this.L2_DOMAIN = uvm_reg_field::type_id::create("L2_DOMAIN",,get_full_name());
      this.L2_DOMAIN.configure(this, 8, 12, "RO", 1, 8'h0, 1, 0, 0);
      this.VID = uvm_reg_field::type_id::create("VID",,get_full_name());
      this.VID.configure(this, 12, 0, "RO", 1, 12'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_trig_apply_misc_MA_TCN_DATA_1)

endclass : ral_reg_trig_apply_misc_MA_TCN_DATA_1


class ral_reg_trig_apply_misc_MA_TCN_PTR_HEAD extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field HEAD;

	function new(string name = "trig_apply_misc_MA_TCN_PTR_HEAD");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 55, 9, "RO", 0, 55'h0, 1, 0, 0);
      this.HEAD = uvm_reg_field::type_id::create("HEAD",,get_full_name());
      this.HEAD.configure(this, 9, 0, "RW", 1, 9'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_trig_apply_misc_MA_TCN_PTR_HEAD)

endclass : ral_reg_trig_apply_misc_MA_TCN_PTR_HEAD


class ral_reg_trig_apply_misc_MA_TCN_PTR_TAIL extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field TAIL;

	function new(string name = "trig_apply_misc_MA_TCN_PTR_TAIL");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 55, 9, "RO", 0, 55'h0, 1, 0, 0);
      this.TAIL = uvm_reg_field::type_id::create("TAIL",,get_full_name());
      this.TAIL.configure(this, 9, 0, "RW", 1, 9'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_trig_apply_misc_MA_TCN_PTR_TAIL)

endclass : ral_reg_trig_apply_misc_MA_TCN_PTR_TAIL


class ral_reg_trig_apply_misc_MA_TCN_IP extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field PENDING_EVENTS;
	rand uvm_reg_field TCN_OVERFLOW;

	function new(string name = "trig_apply_misc_MA_TCN_IP");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 62, 2, "RO", 0, 62'h0, 1, 0, 0);
      this.PENDING_EVENTS = uvm_reg_field::type_id::create("PENDING_EVENTS",,get_full_name());
      this.PENDING_EVENTS.configure(this, 1, 1, "W1C", 1, 1'h0, 1, 0, 0);
      this.TCN_OVERFLOW = uvm_reg_field::type_id::create("TCN_OVERFLOW",,get_full_name());
      this.TCN_OVERFLOW.configure(this, 1, 0, "W1C", 1, 1'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_trig_apply_misc_MA_TCN_IP)

endclass : ral_reg_trig_apply_misc_MA_TCN_IP


class ral_reg_trig_apply_misc_MA_TCN_IM extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field PENDING_EVENTS;
	rand uvm_reg_field TCN_OVERFLOW;

	function new(string name = "trig_apply_misc_MA_TCN_IM");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 62, 2, "RO", 0, 62'h0, 1, 0, 0);
      this.PENDING_EVENTS = uvm_reg_field::type_id::create("PENDING_EVENTS",,get_full_name());
      this.PENDING_EVENTS.configure(this, 1, 1, "RW", 0, 1'h1, 1, 0, 0);
      this.TCN_OVERFLOW = uvm_reg_field::type_id::create("TCN_OVERFLOW",,get_full_name());
      this.TCN_OVERFLOW.configure(this, 1, 0, "RW", 0, 1'h1, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_trig_apply_misc_MA_TCN_IM)

endclass : ral_reg_trig_apply_misc_MA_TCN_IM


class ral_reg_trig_apply_misc_MA_TCN_WM extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field WM;

	function new(string name = "trig_apply_misc_MA_TCN_WM");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 55, 9, "RO", 0, 55'h0, 1, 0, 0);
      this.WM = uvm_reg_field::type_id::create("WM",,get_full_name());
      this.WM.configure(this, 9, 0, "RW", 0, 9'h1e, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_trig_apply_misc_MA_TCN_WM)

endclass : ral_reg_trig_apply_misc_MA_TCN_WM


class ral_reg_trig_apply_misc_MA_TCN_USAGE extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field USAGE;

	function new(string name = "trig_apply_misc_MA_TCN_USAGE");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 55, 9, "RO", 0, 55'h0, 1, 0, 0);
      this.USAGE = uvm_reg_field::type_id::create("USAGE",,get_full_name());
      this.USAGE.configure(this, 9, 0, "RW", 0, 9'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_trig_apply_misc_MA_TCN_USAGE)

endclass : ral_reg_trig_apply_misc_MA_TCN_USAGE


class ral_block_trig_apply_misc extends uvm_reg_block;
	rand ral_reg_trig_apply_misc_TRIGGER_RATE_LIM_CFG_2 TRIGGER_RATE_LIM_CFG_2[16];
	rand ral_reg_trig_apply_misc_TRIGGER_ACTION_METADATA_MASK TRIGGER_ACTION_METADATA_MASK[2][0:3];
	rand ral_reg_trig_apply_misc_TRIGGER_IP TRIGGER_IP[2];
	rand ral_reg_trig_apply_misc_TRIGGER_IM TRIGGER_IM[2];
	rand ral_reg_trig_apply_misc_TRIGGER_RATE_LIM_EMPTY TRIGGER_RATE_LIM_EMPTY;
	rand ral_reg_trig_apply_misc_MA_TCN_FIFO_0 MA_TCN_FIFO_0[512];
	rand ral_reg_trig_apply_misc_MA_TCN_FIFO_1 MA_TCN_FIFO_1[512];
	rand ral_reg_trig_apply_misc_MA_TCN_DEQUEUE MA_TCN_DEQUEUE;
	rand ral_reg_trig_apply_misc_MA_TCN_DATA_0 MA_TCN_DATA_0;
	rand ral_reg_trig_apply_misc_MA_TCN_DATA_1 MA_TCN_DATA_1;
	rand ral_reg_trig_apply_misc_MA_TCN_PTR_HEAD MA_TCN_PTR_HEAD;
	rand ral_reg_trig_apply_misc_MA_TCN_PTR_TAIL MA_TCN_PTR_TAIL;
	rand ral_reg_trig_apply_misc_MA_TCN_IP MA_TCN_IP;
	rand ral_reg_trig_apply_misc_MA_TCN_IM MA_TCN_IM;
	rand ral_reg_trig_apply_misc_MA_TCN_WM MA_TCN_WM[17];
	rand ral_reg_trig_apply_misc_MA_TCN_USAGE MA_TCN_USAGE[17];
	uvm_reg_field TRIGGER_RATE_LIM_CFG_2_RSVD0[16];
	rand uvm_reg_field TRIGGER_RATE_LIM_CFG_2_DROP_MASK[16];
	rand uvm_reg_field DROP_MASK[16];
	rand uvm_reg_field TRIGGER_ACTION_METADATA_MASK_METADATA_MASK[2][0:3];
	rand uvm_reg_field METADATA_MASK[2][0:3];
	uvm_reg_field TRIGGER_IP_RSVD0[2];
	rand uvm_reg_field TRIGGER_IP_PENDING[2];
	rand uvm_reg_field PENDING[2];
	uvm_reg_field TRIGGER_IM_RSVD0[2];
	rand uvm_reg_field TRIGGER_IM_MASK[2];
	rand uvm_reg_field MASK[2];
	uvm_reg_field TRIGGER_RATE_LIM_EMPTY_RSVD0;
	uvm_reg_field TRIGGER_RATE_LIM_EMPTY_EMPTY;
	uvm_reg_field EMPTY;
	uvm_reg_field MA_TCN_FIFO_0_RSVD0[512];
	uvm_reg_field MA_TCN_FIFO_0_PORT[512];
	uvm_reg_field MA_TCN_FIFO_0_MAC_ADDRESS[512];
	uvm_reg_field MA_TCN_FIFO_1_RSVD0[512];
	uvm_reg_field MA_TCN_FIFO_1_L2_DOMAIN[512];
	uvm_reg_field MA_TCN_FIFO_1_VID[512];
	uvm_reg_field MA_TCN_DEQUEUE_RSVD0;
	rand uvm_reg_field MA_TCN_DEQUEUE_READY;
	rand uvm_reg_field READY;
	uvm_reg_field MA_TCN_DATA_0_RSVD0;
	uvm_reg_field MA_TCN_DATA_0_VALID;
	uvm_reg_field VALID;
	uvm_reg_field MA_TCN_DATA_0_PORT;
	uvm_reg_field MA_TCN_DATA_0_MAC_ADDRESS;
	uvm_reg_field MA_TCN_DATA_1_RSVD0;
	uvm_reg_field MA_TCN_DATA_1_L2_DOMAIN;
	uvm_reg_field MA_TCN_DATA_1_VID;
	uvm_reg_field MA_TCN_PTR_HEAD_RSVD0;
	rand uvm_reg_field MA_TCN_PTR_HEAD_HEAD;
	rand uvm_reg_field HEAD;
	uvm_reg_field MA_TCN_PTR_TAIL_RSVD0;
	rand uvm_reg_field MA_TCN_PTR_TAIL_TAIL;
	rand uvm_reg_field TAIL;
	uvm_reg_field MA_TCN_IP_RSVD0;
	rand uvm_reg_field MA_TCN_IP_PENDING_EVENTS;
	rand uvm_reg_field MA_TCN_IP_TCN_OVERFLOW;
	uvm_reg_field MA_TCN_IM_RSVD0;
	rand uvm_reg_field MA_TCN_IM_PENDING_EVENTS;
	rand uvm_reg_field MA_TCN_IM_TCN_OVERFLOW;
	uvm_reg_field MA_TCN_WM_RSVD0[17];
	rand uvm_reg_field MA_TCN_WM_WM[17];
	rand uvm_reg_field WM[17];
	uvm_reg_field MA_TCN_USAGE_RSVD0[17];
	rand uvm_reg_field MA_TCN_USAGE_USAGE[17];
	rand uvm_reg_field USAGE[17];

	function new(string name = "trig_apply_misc");
		super.new(name, build_coverage(UVM_NO_COVERAGE));
	endfunction: new

   virtual function void build();
      this.default_map = create_map("", 0, 8, UVM_LITTLE_ENDIAN, 0);
      foreach (this.TRIGGER_RATE_LIM_CFG_2[i]) begin
         int J = i;
         this.TRIGGER_RATE_LIM_CFG_2[J] = ral_reg_trig_apply_misc_TRIGGER_RATE_LIM_CFG_2::type_id::create($psprintf("TRIGGER_RATE_LIM_CFG_2[%0d]",J),,get_full_name());
         this.TRIGGER_RATE_LIM_CFG_2[J].configure(this, null, "");
         this.TRIGGER_RATE_LIM_CFG_2[J].build();
         this.TRIGGER_RATE_LIM_CFG_2[J].add_hdl_path('{

            '{$psprintf("TRIGGER_RATE_LIM_CFG_2[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.TRIGGER_RATE_LIM_CFG_2[J], `UVM_REG_ADDR_WIDTH'h0+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.TRIGGER_RATE_LIM_CFG_2_RSVD0[J] = this.TRIGGER_RATE_LIM_CFG_2[J].RSVD0;
			this.TRIGGER_RATE_LIM_CFG_2_DROP_MASK[J] = this.TRIGGER_RATE_LIM_CFG_2[J].DROP_MASK;
			this.DROP_MASK[J] = this.TRIGGER_RATE_LIM_CFG_2[J].DROP_MASK;
      end
      foreach (this.TRIGGER_ACTION_METADATA_MASK[i,j]) begin
         int J = i;
         int K = j;
         this.TRIGGER_ACTION_METADATA_MASK[J][K] = ral_reg_trig_apply_misc_TRIGGER_ACTION_METADATA_MASK::type_id::create($psprintf("TRIGGER_ACTION_METADATA_MASK[%0d][%0d]",J,K),,get_full_name());
         this.TRIGGER_ACTION_METADATA_MASK[J][K].configure(this, null, "");
         this.TRIGGER_ACTION_METADATA_MASK[J][K].build();
         this.TRIGGER_ACTION_METADATA_MASK[J][K].add_hdl_path('{

            '{$psprintf("TRIGGER_ACTION_METADATA_MASK[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.TRIGGER_ACTION_METADATA_MASK[J][K], `UVM_REG_ADDR_WIDTH'h80+J*`UVM_REG_ADDR_WIDTH'h20+K*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.TRIGGER_ACTION_METADATA_MASK_METADATA_MASK[J][K] = this.TRIGGER_ACTION_METADATA_MASK[J][K].METADATA_MASK;
			this.METADATA_MASK[J][K] = this.TRIGGER_ACTION_METADATA_MASK[J][K].METADATA_MASK;
      end
      foreach (this.TRIGGER_IP[i]) begin
         int J = i;
         this.TRIGGER_IP[J] = ral_reg_trig_apply_misc_TRIGGER_IP::type_id::create($psprintf("TRIGGER_IP[%0d]",J),,get_full_name());
         this.TRIGGER_IP[J].configure(this, null, "");
         this.TRIGGER_IP[J].build();
         this.TRIGGER_IP[J].add_hdl_path('{

            '{$psprintf("TRIGGER_IP[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.TRIGGER_IP[J], `UVM_REG_ADDR_WIDTH'hC0+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.TRIGGER_IP_RSVD0[J] = this.TRIGGER_IP[J].RSVD0;
			this.TRIGGER_IP_PENDING[J] = this.TRIGGER_IP[J].PENDING;
			this.PENDING[J] = this.TRIGGER_IP[J].PENDING;
      end
      foreach (this.TRIGGER_IM[i]) begin
         int J = i;
         this.TRIGGER_IM[J] = ral_reg_trig_apply_misc_TRIGGER_IM::type_id::create($psprintf("TRIGGER_IM[%0d]",J),,get_full_name());
         this.TRIGGER_IM[J].configure(this, null, "");
         this.TRIGGER_IM[J].build();
         this.TRIGGER_IM[J].add_hdl_path('{

            '{$psprintf("TRIGGER_IM[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.TRIGGER_IM[J], `UVM_REG_ADDR_WIDTH'hD0+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.TRIGGER_IM_RSVD0[J] = this.TRIGGER_IM[J].RSVD0;
			this.TRIGGER_IM_MASK[J] = this.TRIGGER_IM[J].MASK;
			this.MASK[J] = this.TRIGGER_IM[J].MASK;
      end
      this.TRIGGER_RATE_LIM_EMPTY = ral_reg_trig_apply_misc_TRIGGER_RATE_LIM_EMPTY::type_id::create("TRIGGER_RATE_LIM_EMPTY",,get_full_name());
      this.TRIGGER_RATE_LIM_EMPTY.configure(this, null, "");
      this.TRIGGER_RATE_LIM_EMPTY.build();
         this.TRIGGER_RATE_LIM_EMPTY.add_hdl_path('{

            '{"TRIGGER_RATE_LIM_EMPTY", -1, -1}
         });
      this.default_map.add_reg(this.TRIGGER_RATE_LIM_EMPTY, `UVM_REG_ADDR_WIDTH'hE0, "RO", 0);
		this.TRIGGER_RATE_LIM_EMPTY_RSVD0 = this.TRIGGER_RATE_LIM_EMPTY.RSVD0;
		this.TRIGGER_RATE_LIM_EMPTY_EMPTY = this.TRIGGER_RATE_LIM_EMPTY.EMPTY;
		this.EMPTY = this.TRIGGER_RATE_LIM_EMPTY.EMPTY;
      foreach (this.MA_TCN_FIFO_0[i]) begin
         int J = i;
         this.MA_TCN_FIFO_0[J] = ral_reg_trig_apply_misc_MA_TCN_FIFO_0::type_id::create($psprintf("MA_TCN_FIFO_0[%0d]",J),,get_full_name());
         this.MA_TCN_FIFO_0[J].configure(this, null, "");
         this.MA_TCN_FIFO_0[J].build();
         this.MA_TCN_FIFO_0[J].add_hdl_path('{

            '{$psprintf("MA_TCN_FIFO_0[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.MA_TCN_FIFO_0[J], `UVM_REG_ADDR_WIDTH'h100+J*`UVM_REG_ADDR_WIDTH'h8, "RO", 0);
			this.MA_TCN_FIFO_0_RSVD0[J] = this.MA_TCN_FIFO_0[J].RSVD0;
			this.MA_TCN_FIFO_0_PORT[J] = this.MA_TCN_FIFO_0[J].PORT;
			this.MA_TCN_FIFO_0_MAC_ADDRESS[J] = this.MA_TCN_FIFO_0[J].MAC_ADDRESS;
      end
      foreach (this.MA_TCN_FIFO_1[i]) begin
         int J = i;
         this.MA_TCN_FIFO_1[J] = ral_reg_trig_apply_misc_MA_TCN_FIFO_1::type_id::create($psprintf("MA_TCN_FIFO_1[%0d]",J),,get_full_name());
         this.MA_TCN_FIFO_1[J].configure(this, null, "");
         this.MA_TCN_FIFO_1[J].build();
         this.MA_TCN_FIFO_1[J].add_hdl_path('{

            '{$psprintf("MA_TCN_FIFO_1[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.MA_TCN_FIFO_1[J], `UVM_REG_ADDR_WIDTH'h1100+J*`UVM_REG_ADDR_WIDTH'h8, "RO", 0);
			this.MA_TCN_FIFO_1_RSVD0[J] = this.MA_TCN_FIFO_1[J].RSVD0;
			this.MA_TCN_FIFO_1_L2_DOMAIN[J] = this.MA_TCN_FIFO_1[J].L2_DOMAIN;
			this.MA_TCN_FIFO_1_VID[J] = this.MA_TCN_FIFO_1[J].VID;
      end
      this.MA_TCN_DEQUEUE = ral_reg_trig_apply_misc_MA_TCN_DEQUEUE::type_id::create("MA_TCN_DEQUEUE",,get_full_name());
      this.MA_TCN_DEQUEUE.configure(this, null, "");
      this.MA_TCN_DEQUEUE.build();
         this.MA_TCN_DEQUEUE.add_hdl_path('{

            '{"MA_TCN_DEQUEUE", -1, -1}
         });
      this.default_map.add_reg(this.MA_TCN_DEQUEUE, `UVM_REG_ADDR_WIDTH'h2100, "RW", 0);
		this.MA_TCN_DEQUEUE_RSVD0 = this.MA_TCN_DEQUEUE.RSVD0;
		this.MA_TCN_DEQUEUE_READY = this.MA_TCN_DEQUEUE.READY;
		this.READY = this.MA_TCN_DEQUEUE.READY;
      this.MA_TCN_DATA_0 = ral_reg_trig_apply_misc_MA_TCN_DATA_0::type_id::create("MA_TCN_DATA_0",,get_full_name());
      this.MA_TCN_DATA_0.configure(this, null, "");
      this.MA_TCN_DATA_0.build();
         this.MA_TCN_DATA_0.add_hdl_path('{

            '{"MA_TCN_DATA_0", -1, -1}
         });
      this.default_map.add_reg(this.MA_TCN_DATA_0, `UVM_REG_ADDR_WIDTH'h2108, "RO", 0);
		this.MA_TCN_DATA_0_RSVD0 = this.MA_TCN_DATA_0.RSVD0;
		this.MA_TCN_DATA_0_VALID = this.MA_TCN_DATA_0.VALID;
		this.VALID = this.MA_TCN_DATA_0.VALID;
		this.MA_TCN_DATA_0_PORT = this.MA_TCN_DATA_0.PORT;
		this.MA_TCN_DATA_0_MAC_ADDRESS = this.MA_TCN_DATA_0.MAC_ADDRESS;
      this.MA_TCN_DATA_1 = ral_reg_trig_apply_misc_MA_TCN_DATA_1::type_id::create("MA_TCN_DATA_1",,get_full_name());
      this.MA_TCN_DATA_1.configure(this, null, "");
      this.MA_TCN_DATA_1.build();
         this.MA_TCN_DATA_1.add_hdl_path('{

            '{"MA_TCN_DATA_1", -1, -1}
         });
      this.default_map.add_reg(this.MA_TCN_DATA_1, `UVM_REG_ADDR_WIDTH'h2110, "RO", 0);
		this.MA_TCN_DATA_1_RSVD0 = this.MA_TCN_DATA_1.RSVD0;
		this.MA_TCN_DATA_1_L2_DOMAIN = this.MA_TCN_DATA_1.L2_DOMAIN;
		this.MA_TCN_DATA_1_VID = this.MA_TCN_DATA_1.VID;
      this.MA_TCN_PTR_HEAD = ral_reg_trig_apply_misc_MA_TCN_PTR_HEAD::type_id::create("MA_TCN_PTR_HEAD",,get_full_name());
      this.MA_TCN_PTR_HEAD.configure(this, null, "");
      this.MA_TCN_PTR_HEAD.build();
         this.MA_TCN_PTR_HEAD.add_hdl_path('{

            '{"MA_TCN_PTR_HEAD", -1, -1}
         });
      this.default_map.add_reg(this.MA_TCN_PTR_HEAD, `UVM_REG_ADDR_WIDTH'h2118, "RW", 0);
		this.MA_TCN_PTR_HEAD_RSVD0 = this.MA_TCN_PTR_HEAD.RSVD0;
		this.MA_TCN_PTR_HEAD_HEAD = this.MA_TCN_PTR_HEAD.HEAD;
		this.HEAD = this.MA_TCN_PTR_HEAD.HEAD;
      this.MA_TCN_PTR_TAIL = ral_reg_trig_apply_misc_MA_TCN_PTR_TAIL::type_id::create("MA_TCN_PTR_TAIL",,get_full_name());
      this.MA_TCN_PTR_TAIL.configure(this, null, "");
      this.MA_TCN_PTR_TAIL.build();
         this.MA_TCN_PTR_TAIL.add_hdl_path('{

            '{"MA_TCN_PTR_TAIL", -1, -1}
         });
      this.default_map.add_reg(this.MA_TCN_PTR_TAIL, `UVM_REG_ADDR_WIDTH'h2120, "RW", 0);
		this.MA_TCN_PTR_TAIL_RSVD0 = this.MA_TCN_PTR_TAIL.RSVD0;
		this.MA_TCN_PTR_TAIL_TAIL = this.MA_TCN_PTR_TAIL.TAIL;
		this.TAIL = this.MA_TCN_PTR_TAIL.TAIL;
      this.MA_TCN_IP = ral_reg_trig_apply_misc_MA_TCN_IP::type_id::create("MA_TCN_IP",,get_full_name());
      this.MA_TCN_IP.configure(this, null, "");
      this.MA_TCN_IP.build();
         this.MA_TCN_IP.add_hdl_path('{

            '{"MA_TCN_IP", -1, -1}
         });
      this.default_map.add_reg(this.MA_TCN_IP, `UVM_REG_ADDR_WIDTH'h2128, "RW", 0);
		this.MA_TCN_IP_RSVD0 = this.MA_TCN_IP.RSVD0;
		this.MA_TCN_IP_PENDING_EVENTS = this.MA_TCN_IP.PENDING_EVENTS;
		this.MA_TCN_IP_TCN_OVERFLOW = this.MA_TCN_IP.TCN_OVERFLOW;
      this.MA_TCN_IM = ral_reg_trig_apply_misc_MA_TCN_IM::type_id::create("MA_TCN_IM",,get_full_name());
      this.MA_TCN_IM.configure(this, null, "");
      this.MA_TCN_IM.build();
         this.MA_TCN_IM.add_hdl_path('{

            '{"MA_TCN_IM", -1, -1}
         });
      this.default_map.add_reg(this.MA_TCN_IM, `UVM_REG_ADDR_WIDTH'h2130, "RW", 0);
		this.MA_TCN_IM_RSVD0 = this.MA_TCN_IM.RSVD0;
		this.MA_TCN_IM_PENDING_EVENTS = this.MA_TCN_IM.PENDING_EVENTS;
		this.MA_TCN_IM_TCN_OVERFLOW = this.MA_TCN_IM.TCN_OVERFLOW;
      foreach (this.MA_TCN_WM[i]) begin
         int J = i;
         this.MA_TCN_WM[J] = ral_reg_trig_apply_misc_MA_TCN_WM::type_id::create($psprintf("MA_TCN_WM[%0d]",J),,get_full_name());
         this.MA_TCN_WM[J].configure(this, null, "");
         this.MA_TCN_WM[J].build();
         this.MA_TCN_WM[J].add_hdl_path('{

            '{$psprintf("MA_TCN_WM[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.MA_TCN_WM[J], `UVM_REG_ADDR_WIDTH'h2140+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.MA_TCN_WM_RSVD0[J] = this.MA_TCN_WM[J].RSVD0;
			this.MA_TCN_WM_WM[J] = this.MA_TCN_WM[J].WM;
			this.WM[J] = this.MA_TCN_WM[J].WM;
      end
      foreach (this.MA_TCN_USAGE[i]) begin
         int J = i;
         this.MA_TCN_USAGE[J] = ral_reg_trig_apply_misc_MA_TCN_USAGE::type_id::create($psprintf("MA_TCN_USAGE[%0d]",J),,get_full_name());
         this.MA_TCN_USAGE[J].configure(this, null, "");
         this.MA_TCN_USAGE[J].build();
         this.MA_TCN_USAGE[J].add_hdl_path('{

            '{$psprintf("MA_TCN_USAGE[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.MA_TCN_USAGE[J], `UVM_REG_ADDR_WIDTH'h21D0+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.MA_TCN_USAGE_RSVD0[J] = this.MA_TCN_USAGE[J].RSVD0;
			this.MA_TCN_USAGE_USAGE[J] = this.MA_TCN_USAGE[J].USAGE;
			this.USAGE[J] = this.MA_TCN_USAGE[J].USAGE;
      end
   endfunction : build

	`uvm_object_utils(ral_block_trig_apply_misc)

endclass : ral_block_trig_apply_misc



`endif
