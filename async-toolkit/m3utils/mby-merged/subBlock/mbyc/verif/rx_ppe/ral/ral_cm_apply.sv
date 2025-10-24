// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

`ifndef RAL_CM_APPLY
`define RAL_CM_APPLY

import uvm_pkg::*;

class ral_reg_cm_apply_CM_APPLY_TX_SOFTDROP_CFG extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field SOFT_DROP_ON_PRIVATE;
	rand uvm_reg_field SOFT_DROP_ON_SMP_FREE;

	function new(string name = "cm_apply_CM_APPLY_TX_SOFTDROP_CFG");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 62, 2, "RO", 0, 62'h0, 1, 0, 0);
      this.SOFT_DROP_ON_PRIVATE = uvm_reg_field::type_id::create("SOFT_DROP_ON_PRIVATE",,get_full_name());
      this.SOFT_DROP_ON_PRIVATE.configure(this, 1, 1, "RW", 0, 1'h0, 1, 0, 0);
      this.SOFT_DROP_ON_SMP_FREE = uvm_reg_field::type_id::create("SOFT_DROP_ON_SMP_FREE",,get_full_name());
      this.SOFT_DROP_ON_SMP_FREE.configure(this, 1, 0, "RW", 0, 1'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_cm_apply_CM_APPLY_TX_SOFTDROP_CFG)

endclass : ral_reg_cm_apply_CM_APPLY_TX_SOFTDROP_CFG


class ral_reg_cm_apply_CM_APPLY_TX_TC_STATE extends uvm_reg;
	uvm_reg_field RSVD0;
	uvm_reg_field QUEUE_DEPTH;
	uvm_reg_field OVER_SMP_FREE2;
	uvm_reg_field OVER_SMP_FREE;
	uvm_reg_field TX_HOG;
	uvm_reg_field TX_PRIVATE;

	function new(string name = "cm_apply_CM_APPLY_TX_TC_STATE");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 45, 19, "RO", 0, 45'h0, 1, 0, 0);
      this.QUEUE_DEPTH = uvm_reg_field::type_id::create("QUEUE_DEPTH",,get_full_name());
      this.QUEUE_DEPTH.configure(this, 15, 4, "RO", 1, 15'h0, 1, 0, 0);
      this.OVER_SMP_FREE2 = uvm_reg_field::type_id::create("OVER_SMP_FREE2",,get_full_name());
      this.OVER_SMP_FREE2.configure(this, 1, 3, "RO", 1, 1'h0, 1, 0, 0);
      this.OVER_SMP_FREE = uvm_reg_field::type_id::create("OVER_SMP_FREE",,get_full_name());
      this.OVER_SMP_FREE.configure(this, 1, 2, "RO", 1, 1'h0, 1, 0, 0);
      this.TX_HOG = uvm_reg_field::type_id::create("TX_HOG",,get_full_name());
      this.TX_HOG.configure(this, 1, 1, "RO", 1, 1'h0, 1, 0, 0);
      this.TX_PRIVATE = uvm_reg_field::type_id::create("TX_PRIVATE",,get_full_name());
      this.TX_PRIVATE.configure(this, 1, 0, "RO", 1, 1'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_cm_apply_CM_APPLY_TX_TC_STATE)

endclass : ral_reg_cm_apply_CM_APPLY_TX_TC_STATE


class ral_reg_cm_apply_CM_APPLY_TX_TC_QCN_WM_THRESHOLD extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field WM_THRESHOLD;

	function new(string name = "cm_apply_CM_APPLY_TX_TC_QCN_WM_THRESHOLD");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 49, 15, "RO", 0, 49'h0, 1, 0, 0);
      this.WM_THRESHOLD = uvm_reg_field::type_id::create("WM_THRESHOLD",,get_full_name());
      this.WM_THRESHOLD.configure(this, 15, 0, "RW", 0, 15'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_cm_apply_CM_APPLY_TX_TC_QCN_WM_THRESHOLD)

endclass : ral_reg_cm_apply_CM_APPLY_TX_TC_QCN_WM_THRESHOLD


class ral_reg_cm_apply_CM_APPLY_RX_SMP_STATE extends uvm_reg;
	uvm_reg_field RSVD0;
	uvm_reg_field RX_HOG;
	uvm_reg_field RX_PRIVATE;

	function new(string name = "cm_apply_CM_APPLY_RX_SMP_STATE");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 62, 2, "RO", 0, 62'h0, 1, 0, 0);
      this.RX_HOG = uvm_reg_field::type_id::create("RX_HOG",,get_full_name());
      this.RX_HOG.configure(this, 1, 1, "RO", 1, 1'h0, 1, 0, 0);
      this.RX_PRIVATE = uvm_reg_field::type_id::create("RX_PRIVATE",,get_full_name());
      this.RX_PRIVATE.configure(this, 1, 0, "RO", 1, 1'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_cm_apply_CM_APPLY_RX_SMP_STATE)

endclass : ral_reg_cm_apply_CM_APPLY_RX_SMP_STATE


class ral_reg_cm_apply_CM_APPLY_MIRROR_PROFILE_TABLE extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field PORT;

	function new(string name = "cm_apply_CM_APPLY_MIRROR_PROFILE_TABLE");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 59, 5, "RO", 0, 59'h0, 1, 0, 0);
      this.PORT = uvm_reg_field::type_id::create("PORT",,get_full_name());
      this.PORT.configure(this, 5, 0, "RW", 0, 5'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_cm_apply_CM_APPLY_MIRROR_PROFILE_TABLE)

endclass : ral_reg_cm_apply_CM_APPLY_MIRROR_PROFILE_TABLE


class ral_reg_cm_apply_CM_APPLY_DROP_COUNT extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field FRAMES;

	function new(string name = "cm_apply_CM_APPLY_DROP_COUNT");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 16, 48, "RO", 0, 16'h0, 1, 0, 1);
      this.FRAMES = uvm_reg_field::type_id::create("FRAMES",,get_full_name());
      this.FRAMES.configure(this, 48, 0, "RW", 1, 48'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_cm_apply_CM_APPLY_DROP_COUNT)

endclass : ral_reg_cm_apply_CM_APPLY_DROP_COUNT


class ral_reg_cm_apply_CM_APPLY_LOOPBACK_SUPPRESS extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field GLORT_MASK;
	rand uvm_reg_field GLORT;

	function new(string name = "cm_apply_CM_APPLY_LOOPBACK_SUPPRESS");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 32, 32, "RO", 0, 32'h0, 1, 0, 1);
      this.GLORT_MASK = uvm_reg_field::type_id::create("GLORT_MASK",,get_full_name());
      this.GLORT_MASK.configure(this, 16, 16, "RW", 0, 16'h0, 1, 0, 1);
      this.GLORT = uvm_reg_field::type_id::create("GLORT",,get_full_name());
      this.GLORT.configure(this, 16, 0, "RW", 0, 16'hffff, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_cm_apply_CM_APPLY_LOOPBACK_SUPPRESS)

endclass : ral_reg_cm_apply_CM_APPLY_LOOPBACK_SUPPRESS


class ral_reg_cm_apply_CM_APPLY_SOFTDROP_CFG extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field JITTER_BITS;

	function new(string name = "cm_apply_CM_APPLY_SOFTDROP_CFG");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 61, 3, "RO", 0, 61'h0, 1, 0, 0);
      this.JITTER_BITS = uvm_reg_field::type_id::create("JITTER_BITS",,get_full_name());
      this.JITTER_BITS.configure(this, 3, 0, "RW", 0, 3'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_cm_apply_CM_APPLY_SOFTDROP_CFG)

endclass : ral_reg_cm_apply_CM_APPLY_SOFTDROP_CFG


class ral_reg_cm_apply_CM_APPLY_SOFTDROP_STATE extends uvm_reg;
	uvm_reg_field RSVD0;
	uvm_reg_field OVER_LIMIT;
	uvm_reg_field USAGE_OVER_LIMIT;

	function new(string name = "cm_apply_CM_APPLY_SOFTDROP_STATE");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 55, 9, "RO", 0, 55'h0, 1, 0, 0);
      this.OVER_LIMIT = uvm_reg_field::type_id::create("OVER_LIMIT",,get_full_name());
      this.OVER_LIMIT.configure(this, 1, 8, "RO", 1, 1'h0, 1, 0, 0);
      this.USAGE_OVER_LIMIT = uvm_reg_field::type_id::create("USAGE_OVER_LIMIT",,get_full_name());
      this.USAGE_OVER_LIMIT.configure(this, 8, 0, "RO", 1, 8'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_cm_apply_CM_APPLY_SOFTDROP_STATE)

endclass : ral_reg_cm_apply_CM_APPLY_SOFTDROP_STATE


class ral_reg_cm_apply_CM_APPLY_TRAP_GLORT extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field TRAP_GLORT;

	function new(string name = "cm_apply_CM_APPLY_TRAP_GLORT");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 48, 16, "RO", 0, 48'h0, 1, 0, 1);
      this.TRAP_GLORT = uvm_reg_field::type_id::create("TRAP_GLORT",,get_full_name());
      this.TRAP_GLORT.configure(this, 16, 0, "RW", 0, 16'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_cm_apply_CM_APPLY_TRAP_GLORT)

endclass : ral_reg_cm_apply_CM_APPLY_TRAP_GLORT


class ral_reg_cm_apply_CM_APPLY_TC_TO_SMP extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field SMP_7;
	rand uvm_reg_field SMP_6;
	rand uvm_reg_field SMP_5;
	rand uvm_reg_field SMP_4;
	rand uvm_reg_field SMP_3;
	rand uvm_reg_field SMP_2;
	rand uvm_reg_field SMP_1;
	rand uvm_reg_field SMP_0;

	function new(string name = "cm_apply_CM_APPLY_TC_TO_SMP");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 56, 8, "RO", 0, 56'h0, 1, 0, 1);
      this.SMP_7 = uvm_reg_field::type_id::create("SMP_7",,get_full_name());
      this.SMP_7.configure(this, 1, 7, "RW", 0, 1'h0, 1, 0, 0);
      this.SMP_6 = uvm_reg_field::type_id::create("SMP_6",,get_full_name());
      this.SMP_6.configure(this, 1, 6, "RW", 0, 1'h0, 1, 0, 0);
      this.SMP_5 = uvm_reg_field::type_id::create("SMP_5",,get_full_name());
      this.SMP_5.configure(this, 1, 5, "RW", 0, 1'h0, 1, 0, 0);
      this.SMP_4 = uvm_reg_field::type_id::create("SMP_4",,get_full_name());
      this.SMP_4.configure(this, 1, 4, "RW", 0, 1'h0, 1, 0, 0);
      this.SMP_3 = uvm_reg_field::type_id::create("SMP_3",,get_full_name());
      this.SMP_3.configure(this, 1, 3, "RW", 0, 1'h0, 1, 0, 0);
      this.SMP_2 = uvm_reg_field::type_id::create("SMP_2",,get_full_name());
      this.SMP_2.configure(this, 1, 2, "RW", 0, 1'h0, 1, 0, 0);
      this.SMP_1 = uvm_reg_field::type_id::create("SMP_1",,get_full_name());
      this.SMP_1.configure(this, 1, 1, "RW", 0, 1'h0, 1, 0, 0);
      this.SMP_0 = uvm_reg_field::type_id::create("SMP_0",,get_full_name());
      this.SMP_0.configure(this, 1, 0, "RW", 0, 1'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_cm_apply_CM_APPLY_TC_TO_SMP)

endclass : ral_reg_cm_apply_CM_APPLY_TC_TO_SMP


class ral_reg_cm_apply_CM_APPLY_MCAST_EPOCH extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field CURRENT;

	function new(string name = "cm_apply_CM_APPLY_MCAST_EPOCH");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 63, 1, "RO", 0, 63'h0, 1, 0, 0);
      this.CURRENT = uvm_reg_field::type_id::create("CURRENT",,get_full_name());
      this.CURRENT.configure(this, 1, 0, "RW", 0, 1'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_cm_apply_CM_APPLY_MCAST_EPOCH)

endclass : ral_reg_cm_apply_CM_APPLY_MCAST_EPOCH


class ral_reg_cm_apply_CM_APPLY_STATE extends uvm_reg;
	uvm_reg_field RSVD0;
	uvm_reg_field RXS_7;
	uvm_reg_field RXS_6;
	uvm_reg_field RXS_5;
	uvm_reg_field RXS_4;
	uvm_reg_field RXS_3;
	uvm_reg_field RXS_2;
	uvm_reg_field RXS_1;
	uvm_reg_field RXS_0;
	uvm_reg_field GLOBAL_EX;

	function new(string name = "cm_apply_CM_APPLY_STATE");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 55, 9, "RO", 0, 55'h0, 1, 0, 0);
      this.RXS_7 = uvm_reg_field::type_id::create("RXS_7",,get_full_name());
      this.RXS_7.configure(this, 1, 8, "RO", 1, 1'h0, 1, 0, 0);
      this.RXS_6 = uvm_reg_field::type_id::create("RXS_6",,get_full_name());
      this.RXS_6.configure(this, 1, 7, "RO", 1, 1'h0, 1, 0, 0);
      this.RXS_5 = uvm_reg_field::type_id::create("RXS_5",,get_full_name());
      this.RXS_5.configure(this, 1, 6, "RO", 1, 1'h0, 1, 0, 0);
      this.RXS_4 = uvm_reg_field::type_id::create("RXS_4",,get_full_name());
      this.RXS_4.configure(this, 1, 5, "RO", 1, 1'h0, 1, 0, 0);
      this.RXS_3 = uvm_reg_field::type_id::create("RXS_3",,get_full_name());
      this.RXS_3.configure(this, 1, 4, "RO", 1, 1'h0, 1, 0, 0);
      this.RXS_2 = uvm_reg_field::type_id::create("RXS_2",,get_full_name());
      this.RXS_2.configure(this, 1, 3, "RO", 1, 1'h0, 1, 0, 0);
      this.RXS_1 = uvm_reg_field::type_id::create("RXS_1",,get_full_name());
      this.RXS_1.configure(this, 1, 2, "RO", 1, 1'h0, 1, 0, 0);
      this.RXS_0 = uvm_reg_field::type_id::create("RXS_0",,get_full_name());
      this.RXS_0.configure(this, 1, 1, "RO", 1, 1'h0, 1, 0, 0);
      this.GLOBAL_EX = uvm_reg_field::type_id::create("GLOBAL_EX",,get_full_name());
      this.GLOBAL_EX.configure(this, 1, 0, "RO", 1, 1'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_cm_apply_CM_APPLY_STATE)

endclass : ral_reg_cm_apply_CM_APPLY_STATE


class ral_reg_cm_apply_CM_APPLY_CPU_TRAP_MASK extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field DEST_MASK;

	function new(string name = "cm_apply_CM_APPLY_CPU_TRAP_MASK");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 40, 24, "RO", 0, 40'h0, 1, 0, 1);
      this.DEST_MASK = uvm_reg_field::type_id::create("DEST_MASK",,get_full_name());
      this.DEST_MASK.configure(this, 24, 0, "RW", 0, 24'h1, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_cm_apply_CM_APPLY_CPU_TRAP_MASK)

endclass : ral_reg_cm_apply_CM_APPLY_CPU_TRAP_MASK


class ral_reg_cm_apply_CM_APPLY_LOG_MIRROR_PROFILE extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field TRIGGER;
	rand uvm_reg_field TTL;
	rand uvm_reg_field ICMP;
	rand uvm_reg_field ARP_REDIRECT;
	rand uvm_reg_field RESERVED_MAC;
	rand uvm_reg_field FFU;

	function new(string name = "cm_apply_CM_APPLY_LOG_MIRROR_PROFILE");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 28, 36, "RO", 0, 28'h0, 1, 0, 0);
      this.TRIGGER = uvm_reg_field::type_id::create("TRIGGER",,get_full_name());
      this.TRIGGER.configure(this, 6, 30, "RW", 0, 6'h0, 1, 0, 0);
      this.TTL = uvm_reg_field::type_id::create("TTL",,get_full_name());
      this.TTL.configure(this, 6, 24, "RW", 0, 6'h0, 1, 0, 0);
      this.ICMP = uvm_reg_field::type_id::create("ICMP",,get_full_name());
      this.ICMP.configure(this, 6, 18, "RW", 0, 6'h0, 1, 0, 0);
      this.ARP_REDIRECT = uvm_reg_field::type_id::create("ARP_REDIRECT",,get_full_name());
      this.ARP_REDIRECT.configure(this, 6, 12, "RW", 0, 6'h0, 1, 0, 0);
      this.RESERVED_MAC = uvm_reg_field::type_id::create("RESERVED_MAC",,get_full_name());
      this.RESERVED_MAC.configure(this, 6, 6, "RW", 0, 6'h0, 1, 0, 0);
      this.FFU = uvm_reg_field::type_id::create("FFU",,get_full_name());
      this.FFU.configure(this, 6, 0, "RW", 0, 6'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_cm_apply_CM_APPLY_LOG_MIRROR_PROFILE)

endclass : ral_reg_cm_apply_CM_APPLY_LOG_MIRROR_PROFILE


class ral_reg_cm_apply_CM_APPLY_QCN_CFG extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field QUEUE_DEPTH_MODE;
	rand uvm_reg_field QUEUE_DEPTH_START_BIT;
	rand uvm_reg_field SELECT_COMP;
	rand uvm_reg_field SAMPLE_RATE_EXP;

	function new(string name = "cm_apply_CM_APPLY_QCN_CFG");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 54, 10, "RO", 0, 54'h0, 1, 0, 0);
      this.QUEUE_DEPTH_MODE = uvm_reg_field::type_id::create("QUEUE_DEPTH_MODE",,get_full_name());
      this.QUEUE_DEPTH_MODE.configure(this, 1, 9, "RW", 0, 1'h0, 1, 0, 0);
      this.QUEUE_DEPTH_START_BIT = uvm_reg_field::type_id::create("QUEUE_DEPTH_START_BIT",,get_full_name());
      this.QUEUE_DEPTH_START_BIT.configure(this, 3, 6, "RW", 0, 3'h0, 1, 0, 0);
      this.SELECT_COMP = uvm_reg_field::type_id::create("SELECT_COMP",,get_full_name());
      this.SELECT_COMP.configure(this, 1, 5, "RW", 0, 1'h0, 1, 0, 0);
      this.SAMPLE_RATE_EXP = uvm_reg_field::type_id::create("SAMPLE_RATE_EXP",,get_full_name());
      this.SAMPLE_RATE_EXP.configure(this, 5, 0, "RW", 0, 5'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_cm_apply_CM_APPLY_QCN_CFG)

endclass : ral_reg_cm_apply_CM_APPLY_QCN_CFG


class ral_block_cm_apply extends uvm_reg_block;
	rand ral_reg_cm_apply_CM_APPLY_TX_SOFTDROP_CFG CM_APPLY_TX_SOFTDROP_CFG[64][0:7];
	rand ral_reg_cm_apply_CM_APPLY_TX_TC_STATE CM_APPLY_TX_TC_STATE[64][0:7];
	rand ral_reg_cm_apply_CM_APPLY_TX_TC_QCN_WM_THRESHOLD CM_APPLY_TX_TC_QCN_WM_THRESHOLD[64][0:7];
	rand ral_reg_cm_apply_CM_APPLY_RX_SMP_STATE CM_APPLY_RX_SMP_STATE[32][0:1];
	rand ral_reg_cm_apply_CM_APPLY_MIRROR_PROFILE_TABLE CM_APPLY_MIRROR_PROFILE_TABLE[64];
	rand ral_reg_cm_apply_CM_APPLY_DROP_COUNT CM_APPLY_DROP_COUNT[32];
	rand ral_reg_cm_apply_CM_APPLY_LOOPBACK_SUPPRESS CM_APPLY_LOOPBACK_SUPPRESS[32];
	rand ral_reg_cm_apply_CM_APPLY_SOFTDROP_CFG CM_APPLY_SOFTDROP_CFG[8];
	rand ral_reg_cm_apply_CM_APPLY_SOFTDROP_STATE CM_APPLY_SOFTDROP_STATE[8];
	rand ral_reg_cm_apply_CM_APPLY_TRAP_GLORT CM_APPLY_TRAP_GLORT[16];
	rand ral_reg_cm_apply_CM_APPLY_TC_TO_SMP CM_APPLY_TC_TO_SMP;
	rand ral_reg_cm_apply_CM_APPLY_MCAST_EPOCH CM_APPLY_MCAST_EPOCH;
	rand ral_reg_cm_apply_CM_APPLY_STATE CM_APPLY_STATE;
	rand ral_reg_cm_apply_CM_APPLY_CPU_TRAP_MASK CM_APPLY_CPU_TRAP_MASK;
	rand ral_reg_cm_apply_CM_APPLY_LOG_MIRROR_PROFILE CM_APPLY_LOG_MIRROR_PROFILE;
	rand ral_reg_cm_apply_CM_APPLY_QCN_CFG CM_APPLY_QCN_CFG;
	uvm_reg_field CM_APPLY_TX_SOFTDROP_CFG_RSVD0[64][0:7];
	rand uvm_reg_field CM_APPLY_TX_SOFTDROP_CFG_SOFT_DROP_ON_PRIVATE[64][0:7];
	rand uvm_reg_field SOFT_DROP_ON_PRIVATE[64][0:7];
	rand uvm_reg_field CM_APPLY_TX_SOFTDROP_CFG_SOFT_DROP_ON_SMP_FREE[64][0:7];
	rand uvm_reg_field SOFT_DROP_ON_SMP_FREE[64][0:7];
	uvm_reg_field CM_APPLY_TX_TC_STATE_RSVD0[64][0:7];
	uvm_reg_field CM_APPLY_TX_TC_STATE_QUEUE_DEPTH[64][0:7];
	uvm_reg_field QUEUE_DEPTH[64][0:7];
	uvm_reg_field CM_APPLY_TX_TC_STATE_OVER_SMP_FREE2[64][0:7];
	uvm_reg_field OVER_SMP_FREE2[64][0:7];
	uvm_reg_field CM_APPLY_TX_TC_STATE_OVER_SMP_FREE[64][0:7];
	uvm_reg_field OVER_SMP_FREE[64][0:7];
	uvm_reg_field CM_APPLY_TX_TC_STATE_TX_HOG[64][0:7];
	uvm_reg_field TX_HOG[64][0:7];
	uvm_reg_field CM_APPLY_TX_TC_STATE_TX_PRIVATE[64][0:7];
	uvm_reg_field TX_PRIVATE[64][0:7];
	uvm_reg_field CM_APPLY_TX_TC_QCN_WM_THRESHOLD_RSVD0[64][0:7];
	rand uvm_reg_field CM_APPLY_TX_TC_QCN_WM_THRESHOLD_WM_THRESHOLD[64][0:7];
	rand uvm_reg_field WM_THRESHOLD[64][0:7];
	uvm_reg_field CM_APPLY_RX_SMP_STATE_RSVD0[32][0:1];
	uvm_reg_field CM_APPLY_RX_SMP_STATE_RX_HOG[32][0:1];
	uvm_reg_field RX_HOG[32][0:1];
	uvm_reg_field CM_APPLY_RX_SMP_STATE_RX_PRIVATE[32][0:1];
	uvm_reg_field RX_PRIVATE[32][0:1];
	uvm_reg_field CM_APPLY_MIRROR_PROFILE_TABLE_RSVD0[64];
	rand uvm_reg_field CM_APPLY_MIRROR_PROFILE_TABLE_PORT[64];
	rand uvm_reg_field PORT[64];
	uvm_reg_field CM_APPLY_DROP_COUNT_RSVD0[32];
	rand uvm_reg_field CM_APPLY_DROP_COUNT_FRAMES[32];
	rand uvm_reg_field FRAMES[32];
	uvm_reg_field CM_APPLY_LOOPBACK_SUPPRESS_RSVD0[32];
	rand uvm_reg_field CM_APPLY_LOOPBACK_SUPPRESS_GLORT_MASK[32];
	rand uvm_reg_field GLORT_MASK[32];
	rand uvm_reg_field CM_APPLY_LOOPBACK_SUPPRESS_GLORT[32];
	rand uvm_reg_field GLORT[32];
	uvm_reg_field CM_APPLY_SOFTDROP_CFG_RSVD0[8];
	rand uvm_reg_field CM_APPLY_SOFTDROP_CFG_JITTER_BITS[8];
	rand uvm_reg_field JITTER_BITS[8];
	uvm_reg_field CM_APPLY_SOFTDROP_STATE_RSVD0[8];
	uvm_reg_field CM_APPLY_SOFTDROP_STATE_OVER_LIMIT[8];
	uvm_reg_field OVER_LIMIT[8];
	uvm_reg_field CM_APPLY_SOFTDROP_STATE_USAGE_OVER_LIMIT[8];
	uvm_reg_field USAGE_OVER_LIMIT[8];
	uvm_reg_field CM_APPLY_TRAP_GLORT_RSVD0[16];
	rand uvm_reg_field CM_APPLY_TRAP_GLORT_TRAP_GLORT[16];
	rand uvm_reg_field TRAP_GLORT[16];
	uvm_reg_field CM_APPLY_TC_TO_SMP_RSVD0;
	rand uvm_reg_field CM_APPLY_TC_TO_SMP_SMP_7;
	rand uvm_reg_field SMP_7;
	rand uvm_reg_field CM_APPLY_TC_TO_SMP_SMP_6;
	rand uvm_reg_field SMP_6;
	rand uvm_reg_field CM_APPLY_TC_TO_SMP_SMP_5;
	rand uvm_reg_field SMP_5;
	rand uvm_reg_field CM_APPLY_TC_TO_SMP_SMP_4;
	rand uvm_reg_field SMP_4;
	rand uvm_reg_field CM_APPLY_TC_TO_SMP_SMP_3;
	rand uvm_reg_field SMP_3;
	rand uvm_reg_field CM_APPLY_TC_TO_SMP_SMP_2;
	rand uvm_reg_field SMP_2;
	rand uvm_reg_field CM_APPLY_TC_TO_SMP_SMP_1;
	rand uvm_reg_field SMP_1;
	rand uvm_reg_field CM_APPLY_TC_TO_SMP_SMP_0;
	rand uvm_reg_field SMP_0;
	uvm_reg_field CM_APPLY_MCAST_EPOCH_RSVD0;
	rand uvm_reg_field CM_APPLY_MCAST_EPOCH_CURRENT;
	rand uvm_reg_field CURRENT;
	uvm_reg_field CM_APPLY_STATE_RSVD0;
	uvm_reg_field CM_APPLY_STATE_RXS_7;
	uvm_reg_field RXS_7;
	uvm_reg_field CM_APPLY_STATE_RXS_6;
	uvm_reg_field RXS_6;
	uvm_reg_field CM_APPLY_STATE_RXS_5;
	uvm_reg_field RXS_5;
	uvm_reg_field CM_APPLY_STATE_RXS_4;
	uvm_reg_field RXS_4;
	uvm_reg_field CM_APPLY_STATE_RXS_3;
	uvm_reg_field RXS_3;
	uvm_reg_field CM_APPLY_STATE_RXS_2;
	uvm_reg_field RXS_2;
	uvm_reg_field CM_APPLY_STATE_RXS_1;
	uvm_reg_field RXS_1;
	uvm_reg_field CM_APPLY_STATE_RXS_0;
	uvm_reg_field RXS_0;
	uvm_reg_field CM_APPLY_STATE_GLOBAL_EX;
	uvm_reg_field GLOBAL_EX;
	uvm_reg_field CM_APPLY_CPU_TRAP_MASK_RSVD0;
	rand uvm_reg_field CM_APPLY_CPU_TRAP_MASK_DEST_MASK;
	rand uvm_reg_field DEST_MASK;
	uvm_reg_field CM_APPLY_LOG_MIRROR_PROFILE_RSVD0;
	rand uvm_reg_field CM_APPLY_LOG_MIRROR_PROFILE_TRIGGER;
	rand uvm_reg_field TRIGGER;
	rand uvm_reg_field CM_APPLY_LOG_MIRROR_PROFILE_TTL;
	rand uvm_reg_field TTL;
	rand uvm_reg_field CM_APPLY_LOG_MIRROR_PROFILE_ICMP;
	rand uvm_reg_field ICMP;
	rand uvm_reg_field CM_APPLY_LOG_MIRROR_PROFILE_ARP_REDIRECT;
	rand uvm_reg_field ARP_REDIRECT;
	rand uvm_reg_field CM_APPLY_LOG_MIRROR_PROFILE_RESERVED_MAC;
	rand uvm_reg_field RESERVED_MAC;
	rand uvm_reg_field CM_APPLY_LOG_MIRROR_PROFILE_FFU;
	rand uvm_reg_field FFU;
	uvm_reg_field CM_APPLY_QCN_CFG_RSVD0;
	rand uvm_reg_field CM_APPLY_QCN_CFG_QUEUE_DEPTH_MODE;
	rand uvm_reg_field QUEUE_DEPTH_MODE;
	rand uvm_reg_field CM_APPLY_QCN_CFG_QUEUE_DEPTH_START_BIT;
	rand uvm_reg_field QUEUE_DEPTH_START_BIT;
	rand uvm_reg_field CM_APPLY_QCN_CFG_SELECT_COMP;
	rand uvm_reg_field SELECT_COMP;
	rand uvm_reg_field CM_APPLY_QCN_CFG_SAMPLE_RATE_EXP;
	rand uvm_reg_field SAMPLE_RATE_EXP;

	function new(string name = "cm_apply");
		super.new(name, build_coverage(UVM_NO_COVERAGE));
	endfunction: new

   virtual function void build();
      this.default_map = create_map("", 0, 8, UVM_LITTLE_ENDIAN, 0);
      foreach (this.CM_APPLY_TX_SOFTDROP_CFG[i,j]) begin
         int J = i;
         int K = j;
         this.CM_APPLY_TX_SOFTDROP_CFG[J][K] = ral_reg_cm_apply_CM_APPLY_TX_SOFTDROP_CFG::type_id::create($psprintf("CM_APPLY_TX_SOFTDROP_CFG[%0d][%0d]",J,K),,get_full_name());
         this.CM_APPLY_TX_SOFTDROP_CFG[J][K].configure(this, null, "");
         this.CM_APPLY_TX_SOFTDROP_CFG[J][K].build();
         this.CM_APPLY_TX_SOFTDROP_CFG[J][K].add_hdl_path('{

            '{$psprintf("CM_APPLY_TX_SOFTDROP_CFG[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.CM_APPLY_TX_SOFTDROP_CFG[J][K], `UVM_REG_ADDR_WIDTH'h0+J*`UVM_REG_ADDR_WIDTH'h40+K*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.CM_APPLY_TX_SOFTDROP_CFG_RSVD0[J][K] = this.CM_APPLY_TX_SOFTDROP_CFG[J][K].RSVD0;
			this.CM_APPLY_TX_SOFTDROP_CFG_SOFT_DROP_ON_PRIVATE[J][K] = this.CM_APPLY_TX_SOFTDROP_CFG[J][K].SOFT_DROP_ON_PRIVATE;
			this.SOFT_DROP_ON_PRIVATE[J][K] = this.CM_APPLY_TX_SOFTDROP_CFG[J][K].SOFT_DROP_ON_PRIVATE;
			this.CM_APPLY_TX_SOFTDROP_CFG_SOFT_DROP_ON_SMP_FREE[J][K] = this.CM_APPLY_TX_SOFTDROP_CFG[J][K].SOFT_DROP_ON_SMP_FREE;
			this.SOFT_DROP_ON_SMP_FREE[J][K] = this.CM_APPLY_TX_SOFTDROP_CFG[J][K].SOFT_DROP_ON_SMP_FREE;
      end
      foreach (this.CM_APPLY_TX_TC_STATE[i,j]) begin
         int J = i;
         int K = j;
         this.CM_APPLY_TX_TC_STATE[J][K] = ral_reg_cm_apply_CM_APPLY_TX_TC_STATE::type_id::create($psprintf("CM_APPLY_TX_TC_STATE[%0d][%0d]",J,K),,get_full_name());
         this.CM_APPLY_TX_TC_STATE[J][K].configure(this, null, "");
         this.CM_APPLY_TX_TC_STATE[J][K].build();
         this.CM_APPLY_TX_TC_STATE[J][K].add_hdl_path('{

            '{$psprintf("CM_APPLY_TX_TC_STATE[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.CM_APPLY_TX_TC_STATE[J][K], `UVM_REG_ADDR_WIDTH'h1000+J*`UVM_REG_ADDR_WIDTH'h40+K*`UVM_REG_ADDR_WIDTH'h8, "RO", 0);
			this.CM_APPLY_TX_TC_STATE_RSVD0[J][K] = this.CM_APPLY_TX_TC_STATE[J][K].RSVD0;
			this.CM_APPLY_TX_TC_STATE_QUEUE_DEPTH[J][K] = this.CM_APPLY_TX_TC_STATE[J][K].QUEUE_DEPTH;
			this.QUEUE_DEPTH[J][K] = this.CM_APPLY_TX_TC_STATE[J][K].QUEUE_DEPTH;
			this.CM_APPLY_TX_TC_STATE_OVER_SMP_FREE2[J][K] = this.CM_APPLY_TX_TC_STATE[J][K].OVER_SMP_FREE2;
			this.OVER_SMP_FREE2[J][K] = this.CM_APPLY_TX_TC_STATE[J][K].OVER_SMP_FREE2;
			this.CM_APPLY_TX_TC_STATE_OVER_SMP_FREE[J][K] = this.CM_APPLY_TX_TC_STATE[J][K].OVER_SMP_FREE;
			this.OVER_SMP_FREE[J][K] = this.CM_APPLY_TX_TC_STATE[J][K].OVER_SMP_FREE;
			this.CM_APPLY_TX_TC_STATE_TX_HOG[J][K] = this.CM_APPLY_TX_TC_STATE[J][K].TX_HOG;
			this.TX_HOG[J][K] = this.CM_APPLY_TX_TC_STATE[J][K].TX_HOG;
			this.CM_APPLY_TX_TC_STATE_TX_PRIVATE[J][K] = this.CM_APPLY_TX_TC_STATE[J][K].TX_PRIVATE;
			this.TX_PRIVATE[J][K] = this.CM_APPLY_TX_TC_STATE[J][K].TX_PRIVATE;
      end
      foreach (this.CM_APPLY_TX_TC_QCN_WM_THRESHOLD[i,j]) begin
         int J = i;
         int K = j;
         this.CM_APPLY_TX_TC_QCN_WM_THRESHOLD[J][K] = ral_reg_cm_apply_CM_APPLY_TX_TC_QCN_WM_THRESHOLD::type_id::create($psprintf("CM_APPLY_TX_TC_QCN_WM_THRESHOLD[%0d][%0d]",J,K),,get_full_name());
         this.CM_APPLY_TX_TC_QCN_WM_THRESHOLD[J][K].configure(this, null, "");
         this.CM_APPLY_TX_TC_QCN_WM_THRESHOLD[J][K].build();
         this.CM_APPLY_TX_TC_QCN_WM_THRESHOLD[J][K].add_hdl_path('{

            '{$psprintf("CM_APPLY_TX_TC_QCN_WM_THRESHOLD[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.CM_APPLY_TX_TC_QCN_WM_THRESHOLD[J][K], `UVM_REG_ADDR_WIDTH'h2000+J*`UVM_REG_ADDR_WIDTH'h40+K*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.CM_APPLY_TX_TC_QCN_WM_THRESHOLD_RSVD0[J][K] = this.CM_APPLY_TX_TC_QCN_WM_THRESHOLD[J][K].RSVD0;
			this.CM_APPLY_TX_TC_QCN_WM_THRESHOLD_WM_THRESHOLD[J][K] = this.CM_APPLY_TX_TC_QCN_WM_THRESHOLD[J][K].WM_THRESHOLD;
			this.WM_THRESHOLD[J][K] = this.CM_APPLY_TX_TC_QCN_WM_THRESHOLD[J][K].WM_THRESHOLD;
      end
      foreach (this.CM_APPLY_RX_SMP_STATE[i,j]) begin
         int J = i;
         int K = j;
         this.CM_APPLY_RX_SMP_STATE[J][K] = ral_reg_cm_apply_CM_APPLY_RX_SMP_STATE::type_id::create($psprintf("CM_APPLY_RX_SMP_STATE[%0d][%0d]",J,K),,get_full_name());
         this.CM_APPLY_RX_SMP_STATE[J][K].configure(this, null, "");
         this.CM_APPLY_RX_SMP_STATE[J][K].build();
         this.CM_APPLY_RX_SMP_STATE[J][K].add_hdl_path('{

            '{$psprintf("CM_APPLY_RX_SMP_STATE[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.CM_APPLY_RX_SMP_STATE[J][K], `UVM_REG_ADDR_WIDTH'h3000+J*`UVM_REG_ADDR_WIDTH'h10+K*`UVM_REG_ADDR_WIDTH'h8, "RO", 0);
			this.CM_APPLY_RX_SMP_STATE_RSVD0[J][K] = this.CM_APPLY_RX_SMP_STATE[J][K].RSVD0;
			this.CM_APPLY_RX_SMP_STATE_RX_HOG[J][K] = this.CM_APPLY_RX_SMP_STATE[J][K].RX_HOG;
			this.RX_HOG[J][K] = this.CM_APPLY_RX_SMP_STATE[J][K].RX_HOG;
			this.CM_APPLY_RX_SMP_STATE_RX_PRIVATE[J][K] = this.CM_APPLY_RX_SMP_STATE[J][K].RX_PRIVATE;
			this.RX_PRIVATE[J][K] = this.CM_APPLY_RX_SMP_STATE[J][K].RX_PRIVATE;
      end
      foreach (this.CM_APPLY_MIRROR_PROFILE_TABLE[i]) begin
         int J = i;
         this.CM_APPLY_MIRROR_PROFILE_TABLE[J] = ral_reg_cm_apply_CM_APPLY_MIRROR_PROFILE_TABLE::type_id::create($psprintf("CM_APPLY_MIRROR_PROFILE_TABLE[%0d]",J),,get_full_name());
         this.CM_APPLY_MIRROR_PROFILE_TABLE[J].configure(this, null, "");
         this.CM_APPLY_MIRROR_PROFILE_TABLE[J].build();
         this.CM_APPLY_MIRROR_PROFILE_TABLE[J].add_hdl_path('{

            '{$psprintf("CM_APPLY_MIRROR_PROFILE_TABLE[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.CM_APPLY_MIRROR_PROFILE_TABLE[J], `UVM_REG_ADDR_WIDTH'h3200+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.CM_APPLY_MIRROR_PROFILE_TABLE_RSVD0[J] = this.CM_APPLY_MIRROR_PROFILE_TABLE[J].RSVD0;
			this.CM_APPLY_MIRROR_PROFILE_TABLE_PORT[J] = this.CM_APPLY_MIRROR_PROFILE_TABLE[J].PORT;
			this.PORT[J] = this.CM_APPLY_MIRROR_PROFILE_TABLE[J].PORT;
      end
      foreach (this.CM_APPLY_DROP_COUNT[i]) begin
         int J = i;
         this.CM_APPLY_DROP_COUNT[J] = ral_reg_cm_apply_CM_APPLY_DROP_COUNT::type_id::create($psprintf("CM_APPLY_DROP_COUNT[%0d]",J),,get_full_name());
         this.CM_APPLY_DROP_COUNT[J].configure(this, null, "");
         this.CM_APPLY_DROP_COUNT[J].build();
         this.CM_APPLY_DROP_COUNT[J].add_hdl_path('{

            '{$psprintf("CM_APPLY_DROP_COUNT[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.CM_APPLY_DROP_COUNT[J], `UVM_REG_ADDR_WIDTH'h3400+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.CM_APPLY_DROP_COUNT_RSVD0[J] = this.CM_APPLY_DROP_COUNT[J].RSVD0;
			this.CM_APPLY_DROP_COUNT_FRAMES[J] = this.CM_APPLY_DROP_COUNT[J].FRAMES;
			this.FRAMES[J] = this.CM_APPLY_DROP_COUNT[J].FRAMES;
      end
      foreach (this.CM_APPLY_LOOPBACK_SUPPRESS[i]) begin
         int J = i;
         this.CM_APPLY_LOOPBACK_SUPPRESS[J] = ral_reg_cm_apply_CM_APPLY_LOOPBACK_SUPPRESS::type_id::create($psprintf("CM_APPLY_LOOPBACK_SUPPRESS[%0d]",J),,get_full_name());
         this.CM_APPLY_LOOPBACK_SUPPRESS[J].configure(this, null, "");
         this.CM_APPLY_LOOPBACK_SUPPRESS[J].build();
         this.CM_APPLY_LOOPBACK_SUPPRESS[J].add_hdl_path('{

            '{$psprintf("CM_APPLY_LOOPBACK_SUPPRESS[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.CM_APPLY_LOOPBACK_SUPPRESS[J], `UVM_REG_ADDR_WIDTH'h3500+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.CM_APPLY_LOOPBACK_SUPPRESS_RSVD0[J] = this.CM_APPLY_LOOPBACK_SUPPRESS[J].RSVD0;
			this.CM_APPLY_LOOPBACK_SUPPRESS_GLORT_MASK[J] = this.CM_APPLY_LOOPBACK_SUPPRESS[J].GLORT_MASK;
			this.GLORT_MASK[J] = this.CM_APPLY_LOOPBACK_SUPPRESS[J].GLORT_MASK;
			this.CM_APPLY_LOOPBACK_SUPPRESS_GLORT[J] = this.CM_APPLY_LOOPBACK_SUPPRESS[J].GLORT;
			this.GLORT[J] = this.CM_APPLY_LOOPBACK_SUPPRESS[J].GLORT;
      end
      foreach (this.CM_APPLY_SOFTDROP_CFG[i]) begin
         int J = i;
         this.CM_APPLY_SOFTDROP_CFG[J] = ral_reg_cm_apply_CM_APPLY_SOFTDROP_CFG::type_id::create($psprintf("CM_APPLY_SOFTDROP_CFG[%0d]",J),,get_full_name());
         this.CM_APPLY_SOFTDROP_CFG[J].configure(this, null, "");
         this.CM_APPLY_SOFTDROP_CFG[J].build();
         this.CM_APPLY_SOFTDROP_CFG[J].add_hdl_path('{

            '{$psprintf("CM_APPLY_SOFTDROP_CFG[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.CM_APPLY_SOFTDROP_CFG[J], `UVM_REG_ADDR_WIDTH'h3600+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.CM_APPLY_SOFTDROP_CFG_RSVD0[J] = this.CM_APPLY_SOFTDROP_CFG[J].RSVD0;
			this.CM_APPLY_SOFTDROP_CFG_JITTER_BITS[J] = this.CM_APPLY_SOFTDROP_CFG[J].JITTER_BITS;
			this.JITTER_BITS[J] = this.CM_APPLY_SOFTDROP_CFG[J].JITTER_BITS;
      end
      foreach (this.CM_APPLY_SOFTDROP_STATE[i]) begin
         int J = i;
         this.CM_APPLY_SOFTDROP_STATE[J] = ral_reg_cm_apply_CM_APPLY_SOFTDROP_STATE::type_id::create($psprintf("CM_APPLY_SOFTDROP_STATE[%0d]",J),,get_full_name());
         this.CM_APPLY_SOFTDROP_STATE[J].configure(this, null, "");
         this.CM_APPLY_SOFTDROP_STATE[J].build();
         this.CM_APPLY_SOFTDROP_STATE[J].add_hdl_path('{

            '{$psprintf("CM_APPLY_SOFTDROP_STATE[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.CM_APPLY_SOFTDROP_STATE[J], `UVM_REG_ADDR_WIDTH'h3640+J*`UVM_REG_ADDR_WIDTH'h8, "RO", 0);
			this.CM_APPLY_SOFTDROP_STATE_RSVD0[J] = this.CM_APPLY_SOFTDROP_STATE[J].RSVD0;
			this.CM_APPLY_SOFTDROP_STATE_OVER_LIMIT[J] = this.CM_APPLY_SOFTDROP_STATE[J].OVER_LIMIT;
			this.OVER_LIMIT[J] = this.CM_APPLY_SOFTDROP_STATE[J].OVER_LIMIT;
			this.CM_APPLY_SOFTDROP_STATE_USAGE_OVER_LIMIT[J] = this.CM_APPLY_SOFTDROP_STATE[J].USAGE_OVER_LIMIT;
			this.USAGE_OVER_LIMIT[J] = this.CM_APPLY_SOFTDROP_STATE[J].USAGE_OVER_LIMIT;
      end
      foreach (this.CM_APPLY_TRAP_GLORT[i]) begin
         int J = i;
         this.CM_APPLY_TRAP_GLORT[J] = ral_reg_cm_apply_CM_APPLY_TRAP_GLORT::type_id::create($psprintf("CM_APPLY_TRAP_GLORT[%0d]",J),,get_full_name());
         this.CM_APPLY_TRAP_GLORT[J].configure(this, null, "");
         this.CM_APPLY_TRAP_GLORT[J].build();
         this.CM_APPLY_TRAP_GLORT[J].add_hdl_path('{

            '{$psprintf("CM_APPLY_TRAP_GLORT[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.CM_APPLY_TRAP_GLORT[J], `UVM_REG_ADDR_WIDTH'h3680+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.CM_APPLY_TRAP_GLORT_RSVD0[J] = this.CM_APPLY_TRAP_GLORT[J].RSVD0;
			this.CM_APPLY_TRAP_GLORT_TRAP_GLORT[J] = this.CM_APPLY_TRAP_GLORT[J].TRAP_GLORT;
			this.TRAP_GLORT[J] = this.CM_APPLY_TRAP_GLORT[J].TRAP_GLORT;
      end
      this.CM_APPLY_TC_TO_SMP = ral_reg_cm_apply_CM_APPLY_TC_TO_SMP::type_id::create("CM_APPLY_TC_TO_SMP",,get_full_name());
      this.CM_APPLY_TC_TO_SMP.configure(this, null, "");
      this.CM_APPLY_TC_TO_SMP.build();
         this.CM_APPLY_TC_TO_SMP.add_hdl_path('{

            '{"CM_APPLY_TC_TO_SMP", -1, -1}
         });
      this.default_map.add_reg(this.CM_APPLY_TC_TO_SMP, `UVM_REG_ADDR_WIDTH'h3700, "RW", 0);
		this.CM_APPLY_TC_TO_SMP_RSVD0 = this.CM_APPLY_TC_TO_SMP.RSVD0;
		this.CM_APPLY_TC_TO_SMP_SMP_7 = this.CM_APPLY_TC_TO_SMP.SMP_7;
		this.SMP_7 = this.CM_APPLY_TC_TO_SMP.SMP_7;
		this.CM_APPLY_TC_TO_SMP_SMP_6 = this.CM_APPLY_TC_TO_SMP.SMP_6;
		this.SMP_6 = this.CM_APPLY_TC_TO_SMP.SMP_6;
		this.CM_APPLY_TC_TO_SMP_SMP_5 = this.CM_APPLY_TC_TO_SMP.SMP_5;
		this.SMP_5 = this.CM_APPLY_TC_TO_SMP.SMP_5;
		this.CM_APPLY_TC_TO_SMP_SMP_4 = this.CM_APPLY_TC_TO_SMP.SMP_4;
		this.SMP_4 = this.CM_APPLY_TC_TO_SMP.SMP_4;
		this.CM_APPLY_TC_TO_SMP_SMP_3 = this.CM_APPLY_TC_TO_SMP.SMP_3;
		this.SMP_3 = this.CM_APPLY_TC_TO_SMP.SMP_3;
		this.CM_APPLY_TC_TO_SMP_SMP_2 = this.CM_APPLY_TC_TO_SMP.SMP_2;
		this.SMP_2 = this.CM_APPLY_TC_TO_SMP.SMP_2;
		this.CM_APPLY_TC_TO_SMP_SMP_1 = this.CM_APPLY_TC_TO_SMP.SMP_1;
		this.SMP_1 = this.CM_APPLY_TC_TO_SMP.SMP_1;
		this.CM_APPLY_TC_TO_SMP_SMP_0 = this.CM_APPLY_TC_TO_SMP.SMP_0;
		this.SMP_0 = this.CM_APPLY_TC_TO_SMP.SMP_0;
      this.CM_APPLY_MCAST_EPOCH = ral_reg_cm_apply_CM_APPLY_MCAST_EPOCH::type_id::create("CM_APPLY_MCAST_EPOCH",,get_full_name());
      this.CM_APPLY_MCAST_EPOCH.configure(this, null, "");
      this.CM_APPLY_MCAST_EPOCH.build();
         this.CM_APPLY_MCAST_EPOCH.add_hdl_path('{

            '{"CM_APPLY_MCAST_EPOCH", -1, -1}
         });
      this.default_map.add_reg(this.CM_APPLY_MCAST_EPOCH, `UVM_REG_ADDR_WIDTH'h3708, "RW", 0);
		this.CM_APPLY_MCAST_EPOCH_RSVD0 = this.CM_APPLY_MCAST_EPOCH.RSVD0;
		this.CM_APPLY_MCAST_EPOCH_CURRENT = this.CM_APPLY_MCAST_EPOCH.CURRENT;
		this.CURRENT = this.CM_APPLY_MCAST_EPOCH.CURRENT;
      this.CM_APPLY_STATE = ral_reg_cm_apply_CM_APPLY_STATE::type_id::create("CM_APPLY_STATE",,get_full_name());
      this.CM_APPLY_STATE.configure(this, null, "");
      this.CM_APPLY_STATE.build();
         this.CM_APPLY_STATE.add_hdl_path('{

            '{"CM_APPLY_STATE", -1, -1}
         });
      this.default_map.add_reg(this.CM_APPLY_STATE, `UVM_REG_ADDR_WIDTH'h3710, "RO", 0);
		this.CM_APPLY_STATE_RSVD0 = this.CM_APPLY_STATE.RSVD0;
		this.CM_APPLY_STATE_RXS_7 = this.CM_APPLY_STATE.RXS_7;
		this.RXS_7 = this.CM_APPLY_STATE.RXS_7;
		this.CM_APPLY_STATE_RXS_6 = this.CM_APPLY_STATE.RXS_6;
		this.RXS_6 = this.CM_APPLY_STATE.RXS_6;
		this.CM_APPLY_STATE_RXS_5 = this.CM_APPLY_STATE.RXS_5;
		this.RXS_5 = this.CM_APPLY_STATE.RXS_5;
		this.CM_APPLY_STATE_RXS_4 = this.CM_APPLY_STATE.RXS_4;
		this.RXS_4 = this.CM_APPLY_STATE.RXS_4;
		this.CM_APPLY_STATE_RXS_3 = this.CM_APPLY_STATE.RXS_3;
		this.RXS_3 = this.CM_APPLY_STATE.RXS_3;
		this.CM_APPLY_STATE_RXS_2 = this.CM_APPLY_STATE.RXS_2;
		this.RXS_2 = this.CM_APPLY_STATE.RXS_2;
		this.CM_APPLY_STATE_RXS_1 = this.CM_APPLY_STATE.RXS_1;
		this.RXS_1 = this.CM_APPLY_STATE.RXS_1;
		this.CM_APPLY_STATE_RXS_0 = this.CM_APPLY_STATE.RXS_0;
		this.RXS_0 = this.CM_APPLY_STATE.RXS_0;
		this.CM_APPLY_STATE_GLOBAL_EX = this.CM_APPLY_STATE.GLOBAL_EX;
		this.GLOBAL_EX = this.CM_APPLY_STATE.GLOBAL_EX;
      this.CM_APPLY_CPU_TRAP_MASK = ral_reg_cm_apply_CM_APPLY_CPU_TRAP_MASK::type_id::create("CM_APPLY_CPU_TRAP_MASK",,get_full_name());
      this.CM_APPLY_CPU_TRAP_MASK.configure(this, null, "");
      this.CM_APPLY_CPU_TRAP_MASK.build();
         this.CM_APPLY_CPU_TRAP_MASK.add_hdl_path('{

            '{"CM_APPLY_CPU_TRAP_MASK", -1, -1}
         });
      this.default_map.add_reg(this.CM_APPLY_CPU_TRAP_MASK, `UVM_REG_ADDR_WIDTH'h3718, "RW", 0);
		this.CM_APPLY_CPU_TRAP_MASK_RSVD0 = this.CM_APPLY_CPU_TRAP_MASK.RSVD0;
		this.CM_APPLY_CPU_TRAP_MASK_DEST_MASK = this.CM_APPLY_CPU_TRAP_MASK.DEST_MASK;
		this.DEST_MASK = this.CM_APPLY_CPU_TRAP_MASK.DEST_MASK;
      this.CM_APPLY_LOG_MIRROR_PROFILE = ral_reg_cm_apply_CM_APPLY_LOG_MIRROR_PROFILE::type_id::create("CM_APPLY_LOG_MIRROR_PROFILE",,get_full_name());
      this.CM_APPLY_LOG_MIRROR_PROFILE.configure(this, null, "");
      this.CM_APPLY_LOG_MIRROR_PROFILE.build();
         this.CM_APPLY_LOG_MIRROR_PROFILE.add_hdl_path('{

            '{"CM_APPLY_LOG_MIRROR_PROFILE", -1, -1}
         });
      this.default_map.add_reg(this.CM_APPLY_LOG_MIRROR_PROFILE, `UVM_REG_ADDR_WIDTH'h3720, "RW", 0);
		this.CM_APPLY_LOG_MIRROR_PROFILE_RSVD0 = this.CM_APPLY_LOG_MIRROR_PROFILE.RSVD0;
		this.CM_APPLY_LOG_MIRROR_PROFILE_TRIGGER = this.CM_APPLY_LOG_MIRROR_PROFILE.TRIGGER;
		this.TRIGGER = this.CM_APPLY_LOG_MIRROR_PROFILE.TRIGGER;
		this.CM_APPLY_LOG_MIRROR_PROFILE_TTL = this.CM_APPLY_LOG_MIRROR_PROFILE.TTL;
		this.TTL = this.CM_APPLY_LOG_MIRROR_PROFILE.TTL;
		this.CM_APPLY_LOG_MIRROR_PROFILE_ICMP = this.CM_APPLY_LOG_MIRROR_PROFILE.ICMP;
		this.ICMP = this.CM_APPLY_LOG_MIRROR_PROFILE.ICMP;
		this.CM_APPLY_LOG_MIRROR_PROFILE_ARP_REDIRECT = this.CM_APPLY_LOG_MIRROR_PROFILE.ARP_REDIRECT;
		this.ARP_REDIRECT = this.CM_APPLY_LOG_MIRROR_PROFILE.ARP_REDIRECT;
		this.CM_APPLY_LOG_MIRROR_PROFILE_RESERVED_MAC = this.CM_APPLY_LOG_MIRROR_PROFILE.RESERVED_MAC;
		this.RESERVED_MAC = this.CM_APPLY_LOG_MIRROR_PROFILE.RESERVED_MAC;
		this.CM_APPLY_LOG_MIRROR_PROFILE_FFU = this.CM_APPLY_LOG_MIRROR_PROFILE.FFU;
		this.FFU = this.CM_APPLY_LOG_MIRROR_PROFILE.FFU;
      this.CM_APPLY_QCN_CFG = ral_reg_cm_apply_CM_APPLY_QCN_CFG::type_id::create("CM_APPLY_QCN_CFG",,get_full_name());
      this.CM_APPLY_QCN_CFG.configure(this, null, "");
      this.CM_APPLY_QCN_CFG.build();
         this.CM_APPLY_QCN_CFG.add_hdl_path('{

            '{"CM_APPLY_QCN_CFG", -1, -1}
         });
      this.default_map.add_reg(this.CM_APPLY_QCN_CFG, `UVM_REG_ADDR_WIDTH'h3728, "RW", 0);
		this.CM_APPLY_QCN_CFG_RSVD0 = this.CM_APPLY_QCN_CFG.RSVD0;
		this.CM_APPLY_QCN_CFG_QUEUE_DEPTH_MODE = this.CM_APPLY_QCN_CFG.QUEUE_DEPTH_MODE;
		this.QUEUE_DEPTH_MODE = this.CM_APPLY_QCN_CFG.QUEUE_DEPTH_MODE;
		this.CM_APPLY_QCN_CFG_QUEUE_DEPTH_START_BIT = this.CM_APPLY_QCN_CFG.QUEUE_DEPTH_START_BIT;
		this.QUEUE_DEPTH_START_BIT = this.CM_APPLY_QCN_CFG.QUEUE_DEPTH_START_BIT;
		this.CM_APPLY_QCN_CFG_SELECT_COMP = this.CM_APPLY_QCN_CFG.SELECT_COMP;
		this.SELECT_COMP = this.CM_APPLY_QCN_CFG.SELECT_COMP;
		this.CM_APPLY_QCN_CFG_SAMPLE_RATE_EXP = this.CM_APPLY_QCN_CFG.SAMPLE_RATE_EXP;
		this.SAMPLE_RATE_EXP = this.CM_APPLY_QCN_CFG.SAMPLE_RATE_EXP;
   endfunction : build

	`uvm_object_utils(ral_block_cm_apply)

endclass : ral_block_cm_apply



`endif
