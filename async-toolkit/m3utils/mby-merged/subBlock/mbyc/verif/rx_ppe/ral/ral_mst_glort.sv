// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

`ifndef RAL_MST_GLORT
`define RAL_MST_GLORT

import uvm_pkg::*;

class ral_reg_mst_glort_EGRESS_VID_TABLE extends uvm_reg;
	rand uvm_reg_field MEMBERSHIP;

	function new(string name = "mst_glort_EGRESS_VID_TABLE");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.MEMBERSHIP = uvm_reg_field::type_id::create("MEMBERSHIP",,get_full_name());
      this.MEMBERSHIP.configure(this, 64, 0, "RW", 0, 64'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_mst_glort_EGRESS_VID_TABLE)

endclass : ral_reg_mst_glort_EGRESS_VID_TABLE


class ral_reg_mst_glort_EGRESS_VID_CFG extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field TRIG_ID;

	function new(string name = "mst_glort_EGRESS_VID_CFG");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 58, 6, "RO", 0, 58'h0, 1, 0, 0);
      this.TRIG_ID = uvm_reg_field::type_id::create("TRIG_ID",,get_full_name());
      this.TRIG_ID.configure(this, 6, 0, "RW", 0, 6'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_mst_glort_EGRESS_VID_CFG)

endclass : ral_reg_mst_glort_EGRESS_VID_CFG


class ral_reg_mst_glort_INGRESS_MST_TABLE extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field STP_STATE_17;
	rand uvm_reg_field STP_STATE_16;
	rand uvm_reg_field STP_STATE_15;
	rand uvm_reg_field STP_STATE_14;
	rand uvm_reg_field STP_STATE_13;
	rand uvm_reg_field STP_STATE_12;
	rand uvm_reg_field STP_STATE_11;
	rand uvm_reg_field STP_STATE_10;
	rand uvm_reg_field STP_STATE_9;
	rand uvm_reg_field STP_STATE_8;
	rand uvm_reg_field STP_STATE_7;
	rand uvm_reg_field STP_STATE_6;
	rand uvm_reg_field STP_STATE_5;
	rand uvm_reg_field STP_STATE_4;
	rand uvm_reg_field STP_STATE_3;
	rand uvm_reg_field STP_STATE_2;
	rand uvm_reg_field STP_STATE_1;
	rand uvm_reg_field STP_STATE_0;

	function new(string name = "mst_glort_INGRESS_MST_TABLE");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 28, 36, "RO", 0, 28'h0, 1, 0, 0);
      this.STP_STATE_17 = uvm_reg_field::type_id::create("STP_STATE_17",,get_full_name());
      this.STP_STATE_17.configure(this, 2, 34, "RW", 0, 2'h0, 1, 0, 0);
      this.STP_STATE_16 = uvm_reg_field::type_id::create("STP_STATE_16",,get_full_name());
      this.STP_STATE_16.configure(this, 2, 32, "RW", 0, 2'h0, 1, 0, 0);
      this.STP_STATE_15 = uvm_reg_field::type_id::create("STP_STATE_15",,get_full_name());
      this.STP_STATE_15.configure(this, 2, 30, "RW", 0, 2'h0, 1, 0, 0);
      this.STP_STATE_14 = uvm_reg_field::type_id::create("STP_STATE_14",,get_full_name());
      this.STP_STATE_14.configure(this, 2, 28, "RW", 0, 2'h0, 1, 0, 0);
      this.STP_STATE_13 = uvm_reg_field::type_id::create("STP_STATE_13",,get_full_name());
      this.STP_STATE_13.configure(this, 2, 26, "RW", 0, 2'h0, 1, 0, 0);
      this.STP_STATE_12 = uvm_reg_field::type_id::create("STP_STATE_12",,get_full_name());
      this.STP_STATE_12.configure(this, 2, 24, "RW", 0, 2'h0, 1, 0, 0);
      this.STP_STATE_11 = uvm_reg_field::type_id::create("STP_STATE_11",,get_full_name());
      this.STP_STATE_11.configure(this, 2, 22, "RW", 0, 2'h0, 1, 0, 0);
      this.STP_STATE_10 = uvm_reg_field::type_id::create("STP_STATE_10",,get_full_name());
      this.STP_STATE_10.configure(this, 2, 20, "RW", 0, 2'h0, 1, 0, 0);
      this.STP_STATE_9 = uvm_reg_field::type_id::create("STP_STATE_9",,get_full_name());
      this.STP_STATE_9.configure(this, 2, 18, "RW", 0, 2'h0, 1, 0, 0);
      this.STP_STATE_8 = uvm_reg_field::type_id::create("STP_STATE_8",,get_full_name());
      this.STP_STATE_8.configure(this, 2, 16, "RW", 0, 2'h0, 1, 0, 0);
      this.STP_STATE_7 = uvm_reg_field::type_id::create("STP_STATE_7",,get_full_name());
      this.STP_STATE_7.configure(this, 2, 14, "RW", 0, 2'h0, 1, 0, 0);
      this.STP_STATE_6 = uvm_reg_field::type_id::create("STP_STATE_6",,get_full_name());
      this.STP_STATE_6.configure(this, 2, 12, "RW", 0, 2'h0, 1, 0, 0);
      this.STP_STATE_5 = uvm_reg_field::type_id::create("STP_STATE_5",,get_full_name());
      this.STP_STATE_5.configure(this, 2, 10, "RW", 0, 2'h0, 1, 0, 0);
      this.STP_STATE_4 = uvm_reg_field::type_id::create("STP_STATE_4",,get_full_name());
      this.STP_STATE_4.configure(this, 2, 8, "RW", 0, 2'h0, 1, 0, 0);
      this.STP_STATE_3 = uvm_reg_field::type_id::create("STP_STATE_3",,get_full_name());
      this.STP_STATE_3.configure(this, 2, 6, "RW", 0, 2'h0, 1, 0, 0);
      this.STP_STATE_2 = uvm_reg_field::type_id::create("STP_STATE_2",,get_full_name());
      this.STP_STATE_2.configure(this, 2, 4, "RW", 0, 2'h0, 1, 0, 0);
      this.STP_STATE_1 = uvm_reg_field::type_id::create("STP_STATE_1",,get_full_name());
      this.STP_STATE_1.configure(this, 2, 2, "RW", 0, 2'h0, 1, 0, 0);
      this.STP_STATE_0 = uvm_reg_field::type_id::create("STP_STATE_0",,get_full_name());
      this.STP_STATE_0.configure(this, 2, 0, "RW", 0, 2'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_mst_glort_INGRESS_MST_TABLE)

endclass : ral_reg_mst_glort_INGRESS_MST_TABLE


class ral_reg_mst_glort_EGRESS_MST_TABLE extends uvm_reg;
	rand uvm_reg_field FORWARDING;

	function new(string name = "mst_glort_EGRESS_MST_TABLE");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.FORWARDING = uvm_reg_field::type_id::create("FORWARDING",,get_full_name());
      this.FORWARDING.configure(this, 64, 0, "RW", 0, 64'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_mst_glort_EGRESS_MST_TABLE)

endclass : ral_reg_mst_glort_EGRESS_MST_TABLE


class ral_reg_mst_glort_GLORT_DIRECT_MAP_CTRL extends uvm_reg;
	rand uvm_reg_field GO_COMPL;
	rand uvm_reg_field STATUS;
	rand uvm_reg_field OP_TYPE;
	uvm_reg_field _RSVD0_;
	rand uvm_reg_field REG_ID;
	rand uvm_reg_field REG_SUB_ID;
	rand uvm_reg_field REG_INDX;

	function new(string name = "mst_glort_GLORT_DIRECT_MAP_CTRL");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.GO_COMPL = uvm_reg_field::type_id::create("GO_COMPL",,get_full_name());
      this.GO_COMPL.configure(this, 1, 63, "RW", 1, 1'h0, 1, 0, 0);
      this.STATUS = uvm_reg_field::type_id::create("STATUS",,get_full_name());
      this.STATUS.configure(this, 1, 62, "RW", 1, 1'h0, 1, 0, 0);
      this.OP_TYPE = uvm_reg_field::type_id::create("OP_TYPE",,get_full_name());
      this.OP_TYPE.configure(this, 1, 61, "RW", 0, 1'h0, 1, 0, 0);
      this._RSVD0_ = uvm_reg_field::type_id::create("_RSVD0_",,get_full_name());
      this._RSVD0_.configure(this, 13, 48, "RO", 1, 13'h0, 1, 0, 0);
      this.REG_ID = uvm_reg_field::type_id::create("REG_ID",,get_full_name());
      this.REG_ID.configure(this, 8, 40, "RW", 0, 8'h0, 1, 0, 1);
      this.REG_SUB_ID = uvm_reg_field::type_id::create("REG_SUB_ID",,get_full_name());
      this.REG_SUB_ID.configure(this, 8, 32, "RW", 0, 8'h0, 1, 0, 1);
      this.REG_INDX = uvm_reg_field::type_id::create("REG_INDX",,get_full_name());
      this.REG_INDX.configure(this, 32, 0, "RW", 0, 32'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_mst_glort_GLORT_DIRECT_MAP_CTRL)

endclass : ral_reg_mst_glort_GLORT_DIRECT_MAP_CTRL


class ral_reg_mst_glort_GLORT_DIRECT_MAP_DST0 extends uvm_reg;
	rand uvm_reg_field DEST_MASK;

	function new(string name = "mst_glort_GLORT_DIRECT_MAP_DST0");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.DEST_MASK = uvm_reg_field::type_id::create("DEST_MASK",,get_full_name());
      this.DEST_MASK.configure(this, 64, 0, "RW", 0, 64'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_mst_glort_GLORT_DIRECT_MAP_DST0)

endclass : ral_reg_mst_glort_GLORT_DIRECT_MAP_DST0


class ral_reg_mst_glort_GLORT_DIRECT_MAP_DST1 extends uvm_reg;
	rand uvm_reg_field DEST_MASK;

	function new(string name = "mst_glort_GLORT_DIRECT_MAP_DST1");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.DEST_MASK = uvm_reg_field::type_id::create("DEST_MASK",,get_full_name());
      this.DEST_MASK.configure(this, 64, 0, "RW", 0, 64'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_mst_glort_GLORT_DIRECT_MAP_DST1)

endclass : ral_reg_mst_glort_GLORT_DIRECT_MAP_DST1


class ral_reg_mst_glort_GLORT_DIRECT_MAP_DST2 extends uvm_reg;
	rand uvm_reg_field DEST_MASK;

	function new(string name = "mst_glort_GLORT_DIRECT_MAP_DST2");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.DEST_MASK = uvm_reg_field::type_id::create("DEST_MASK",,get_full_name());
      this.DEST_MASK.configure(this, 64, 0, "RW", 0, 64'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_mst_glort_GLORT_DIRECT_MAP_DST2)

endclass : ral_reg_mst_glort_GLORT_DIRECT_MAP_DST2


class ral_reg_mst_glort_GLORT_DIRECT_MAP_DST3 extends uvm_reg;
	rand uvm_reg_field DEST_MASK;

	function new(string name = "mst_glort_GLORT_DIRECT_MAP_DST3");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.DEST_MASK = uvm_reg_field::type_id::create("DEST_MASK",,get_full_name());
      this.DEST_MASK.configure(this, 64, 0, "RW", 0, 64'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_mst_glort_GLORT_DIRECT_MAP_DST3)

endclass : ral_reg_mst_glort_GLORT_DIRECT_MAP_DST3


class ral_reg_mst_glort_GLORT_DIRECT_MAP_DST4 extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field IP_MULTICAST_INDEX;
	rand uvm_reg_field DEST_MASK;

	function new(string name = "mst_glort_GLORT_DIRECT_MAP_DST4");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 50, 14, "RO", 0, 50'h0, 1, 0, 0);
      this.IP_MULTICAST_INDEX = uvm_reg_field::type_id::create("IP_MULTICAST_INDEX",,get_full_name());
      this.IP_MULTICAST_INDEX.configure(this, 12, 2, "RW", 0, 12'h0, 1, 0, 0);
      this.DEST_MASK = uvm_reg_field::type_id::create("DEST_MASK",,get_full_name());
      this.DEST_MASK.configure(this, 2, 0, "RW", 0, 2'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_mst_glort_GLORT_DIRECT_MAP_DST4)

endclass : ral_reg_mst_glort_GLORT_DIRECT_MAP_DST4


class ral_reg_mst_glort_GLORT_RAM extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field SKIP_DGLORT_DEC;
	rand uvm_reg_field HASH_ROTATION;
	rand uvm_reg_field DEST_COUNT;
	rand uvm_reg_field RANGE_SUB_INDEX_B;
	rand uvm_reg_field RANGE_SUB_INDEX_A;
	rand uvm_reg_field DEST_INDEX;
	rand uvm_reg_field STRICT;

	function new(string name = "mst_glort_GLORT_RAM");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 28, 36, "RO", 0, 28'h0, 1, 0, 0);
      this.SKIP_DGLORT_DEC = uvm_reg_field::type_id::create("SKIP_DGLORT_DEC",,get_full_name());
      this.SKIP_DGLORT_DEC.configure(this, 1, 35, "RW", 0, 1'h0, 1, 0, 0);
      this.HASH_ROTATION = uvm_reg_field::type_id::create("HASH_ROTATION",,get_full_name());
      this.HASH_ROTATION.configure(this, 1, 34, "RW", 0, 1'h0, 1, 0, 0);
      this.DEST_COUNT = uvm_reg_field::type_id::create("DEST_COUNT",,get_full_name());
      this.DEST_COUNT.configure(this, 4, 30, "RW", 0, 4'h0, 1, 0, 0);
      this.RANGE_SUB_INDEX_B = uvm_reg_field::type_id::create("RANGE_SUB_INDEX_B",,get_full_name());
      this.RANGE_SUB_INDEX_B.configure(this, 8, 22, "RW", 0, 8'h0, 1, 0, 0);
      this.RANGE_SUB_INDEX_A = uvm_reg_field::type_id::create("RANGE_SUB_INDEX_A",,get_full_name());
      this.RANGE_SUB_INDEX_A.configure(this, 8, 14, "RW", 0, 8'h0, 1, 0, 0);
      this.DEST_INDEX = uvm_reg_field::type_id::create("DEST_INDEX",,get_full_name());
      this.DEST_INDEX.configure(this, 12, 2, "RW", 0, 12'h0, 1, 0, 0);
      this.STRICT = uvm_reg_field::type_id::create("STRICT",,get_full_name());
      this.STRICT.configure(this, 2, 0, "RW", 0, 2'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_mst_glort_GLORT_RAM)

endclass : ral_reg_mst_glort_GLORT_RAM


class ral_reg_mst_glort_GLORT_CAM extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field KEY_INVERT;
	rand uvm_reg_field KEY;

	function new(string name = "mst_glort_GLORT_CAM");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 32, 32, "RO", 0, 32'h0, 1, 0, 1);
      this.KEY_INVERT = uvm_reg_field::type_id::create("KEY_INVERT",,get_full_name());
      this.KEY_INVERT.configure(this, 16, 16, "RW", 0, 16'hffff, 1, 0, 1);
      this.KEY = uvm_reg_field::type_id::create("KEY",,get_full_name());
      this.KEY.configure(this, 16, 0, "RW", 0, 16'hffff, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_mst_glort_GLORT_CAM)

endclass : ral_reg_mst_glort_GLORT_CAM


class ral_reg_mst_glort_CGRP_USED_TABLE extends uvm_reg;
	rand uvm_reg_field USED;

	function new(string name = "mst_glort_CGRP_USED_TABLE");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.USED = uvm_reg_field::type_id::create("USED",,get_full_name());
      this.USED.configure(this, 64, 0, "W1C", 1, 64'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_mst_glort_CGRP_USED_TABLE)

endclass : ral_reg_mst_glort_CGRP_USED_TABLE


class ral_block_mst_glort extends uvm_reg_block;
	rand ral_reg_mst_glort_EGRESS_VID_TABLE EGRESS_VID_TABLE[4096][0:4];
	rand ral_reg_mst_glort_EGRESS_VID_CFG EGRESS_VID_CFG[4096];
	rand ral_reg_mst_glort_INGRESS_MST_TABLE INGRESS_MST_TABLE[4096];
	rand ral_reg_mst_glort_EGRESS_MST_TABLE EGRESS_MST_TABLE[4096][0:4];
	rand ral_reg_mst_glort_GLORT_DIRECT_MAP_CTRL GLORT_DIRECT_MAP_CTRL;
	rand ral_reg_mst_glort_GLORT_DIRECT_MAP_DST0 GLORT_DIRECT_MAP_DST0;
	rand ral_reg_mst_glort_GLORT_DIRECT_MAP_DST1 GLORT_DIRECT_MAP_DST1;
	rand ral_reg_mst_glort_GLORT_DIRECT_MAP_DST2 GLORT_DIRECT_MAP_DST2;
	rand ral_reg_mst_glort_GLORT_DIRECT_MAP_DST3 GLORT_DIRECT_MAP_DST3;
	rand ral_reg_mst_glort_GLORT_DIRECT_MAP_DST4 GLORT_DIRECT_MAP_DST4;
	rand ral_reg_mst_glort_GLORT_RAM GLORT_RAM[64];
	rand ral_reg_mst_glort_GLORT_CAM GLORT_CAM[64];
	rand ral_reg_mst_glort_CGRP_USED_TABLE CGRP_USED_TABLE[65];
	rand uvm_reg_field EGRESS_VID_TABLE_MEMBERSHIP[4096][0:4];
	rand uvm_reg_field MEMBERSHIP[4096][0:4];
	uvm_reg_field EGRESS_VID_CFG_RSVD0[4096];
	rand uvm_reg_field EGRESS_VID_CFG_TRIG_ID[4096];
	rand uvm_reg_field TRIG_ID[4096];
	uvm_reg_field INGRESS_MST_TABLE_RSVD0[4096];
	rand uvm_reg_field INGRESS_MST_TABLE_STP_STATE_17[4096];
	rand uvm_reg_field STP_STATE_17[4096];
	rand uvm_reg_field INGRESS_MST_TABLE_STP_STATE_16[4096];
	rand uvm_reg_field STP_STATE_16[4096];
	rand uvm_reg_field INGRESS_MST_TABLE_STP_STATE_15[4096];
	rand uvm_reg_field STP_STATE_15[4096];
	rand uvm_reg_field INGRESS_MST_TABLE_STP_STATE_14[4096];
	rand uvm_reg_field STP_STATE_14[4096];
	rand uvm_reg_field INGRESS_MST_TABLE_STP_STATE_13[4096];
	rand uvm_reg_field STP_STATE_13[4096];
	rand uvm_reg_field INGRESS_MST_TABLE_STP_STATE_12[4096];
	rand uvm_reg_field STP_STATE_12[4096];
	rand uvm_reg_field INGRESS_MST_TABLE_STP_STATE_11[4096];
	rand uvm_reg_field STP_STATE_11[4096];
	rand uvm_reg_field INGRESS_MST_TABLE_STP_STATE_10[4096];
	rand uvm_reg_field STP_STATE_10[4096];
	rand uvm_reg_field INGRESS_MST_TABLE_STP_STATE_9[4096];
	rand uvm_reg_field STP_STATE_9[4096];
	rand uvm_reg_field INGRESS_MST_TABLE_STP_STATE_8[4096];
	rand uvm_reg_field STP_STATE_8[4096];
	rand uvm_reg_field INGRESS_MST_TABLE_STP_STATE_7[4096];
	rand uvm_reg_field STP_STATE_7[4096];
	rand uvm_reg_field INGRESS_MST_TABLE_STP_STATE_6[4096];
	rand uvm_reg_field STP_STATE_6[4096];
	rand uvm_reg_field INGRESS_MST_TABLE_STP_STATE_5[4096];
	rand uvm_reg_field STP_STATE_5[4096];
	rand uvm_reg_field INGRESS_MST_TABLE_STP_STATE_4[4096];
	rand uvm_reg_field STP_STATE_4[4096];
	rand uvm_reg_field INGRESS_MST_TABLE_STP_STATE_3[4096];
	rand uvm_reg_field STP_STATE_3[4096];
	rand uvm_reg_field INGRESS_MST_TABLE_STP_STATE_2[4096];
	rand uvm_reg_field STP_STATE_2[4096];
	rand uvm_reg_field INGRESS_MST_TABLE_STP_STATE_1[4096];
	rand uvm_reg_field STP_STATE_1[4096];
	rand uvm_reg_field INGRESS_MST_TABLE_STP_STATE_0[4096];
	rand uvm_reg_field STP_STATE_0[4096];
	rand uvm_reg_field EGRESS_MST_TABLE_FORWARDING[4096][0:4];
	rand uvm_reg_field FORWARDING[4096][0:4];
	rand uvm_reg_field GLORT_DIRECT_MAP_CTRL_GO_COMPL;
	rand uvm_reg_field GO_COMPL;
	rand uvm_reg_field GLORT_DIRECT_MAP_CTRL_STATUS;
	rand uvm_reg_field STATUS;
	rand uvm_reg_field GLORT_DIRECT_MAP_CTRL_OP_TYPE;
	rand uvm_reg_field OP_TYPE;
	uvm_reg_field GLORT_DIRECT_MAP_CTRL__RSVD0_;
	uvm_reg_field _RSVD0_;
	rand uvm_reg_field GLORT_DIRECT_MAP_CTRL_REG_ID;
	rand uvm_reg_field REG_ID;
	rand uvm_reg_field GLORT_DIRECT_MAP_CTRL_REG_SUB_ID;
	rand uvm_reg_field REG_SUB_ID;
	rand uvm_reg_field GLORT_DIRECT_MAP_CTRL_REG_INDX;
	rand uvm_reg_field REG_INDX;
	rand uvm_reg_field GLORT_DIRECT_MAP_DST0_DEST_MASK;
	rand uvm_reg_field GLORT_DIRECT_MAP_DST1_DEST_MASK;
	rand uvm_reg_field GLORT_DIRECT_MAP_DST2_DEST_MASK;
	rand uvm_reg_field GLORT_DIRECT_MAP_DST3_DEST_MASK;
	uvm_reg_field GLORT_DIRECT_MAP_DST4_RSVD0;
	rand uvm_reg_field GLORT_DIRECT_MAP_DST4_IP_MULTICAST_INDEX;
	rand uvm_reg_field IP_MULTICAST_INDEX;
	rand uvm_reg_field GLORT_DIRECT_MAP_DST4_DEST_MASK;
	uvm_reg_field GLORT_RAM_RSVD0[64];
	rand uvm_reg_field GLORT_RAM_SKIP_DGLORT_DEC[64];
	rand uvm_reg_field SKIP_DGLORT_DEC[64];
	rand uvm_reg_field GLORT_RAM_HASH_ROTATION[64];
	rand uvm_reg_field HASH_ROTATION[64];
	rand uvm_reg_field GLORT_RAM_DEST_COUNT[64];
	rand uvm_reg_field DEST_COUNT[64];
	rand uvm_reg_field GLORT_RAM_RANGE_SUB_INDEX_B[64];
	rand uvm_reg_field RANGE_SUB_INDEX_B[64];
	rand uvm_reg_field GLORT_RAM_RANGE_SUB_INDEX_A[64];
	rand uvm_reg_field RANGE_SUB_INDEX_A[64];
	rand uvm_reg_field GLORT_RAM_DEST_INDEX[64];
	rand uvm_reg_field DEST_INDEX[64];
	rand uvm_reg_field GLORT_RAM_STRICT[64];
	rand uvm_reg_field STRICT[64];
	uvm_reg_field GLORT_CAM_RSVD0[64];
	rand uvm_reg_field GLORT_CAM_KEY_INVERT[64];
	rand uvm_reg_field KEY_INVERT[64];
	rand uvm_reg_field GLORT_CAM_KEY[64];
	rand uvm_reg_field KEY[64];
	rand uvm_reg_field CGRP_USED_TABLE_USED[65];
	rand uvm_reg_field USED[65];

	function new(string name = "mst_glort");
		super.new(name, build_coverage(UVM_NO_COVERAGE));
	endfunction: new

   virtual function void build();
      this.default_map = create_map("", 0, 8, UVM_LITTLE_ENDIAN, 0);
      foreach (this.EGRESS_VID_TABLE[i,j]) begin
         int J = i;
         int K = j;
         this.EGRESS_VID_TABLE[J][K] = ral_reg_mst_glort_EGRESS_VID_TABLE::type_id::create($psprintf("EGRESS_VID_TABLE[%0d][%0d]",J,K),,get_full_name());
         this.EGRESS_VID_TABLE[J][K].configure(this, null, "");
         this.EGRESS_VID_TABLE[J][K].build();
         this.EGRESS_VID_TABLE[J][K].add_hdl_path('{

            '{$psprintf("EGRESS_VID_TABLE[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.EGRESS_VID_TABLE[J][K], `UVM_REG_ADDR_WIDTH'h0+J*`UVM_REG_ADDR_WIDTH'h40+K*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.EGRESS_VID_TABLE_MEMBERSHIP[J][K] = this.EGRESS_VID_TABLE[J][K].MEMBERSHIP;
			this.MEMBERSHIP[J][K] = this.EGRESS_VID_TABLE[J][K].MEMBERSHIP;
      end
      foreach (this.EGRESS_VID_CFG[i]) begin
         int J = i;
         this.EGRESS_VID_CFG[J] = ral_reg_mst_glort_EGRESS_VID_CFG::type_id::create($psprintf("EGRESS_VID_CFG[%0d]",J),,get_full_name());
         this.EGRESS_VID_CFG[J].configure(this, null, "");
         this.EGRESS_VID_CFG[J].build();
         this.EGRESS_VID_CFG[J].add_hdl_path('{

            '{$psprintf("EGRESS_VID_CFG[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.EGRESS_VID_CFG[J], `UVM_REG_ADDR_WIDTH'h40000+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.EGRESS_VID_CFG_RSVD0[J] = this.EGRESS_VID_CFG[J].RSVD0;
			this.EGRESS_VID_CFG_TRIG_ID[J] = this.EGRESS_VID_CFG[J].TRIG_ID;
			this.TRIG_ID[J] = this.EGRESS_VID_CFG[J].TRIG_ID;
      end
      foreach (this.INGRESS_MST_TABLE[i]) begin
         int J = i;
         this.INGRESS_MST_TABLE[J] = ral_reg_mst_glort_INGRESS_MST_TABLE::type_id::create($psprintf("INGRESS_MST_TABLE[%0d]",J),,get_full_name());
         this.INGRESS_MST_TABLE[J].configure(this, null, "");
         this.INGRESS_MST_TABLE[J].build();
         this.INGRESS_MST_TABLE[J].add_hdl_path('{

            '{$psprintf("INGRESS_MST_TABLE[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.INGRESS_MST_TABLE[J], `UVM_REG_ADDR_WIDTH'h48000+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.INGRESS_MST_TABLE_RSVD0[J] = this.INGRESS_MST_TABLE[J].RSVD0;
			this.INGRESS_MST_TABLE_STP_STATE_17[J] = this.INGRESS_MST_TABLE[J].STP_STATE_17;
			this.STP_STATE_17[J] = this.INGRESS_MST_TABLE[J].STP_STATE_17;
			this.INGRESS_MST_TABLE_STP_STATE_16[J] = this.INGRESS_MST_TABLE[J].STP_STATE_16;
			this.STP_STATE_16[J] = this.INGRESS_MST_TABLE[J].STP_STATE_16;
			this.INGRESS_MST_TABLE_STP_STATE_15[J] = this.INGRESS_MST_TABLE[J].STP_STATE_15;
			this.STP_STATE_15[J] = this.INGRESS_MST_TABLE[J].STP_STATE_15;
			this.INGRESS_MST_TABLE_STP_STATE_14[J] = this.INGRESS_MST_TABLE[J].STP_STATE_14;
			this.STP_STATE_14[J] = this.INGRESS_MST_TABLE[J].STP_STATE_14;
			this.INGRESS_MST_TABLE_STP_STATE_13[J] = this.INGRESS_MST_TABLE[J].STP_STATE_13;
			this.STP_STATE_13[J] = this.INGRESS_MST_TABLE[J].STP_STATE_13;
			this.INGRESS_MST_TABLE_STP_STATE_12[J] = this.INGRESS_MST_TABLE[J].STP_STATE_12;
			this.STP_STATE_12[J] = this.INGRESS_MST_TABLE[J].STP_STATE_12;
			this.INGRESS_MST_TABLE_STP_STATE_11[J] = this.INGRESS_MST_TABLE[J].STP_STATE_11;
			this.STP_STATE_11[J] = this.INGRESS_MST_TABLE[J].STP_STATE_11;
			this.INGRESS_MST_TABLE_STP_STATE_10[J] = this.INGRESS_MST_TABLE[J].STP_STATE_10;
			this.STP_STATE_10[J] = this.INGRESS_MST_TABLE[J].STP_STATE_10;
			this.INGRESS_MST_TABLE_STP_STATE_9[J] = this.INGRESS_MST_TABLE[J].STP_STATE_9;
			this.STP_STATE_9[J] = this.INGRESS_MST_TABLE[J].STP_STATE_9;
			this.INGRESS_MST_TABLE_STP_STATE_8[J] = this.INGRESS_MST_TABLE[J].STP_STATE_8;
			this.STP_STATE_8[J] = this.INGRESS_MST_TABLE[J].STP_STATE_8;
			this.INGRESS_MST_TABLE_STP_STATE_7[J] = this.INGRESS_MST_TABLE[J].STP_STATE_7;
			this.STP_STATE_7[J] = this.INGRESS_MST_TABLE[J].STP_STATE_7;
			this.INGRESS_MST_TABLE_STP_STATE_6[J] = this.INGRESS_MST_TABLE[J].STP_STATE_6;
			this.STP_STATE_6[J] = this.INGRESS_MST_TABLE[J].STP_STATE_6;
			this.INGRESS_MST_TABLE_STP_STATE_5[J] = this.INGRESS_MST_TABLE[J].STP_STATE_5;
			this.STP_STATE_5[J] = this.INGRESS_MST_TABLE[J].STP_STATE_5;
			this.INGRESS_MST_TABLE_STP_STATE_4[J] = this.INGRESS_MST_TABLE[J].STP_STATE_4;
			this.STP_STATE_4[J] = this.INGRESS_MST_TABLE[J].STP_STATE_4;
			this.INGRESS_MST_TABLE_STP_STATE_3[J] = this.INGRESS_MST_TABLE[J].STP_STATE_3;
			this.STP_STATE_3[J] = this.INGRESS_MST_TABLE[J].STP_STATE_3;
			this.INGRESS_MST_TABLE_STP_STATE_2[J] = this.INGRESS_MST_TABLE[J].STP_STATE_2;
			this.STP_STATE_2[J] = this.INGRESS_MST_TABLE[J].STP_STATE_2;
			this.INGRESS_MST_TABLE_STP_STATE_1[J] = this.INGRESS_MST_TABLE[J].STP_STATE_1;
			this.STP_STATE_1[J] = this.INGRESS_MST_TABLE[J].STP_STATE_1;
			this.INGRESS_MST_TABLE_STP_STATE_0[J] = this.INGRESS_MST_TABLE[J].STP_STATE_0;
			this.STP_STATE_0[J] = this.INGRESS_MST_TABLE[J].STP_STATE_0;
      end
      foreach (this.EGRESS_MST_TABLE[i,j]) begin
         int J = i;
         int K = j;
         this.EGRESS_MST_TABLE[J][K] = ral_reg_mst_glort_EGRESS_MST_TABLE::type_id::create($psprintf("EGRESS_MST_TABLE[%0d][%0d]",J,K),,get_full_name());
         this.EGRESS_MST_TABLE[J][K].configure(this, null, "");
         this.EGRESS_MST_TABLE[J][K].build();
         this.EGRESS_MST_TABLE[J][K].add_hdl_path('{

            '{$psprintf("EGRESS_MST_TABLE[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.EGRESS_MST_TABLE[J][K], `UVM_REG_ADDR_WIDTH'h80000+J*`UVM_REG_ADDR_WIDTH'h40+K*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.EGRESS_MST_TABLE_FORWARDING[J][K] = this.EGRESS_MST_TABLE[J][K].FORWARDING;
			this.FORWARDING[J][K] = this.EGRESS_MST_TABLE[J][K].FORWARDING;
      end
      this.GLORT_DIRECT_MAP_CTRL = ral_reg_mst_glort_GLORT_DIRECT_MAP_CTRL::type_id::create("GLORT_DIRECT_MAP_CTRL",,get_full_name());
      this.GLORT_DIRECT_MAP_CTRL.configure(this, null, "");
      this.GLORT_DIRECT_MAP_CTRL.build();
         this.GLORT_DIRECT_MAP_CTRL.add_hdl_path('{

            '{"GLORT_DIRECT_MAP_CTRL", -1, -1}
         });
      this.default_map.add_reg(this.GLORT_DIRECT_MAP_CTRL, `UVM_REG_ADDR_WIDTH'hC0000, "RW", 0);
		this.GLORT_DIRECT_MAP_CTRL_GO_COMPL = this.GLORT_DIRECT_MAP_CTRL.GO_COMPL;
		this.GO_COMPL = this.GLORT_DIRECT_MAP_CTRL.GO_COMPL;
		this.GLORT_DIRECT_MAP_CTRL_STATUS = this.GLORT_DIRECT_MAP_CTRL.STATUS;
		this.STATUS = this.GLORT_DIRECT_MAP_CTRL.STATUS;
		this.GLORT_DIRECT_MAP_CTRL_OP_TYPE = this.GLORT_DIRECT_MAP_CTRL.OP_TYPE;
		this.OP_TYPE = this.GLORT_DIRECT_MAP_CTRL.OP_TYPE;
		this.GLORT_DIRECT_MAP_CTRL__RSVD0_ = this.GLORT_DIRECT_MAP_CTRL._RSVD0_;
		this._RSVD0_ = this.GLORT_DIRECT_MAP_CTRL._RSVD0_;
		this.GLORT_DIRECT_MAP_CTRL_REG_ID = this.GLORT_DIRECT_MAP_CTRL.REG_ID;
		this.REG_ID = this.GLORT_DIRECT_MAP_CTRL.REG_ID;
		this.GLORT_DIRECT_MAP_CTRL_REG_SUB_ID = this.GLORT_DIRECT_MAP_CTRL.REG_SUB_ID;
		this.REG_SUB_ID = this.GLORT_DIRECT_MAP_CTRL.REG_SUB_ID;
		this.GLORT_DIRECT_MAP_CTRL_REG_INDX = this.GLORT_DIRECT_MAP_CTRL.REG_INDX;
		this.REG_INDX = this.GLORT_DIRECT_MAP_CTRL.REG_INDX;
      this.GLORT_DIRECT_MAP_DST0 = ral_reg_mst_glort_GLORT_DIRECT_MAP_DST0::type_id::create("GLORT_DIRECT_MAP_DST0",,get_full_name());
      this.GLORT_DIRECT_MAP_DST0.configure(this, null, "");
      this.GLORT_DIRECT_MAP_DST0.build();
         this.GLORT_DIRECT_MAP_DST0.add_hdl_path('{

            '{"GLORT_DIRECT_MAP_DST0", -1, -1}
         });
      this.default_map.add_reg(this.GLORT_DIRECT_MAP_DST0, `UVM_REG_ADDR_WIDTH'hC0008, "RW", 0);
		this.GLORT_DIRECT_MAP_DST0_DEST_MASK = this.GLORT_DIRECT_MAP_DST0.DEST_MASK;
      this.GLORT_DIRECT_MAP_DST1 = ral_reg_mst_glort_GLORT_DIRECT_MAP_DST1::type_id::create("GLORT_DIRECT_MAP_DST1",,get_full_name());
      this.GLORT_DIRECT_MAP_DST1.configure(this, null, "");
      this.GLORT_DIRECT_MAP_DST1.build();
         this.GLORT_DIRECT_MAP_DST1.add_hdl_path('{

            '{"GLORT_DIRECT_MAP_DST1", -1, -1}
         });
      this.default_map.add_reg(this.GLORT_DIRECT_MAP_DST1, `UVM_REG_ADDR_WIDTH'hC0010, "RW", 0);
		this.GLORT_DIRECT_MAP_DST1_DEST_MASK = this.GLORT_DIRECT_MAP_DST1.DEST_MASK;
      this.GLORT_DIRECT_MAP_DST2 = ral_reg_mst_glort_GLORT_DIRECT_MAP_DST2::type_id::create("GLORT_DIRECT_MAP_DST2",,get_full_name());
      this.GLORT_DIRECT_MAP_DST2.configure(this, null, "");
      this.GLORT_DIRECT_MAP_DST2.build();
         this.GLORT_DIRECT_MAP_DST2.add_hdl_path('{

            '{"GLORT_DIRECT_MAP_DST2", -1, -1}
         });
      this.default_map.add_reg(this.GLORT_DIRECT_MAP_DST2, `UVM_REG_ADDR_WIDTH'hC0018, "RW", 0);
		this.GLORT_DIRECT_MAP_DST2_DEST_MASK = this.GLORT_DIRECT_MAP_DST2.DEST_MASK;
      this.GLORT_DIRECT_MAP_DST3 = ral_reg_mst_glort_GLORT_DIRECT_MAP_DST3::type_id::create("GLORT_DIRECT_MAP_DST3",,get_full_name());
      this.GLORT_DIRECT_MAP_DST3.configure(this, null, "");
      this.GLORT_DIRECT_MAP_DST3.build();
         this.GLORT_DIRECT_MAP_DST3.add_hdl_path('{

            '{"GLORT_DIRECT_MAP_DST3", -1, -1}
         });
      this.default_map.add_reg(this.GLORT_DIRECT_MAP_DST3, `UVM_REG_ADDR_WIDTH'hC0020, "RW", 0);
		this.GLORT_DIRECT_MAP_DST3_DEST_MASK = this.GLORT_DIRECT_MAP_DST3.DEST_MASK;
      this.GLORT_DIRECT_MAP_DST4 = ral_reg_mst_glort_GLORT_DIRECT_MAP_DST4::type_id::create("GLORT_DIRECT_MAP_DST4",,get_full_name());
      this.GLORT_DIRECT_MAP_DST4.configure(this, null, "");
      this.GLORT_DIRECT_MAP_DST4.build();
         this.GLORT_DIRECT_MAP_DST4.add_hdl_path('{

            '{"GLORT_DIRECT_MAP_DST4", -1, -1}
         });
      this.default_map.add_reg(this.GLORT_DIRECT_MAP_DST4, `UVM_REG_ADDR_WIDTH'hC0028, "RW", 0);
		this.GLORT_DIRECT_MAP_DST4_RSVD0 = this.GLORT_DIRECT_MAP_DST4.RSVD0;
		this.GLORT_DIRECT_MAP_DST4_IP_MULTICAST_INDEX = this.GLORT_DIRECT_MAP_DST4.IP_MULTICAST_INDEX;
		this.IP_MULTICAST_INDEX = this.GLORT_DIRECT_MAP_DST4.IP_MULTICAST_INDEX;
		this.GLORT_DIRECT_MAP_DST4_DEST_MASK = this.GLORT_DIRECT_MAP_DST4.DEST_MASK;
      foreach (this.GLORT_RAM[i]) begin
         int J = i;
         this.GLORT_RAM[J] = ral_reg_mst_glort_GLORT_RAM::type_id::create($psprintf("GLORT_RAM[%0d]",J),,get_full_name());
         this.GLORT_RAM[J].configure(this, null, "");
         this.GLORT_RAM[J].build();
         this.GLORT_RAM[J].add_hdl_path('{

            '{$psprintf("GLORT_RAM[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.GLORT_RAM[J], `UVM_REG_ADDR_WIDTH'hC0200+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.GLORT_RAM_RSVD0[J] = this.GLORT_RAM[J].RSVD0;
			this.GLORT_RAM_SKIP_DGLORT_DEC[J] = this.GLORT_RAM[J].SKIP_DGLORT_DEC;
			this.SKIP_DGLORT_DEC[J] = this.GLORT_RAM[J].SKIP_DGLORT_DEC;
			this.GLORT_RAM_HASH_ROTATION[J] = this.GLORT_RAM[J].HASH_ROTATION;
			this.HASH_ROTATION[J] = this.GLORT_RAM[J].HASH_ROTATION;
			this.GLORT_RAM_DEST_COUNT[J] = this.GLORT_RAM[J].DEST_COUNT;
			this.DEST_COUNT[J] = this.GLORT_RAM[J].DEST_COUNT;
			this.GLORT_RAM_RANGE_SUB_INDEX_B[J] = this.GLORT_RAM[J].RANGE_SUB_INDEX_B;
			this.RANGE_SUB_INDEX_B[J] = this.GLORT_RAM[J].RANGE_SUB_INDEX_B;
			this.GLORT_RAM_RANGE_SUB_INDEX_A[J] = this.GLORT_RAM[J].RANGE_SUB_INDEX_A;
			this.RANGE_SUB_INDEX_A[J] = this.GLORT_RAM[J].RANGE_SUB_INDEX_A;
			this.GLORT_RAM_DEST_INDEX[J] = this.GLORT_RAM[J].DEST_INDEX;
			this.DEST_INDEX[J] = this.GLORT_RAM[J].DEST_INDEX;
			this.GLORT_RAM_STRICT[J] = this.GLORT_RAM[J].STRICT;
			this.STRICT[J] = this.GLORT_RAM[J].STRICT;
      end
      foreach (this.GLORT_CAM[i]) begin
         int J = i;
         this.GLORT_CAM[J] = ral_reg_mst_glort_GLORT_CAM::type_id::create($psprintf("GLORT_CAM[%0d]",J),,get_full_name());
         this.GLORT_CAM[J].configure(this, null, "");
         this.GLORT_CAM[J].build();
         this.GLORT_CAM[J].add_hdl_path('{

            '{$psprintf("GLORT_CAM[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.GLORT_CAM[J], `UVM_REG_ADDR_WIDTH'hC0400+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.GLORT_CAM_RSVD0[J] = this.GLORT_CAM[J].RSVD0;
			this.GLORT_CAM_KEY_INVERT[J] = this.GLORT_CAM[J].KEY_INVERT;
			this.KEY_INVERT[J] = this.GLORT_CAM[J].KEY_INVERT;
			this.GLORT_CAM_KEY[J] = this.GLORT_CAM[J].KEY;
			this.KEY[J] = this.GLORT_CAM[J].KEY;
      end
      foreach (this.CGRP_USED_TABLE[i]) begin
         int J = i;
         this.CGRP_USED_TABLE[J] = ral_reg_mst_glort_CGRP_USED_TABLE::type_id::create($psprintf("CGRP_USED_TABLE[%0d]",J),,get_full_name());
         this.CGRP_USED_TABLE[J].configure(this, null, "");
         this.CGRP_USED_TABLE[J].build();
         this.CGRP_USED_TABLE[J].add_hdl_path('{

            '{$psprintf("CGRP_USED_TABLE[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.CGRP_USED_TABLE[J], `UVM_REG_ADDR_WIDTH'hC0760+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.CGRP_USED_TABLE_USED[J] = this.CGRP_USED_TABLE[J].USED;
			this.USED[J] = this.CGRP_USED_TABLE[J].USED;
      end
   endfunction : build

	`uvm_object_utils(ral_block_mst_glort)

endclass : ral_block_mst_glort



`endif
