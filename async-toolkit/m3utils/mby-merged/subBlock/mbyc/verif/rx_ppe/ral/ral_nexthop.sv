// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

`ifndef RAL_NEXTHOP
`define RAL_NEXTHOP

import uvm_pkg::*;

class ral_reg_nexthop_NH_CONFIG extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field SWEEPER_RATE;
	rand uvm_reg_field FLOWLET_INT_EN;
	rand uvm_reg_field FLOWLET_ENABLE;

	function new(string name = "nexthop_NH_CONFIG");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 54, 10, "RO", 0, 54'h0, 1, 0, 0);
      this.SWEEPER_RATE = uvm_reg_field::type_id::create("SWEEPER_RATE",,get_full_name());
      this.SWEEPER_RATE.configure(this, 8, 2, "RW", 0, 8'h0, 1, 0, 0);
      this.FLOWLET_INT_EN = uvm_reg_field::type_id::create("FLOWLET_INT_EN",,get_full_name());
      this.FLOWLET_INT_EN.configure(this, 1, 1, "RW", 0, 1'h0, 1, 0, 0);
      this.FLOWLET_ENABLE = uvm_reg_field::type_id::create("FLOWLET_ENABLE",,get_full_name());
      this.FLOWLET_ENABLE.configure(this, 1, 0, "RW", 0, 1'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_nexthop_NH_CONFIG)

endclass : ral_reg_nexthop_NH_CONFIG


class ral_reg_nexthop_NH_STATUS extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field FLOWLET;

	function new(string name = "nexthop_NH_STATUS");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 50, 14, "RO", 0, 50'h0, 1, 0, 0);
      this.FLOWLET = uvm_reg_field::type_id::create("FLOWLET",,get_full_name());
      this.FLOWLET.configure(this, 14, 0, "RW", 0, 14'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_nexthop_NH_STATUS)

endclass : ral_reg_nexthop_NH_STATUS


class ral_reg_nexthop_NH_NEIGHBORS_0 extends uvm_reg;
	rand uvm_reg_field DGLORT;
	rand uvm_reg_field DST_MAC;

	function new(string name = "nexthop_NH_NEIGHBORS_0");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.DGLORT = uvm_reg_field::type_id::create("DGLORT",,get_full_name());
      this.DGLORT.configure(this, 16, 48, "RW", 0, 16'h0, 1, 0, 1);
      this.DST_MAC = uvm_reg_field::type_id::create("DST_MAC",,get_full_name());
      this.DST_MAC.configure(this, 48, 0, "RW", 0, 48'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_nexthop_NH_NEIGHBORS_0)

endclass : ral_reg_nexthop_NH_NEIGHBORS_0


class ral_reg_nexthop_NH_NEIGHBORS_1 extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field ENTRY_TYPE;
	rand uvm_reg_field IPV6_ENTRY;
	rand uvm_reg_field MARK_ROUTED;
	rand uvm_reg_field UPDATE_L3_DOMAIN;
	rand uvm_reg_field UPDATE_L2_DOMAIN;
	rand uvm_reg_field L3_DOMAIN;
	rand uvm_reg_field L2_DOMAIN;
	rand uvm_reg_field MOD_IDX;
	rand uvm_reg_field MTU_INDEX;
	rand uvm_reg_field EVID;

	function new(string name = "nexthop_NH_NEIGHBORS_1");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 6, 58, "RO", 0, 6'h0, 1, 0, 0);
      this.ENTRY_TYPE = uvm_reg_field::type_id::create("ENTRY_TYPE",,get_full_name());
      this.ENTRY_TYPE.configure(this, 1, 57, "RW", 0, 1'h0, 1, 0, 0);
      this.IPV6_ENTRY = uvm_reg_field::type_id::create("IPV6_ENTRY",,get_full_name());
      this.IPV6_ENTRY.configure(this, 1, 56, "RW", 0, 1'h0, 1, 0, 0);
      this.MARK_ROUTED = uvm_reg_field::type_id::create("MARK_ROUTED",,get_full_name());
      this.MARK_ROUTED.configure(this, 1, 55, "RW", 0, 1'h0, 1, 0, 0);
      this.UPDATE_L3_DOMAIN = uvm_reg_field::type_id::create("UPDATE_L3_DOMAIN",,get_full_name());
      this.UPDATE_L3_DOMAIN.configure(this, 1, 54, "RW", 0, 1'h0, 1, 0, 0);
      this.UPDATE_L2_DOMAIN = uvm_reg_field::type_id::create("UPDATE_L2_DOMAIN",,get_full_name());
      this.UPDATE_L2_DOMAIN.configure(this, 1, 53, "RW", 0, 1'h0, 1, 0, 0);
      this.L3_DOMAIN = uvm_reg_field::type_id::create("L3_DOMAIN",,get_full_name());
      this.L3_DOMAIN.configure(this, 6, 47, "RW", 0, 6'h0, 1, 0, 0);
      this.L2_DOMAIN = uvm_reg_field::type_id::create("L2_DOMAIN",,get_full_name());
      this.L2_DOMAIN.configure(this, 8, 39, "RW", 0, 8'h0, 1, 0, 0);
      this.MOD_IDX = uvm_reg_field::type_id::create("MOD_IDX",,get_full_name());
      this.MOD_IDX.configure(this, 24, 15, "RW", 0, 24'h0, 1, 0, 0);
      this.MTU_INDEX = uvm_reg_field::type_id::create("MTU_INDEX",,get_full_name());
      this.MTU_INDEX.configure(this, 3, 12, "RW", 0, 3'h0, 1, 0, 0);
      this.EVID = uvm_reg_field::type_id::create("EVID",,get_full_name());
      this.EVID.configure(this, 12, 0, "RW", 0, 12'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_nexthop_NH_NEIGHBORS_1)

endclass : ral_reg_nexthop_NH_NEIGHBORS_1


class ral_reg_nexthop_NH_GROUPS_0 extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field WEIGHT_ROW_OFFSET;
	rand uvm_reg_field WEIGHT_ROW;
	rand uvm_reg_field N_GROUP_SIZE;
	rand uvm_reg_field N_GROUP_SZ_TYPE;
	rand uvm_reg_field BASE_INDEX;
	rand uvm_reg_field GROUP_TYPE;

	function new(string name = "nexthop_NH_GROUPS_0");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 24, 40, "RO", 0, 24'h0, 1, 0, 1);
      this.WEIGHT_ROW_OFFSET = uvm_reg_field::type_id::create("WEIGHT_ROW_OFFSET",,get_full_name());
      this.WEIGHT_ROW_OFFSET.configure(this, 6, 34, "RW", 0, 6'h0, 1, 0, 0);
      this.WEIGHT_ROW = uvm_reg_field::type_id::create("WEIGHT_ROW",,get_full_name());
      this.WEIGHT_ROW.configure(this, 11, 23, "RW", 0, 11'h0, 1, 0, 0);
      this.N_GROUP_SIZE = uvm_reg_field::type_id::create("N_GROUP_SIZE",,get_full_name());
      this.N_GROUP_SIZE.configure(this, 6, 17, "RW", 0, 6'h0, 1, 0, 0);
      this.N_GROUP_SZ_TYPE = uvm_reg_field::type_id::create("N_GROUP_SZ_TYPE",,get_full_name());
      this.N_GROUP_SZ_TYPE.configure(this, 1, 16, "RW", 0, 1'h0, 1, 0, 0);
      this.BASE_INDEX = uvm_reg_field::type_id::create("BASE_INDEX",,get_full_name());
      this.BASE_INDEX.configure(this, 14, 2, "RW", 0, 14'h0, 1, 0, 0);
      this.GROUP_TYPE = uvm_reg_field::type_id::create("GROUP_TYPE",,get_full_name());
      this.GROUP_TYPE.configure(this, 2, 0, "RW", 0, 2'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_nexthop_NH_GROUPS_0)

endclass : ral_reg_nexthop_NH_GROUPS_0


class ral_reg_nexthop_NH_GROUPS_1 extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field GROUP_MIN_INDEX;
	rand uvm_reg_field FLOWLET_AGE_RESET;
	rand uvm_reg_field FLOWLET_POLICY;
	rand uvm_reg_field R_GROUP_SIZE;
	rand uvm_reg_field R_GROUP_SZ_TYPE;

	function new(string name = "nexthop_NH_GROUPS_1");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 35, 29, "RO", 0, 35'h0, 1, 0, 0);
      this.GROUP_MIN_INDEX = uvm_reg_field::type_id::create("GROUP_MIN_INDEX",,get_full_name());
      this.GROUP_MIN_INDEX.configure(this, 11, 18, "RW", 0, 11'h0, 1, 0, 0);
      this.FLOWLET_AGE_RESET = uvm_reg_field::type_id::create("FLOWLET_AGE_RESET",,get_full_name());
      this.FLOWLET_AGE_RESET.configure(this, 8, 10, "RW", 0, 8'h0, 1, 0, 0);
      this.FLOWLET_POLICY = uvm_reg_field::type_id::create("FLOWLET_POLICY",,get_full_name());
      this.FLOWLET_POLICY.configure(this, 3, 7, "RW", 0, 3'h0, 1, 0, 0);
      this.R_GROUP_SIZE = uvm_reg_field::type_id::create("R_GROUP_SIZE",,get_full_name());
      this.R_GROUP_SIZE.configure(this, 6, 1, "RW", 0, 6'h0, 1, 0, 0);
      this.R_GROUP_SZ_TYPE = uvm_reg_field::type_id::create("R_GROUP_SZ_TYPE",,get_full_name());
      this.R_GROUP_SZ_TYPE.configure(this, 1, 0, "RW", 0, 1'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_nexthop_NH_GROUPS_1)

endclass : ral_reg_nexthop_NH_GROUPS_1


class ral_reg_nexthop_NH_ROUTES extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field AGE_COUNTER;
	rand uvm_reg_field GROUP_IDX;
	rand uvm_reg_field NEIGHBOR_IDX;

	function new(string name = "nexthop_NH_ROUTES");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 30, 34, "RO", 0, 30'h0, 1, 0, 0);
      this.AGE_COUNTER = uvm_reg_field::type_id::create("AGE_COUNTER",,get_full_name());
      this.AGE_COUNTER.configure(this, 8, 26, "RW", 0, 8'h0, 1, 0, 0);
      this.GROUP_IDX = uvm_reg_field::type_id::create("GROUP_IDX",,get_full_name());
      this.GROUP_IDX.configure(this, 12, 14, "RW", 0, 12'h0, 1, 0, 0);
      this.NEIGHBOR_IDX = uvm_reg_field::type_id::create("NEIGHBOR_IDX",,get_full_name());
      this.NEIGHBOR_IDX.configure(this, 14, 0, "RW", 0, 14'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_nexthop_NH_ROUTES)

endclass : ral_reg_nexthop_NH_ROUTES


class ral_reg_nexthop_NH_WEIGHTS extends uvm_reg;
	rand uvm_reg_field WEIGHT_7;
	rand uvm_reg_field WEIGHT_6;
	rand uvm_reg_field WEIGHT_5;
	rand uvm_reg_field WEIGHT_4;
	rand uvm_reg_field WEIGHT_3;
	rand uvm_reg_field WEIGHT_2;
	rand uvm_reg_field WEIGHT_1;
	rand uvm_reg_field WEIGHT_0;

	function new(string name = "nexthop_NH_WEIGHTS");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.WEIGHT_7 = uvm_reg_field::type_id::create("WEIGHT_7",,get_full_name());
      this.WEIGHT_7.configure(this, 8, 56, "RW", 0, 8'h0, 1, 0, 1);
      this.WEIGHT_6 = uvm_reg_field::type_id::create("WEIGHT_6",,get_full_name());
      this.WEIGHT_6.configure(this, 8, 48, "RW", 0, 8'h0, 1, 0, 1);
      this.WEIGHT_5 = uvm_reg_field::type_id::create("WEIGHT_5",,get_full_name());
      this.WEIGHT_5.configure(this, 8, 40, "RW", 0, 8'h0, 1, 0, 1);
      this.WEIGHT_4 = uvm_reg_field::type_id::create("WEIGHT_4",,get_full_name());
      this.WEIGHT_4.configure(this, 8, 32, "RW", 0, 8'h0, 1, 0, 1);
      this.WEIGHT_3 = uvm_reg_field::type_id::create("WEIGHT_3",,get_full_name());
      this.WEIGHT_3.configure(this, 8, 24, "RW", 0, 8'h0, 1, 0, 1);
      this.WEIGHT_2 = uvm_reg_field::type_id::create("WEIGHT_2",,get_full_name());
      this.WEIGHT_2.configure(this, 8, 16, "RW", 0, 8'h0, 1, 0, 1);
      this.WEIGHT_1 = uvm_reg_field::type_id::create("WEIGHT_1",,get_full_name());
      this.WEIGHT_1.configure(this, 8, 8, "RW", 0, 8'h0, 1, 0, 1);
      this.WEIGHT_0 = uvm_reg_field::type_id::create("WEIGHT_0",,get_full_name());
      this.WEIGHT_0.configure(this, 8, 0, "RW", 0, 8'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_nexthop_NH_WEIGHTS)

endclass : ral_reg_nexthop_NH_WEIGHTS


class ral_reg_nexthop_INGRESS_VID_TABLE extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field TRAP_IGMP;
	rand uvm_reg_field REFLECT;
	rand uvm_reg_field MEMBERSHIP;

	function new(string name = "nexthop_INGRESS_VID_TABLE");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 45, 19, "RO", 0, 45'h0, 1, 0, 0);
      this.TRAP_IGMP = uvm_reg_field::type_id::create("TRAP_IGMP",,get_full_name());
      this.TRAP_IGMP.configure(this, 1, 18, "RW", 0, 1'h0, 1, 0, 0);
      this.REFLECT = uvm_reg_field::type_id::create("REFLECT",,get_full_name());
      this.REFLECT.configure(this, 1, 17, "RW", 0, 1'h0, 1, 0, 0);
      this.MEMBERSHIP = uvm_reg_field::type_id::create("MEMBERSHIP",,get_full_name());
      this.MEMBERSHIP.configure(this, 17, 0, "RW", 0, 17'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_nexthop_INGRESS_VID_TABLE)

endclass : ral_reg_nexthop_INGRESS_VID_TABLE


class ral_reg_nexthop_FLOOD_GLORT_TABLE extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field BROADCAST_GLORT;
	rand uvm_reg_field FLOOD_MULTICAST_GLORT;
	rand uvm_reg_field FLOOD_UNICAST_GLORT;

	function new(string name = "nexthop_FLOOD_GLORT_TABLE");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 16, 48, "RO", 0, 16'h0, 1, 0, 1);
      this.BROADCAST_GLORT = uvm_reg_field::type_id::create("BROADCAST_GLORT",,get_full_name());
      this.BROADCAST_GLORT.configure(this, 16, 32, "RW", 0, 16'h0, 1, 0, 1);
      this.FLOOD_MULTICAST_GLORT = uvm_reg_field::type_id::create("FLOOD_MULTICAST_GLORT",,get_full_name());
      this.FLOOD_MULTICAST_GLORT.configure(this, 16, 16, "RW", 0, 16'h0, 1, 0, 1);
      this.FLOOD_UNICAST_GLORT = uvm_reg_field::type_id::create("FLOOD_UNICAST_GLORT",,get_full_name());
      this.FLOOD_UNICAST_GLORT.configure(this, 16, 0, "RW", 0, 16'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_nexthop_FLOOD_GLORT_TABLE)

endclass : ral_reg_nexthop_FLOOD_GLORT_TABLE


class ral_reg_nexthop_NH_USED extends uvm_reg;
	rand uvm_reg_field USED;

	function new(string name = "nexthop_NH_USED");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.USED = uvm_reg_field::type_id::create("USED",,get_full_name());
      this.USED.configure(this, 64, 0, "W1C", 1, 64'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_nexthop_NH_USED)

endclass : ral_reg_nexthop_NH_USED


class ral_reg_nexthop_NH_PATH_CTRS extends uvm_reg;
	rand uvm_reg_field BYTES;

	function new(string name = "nexthop_NH_PATH_CTRS");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.BYTES = uvm_reg_field::type_id::create("BYTES",,get_full_name());
      this.BYTES.configure(this, 64, 0, "RW", 0, 64'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_nexthop_NH_PATH_CTRS)

endclass : ral_reg_nexthop_NH_PATH_CTRS


class ral_reg_nexthop_NH_GROUP_MIN extends uvm_reg;
	rand uvm_reg_field MIN_3;
	rand uvm_reg_field MIN_2;
	rand uvm_reg_field MIN_1;
	rand uvm_reg_field MIN_0;

	function new(string name = "nexthop_NH_GROUP_MIN");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.MIN_3 = uvm_reg_field::type_id::create("MIN_3",,get_full_name());
      this.MIN_3.configure(this, 16, 48, "RW", 0, 16'h0, 1, 0, 1);
      this.MIN_2 = uvm_reg_field::type_id::create("MIN_2",,get_full_name());
      this.MIN_2.configure(this, 16, 32, "RW", 0, 16'h0, 1, 0, 1);
      this.MIN_1 = uvm_reg_field::type_id::create("MIN_1",,get_full_name());
      this.MIN_1.configure(this, 16, 16, "RW", 0, 16'h0, 1, 0, 1);
      this.MIN_0 = uvm_reg_field::type_id::create("MIN_0",,get_full_name());
      this.MIN_0.configure(this, 16, 0, "RW", 0, 16'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_nexthop_NH_GROUP_MIN)

endclass : ral_reg_nexthop_NH_GROUP_MIN


class ral_reg_nexthop_MTU_TABLE extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field MTU;

	function new(string name = "nexthop_MTU_TABLE");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 50, 14, "RO", 0, 50'h0, 1, 0, 0);
      this.MTU = uvm_reg_field::type_id::create("MTU",,get_full_name());
      this.MTU.configure(this, 14, 0, "RW", 0, 14'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_nexthop_MTU_TABLE)

endclass : ral_reg_nexthop_MTU_TABLE


class ral_block_nexthop extends uvm_reg_block;
	rand ral_reg_nexthop_NH_CONFIG NH_CONFIG;
	rand ral_reg_nexthop_NH_STATUS NH_STATUS;
	rand ral_reg_nexthop_NH_NEIGHBORS_0 NH_NEIGHBORS_0[16384];
	rand ral_reg_nexthop_NH_NEIGHBORS_1 NH_NEIGHBORS_1[16384];
	rand ral_reg_nexthop_NH_GROUPS_0 NH_GROUPS_0[4096];
	rand ral_reg_nexthop_NH_GROUPS_1 NH_GROUPS_1[4096];
	rand ral_reg_nexthop_NH_ROUTES NH_ROUTES[16384];
	rand ral_reg_nexthop_NH_WEIGHTS NH_WEIGHTS[2048][0:7];
	rand ral_reg_nexthop_INGRESS_VID_TABLE INGRESS_VID_TABLE[4096];
	rand ral_reg_nexthop_FLOOD_GLORT_TABLE FLOOD_GLORT_TABLE[256];
	rand ral_reg_nexthop_NH_USED NH_USED[256];
	rand ral_reg_nexthop_NH_PATH_CTRS NH_PATH_CTRS[16384];
	rand ral_reg_nexthop_NH_GROUP_MIN NH_GROUP_MIN[512];
	rand ral_reg_nexthop_MTU_TABLE MTU_TABLE[8];
	uvm_reg_field NH_CONFIG_RSVD0;
	rand uvm_reg_field NH_CONFIG_SWEEPER_RATE;
	rand uvm_reg_field SWEEPER_RATE;
	rand uvm_reg_field NH_CONFIG_FLOWLET_INT_EN;
	rand uvm_reg_field FLOWLET_INT_EN;
	rand uvm_reg_field NH_CONFIG_FLOWLET_ENABLE;
	rand uvm_reg_field FLOWLET_ENABLE;
	uvm_reg_field NH_STATUS_RSVD0;
	rand uvm_reg_field NH_STATUS_FLOWLET;
	rand uvm_reg_field FLOWLET;
	rand uvm_reg_field NH_NEIGHBORS_0_DGLORT[16384];
	rand uvm_reg_field DGLORT[16384];
	rand uvm_reg_field NH_NEIGHBORS_0_DST_MAC[16384];
	rand uvm_reg_field DST_MAC[16384];
	uvm_reg_field NH_NEIGHBORS_1_RSVD0[16384];
	rand uvm_reg_field NH_NEIGHBORS_1_ENTRY_TYPE[16384];
	rand uvm_reg_field ENTRY_TYPE[16384];
	rand uvm_reg_field NH_NEIGHBORS_1_IPV6_ENTRY[16384];
	rand uvm_reg_field IPV6_ENTRY[16384];
	rand uvm_reg_field NH_NEIGHBORS_1_MARK_ROUTED[16384];
	rand uvm_reg_field MARK_ROUTED[16384];
	rand uvm_reg_field NH_NEIGHBORS_1_UPDATE_L3_DOMAIN[16384];
	rand uvm_reg_field UPDATE_L3_DOMAIN[16384];
	rand uvm_reg_field NH_NEIGHBORS_1_UPDATE_L2_DOMAIN[16384];
	rand uvm_reg_field UPDATE_L2_DOMAIN[16384];
	rand uvm_reg_field NH_NEIGHBORS_1_L3_DOMAIN[16384];
	rand uvm_reg_field L3_DOMAIN[16384];
	rand uvm_reg_field NH_NEIGHBORS_1_L2_DOMAIN[16384];
	rand uvm_reg_field L2_DOMAIN[16384];
	rand uvm_reg_field NH_NEIGHBORS_1_MOD_IDX[16384];
	rand uvm_reg_field MOD_IDX[16384];
	rand uvm_reg_field NH_NEIGHBORS_1_MTU_INDEX[16384];
	rand uvm_reg_field MTU_INDEX[16384];
	rand uvm_reg_field NH_NEIGHBORS_1_EVID[16384];
	rand uvm_reg_field EVID[16384];
	uvm_reg_field NH_GROUPS_0_RSVD0[4096];
	rand uvm_reg_field NH_GROUPS_0_WEIGHT_ROW_OFFSET[4096];
	rand uvm_reg_field WEIGHT_ROW_OFFSET[4096];
	rand uvm_reg_field NH_GROUPS_0_WEIGHT_ROW[4096];
	rand uvm_reg_field WEIGHT_ROW[4096];
	rand uvm_reg_field NH_GROUPS_0_N_GROUP_SIZE[4096];
	rand uvm_reg_field N_GROUP_SIZE[4096];
	rand uvm_reg_field NH_GROUPS_0_N_GROUP_SZ_TYPE[4096];
	rand uvm_reg_field N_GROUP_SZ_TYPE[4096];
	rand uvm_reg_field NH_GROUPS_0_BASE_INDEX[4096];
	rand uvm_reg_field BASE_INDEX[4096];
	rand uvm_reg_field NH_GROUPS_0_GROUP_TYPE[4096];
	rand uvm_reg_field GROUP_TYPE[4096];
	uvm_reg_field NH_GROUPS_1_RSVD0[4096];
	rand uvm_reg_field NH_GROUPS_1_GROUP_MIN_INDEX[4096];
	rand uvm_reg_field GROUP_MIN_INDEX[4096];
	rand uvm_reg_field NH_GROUPS_1_FLOWLET_AGE_RESET[4096];
	rand uvm_reg_field FLOWLET_AGE_RESET[4096];
	rand uvm_reg_field NH_GROUPS_1_FLOWLET_POLICY[4096];
	rand uvm_reg_field FLOWLET_POLICY[4096];
	rand uvm_reg_field NH_GROUPS_1_R_GROUP_SIZE[4096];
	rand uvm_reg_field R_GROUP_SIZE[4096];
	rand uvm_reg_field NH_GROUPS_1_R_GROUP_SZ_TYPE[4096];
	rand uvm_reg_field R_GROUP_SZ_TYPE[4096];
	uvm_reg_field NH_ROUTES_RSVD0[16384];
	rand uvm_reg_field NH_ROUTES_AGE_COUNTER[16384];
	rand uvm_reg_field AGE_COUNTER[16384];
	rand uvm_reg_field NH_ROUTES_GROUP_IDX[16384];
	rand uvm_reg_field GROUP_IDX[16384];
	rand uvm_reg_field NH_ROUTES_NEIGHBOR_IDX[16384];
	rand uvm_reg_field NEIGHBOR_IDX[16384];
	rand uvm_reg_field NH_WEIGHTS_WEIGHT_7[2048][0:7];
	rand uvm_reg_field WEIGHT_7[2048][0:7];
	rand uvm_reg_field NH_WEIGHTS_WEIGHT_6[2048][0:7];
	rand uvm_reg_field WEIGHT_6[2048][0:7];
	rand uvm_reg_field NH_WEIGHTS_WEIGHT_5[2048][0:7];
	rand uvm_reg_field WEIGHT_5[2048][0:7];
	rand uvm_reg_field NH_WEIGHTS_WEIGHT_4[2048][0:7];
	rand uvm_reg_field WEIGHT_4[2048][0:7];
	rand uvm_reg_field NH_WEIGHTS_WEIGHT_3[2048][0:7];
	rand uvm_reg_field WEIGHT_3[2048][0:7];
	rand uvm_reg_field NH_WEIGHTS_WEIGHT_2[2048][0:7];
	rand uvm_reg_field WEIGHT_2[2048][0:7];
	rand uvm_reg_field NH_WEIGHTS_WEIGHT_1[2048][0:7];
	rand uvm_reg_field WEIGHT_1[2048][0:7];
	rand uvm_reg_field NH_WEIGHTS_WEIGHT_0[2048][0:7];
	rand uvm_reg_field WEIGHT_0[2048][0:7];
	uvm_reg_field INGRESS_VID_TABLE_RSVD0[4096];
	rand uvm_reg_field INGRESS_VID_TABLE_TRAP_IGMP[4096];
	rand uvm_reg_field TRAP_IGMP[4096];
	rand uvm_reg_field INGRESS_VID_TABLE_REFLECT[4096];
	rand uvm_reg_field REFLECT[4096];
	rand uvm_reg_field INGRESS_VID_TABLE_MEMBERSHIP[4096];
	rand uvm_reg_field MEMBERSHIP[4096];
	uvm_reg_field FLOOD_GLORT_TABLE_RSVD0[256];
	rand uvm_reg_field FLOOD_GLORT_TABLE_BROADCAST_GLORT[256];
	rand uvm_reg_field BROADCAST_GLORT[256];
	rand uvm_reg_field FLOOD_GLORT_TABLE_FLOOD_MULTICAST_GLORT[256];
	rand uvm_reg_field FLOOD_MULTICAST_GLORT[256];
	rand uvm_reg_field FLOOD_GLORT_TABLE_FLOOD_UNICAST_GLORT[256];
	rand uvm_reg_field FLOOD_UNICAST_GLORT[256];
	rand uvm_reg_field NH_USED_USED[256];
	rand uvm_reg_field USED[256];
	rand uvm_reg_field NH_PATH_CTRS_BYTES[16384];
	rand uvm_reg_field BYTES[16384];
	rand uvm_reg_field NH_GROUP_MIN_MIN_3[512];
	rand uvm_reg_field MIN_3[512];
	rand uvm_reg_field NH_GROUP_MIN_MIN_2[512];
	rand uvm_reg_field MIN_2[512];
	rand uvm_reg_field NH_GROUP_MIN_MIN_1[512];
	rand uvm_reg_field MIN_1[512];
	rand uvm_reg_field NH_GROUP_MIN_MIN_0[512];
	rand uvm_reg_field MIN_0[512];
	uvm_reg_field MTU_TABLE_RSVD0[8];
	rand uvm_reg_field MTU_TABLE_MTU[8];
	rand uvm_reg_field MTU[8];

	function new(string name = "nexthop");
		super.new(name, build_coverage(UVM_NO_COVERAGE));
	endfunction: new

   virtual function void build();
      this.default_map = create_map("", 0, 8, UVM_LITTLE_ENDIAN, 0);
      this.NH_CONFIG = ral_reg_nexthop_NH_CONFIG::type_id::create("NH_CONFIG",,get_full_name());
      this.NH_CONFIG.configure(this, null, "");
      this.NH_CONFIG.build();
         this.NH_CONFIG.add_hdl_path('{

            '{"NH_CONFIG", -1, -1}
         });
      this.default_map.add_reg(this.NH_CONFIG, `UVM_REG_ADDR_WIDTH'h0, "RW", 0);
		this.NH_CONFIG_RSVD0 = this.NH_CONFIG.RSVD0;
		this.NH_CONFIG_SWEEPER_RATE = this.NH_CONFIG.SWEEPER_RATE;
		this.SWEEPER_RATE = this.NH_CONFIG.SWEEPER_RATE;
		this.NH_CONFIG_FLOWLET_INT_EN = this.NH_CONFIG.FLOWLET_INT_EN;
		this.FLOWLET_INT_EN = this.NH_CONFIG.FLOWLET_INT_EN;
		this.NH_CONFIG_FLOWLET_ENABLE = this.NH_CONFIG.FLOWLET_ENABLE;
		this.FLOWLET_ENABLE = this.NH_CONFIG.FLOWLET_ENABLE;
      this.NH_STATUS = ral_reg_nexthop_NH_STATUS::type_id::create("NH_STATUS",,get_full_name());
      this.NH_STATUS.configure(this, null, "");
      this.NH_STATUS.build();
         this.NH_STATUS.add_hdl_path('{

            '{"NH_STATUS", -1, -1}
         });
      this.default_map.add_reg(this.NH_STATUS, `UVM_REG_ADDR_WIDTH'h8, "RW", 0);
		this.NH_STATUS_RSVD0 = this.NH_STATUS.RSVD0;
		this.NH_STATUS_FLOWLET = this.NH_STATUS.FLOWLET;
		this.FLOWLET = this.NH_STATUS.FLOWLET;
      foreach (this.NH_NEIGHBORS_0[i]) begin
         int J = i;
         this.NH_NEIGHBORS_0[J] = ral_reg_nexthop_NH_NEIGHBORS_0::type_id::create($psprintf("NH_NEIGHBORS_0[%0d]",J),,get_full_name());
         this.NH_NEIGHBORS_0[J].configure(this, null, "");
         this.NH_NEIGHBORS_0[J].build();
         this.NH_NEIGHBORS_0[J].add_hdl_path('{

            '{$psprintf("NH_NEIGHBORS_0[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.NH_NEIGHBORS_0[J], `UVM_REG_ADDR_WIDTH'h20000+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.NH_NEIGHBORS_0_DGLORT[J] = this.NH_NEIGHBORS_0[J].DGLORT;
			this.DGLORT[J] = this.NH_NEIGHBORS_0[J].DGLORT;
			this.NH_NEIGHBORS_0_DST_MAC[J] = this.NH_NEIGHBORS_0[J].DST_MAC;
			this.DST_MAC[J] = this.NH_NEIGHBORS_0[J].DST_MAC;
      end
      foreach (this.NH_NEIGHBORS_1[i]) begin
         int J = i;
         this.NH_NEIGHBORS_1[J] = ral_reg_nexthop_NH_NEIGHBORS_1::type_id::create($psprintf("NH_NEIGHBORS_1[%0d]",J),,get_full_name());
         this.NH_NEIGHBORS_1[J].configure(this, null, "");
         this.NH_NEIGHBORS_1[J].build();
         this.NH_NEIGHBORS_1[J].add_hdl_path('{

            '{$psprintf("NH_NEIGHBORS_1[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.NH_NEIGHBORS_1[J], `UVM_REG_ADDR_WIDTH'h40000+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.NH_NEIGHBORS_1_RSVD0[J] = this.NH_NEIGHBORS_1[J].RSVD0;
			this.NH_NEIGHBORS_1_ENTRY_TYPE[J] = this.NH_NEIGHBORS_1[J].ENTRY_TYPE;
			this.ENTRY_TYPE[J] = this.NH_NEIGHBORS_1[J].ENTRY_TYPE;
			this.NH_NEIGHBORS_1_IPV6_ENTRY[J] = this.NH_NEIGHBORS_1[J].IPV6_ENTRY;
			this.IPV6_ENTRY[J] = this.NH_NEIGHBORS_1[J].IPV6_ENTRY;
			this.NH_NEIGHBORS_1_MARK_ROUTED[J] = this.NH_NEIGHBORS_1[J].MARK_ROUTED;
			this.MARK_ROUTED[J] = this.NH_NEIGHBORS_1[J].MARK_ROUTED;
			this.NH_NEIGHBORS_1_UPDATE_L3_DOMAIN[J] = this.NH_NEIGHBORS_1[J].UPDATE_L3_DOMAIN;
			this.UPDATE_L3_DOMAIN[J] = this.NH_NEIGHBORS_1[J].UPDATE_L3_DOMAIN;
			this.NH_NEIGHBORS_1_UPDATE_L2_DOMAIN[J] = this.NH_NEIGHBORS_1[J].UPDATE_L2_DOMAIN;
			this.UPDATE_L2_DOMAIN[J] = this.NH_NEIGHBORS_1[J].UPDATE_L2_DOMAIN;
			this.NH_NEIGHBORS_1_L3_DOMAIN[J] = this.NH_NEIGHBORS_1[J].L3_DOMAIN;
			this.L3_DOMAIN[J] = this.NH_NEIGHBORS_1[J].L3_DOMAIN;
			this.NH_NEIGHBORS_1_L2_DOMAIN[J] = this.NH_NEIGHBORS_1[J].L2_DOMAIN;
			this.L2_DOMAIN[J] = this.NH_NEIGHBORS_1[J].L2_DOMAIN;
			this.NH_NEIGHBORS_1_MOD_IDX[J] = this.NH_NEIGHBORS_1[J].MOD_IDX;
			this.MOD_IDX[J] = this.NH_NEIGHBORS_1[J].MOD_IDX;
			this.NH_NEIGHBORS_1_MTU_INDEX[J] = this.NH_NEIGHBORS_1[J].MTU_INDEX;
			this.MTU_INDEX[J] = this.NH_NEIGHBORS_1[J].MTU_INDEX;
			this.NH_NEIGHBORS_1_EVID[J] = this.NH_NEIGHBORS_1[J].EVID;
			this.EVID[J] = this.NH_NEIGHBORS_1[J].EVID;
      end
      foreach (this.NH_GROUPS_0[i]) begin
         int J = i;
         this.NH_GROUPS_0[J] = ral_reg_nexthop_NH_GROUPS_0::type_id::create($psprintf("NH_GROUPS_0[%0d]",J),,get_full_name());
         this.NH_GROUPS_0[J].configure(this, null, "");
         this.NH_GROUPS_0[J].build();
         this.NH_GROUPS_0[J].add_hdl_path('{

            '{$psprintf("NH_GROUPS_0[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.NH_GROUPS_0[J], `UVM_REG_ADDR_WIDTH'h60000+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.NH_GROUPS_0_RSVD0[J] = this.NH_GROUPS_0[J].RSVD0;
			this.NH_GROUPS_0_WEIGHT_ROW_OFFSET[J] = this.NH_GROUPS_0[J].WEIGHT_ROW_OFFSET;
			this.WEIGHT_ROW_OFFSET[J] = this.NH_GROUPS_0[J].WEIGHT_ROW_OFFSET;
			this.NH_GROUPS_0_WEIGHT_ROW[J] = this.NH_GROUPS_0[J].WEIGHT_ROW;
			this.WEIGHT_ROW[J] = this.NH_GROUPS_0[J].WEIGHT_ROW;
			this.NH_GROUPS_0_N_GROUP_SIZE[J] = this.NH_GROUPS_0[J].N_GROUP_SIZE;
			this.N_GROUP_SIZE[J] = this.NH_GROUPS_0[J].N_GROUP_SIZE;
			this.NH_GROUPS_0_N_GROUP_SZ_TYPE[J] = this.NH_GROUPS_0[J].N_GROUP_SZ_TYPE;
			this.N_GROUP_SZ_TYPE[J] = this.NH_GROUPS_0[J].N_GROUP_SZ_TYPE;
			this.NH_GROUPS_0_BASE_INDEX[J] = this.NH_GROUPS_0[J].BASE_INDEX;
			this.BASE_INDEX[J] = this.NH_GROUPS_0[J].BASE_INDEX;
			this.NH_GROUPS_0_GROUP_TYPE[J] = this.NH_GROUPS_0[J].GROUP_TYPE;
			this.GROUP_TYPE[J] = this.NH_GROUPS_0[J].GROUP_TYPE;
      end
      foreach (this.NH_GROUPS_1[i]) begin
         int J = i;
         this.NH_GROUPS_1[J] = ral_reg_nexthop_NH_GROUPS_1::type_id::create($psprintf("NH_GROUPS_1[%0d]",J),,get_full_name());
         this.NH_GROUPS_1[J].configure(this, null, "");
         this.NH_GROUPS_1[J].build();
         this.NH_GROUPS_1[J].add_hdl_path('{

            '{$psprintf("NH_GROUPS_1[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.NH_GROUPS_1[J], `UVM_REG_ADDR_WIDTH'h68000+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.NH_GROUPS_1_RSVD0[J] = this.NH_GROUPS_1[J].RSVD0;
			this.NH_GROUPS_1_GROUP_MIN_INDEX[J] = this.NH_GROUPS_1[J].GROUP_MIN_INDEX;
			this.GROUP_MIN_INDEX[J] = this.NH_GROUPS_1[J].GROUP_MIN_INDEX;
			this.NH_GROUPS_1_FLOWLET_AGE_RESET[J] = this.NH_GROUPS_1[J].FLOWLET_AGE_RESET;
			this.FLOWLET_AGE_RESET[J] = this.NH_GROUPS_1[J].FLOWLET_AGE_RESET;
			this.NH_GROUPS_1_FLOWLET_POLICY[J] = this.NH_GROUPS_1[J].FLOWLET_POLICY;
			this.FLOWLET_POLICY[J] = this.NH_GROUPS_1[J].FLOWLET_POLICY;
			this.NH_GROUPS_1_R_GROUP_SIZE[J] = this.NH_GROUPS_1[J].R_GROUP_SIZE;
			this.R_GROUP_SIZE[J] = this.NH_GROUPS_1[J].R_GROUP_SIZE;
			this.NH_GROUPS_1_R_GROUP_SZ_TYPE[J] = this.NH_GROUPS_1[J].R_GROUP_SZ_TYPE;
			this.R_GROUP_SZ_TYPE[J] = this.NH_GROUPS_1[J].R_GROUP_SZ_TYPE;
      end
      foreach (this.NH_ROUTES[i]) begin
         int J = i;
         this.NH_ROUTES[J] = ral_reg_nexthop_NH_ROUTES::type_id::create($psprintf("NH_ROUTES[%0d]",J),,get_full_name());
         this.NH_ROUTES[J].configure(this, null, "");
         this.NH_ROUTES[J].build();
         this.NH_ROUTES[J].add_hdl_path('{

            '{$psprintf("NH_ROUTES[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.NH_ROUTES[J], `UVM_REG_ADDR_WIDTH'h80000+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.NH_ROUTES_RSVD0[J] = this.NH_ROUTES[J].RSVD0;
			this.NH_ROUTES_AGE_COUNTER[J] = this.NH_ROUTES[J].AGE_COUNTER;
			this.AGE_COUNTER[J] = this.NH_ROUTES[J].AGE_COUNTER;
			this.NH_ROUTES_GROUP_IDX[J] = this.NH_ROUTES[J].GROUP_IDX;
			this.GROUP_IDX[J] = this.NH_ROUTES[J].GROUP_IDX;
			this.NH_ROUTES_NEIGHBOR_IDX[J] = this.NH_ROUTES[J].NEIGHBOR_IDX;
			this.NEIGHBOR_IDX[J] = this.NH_ROUTES[J].NEIGHBOR_IDX;
      end
      foreach (this.NH_WEIGHTS[i,j]) begin
         int J = i;
         int K = j;
         this.NH_WEIGHTS[J][K] = ral_reg_nexthop_NH_WEIGHTS::type_id::create($psprintf("NH_WEIGHTS[%0d][%0d]",J,K),,get_full_name());
         this.NH_WEIGHTS[J][K].configure(this, null, "");
         this.NH_WEIGHTS[J][K].build();
         this.NH_WEIGHTS[J][K].add_hdl_path('{

            '{$psprintf("NH_WEIGHTS[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.NH_WEIGHTS[J][K], `UVM_REG_ADDR_WIDTH'hA0000+J*`UVM_REG_ADDR_WIDTH'h40+K*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.NH_WEIGHTS_WEIGHT_7[J][K] = this.NH_WEIGHTS[J][K].WEIGHT_7;
			this.WEIGHT_7[J][K] = this.NH_WEIGHTS[J][K].WEIGHT_7;
			this.NH_WEIGHTS_WEIGHT_6[J][K] = this.NH_WEIGHTS[J][K].WEIGHT_6;
			this.WEIGHT_6[J][K] = this.NH_WEIGHTS[J][K].WEIGHT_6;
			this.NH_WEIGHTS_WEIGHT_5[J][K] = this.NH_WEIGHTS[J][K].WEIGHT_5;
			this.WEIGHT_5[J][K] = this.NH_WEIGHTS[J][K].WEIGHT_5;
			this.NH_WEIGHTS_WEIGHT_4[J][K] = this.NH_WEIGHTS[J][K].WEIGHT_4;
			this.WEIGHT_4[J][K] = this.NH_WEIGHTS[J][K].WEIGHT_4;
			this.NH_WEIGHTS_WEIGHT_3[J][K] = this.NH_WEIGHTS[J][K].WEIGHT_3;
			this.WEIGHT_3[J][K] = this.NH_WEIGHTS[J][K].WEIGHT_3;
			this.NH_WEIGHTS_WEIGHT_2[J][K] = this.NH_WEIGHTS[J][K].WEIGHT_2;
			this.WEIGHT_2[J][K] = this.NH_WEIGHTS[J][K].WEIGHT_2;
			this.NH_WEIGHTS_WEIGHT_1[J][K] = this.NH_WEIGHTS[J][K].WEIGHT_1;
			this.WEIGHT_1[J][K] = this.NH_WEIGHTS[J][K].WEIGHT_1;
			this.NH_WEIGHTS_WEIGHT_0[J][K] = this.NH_WEIGHTS[J][K].WEIGHT_0;
			this.WEIGHT_0[J][K] = this.NH_WEIGHTS[J][K].WEIGHT_0;
      end
      foreach (this.INGRESS_VID_TABLE[i]) begin
         int J = i;
         this.INGRESS_VID_TABLE[J] = ral_reg_nexthop_INGRESS_VID_TABLE::type_id::create($psprintf("INGRESS_VID_TABLE[%0d]",J),,get_full_name());
         this.INGRESS_VID_TABLE[J].configure(this, null, "");
         this.INGRESS_VID_TABLE[J].build();
         this.INGRESS_VID_TABLE[J].add_hdl_path('{

            '{$psprintf("INGRESS_VID_TABLE[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.INGRESS_VID_TABLE[J], `UVM_REG_ADDR_WIDTH'hC0000+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.INGRESS_VID_TABLE_RSVD0[J] = this.INGRESS_VID_TABLE[J].RSVD0;
			this.INGRESS_VID_TABLE_TRAP_IGMP[J] = this.INGRESS_VID_TABLE[J].TRAP_IGMP;
			this.TRAP_IGMP[J] = this.INGRESS_VID_TABLE[J].TRAP_IGMP;
			this.INGRESS_VID_TABLE_REFLECT[J] = this.INGRESS_VID_TABLE[J].REFLECT;
			this.REFLECT[J] = this.INGRESS_VID_TABLE[J].REFLECT;
			this.INGRESS_VID_TABLE_MEMBERSHIP[J] = this.INGRESS_VID_TABLE[J].MEMBERSHIP;
			this.MEMBERSHIP[J] = this.INGRESS_VID_TABLE[J].MEMBERSHIP;
      end
      foreach (this.FLOOD_GLORT_TABLE[i]) begin
         int J = i;
         this.FLOOD_GLORT_TABLE[J] = ral_reg_nexthop_FLOOD_GLORT_TABLE::type_id::create($psprintf("FLOOD_GLORT_TABLE[%0d]",J),,get_full_name());
         this.FLOOD_GLORT_TABLE[J].configure(this, null, "");
         this.FLOOD_GLORT_TABLE[J].build();
         this.FLOOD_GLORT_TABLE[J].add_hdl_path('{

            '{$psprintf("FLOOD_GLORT_TABLE[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.FLOOD_GLORT_TABLE[J], `UVM_REG_ADDR_WIDTH'hC8000+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.FLOOD_GLORT_TABLE_RSVD0[J] = this.FLOOD_GLORT_TABLE[J].RSVD0;
			this.FLOOD_GLORT_TABLE_BROADCAST_GLORT[J] = this.FLOOD_GLORT_TABLE[J].BROADCAST_GLORT;
			this.BROADCAST_GLORT[J] = this.FLOOD_GLORT_TABLE[J].BROADCAST_GLORT;
			this.FLOOD_GLORT_TABLE_FLOOD_MULTICAST_GLORT[J] = this.FLOOD_GLORT_TABLE[J].FLOOD_MULTICAST_GLORT;
			this.FLOOD_MULTICAST_GLORT[J] = this.FLOOD_GLORT_TABLE[J].FLOOD_MULTICAST_GLORT;
			this.FLOOD_GLORT_TABLE_FLOOD_UNICAST_GLORT[J] = this.FLOOD_GLORT_TABLE[J].FLOOD_UNICAST_GLORT;
			this.FLOOD_UNICAST_GLORT[J] = this.FLOOD_GLORT_TABLE[J].FLOOD_UNICAST_GLORT;
      end
      foreach (this.NH_USED[i]) begin
         int J = i;
         this.NH_USED[J] = ral_reg_nexthop_NH_USED::type_id::create($psprintf("NH_USED[%0d]",J),,get_full_name());
         this.NH_USED[J].configure(this, null, "");
         this.NH_USED[J].build();
         this.NH_USED[J].add_hdl_path('{

            '{$psprintf("NH_USED[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.NH_USED[J], `UVM_REG_ADDR_WIDTH'hC8800+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.NH_USED_USED[J] = this.NH_USED[J].USED;
			this.USED[J] = this.NH_USED[J].USED;
      end
      foreach (this.NH_PATH_CTRS[i]) begin
         int J = i;
         this.NH_PATH_CTRS[J] = ral_reg_nexthop_NH_PATH_CTRS::type_id::create($psprintf("NH_PATH_CTRS[%0d]",J),,get_full_name());
         this.NH_PATH_CTRS[J].configure(this, null, "");
         this.NH_PATH_CTRS[J].build();
         this.NH_PATH_CTRS[J].add_hdl_path('{

            '{$psprintf("NH_PATH_CTRS[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.NH_PATH_CTRS[J], `UVM_REG_ADDR_WIDTH'hE0000+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.NH_PATH_CTRS_BYTES[J] = this.NH_PATH_CTRS[J].BYTES;
			this.BYTES[J] = this.NH_PATH_CTRS[J].BYTES;
      end
      foreach (this.NH_GROUP_MIN[i]) begin
         int J = i;
         this.NH_GROUP_MIN[J] = ral_reg_nexthop_NH_GROUP_MIN::type_id::create($psprintf("NH_GROUP_MIN[%0d]",J),,get_full_name());
         this.NH_GROUP_MIN[J].configure(this, null, "");
         this.NH_GROUP_MIN[J].build();
         this.NH_GROUP_MIN[J].add_hdl_path('{

            '{$psprintf("NH_GROUP_MIN[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.NH_GROUP_MIN[J], `UVM_REG_ADDR_WIDTH'h100000+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.NH_GROUP_MIN_MIN_3[J] = this.NH_GROUP_MIN[J].MIN_3;
			this.MIN_3[J] = this.NH_GROUP_MIN[J].MIN_3;
			this.NH_GROUP_MIN_MIN_2[J] = this.NH_GROUP_MIN[J].MIN_2;
			this.MIN_2[J] = this.NH_GROUP_MIN[J].MIN_2;
			this.NH_GROUP_MIN_MIN_1[J] = this.NH_GROUP_MIN[J].MIN_1;
			this.MIN_1[J] = this.NH_GROUP_MIN[J].MIN_1;
			this.NH_GROUP_MIN_MIN_0[J] = this.NH_GROUP_MIN[J].MIN_0;
			this.MIN_0[J] = this.NH_GROUP_MIN[J].MIN_0;
      end
      foreach (this.MTU_TABLE[i]) begin
         int J = i;
         this.MTU_TABLE[J] = ral_reg_nexthop_MTU_TABLE::type_id::create($psprintf("MTU_TABLE[%0d]",J),,get_full_name());
         this.MTU_TABLE[J].configure(this, null, "");
         this.MTU_TABLE[J].build();
         this.MTU_TABLE[J].add_hdl_path('{

            '{$psprintf("MTU_TABLE[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.MTU_TABLE[J], `UVM_REG_ADDR_WIDTH'h101000+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.MTU_TABLE_RSVD0[J] = this.MTU_TABLE[J].RSVD0;
			this.MTU_TABLE_MTU[J] = this.MTU_TABLE[J].MTU;
			this.MTU[J] = this.MTU_TABLE[J].MTU;
      end
   endfunction : build

	`uvm_object_utils(ral_block_nexthop)

endclass : ral_block_nexthop



`endif
