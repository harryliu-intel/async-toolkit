`ifndef RAL_TRIG_APPLY
`define RAL_TRIG_APPLY

import uvm_pkg::*;

class ral_reg_trig_apply_TRIGGER_CONDITION_CFG extends uvm_reg;
	uvm_reg_field _RSVD4_;
	rand uvm_reg_field MATCH_TX;
	rand uvm_reg_field MATCH_RANDOM_THRESHOLD;
	rand uvm_reg_field MATCH_RANDOM_IF_LESS;
	rand uvm_reg_field MATCH_RANDOM_NUMBER;
	rand uvm_reg_field MATCH_BY_PRECEDENCE;
	uvm_reg_field _RSVD3_;
	rand uvm_reg_field MATCH_EGRESS_DOMAIN;
	rand uvm_reg_field MATCH_DEST_GLORT;
	uvm_reg_field _RSVD2_;
	rand uvm_reg_field MATCH_TC;
	rand uvm_reg_field MATCH_CGRP;
	rand uvm_reg_field MATCH_VLAN;
	uvm_reg_field _RSVD1_;
	rand uvm_reg_field LEARN;
	uvm_reg_field _RSVD0_;

	function new(string name = "trig_apply_TRIGGER_CONDITION_CFG");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this._RSVD4_ = uvm_reg_field::type_id::create("_RSVD4_",,get_full_name());
      this._RSVD4_.configure(this, 28, 36, "RO", 1, 28'h0, 1, 0, 0);
      this.MATCH_TX = uvm_reg_field::type_id::create("MATCH_TX",,get_full_name());
      this.MATCH_TX.configure(this, 2, 34, "RW", 0, 2'h0, 1, 0, 0);
      this.MATCH_RANDOM_THRESHOLD = uvm_reg_field::type_id::create("MATCH_RANDOM_THRESHOLD",,get_full_name());
      this.MATCH_RANDOM_THRESHOLD.configure(this, 5, 29, "RW", 0, 5'h18, 1, 0, 0);
      this.MATCH_RANDOM_IF_LESS = uvm_reg_field::type_id::create("MATCH_RANDOM_IF_LESS",,get_full_name());
      this.MATCH_RANDOM_IF_LESS.configure(this, 1, 28, "RW", 0, 1'h1, 1, 0, 0);
      this.MATCH_RANDOM_NUMBER = uvm_reg_field::type_id::create("MATCH_RANDOM_NUMBER",,get_full_name());
      this.MATCH_RANDOM_NUMBER.configure(this, 1, 27, "RW", 0, 1'h0, 1, 0, 0);
      this.MATCH_BY_PRECEDENCE = uvm_reg_field::type_id::create("MATCH_BY_PRECEDENCE",,get_full_name());
      this.MATCH_BY_PRECEDENCE.configure(this, 1, 26, "RW", 0, 1'h0, 1, 0, 0);
      this._RSVD3_ = uvm_reg_field::type_id::create("_RSVD3_",,get_full_name());
      this._RSVD3_.configure(this, 4, 22, "RO", 1, 4'h0, 1, 0, 0);
      this.MATCH_EGRESS_DOMAIN = uvm_reg_field::type_id::create("MATCH_EGRESS_DOMAIN",,get_full_name());
      this.MATCH_EGRESS_DOMAIN.configure(this, 2, 20, "RW", 0, 2'h2, 1, 0, 0);
      this.MATCH_DEST_GLORT = uvm_reg_field::type_id::create("MATCH_DEST_GLORT",,get_full_name());
      this.MATCH_DEST_GLORT.configure(this, 2, 18, "RW", 0, 2'h2, 1, 0, 0);
      this._RSVD2_ = uvm_reg_field::type_id::create("_RSVD2_",,get_full_name());
      this._RSVD2_.configure(this, 2, 16, "RO", 1, 2'h0, 1, 0, 0);
      this.MATCH_TC = uvm_reg_field::type_id::create("MATCH_TC",,get_full_name());
      this.MATCH_TC.configure(this, 2, 14, "RW", 0, 2'h2, 1, 0, 0);
      this.MATCH_CGRP = uvm_reg_field::type_id::create("MATCH_CGRP",,get_full_name());
      this.MATCH_CGRP.configure(this, 2, 12, "RW", 0, 2'h2, 1, 0, 0);
      this.MATCH_VLAN = uvm_reg_field::type_id::create("MATCH_VLAN",,get_full_name());
      this.MATCH_VLAN.configure(this, 2, 10, "RW", 0, 2'h2, 1, 0, 0);
      this._RSVD1_ = uvm_reg_field::type_id::create("_RSVD1_",,get_full_name());
      this._RSVD1_.configure(this, 4, 6, "RO", 1, 4'h0, 1, 0, 0);
      this.LEARN = uvm_reg_field::type_id::create("LEARN",,get_full_name());
      this.LEARN.configure(this, 2, 4, "RW", 0, 2'h2, 1, 0, 0);
      this._RSVD0_ = uvm_reg_field::type_id::create("_RSVD0_",,get_full_name());
      this._RSVD0_.configure(this, 4, 0, "RO", 1, 4'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_trig_apply_TRIGGER_CONDITION_CFG)

endclass : ral_reg_trig_apply_TRIGGER_CONDITION_CFG


class ral_reg_trig_apply_TRIGGER_CONDITION_PARAM extends uvm_reg;
	uvm_reg_field _RSVD3_;
	rand uvm_reg_field EGRESS_DOMAIN_MASK;
	uvm_reg_field _RSVD2_;
	rand uvm_reg_field EGRESS_DOMAIN_VALUE;
	uvm_reg_field _RSVD1_;
	rand uvm_reg_field ROUTED_MASK;
	rand uvm_reg_field FRAME_CLASS_MASK;
	rand uvm_reg_field TC;
	rand uvm_reg_field VID_ID;
	uvm_reg_field _RSVD0_;

	function new(string name = "trig_apply_TRIGGER_CONDITION_PARAM");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this._RSVD3_ = uvm_reg_field::type_id::create("_RSVD3_",,get_full_name());
      this._RSVD3_.configure(this, 5, 59, "RO", 1, 5'h0, 1, 0, 0);
      this.EGRESS_DOMAIN_MASK = uvm_reg_field::type_id::create("EGRESS_DOMAIN_MASK",,get_full_name());
      this.EGRESS_DOMAIN_MASK.configure(this, 14, 45, "RW", 0, 14'h0, 1, 0, 0);
      this._RSVD2_ = uvm_reg_field::type_id::create("_RSVD2_",,get_full_name());
      this._RSVD2_.configure(this, 1, 44, "RO", 1, 1'h0, 1, 0, 0);
      this.EGRESS_DOMAIN_VALUE = uvm_reg_field::type_id::create("EGRESS_DOMAIN_VALUE",,get_full_name());
      this.EGRESS_DOMAIN_VALUE.configure(this, 14, 30, "RW", 0, 14'h0, 1, 0, 0);
      this._RSVD1_ = uvm_reg_field::type_id::create("_RSVD1_",,get_full_name());
      this._RSVD1_.configure(this, 4, 26, "RO", 1, 4'h0, 1, 0, 0);
      this.ROUTED_MASK = uvm_reg_field::type_id::create("ROUTED_MASK",,get_full_name());
      this.ROUTED_MASK.configure(this, 2, 24, "RW", 0, 2'h3, 1, 0, 0);
      this.FRAME_CLASS_MASK = uvm_reg_field::type_id::create("FRAME_CLASS_MASK",,get_full_name());
      this.FRAME_CLASS_MASK.configure(this, 3, 21, "RW", 0, 3'h7, 1, 0, 0);
      this.TC = uvm_reg_field::type_id::create("TC",,get_full_name());
      this.TC.configure(this, 3, 18, "RW", 0, 3'h0, 1, 0, 0);
      this.VID_ID = uvm_reg_field::type_id::create("VID_ID",,get_full_name());
      this.VID_ID.configure(this, 6, 12, "RW", 0, 6'h0, 1, 0, 0);
      this._RSVD0_ = uvm_reg_field::type_id::create("_RSVD0_",,get_full_name());
      this._RSVD0_.configure(this, 12, 0, "RO", 1, 12'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_trig_apply_TRIGGER_CONDITION_PARAM)

endclass : ral_reg_trig_apply_TRIGGER_CONDITION_PARAM


class ral_reg_trig_apply_TRIGGER_CONDITION_CGRP extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field CGRP_MASK;
	rand uvm_reg_field CGRP_ID;

	function new(string name = "trig_apply_TRIGGER_CONDITION_CGRP");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 48, 16, "RO", 0, 48'h0, 1, 0, 1);
      this.CGRP_MASK = uvm_reg_field::type_id::create("CGRP_MASK",,get_full_name());
      this.CGRP_MASK.configure(this, 8, 8, "RW", 0, 8'h0, 1, 0, 1);
      this.CGRP_ID = uvm_reg_field::type_id::create("CGRP_ID",,get_full_name());
      this.CGRP_ID.configure(this, 8, 0, "RW", 0, 8'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_trig_apply_TRIGGER_CONDITION_CGRP)

endclass : ral_reg_trig_apply_TRIGGER_CONDITION_CGRP


class ral_reg_trig_apply_TRIGGER_CONDITION_GLORT extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field GLORT_MASK;
	rand uvm_reg_field DEST_GLORT;

	function new(string name = "trig_apply_TRIGGER_CONDITION_GLORT");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 32, 32, "RO", 0, 32'h0, 1, 0, 1);
      this.GLORT_MASK = uvm_reg_field::type_id::create("GLORT_MASK",,get_full_name());
      this.GLORT_MASK.configure(this, 16, 16, "RW", 0, 16'h0, 1, 0, 1);
      this.DEST_GLORT = uvm_reg_field::type_id::create("DEST_GLORT",,get_full_name());
      this.DEST_GLORT.configure(this, 16, 0, "RW", 0, 16'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_trig_apply_TRIGGER_CONDITION_GLORT)

endclass : ral_reg_trig_apply_TRIGGER_CONDITION_GLORT


class ral_reg_trig_apply_TRIGGER_CONDITION_RX extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field SRC_PORT_MASK;

	function new(string name = "trig_apply_TRIGGER_CONDITION_RX");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 46, 18, "RO", 0, 46'h0, 1, 0, 0);
      this.SRC_PORT_MASK = uvm_reg_field::type_id::create("SRC_PORT_MASK",,get_full_name());
      this.SRC_PORT_MASK.configure(this, 18, 0, "RW", 0, 18'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_trig_apply_TRIGGER_CONDITION_RX)

endclass : ral_reg_trig_apply_TRIGGER_CONDITION_RX


class ral_reg_trig_apply_TRIGGER_CONDITION_AMASK_1 extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field HANDLER_ACTION_MASK;

	function new(string name = "trig_apply_TRIGGER_CONDITION_AMASK_1");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 32, 32, "RO", 0, 32'h0, 1, 0, 1);
      this.HANDLER_ACTION_MASK = uvm_reg_field::type_id::create("HANDLER_ACTION_MASK",,get_full_name());
      this.HANDLER_ACTION_MASK.configure(this, 32, 0, "RW", 0, 32'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_trig_apply_TRIGGER_CONDITION_AMASK_1)

endclass : ral_reg_trig_apply_TRIGGER_CONDITION_AMASK_1


class ral_reg_trig_apply_TRIGGER_CONDITION_AMASK_2 extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field HANDLER_ACTION_MASK;

	function new(string name = "trig_apply_TRIGGER_CONDITION_AMASK_2");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 51, 13, "RO", 0, 51'h0, 1, 0, 0);
      this.HANDLER_ACTION_MASK = uvm_reg_field::type_id::create("HANDLER_ACTION_MASK",,get_full_name());
      this.HANDLER_ACTION_MASK.configure(this, 13, 0, "RW", 0, 13'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_trig_apply_TRIGGER_CONDITION_AMASK_2)

endclass : ral_reg_trig_apply_TRIGGER_CONDITION_AMASK_2


class ral_reg_trig_apply_TRIGGER_ACTION_CFG_1 extends uvm_reg;
	uvm_reg_field _RSVD2_;
	rand uvm_reg_field MIRRORING_ACTION3;
	rand uvm_reg_field MIRRORING_ACTION2;
	rand uvm_reg_field MIRRORING_ACTION1;
	rand uvm_reg_field MIRRORING_ACTION0;
	rand uvm_reg_field NO_MODIFY_ACTION;
	rand uvm_reg_field POLICER_ACTION;
	rand uvm_reg_field EGRESS_L3_DOMAIN_ACTION;
	rand uvm_reg_field EGRESS_L2_DOMAIN_ACTION;
	uvm_reg_field _RSVD1_;
	rand uvm_reg_field RATE_LIMIT_ACTION;
	rand uvm_reg_field LEARNING_ACTION;
	rand uvm_reg_field VLAN_ACTION;
	rand uvm_reg_field TC_ACTION;
	uvm_reg_field _RSVD0_;
	rand uvm_reg_field TRAP_ACTION;
	rand uvm_reg_field FORWARDING_ACTION;

	function new(string name = "trig_apply_TRIGGER_ACTION_CFG_1");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this._RSVD2_ = uvm_reg_field::type_id::create("_RSVD2_",,get_full_name());
      this._RSVD2_.configure(this, 35, 29, "RO", 1, 35'h0, 1, 0, 0);
      this.MIRRORING_ACTION3 = uvm_reg_field::type_id::create("MIRRORING_ACTION3",,get_full_name());
      this.MIRRORING_ACTION3.configure(this, 2, 27, "RW", 0, 2'h0, 1, 0, 0);
      this.MIRRORING_ACTION2 = uvm_reg_field::type_id::create("MIRRORING_ACTION2",,get_full_name());
      this.MIRRORING_ACTION2.configure(this, 2, 25, "RW", 0, 2'h0, 1, 0, 0);
      this.MIRRORING_ACTION1 = uvm_reg_field::type_id::create("MIRRORING_ACTION1",,get_full_name());
      this.MIRRORING_ACTION1.configure(this, 2, 23, "RW", 0, 2'h0, 1, 0, 0);
      this.MIRRORING_ACTION0 = uvm_reg_field::type_id::create("MIRRORING_ACTION0",,get_full_name());
      this.MIRRORING_ACTION0.configure(this, 2, 21, "RW", 0, 2'h0, 1, 0, 0);
      this.NO_MODIFY_ACTION = uvm_reg_field::type_id::create("NO_MODIFY_ACTION",,get_full_name());
      this.NO_MODIFY_ACTION.configure(this, 1, 20, "RW", 0, 1'h0, 1, 0, 0);
      this.POLICER_ACTION = uvm_reg_field::type_id::create("POLICER_ACTION",,get_full_name());
      this.POLICER_ACTION.configure(this, 1, 19, "RW", 0, 1'h0, 1, 0, 0);
      this.EGRESS_L3_DOMAIN_ACTION = uvm_reg_field::type_id::create("EGRESS_L3_DOMAIN_ACTION",,get_full_name());
      this.EGRESS_L3_DOMAIN_ACTION.configure(this, 1, 18, "RW", 0, 1'h0, 1, 0, 0);
      this.EGRESS_L2_DOMAIN_ACTION = uvm_reg_field::type_id::create("EGRESS_L2_DOMAIN_ACTION",,get_full_name());
      this.EGRESS_L2_DOMAIN_ACTION.configure(this, 1, 17, "RW", 0, 1'h0, 1, 0, 0);
      this._RSVD1_ = uvm_reg_field::type_id::create("_RSVD1_",,get_full_name());
      this._RSVD1_.configure(this, 4, 13, "RO", 1, 4'h0, 1, 0, 0);
      this.RATE_LIMIT_ACTION = uvm_reg_field::type_id::create("RATE_LIMIT_ACTION",,get_full_name());
      this.RATE_LIMIT_ACTION.configure(this, 1, 12, "RW", 0, 1'h0, 1, 0, 0);
      this.LEARNING_ACTION = uvm_reg_field::type_id::create("LEARNING_ACTION",,get_full_name());
      this.LEARNING_ACTION.configure(this, 2, 10, "RW", 0, 2'h0, 1, 0, 0);
      this.VLAN_ACTION = uvm_reg_field::type_id::create("VLAN_ACTION",,get_full_name());
      this.VLAN_ACTION.configure(this, 1, 9, "RW", 0, 1'h0, 1, 0, 0);
      this.TC_ACTION = uvm_reg_field::type_id::create("TC_ACTION",,get_full_name());
      this.TC_ACTION.configure(this, 1, 8, "RW", 0, 1'h0, 1, 0, 0);
      this._RSVD0_ = uvm_reg_field::type_id::create("_RSVD0_",,get_full_name());
      this._RSVD0_.configure(this, 4, 4, "RO", 1, 4'h0, 1, 0, 0);
      this.TRAP_ACTION = uvm_reg_field::type_id::create("TRAP_ACTION",,get_full_name());
      this.TRAP_ACTION.configure(this, 2, 2, "RW", 0, 2'h0, 1, 0, 0);
      this.FORWARDING_ACTION = uvm_reg_field::type_id::create("FORWARDING_ACTION",,get_full_name());
      this.FORWARDING_ACTION.configure(this, 2, 0, "RW", 0, 2'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_trig_apply_TRIGGER_ACTION_CFG_1)

endclass : ral_reg_trig_apply_TRIGGER_ACTION_CFG_1


class ral_reg_trig_apply_TRIGGER_ACTION_CFG_2 extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field TRAP_CODE;
	rand uvm_reg_field RATE_LIMIT_NUM;
	rand uvm_reg_field NEW_EVID;
	rand uvm_reg_field NEW_TC;

	function new(string name = "trig_apply_TRIGGER_ACTION_CFG_2");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 42, 22, "RO", 0, 42'h0, 1, 0, 0);
      this.TRAP_CODE = uvm_reg_field::type_id::create("TRAP_CODE",,get_full_name());
      this.TRAP_CODE.configure(this, 3, 19, "RW", 0, 3'h0, 1, 0, 0);
      this.RATE_LIMIT_NUM = uvm_reg_field::type_id::create("RATE_LIMIT_NUM",,get_full_name());
      this.RATE_LIMIT_NUM.configure(this, 4, 15, "RW", 0, 4'h0, 1, 0, 0);
      this.NEW_EVID = uvm_reg_field::type_id::create("NEW_EVID",,get_full_name());
      this.NEW_EVID.configure(this, 12, 3, "RW", 0, 12'h0, 1, 0, 0);
      this.NEW_TC = uvm_reg_field::type_id::create("NEW_TC",,get_full_name());
      this.NEW_TC.configure(this, 3, 0, "RW", 0, 3'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_trig_apply_TRIGGER_ACTION_CFG_2)

endclass : ral_reg_trig_apply_TRIGGER_ACTION_CFG_2


class ral_reg_trig_apply_TRIGGER_ACTION_GLORT extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field NEW_DEST_GLORT_MASK;
	rand uvm_reg_field NEW_DEST_GLORT;

	function new(string name = "trig_apply_TRIGGER_ACTION_GLORT");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 32, 32, "RO", 0, 32'h0, 1, 0, 1);
      this.NEW_DEST_GLORT_MASK = uvm_reg_field::type_id::create("NEW_DEST_GLORT_MASK",,get_full_name());
      this.NEW_DEST_GLORT_MASK.configure(this, 16, 16, "RW", 0, 16'h0, 1, 0, 1);
      this.NEW_DEST_GLORT = uvm_reg_field::type_id::create("NEW_DEST_GLORT",,get_full_name());
      this.NEW_DEST_GLORT.configure(this, 16, 0, "RW", 0, 16'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_trig_apply_TRIGGER_ACTION_GLORT)

endclass : ral_reg_trig_apply_TRIGGER_ACTION_GLORT


class ral_reg_trig_apply_TRIGGER_ACTION_MIRROR extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field MIRROR_PROFILE_INDEX3;
	rand uvm_reg_field MIRROR_PROFILE_INDEX2;
	rand uvm_reg_field MIRROR_PROFILE_INDEX1;
	rand uvm_reg_field MIRROR_PROFILE_INDEX0;

	function new(string name = "trig_apply_TRIGGER_ACTION_MIRROR");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 40, 24, "RO", 0, 40'h0, 1, 0, 1);
      this.MIRROR_PROFILE_INDEX3 = uvm_reg_field::type_id::create("MIRROR_PROFILE_INDEX3",,get_full_name());
      this.MIRROR_PROFILE_INDEX3.configure(this, 6, 18, "RW", 0, 6'h0, 1, 0, 0);
      this.MIRROR_PROFILE_INDEX2 = uvm_reg_field::type_id::create("MIRROR_PROFILE_INDEX2",,get_full_name());
      this.MIRROR_PROFILE_INDEX2.configure(this, 6, 12, "RW", 0, 6'h0, 1, 0, 0);
      this.MIRROR_PROFILE_INDEX1 = uvm_reg_field::type_id::create("MIRROR_PROFILE_INDEX1",,get_full_name());
      this.MIRROR_PROFILE_INDEX1.configure(this, 6, 6, "RW", 0, 6'h0, 1, 0, 0);
      this.MIRROR_PROFILE_INDEX0 = uvm_reg_field::type_id::create("MIRROR_PROFILE_INDEX0",,get_full_name());
      this.MIRROR_PROFILE_INDEX0.configure(this, 6, 0, "RW", 0, 6'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_trig_apply_TRIGGER_ACTION_MIRROR)

endclass : ral_reg_trig_apply_TRIGGER_ACTION_MIRROR


class ral_reg_trig_apply_TRIGGER_STATS extends uvm_reg;
	rand uvm_reg_field COUNT;

	function new(string name = "trig_apply_TRIGGER_STATS");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.COUNT = uvm_reg_field::type_id::create("COUNT",,get_full_name());
      this.COUNT.configure(this, 64, 0, "RW", 1, 64'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_trig_apply_TRIGGER_STATS)

endclass : ral_reg_trig_apply_TRIGGER_STATS


class ral_reg_trig_apply_TRIGGER_DIRECT_MAP_CTRL extends uvm_reg;
	rand uvm_reg_field GO_COMPL;
	rand uvm_reg_field STATUS;
	rand uvm_reg_field OP_TYPE;
	uvm_reg_field _RSVD0_;
	rand uvm_reg_field REG_ID;
	rand uvm_reg_field REG_SUB_ID;
	rand uvm_reg_field REG_INDX;

	function new(string name = "trig_apply_TRIGGER_DIRECT_MAP_CTRL");
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

	`uvm_object_utils(ral_reg_trig_apply_TRIGGER_DIRECT_MAP_CTRL)

endclass : ral_reg_trig_apply_TRIGGER_DIRECT_MAP_CTRL


class ral_reg_trig_apply_TRIGGER_DIRECT_MAP_CTX0 extends uvm_reg;
	rand uvm_reg_field DEST_PORT_MASK;

	function new(string name = "trig_apply_TRIGGER_DIRECT_MAP_CTX0");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.DEST_PORT_MASK = uvm_reg_field::type_id::create("DEST_PORT_MASK",,get_full_name());
      this.DEST_PORT_MASK.configure(this, 64, 0, "RW", 0, 64'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_trig_apply_TRIGGER_DIRECT_MAP_CTX0)

endclass : ral_reg_trig_apply_TRIGGER_DIRECT_MAP_CTX0


class ral_reg_trig_apply_TRIGGER_DIRECT_MAP_CTX1 extends uvm_reg;
	rand uvm_reg_field DEST_PORT_MASK;

	function new(string name = "trig_apply_TRIGGER_DIRECT_MAP_CTX1");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.DEST_PORT_MASK = uvm_reg_field::type_id::create("DEST_PORT_MASK",,get_full_name());
      this.DEST_PORT_MASK.configure(this, 64, 0, "RW", 0, 64'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_trig_apply_TRIGGER_DIRECT_MAP_CTX1)

endclass : ral_reg_trig_apply_TRIGGER_DIRECT_MAP_CTX1


class ral_reg_trig_apply_TRIGGER_DIRECT_MAP_CTX2 extends uvm_reg;
	rand uvm_reg_field DEST_PORT_MASK;

	function new(string name = "trig_apply_TRIGGER_DIRECT_MAP_CTX2");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.DEST_PORT_MASK = uvm_reg_field::type_id::create("DEST_PORT_MASK",,get_full_name());
      this.DEST_PORT_MASK.configure(this, 64, 0, "RW", 0, 64'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_trig_apply_TRIGGER_DIRECT_MAP_CTX2)

endclass : ral_reg_trig_apply_TRIGGER_DIRECT_MAP_CTX2


class ral_reg_trig_apply_TRIGGER_DIRECT_MAP_CTX3 extends uvm_reg;
	rand uvm_reg_field DEST_PORT_MASK;

	function new(string name = "trig_apply_TRIGGER_DIRECT_MAP_CTX3");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.DEST_PORT_MASK = uvm_reg_field::type_id::create("DEST_PORT_MASK",,get_full_name());
      this.DEST_PORT_MASK.configure(this, 64, 0, "RW", 0, 64'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_trig_apply_TRIGGER_DIRECT_MAP_CTX3)

endclass : ral_reg_trig_apply_TRIGGER_DIRECT_MAP_CTX3


class ral_reg_trig_apply_TRIGGER_DIRECT_MAP_CTX4 extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field DEST_PORT_MASK;

	function new(string name = "trig_apply_TRIGGER_DIRECT_MAP_CTX4");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 62, 2, "RO", 0, 62'h0, 1, 0, 0);
      this.DEST_PORT_MASK = uvm_reg_field::type_id::create("DEST_PORT_MASK",,get_full_name());
      this.DEST_PORT_MASK.configure(this, 2, 0, "RW", 0, 2'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_trig_apply_TRIGGER_DIRECT_MAP_CTX4)

endclass : ral_reg_trig_apply_TRIGGER_DIRECT_MAP_CTX4


class ral_reg_trig_apply_TRIGGER_DIRECT_MAP_ADM0 extends uvm_reg;
	rand uvm_reg_field NEW_DEST_MASK;

	function new(string name = "trig_apply_TRIGGER_DIRECT_MAP_ADM0");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.NEW_DEST_MASK = uvm_reg_field::type_id::create("NEW_DEST_MASK",,get_full_name());
      this.NEW_DEST_MASK.configure(this, 64, 0, "RW", 0, 64'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_trig_apply_TRIGGER_DIRECT_MAP_ADM0)

endclass : ral_reg_trig_apply_TRIGGER_DIRECT_MAP_ADM0


class ral_reg_trig_apply_TRIGGER_DIRECT_MAP_ADM1 extends uvm_reg;
	rand uvm_reg_field NEW_DEST_MASK;

	function new(string name = "trig_apply_TRIGGER_DIRECT_MAP_ADM1");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.NEW_DEST_MASK = uvm_reg_field::type_id::create("NEW_DEST_MASK",,get_full_name());
      this.NEW_DEST_MASK.configure(this, 64, 0, "RW", 0, 64'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_trig_apply_TRIGGER_DIRECT_MAP_ADM1)

endclass : ral_reg_trig_apply_TRIGGER_DIRECT_MAP_ADM1


class ral_reg_trig_apply_TRIGGER_DIRECT_MAP_ADM2 extends uvm_reg;
	rand uvm_reg_field NEW_DEST_MASK;

	function new(string name = "trig_apply_TRIGGER_DIRECT_MAP_ADM2");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.NEW_DEST_MASK = uvm_reg_field::type_id::create("NEW_DEST_MASK",,get_full_name());
      this.NEW_DEST_MASK.configure(this, 64, 0, "RW", 0, 64'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_trig_apply_TRIGGER_DIRECT_MAP_ADM2)

endclass : ral_reg_trig_apply_TRIGGER_DIRECT_MAP_ADM2


class ral_reg_trig_apply_TRIGGER_DIRECT_MAP_ADM3 extends uvm_reg;
	rand uvm_reg_field NEW_DEST_MASK;

	function new(string name = "trig_apply_TRIGGER_DIRECT_MAP_ADM3");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.NEW_DEST_MASK = uvm_reg_field::type_id::create("NEW_DEST_MASK",,get_full_name());
      this.NEW_DEST_MASK.configure(this, 64, 0, "RW", 0, 64'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_trig_apply_TRIGGER_DIRECT_MAP_ADM3)

endclass : ral_reg_trig_apply_TRIGGER_DIRECT_MAP_ADM3


class ral_reg_trig_apply_TRIGGER_DIRECT_MAP_ADM4 extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field FILTER_DEST_MASK;
	rand uvm_reg_field NEW_DEST_MASK;

	function new(string name = "trig_apply_TRIGGER_DIRECT_MAP_ADM4");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 61, 3, "RO", 0, 61'h0, 1, 0, 0);
      this.FILTER_DEST_MASK = uvm_reg_field::type_id::create("FILTER_DEST_MASK",,get_full_name());
      this.FILTER_DEST_MASK.configure(this, 1, 2, "RW", 0, 1'h1, 1, 0, 0);
      this.NEW_DEST_MASK = uvm_reg_field::type_id::create("NEW_DEST_MASK",,get_full_name());
      this.NEW_DEST_MASK.configure(this, 2, 0, "RW", 0, 2'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_trig_apply_TRIGGER_DIRECT_MAP_ADM4)

endclass : ral_reg_trig_apply_TRIGGER_DIRECT_MAP_ADM4


class ral_reg_trig_apply_TRIGGER_DIRECT_MAP_ADR0 extends uvm_reg;
	rand uvm_reg_field DROP_MASK;

	function new(string name = "trig_apply_TRIGGER_DIRECT_MAP_ADR0");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.DROP_MASK = uvm_reg_field::type_id::create("DROP_MASK",,get_full_name());
      this.DROP_MASK.configure(this, 64, 0, "RW", 0, 64'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_trig_apply_TRIGGER_DIRECT_MAP_ADR0)

endclass : ral_reg_trig_apply_TRIGGER_DIRECT_MAP_ADR0


class ral_reg_trig_apply_TRIGGER_DIRECT_MAP_ADR1 extends uvm_reg;
	rand uvm_reg_field DROP_MASK;

	function new(string name = "trig_apply_TRIGGER_DIRECT_MAP_ADR1");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.DROP_MASK = uvm_reg_field::type_id::create("DROP_MASK",,get_full_name());
      this.DROP_MASK.configure(this, 64, 0, "RW", 0, 64'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_trig_apply_TRIGGER_DIRECT_MAP_ADR1)

endclass : ral_reg_trig_apply_TRIGGER_DIRECT_MAP_ADR1


class ral_reg_trig_apply_TRIGGER_DIRECT_MAP_ADR2 extends uvm_reg;
	rand uvm_reg_field DROP_MASK;

	function new(string name = "trig_apply_TRIGGER_DIRECT_MAP_ADR2");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.DROP_MASK = uvm_reg_field::type_id::create("DROP_MASK",,get_full_name());
      this.DROP_MASK.configure(this, 64, 0, "RW", 0, 64'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_trig_apply_TRIGGER_DIRECT_MAP_ADR2)

endclass : ral_reg_trig_apply_TRIGGER_DIRECT_MAP_ADR2


class ral_reg_trig_apply_TRIGGER_DIRECT_MAP_ADR3 extends uvm_reg;
	rand uvm_reg_field DROP_MASK;

	function new(string name = "trig_apply_TRIGGER_DIRECT_MAP_ADR3");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.DROP_MASK = uvm_reg_field::type_id::create("DROP_MASK",,get_full_name());
      this.DROP_MASK.configure(this, 64, 0, "RW", 0, 64'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_trig_apply_TRIGGER_DIRECT_MAP_ADR3)

endclass : ral_reg_trig_apply_TRIGGER_DIRECT_MAP_ADR3


class ral_reg_trig_apply_TRIGGER_DIRECT_MAP_ADR4 extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field DROP_MASK;

	function new(string name = "trig_apply_TRIGGER_DIRECT_MAP_ADR4");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 62, 2, "RO", 0, 62'h0, 1, 0, 0);
      this.DROP_MASK = uvm_reg_field::type_id::create("DROP_MASK",,get_full_name());
      this.DROP_MASK.configure(this, 2, 0, "RW", 0, 2'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_trig_apply_TRIGGER_DIRECT_MAP_ADR4)

endclass : ral_reg_trig_apply_TRIGGER_DIRECT_MAP_ADR4


class ral_block_trig_apply extends uvm_reg_block;
	rand ral_reg_trig_apply_TRIGGER_CONDITION_CFG TRIGGER_CONDITION_CFG[96];
	rand ral_reg_trig_apply_TRIGGER_CONDITION_PARAM TRIGGER_CONDITION_PARAM[96];
	rand ral_reg_trig_apply_TRIGGER_CONDITION_CGRP TRIGGER_CONDITION_CGRP[96];
	rand ral_reg_trig_apply_TRIGGER_CONDITION_GLORT TRIGGER_CONDITION_GLORT[96];
	rand ral_reg_trig_apply_TRIGGER_CONDITION_RX TRIGGER_CONDITION_RX[96];
	rand ral_reg_trig_apply_TRIGGER_CONDITION_AMASK_1 TRIGGER_CONDITION_AMASK_1[96];
	rand ral_reg_trig_apply_TRIGGER_CONDITION_AMASK_2 TRIGGER_CONDITION_AMASK_2[96];
	rand ral_reg_trig_apply_TRIGGER_ACTION_CFG_1 TRIGGER_ACTION_CFG_1[96];
	rand ral_reg_trig_apply_TRIGGER_ACTION_CFG_2 TRIGGER_ACTION_CFG_2[96];
	rand ral_reg_trig_apply_TRIGGER_ACTION_GLORT TRIGGER_ACTION_GLORT[96];
	rand ral_reg_trig_apply_TRIGGER_ACTION_MIRROR TRIGGER_ACTION_MIRROR[96];
	rand ral_reg_trig_apply_TRIGGER_STATS TRIGGER_STATS[96];
	rand ral_reg_trig_apply_TRIGGER_DIRECT_MAP_CTRL TRIGGER_DIRECT_MAP_CTRL;
	rand ral_reg_trig_apply_TRIGGER_DIRECT_MAP_CTX0 TRIGGER_DIRECT_MAP_CTX0;
	rand ral_reg_trig_apply_TRIGGER_DIRECT_MAP_CTX1 TRIGGER_DIRECT_MAP_CTX1;
	rand ral_reg_trig_apply_TRIGGER_DIRECT_MAP_CTX2 TRIGGER_DIRECT_MAP_CTX2;
	rand ral_reg_trig_apply_TRIGGER_DIRECT_MAP_CTX3 TRIGGER_DIRECT_MAP_CTX3;
	rand ral_reg_trig_apply_TRIGGER_DIRECT_MAP_CTX4 TRIGGER_DIRECT_MAP_CTX4;
	rand ral_reg_trig_apply_TRIGGER_DIRECT_MAP_ADM0 TRIGGER_DIRECT_MAP_ADM0;
	rand ral_reg_trig_apply_TRIGGER_DIRECT_MAP_ADM1 TRIGGER_DIRECT_MAP_ADM1;
	rand ral_reg_trig_apply_TRIGGER_DIRECT_MAP_ADM2 TRIGGER_DIRECT_MAP_ADM2;
	rand ral_reg_trig_apply_TRIGGER_DIRECT_MAP_ADM3 TRIGGER_DIRECT_MAP_ADM3;
	rand ral_reg_trig_apply_TRIGGER_DIRECT_MAP_ADM4 TRIGGER_DIRECT_MAP_ADM4;
	rand ral_reg_trig_apply_TRIGGER_DIRECT_MAP_ADR0 TRIGGER_DIRECT_MAP_ADR0;
	rand ral_reg_trig_apply_TRIGGER_DIRECT_MAP_ADR1 TRIGGER_DIRECT_MAP_ADR1;
	rand ral_reg_trig_apply_TRIGGER_DIRECT_MAP_ADR2 TRIGGER_DIRECT_MAP_ADR2;
	rand ral_reg_trig_apply_TRIGGER_DIRECT_MAP_ADR3 TRIGGER_DIRECT_MAP_ADR3;
	rand ral_reg_trig_apply_TRIGGER_DIRECT_MAP_ADR4 TRIGGER_DIRECT_MAP_ADR4;
	uvm_reg_field TRIGGER_CONDITION_CFG__RSVD4_[96];
	uvm_reg_field _RSVD4_[96];
	rand uvm_reg_field TRIGGER_CONDITION_CFG_MATCH_TX[96];
	rand uvm_reg_field MATCH_TX[96];
	rand uvm_reg_field TRIGGER_CONDITION_CFG_MATCH_RANDOM_THRESHOLD[96];
	rand uvm_reg_field MATCH_RANDOM_THRESHOLD[96];
	rand uvm_reg_field TRIGGER_CONDITION_CFG_MATCH_RANDOM_IF_LESS[96];
	rand uvm_reg_field MATCH_RANDOM_IF_LESS[96];
	rand uvm_reg_field TRIGGER_CONDITION_CFG_MATCH_RANDOM_NUMBER[96];
	rand uvm_reg_field MATCH_RANDOM_NUMBER[96];
	rand uvm_reg_field TRIGGER_CONDITION_CFG_MATCH_BY_PRECEDENCE[96];
	rand uvm_reg_field MATCH_BY_PRECEDENCE[96];
	uvm_reg_field TRIGGER_CONDITION_CFG__RSVD3_[96];
	rand uvm_reg_field TRIGGER_CONDITION_CFG_MATCH_EGRESS_DOMAIN[96];
	rand uvm_reg_field MATCH_EGRESS_DOMAIN[96];
	rand uvm_reg_field TRIGGER_CONDITION_CFG_MATCH_DEST_GLORT[96];
	rand uvm_reg_field MATCH_DEST_GLORT[96];
	uvm_reg_field TRIGGER_CONDITION_CFG__RSVD2_[96];
	rand uvm_reg_field TRIGGER_CONDITION_CFG_MATCH_TC[96];
	rand uvm_reg_field MATCH_TC[96];
	rand uvm_reg_field TRIGGER_CONDITION_CFG_MATCH_CGRP[96];
	rand uvm_reg_field MATCH_CGRP[96];
	rand uvm_reg_field TRIGGER_CONDITION_CFG_MATCH_VLAN[96];
	rand uvm_reg_field MATCH_VLAN[96];
	uvm_reg_field TRIGGER_CONDITION_CFG__RSVD1_[96];
	rand uvm_reg_field TRIGGER_CONDITION_CFG_LEARN[96];
	rand uvm_reg_field LEARN[96];
	uvm_reg_field TRIGGER_CONDITION_CFG__RSVD0_[96];
	uvm_reg_field TRIGGER_CONDITION_PARAM__RSVD3_[96];
	rand uvm_reg_field TRIGGER_CONDITION_PARAM_EGRESS_DOMAIN_MASK[96];
	rand uvm_reg_field EGRESS_DOMAIN_MASK[96];
	uvm_reg_field TRIGGER_CONDITION_PARAM__RSVD2_[96];
	rand uvm_reg_field TRIGGER_CONDITION_PARAM_EGRESS_DOMAIN_VALUE[96];
	rand uvm_reg_field EGRESS_DOMAIN_VALUE[96];
	uvm_reg_field TRIGGER_CONDITION_PARAM__RSVD1_[96];
	rand uvm_reg_field TRIGGER_CONDITION_PARAM_ROUTED_MASK[96];
	rand uvm_reg_field ROUTED_MASK[96];
	rand uvm_reg_field TRIGGER_CONDITION_PARAM_FRAME_CLASS_MASK[96];
	rand uvm_reg_field FRAME_CLASS_MASK[96];
	rand uvm_reg_field TRIGGER_CONDITION_PARAM_TC[96];
	rand uvm_reg_field TC[96];
	rand uvm_reg_field TRIGGER_CONDITION_PARAM_VID_ID[96];
	rand uvm_reg_field VID_ID[96];
	uvm_reg_field TRIGGER_CONDITION_PARAM__RSVD0_[96];
	uvm_reg_field TRIGGER_CONDITION_CGRP_RSVD0[96];
	rand uvm_reg_field TRIGGER_CONDITION_CGRP_CGRP_MASK[96];
	rand uvm_reg_field CGRP_MASK[96];
	rand uvm_reg_field TRIGGER_CONDITION_CGRP_CGRP_ID[96];
	rand uvm_reg_field CGRP_ID[96];
	uvm_reg_field TRIGGER_CONDITION_GLORT_RSVD0[96];
	rand uvm_reg_field TRIGGER_CONDITION_GLORT_GLORT_MASK[96];
	rand uvm_reg_field GLORT_MASK[96];
	rand uvm_reg_field TRIGGER_CONDITION_GLORT_DEST_GLORT[96];
	rand uvm_reg_field DEST_GLORT[96];
	uvm_reg_field TRIGGER_CONDITION_RX_RSVD0[96];
	rand uvm_reg_field TRIGGER_CONDITION_RX_SRC_PORT_MASK[96];
	rand uvm_reg_field SRC_PORT_MASK[96];
	uvm_reg_field TRIGGER_CONDITION_AMASK_1_RSVD0[96];
	rand uvm_reg_field TRIGGER_CONDITION_AMASK_1_HANDLER_ACTION_MASK[96];
	uvm_reg_field TRIGGER_CONDITION_AMASK_2_RSVD0[96];
	rand uvm_reg_field TRIGGER_CONDITION_AMASK_2_HANDLER_ACTION_MASK[96];
	uvm_reg_field TRIGGER_ACTION_CFG_1__RSVD2_[96];
	rand uvm_reg_field TRIGGER_ACTION_CFG_1_MIRRORING_ACTION3[96];
	rand uvm_reg_field MIRRORING_ACTION3[96];
	rand uvm_reg_field TRIGGER_ACTION_CFG_1_MIRRORING_ACTION2[96];
	rand uvm_reg_field MIRRORING_ACTION2[96];
	rand uvm_reg_field TRIGGER_ACTION_CFG_1_MIRRORING_ACTION1[96];
	rand uvm_reg_field MIRRORING_ACTION1[96];
	rand uvm_reg_field TRIGGER_ACTION_CFG_1_MIRRORING_ACTION0[96];
	rand uvm_reg_field MIRRORING_ACTION0[96];
	rand uvm_reg_field TRIGGER_ACTION_CFG_1_NO_MODIFY_ACTION[96];
	rand uvm_reg_field NO_MODIFY_ACTION[96];
	rand uvm_reg_field TRIGGER_ACTION_CFG_1_POLICER_ACTION[96];
	rand uvm_reg_field POLICER_ACTION[96];
	rand uvm_reg_field TRIGGER_ACTION_CFG_1_EGRESS_L3_DOMAIN_ACTION[96];
	rand uvm_reg_field EGRESS_L3_DOMAIN_ACTION[96];
	rand uvm_reg_field TRIGGER_ACTION_CFG_1_EGRESS_L2_DOMAIN_ACTION[96];
	rand uvm_reg_field EGRESS_L2_DOMAIN_ACTION[96];
	uvm_reg_field TRIGGER_ACTION_CFG_1__RSVD1_[96];
	rand uvm_reg_field TRIGGER_ACTION_CFG_1_RATE_LIMIT_ACTION[96];
	rand uvm_reg_field RATE_LIMIT_ACTION[96];
	rand uvm_reg_field TRIGGER_ACTION_CFG_1_LEARNING_ACTION[96];
	rand uvm_reg_field LEARNING_ACTION[96];
	rand uvm_reg_field TRIGGER_ACTION_CFG_1_VLAN_ACTION[96];
	rand uvm_reg_field VLAN_ACTION[96];
	rand uvm_reg_field TRIGGER_ACTION_CFG_1_TC_ACTION[96];
	rand uvm_reg_field TC_ACTION[96];
	uvm_reg_field TRIGGER_ACTION_CFG_1__RSVD0_[96];
	rand uvm_reg_field TRIGGER_ACTION_CFG_1_TRAP_ACTION[96];
	rand uvm_reg_field TRAP_ACTION[96];
	rand uvm_reg_field TRIGGER_ACTION_CFG_1_FORWARDING_ACTION[96];
	rand uvm_reg_field FORWARDING_ACTION[96];
	uvm_reg_field TRIGGER_ACTION_CFG_2_RSVD0[96];
	rand uvm_reg_field TRIGGER_ACTION_CFG_2_TRAP_CODE[96];
	rand uvm_reg_field TRAP_CODE[96];
	rand uvm_reg_field TRIGGER_ACTION_CFG_2_RATE_LIMIT_NUM[96];
	rand uvm_reg_field RATE_LIMIT_NUM[96];
	rand uvm_reg_field TRIGGER_ACTION_CFG_2_NEW_EVID[96];
	rand uvm_reg_field NEW_EVID[96];
	rand uvm_reg_field TRIGGER_ACTION_CFG_2_NEW_TC[96];
	rand uvm_reg_field NEW_TC[96];
	uvm_reg_field TRIGGER_ACTION_GLORT_RSVD0[96];
	rand uvm_reg_field TRIGGER_ACTION_GLORT_NEW_DEST_GLORT_MASK[96];
	rand uvm_reg_field NEW_DEST_GLORT_MASK[96];
	rand uvm_reg_field TRIGGER_ACTION_GLORT_NEW_DEST_GLORT[96];
	rand uvm_reg_field NEW_DEST_GLORT[96];
	uvm_reg_field TRIGGER_ACTION_MIRROR_RSVD0[96];
	rand uvm_reg_field TRIGGER_ACTION_MIRROR_MIRROR_PROFILE_INDEX3[96];
	rand uvm_reg_field MIRROR_PROFILE_INDEX3[96];
	rand uvm_reg_field TRIGGER_ACTION_MIRROR_MIRROR_PROFILE_INDEX2[96];
	rand uvm_reg_field MIRROR_PROFILE_INDEX2[96];
	rand uvm_reg_field TRIGGER_ACTION_MIRROR_MIRROR_PROFILE_INDEX1[96];
	rand uvm_reg_field MIRROR_PROFILE_INDEX1[96];
	rand uvm_reg_field TRIGGER_ACTION_MIRROR_MIRROR_PROFILE_INDEX0[96];
	rand uvm_reg_field MIRROR_PROFILE_INDEX0[96];
	rand uvm_reg_field TRIGGER_STATS_COUNT[96];
	rand uvm_reg_field COUNT[96];
	rand uvm_reg_field TRIGGER_DIRECT_MAP_CTRL_GO_COMPL;
	rand uvm_reg_field GO_COMPL;
	rand uvm_reg_field TRIGGER_DIRECT_MAP_CTRL_STATUS;
	rand uvm_reg_field STATUS;
	rand uvm_reg_field TRIGGER_DIRECT_MAP_CTRL_OP_TYPE;
	rand uvm_reg_field OP_TYPE;
	uvm_reg_field TRIGGER_DIRECT_MAP_CTRL__RSVD0_;
	rand uvm_reg_field TRIGGER_DIRECT_MAP_CTRL_REG_ID;
	rand uvm_reg_field REG_ID;
	rand uvm_reg_field TRIGGER_DIRECT_MAP_CTRL_REG_SUB_ID;
	rand uvm_reg_field REG_SUB_ID;
	rand uvm_reg_field TRIGGER_DIRECT_MAP_CTRL_REG_INDX;
	rand uvm_reg_field REG_INDX;
	rand uvm_reg_field TRIGGER_DIRECT_MAP_CTX0_DEST_PORT_MASK;
	rand uvm_reg_field TRIGGER_DIRECT_MAP_CTX1_DEST_PORT_MASK;
	rand uvm_reg_field TRIGGER_DIRECT_MAP_CTX2_DEST_PORT_MASK;
	rand uvm_reg_field TRIGGER_DIRECT_MAP_CTX3_DEST_PORT_MASK;
	uvm_reg_field TRIGGER_DIRECT_MAP_CTX4_RSVD0;
	rand uvm_reg_field TRIGGER_DIRECT_MAP_CTX4_DEST_PORT_MASK;
	rand uvm_reg_field TRIGGER_DIRECT_MAP_ADM0_NEW_DEST_MASK;
	rand uvm_reg_field TRIGGER_DIRECT_MAP_ADM1_NEW_DEST_MASK;
	rand uvm_reg_field TRIGGER_DIRECT_MAP_ADM2_NEW_DEST_MASK;
	rand uvm_reg_field TRIGGER_DIRECT_MAP_ADM3_NEW_DEST_MASK;
	uvm_reg_field TRIGGER_DIRECT_MAP_ADM4_RSVD0;
	rand uvm_reg_field TRIGGER_DIRECT_MAP_ADM4_FILTER_DEST_MASK;
	rand uvm_reg_field FILTER_DEST_MASK;
	rand uvm_reg_field TRIGGER_DIRECT_MAP_ADM4_NEW_DEST_MASK;
	rand uvm_reg_field TRIGGER_DIRECT_MAP_ADR0_DROP_MASK;
	rand uvm_reg_field TRIGGER_DIRECT_MAP_ADR1_DROP_MASK;
	rand uvm_reg_field TRIGGER_DIRECT_MAP_ADR2_DROP_MASK;
	rand uvm_reg_field TRIGGER_DIRECT_MAP_ADR3_DROP_MASK;
	uvm_reg_field TRIGGER_DIRECT_MAP_ADR4_RSVD0;
	rand uvm_reg_field TRIGGER_DIRECT_MAP_ADR4_DROP_MASK;

	function new(string name = "trig_apply");
		super.new(name, build_coverage(UVM_NO_COVERAGE));
	endfunction: new

   virtual function void build();
      this.default_map = create_map("", 0, 8, UVM_LITTLE_ENDIAN, 0);
      foreach (this.TRIGGER_CONDITION_CFG[i]) begin
         int J = i;
         this.TRIGGER_CONDITION_CFG[J] = ral_reg_trig_apply_TRIGGER_CONDITION_CFG::type_id::create($psprintf("TRIGGER_CONDITION_CFG[%0d]",J),,get_full_name());
         this.TRIGGER_CONDITION_CFG[J].configure(this, null, "");
         this.TRIGGER_CONDITION_CFG[J].build();
         this.TRIGGER_CONDITION_CFG[J].add_hdl_path('{

            '{$psprintf("TRIGGER_CONDITION_CFG[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.TRIGGER_CONDITION_CFG[J], `UVM_REG_ADDR_WIDTH'h0+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.TRIGGER_CONDITION_CFG__RSVD4_[J] = this.TRIGGER_CONDITION_CFG[J]._RSVD4_;
			this._RSVD4_[J] = this.TRIGGER_CONDITION_CFG[J]._RSVD4_;
			this.TRIGGER_CONDITION_CFG_MATCH_TX[J] = this.TRIGGER_CONDITION_CFG[J].MATCH_TX;
			this.MATCH_TX[J] = this.TRIGGER_CONDITION_CFG[J].MATCH_TX;
			this.TRIGGER_CONDITION_CFG_MATCH_RANDOM_THRESHOLD[J] = this.TRIGGER_CONDITION_CFG[J].MATCH_RANDOM_THRESHOLD;
			this.MATCH_RANDOM_THRESHOLD[J] = this.TRIGGER_CONDITION_CFG[J].MATCH_RANDOM_THRESHOLD;
			this.TRIGGER_CONDITION_CFG_MATCH_RANDOM_IF_LESS[J] = this.TRIGGER_CONDITION_CFG[J].MATCH_RANDOM_IF_LESS;
			this.MATCH_RANDOM_IF_LESS[J] = this.TRIGGER_CONDITION_CFG[J].MATCH_RANDOM_IF_LESS;
			this.TRIGGER_CONDITION_CFG_MATCH_RANDOM_NUMBER[J] = this.TRIGGER_CONDITION_CFG[J].MATCH_RANDOM_NUMBER;
			this.MATCH_RANDOM_NUMBER[J] = this.TRIGGER_CONDITION_CFG[J].MATCH_RANDOM_NUMBER;
			this.TRIGGER_CONDITION_CFG_MATCH_BY_PRECEDENCE[J] = this.TRIGGER_CONDITION_CFG[J].MATCH_BY_PRECEDENCE;
			this.MATCH_BY_PRECEDENCE[J] = this.TRIGGER_CONDITION_CFG[J].MATCH_BY_PRECEDENCE;
			this.TRIGGER_CONDITION_CFG__RSVD3_[J] = this.TRIGGER_CONDITION_CFG[J]._RSVD3_;
			this.TRIGGER_CONDITION_CFG_MATCH_EGRESS_DOMAIN[J] = this.TRIGGER_CONDITION_CFG[J].MATCH_EGRESS_DOMAIN;
			this.MATCH_EGRESS_DOMAIN[J] = this.TRIGGER_CONDITION_CFG[J].MATCH_EGRESS_DOMAIN;
			this.TRIGGER_CONDITION_CFG_MATCH_DEST_GLORT[J] = this.TRIGGER_CONDITION_CFG[J].MATCH_DEST_GLORT;
			this.MATCH_DEST_GLORT[J] = this.TRIGGER_CONDITION_CFG[J].MATCH_DEST_GLORT;
			this.TRIGGER_CONDITION_CFG__RSVD2_[J] = this.TRIGGER_CONDITION_CFG[J]._RSVD2_;
			this.TRIGGER_CONDITION_CFG_MATCH_TC[J] = this.TRIGGER_CONDITION_CFG[J].MATCH_TC;
			this.MATCH_TC[J] = this.TRIGGER_CONDITION_CFG[J].MATCH_TC;
			this.TRIGGER_CONDITION_CFG_MATCH_CGRP[J] = this.TRIGGER_CONDITION_CFG[J].MATCH_CGRP;
			this.MATCH_CGRP[J] = this.TRIGGER_CONDITION_CFG[J].MATCH_CGRP;
			this.TRIGGER_CONDITION_CFG_MATCH_VLAN[J] = this.TRIGGER_CONDITION_CFG[J].MATCH_VLAN;
			this.MATCH_VLAN[J] = this.TRIGGER_CONDITION_CFG[J].MATCH_VLAN;
			this.TRIGGER_CONDITION_CFG__RSVD1_[J] = this.TRIGGER_CONDITION_CFG[J]._RSVD1_;
			this.TRIGGER_CONDITION_CFG_LEARN[J] = this.TRIGGER_CONDITION_CFG[J].LEARN;
			this.LEARN[J] = this.TRIGGER_CONDITION_CFG[J].LEARN;
			this.TRIGGER_CONDITION_CFG__RSVD0_[J] = this.TRIGGER_CONDITION_CFG[J]._RSVD0_;
      end
      foreach (this.TRIGGER_CONDITION_PARAM[i]) begin
         int J = i;
         this.TRIGGER_CONDITION_PARAM[J] = ral_reg_trig_apply_TRIGGER_CONDITION_PARAM::type_id::create($psprintf("TRIGGER_CONDITION_PARAM[%0d]",J),,get_full_name());
         this.TRIGGER_CONDITION_PARAM[J].configure(this, null, "");
         this.TRIGGER_CONDITION_PARAM[J].build();
         this.TRIGGER_CONDITION_PARAM[J].add_hdl_path('{

            '{$psprintf("TRIGGER_CONDITION_PARAM[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.TRIGGER_CONDITION_PARAM[J], `UVM_REG_ADDR_WIDTH'h300+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.TRIGGER_CONDITION_PARAM__RSVD3_[J] = this.TRIGGER_CONDITION_PARAM[J]._RSVD3_;
			this.TRIGGER_CONDITION_PARAM_EGRESS_DOMAIN_MASK[J] = this.TRIGGER_CONDITION_PARAM[J].EGRESS_DOMAIN_MASK;
			this.EGRESS_DOMAIN_MASK[J] = this.TRIGGER_CONDITION_PARAM[J].EGRESS_DOMAIN_MASK;
			this.TRIGGER_CONDITION_PARAM__RSVD2_[J] = this.TRIGGER_CONDITION_PARAM[J]._RSVD2_;
			this.TRIGGER_CONDITION_PARAM_EGRESS_DOMAIN_VALUE[J] = this.TRIGGER_CONDITION_PARAM[J].EGRESS_DOMAIN_VALUE;
			this.EGRESS_DOMAIN_VALUE[J] = this.TRIGGER_CONDITION_PARAM[J].EGRESS_DOMAIN_VALUE;
			this.TRIGGER_CONDITION_PARAM__RSVD1_[J] = this.TRIGGER_CONDITION_PARAM[J]._RSVD1_;
			this.TRIGGER_CONDITION_PARAM_ROUTED_MASK[J] = this.TRIGGER_CONDITION_PARAM[J].ROUTED_MASK;
			this.ROUTED_MASK[J] = this.TRIGGER_CONDITION_PARAM[J].ROUTED_MASK;
			this.TRIGGER_CONDITION_PARAM_FRAME_CLASS_MASK[J] = this.TRIGGER_CONDITION_PARAM[J].FRAME_CLASS_MASK;
			this.FRAME_CLASS_MASK[J] = this.TRIGGER_CONDITION_PARAM[J].FRAME_CLASS_MASK;
			this.TRIGGER_CONDITION_PARAM_TC[J] = this.TRIGGER_CONDITION_PARAM[J].TC;
			this.TC[J] = this.TRIGGER_CONDITION_PARAM[J].TC;
			this.TRIGGER_CONDITION_PARAM_VID_ID[J] = this.TRIGGER_CONDITION_PARAM[J].VID_ID;
			this.VID_ID[J] = this.TRIGGER_CONDITION_PARAM[J].VID_ID;
			this.TRIGGER_CONDITION_PARAM__RSVD0_[J] = this.TRIGGER_CONDITION_PARAM[J]._RSVD0_;
      end
      foreach (this.TRIGGER_CONDITION_CGRP[i]) begin
         int J = i;
         this.TRIGGER_CONDITION_CGRP[J] = ral_reg_trig_apply_TRIGGER_CONDITION_CGRP::type_id::create($psprintf("TRIGGER_CONDITION_CGRP[%0d]",J),,get_full_name());
         this.TRIGGER_CONDITION_CGRP[J].configure(this, null, "");
         this.TRIGGER_CONDITION_CGRP[J].build();
         this.TRIGGER_CONDITION_CGRP[J].add_hdl_path('{

            '{$psprintf("TRIGGER_CONDITION_CGRP[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.TRIGGER_CONDITION_CGRP[J], `UVM_REG_ADDR_WIDTH'h600+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.TRIGGER_CONDITION_CGRP_RSVD0[J] = this.TRIGGER_CONDITION_CGRP[J].RSVD0;
			this.TRIGGER_CONDITION_CGRP_CGRP_MASK[J] = this.TRIGGER_CONDITION_CGRP[J].CGRP_MASK;
			this.CGRP_MASK[J] = this.TRIGGER_CONDITION_CGRP[J].CGRP_MASK;
			this.TRIGGER_CONDITION_CGRP_CGRP_ID[J] = this.TRIGGER_CONDITION_CGRP[J].CGRP_ID;
			this.CGRP_ID[J] = this.TRIGGER_CONDITION_CGRP[J].CGRP_ID;
      end
      foreach (this.TRIGGER_CONDITION_GLORT[i]) begin
         int J = i;
         this.TRIGGER_CONDITION_GLORT[J] = ral_reg_trig_apply_TRIGGER_CONDITION_GLORT::type_id::create($psprintf("TRIGGER_CONDITION_GLORT[%0d]",J),,get_full_name());
         this.TRIGGER_CONDITION_GLORT[J].configure(this, null, "");
         this.TRIGGER_CONDITION_GLORT[J].build();
         this.TRIGGER_CONDITION_GLORT[J].add_hdl_path('{

            '{$psprintf("TRIGGER_CONDITION_GLORT[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.TRIGGER_CONDITION_GLORT[J], `UVM_REG_ADDR_WIDTH'h900+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.TRIGGER_CONDITION_GLORT_RSVD0[J] = this.TRIGGER_CONDITION_GLORT[J].RSVD0;
			this.TRIGGER_CONDITION_GLORT_GLORT_MASK[J] = this.TRIGGER_CONDITION_GLORT[J].GLORT_MASK;
			this.GLORT_MASK[J] = this.TRIGGER_CONDITION_GLORT[J].GLORT_MASK;
			this.TRIGGER_CONDITION_GLORT_DEST_GLORT[J] = this.TRIGGER_CONDITION_GLORT[J].DEST_GLORT;
			this.DEST_GLORT[J] = this.TRIGGER_CONDITION_GLORT[J].DEST_GLORT;
      end
      foreach (this.TRIGGER_CONDITION_RX[i]) begin
         int J = i;
         this.TRIGGER_CONDITION_RX[J] = ral_reg_trig_apply_TRIGGER_CONDITION_RX::type_id::create($psprintf("TRIGGER_CONDITION_RX[%0d]",J),,get_full_name());
         this.TRIGGER_CONDITION_RX[J].configure(this, null, "");
         this.TRIGGER_CONDITION_RX[J].build();
         this.TRIGGER_CONDITION_RX[J].add_hdl_path('{

            '{$psprintf("TRIGGER_CONDITION_RX[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.TRIGGER_CONDITION_RX[J], `UVM_REG_ADDR_WIDTH'hC00+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.TRIGGER_CONDITION_RX_RSVD0[J] = this.TRIGGER_CONDITION_RX[J].RSVD0;
			this.TRIGGER_CONDITION_RX_SRC_PORT_MASK[J] = this.TRIGGER_CONDITION_RX[J].SRC_PORT_MASK;
			this.SRC_PORT_MASK[J] = this.TRIGGER_CONDITION_RX[J].SRC_PORT_MASK;
      end
      foreach (this.TRIGGER_CONDITION_AMASK_1[i]) begin
         int J = i;
         this.TRIGGER_CONDITION_AMASK_1[J] = ral_reg_trig_apply_TRIGGER_CONDITION_AMASK_1::type_id::create($psprintf("TRIGGER_CONDITION_AMASK_1[%0d]",J),,get_full_name());
         this.TRIGGER_CONDITION_AMASK_1[J].configure(this, null, "");
         this.TRIGGER_CONDITION_AMASK_1[J].build();
         this.TRIGGER_CONDITION_AMASK_1[J].add_hdl_path('{

            '{$psprintf("TRIGGER_CONDITION_AMASK_1[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.TRIGGER_CONDITION_AMASK_1[J], `UVM_REG_ADDR_WIDTH'hF00+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.TRIGGER_CONDITION_AMASK_1_RSVD0[J] = this.TRIGGER_CONDITION_AMASK_1[J].RSVD0;
			this.TRIGGER_CONDITION_AMASK_1_HANDLER_ACTION_MASK[J] = this.TRIGGER_CONDITION_AMASK_1[J].HANDLER_ACTION_MASK;
      end
      foreach (this.TRIGGER_CONDITION_AMASK_2[i]) begin
         int J = i;
         this.TRIGGER_CONDITION_AMASK_2[J] = ral_reg_trig_apply_TRIGGER_CONDITION_AMASK_2::type_id::create($psprintf("TRIGGER_CONDITION_AMASK_2[%0d]",J),,get_full_name());
         this.TRIGGER_CONDITION_AMASK_2[J].configure(this, null, "");
         this.TRIGGER_CONDITION_AMASK_2[J].build();
         this.TRIGGER_CONDITION_AMASK_2[J].add_hdl_path('{

            '{$psprintf("TRIGGER_CONDITION_AMASK_2[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.TRIGGER_CONDITION_AMASK_2[J], `UVM_REG_ADDR_WIDTH'h1200+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.TRIGGER_CONDITION_AMASK_2_RSVD0[J] = this.TRIGGER_CONDITION_AMASK_2[J].RSVD0;
			this.TRIGGER_CONDITION_AMASK_2_HANDLER_ACTION_MASK[J] = this.TRIGGER_CONDITION_AMASK_2[J].HANDLER_ACTION_MASK;
      end
      foreach (this.TRIGGER_ACTION_CFG_1[i]) begin
         int J = i;
         this.TRIGGER_ACTION_CFG_1[J] = ral_reg_trig_apply_TRIGGER_ACTION_CFG_1::type_id::create($psprintf("TRIGGER_ACTION_CFG_1[%0d]",J),,get_full_name());
         this.TRIGGER_ACTION_CFG_1[J].configure(this, null, "");
         this.TRIGGER_ACTION_CFG_1[J].build();
         this.TRIGGER_ACTION_CFG_1[J].add_hdl_path('{

            '{$psprintf("TRIGGER_ACTION_CFG_1[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.TRIGGER_ACTION_CFG_1[J], `UVM_REG_ADDR_WIDTH'h1500+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.TRIGGER_ACTION_CFG_1__RSVD2_[J] = this.TRIGGER_ACTION_CFG_1[J]._RSVD2_;
			this.TRIGGER_ACTION_CFG_1_MIRRORING_ACTION3[J] = this.TRIGGER_ACTION_CFG_1[J].MIRRORING_ACTION3;
			this.MIRRORING_ACTION3[J] = this.TRIGGER_ACTION_CFG_1[J].MIRRORING_ACTION3;
			this.TRIGGER_ACTION_CFG_1_MIRRORING_ACTION2[J] = this.TRIGGER_ACTION_CFG_1[J].MIRRORING_ACTION2;
			this.MIRRORING_ACTION2[J] = this.TRIGGER_ACTION_CFG_1[J].MIRRORING_ACTION2;
			this.TRIGGER_ACTION_CFG_1_MIRRORING_ACTION1[J] = this.TRIGGER_ACTION_CFG_1[J].MIRRORING_ACTION1;
			this.MIRRORING_ACTION1[J] = this.TRIGGER_ACTION_CFG_1[J].MIRRORING_ACTION1;
			this.TRIGGER_ACTION_CFG_1_MIRRORING_ACTION0[J] = this.TRIGGER_ACTION_CFG_1[J].MIRRORING_ACTION0;
			this.MIRRORING_ACTION0[J] = this.TRIGGER_ACTION_CFG_1[J].MIRRORING_ACTION0;
			this.TRIGGER_ACTION_CFG_1_NO_MODIFY_ACTION[J] = this.TRIGGER_ACTION_CFG_1[J].NO_MODIFY_ACTION;
			this.NO_MODIFY_ACTION[J] = this.TRIGGER_ACTION_CFG_1[J].NO_MODIFY_ACTION;
			this.TRIGGER_ACTION_CFG_1_POLICER_ACTION[J] = this.TRIGGER_ACTION_CFG_1[J].POLICER_ACTION;
			this.POLICER_ACTION[J] = this.TRIGGER_ACTION_CFG_1[J].POLICER_ACTION;
			this.TRIGGER_ACTION_CFG_1_EGRESS_L3_DOMAIN_ACTION[J] = this.TRIGGER_ACTION_CFG_1[J].EGRESS_L3_DOMAIN_ACTION;
			this.EGRESS_L3_DOMAIN_ACTION[J] = this.TRIGGER_ACTION_CFG_1[J].EGRESS_L3_DOMAIN_ACTION;
			this.TRIGGER_ACTION_CFG_1_EGRESS_L2_DOMAIN_ACTION[J] = this.TRIGGER_ACTION_CFG_1[J].EGRESS_L2_DOMAIN_ACTION;
			this.EGRESS_L2_DOMAIN_ACTION[J] = this.TRIGGER_ACTION_CFG_1[J].EGRESS_L2_DOMAIN_ACTION;
			this.TRIGGER_ACTION_CFG_1__RSVD1_[J] = this.TRIGGER_ACTION_CFG_1[J]._RSVD1_;
			this.TRIGGER_ACTION_CFG_1_RATE_LIMIT_ACTION[J] = this.TRIGGER_ACTION_CFG_1[J].RATE_LIMIT_ACTION;
			this.RATE_LIMIT_ACTION[J] = this.TRIGGER_ACTION_CFG_1[J].RATE_LIMIT_ACTION;
			this.TRIGGER_ACTION_CFG_1_LEARNING_ACTION[J] = this.TRIGGER_ACTION_CFG_1[J].LEARNING_ACTION;
			this.LEARNING_ACTION[J] = this.TRIGGER_ACTION_CFG_1[J].LEARNING_ACTION;
			this.TRIGGER_ACTION_CFG_1_VLAN_ACTION[J] = this.TRIGGER_ACTION_CFG_1[J].VLAN_ACTION;
			this.VLAN_ACTION[J] = this.TRIGGER_ACTION_CFG_1[J].VLAN_ACTION;
			this.TRIGGER_ACTION_CFG_1_TC_ACTION[J] = this.TRIGGER_ACTION_CFG_1[J].TC_ACTION;
			this.TC_ACTION[J] = this.TRIGGER_ACTION_CFG_1[J].TC_ACTION;
			this.TRIGGER_ACTION_CFG_1__RSVD0_[J] = this.TRIGGER_ACTION_CFG_1[J]._RSVD0_;
			this.TRIGGER_ACTION_CFG_1_TRAP_ACTION[J] = this.TRIGGER_ACTION_CFG_1[J].TRAP_ACTION;
			this.TRAP_ACTION[J] = this.TRIGGER_ACTION_CFG_1[J].TRAP_ACTION;
			this.TRIGGER_ACTION_CFG_1_FORWARDING_ACTION[J] = this.TRIGGER_ACTION_CFG_1[J].FORWARDING_ACTION;
			this.FORWARDING_ACTION[J] = this.TRIGGER_ACTION_CFG_1[J].FORWARDING_ACTION;
      end
      foreach (this.TRIGGER_ACTION_CFG_2[i]) begin
         int J = i;
         this.TRIGGER_ACTION_CFG_2[J] = ral_reg_trig_apply_TRIGGER_ACTION_CFG_2::type_id::create($psprintf("TRIGGER_ACTION_CFG_2[%0d]",J),,get_full_name());
         this.TRIGGER_ACTION_CFG_2[J].configure(this, null, "");
         this.TRIGGER_ACTION_CFG_2[J].build();
         this.TRIGGER_ACTION_CFG_2[J].add_hdl_path('{

            '{$psprintf("TRIGGER_ACTION_CFG_2[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.TRIGGER_ACTION_CFG_2[J], `UVM_REG_ADDR_WIDTH'h1800+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.TRIGGER_ACTION_CFG_2_RSVD0[J] = this.TRIGGER_ACTION_CFG_2[J].RSVD0;
			this.TRIGGER_ACTION_CFG_2_TRAP_CODE[J] = this.TRIGGER_ACTION_CFG_2[J].TRAP_CODE;
			this.TRAP_CODE[J] = this.TRIGGER_ACTION_CFG_2[J].TRAP_CODE;
			this.TRIGGER_ACTION_CFG_2_RATE_LIMIT_NUM[J] = this.TRIGGER_ACTION_CFG_2[J].RATE_LIMIT_NUM;
			this.RATE_LIMIT_NUM[J] = this.TRIGGER_ACTION_CFG_2[J].RATE_LIMIT_NUM;
			this.TRIGGER_ACTION_CFG_2_NEW_EVID[J] = this.TRIGGER_ACTION_CFG_2[J].NEW_EVID;
			this.NEW_EVID[J] = this.TRIGGER_ACTION_CFG_2[J].NEW_EVID;
			this.TRIGGER_ACTION_CFG_2_NEW_TC[J] = this.TRIGGER_ACTION_CFG_2[J].NEW_TC;
			this.NEW_TC[J] = this.TRIGGER_ACTION_CFG_2[J].NEW_TC;
      end
      foreach (this.TRIGGER_ACTION_GLORT[i]) begin
         int J = i;
         this.TRIGGER_ACTION_GLORT[J] = ral_reg_trig_apply_TRIGGER_ACTION_GLORT::type_id::create($psprintf("TRIGGER_ACTION_GLORT[%0d]",J),,get_full_name());
         this.TRIGGER_ACTION_GLORT[J].configure(this, null, "");
         this.TRIGGER_ACTION_GLORT[J].build();
         this.TRIGGER_ACTION_GLORT[J].add_hdl_path('{

            '{$psprintf("TRIGGER_ACTION_GLORT[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.TRIGGER_ACTION_GLORT[J], `UVM_REG_ADDR_WIDTH'h1B00+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.TRIGGER_ACTION_GLORT_RSVD0[J] = this.TRIGGER_ACTION_GLORT[J].RSVD0;
			this.TRIGGER_ACTION_GLORT_NEW_DEST_GLORT_MASK[J] = this.TRIGGER_ACTION_GLORT[J].NEW_DEST_GLORT_MASK;
			this.NEW_DEST_GLORT_MASK[J] = this.TRIGGER_ACTION_GLORT[J].NEW_DEST_GLORT_MASK;
			this.TRIGGER_ACTION_GLORT_NEW_DEST_GLORT[J] = this.TRIGGER_ACTION_GLORT[J].NEW_DEST_GLORT;
			this.NEW_DEST_GLORT[J] = this.TRIGGER_ACTION_GLORT[J].NEW_DEST_GLORT;
      end
      foreach (this.TRIGGER_ACTION_MIRROR[i]) begin
         int J = i;
         this.TRIGGER_ACTION_MIRROR[J] = ral_reg_trig_apply_TRIGGER_ACTION_MIRROR::type_id::create($psprintf("TRIGGER_ACTION_MIRROR[%0d]",J),,get_full_name());
         this.TRIGGER_ACTION_MIRROR[J].configure(this, null, "");
         this.TRIGGER_ACTION_MIRROR[J].build();
         this.TRIGGER_ACTION_MIRROR[J].add_hdl_path('{

            '{$psprintf("TRIGGER_ACTION_MIRROR[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.TRIGGER_ACTION_MIRROR[J], `UVM_REG_ADDR_WIDTH'h1E00+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.TRIGGER_ACTION_MIRROR_RSVD0[J] = this.TRIGGER_ACTION_MIRROR[J].RSVD0;
			this.TRIGGER_ACTION_MIRROR_MIRROR_PROFILE_INDEX3[J] = this.TRIGGER_ACTION_MIRROR[J].MIRROR_PROFILE_INDEX3;
			this.MIRROR_PROFILE_INDEX3[J] = this.TRIGGER_ACTION_MIRROR[J].MIRROR_PROFILE_INDEX3;
			this.TRIGGER_ACTION_MIRROR_MIRROR_PROFILE_INDEX2[J] = this.TRIGGER_ACTION_MIRROR[J].MIRROR_PROFILE_INDEX2;
			this.MIRROR_PROFILE_INDEX2[J] = this.TRIGGER_ACTION_MIRROR[J].MIRROR_PROFILE_INDEX2;
			this.TRIGGER_ACTION_MIRROR_MIRROR_PROFILE_INDEX1[J] = this.TRIGGER_ACTION_MIRROR[J].MIRROR_PROFILE_INDEX1;
			this.MIRROR_PROFILE_INDEX1[J] = this.TRIGGER_ACTION_MIRROR[J].MIRROR_PROFILE_INDEX1;
			this.TRIGGER_ACTION_MIRROR_MIRROR_PROFILE_INDEX0[J] = this.TRIGGER_ACTION_MIRROR[J].MIRROR_PROFILE_INDEX0;
			this.MIRROR_PROFILE_INDEX0[J] = this.TRIGGER_ACTION_MIRROR[J].MIRROR_PROFILE_INDEX0;
      end
      foreach (this.TRIGGER_STATS[i]) begin
         int J = i;
         this.TRIGGER_STATS[J] = ral_reg_trig_apply_TRIGGER_STATS::type_id::create($psprintf("TRIGGER_STATS[%0d]",J),,get_full_name());
         this.TRIGGER_STATS[J].configure(this, null, "");
         this.TRIGGER_STATS[J].build();
         this.TRIGGER_STATS[J].add_hdl_path('{

            '{$psprintf("TRIGGER_STATS[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.TRIGGER_STATS[J], `UVM_REG_ADDR_WIDTH'h2100+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.TRIGGER_STATS_COUNT[J] = this.TRIGGER_STATS[J].COUNT;
			this.COUNT[J] = this.TRIGGER_STATS[J].COUNT;
      end
      this.TRIGGER_DIRECT_MAP_CTRL = ral_reg_trig_apply_TRIGGER_DIRECT_MAP_CTRL::type_id::create("TRIGGER_DIRECT_MAP_CTRL",,get_full_name());
      this.TRIGGER_DIRECT_MAP_CTRL.configure(this, null, "");
      this.TRIGGER_DIRECT_MAP_CTRL.build();
         this.TRIGGER_DIRECT_MAP_CTRL.add_hdl_path('{

            '{"TRIGGER_DIRECT_MAP_CTRL", -1, -1}
         });
      this.default_map.add_reg(this.TRIGGER_DIRECT_MAP_CTRL, `UVM_REG_ADDR_WIDTH'h2400, "RW", 0);
		this.TRIGGER_DIRECT_MAP_CTRL_GO_COMPL = this.TRIGGER_DIRECT_MAP_CTRL.GO_COMPL;
		this.GO_COMPL = this.TRIGGER_DIRECT_MAP_CTRL.GO_COMPL;
		this.TRIGGER_DIRECT_MAP_CTRL_STATUS = this.TRIGGER_DIRECT_MAP_CTRL.STATUS;
		this.STATUS = this.TRIGGER_DIRECT_MAP_CTRL.STATUS;
		this.TRIGGER_DIRECT_MAP_CTRL_OP_TYPE = this.TRIGGER_DIRECT_MAP_CTRL.OP_TYPE;
		this.OP_TYPE = this.TRIGGER_DIRECT_MAP_CTRL.OP_TYPE;
		this.TRIGGER_DIRECT_MAP_CTRL__RSVD0_ = this.TRIGGER_DIRECT_MAP_CTRL._RSVD0_;
		this.TRIGGER_DIRECT_MAP_CTRL_REG_ID = this.TRIGGER_DIRECT_MAP_CTRL.REG_ID;
		this.REG_ID = this.TRIGGER_DIRECT_MAP_CTRL.REG_ID;
		this.TRIGGER_DIRECT_MAP_CTRL_REG_SUB_ID = this.TRIGGER_DIRECT_MAP_CTRL.REG_SUB_ID;
		this.REG_SUB_ID = this.TRIGGER_DIRECT_MAP_CTRL.REG_SUB_ID;
		this.TRIGGER_DIRECT_MAP_CTRL_REG_INDX = this.TRIGGER_DIRECT_MAP_CTRL.REG_INDX;
		this.REG_INDX = this.TRIGGER_DIRECT_MAP_CTRL.REG_INDX;
      this.TRIGGER_DIRECT_MAP_CTX0 = ral_reg_trig_apply_TRIGGER_DIRECT_MAP_CTX0::type_id::create("TRIGGER_DIRECT_MAP_CTX0",,get_full_name());
      this.TRIGGER_DIRECT_MAP_CTX0.configure(this, null, "");
      this.TRIGGER_DIRECT_MAP_CTX0.build();
         this.TRIGGER_DIRECT_MAP_CTX0.add_hdl_path('{

            '{"TRIGGER_DIRECT_MAP_CTX0", -1, -1}
         });
      this.default_map.add_reg(this.TRIGGER_DIRECT_MAP_CTX0, `UVM_REG_ADDR_WIDTH'h2408, "RW", 0);
		this.TRIGGER_DIRECT_MAP_CTX0_DEST_PORT_MASK = this.TRIGGER_DIRECT_MAP_CTX0.DEST_PORT_MASK;
      this.TRIGGER_DIRECT_MAP_CTX1 = ral_reg_trig_apply_TRIGGER_DIRECT_MAP_CTX1::type_id::create("TRIGGER_DIRECT_MAP_CTX1",,get_full_name());
      this.TRIGGER_DIRECT_MAP_CTX1.configure(this, null, "");
      this.TRIGGER_DIRECT_MAP_CTX1.build();
         this.TRIGGER_DIRECT_MAP_CTX1.add_hdl_path('{

            '{"TRIGGER_DIRECT_MAP_CTX1", -1, -1}
         });
      this.default_map.add_reg(this.TRIGGER_DIRECT_MAP_CTX1, `UVM_REG_ADDR_WIDTH'h2410, "RW", 0);
		this.TRIGGER_DIRECT_MAP_CTX1_DEST_PORT_MASK = this.TRIGGER_DIRECT_MAP_CTX1.DEST_PORT_MASK;
      this.TRIGGER_DIRECT_MAP_CTX2 = ral_reg_trig_apply_TRIGGER_DIRECT_MAP_CTX2::type_id::create("TRIGGER_DIRECT_MAP_CTX2",,get_full_name());
      this.TRIGGER_DIRECT_MAP_CTX2.configure(this, null, "");
      this.TRIGGER_DIRECT_MAP_CTX2.build();
         this.TRIGGER_DIRECT_MAP_CTX2.add_hdl_path('{

            '{"TRIGGER_DIRECT_MAP_CTX2", -1, -1}
         });
      this.default_map.add_reg(this.TRIGGER_DIRECT_MAP_CTX2, `UVM_REG_ADDR_WIDTH'h2418, "RW", 0);
		this.TRIGGER_DIRECT_MAP_CTX2_DEST_PORT_MASK = this.TRIGGER_DIRECT_MAP_CTX2.DEST_PORT_MASK;
      this.TRIGGER_DIRECT_MAP_CTX3 = ral_reg_trig_apply_TRIGGER_DIRECT_MAP_CTX3::type_id::create("TRIGGER_DIRECT_MAP_CTX3",,get_full_name());
      this.TRIGGER_DIRECT_MAP_CTX3.configure(this, null, "");
      this.TRIGGER_DIRECT_MAP_CTX3.build();
         this.TRIGGER_DIRECT_MAP_CTX3.add_hdl_path('{

            '{"TRIGGER_DIRECT_MAP_CTX3", -1, -1}
         });
      this.default_map.add_reg(this.TRIGGER_DIRECT_MAP_CTX3, `UVM_REG_ADDR_WIDTH'h2420, "RW", 0);
		this.TRIGGER_DIRECT_MAP_CTX3_DEST_PORT_MASK = this.TRIGGER_DIRECT_MAP_CTX3.DEST_PORT_MASK;
      this.TRIGGER_DIRECT_MAP_CTX4 = ral_reg_trig_apply_TRIGGER_DIRECT_MAP_CTX4::type_id::create("TRIGGER_DIRECT_MAP_CTX4",,get_full_name());
      this.TRIGGER_DIRECT_MAP_CTX4.configure(this, null, "");
      this.TRIGGER_DIRECT_MAP_CTX4.build();
         this.TRIGGER_DIRECT_MAP_CTX4.add_hdl_path('{

            '{"TRIGGER_DIRECT_MAP_CTX4", -1, -1}
         });
      this.default_map.add_reg(this.TRIGGER_DIRECT_MAP_CTX4, `UVM_REG_ADDR_WIDTH'h2428, "RW", 0);
		this.TRIGGER_DIRECT_MAP_CTX4_RSVD0 = this.TRIGGER_DIRECT_MAP_CTX4.RSVD0;
		this.TRIGGER_DIRECT_MAP_CTX4_DEST_PORT_MASK = this.TRIGGER_DIRECT_MAP_CTX4.DEST_PORT_MASK;
      this.TRIGGER_DIRECT_MAP_ADM0 = ral_reg_trig_apply_TRIGGER_DIRECT_MAP_ADM0::type_id::create("TRIGGER_DIRECT_MAP_ADM0",,get_full_name());
      this.TRIGGER_DIRECT_MAP_ADM0.configure(this, null, "");
      this.TRIGGER_DIRECT_MAP_ADM0.build();
         this.TRIGGER_DIRECT_MAP_ADM0.add_hdl_path('{

            '{"TRIGGER_DIRECT_MAP_ADM0", -1, -1}
         });
      this.default_map.add_reg(this.TRIGGER_DIRECT_MAP_ADM0, `UVM_REG_ADDR_WIDTH'h2430, "RW", 0);
		this.TRIGGER_DIRECT_MAP_ADM0_NEW_DEST_MASK = this.TRIGGER_DIRECT_MAP_ADM0.NEW_DEST_MASK;
      this.TRIGGER_DIRECT_MAP_ADM1 = ral_reg_trig_apply_TRIGGER_DIRECT_MAP_ADM1::type_id::create("TRIGGER_DIRECT_MAP_ADM1",,get_full_name());
      this.TRIGGER_DIRECT_MAP_ADM1.configure(this, null, "");
      this.TRIGGER_DIRECT_MAP_ADM1.build();
         this.TRIGGER_DIRECT_MAP_ADM1.add_hdl_path('{

            '{"TRIGGER_DIRECT_MAP_ADM1", -1, -1}
         });
      this.default_map.add_reg(this.TRIGGER_DIRECT_MAP_ADM1, `UVM_REG_ADDR_WIDTH'h2438, "RW", 0);
		this.TRIGGER_DIRECT_MAP_ADM1_NEW_DEST_MASK = this.TRIGGER_DIRECT_MAP_ADM1.NEW_DEST_MASK;
      this.TRIGGER_DIRECT_MAP_ADM2 = ral_reg_trig_apply_TRIGGER_DIRECT_MAP_ADM2::type_id::create("TRIGGER_DIRECT_MAP_ADM2",,get_full_name());
      this.TRIGGER_DIRECT_MAP_ADM2.configure(this, null, "");
      this.TRIGGER_DIRECT_MAP_ADM2.build();
         this.TRIGGER_DIRECT_MAP_ADM2.add_hdl_path('{

            '{"TRIGGER_DIRECT_MAP_ADM2", -1, -1}
         });
      this.default_map.add_reg(this.TRIGGER_DIRECT_MAP_ADM2, `UVM_REG_ADDR_WIDTH'h2440, "RW", 0);
		this.TRIGGER_DIRECT_MAP_ADM2_NEW_DEST_MASK = this.TRIGGER_DIRECT_MAP_ADM2.NEW_DEST_MASK;
      this.TRIGGER_DIRECT_MAP_ADM3 = ral_reg_trig_apply_TRIGGER_DIRECT_MAP_ADM3::type_id::create("TRIGGER_DIRECT_MAP_ADM3",,get_full_name());
      this.TRIGGER_DIRECT_MAP_ADM3.configure(this, null, "");
      this.TRIGGER_DIRECT_MAP_ADM3.build();
         this.TRIGGER_DIRECT_MAP_ADM3.add_hdl_path('{

            '{"TRIGGER_DIRECT_MAP_ADM3", -1, -1}
         });
      this.default_map.add_reg(this.TRIGGER_DIRECT_MAP_ADM3, `UVM_REG_ADDR_WIDTH'h2448, "RW", 0);
		this.TRIGGER_DIRECT_MAP_ADM3_NEW_DEST_MASK = this.TRIGGER_DIRECT_MAP_ADM3.NEW_DEST_MASK;
      this.TRIGGER_DIRECT_MAP_ADM4 = ral_reg_trig_apply_TRIGGER_DIRECT_MAP_ADM4::type_id::create("TRIGGER_DIRECT_MAP_ADM4",,get_full_name());
      this.TRIGGER_DIRECT_MAP_ADM4.configure(this, null, "");
      this.TRIGGER_DIRECT_MAP_ADM4.build();
         this.TRIGGER_DIRECT_MAP_ADM4.add_hdl_path('{

            '{"TRIGGER_DIRECT_MAP_ADM4", -1, -1}
         });
      this.default_map.add_reg(this.TRIGGER_DIRECT_MAP_ADM4, `UVM_REG_ADDR_WIDTH'h2450, "RW", 0);
		this.TRIGGER_DIRECT_MAP_ADM4_RSVD0 = this.TRIGGER_DIRECT_MAP_ADM4.RSVD0;
		this.TRIGGER_DIRECT_MAP_ADM4_FILTER_DEST_MASK = this.TRIGGER_DIRECT_MAP_ADM4.FILTER_DEST_MASK;
		this.FILTER_DEST_MASK = this.TRIGGER_DIRECT_MAP_ADM4.FILTER_DEST_MASK;
		this.TRIGGER_DIRECT_MAP_ADM4_NEW_DEST_MASK = this.TRIGGER_DIRECT_MAP_ADM4.NEW_DEST_MASK;
      this.TRIGGER_DIRECT_MAP_ADR0 = ral_reg_trig_apply_TRIGGER_DIRECT_MAP_ADR0::type_id::create("TRIGGER_DIRECT_MAP_ADR0",,get_full_name());
      this.TRIGGER_DIRECT_MAP_ADR0.configure(this, null, "");
      this.TRIGGER_DIRECT_MAP_ADR0.build();
         this.TRIGGER_DIRECT_MAP_ADR0.add_hdl_path('{

            '{"TRIGGER_DIRECT_MAP_ADR0", -1, -1}
         });
      this.default_map.add_reg(this.TRIGGER_DIRECT_MAP_ADR0, `UVM_REG_ADDR_WIDTH'h2458, "RW", 0);
		this.TRIGGER_DIRECT_MAP_ADR0_DROP_MASK = this.TRIGGER_DIRECT_MAP_ADR0.DROP_MASK;
      this.TRIGGER_DIRECT_MAP_ADR1 = ral_reg_trig_apply_TRIGGER_DIRECT_MAP_ADR1::type_id::create("TRIGGER_DIRECT_MAP_ADR1",,get_full_name());
      this.TRIGGER_DIRECT_MAP_ADR1.configure(this, null, "");
      this.TRIGGER_DIRECT_MAP_ADR1.build();
         this.TRIGGER_DIRECT_MAP_ADR1.add_hdl_path('{

            '{"TRIGGER_DIRECT_MAP_ADR1", -1, -1}
         });
      this.default_map.add_reg(this.TRIGGER_DIRECT_MAP_ADR1, `UVM_REG_ADDR_WIDTH'h2460, "RW", 0);
		this.TRIGGER_DIRECT_MAP_ADR1_DROP_MASK = this.TRIGGER_DIRECT_MAP_ADR1.DROP_MASK;
      this.TRIGGER_DIRECT_MAP_ADR2 = ral_reg_trig_apply_TRIGGER_DIRECT_MAP_ADR2::type_id::create("TRIGGER_DIRECT_MAP_ADR2",,get_full_name());
      this.TRIGGER_DIRECT_MAP_ADR2.configure(this, null, "");
      this.TRIGGER_DIRECT_MAP_ADR2.build();
         this.TRIGGER_DIRECT_MAP_ADR2.add_hdl_path('{

            '{"TRIGGER_DIRECT_MAP_ADR2", -1, -1}
         });
      this.default_map.add_reg(this.TRIGGER_DIRECT_MAP_ADR2, `UVM_REG_ADDR_WIDTH'h2468, "RW", 0);
		this.TRIGGER_DIRECT_MAP_ADR2_DROP_MASK = this.TRIGGER_DIRECT_MAP_ADR2.DROP_MASK;
      this.TRIGGER_DIRECT_MAP_ADR3 = ral_reg_trig_apply_TRIGGER_DIRECT_MAP_ADR3::type_id::create("TRIGGER_DIRECT_MAP_ADR3",,get_full_name());
      this.TRIGGER_DIRECT_MAP_ADR3.configure(this, null, "");
      this.TRIGGER_DIRECT_MAP_ADR3.build();
         this.TRIGGER_DIRECT_MAP_ADR3.add_hdl_path('{

            '{"TRIGGER_DIRECT_MAP_ADR3", -1, -1}
         });
      this.default_map.add_reg(this.TRIGGER_DIRECT_MAP_ADR3, `UVM_REG_ADDR_WIDTH'h2470, "RW", 0);
		this.TRIGGER_DIRECT_MAP_ADR3_DROP_MASK = this.TRIGGER_DIRECT_MAP_ADR3.DROP_MASK;
      this.TRIGGER_DIRECT_MAP_ADR4 = ral_reg_trig_apply_TRIGGER_DIRECT_MAP_ADR4::type_id::create("TRIGGER_DIRECT_MAP_ADR4",,get_full_name());
      this.TRIGGER_DIRECT_MAP_ADR4.configure(this, null, "");
      this.TRIGGER_DIRECT_MAP_ADR4.build();
         this.TRIGGER_DIRECT_MAP_ADR4.add_hdl_path('{

            '{"TRIGGER_DIRECT_MAP_ADR4", -1, -1}
         });
      this.default_map.add_reg(this.TRIGGER_DIRECT_MAP_ADR4, `UVM_REG_ADDR_WIDTH'h2478, "RW", 0);
		this.TRIGGER_DIRECT_MAP_ADR4_RSVD0 = this.TRIGGER_DIRECT_MAP_ADR4.RSVD0;
		this.TRIGGER_DIRECT_MAP_ADR4_DROP_MASK = this.TRIGGER_DIRECT_MAP_ADR4.DROP_MASK;
   endfunction : build

	`uvm_object_utils(ral_block_trig_apply)

endclass : ral_block_trig_apply



`endif
