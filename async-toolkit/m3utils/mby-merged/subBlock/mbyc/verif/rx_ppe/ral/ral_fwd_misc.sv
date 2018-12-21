`ifndef RAL_FWD_MISC
`define RAL_FWD_MISC

import uvm_pkg::*;

class ral_reg_fwd_misc_FWD_PORT_CFG_2 extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field DESTINATION_MASK;

	function new(string name = "fwd_misc_FWD_PORT_CFG_2");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 40, 24, "RO", 0, 40'h0, 1, 0, 1);
      this.DESTINATION_MASK = uvm_reg_field::type_id::create("DESTINATION_MASK",,get_full_name());
      this.DESTINATION_MASK.configure(this, 24, 0, "RW", 0, 24'hffffff, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_fwd_misc_FWD_PORT_CFG_2)

endclass : ral_reg_fwd_misc_FWD_PORT_CFG_2


class ral_reg_fwd_misc_FWD_LAG_CFG extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field IN_LAG;
	rand uvm_reg_field HASH_ROTATION;
	rand uvm_reg_field INDEX;
	rand uvm_reg_field LAG_SIZE;

	function new(string name = "fwd_misc_FWD_LAG_CFG");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 54, 10, "RO", 0, 54'h0, 1, 0, 0);
      this.IN_LAG = uvm_reg_field::type_id::create("IN_LAG",,get_full_name());
      this.IN_LAG.configure(this, 1, 9, "RW", 0, 1'h0, 1, 0, 0);
      this.HASH_ROTATION = uvm_reg_field::type_id::create("HASH_ROTATION",,get_full_name());
      this.HASH_ROTATION.configure(this, 1, 8, "RW", 0, 1'h0, 1, 0, 0);
      this.INDEX = uvm_reg_field::type_id::create("INDEX",,get_full_name());
      this.INDEX.configure(this, 4, 4, "RW", 0, 4'h0, 1, 0, 0);
      this.LAG_SIZE = uvm_reg_field::type_id::create("LAG_SIZE",,get_full_name());
      this.LAG_SIZE.configure(this, 4, 0, "RW", 0, 4'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_fwd_misc_FWD_LAG_CFG)

endclass : ral_reg_fwd_misc_FWD_LAG_CFG


class ral_reg_fwd_misc_FWD_PORT_CFG_1 extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field LEARNING_ENABLE;
	rand uvm_reg_field FILTER_VLAN_INGRESS;
	rand uvm_reg_field DESTINATION_MASK;

	function new(string name = "fwd_misc_FWD_PORT_CFG_1");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 38, 26, "RO", 0, 38'h0, 1, 0, 0);
      this.LEARNING_ENABLE = uvm_reg_field::type_id::create("LEARNING_ENABLE",,get_full_name());
      this.LEARNING_ENABLE.configure(this, 1, 25, "RW", 0, 1'h1, 1, 0, 0);
      this.FILTER_VLAN_INGRESS = uvm_reg_field::type_id::create("FILTER_VLAN_INGRESS",,get_full_name());
      this.FILTER_VLAN_INGRESS.configure(this, 1, 24, "RW", 0, 1'h1, 1, 0, 0);
      this.DESTINATION_MASK = uvm_reg_field::type_id::create("DESTINATION_MASK",,get_full_name());
      this.DESTINATION_MASK.configure(this, 24, 0, "RW", 0, 24'hffffff, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_fwd_misc_FWD_PORT_CFG_1)

endclass : ral_reg_fwd_misc_FWD_PORT_CFG_1


class ral_reg_fwd_misc_FWD_SYS_CFG_1 extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field STORE_TRAP_ACTION;
	rand uvm_reg_field DROP_MAC_CTRL_ETHERTYPE;
	rand uvm_reg_field DROP_INVALID_SMAC;
	rand uvm_reg_field ENABLE_TRAP_PLUS_LOG;
	rand uvm_reg_field TRAP_MTU_VIOLATIONS;

	function new(string name = "fwd_misc_FWD_SYS_CFG_1");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 59, 5, "RO", 0, 59'h0, 1, 0, 0);
      this.STORE_TRAP_ACTION = uvm_reg_field::type_id::create("STORE_TRAP_ACTION",,get_full_name());
      this.STORE_TRAP_ACTION.configure(this, 1, 4, "RW", 0, 1'h1, 1, 0, 0);
      this.DROP_MAC_CTRL_ETHERTYPE = uvm_reg_field::type_id::create("DROP_MAC_CTRL_ETHERTYPE",,get_full_name());
      this.DROP_MAC_CTRL_ETHERTYPE.configure(this, 1, 3, "RW", 0, 1'h1, 1, 0, 0);
      this.DROP_INVALID_SMAC = uvm_reg_field::type_id::create("DROP_INVALID_SMAC",,get_full_name());
      this.DROP_INVALID_SMAC.configure(this, 1, 2, "RW", 0, 1'h1, 1, 0, 0);
      this.ENABLE_TRAP_PLUS_LOG = uvm_reg_field::type_id::create("ENABLE_TRAP_PLUS_LOG",,get_full_name());
      this.ENABLE_TRAP_PLUS_LOG.configure(this, 1, 1, "RW", 0, 1'h1, 1, 0, 0);
      this.TRAP_MTU_VIOLATIONS = uvm_reg_field::type_id::create("TRAP_MTU_VIOLATIONS",,get_full_name());
      this.TRAP_MTU_VIOLATIONS.configure(this, 1, 0, "RW", 0, 1'h1, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_fwd_misc_FWD_SYS_CFG_1)

endclass : ral_reg_fwd_misc_FWD_SYS_CFG_1


class ral_reg_fwd_misc_FWD_CPU_MAC extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field MAC_ADDR;

	function new(string name = "fwd_misc_FWD_CPU_MAC");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 16, 48, "RO", 0, 16'h0, 1, 0, 1);
      this.MAC_ADDR = uvm_reg_field::type_id::create("MAC_ADDR",,get_full_name());
      this.MAC_ADDR.configure(this, 48, 0, "RW", 0, 48'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_fwd_misc_FWD_CPU_MAC)

endclass : ral_reg_fwd_misc_FWD_CPU_MAC


class ral_reg_fwd_misc_FWD_SYS_CFG_ROUTER extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field TRAP_IP_OPTIONS;
	rand uvm_reg_field TRAP_TTL1;

	function new(string name = "fwd_misc_FWD_SYS_CFG_ROUTER");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 61, 3, "RO", 0, 61'h0, 1, 0, 0);
      this.TRAP_IP_OPTIONS = uvm_reg_field::type_id::create("TRAP_IP_OPTIONS",,get_full_name());
      this.TRAP_IP_OPTIONS.configure(this, 1, 2, "RW", 0, 1'h0, 1, 0, 0);
      this.TRAP_TTL1 = uvm_reg_field::type_id::create("TRAP_TTL1",,get_full_name());
      this.TRAP_TTL1.configure(this, 2, 0, "RW", 0, 2'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_fwd_misc_FWD_SYS_CFG_ROUTER)

endclass : ral_reg_fwd_misc_FWD_SYS_CFG_ROUTER


class ral_reg_fwd_misc_FWD_RX_MIRROR_CFG extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field MIRROR_PROFILE_IDX;

	function new(string name = "fwd_misc_FWD_RX_MIRROR_CFG");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 58, 6, "RO", 0, 58'h0, 1, 0, 0);
      this.MIRROR_PROFILE_IDX = uvm_reg_field::type_id::create("MIRROR_PROFILE_IDX",,get_full_name());
      this.MIRROR_PROFILE_IDX.configure(this, 6, 0, "RW", 0, 6'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_fwd_misc_FWD_RX_MIRROR_CFG)

endclass : ral_reg_fwd_misc_FWD_RX_MIRROR_CFG


class ral_reg_fwd_misc_FWD_QCN_MIRROR_CFG extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field MIRROR_PROFILE_IDX;
	rand uvm_reg_field MIRROR_SESSION;

	function new(string name = "fwd_misc_FWD_QCN_MIRROR_CFG");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 56, 8, "RO", 0, 56'h0, 1, 0, 1);
      this.MIRROR_PROFILE_IDX = uvm_reg_field::type_id::create("MIRROR_PROFILE_IDX",,get_full_name());
      this.MIRROR_PROFILE_IDX.configure(this, 6, 2, "RW", 0, 6'h0, 1, 0, 0);
      this.MIRROR_SESSION = uvm_reg_field::type_id::create("MIRROR_SESSION",,get_full_name());
      this.MIRROR_SESSION.configure(this, 2, 0, "RW", 0, 2'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_fwd_misc_FWD_QCN_MIRROR_CFG)

endclass : ral_reg_fwd_misc_FWD_QCN_MIRROR_CFG


class ral_reg_fwd_misc_FWD_IEEE_RESERVED_MAC_ACTION extends uvm_reg;
	rand uvm_reg_field ACTION_63;
	rand uvm_reg_field ACTION_62;
	rand uvm_reg_field ACTION_61;
	rand uvm_reg_field ACTION_60;
	rand uvm_reg_field ACTION_59;
	rand uvm_reg_field ACTION_58;
	rand uvm_reg_field ACTION_57;
	rand uvm_reg_field ACTION_56;
	rand uvm_reg_field ACTION_55;
	rand uvm_reg_field ACTION_54;
	rand uvm_reg_field ACTION_53;
	rand uvm_reg_field ACTION_52;
	rand uvm_reg_field ACTION_51;
	rand uvm_reg_field ACTION_50;
	rand uvm_reg_field ACTION_49;
	rand uvm_reg_field ACTION_48;
	rand uvm_reg_field ACTION_47;
	rand uvm_reg_field ACTION_46;
	rand uvm_reg_field ACTION_45;
	rand uvm_reg_field ACTION_44;
	rand uvm_reg_field ACTION_43;
	rand uvm_reg_field ACTION_42;
	rand uvm_reg_field ACTION_41;
	rand uvm_reg_field ACTION_40;
	rand uvm_reg_field ACTION_39;
	rand uvm_reg_field ACTION_38;
	rand uvm_reg_field ACTION_37;
	rand uvm_reg_field ACTION_36;
	rand uvm_reg_field ACTION_35;
	rand uvm_reg_field ACTION_34;
	rand uvm_reg_field ACTION_33;
	rand uvm_reg_field ACTION_32;
	rand uvm_reg_field ACTION_31;
	rand uvm_reg_field ACTION_30;
	rand uvm_reg_field ACTION_29;
	rand uvm_reg_field ACTION_28;
	rand uvm_reg_field ACTION_27;
	rand uvm_reg_field ACTION_26;
	rand uvm_reg_field ACTION_25;
	rand uvm_reg_field ACTION_24;
	rand uvm_reg_field ACTION_23;
	rand uvm_reg_field ACTION_22;
	rand uvm_reg_field ACTION_21;
	rand uvm_reg_field ACTION_20;
	rand uvm_reg_field ACTION_19;
	rand uvm_reg_field ACTION_18;
	rand uvm_reg_field ACTION_17;
	rand uvm_reg_field ACTION_16;
	rand uvm_reg_field ACTION_15;
	rand uvm_reg_field ACTION_14;
	rand uvm_reg_field ACTION_13;
	rand uvm_reg_field ACTION_12;
	rand uvm_reg_field ACTION_11;
	rand uvm_reg_field ACTION_10;
	rand uvm_reg_field ACTION_9;
	rand uvm_reg_field ACTION_8;
	rand uvm_reg_field ACTION_7;
	rand uvm_reg_field ACTION_6;
	rand uvm_reg_field ACTION_5;
	rand uvm_reg_field ACTION_4;
	rand uvm_reg_field ACTION_3;
	rand uvm_reg_field ACTION_2;
	rand uvm_reg_field ACTION_1;
	rand uvm_reg_field ACTION_0;

	function new(string name = "fwd_misc_FWD_IEEE_RESERVED_MAC_ACTION");
		super.new(name, 128,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.ACTION_63 = uvm_reg_field::type_id::create("ACTION_63",,get_full_name());
      this.ACTION_63.configure(this, 2, 126, "RW", 0, 2'h2, 1, 0, 0);
      this.ACTION_62 = uvm_reg_field::type_id::create("ACTION_62",,get_full_name());
      this.ACTION_62.configure(this, 2, 124, "RW", 0, 2'h2, 1, 0, 0);
      this.ACTION_61 = uvm_reg_field::type_id::create("ACTION_61",,get_full_name());
      this.ACTION_61.configure(this, 2, 122, "RW", 0, 2'h2, 1, 0, 0);
      this.ACTION_60 = uvm_reg_field::type_id::create("ACTION_60",,get_full_name());
      this.ACTION_60.configure(this, 2, 120, "RW", 0, 2'h2, 1, 0, 0);
      this.ACTION_59 = uvm_reg_field::type_id::create("ACTION_59",,get_full_name());
      this.ACTION_59.configure(this, 2, 118, "RW", 0, 2'h2, 1, 0, 0);
      this.ACTION_58 = uvm_reg_field::type_id::create("ACTION_58",,get_full_name());
      this.ACTION_58.configure(this, 2, 116, "RW", 0, 2'h2, 1, 0, 0);
      this.ACTION_57 = uvm_reg_field::type_id::create("ACTION_57",,get_full_name());
      this.ACTION_57.configure(this, 2, 114, "RW", 0, 2'h2, 1, 0, 0);
      this.ACTION_56 = uvm_reg_field::type_id::create("ACTION_56",,get_full_name());
      this.ACTION_56.configure(this, 2, 112, "RW", 0, 2'h2, 1, 0, 0);
      this.ACTION_55 = uvm_reg_field::type_id::create("ACTION_55",,get_full_name());
      this.ACTION_55.configure(this, 2, 110, "RW", 0, 2'h2, 1, 0, 0);
      this.ACTION_54 = uvm_reg_field::type_id::create("ACTION_54",,get_full_name());
      this.ACTION_54.configure(this, 2, 108, "RW", 0, 2'h2, 1, 0, 0);
      this.ACTION_53 = uvm_reg_field::type_id::create("ACTION_53",,get_full_name());
      this.ACTION_53.configure(this, 2, 106, "RW", 0, 2'h2, 1, 0, 0);
      this.ACTION_52 = uvm_reg_field::type_id::create("ACTION_52",,get_full_name());
      this.ACTION_52.configure(this, 2, 104, "RW", 0, 2'h2, 1, 0, 0);
      this.ACTION_51 = uvm_reg_field::type_id::create("ACTION_51",,get_full_name());
      this.ACTION_51.configure(this, 2, 102, "RW", 0, 2'h2, 1, 0, 0);
      this.ACTION_50 = uvm_reg_field::type_id::create("ACTION_50",,get_full_name());
      this.ACTION_50.configure(this, 2, 100, "RW", 0, 2'h2, 1, 0, 0);
      this.ACTION_49 = uvm_reg_field::type_id::create("ACTION_49",,get_full_name());
      this.ACTION_49.configure(this, 2, 98, "RW", 0, 2'h2, 1, 0, 0);
      this.ACTION_48 = uvm_reg_field::type_id::create("ACTION_48",,get_full_name());
      this.ACTION_48.configure(this, 2, 96, "RW", 0, 2'h2, 1, 0, 0);
      this.ACTION_47 = uvm_reg_field::type_id::create("ACTION_47",,get_full_name());
      this.ACTION_47.configure(this, 2, 94, "RW", 0, 2'h2, 1, 0, 0);
      this.ACTION_46 = uvm_reg_field::type_id::create("ACTION_46",,get_full_name());
      this.ACTION_46.configure(this, 2, 92, "RW", 0, 2'h2, 1, 0, 0);
      this.ACTION_45 = uvm_reg_field::type_id::create("ACTION_45",,get_full_name());
      this.ACTION_45.configure(this, 2, 90, "RW", 0, 2'h2, 1, 0, 0);
      this.ACTION_44 = uvm_reg_field::type_id::create("ACTION_44",,get_full_name());
      this.ACTION_44.configure(this, 2, 88, "RW", 0, 2'h2, 1, 0, 0);
      this.ACTION_43 = uvm_reg_field::type_id::create("ACTION_43",,get_full_name());
      this.ACTION_43.configure(this, 2, 86, "RW", 0, 2'h2, 1, 0, 0);
      this.ACTION_42 = uvm_reg_field::type_id::create("ACTION_42",,get_full_name());
      this.ACTION_42.configure(this, 2, 84, "RW", 0, 2'h2, 1, 0, 0);
      this.ACTION_41 = uvm_reg_field::type_id::create("ACTION_41",,get_full_name());
      this.ACTION_41.configure(this, 2, 82, "RW", 0, 2'h2, 1, 0, 0);
      this.ACTION_40 = uvm_reg_field::type_id::create("ACTION_40",,get_full_name());
      this.ACTION_40.configure(this, 2, 80, "RW", 0, 2'h2, 1, 0, 0);
      this.ACTION_39 = uvm_reg_field::type_id::create("ACTION_39",,get_full_name());
      this.ACTION_39.configure(this, 2, 78, "RW", 0, 2'h2, 1, 0, 0);
      this.ACTION_38 = uvm_reg_field::type_id::create("ACTION_38",,get_full_name());
      this.ACTION_38.configure(this, 2, 76, "RW", 0, 2'h2, 1, 0, 0);
      this.ACTION_37 = uvm_reg_field::type_id::create("ACTION_37",,get_full_name());
      this.ACTION_37.configure(this, 2, 74, "RW", 0, 2'h2, 1, 0, 0);
      this.ACTION_36 = uvm_reg_field::type_id::create("ACTION_36",,get_full_name());
      this.ACTION_36.configure(this, 2, 72, "RW", 0, 2'h2, 1, 0, 0);
      this.ACTION_35 = uvm_reg_field::type_id::create("ACTION_35",,get_full_name());
      this.ACTION_35.configure(this, 2, 70, "RW", 0, 2'h2, 1, 0, 0);
      this.ACTION_34 = uvm_reg_field::type_id::create("ACTION_34",,get_full_name());
      this.ACTION_34.configure(this, 2, 68, "RW", 0, 2'h2, 1, 0, 0);
      this.ACTION_33 = uvm_reg_field::type_id::create("ACTION_33",,get_full_name());
      this.ACTION_33.configure(this, 2, 66, "RW", 0, 2'h2, 1, 0, 0);
      this.ACTION_32 = uvm_reg_field::type_id::create("ACTION_32",,get_full_name());
      this.ACTION_32.configure(this, 2, 64, "RW", 0, 2'h2, 1, 0, 0);
      this.ACTION_31 = uvm_reg_field::type_id::create("ACTION_31",,get_full_name());
      this.ACTION_31.configure(this, 2, 62, "RW", 0, 2'h2, 1, 0, 0);
      this.ACTION_30 = uvm_reg_field::type_id::create("ACTION_30",,get_full_name());
      this.ACTION_30.configure(this, 2, 60, "RW", 0, 2'h2, 1, 0, 0);
      this.ACTION_29 = uvm_reg_field::type_id::create("ACTION_29",,get_full_name());
      this.ACTION_29.configure(this, 2, 58, "RW", 0, 2'h2, 1, 0, 0);
      this.ACTION_28 = uvm_reg_field::type_id::create("ACTION_28",,get_full_name());
      this.ACTION_28.configure(this, 2, 56, "RW", 0, 2'h2, 1, 0, 0);
      this.ACTION_27 = uvm_reg_field::type_id::create("ACTION_27",,get_full_name());
      this.ACTION_27.configure(this, 2, 54, "RW", 0, 2'h2, 1, 0, 0);
      this.ACTION_26 = uvm_reg_field::type_id::create("ACTION_26",,get_full_name());
      this.ACTION_26.configure(this, 2, 52, "RW", 0, 2'h2, 1, 0, 0);
      this.ACTION_25 = uvm_reg_field::type_id::create("ACTION_25",,get_full_name());
      this.ACTION_25.configure(this, 2, 50, "RW", 0, 2'h2, 1, 0, 0);
      this.ACTION_24 = uvm_reg_field::type_id::create("ACTION_24",,get_full_name());
      this.ACTION_24.configure(this, 2, 48, "RW", 0, 2'h2, 1, 0, 0);
      this.ACTION_23 = uvm_reg_field::type_id::create("ACTION_23",,get_full_name());
      this.ACTION_23.configure(this, 2, 46, "RW", 0, 2'h2, 1, 0, 0);
      this.ACTION_22 = uvm_reg_field::type_id::create("ACTION_22",,get_full_name());
      this.ACTION_22.configure(this, 2, 44, "RW", 0, 2'h2, 1, 0, 0);
      this.ACTION_21 = uvm_reg_field::type_id::create("ACTION_21",,get_full_name());
      this.ACTION_21.configure(this, 2, 42, "RW", 0, 2'h2, 1, 0, 0);
      this.ACTION_20 = uvm_reg_field::type_id::create("ACTION_20",,get_full_name());
      this.ACTION_20.configure(this, 2, 40, "RW", 0, 2'h2, 1, 0, 0);
      this.ACTION_19 = uvm_reg_field::type_id::create("ACTION_19",,get_full_name());
      this.ACTION_19.configure(this, 2, 38, "RW", 0, 2'h2, 1, 0, 0);
      this.ACTION_18 = uvm_reg_field::type_id::create("ACTION_18",,get_full_name());
      this.ACTION_18.configure(this, 2, 36, "RW", 0, 2'h2, 1, 0, 0);
      this.ACTION_17 = uvm_reg_field::type_id::create("ACTION_17",,get_full_name());
      this.ACTION_17.configure(this, 2, 34, "RW", 0, 2'h2, 1, 0, 0);
      this.ACTION_16 = uvm_reg_field::type_id::create("ACTION_16",,get_full_name());
      this.ACTION_16.configure(this, 2, 32, "RW", 0, 2'h2, 1, 0, 0);
      this.ACTION_15 = uvm_reg_field::type_id::create("ACTION_15",,get_full_name());
      this.ACTION_15.configure(this, 2, 30, "RW", 0, 2'h2, 1, 0, 0);
      this.ACTION_14 = uvm_reg_field::type_id::create("ACTION_14",,get_full_name());
      this.ACTION_14.configure(this, 2, 28, "RW", 0, 2'h2, 1, 0, 0);
      this.ACTION_13 = uvm_reg_field::type_id::create("ACTION_13",,get_full_name());
      this.ACTION_13.configure(this, 2, 26, "RW", 0, 2'h2, 1, 0, 0);
      this.ACTION_12 = uvm_reg_field::type_id::create("ACTION_12",,get_full_name());
      this.ACTION_12.configure(this, 2, 24, "RW", 0, 2'h2, 1, 0, 0);
      this.ACTION_11 = uvm_reg_field::type_id::create("ACTION_11",,get_full_name());
      this.ACTION_11.configure(this, 2, 22, "RW", 0, 2'h2, 1, 0, 0);
      this.ACTION_10 = uvm_reg_field::type_id::create("ACTION_10",,get_full_name());
      this.ACTION_10.configure(this, 2, 20, "RW", 0, 2'h2, 1, 0, 0);
      this.ACTION_9 = uvm_reg_field::type_id::create("ACTION_9",,get_full_name());
      this.ACTION_9.configure(this, 2, 18, "RW", 0, 2'h2, 1, 0, 0);
      this.ACTION_8 = uvm_reg_field::type_id::create("ACTION_8",,get_full_name());
      this.ACTION_8.configure(this, 2, 16, "RW", 0, 2'h2, 1, 0, 0);
      this.ACTION_7 = uvm_reg_field::type_id::create("ACTION_7",,get_full_name());
      this.ACTION_7.configure(this, 2, 14, "RW", 0, 2'h2, 1, 0, 0);
      this.ACTION_6 = uvm_reg_field::type_id::create("ACTION_6",,get_full_name());
      this.ACTION_6.configure(this, 2, 12, "RW", 0, 2'h2, 1, 0, 0);
      this.ACTION_5 = uvm_reg_field::type_id::create("ACTION_5",,get_full_name());
      this.ACTION_5.configure(this, 2, 10, "RW", 0, 2'h2, 1, 0, 0);
      this.ACTION_4 = uvm_reg_field::type_id::create("ACTION_4",,get_full_name());
      this.ACTION_4.configure(this, 2, 8, "RW", 0, 2'h2, 1, 0, 0);
      this.ACTION_3 = uvm_reg_field::type_id::create("ACTION_3",,get_full_name());
      this.ACTION_3.configure(this, 2, 6, "RW", 0, 2'h2, 1, 0, 0);
      this.ACTION_2 = uvm_reg_field::type_id::create("ACTION_2",,get_full_name());
      this.ACTION_2.configure(this, 2, 4, "RW", 0, 2'h2, 1, 0, 0);
      this.ACTION_1 = uvm_reg_field::type_id::create("ACTION_1",,get_full_name());
      this.ACTION_1.configure(this, 2, 2, "RW", 0, 2'h2, 1, 0, 0);
      this.ACTION_0 = uvm_reg_field::type_id::create("ACTION_0",,get_full_name());
      this.ACTION_0.configure(this, 2, 0, "RW", 0, 2'h2, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_fwd_misc_FWD_IEEE_RESERVED_MAC_ACTION)

endclass : ral_reg_fwd_misc_FWD_IEEE_RESERVED_MAC_ACTION


class ral_reg_fwd_misc_FWD_IEEE_RESERVED_MAC_TRAP_PRIORITY extends uvm_reg;
	rand uvm_reg_field SELECT;

	function new(string name = "fwd_misc_FWD_IEEE_RESERVED_MAC_TRAP_PRIORITY");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.SELECT = uvm_reg_field::type_id::create("SELECT",,get_full_name());
      this.SELECT.configure(this, 64, 0, "RW", 0, 64'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_fwd_misc_FWD_IEEE_RESERVED_MAC_TRAP_PRIORITY)

endclass : ral_reg_fwd_misc_FWD_IEEE_RESERVED_MAC_TRAP_PRIORITY


class ral_reg_fwd_misc_FWD_IEEE_RESERVED_MAC_CFG extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field TRAP_TC;

	function new(string name = "fwd_misc_FWD_IEEE_RESERVED_MAC_CFG");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 61, 3, "RO", 0, 61'h0, 1, 0, 0);
      this.TRAP_TC = uvm_reg_field::type_id::create("TRAP_TC",,get_full_name());
      this.TRAP_TC.configure(this, 3, 0, "RW", 0, 3'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_fwd_misc_FWD_IEEE_RESERVED_MAC_CFG)

endclass : ral_reg_fwd_misc_FWD_IEEE_RESERVED_MAC_CFG


class ral_reg_fwd_misc_FWD_IP extends uvm_reg;
	uvm_reg_field RSVD0;
	uvm_reg_field MA_TCN;
	rand uvm_reg_field WB_FIFO_PERR;
	uvm_reg_field TRIGGER;
	uvm_reg_field TCAM_ERR;
	uvm_reg_field ENTRY_COUNT;
	rand uvm_reg_field SHELL_CTRL_U_ERR;

	function new(string name = "fwd_misc_FWD_IP");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 58, 6, "RO", 0, 58'h0, 1, 0, 0);
      this.MA_TCN = uvm_reg_field::type_id::create("MA_TCN",,get_full_name());
      this.MA_TCN.configure(this, 1, 5, "RO", 1, 1'h0, 1, 0, 0);
      this.WB_FIFO_PERR = uvm_reg_field::type_id::create("WB_FIFO_PERR",,get_full_name());
      this.WB_FIFO_PERR.configure(this, 1, 4, "W1C", 1, 1'h0, 1, 0, 0);
      this.TRIGGER = uvm_reg_field::type_id::create("TRIGGER",,get_full_name());
      this.TRIGGER.configure(this, 1, 3, "RO", 1, 1'h0, 1, 0, 0);
      this.TCAM_ERR = uvm_reg_field::type_id::create("TCAM_ERR",,get_full_name());
      this.TCAM_ERR.configure(this, 1, 2, "RO", 1, 1'h0, 1, 0, 0);
      this.ENTRY_COUNT = uvm_reg_field::type_id::create("ENTRY_COUNT",,get_full_name());
      this.ENTRY_COUNT.configure(this, 1, 1, "RO", 1, 1'h0, 1, 0, 0);
      this.SHELL_CTRL_U_ERR = uvm_reg_field::type_id::create("SHELL_CTRL_U_ERR",,get_full_name());
      this.SHELL_CTRL_U_ERR.configure(this, 1, 0, "W1C", 1, 1'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_fwd_misc_FWD_IP)

endclass : ral_reg_fwd_misc_FWD_IP


class ral_reg_fwd_misc_FWD_IM extends uvm_reg;
	uvm_reg_field RSVD0;
	uvm_reg_field MA_TCN;
	rand uvm_reg_field WB_FIFO_PERR;
	rand uvm_reg_field TRIGGER;
	rand uvm_reg_field TCAM_ERR;
	rand uvm_reg_field ENTRY_COUNT;
	rand uvm_reg_field SHELL_CTRL_U_ERR;

	function new(string name = "fwd_misc_FWD_IM");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 58, 6, "RO", 0, 58'h0, 1, 0, 0);
      this.MA_TCN = uvm_reg_field::type_id::create("MA_TCN",,get_full_name());
      this.MA_TCN.configure(this, 1, 5, "RO", 1, 1'h0, 1, 0, 0);
      this.WB_FIFO_PERR = uvm_reg_field::type_id::create("WB_FIFO_PERR",,get_full_name());
      this.WB_FIFO_PERR.configure(this, 1, 4, "RW", 0, 1'h1, 1, 0, 0);
      this.TRIGGER = uvm_reg_field::type_id::create("TRIGGER",,get_full_name());
      this.TRIGGER.configure(this, 1, 3, "RW", 0, 1'h1, 1, 0, 0);
      this.TCAM_ERR = uvm_reg_field::type_id::create("TCAM_ERR",,get_full_name());
      this.TCAM_ERR.configure(this, 1, 2, "RW", 0, 1'h1, 1, 0, 0);
      this.ENTRY_COUNT = uvm_reg_field::type_id::create("ENTRY_COUNT",,get_full_name());
      this.ENTRY_COUNT.configure(this, 1, 1, "RW", 0, 1'h1, 1, 0, 0);
      this.SHELL_CTRL_U_ERR = uvm_reg_field::type_id::create("SHELL_CTRL_U_ERR",,get_full_name());
      this.SHELL_CTRL_U_ERR.configure(this, 1, 0, "RW", 0, 1'h1, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_fwd_misc_FWD_IM)

endclass : ral_reg_fwd_misc_FWD_IM


class ral_block_fwd_misc extends uvm_reg_block;
	rand ral_reg_fwd_misc_FWD_PORT_CFG_2 FWD_PORT_CFG_2[256];
	rand ral_reg_fwd_misc_FWD_LAG_CFG FWD_LAG_CFG[17];
	rand ral_reg_fwd_misc_FWD_PORT_CFG_1 FWD_PORT_CFG_1[17];
	rand ral_reg_fwd_misc_FWD_SYS_CFG_1 FWD_SYS_CFG_1;
	rand ral_reg_fwd_misc_FWD_CPU_MAC FWD_CPU_MAC;
	rand ral_reg_fwd_misc_FWD_SYS_CFG_ROUTER FWD_SYS_CFG_ROUTER;
	rand ral_reg_fwd_misc_FWD_RX_MIRROR_CFG FWD_RX_MIRROR_CFG;
	rand ral_reg_fwd_misc_FWD_QCN_MIRROR_CFG FWD_QCN_MIRROR_CFG;
	rand ral_reg_fwd_misc_FWD_IEEE_RESERVED_MAC_ACTION FWD_IEEE_RESERVED_MAC_ACTION;
	rand ral_reg_fwd_misc_FWD_IEEE_RESERVED_MAC_TRAP_PRIORITY FWD_IEEE_RESERVED_MAC_TRAP_PRIORITY;
	rand ral_reg_fwd_misc_FWD_IEEE_RESERVED_MAC_CFG FWD_IEEE_RESERVED_MAC_CFG;
	rand ral_reg_fwd_misc_FWD_IP FWD_IP;
	rand ral_reg_fwd_misc_FWD_IM FWD_IM;
	uvm_reg_field FWD_PORT_CFG_2_RSVD0[256];
	rand uvm_reg_field FWD_PORT_CFG_2_DESTINATION_MASK[256];
	uvm_reg_field FWD_LAG_CFG_RSVD0[17];
	rand uvm_reg_field FWD_LAG_CFG_IN_LAG[17];
	rand uvm_reg_field IN_LAG[17];
	rand uvm_reg_field FWD_LAG_CFG_HASH_ROTATION[17];
	rand uvm_reg_field HASH_ROTATION[17];
	rand uvm_reg_field FWD_LAG_CFG_INDEX[17];
	rand uvm_reg_field INDEX[17];
	rand uvm_reg_field FWD_LAG_CFG_LAG_SIZE[17];
	rand uvm_reg_field LAG_SIZE[17];
	uvm_reg_field FWD_PORT_CFG_1_RSVD0[17];
	rand uvm_reg_field FWD_PORT_CFG_1_LEARNING_ENABLE[17];
	rand uvm_reg_field LEARNING_ENABLE[17];
	rand uvm_reg_field FWD_PORT_CFG_1_FILTER_VLAN_INGRESS[17];
	rand uvm_reg_field FILTER_VLAN_INGRESS[17];
	rand uvm_reg_field FWD_PORT_CFG_1_DESTINATION_MASK[17];
	uvm_reg_field FWD_SYS_CFG_1_RSVD0;
	rand uvm_reg_field FWD_SYS_CFG_1_STORE_TRAP_ACTION;
	rand uvm_reg_field STORE_TRAP_ACTION;
	rand uvm_reg_field FWD_SYS_CFG_1_DROP_MAC_CTRL_ETHERTYPE;
	rand uvm_reg_field DROP_MAC_CTRL_ETHERTYPE;
	rand uvm_reg_field FWD_SYS_CFG_1_DROP_INVALID_SMAC;
	rand uvm_reg_field DROP_INVALID_SMAC;
	rand uvm_reg_field FWD_SYS_CFG_1_ENABLE_TRAP_PLUS_LOG;
	rand uvm_reg_field ENABLE_TRAP_PLUS_LOG;
	rand uvm_reg_field FWD_SYS_CFG_1_TRAP_MTU_VIOLATIONS;
	rand uvm_reg_field TRAP_MTU_VIOLATIONS;
	uvm_reg_field FWD_CPU_MAC_RSVD0;
	rand uvm_reg_field FWD_CPU_MAC_MAC_ADDR;
	rand uvm_reg_field MAC_ADDR;
	uvm_reg_field FWD_SYS_CFG_ROUTER_RSVD0;
	rand uvm_reg_field FWD_SYS_CFG_ROUTER_TRAP_IP_OPTIONS;
	rand uvm_reg_field TRAP_IP_OPTIONS;
	rand uvm_reg_field FWD_SYS_CFG_ROUTER_TRAP_TTL1;
	rand uvm_reg_field TRAP_TTL1;
	uvm_reg_field FWD_RX_MIRROR_CFG_RSVD0;
	rand uvm_reg_field FWD_RX_MIRROR_CFG_MIRROR_PROFILE_IDX;
	uvm_reg_field FWD_QCN_MIRROR_CFG_RSVD0;
	rand uvm_reg_field FWD_QCN_MIRROR_CFG_MIRROR_PROFILE_IDX;
	rand uvm_reg_field FWD_QCN_MIRROR_CFG_MIRROR_SESSION;
	rand uvm_reg_field MIRROR_SESSION;
	rand uvm_reg_field FWD_IEEE_RESERVED_MAC_ACTION_ACTION_63;
	rand uvm_reg_field ACTION_63;
	rand uvm_reg_field FWD_IEEE_RESERVED_MAC_ACTION_ACTION_62;
	rand uvm_reg_field ACTION_62;
	rand uvm_reg_field FWD_IEEE_RESERVED_MAC_ACTION_ACTION_61;
	rand uvm_reg_field ACTION_61;
	rand uvm_reg_field FWD_IEEE_RESERVED_MAC_ACTION_ACTION_60;
	rand uvm_reg_field ACTION_60;
	rand uvm_reg_field FWD_IEEE_RESERVED_MAC_ACTION_ACTION_59;
	rand uvm_reg_field ACTION_59;
	rand uvm_reg_field FWD_IEEE_RESERVED_MAC_ACTION_ACTION_58;
	rand uvm_reg_field ACTION_58;
	rand uvm_reg_field FWD_IEEE_RESERVED_MAC_ACTION_ACTION_57;
	rand uvm_reg_field ACTION_57;
	rand uvm_reg_field FWD_IEEE_RESERVED_MAC_ACTION_ACTION_56;
	rand uvm_reg_field ACTION_56;
	rand uvm_reg_field FWD_IEEE_RESERVED_MAC_ACTION_ACTION_55;
	rand uvm_reg_field ACTION_55;
	rand uvm_reg_field FWD_IEEE_RESERVED_MAC_ACTION_ACTION_54;
	rand uvm_reg_field ACTION_54;
	rand uvm_reg_field FWD_IEEE_RESERVED_MAC_ACTION_ACTION_53;
	rand uvm_reg_field ACTION_53;
	rand uvm_reg_field FWD_IEEE_RESERVED_MAC_ACTION_ACTION_52;
	rand uvm_reg_field ACTION_52;
	rand uvm_reg_field FWD_IEEE_RESERVED_MAC_ACTION_ACTION_51;
	rand uvm_reg_field ACTION_51;
	rand uvm_reg_field FWD_IEEE_RESERVED_MAC_ACTION_ACTION_50;
	rand uvm_reg_field ACTION_50;
	rand uvm_reg_field FWD_IEEE_RESERVED_MAC_ACTION_ACTION_49;
	rand uvm_reg_field ACTION_49;
	rand uvm_reg_field FWD_IEEE_RESERVED_MAC_ACTION_ACTION_48;
	rand uvm_reg_field ACTION_48;
	rand uvm_reg_field FWD_IEEE_RESERVED_MAC_ACTION_ACTION_47;
	rand uvm_reg_field ACTION_47;
	rand uvm_reg_field FWD_IEEE_RESERVED_MAC_ACTION_ACTION_46;
	rand uvm_reg_field ACTION_46;
	rand uvm_reg_field FWD_IEEE_RESERVED_MAC_ACTION_ACTION_45;
	rand uvm_reg_field ACTION_45;
	rand uvm_reg_field FWD_IEEE_RESERVED_MAC_ACTION_ACTION_44;
	rand uvm_reg_field ACTION_44;
	rand uvm_reg_field FWD_IEEE_RESERVED_MAC_ACTION_ACTION_43;
	rand uvm_reg_field ACTION_43;
	rand uvm_reg_field FWD_IEEE_RESERVED_MAC_ACTION_ACTION_42;
	rand uvm_reg_field ACTION_42;
	rand uvm_reg_field FWD_IEEE_RESERVED_MAC_ACTION_ACTION_41;
	rand uvm_reg_field ACTION_41;
	rand uvm_reg_field FWD_IEEE_RESERVED_MAC_ACTION_ACTION_40;
	rand uvm_reg_field ACTION_40;
	rand uvm_reg_field FWD_IEEE_RESERVED_MAC_ACTION_ACTION_39;
	rand uvm_reg_field ACTION_39;
	rand uvm_reg_field FWD_IEEE_RESERVED_MAC_ACTION_ACTION_38;
	rand uvm_reg_field ACTION_38;
	rand uvm_reg_field FWD_IEEE_RESERVED_MAC_ACTION_ACTION_37;
	rand uvm_reg_field ACTION_37;
	rand uvm_reg_field FWD_IEEE_RESERVED_MAC_ACTION_ACTION_36;
	rand uvm_reg_field ACTION_36;
	rand uvm_reg_field FWD_IEEE_RESERVED_MAC_ACTION_ACTION_35;
	rand uvm_reg_field ACTION_35;
	rand uvm_reg_field FWD_IEEE_RESERVED_MAC_ACTION_ACTION_34;
	rand uvm_reg_field ACTION_34;
	rand uvm_reg_field FWD_IEEE_RESERVED_MAC_ACTION_ACTION_33;
	rand uvm_reg_field ACTION_33;
	rand uvm_reg_field FWD_IEEE_RESERVED_MAC_ACTION_ACTION_32;
	rand uvm_reg_field ACTION_32;
	rand uvm_reg_field FWD_IEEE_RESERVED_MAC_ACTION_ACTION_31;
	rand uvm_reg_field ACTION_31;
	rand uvm_reg_field FWD_IEEE_RESERVED_MAC_ACTION_ACTION_30;
	rand uvm_reg_field ACTION_30;
	rand uvm_reg_field FWD_IEEE_RESERVED_MAC_ACTION_ACTION_29;
	rand uvm_reg_field ACTION_29;
	rand uvm_reg_field FWD_IEEE_RESERVED_MAC_ACTION_ACTION_28;
	rand uvm_reg_field ACTION_28;
	rand uvm_reg_field FWD_IEEE_RESERVED_MAC_ACTION_ACTION_27;
	rand uvm_reg_field ACTION_27;
	rand uvm_reg_field FWD_IEEE_RESERVED_MAC_ACTION_ACTION_26;
	rand uvm_reg_field ACTION_26;
	rand uvm_reg_field FWD_IEEE_RESERVED_MAC_ACTION_ACTION_25;
	rand uvm_reg_field ACTION_25;
	rand uvm_reg_field FWD_IEEE_RESERVED_MAC_ACTION_ACTION_24;
	rand uvm_reg_field ACTION_24;
	rand uvm_reg_field FWD_IEEE_RESERVED_MAC_ACTION_ACTION_23;
	rand uvm_reg_field ACTION_23;
	rand uvm_reg_field FWD_IEEE_RESERVED_MAC_ACTION_ACTION_22;
	rand uvm_reg_field ACTION_22;
	rand uvm_reg_field FWD_IEEE_RESERVED_MAC_ACTION_ACTION_21;
	rand uvm_reg_field ACTION_21;
	rand uvm_reg_field FWD_IEEE_RESERVED_MAC_ACTION_ACTION_20;
	rand uvm_reg_field ACTION_20;
	rand uvm_reg_field FWD_IEEE_RESERVED_MAC_ACTION_ACTION_19;
	rand uvm_reg_field ACTION_19;
	rand uvm_reg_field FWD_IEEE_RESERVED_MAC_ACTION_ACTION_18;
	rand uvm_reg_field ACTION_18;
	rand uvm_reg_field FWD_IEEE_RESERVED_MAC_ACTION_ACTION_17;
	rand uvm_reg_field ACTION_17;
	rand uvm_reg_field FWD_IEEE_RESERVED_MAC_ACTION_ACTION_16;
	rand uvm_reg_field ACTION_16;
	rand uvm_reg_field FWD_IEEE_RESERVED_MAC_ACTION_ACTION_15;
	rand uvm_reg_field ACTION_15;
	rand uvm_reg_field FWD_IEEE_RESERVED_MAC_ACTION_ACTION_14;
	rand uvm_reg_field ACTION_14;
	rand uvm_reg_field FWD_IEEE_RESERVED_MAC_ACTION_ACTION_13;
	rand uvm_reg_field ACTION_13;
	rand uvm_reg_field FWD_IEEE_RESERVED_MAC_ACTION_ACTION_12;
	rand uvm_reg_field ACTION_12;
	rand uvm_reg_field FWD_IEEE_RESERVED_MAC_ACTION_ACTION_11;
	rand uvm_reg_field ACTION_11;
	rand uvm_reg_field FWD_IEEE_RESERVED_MAC_ACTION_ACTION_10;
	rand uvm_reg_field ACTION_10;
	rand uvm_reg_field FWD_IEEE_RESERVED_MAC_ACTION_ACTION_9;
	rand uvm_reg_field ACTION_9;
	rand uvm_reg_field FWD_IEEE_RESERVED_MAC_ACTION_ACTION_8;
	rand uvm_reg_field ACTION_8;
	rand uvm_reg_field FWD_IEEE_RESERVED_MAC_ACTION_ACTION_7;
	rand uvm_reg_field ACTION_7;
	rand uvm_reg_field FWD_IEEE_RESERVED_MAC_ACTION_ACTION_6;
	rand uvm_reg_field ACTION_6;
	rand uvm_reg_field FWD_IEEE_RESERVED_MAC_ACTION_ACTION_5;
	rand uvm_reg_field ACTION_5;
	rand uvm_reg_field FWD_IEEE_RESERVED_MAC_ACTION_ACTION_4;
	rand uvm_reg_field ACTION_4;
	rand uvm_reg_field FWD_IEEE_RESERVED_MAC_ACTION_ACTION_3;
	rand uvm_reg_field ACTION_3;
	rand uvm_reg_field FWD_IEEE_RESERVED_MAC_ACTION_ACTION_2;
	rand uvm_reg_field ACTION_2;
	rand uvm_reg_field FWD_IEEE_RESERVED_MAC_ACTION_ACTION_1;
	rand uvm_reg_field ACTION_1;
	rand uvm_reg_field FWD_IEEE_RESERVED_MAC_ACTION_ACTION_0;
	rand uvm_reg_field ACTION_0;
	rand uvm_reg_field FWD_IEEE_RESERVED_MAC_TRAP_PRIORITY_SELECT;
	rand uvm_reg_field SELECT;
	uvm_reg_field FWD_IEEE_RESERVED_MAC_CFG_RSVD0;
	rand uvm_reg_field FWD_IEEE_RESERVED_MAC_CFG_TRAP_TC;
	rand uvm_reg_field TRAP_TC;
	uvm_reg_field FWD_IP_RSVD0;
	uvm_reg_field FWD_IP_MA_TCN;
	rand uvm_reg_field FWD_IP_WB_FIFO_PERR;
	uvm_reg_field FWD_IP_TRIGGER;
	uvm_reg_field FWD_IP_TCAM_ERR;
	uvm_reg_field FWD_IP_ENTRY_COUNT;
	rand uvm_reg_field FWD_IP_SHELL_CTRL_U_ERR;
	uvm_reg_field FWD_IM_RSVD0;
	uvm_reg_field FWD_IM_MA_TCN;
	rand uvm_reg_field FWD_IM_WB_FIFO_PERR;
	rand uvm_reg_field FWD_IM_TRIGGER;
	rand uvm_reg_field FWD_IM_TCAM_ERR;
	rand uvm_reg_field FWD_IM_ENTRY_COUNT;
	rand uvm_reg_field FWD_IM_SHELL_CTRL_U_ERR;

	function new(string name = "fwd_misc");
		super.new(name, build_coverage(UVM_NO_COVERAGE));
	endfunction: new

   virtual function void build();
      this.default_map = create_map("", 0, 8, UVM_LITTLE_ENDIAN, 0);
      foreach (this.FWD_PORT_CFG_2[i]) begin
         int J = i;
         this.FWD_PORT_CFG_2[J] = ral_reg_fwd_misc_FWD_PORT_CFG_2::type_id::create($psprintf("FWD_PORT_CFG_2[%0d]",J),,get_full_name());
         this.FWD_PORT_CFG_2[J].configure(this, null, "");
         this.FWD_PORT_CFG_2[J].build();
         this.FWD_PORT_CFG_2[J].add_hdl_path('{

            '{$psprintf("FWD_PORT_CFG_2[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.FWD_PORT_CFG_2[J], `UVM_REG_ADDR_WIDTH'h0+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.FWD_PORT_CFG_2_RSVD0[J] = this.FWD_PORT_CFG_2[J].RSVD0;
			this.FWD_PORT_CFG_2_DESTINATION_MASK[J] = this.FWD_PORT_CFG_2[J].DESTINATION_MASK;
      end
      foreach (this.FWD_LAG_CFG[i]) begin
         int J = i;
         this.FWD_LAG_CFG[J] = ral_reg_fwd_misc_FWD_LAG_CFG::type_id::create($psprintf("FWD_LAG_CFG[%0d]",J),,get_full_name());
         this.FWD_LAG_CFG[J].configure(this, null, "");
         this.FWD_LAG_CFG[J].build();
         this.FWD_LAG_CFG[J].add_hdl_path('{

            '{$psprintf("FWD_LAG_CFG[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.FWD_LAG_CFG[J], `UVM_REG_ADDR_WIDTH'h1000+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.FWD_LAG_CFG_RSVD0[J] = this.FWD_LAG_CFG[J].RSVD0;
			this.FWD_LAG_CFG_IN_LAG[J] = this.FWD_LAG_CFG[J].IN_LAG;
			this.IN_LAG[J] = this.FWD_LAG_CFG[J].IN_LAG;
			this.FWD_LAG_CFG_HASH_ROTATION[J] = this.FWD_LAG_CFG[J].HASH_ROTATION;
			this.HASH_ROTATION[J] = this.FWD_LAG_CFG[J].HASH_ROTATION;
			this.FWD_LAG_CFG_INDEX[J] = this.FWD_LAG_CFG[J].INDEX;
			this.INDEX[J] = this.FWD_LAG_CFG[J].INDEX;
			this.FWD_LAG_CFG_LAG_SIZE[J] = this.FWD_LAG_CFG[J].LAG_SIZE;
			this.LAG_SIZE[J] = this.FWD_LAG_CFG[J].LAG_SIZE;
      end
      foreach (this.FWD_PORT_CFG_1[i]) begin
         int J = i;
         this.FWD_PORT_CFG_1[J] = ral_reg_fwd_misc_FWD_PORT_CFG_1::type_id::create($psprintf("FWD_PORT_CFG_1[%0d]",J),,get_full_name());
         this.FWD_PORT_CFG_1[J].configure(this, null, "");
         this.FWD_PORT_CFG_1[J].build();
         this.FWD_PORT_CFG_1[J].add_hdl_path('{

            '{$psprintf("FWD_PORT_CFG_1[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.FWD_PORT_CFG_1[J], `UVM_REG_ADDR_WIDTH'h1100+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.FWD_PORT_CFG_1_RSVD0[J] = this.FWD_PORT_CFG_1[J].RSVD0;
			this.FWD_PORT_CFG_1_LEARNING_ENABLE[J] = this.FWD_PORT_CFG_1[J].LEARNING_ENABLE;
			this.LEARNING_ENABLE[J] = this.FWD_PORT_CFG_1[J].LEARNING_ENABLE;
			this.FWD_PORT_CFG_1_FILTER_VLAN_INGRESS[J] = this.FWD_PORT_CFG_1[J].FILTER_VLAN_INGRESS;
			this.FILTER_VLAN_INGRESS[J] = this.FWD_PORT_CFG_1[J].FILTER_VLAN_INGRESS;
			this.FWD_PORT_CFG_1_DESTINATION_MASK[J] = this.FWD_PORT_CFG_1[J].DESTINATION_MASK;
      end
      this.FWD_SYS_CFG_1 = ral_reg_fwd_misc_FWD_SYS_CFG_1::type_id::create("FWD_SYS_CFG_1",,get_full_name());
      this.FWD_SYS_CFG_1.configure(this, null, "");
      this.FWD_SYS_CFG_1.build();
         this.FWD_SYS_CFG_1.add_hdl_path('{

            '{"FWD_SYS_CFG_1", -1, -1}
         });
      this.default_map.add_reg(this.FWD_SYS_CFG_1, `UVM_REG_ADDR_WIDTH'h1200, "RW", 0);
		this.FWD_SYS_CFG_1_RSVD0 = this.FWD_SYS_CFG_1.RSVD0;
		this.FWD_SYS_CFG_1_STORE_TRAP_ACTION = this.FWD_SYS_CFG_1.STORE_TRAP_ACTION;
		this.STORE_TRAP_ACTION = this.FWD_SYS_CFG_1.STORE_TRAP_ACTION;
		this.FWD_SYS_CFG_1_DROP_MAC_CTRL_ETHERTYPE = this.FWD_SYS_CFG_1.DROP_MAC_CTRL_ETHERTYPE;
		this.DROP_MAC_CTRL_ETHERTYPE = this.FWD_SYS_CFG_1.DROP_MAC_CTRL_ETHERTYPE;
		this.FWD_SYS_CFG_1_DROP_INVALID_SMAC = this.FWD_SYS_CFG_1.DROP_INVALID_SMAC;
		this.DROP_INVALID_SMAC = this.FWD_SYS_CFG_1.DROP_INVALID_SMAC;
		this.FWD_SYS_CFG_1_ENABLE_TRAP_PLUS_LOG = this.FWD_SYS_CFG_1.ENABLE_TRAP_PLUS_LOG;
		this.ENABLE_TRAP_PLUS_LOG = this.FWD_SYS_CFG_1.ENABLE_TRAP_PLUS_LOG;
		this.FWD_SYS_CFG_1_TRAP_MTU_VIOLATIONS = this.FWD_SYS_CFG_1.TRAP_MTU_VIOLATIONS;
		this.TRAP_MTU_VIOLATIONS = this.FWD_SYS_CFG_1.TRAP_MTU_VIOLATIONS;
      this.FWD_CPU_MAC = ral_reg_fwd_misc_FWD_CPU_MAC::type_id::create("FWD_CPU_MAC",,get_full_name());
      this.FWD_CPU_MAC.configure(this, null, "");
      this.FWD_CPU_MAC.build();
         this.FWD_CPU_MAC.add_hdl_path('{

            '{"FWD_CPU_MAC", -1, -1}
         });
      this.default_map.add_reg(this.FWD_CPU_MAC, `UVM_REG_ADDR_WIDTH'h1208, "RW", 0);
		this.FWD_CPU_MAC_RSVD0 = this.FWD_CPU_MAC.RSVD0;
		this.FWD_CPU_MAC_MAC_ADDR = this.FWD_CPU_MAC.MAC_ADDR;
		this.MAC_ADDR = this.FWD_CPU_MAC.MAC_ADDR;
      this.FWD_SYS_CFG_ROUTER = ral_reg_fwd_misc_FWD_SYS_CFG_ROUTER::type_id::create("FWD_SYS_CFG_ROUTER",,get_full_name());
      this.FWD_SYS_CFG_ROUTER.configure(this, null, "");
      this.FWD_SYS_CFG_ROUTER.build();
         this.FWD_SYS_CFG_ROUTER.add_hdl_path('{

            '{"FWD_SYS_CFG_ROUTER", -1, -1}
         });
      this.default_map.add_reg(this.FWD_SYS_CFG_ROUTER, `UVM_REG_ADDR_WIDTH'h1210, "RW", 0);
		this.FWD_SYS_CFG_ROUTER_RSVD0 = this.FWD_SYS_CFG_ROUTER.RSVD0;
		this.FWD_SYS_CFG_ROUTER_TRAP_IP_OPTIONS = this.FWD_SYS_CFG_ROUTER.TRAP_IP_OPTIONS;
		this.TRAP_IP_OPTIONS = this.FWD_SYS_CFG_ROUTER.TRAP_IP_OPTIONS;
		this.FWD_SYS_CFG_ROUTER_TRAP_TTL1 = this.FWD_SYS_CFG_ROUTER.TRAP_TTL1;
		this.TRAP_TTL1 = this.FWD_SYS_CFG_ROUTER.TRAP_TTL1;
      this.FWD_RX_MIRROR_CFG = ral_reg_fwd_misc_FWD_RX_MIRROR_CFG::type_id::create("FWD_RX_MIRROR_CFG",,get_full_name());
      this.FWD_RX_MIRROR_CFG.configure(this, null, "");
      this.FWD_RX_MIRROR_CFG.build();
         this.FWD_RX_MIRROR_CFG.add_hdl_path('{

            '{"FWD_RX_MIRROR_CFG", -1, -1}
         });
      this.default_map.add_reg(this.FWD_RX_MIRROR_CFG, `UVM_REG_ADDR_WIDTH'h1218, "RW", 0);
		this.FWD_RX_MIRROR_CFG_RSVD0 = this.FWD_RX_MIRROR_CFG.RSVD0;
		this.FWD_RX_MIRROR_CFG_MIRROR_PROFILE_IDX = this.FWD_RX_MIRROR_CFG.MIRROR_PROFILE_IDX;
      this.FWD_QCN_MIRROR_CFG = ral_reg_fwd_misc_FWD_QCN_MIRROR_CFG::type_id::create("FWD_QCN_MIRROR_CFG",,get_full_name());
      this.FWD_QCN_MIRROR_CFG.configure(this, null, "");
      this.FWD_QCN_MIRROR_CFG.build();
         this.FWD_QCN_MIRROR_CFG.add_hdl_path('{

            '{"FWD_QCN_MIRROR_CFG", -1, -1}
         });
      this.default_map.add_reg(this.FWD_QCN_MIRROR_CFG, `UVM_REG_ADDR_WIDTH'h1220, "RW", 0);
		this.FWD_QCN_MIRROR_CFG_RSVD0 = this.FWD_QCN_MIRROR_CFG.RSVD0;
		this.FWD_QCN_MIRROR_CFG_MIRROR_PROFILE_IDX = this.FWD_QCN_MIRROR_CFG.MIRROR_PROFILE_IDX;
		this.FWD_QCN_MIRROR_CFG_MIRROR_SESSION = this.FWD_QCN_MIRROR_CFG.MIRROR_SESSION;
		this.MIRROR_SESSION = this.FWD_QCN_MIRROR_CFG.MIRROR_SESSION;
      this.FWD_IEEE_RESERVED_MAC_ACTION = ral_reg_fwd_misc_FWD_IEEE_RESERVED_MAC_ACTION::type_id::create("FWD_IEEE_RESERVED_MAC_ACTION",,get_full_name());
      this.FWD_IEEE_RESERVED_MAC_ACTION.configure(this, null, "");
      this.FWD_IEEE_RESERVED_MAC_ACTION.build();
         this.FWD_IEEE_RESERVED_MAC_ACTION.add_hdl_path('{

            '{"FWD_IEEE_RESERVED_MAC_ACTION", -1, -1}
         });
      this.default_map.add_reg(this.FWD_IEEE_RESERVED_MAC_ACTION, `UVM_REG_ADDR_WIDTH'h1230, "RW", 0);
		this.FWD_IEEE_RESERVED_MAC_ACTION_ACTION_63 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_63;
		this.ACTION_63 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_63;
		this.FWD_IEEE_RESERVED_MAC_ACTION_ACTION_62 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_62;
		this.ACTION_62 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_62;
		this.FWD_IEEE_RESERVED_MAC_ACTION_ACTION_61 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_61;
		this.ACTION_61 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_61;
		this.FWD_IEEE_RESERVED_MAC_ACTION_ACTION_60 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_60;
		this.ACTION_60 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_60;
		this.FWD_IEEE_RESERVED_MAC_ACTION_ACTION_59 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_59;
		this.ACTION_59 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_59;
		this.FWD_IEEE_RESERVED_MAC_ACTION_ACTION_58 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_58;
		this.ACTION_58 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_58;
		this.FWD_IEEE_RESERVED_MAC_ACTION_ACTION_57 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_57;
		this.ACTION_57 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_57;
		this.FWD_IEEE_RESERVED_MAC_ACTION_ACTION_56 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_56;
		this.ACTION_56 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_56;
		this.FWD_IEEE_RESERVED_MAC_ACTION_ACTION_55 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_55;
		this.ACTION_55 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_55;
		this.FWD_IEEE_RESERVED_MAC_ACTION_ACTION_54 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_54;
		this.ACTION_54 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_54;
		this.FWD_IEEE_RESERVED_MAC_ACTION_ACTION_53 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_53;
		this.ACTION_53 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_53;
		this.FWD_IEEE_RESERVED_MAC_ACTION_ACTION_52 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_52;
		this.ACTION_52 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_52;
		this.FWD_IEEE_RESERVED_MAC_ACTION_ACTION_51 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_51;
		this.ACTION_51 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_51;
		this.FWD_IEEE_RESERVED_MAC_ACTION_ACTION_50 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_50;
		this.ACTION_50 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_50;
		this.FWD_IEEE_RESERVED_MAC_ACTION_ACTION_49 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_49;
		this.ACTION_49 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_49;
		this.FWD_IEEE_RESERVED_MAC_ACTION_ACTION_48 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_48;
		this.ACTION_48 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_48;
		this.FWD_IEEE_RESERVED_MAC_ACTION_ACTION_47 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_47;
		this.ACTION_47 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_47;
		this.FWD_IEEE_RESERVED_MAC_ACTION_ACTION_46 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_46;
		this.ACTION_46 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_46;
		this.FWD_IEEE_RESERVED_MAC_ACTION_ACTION_45 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_45;
		this.ACTION_45 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_45;
		this.FWD_IEEE_RESERVED_MAC_ACTION_ACTION_44 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_44;
		this.ACTION_44 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_44;
		this.FWD_IEEE_RESERVED_MAC_ACTION_ACTION_43 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_43;
		this.ACTION_43 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_43;
		this.FWD_IEEE_RESERVED_MAC_ACTION_ACTION_42 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_42;
		this.ACTION_42 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_42;
		this.FWD_IEEE_RESERVED_MAC_ACTION_ACTION_41 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_41;
		this.ACTION_41 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_41;
		this.FWD_IEEE_RESERVED_MAC_ACTION_ACTION_40 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_40;
		this.ACTION_40 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_40;
		this.FWD_IEEE_RESERVED_MAC_ACTION_ACTION_39 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_39;
		this.ACTION_39 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_39;
		this.FWD_IEEE_RESERVED_MAC_ACTION_ACTION_38 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_38;
		this.ACTION_38 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_38;
		this.FWD_IEEE_RESERVED_MAC_ACTION_ACTION_37 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_37;
		this.ACTION_37 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_37;
		this.FWD_IEEE_RESERVED_MAC_ACTION_ACTION_36 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_36;
		this.ACTION_36 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_36;
		this.FWD_IEEE_RESERVED_MAC_ACTION_ACTION_35 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_35;
		this.ACTION_35 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_35;
		this.FWD_IEEE_RESERVED_MAC_ACTION_ACTION_34 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_34;
		this.ACTION_34 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_34;
		this.FWD_IEEE_RESERVED_MAC_ACTION_ACTION_33 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_33;
		this.ACTION_33 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_33;
		this.FWD_IEEE_RESERVED_MAC_ACTION_ACTION_32 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_32;
		this.ACTION_32 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_32;
		this.FWD_IEEE_RESERVED_MAC_ACTION_ACTION_31 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_31;
		this.ACTION_31 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_31;
		this.FWD_IEEE_RESERVED_MAC_ACTION_ACTION_30 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_30;
		this.ACTION_30 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_30;
		this.FWD_IEEE_RESERVED_MAC_ACTION_ACTION_29 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_29;
		this.ACTION_29 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_29;
		this.FWD_IEEE_RESERVED_MAC_ACTION_ACTION_28 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_28;
		this.ACTION_28 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_28;
		this.FWD_IEEE_RESERVED_MAC_ACTION_ACTION_27 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_27;
		this.ACTION_27 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_27;
		this.FWD_IEEE_RESERVED_MAC_ACTION_ACTION_26 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_26;
		this.ACTION_26 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_26;
		this.FWD_IEEE_RESERVED_MAC_ACTION_ACTION_25 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_25;
		this.ACTION_25 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_25;
		this.FWD_IEEE_RESERVED_MAC_ACTION_ACTION_24 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_24;
		this.ACTION_24 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_24;
		this.FWD_IEEE_RESERVED_MAC_ACTION_ACTION_23 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_23;
		this.ACTION_23 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_23;
		this.FWD_IEEE_RESERVED_MAC_ACTION_ACTION_22 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_22;
		this.ACTION_22 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_22;
		this.FWD_IEEE_RESERVED_MAC_ACTION_ACTION_21 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_21;
		this.ACTION_21 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_21;
		this.FWD_IEEE_RESERVED_MAC_ACTION_ACTION_20 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_20;
		this.ACTION_20 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_20;
		this.FWD_IEEE_RESERVED_MAC_ACTION_ACTION_19 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_19;
		this.ACTION_19 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_19;
		this.FWD_IEEE_RESERVED_MAC_ACTION_ACTION_18 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_18;
		this.ACTION_18 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_18;
		this.FWD_IEEE_RESERVED_MAC_ACTION_ACTION_17 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_17;
		this.ACTION_17 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_17;
		this.FWD_IEEE_RESERVED_MAC_ACTION_ACTION_16 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_16;
		this.ACTION_16 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_16;
		this.FWD_IEEE_RESERVED_MAC_ACTION_ACTION_15 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_15;
		this.ACTION_15 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_15;
		this.FWD_IEEE_RESERVED_MAC_ACTION_ACTION_14 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_14;
		this.ACTION_14 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_14;
		this.FWD_IEEE_RESERVED_MAC_ACTION_ACTION_13 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_13;
		this.ACTION_13 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_13;
		this.FWD_IEEE_RESERVED_MAC_ACTION_ACTION_12 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_12;
		this.ACTION_12 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_12;
		this.FWD_IEEE_RESERVED_MAC_ACTION_ACTION_11 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_11;
		this.ACTION_11 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_11;
		this.FWD_IEEE_RESERVED_MAC_ACTION_ACTION_10 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_10;
		this.ACTION_10 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_10;
		this.FWD_IEEE_RESERVED_MAC_ACTION_ACTION_9 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_9;
		this.ACTION_9 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_9;
		this.FWD_IEEE_RESERVED_MAC_ACTION_ACTION_8 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_8;
		this.ACTION_8 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_8;
		this.FWD_IEEE_RESERVED_MAC_ACTION_ACTION_7 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_7;
		this.ACTION_7 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_7;
		this.FWD_IEEE_RESERVED_MAC_ACTION_ACTION_6 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_6;
		this.ACTION_6 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_6;
		this.FWD_IEEE_RESERVED_MAC_ACTION_ACTION_5 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_5;
		this.ACTION_5 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_5;
		this.FWD_IEEE_RESERVED_MAC_ACTION_ACTION_4 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_4;
		this.ACTION_4 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_4;
		this.FWD_IEEE_RESERVED_MAC_ACTION_ACTION_3 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_3;
		this.ACTION_3 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_3;
		this.FWD_IEEE_RESERVED_MAC_ACTION_ACTION_2 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_2;
		this.ACTION_2 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_2;
		this.FWD_IEEE_RESERVED_MAC_ACTION_ACTION_1 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_1;
		this.ACTION_1 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_1;
		this.FWD_IEEE_RESERVED_MAC_ACTION_ACTION_0 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_0;
		this.ACTION_0 = this.FWD_IEEE_RESERVED_MAC_ACTION.ACTION_0;
      this.FWD_IEEE_RESERVED_MAC_TRAP_PRIORITY = ral_reg_fwd_misc_FWD_IEEE_RESERVED_MAC_TRAP_PRIORITY::type_id::create("FWD_IEEE_RESERVED_MAC_TRAP_PRIORITY",,get_full_name());
      this.FWD_IEEE_RESERVED_MAC_TRAP_PRIORITY.configure(this, null, "");
      this.FWD_IEEE_RESERVED_MAC_TRAP_PRIORITY.build();
         this.FWD_IEEE_RESERVED_MAC_TRAP_PRIORITY.add_hdl_path('{

            '{"FWD_IEEE_RESERVED_MAC_TRAP_PRIORITY", -1, -1}
         });
      this.default_map.add_reg(this.FWD_IEEE_RESERVED_MAC_TRAP_PRIORITY, `UVM_REG_ADDR_WIDTH'h1340, "RW", 0);
		this.FWD_IEEE_RESERVED_MAC_TRAP_PRIORITY_SELECT = this.FWD_IEEE_RESERVED_MAC_TRAP_PRIORITY.SELECT;
		this.SELECT = this.FWD_IEEE_RESERVED_MAC_TRAP_PRIORITY.SELECT;
      this.FWD_IEEE_RESERVED_MAC_CFG = ral_reg_fwd_misc_FWD_IEEE_RESERVED_MAC_CFG::type_id::create("FWD_IEEE_RESERVED_MAC_CFG",,get_full_name());
      this.FWD_IEEE_RESERVED_MAC_CFG.configure(this, null, "");
      this.FWD_IEEE_RESERVED_MAC_CFG.build();
         this.FWD_IEEE_RESERVED_MAC_CFG.add_hdl_path('{

            '{"FWD_IEEE_RESERVED_MAC_CFG", -1, -1}
         });
      this.default_map.add_reg(this.FWD_IEEE_RESERVED_MAC_CFG, `UVM_REG_ADDR_WIDTH'h1350, "RW", 0);
		this.FWD_IEEE_RESERVED_MAC_CFG_RSVD0 = this.FWD_IEEE_RESERVED_MAC_CFG.RSVD0;
		this.FWD_IEEE_RESERVED_MAC_CFG_TRAP_TC = this.FWD_IEEE_RESERVED_MAC_CFG.TRAP_TC;
		this.TRAP_TC = this.FWD_IEEE_RESERVED_MAC_CFG.TRAP_TC;
      this.FWD_IP = ral_reg_fwd_misc_FWD_IP::type_id::create("FWD_IP",,get_full_name());
      this.FWD_IP.configure(this, null, "");
      this.FWD_IP.build();
         this.FWD_IP.add_hdl_path('{

            '{"FWD_IP", -1, -1}
         });
      this.default_map.add_reg(this.FWD_IP, `UVM_REG_ADDR_WIDTH'h1360, "RW", 0);
		this.FWD_IP_RSVD0 = this.FWD_IP.RSVD0;
		this.FWD_IP_MA_TCN = this.FWD_IP.MA_TCN;
		this.FWD_IP_WB_FIFO_PERR = this.FWD_IP.WB_FIFO_PERR;
		this.FWD_IP_TRIGGER = this.FWD_IP.TRIGGER;
		this.FWD_IP_TCAM_ERR = this.FWD_IP.TCAM_ERR;
		this.FWD_IP_ENTRY_COUNT = this.FWD_IP.ENTRY_COUNT;
		this.FWD_IP_SHELL_CTRL_U_ERR = this.FWD_IP.SHELL_CTRL_U_ERR;
      this.FWD_IM = ral_reg_fwd_misc_FWD_IM::type_id::create("FWD_IM",,get_full_name());
      this.FWD_IM.configure(this, null, "");
      this.FWD_IM.build();
         this.FWD_IM.add_hdl_path('{

            '{"FWD_IM", -1, -1}
         });
      this.default_map.add_reg(this.FWD_IM, `UVM_REG_ADDR_WIDTH'h1370, "RW", 0);
		this.FWD_IM_RSVD0 = this.FWD_IM.RSVD0;
		this.FWD_IM_MA_TCN = this.FWD_IM.MA_TCN;
		this.FWD_IM_WB_FIFO_PERR = this.FWD_IM.WB_FIFO_PERR;
		this.FWD_IM_TRIGGER = this.FWD_IM.TRIGGER;
		this.FWD_IM_TCAM_ERR = this.FWD_IM.TCAM_ERR;
		this.FWD_IM_ENTRY_COUNT = this.FWD_IM.ENTRY_COUNT;
		this.FWD_IM_SHELL_CTRL_U_ERR = this.FWD_IM.SHELL_CTRL_U_ERR;
   endfunction : build

	`uvm_object_utils(ral_block_fwd_misc)

endclass : ral_block_fwd_misc



`endif
