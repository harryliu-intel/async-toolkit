`ifndef RAL_MAPPER
`define RAL_MAPPER

import uvm_pkg::*;

class ral_reg_mapper_MAP_PORT_CFG extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field DEFAULT_SGLORT;
	rand uvm_reg_field DEFAULT_SGLORT_EN;
	rand uvm_reg_field PORT_PROFILE;

	function new(string name = "mapper_MAP_PORT_CFG");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 39, 25, "RO", 0, 39'h0, 1, 0, 0);
      this.DEFAULT_SGLORT = uvm_reg_field::type_id::create("DEFAULT_SGLORT",,get_full_name());
      this.DEFAULT_SGLORT.configure(this, 16, 9, "RW", 0, 16'h0, 1, 0, 0);
      this.DEFAULT_SGLORT_EN = uvm_reg_field::type_id::create("DEFAULT_SGLORT_EN",,get_full_name());
      this.DEFAULT_SGLORT_EN.configure(this, 1, 8, "RW", 0, 1'h0, 1, 0, 0);
      this.PORT_PROFILE = uvm_reg_field::type_id::create("PORT_PROFILE",,get_full_name());
      this.PORT_PROFILE.configure(this, 8, 0, "RW", 0, 8'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_mapper_MAP_PORT_CFG)

endclass : ral_reg_mapper_MAP_PORT_CFG


class ral_reg_mapper_MAP_PORT_DEFAULT extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field VALUE;
	uvm_reg_field RSVD1;
	rand uvm_reg_field TARGET;

	function new(string name = "mapper_MAP_PORT_DEFAULT");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 32, 32, "RO", 0, 32'h0, 1, 0, 1);
      this.VALUE = uvm_reg_field::type_id::create("VALUE",,get_full_name());
      this.VALUE.configure(this, 16, 16, "RW", 0, 16'h0, 1, 0, 1);
      this.RSVD1 = uvm_reg_field::type_id::create("RSVD1",,get_full_name());
      this.RSVD1.configure(this, 8, 8, "RO", 0, 8'h0, 1, 0, 1);
      this.TARGET = uvm_reg_field::type_id::create("TARGET",,get_full_name());
      this.TARGET.configure(this, 8, 0, "RW", 0, 8'hff, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_mapper_MAP_PORT_DEFAULT)

endclass : ral_reg_mapper_MAP_PORT_DEFAULT


class ral_reg_mapper_MAP_DOMAIN_TCAM extends uvm_reg;
	uvm_reg_field _RSVD1_;
	rand uvm_reg_field PORT_KEY_INVERT;
	rand uvm_reg_field VID2_VALID_INVERT;
	rand uvm_reg_field VID2_KEY_INVERT;
	rand uvm_reg_field VID1_VALID_INVERT;
	rand uvm_reg_field VID1_KEY_INVERT;
	uvm_reg_field _RSVD0_;
	rand uvm_reg_field PORT_KEY;
	rand uvm_reg_field VID2_VALID;
	rand uvm_reg_field VID2_KEY;
	rand uvm_reg_field VID1_VALID;
	rand uvm_reg_field VID1_KEY;

	function new(string name = "mapper_MAP_DOMAIN_TCAM");
		super.new(name, 128,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this._RSVD1_ = uvm_reg_field::type_id::create("_RSVD1_",,get_full_name());
      this._RSVD1_.configure(this, 20, 108, "RO", 1, 20'h0, 1, 0, 0);
      this.PORT_KEY_INVERT = uvm_reg_field::type_id::create("PORT_KEY_INVERT",,get_full_name());
      this.PORT_KEY_INVERT.configure(this, 18, 90, "RW", 0, 18'h3ffff, 1, 0, 0);
      this.VID2_VALID_INVERT = uvm_reg_field::type_id::create("VID2_VALID_INVERT",,get_full_name());
      this.VID2_VALID_INVERT.configure(this, 1, 89, "RW", 0, 1'h1, 1, 0, 0);
      this.VID2_KEY_INVERT = uvm_reg_field::type_id::create("VID2_KEY_INVERT",,get_full_name());
      this.VID2_KEY_INVERT.configure(this, 12, 77, "RW", 0, 12'hfff, 1, 0, 0);
      this.VID1_VALID_INVERT = uvm_reg_field::type_id::create("VID1_VALID_INVERT",,get_full_name());
      this.VID1_VALID_INVERT.configure(this, 1, 76, "RW", 0, 1'h1, 1, 0, 0);
      this.VID1_KEY_INVERT = uvm_reg_field::type_id::create("VID1_KEY_INVERT",,get_full_name());
      this.VID1_KEY_INVERT.configure(this, 12, 64, "RW", 0, 12'hfff, 1, 0, 0);
      this._RSVD0_ = uvm_reg_field::type_id::create("_RSVD0_",,get_full_name());
      this._RSVD0_.configure(this, 20, 44, "RO", 1, 20'h0, 1, 0, 0);
      this.PORT_KEY = uvm_reg_field::type_id::create("PORT_KEY",,get_full_name());
      this.PORT_KEY.configure(this, 18, 26, "RW", 0, 18'h3ffff, 1, 0, 0);
      this.VID2_VALID = uvm_reg_field::type_id::create("VID2_VALID",,get_full_name());
      this.VID2_VALID.configure(this, 1, 25, "RW", 0, 1'h1, 1, 0, 0);
      this.VID2_KEY = uvm_reg_field::type_id::create("VID2_KEY",,get_full_name());
      this.VID2_KEY.configure(this, 12, 13, "RW", 0, 12'hfff, 1, 0, 0);
      this.VID1_VALID = uvm_reg_field::type_id::create("VID1_VALID",,get_full_name());
      this.VID1_VALID.configure(this, 1, 12, "RW", 0, 1'h1, 1, 0, 0);
      this.VID1_KEY = uvm_reg_field::type_id::create("VID1_KEY",,get_full_name());
      this.VID1_KEY.configure(this, 12, 0, "RW", 0, 12'hfff, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_mapper_MAP_DOMAIN_TCAM)

endclass : ral_reg_mapper_MAP_DOMAIN_TCAM


class ral_reg_mapper_MAP_DOMAIN_ACTION0 extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field L2_DOMAIN;
	rand uvm_reg_field L3_DOMAIN;
	rand uvm_reg_field NAD;
	rand uvm_reg_field UPDATE_DOMAINS;
	rand uvm_reg_field LEARN_EN;
	rand uvm_reg_field LEARN_MODE;
	rand uvm_reg_field PRIORITY_PROFILE;
	rand uvm_reg_field PRI_SOURCE;
	rand uvm_reg_field FORCE_DEFAULT_PRI;
	rand uvm_reg_field DEFAULT_PRI;

	function new(string name = "mapper_MAP_DOMAIN_ACTION0");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 26, 38, "RO", 0, 26'h0, 1, 0, 0);
      this.L2_DOMAIN = uvm_reg_field::type_id::create("L2_DOMAIN",,get_full_name());
      this.L2_DOMAIN.configure(this, 8, 30, "RW", 0, 8'h0, 1, 0, 0);
      this.L3_DOMAIN = uvm_reg_field::type_id::create("L3_DOMAIN",,get_full_name());
      this.L3_DOMAIN.configure(this, 6, 24, "RW", 0, 6'h0, 1, 0, 0);
      this.NAD = uvm_reg_field::type_id::create("NAD",,get_full_name());
      this.NAD.configure(this, 4, 20, "RW", 0, 4'h0, 1, 0, 0);
      this.UPDATE_DOMAINS = uvm_reg_field::type_id::create("UPDATE_DOMAINS",,get_full_name());
      this.UPDATE_DOMAINS.configure(this, 1, 19, "RW", 0, 1'h0, 1, 0, 0);
      this.LEARN_EN = uvm_reg_field::type_id::create("LEARN_EN",,get_full_name());
      this.LEARN_EN.configure(this, 1, 18, "RW", 0, 1'h0, 1, 0, 0);
      this.LEARN_MODE = uvm_reg_field::type_id::create("LEARN_MODE",,get_full_name());
      this.LEARN_MODE.configure(this, 1, 17, "RW", 0, 1'h0, 1, 0, 0);
      this.PRIORITY_PROFILE = uvm_reg_field::type_id::create("PRIORITY_PROFILE",,get_full_name());
      this.PRIORITY_PROFILE.configure(this, 5, 12, "RW", 0, 5'h0, 1, 0, 0);
      this.PRI_SOURCE = uvm_reg_field::type_id::create("PRI_SOURCE",,get_full_name());
      this.PRI_SOURCE.configure(this, 8, 4, "RW", 0, 8'h0, 1, 0, 0);
      this.FORCE_DEFAULT_PRI = uvm_reg_field::type_id::create("FORCE_DEFAULT_PRI",,get_full_name());
      this.FORCE_DEFAULT_PRI.configure(this, 1, 3, "RW", 0, 1'h0, 1, 0, 0);
      this.DEFAULT_PRI = uvm_reg_field::type_id::create("DEFAULT_PRI",,get_full_name());
      this.DEFAULT_PRI.configure(this, 3, 0, "RW", 0, 3'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_mapper_MAP_DOMAIN_ACTION0)

endclass : ral_reg_mapper_MAP_DOMAIN_ACTION0


class ral_reg_mapper_MAP_DOMAIN_ACTION1 extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field DOMAIN_PROFILE;
	rand uvm_reg_field L2_POLICER;
	rand uvm_reg_field L3_POLICER;
	rand uvm_reg_field VLAN_COUNTER;

	function new(string name = "mapper_MAP_DOMAIN_ACTION1");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 20, 44, "RO", 0, 20'h0, 1, 0, 0);
      this.DOMAIN_PROFILE = uvm_reg_field::type_id::create("DOMAIN_PROFILE",,get_full_name());
      this.DOMAIN_PROFILE.configure(this, 8, 36, "RW", 0, 8'h0, 1, 0, 0);
      this.L2_POLICER = uvm_reg_field::type_id::create("L2_POLICER",,get_full_name());
      this.L2_POLICER.configure(this, 12, 24, "RW", 0, 12'h0, 1, 0, 0);
      this.L3_POLICER = uvm_reg_field::type_id::create("L3_POLICER",,get_full_name());
      this.L3_POLICER.configure(this, 12, 12, "RW", 0, 12'h0, 1, 0, 0);
      this.VLAN_COUNTER = uvm_reg_field::type_id::create("VLAN_COUNTER",,get_full_name());
      this.VLAN_COUNTER.configure(this, 12, 0, "RW", 0, 12'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_mapper_MAP_DOMAIN_ACTION1)

endclass : ral_reg_mapper_MAP_DOMAIN_ACTION1


class ral_reg_mapper_MAP_DOMAIN_PROFILE extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field PRIORITY_PROFILE;

	function new(string name = "mapper_MAP_DOMAIN_PROFILE");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 59, 5, "RO", 0, 59'h0, 1, 0, 0);
      this.PRIORITY_PROFILE = uvm_reg_field::type_id::create("PRIORITY_PROFILE",,get_full_name());
      this.PRIORITY_PROFILE.configure(this, 5, 0, "RW", 0, 5'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_mapper_MAP_DOMAIN_PROFILE)

endclass : ral_reg_mapper_MAP_DOMAIN_PROFILE


class ral_reg_mapper_MAP_PORT extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field MAP_PORT;

	function new(string name = "mapper_MAP_PORT");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 56, 8, "RO", 0, 56'h0, 1, 0, 1);
      this.MAP_PORT = uvm_reg_field::type_id::create("MAP_PORT",,get_full_name());
      this.MAP_PORT.configure(this, 8, 0, "RW", 0, 8'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_mapper_MAP_PORT)

endclass : ral_reg_mapper_MAP_PORT


class ral_reg_mapper_MAP_MAC extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field MAC_ROUTABLE;
	rand uvm_reg_field MAP_MAC;
	rand uvm_reg_field VALID;
	rand uvm_reg_field IGNORE_LENGTH;
	rand uvm_reg_field MAC;

	function new(string name = "mapper_MAP_MAC");
		super.new(name, 128,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 61, 67, "RO", 0, 61'h0, 1, 0, 0);
      this.MAC_ROUTABLE = uvm_reg_field::type_id::create("MAC_ROUTABLE",,get_full_name());
      this.MAC_ROUTABLE.configure(this, 1, 66, "RW", 0, 1'h0, 1, 0, 0);
      this.MAP_MAC = uvm_reg_field::type_id::create("MAP_MAC",,get_full_name());
      this.MAP_MAC.configure(this, 8, 58, "RW", 0, 8'h0, 1, 0, 0);
      this.VALID = uvm_reg_field::type_id::create("VALID",,get_full_name());
      this.VALID.configure(this, 4, 54, "RW", 0, 4'h0, 1, 0, 0);
      this.IGNORE_LENGTH = uvm_reg_field::type_id::create("IGNORE_LENGTH",,get_full_name());
      this.IGNORE_LENGTH.configure(this, 6, 48, "RW", 0, 6'h0, 1, 0, 0);
      this.MAC = uvm_reg_field::type_id::create("MAC",,get_full_name());
      this.MAC.configure(this, 48, 0, "RW", 0, 48'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_mapper_MAP_MAC)

endclass : ral_reg_mapper_MAP_MAC


class ral_reg_mapper_MAP_IP_LO extends uvm_reg;
	rand uvm_reg_field IP_LO;

	function new(string name = "mapper_MAP_IP_LO");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.IP_LO = uvm_reg_field::type_id::create("IP_LO",,get_full_name());
      this.IP_LO.configure(this, 64, 0, "RW", 0, 64'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_mapper_MAP_IP_LO)

endclass : ral_reg_mapper_MAP_IP_LO


class ral_reg_mapper_MAP_IP_HI extends uvm_reg;
	rand uvm_reg_field IP_HI;

	function new(string name = "mapper_MAP_IP_HI");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.IP_HI = uvm_reg_field::type_id::create("IP_HI",,get_full_name());
      this.IP_HI.configure(this, 64, 0, "RW", 0, 64'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_mapper_MAP_IP_HI)

endclass : ral_reg_mapper_MAP_IP_HI


class ral_reg_mapper_MAP_IP_CFG extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field MATCH_LENGTH;
	rand uvm_reg_field VALID;
	rand uvm_reg_field MAP_IP;
	rand uvm_reg_field IP_PROFILE;
	rand uvm_reg_field IS_IPV6;

	function new(string name = "mapper_MAP_IP_CFG");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 45, 19, "RO", 0, 45'h0, 1, 0, 0);
      this.MATCH_LENGTH = uvm_reg_field::type_id::create("MATCH_LENGTH",,get_full_name());
      this.MATCH_LENGTH.configure(this, 8, 11, "RW", 0, 8'h0, 1, 0, 0);
      this.VALID = uvm_reg_field::type_id::create("VALID",,get_full_name());
      this.VALID.configure(this, 4, 7, "RW", 0, 4'h0, 1, 0, 0);
      this.MAP_IP = uvm_reg_field::type_id::create("MAP_IP",,get_full_name());
      this.MAP_IP.configure(this, 4, 3, "RW", 0, 4'h0, 1, 0, 0);
      this.IP_PROFILE = uvm_reg_field::type_id::create("IP_PROFILE",,get_full_name());
      this.IP_PROFILE.configure(this, 2, 1, "RW", 0, 2'h0, 1, 0, 0);
      this.IS_IPV6 = uvm_reg_field::type_id::create("IS_IPV6",,get_full_name());
      this.IS_IPV6.configure(this, 1, 0, "RW", 0, 1'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_mapper_MAP_IP_CFG)

endclass : ral_reg_mapper_MAP_IP_CFG


class ral_reg_mapper_MAP_PROT extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field MAP_PROT;
	rand uvm_reg_field PROT;

	function new(string name = "mapper_MAP_PROT");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 53, 11, "RO", 0, 53'h0, 1, 0, 0);
      this.MAP_PROT = uvm_reg_field::type_id::create("MAP_PROT",,get_full_name());
      this.MAP_PROT.configure(this, 3, 8, "RW", 0, 3'h0, 1, 0, 0);
      this.PROT = uvm_reg_field::type_id::create("PROT",,get_full_name());
      this.PROT.configure(this, 8, 0, "RW", 0, 8'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_mapper_MAP_PROT)

endclass : ral_reg_mapper_MAP_PROT


class ral_reg_mapper_MAP_L4_SRC extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field MAP_L4_SRC;
	rand uvm_reg_field VALID;
	rand uvm_reg_field MAP_PROT;
	rand uvm_reg_field L4_SRC;

	function new(string name = "mapper_MAP_L4_SRC");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 27, 37, "RO", 0, 27'h0, 1, 0, 0);
      this.MAP_L4_SRC = uvm_reg_field::type_id::create("MAP_L4_SRC",,get_full_name());
      this.MAP_L4_SRC.configure(this, 16, 21, "RW", 0, 16'h0, 1, 0, 0);
      this.VALID = uvm_reg_field::type_id::create("VALID",,get_full_name());
      this.VALID.configure(this, 2, 19, "RW", 0, 2'h0, 1, 0, 0);
      this.MAP_PROT = uvm_reg_field::type_id::create("MAP_PROT",,get_full_name());
      this.MAP_PROT.configure(this, 3, 16, "RW", 0, 3'h0, 1, 0, 0);
      this.L4_SRC = uvm_reg_field::type_id::create("L4_SRC",,get_full_name());
      this.L4_SRC.configure(this, 16, 0, "RW", 0, 16'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_mapper_MAP_L4_SRC)

endclass : ral_reg_mapper_MAP_L4_SRC


class ral_reg_mapper_MAP_L4_DST extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field MAP_L4_DST;
	rand uvm_reg_field VALID;
	rand uvm_reg_field MAP_PROT;
	rand uvm_reg_field L4_DST;

	function new(string name = "mapper_MAP_L4_DST");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 27, 37, "RO", 0, 27'h0, 1, 0, 0);
      this.MAP_L4_DST = uvm_reg_field::type_id::create("MAP_L4_DST",,get_full_name());
      this.MAP_L4_DST.configure(this, 16, 21, "RW", 0, 16'h0, 1, 0, 0);
      this.VALID = uvm_reg_field::type_id::create("VALID",,get_full_name());
      this.VALID.configure(this, 2, 19, "RW", 0, 2'h0, 1, 0, 0);
      this.MAP_PROT = uvm_reg_field::type_id::create("MAP_PROT",,get_full_name());
      this.MAP_PROT.configure(this, 3, 16, "RW", 0, 3'h0, 1, 0, 0);
      this.L4_DST = uvm_reg_field::type_id::create("L4_DST",,get_full_name());
      this.L4_DST.configure(this, 16, 0, "RW", 0, 16'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_mapper_MAP_L4_DST)

endclass : ral_reg_mapper_MAP_L4_DST


class ral_reg_mapper_MAP_EXP_TC extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field TC_BY_EXP;

	function new(string name = "mapper_MAP_EXP_TC");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 40, 24, "RO", 0, 40'h0, 1, 0, 1);
      this.TC_BY_EXP = uvm_reg_field::type_id::create("TC_BY_EXP",,get_full_name());
      this.TC_BY_EXP.configure(this, 24, 0, "RW", 0, 24'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_mapper_MAP_EXP_TC)

endclass : ral_reg_mapper_MAP_EXP_TC


class ral_reg_mapper_MAP_DSCP_TC extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field TC;
	rand uvm_reg_field DSCP;

	function new(string name = "mapper_MAP_DSCP_TC");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 55, 9, "RO", 0, 55'h0, 1, 0, 0);
      this.TC = uvm_reg_field::type_id::create("TC",,get_full_name());
      this.TC.configure(this, 3, 6, "RW", 0, 3'h0, 1, 0, 0);
      this.DSCP = uvm_reg_field::type_id::create("DSCP",,get_full_name());
      this.DSCP.configure(this, 6, 0, "RW", 0, 6'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_mapper_MAP_DSCP_TC)

endclass : ral_reg_mapper_MAP_DSCP_TC


class ral_reg_mapper_MAP_VPRI_TC extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field TC_BY_VPRI;

	function new(string name = "mapper_MAP_VPRI_TC");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 16, 48, "RO", 0, 16'h0, 1, 0, 1);
      this.TC_BY_VPRI = uvm_reg_field::type_id::create("TC_BY_VPRI",,get_full_name());
      this.TC_BY_VPRI.configure(this, 48, 0, "RW", 0, 48'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_mapper_MAP_VPRI_TC)

endclass : ral_reg_mapper_MAP_VPRI_TC


class ral_reg_mapper_MAP_VPRI extends uvm_reg;
	rand uvm_reg_field VPRI_BY_VPRI;

	function new(string name = "mapper_MAP_VPRI");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.VPRI_BY_VPRI = uvm_reg_field::type_id::create("VPRI_BY_VPRI",,get_full_name());
      this.VPRI_BY_VPRI.configure(this, 64, 0, "RW", 0, 64'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_mapper_MAP_VPRI)

endclass : ral_reg_mapper_MAP_VPRI


class ral_reg_mapper_MAP_PROFILE_KEY0 extends uvm_reg;
	uvm_reg_field _RSVD1_;
	rand uvm_reg_field PTRS_ERR;
	rand uvm_reg_field EX;
	rand uvm_reg_field CSUM;
	uvm_reg_field _RSVD0_;
	rand uvm_reg_field IHL_OK;
	rand uvm_reg_field IHL_FITS;
	rand uvm_reg_field FLAGS;
	rand uvm_reg_field RSVD;

	function new(string name = "mapper_MAP_PROFILE_KEY0");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this._RSVD1_ = uvm_reg_field::type_id::create("_RSVD1_",,get_full_name());
      this._RSVD1_.configure(this, 4, 60, "RO", 1, 4'h0, 1, 0, 0);
      this.PTRS_ERR = uvm_reg_field::type_id::create("PTRS_ERR",,get_full_name());
      this.PTRS_ERR.configure(this, 1, 59, "RW", 0, 1'h1, 1, 0, 0);
      this.EX = uvm_reg_field::type_id::create("EX",,get_full_name());
      this.EX.configure(this, 3, 56, "RW", 0, 3'h7, 1, 0, 0);
      this.CSUM = uvm_reg_field::type_id::create("CSUM",,get_full_name());
      this.CSUM.configure(this, 2, 54, "RW", 0, 2'h3, 1, 0, 0);
      this._RSVD0_ = uvm_reg_field::type_id::create("_RSVD0_",,get_full_name());
      this._RSVD0_.configure(this, 4, 50, "RO", 1, 4'h0, 1, 0, 0);
      this.IHL_OK = uvm_reg_field::type_id::create("IHL_OK",,get_full_name());
      this.IHL_OK.configure(this, 1, 49, "RW", 0, 1'h1, 1, 0, 0);
      this.IHL_FITS = uvm_reg_field::type_id::create("IHL_FITS",,get_full_name());
      this.IHL_FITS.configure(this, 1, 48, "RW", 0, 1'h1, 1, 0, 0);
      this.FLAGS = uvm_reg_field::type_id::create("FLAGS",,get_full_name());
      this.FLAGS.configure(this, 47, 1, "RW", 0, 47'h7fffffffffff, 1, 0, 0);
      this.RSVD = uvm_reg_field::type_id::create("RSVD",,get_full_name());
      this.RSVD.configure(this, 1, 0, "RW", 0, 1'h1, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_mapper_MAP_PROFILE_KEY0)

endclass : ral_reg_mapper_MAP_PROFILE_KEY0


class ral_reg_mapper_MAP_PROFILE_KEY_INVERT0 extends uvm_reg;
	uvm_reg_field _RSVD1_;
	rand uvm_reg_field PTRS_ERR;
	rand uvm_reg_field EX;
	rand uvm_reg_field CSUM;
	uvm_reg_field _RSVD0_;
	rand uvm_reg_field IHL_OK;
	rand uvm_reg_field IHL_FITS;
	rand uvm_reg_field FLAGS;
	rand uvm_reg_field RSVD;

	function new(string name = "mapper_MAP_PROFILE_KEY_INVERT0");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this._RSVD1_ = uvm_reg_field::type_id::create("_RSVD1_",,get_full_name());
      this._RSVD1_.configure(this, 4, 60, "RO", 1, 4'h0, 1, 0, 0);
      this.PTRS_ERR = uvm_reg_field::type_id::create("PTRS_ERR",,get_full_name());
      this.PTRS_ERR.configure(this, 1, 59, "RW", 0, 1'h1, 1, 0, 0);
      this.EX = uvm_reg_field::type_id::create("EX",,get_full_name());
      this.EX.configure(this, 3, 56, "RW", 0, 3'h7, 1, 0, 0);
      this.CSUM = uvm_reg_field::type_id::create("CSUM",,get_full_name());
      this.CSUM.configure(this, 2, 54, "RW", 0, 2'h3, 1, 0, 0);
      this._RSVD0_ = uvm_reg_field::type_id::create("_RSVD0_",,get_full_name());
      this._RSVD0_.configure(this, 4, 50, "RO", 1, 4'h0, 1, 0, 0);
      this.IHL_OK = uvm_reg_field::type_id::create("IHL_OK",,get_full_name());
      this.IHL_OK.configure(this, 1, 49, "RW", 0, 1'h1, 1, 0, 0);
      this.IHL_FITS = uvm_reg_field::type_id::create("IHL_FITS",,get_full_name());
      this.IHL_FITS.configure(this, 1, 48, "RW", 0, 1'h1, 1, 0, 0);
      this.FLAGS = uvm_reg_field::type_id::create("FLAGS",,get_full_name());
      this.FLAGS.configure(this, 47, 1, "RW", 0, 47'h7fffffffffff, 1, 0, 0);
      this.RSVD = uvm_reg_field::type_id::create("RSVD",,get_full_name());
      this.RSVD.configure(this, 1, 0, "RW", 0, 1'h1, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_mapper_MAP_PROFILE_KEY_INVERT0)

endclass : ral_reg_mapper_MAP_PROFILE_KEY_INVERT0


class ral_reg_mapper_MAP_PROFILE_KEY1 extends uvm_reg;
	uvm_reg_field _RSVD1_;
	rand uvm_reg_field PTYPE;
	rand uvm_reg_field L2_DOMAIN;
	rand uvm_reg_field L3_DOMAIN;
	uvm_reg_field _RSVD0_;
	rand uvm_reg_field PORT_PROFILE;
	rand uvm_reg_field DOMAIN_PROFILE;
	rand uvm_reg_field MAC_ROUTABLE;
	rand uvm_reg_field MAC_MBCAST;

	function new(string name = "mapper_MAP_PROFILE_KEY1");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this._RSVD1_ = uvm_reg_field::type_id::create("_RSVD1_",,get_full_name());
      this._RSVD1_.configure(this, 14, 50, "RO", 1, 14'h0, 1, 0, 0);
      this.PTYPE = uvm_reg_field::type_id::create("PTYPE",,get_full_name());
      this.PTYPE.configure(this, 10, 40, "RW", 0, 10'h0, 1, 0, 0);
      this.L2_DOMAIN = uvm_reg_field::type_id::create("L2_DOMAIN",,get_full_name());
      this.L2_DOMAIN.configure(this, 8, 32, "RW", 0, 8'hff, 1, 0, 1);
      this.L3_DOMAIN = uvm_reg_field::type_id::create("L3_DOMAIN",,get_full_name());
      this.L3_DOMAIN.configure(this, 6, 26, "RW", 0, 6'h3f, 1, 0, 0);
      this._RSVD0_ = uvm_reg_field::type_id::create("_RSVD0_",,get_full_name());
      this._RSVD0_.configure(this, 4, 22, "RO", 1, 4'h0, 1, 0, 0);
      this.PORT_PROFILE = uvm_reg_field::type_id::create("PORT_PROFILE",,get_full_name());
      this.PORT_PROFILE.configure(this, 8, 14, "RW", 0, 8'hff, 1, 0, 0);
      this.DOMAIN_PROFILE = uvm_reg_field::type_id::create("DOMAIN_PROFILE",,get_full_name());
      this.DOMAIN_PROFILE.configure(this, 8, 6, "RW", 0, 8'hff, 1, 0, 0);
      this.MAC_ROUTABLE = uvm_reg_field::type_id::create("MAC_ROUTABLE",,get_full_name());
      this.MAC_ROUTABLE.configure(this, 4, 2, "RW", 0, 4'hf, 1, 0, 0);
      this.MAC_MBCAST = uvm_reg_field::type_id::create("MAC_MBCAST",,get_full_name());
      this.MAC_MBCAST.configure(this, 2, 0, "RW", 0, 2'h3, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_mapper_MAP_PROFILE_KEY1)

endclass : ral_reg_mapper_MAP_PROFILE_KEY1


class ral_reg_mapper_MAP_PROFILE_KEY_INVERT1 extends uvm_reg;
	uvm_reg_field _RSVD1_;
	rand uvm_reg_field PTYPE;
	rand uvm_reg_field L2_DOMAIN;
	rand uvm_reg_field L3_DOMAIN;
	uvm_reg_field _RSVD0_;
	rand uvm_reg_field PORT_PROFILE;
	rand uvm_reg_field DOMAIN_PROFILE;
	rand uvm_reg_field MAC_ROUTABLE;
	rand uvm_reg_field MAC_MBCAST;

	function new(string name = "mapper_MAP_PROFILE_KEY_INVERT1");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this._RSVD1_ = uvm_reg_field::type_id::create("_RSVD1_",,get_full_name());
      this._RSVD1_.configure(this, 14, 50, "RO", 1, 14'h0, 1, 0, 0);
      this.PTYPE = uvm_reg_field::type_id::create("PTYPE",,get_full_name());
      this.PTYPE.configure(this, 10, 40, "RW", 0, 10'h0, 1, 0, 0);
      this.L2_DOMAIN = uvm_reg_field::type_id::create("L2_DOMAIN",,get_full_name());
      this.L2_DOMAIN.configure(this, 8, 32, "RW", 0, 8'hff, 1, 0, 1);
      this.L3_DOMAIN = uvm_reg_field::type_id::create("L3_DOMAIN",,get_full_name());
      this.L3_DOMAIN.configure(this, 6, 26, "RW", 0, 6'h3f, 1, 0, 0);
      this._RSVD0_ = uvm_reg_field::type_id::create("_RSVD0_",,get_full_name());
      this._RSVD0_.configure(this, 4, 22, "RO", 1, 4'h0, 1, 0, 0);
      this.PORT_PROFILE = uvm_reg_field::type_id::create("PORT_PROFILE",,get_full_name());
      this.PORT_PROFILE.configure(this, 8, 14, "RW", 0, 8'hff, 1, 0, 0);
      this.DOMAIN_PROFILE = uvm_reg_field::type_id::create("DOMAIN_PROFILE",,get_full_name());
      this.DOMAIN_PROFILE.configure(this, 8, 6, "RW", 0, 8'hff, 1, 0, 0);
      this.MAC_ROUTABLE = uvm_reg_field::type_id::create("MAC_ROUTABLE",,get_full_name());
      this.MAC_ROUTABLE.configure(this, 4, 2, "RW", 0, 4'hf, 1, 0, 0);
      this.MAC_MBCAST = uvm_reg_field::type_id::create("MAC_MBCAST",,get_full_name());
      this.MAC_MBCAST.configure(this, 2, 0, "RW", 0, 2'h3, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_mapper_MAP_PROFILE_KEY_INVERT1)

endclass : ral_reg_mapper_MAP_PROFILE_KEY_INVERT1


class ral_reg_mapper_MAP_PROFILE_ACTION extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field PROFILE_VALID;
	rand uvm_reg_field PROFILE;
	rand uvm_reg_field REWRITE_PROFILE;
	rand uvm_reg_field TRIG_VALID;
	rand uvm_reg_field PROFILE_TRIG;
	rand uvm_reg_field PARSER_ERROR;
	rand uvm_reg_field IP_OPTIONS_MASK;
	rand uvm_reg_field PRIOS_VALID;
	rand uvm_reg_field VPRI_TGT;
	rand uvm_reg_field DSCP_TGT;

	function new(string name = "mapper_MAP_PROFILE_ACTION");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 29, 35, "RO", 0, 29'h0, 1, 0, 0);
      this.PROFILE_VALID = uvm_reg_field::type_id::create("PROFILE_VALID",,get_full_name());
      this.PROFILE_VALID.configure(this, 1, 34, "RW", 0, 1'h0, 1, 0, 0);
      this.PROFILE = uvm_reg_field::type_id::create("PROFILE",,get_full_name());
      this.PROFILE.configure(this, 6, 28, "RW", 0, 6'h0, 1, 0, 0);
      this.REWRITE_PROFILE = uvm_reg_field::type_id::create("REWRITE_PROFILE",,get_full_name());
      this.REWRITE_PROFILE.configure(this, 4, 24, "RW", 0, 4'h0, 1, 0, 0);
      this.TRIG_VALID = uvm_reg_field::type_id::create("TRIG_VALID",,get_full_name());
      this.TRIG_VALID.configure(this, 1, 23, "RW", 0, 1'h0, 1, 0, 0);
      this.PROFILE_TRIG = uvm_reg_field::type_id::create("PROFILE_TRIG",,get_full_name());
      this.PROFILE_TRIG.configure(this, 8, 15, "RW", 0, 8'h0, 1, 0, 0);
      this.PARSER_ERROR = uvm_reg_field::type_id::create("PARSER_ERROR",,get_full_name());
      this.PARSER_ERROR.configure(this, 1, 14, "RW", 0, 1'h0, 1, 0, 0);
      this.IP_OPTIONS_MASK = uvm_reg_field::type_id::create("IP_OPTIONS_MASK",,get_full_name());
      this.IP_OPTIONS_MASK.configure(this, 7, 7, "RW", 0, 7'h0, 1, 0, 0);
      this.PRIOS_VALID = uvm_reg_field::type_id::create("PRIOS_VALID",,get_full_name());
      this.PRIOS_VALID.configure(this, 1, 6, "RW", 0, 1'h0, 1, 0, 0);
      this.VPRI_TGT = uvm_reg_field::type_id::create("VPRI_TGT",,get_full_name());
      this.VPRI_TGT.configure(this, 3, 3, "RW", 0, 3'h0, 1, 0, 0);
      this.DSCP_TGT = uvm_reg_field::type_id::create("DSCP_TGT",,get_full_name());
      this.DSCP_TGT.configure(this, 3, 0, "RW", 0, 3'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_mapper_MAP_PROFILE_ACTION)

endclass : ral_reg_mapper_MAP_PROFILE_ACTION


class ral_reg_mapper_MAP_DOMAIN_POL_CFG extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field L3_COLOR_CFG;
	rand uvm_reg_field L2_COLOR_CFG;

	function new(string name = "mapper_MAP_DOMAIN_POL_CFG");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 58, 6, "RO", 0, 58'h0, 1, 0, 0);
      this.L3_COLOR_CFG = uvm_reg_field::type_id::create("L3_COLOR_CFG",,get_full_name());
      this.L3_COLOR_CFG.configure(this, 3, 3, "RW", 0, 3'h0, 1, 0, 0);
      this.L2_COLOR_CFG = uvm_reg_field::type_id::create("L2_COLOR_CFG",,get_full_name());
      this.L2_COLOR_CFG.configure(this, 3, 0, "RW", 0, 3'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_mapper_MAP_DOMAIN_POL_CFG)

endclass : ral_reg_mapper_MAP_DOMAIN_POL_CFG


class ral_reg_mapper_MAP_REWRITE extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field SRC_ID;

	function new(string name = "mapper_MAP_REWRITE");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 58, 6, "RO", 0, 58'h0, 1, 0, 0);
      this.SRC_ID = uvm_reg_field::type_id::create("SRC_ID",,get_full_name());
      this.SRC_ID.configure(this, 6, 0, "RW", 0, 6'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_mapper_MAP_REWRITE)

endclass : ral_reg_mapper_MAP_REWRITE


class ral_block_mapper extends uvm_reg_block;
	rand ral_reg_mapper_MAP_PORT_CFG MAP_PORT_CFG[18];
	rand ral_reg_mapper_MAP_PORT_DEFAULT MAP_PORT_DEFAULT[18][0:5];
	rand ral_reg_mapper_MAP_DOMAIN_TCAM MAP_DOMAIN_TCAM[4096];
	rand ral_reg_mapper_MAP_DOMAIN_ACTION0 MAP_DOMAIN_ACTION0[4096];
	rand ral_reg_mapper_MAP_DOMAIN_ACTION1 MAP_DOMAIN_ACTION1[4096];
	rand ral_reg_mapper_MAP_DOMAIN_PROFILE MAP_DOMAIN_PROFILE[256];
	rand ral_reg_mapper_MAP_PORT MAP_PORT[18];
	rand ral_reg_mapper_MAP_MAC MAP_MAC[96];
	rand ral_reg_mapper_MAP_IP_LO MAP_IP_LO[16];
	rand ral_reg_mapper_MAP_IP_HI MAP_IP_HI[16];
	rand ral_reg_mapper_MAP_IP_CFG MAP_IP_CFG[16];
	rand ral_reg_mapper_MAP_PROT MAP_PROT[8];
	rand ral_reg_mapper_MAP_L4_SRC MAP_L4_SRC[64];
	rand ral_reg_mapper_MAP_L4_DST MAP_L4_DST[64];
	rand ral_reg_mapper_MAP_EXP_TC MAP_EXP_TC[32];
	rand ral_reg_mapper_MAP_DSCP_TC MAP_DSCP_TC[2048];
	rand ral_reg_mapper_MAP_VPRI_TC MAP_VPRI_TC[32];
	rand ral_reg_mapper_MAP_VPRI MAP_VPRI[32];
	rand ral_reg_mapper_MAP_PROFILE_KEY0 MAP_PROFILE_KEY0[96];
	rand ral_reg_mapper_MAP_PROFILE_KEY_INVERT0 MAP_PROFILE_KEY_INVERT0[96];
	rand ral_reg_mapper_MAP_PROFILE_KEY1 MAP_PROFILE_KEY1[96];
	rand ral_reg_mapper_MAP_PROFILE_KEY_INVERT1 MAP_PROFILE_KEY_INVERT1[96];
	rand ral_reg_mapper_MAP_PROFILE_ACTION MAP_PROFILE_ACTION[96];
	rand ral_reg_mapper_MAP_DOMAIN_POL_CFG MAP_DOMAIN_POL_CFG;
	rand ral_reg_mapper_MAP_REWRITE MAP_REWRITE[16][0:31];
	uvm_reg_field MAP_PORT_CFG_RSVD0[18];
	rand uvm_reg_field MAP_PORT_CFG_DEFAULT_SGLORT[18];
	rand uvm_reg_field DEFAULT_SGLORT[18];
	rand uvm_reg_field MAP_PORT_CFG_DEFAULT_SGLORT_EN[18];
	rand uvm_reg_field DEFAULT_SGLORT_EN[18];
	rand uvm_reg_field MAP_PORT_CFG_PORT_PROFILE[18];
	uvm_reg_field MAP_PORT_DEFAULT_RSVD0[18][0:5];
	rand uvm_reg_field MAP_PORT_DEFAULT_VALUE[18][0:5];
	rand uvm_reg_field VALUE[18][0:5];
	uvm_reg_field MAP_PORT_DEFAULT_RSVD1[18][0:5];
	uvm_reg_field RSVD1[18][0:5];
	rand uvm_reg_field MAP_PORT_DEFAULT_TARGET[18][0:5];
	rand uvm_reg_field TARGET[18][0:5];
	uvm_reg_field MAP_DOMAIN_TCAM__RSVD1_[4096];
	rand uvm_reg_field MAP_DOMAIN_TCAM_PORT_KEY_INVERT[4096];
	rand uvm_reg_field PORT_KEY_INVERT[4096];
	rand uvm_reg_field MAP_DOMAIN_TCAM_VID2_VALID_INVERT[4096];
	rand uvm_reg_field VID2_VALID_INVERT[4096];
	rand uvm_reg_field MAP_DOMAIN_TCAM_VID2_KEY_INVERT[4096];
	rand uvm_reg_field VID2_KEY_INVERT[4096];
	rand uvm_reg_field MAP_DOMAIN_TCAM_VID1_VALID_INVERT[4096];
	rand uvm_reg_field VID1_VALID_INVERT[4096];
	rand uvm_reg_field MAP_DOMAIN_TCAM_VID1_KEY_INVERT[4096];
	rand uvm_reg_field VID1_KEY_INVERT[4096];
	uvm_reg_field MAP_DOMAIN_TCAM__RSVD0_[4096];
	rand uvm_reg_field MAP_DOMAIN_TCAM_PORT_KEY[4096];
	rand uvm_reg_field PORT_KEY[4096];
	rand uvm_reg_field MAP_DOMAIN_TCAM_VID2_VALID[4096];
	rand uvm_reg_field VID2_VALID[4096];
	rand uvm_reg_field MAP_DOMAIN_TCAM_VID2_KEY[4096];
	rand uvm_reg_field VID2_KEY[4096];
	rand uvm_reg_field MAP_DOMAIN_TCAM_VID1_VALID[4096];
	rand uvm_reg_field VID1_VALID[4096];
	rand uvm_reg_field MAP_DOMAIN_TCAM_VID1_KEY[4096];
	rand uvm_reg_field VID1_KEY[4096];
	uvm_reg_field MAP_DOMAIN_ACTION0_RSVD0[4096];
	rand uvm_reg_field MAP_DOMAIN_ACTION0_L2_DOMAIN[4096];
	rand uvm_reg_field MAP_DOMAIN_ACTION0_L3_DOMAIN[4096];
	rand uvm_reg_field MAP_DOMAIN_ACTION0_NAD[4096];
	rand uvm_reg_field NAD[4096];
	rand uvm_reg_field MAP_DOMAIN_ACTION0_UPDATE_DOMAINS[4096];
	rand uvm_reg_field UPDATE_DOMAINS[4096];
	rand uvm_reg_field MAP_DOMAIN_ACTION0_LEARN_EN[4096];
	rand uvm_reg_field LEARN_EN[4096];
	rand uvm_reg_field MAP_DOMAIN_ACTION0_LEARN_MODE[4096];
	rand uvm_reg_field LEARN_MODE[4096];
	rand uvm_reg_field MAP_DOMAIN_ACTION0_PRIORITY_PROFILE[4096];
	rand uvm_reg_field MAP_DOMAIN_ACTION0_PRI_SOURCE[4096];
	rand uvm_reg_field PRI_SOURCE[4096];
	rand uvm_reg_field MAP_DOMAIN_ACTION0_FORCE_DEFAULT_PRI[4096];
	rand uvm_reg_field FORCE_DEFAULT_PRI[4096];
	rand uvm_reg_field MAP_DOMAIN_ACTION0_DEFAULT_PRI[4096];
	rand uvm_reg_field DEFAULT_PRI[4096];
	uvm_reg_field MAP_DOMAIN_ACTION1_RSVD0[4096];
	rand uvm_reg_field MAP_DOMAIN_ACTION1_DOMAIN_PROFILE[4096];
	rand uvm_reg_field MAP_DOMAIN_ACTION1_L2_POLICER[4096];
	rand uvm_reg_field L2_POLICER[4096];
	rand uvm_reg_field MAP_DOMAIN_ACTION1_L3_POLICER[4096];
	rand uvm_reg_field L3_POLICER[4096];
	rand uvm_reg_field MAP_DOMAIN_ACTION1_VLAN_COUNTER[4096];
	rand uvm_reg_field VLAN_COUNTER[4096];
	uvm_reg_field MAP_DOMAIN_PROFILE_RSVD0[256];
	rand uvm_reg_field MAP_DOMAIN_PROFILE_PRIORITY_PROFILE[256];
	uvm_reg_field MAP_PORT_RSVD0[18];
	rand uvm_reg_field MAP_PORT_MAP_PORT[18];
	uvm_reg_field MAP_MAC_RSVD0[96];
	rand uvm_reg_field MAP_MAC_MAC_ROUTABLE[96];
	rand uvm_reg_field MAP_MAC_MAP_MAC[96];
	rand uvm_reg_field MAP_MAC_VALID[96];
	rand uvm_reg_field MAP_MAC_IGNORE_LENGTH[96];
	rand uvm_reg_field IGNORE_LENGTH[96];
	rand uvm_reg_field MAP_MAC_MAC[96];
	rand uvm_reg_field MAC[96];
	rand uvm_reg_field MAP_IP_LO_IP_LO[16];
	rand uvm_reg_field IP_LO[16];
	rand uvm_reg_field MAP_IP_HI_IP_HI[16];
	rand uvm_reg_field IP_HI[16];
	uvm_reg_field MAP_IP_CFG_RSVD0[16];
	rand uvm_reg_field MAP_IP_CFG_MATCH_LENGTH[16];
	rand uvm_reg_field MATCH_LENGTH[16];
	rand uvm_reg_field MAP_IP_CFG_VALID[16];
	rand uvm_reg_field MAP_IP_CFG_MAP_IP[16];
	rand uvm_reg_field MAP_IP[16];
	rand uvm_reg_field MAP_IP_CFG_IP_PROFILE[16];
	rand uvm_reg_field IP_PROFILE[16];
	rand uvm_reg_field MAP_IP_CFG_IS_IPV6[16];
	rand uvm_reg_field IS_IPV6[16];
	uvm_reg_field MAP_PROT_RSVD0[8];
	rand uvm_reg_field MAP_PROT_MAP_PROT[8];
	rand uvm_reg_field MAP_PROT_PROT[8];
	rand uvm_reg_field PROT[8];
	uvm_reg_field MAP_L4_SRC_RSVD0[64];
	rand uvm_reg_field MAP_L4_SRC_MAP_L4_SRC[64];
	rand uvm_reg_field MAP_L4_SRC_VALID[64];
	rand uvm_reg_field MAP_L4_SRC_MAP_PROT[64];
	rand uvm_reg_field MAP_L4_SRC_L4_SRC[64];
	rand uvm_reg_field L4_SRC[64];
	uvm_reg_field MAP_L4_DST_RSVD0[64];
	rand uvm_reg_field MAP_L4_DST_MAP_L4_DST[64];
	rand uvm_reg_field MAP_L4_DST_VALID[64];
	rand uvm_reg_field MAP_L4_DST_MAP_PROT[64];
	rand uvm_reg_field MAP_L4_DST_L4_DST[64];
	rand uvm_reg_field L4_DST[64];
	uvm_reg_field MAP_EXP_TC_RSVD0[32];
	rand uvm_reg_field MAP_EXP_TC_TC_BY_EXP[32];
	rand uvm_reg_field TC_BY_EXP[32];
	uvm_reg_field MAP_DSCP_TC_RSVD0[2048];
	rand uvm_reg_field MAP_DSCP_TC_TC[2048];
	rand uvm_reg_field TC[2048];
	rand uvm_reg_field MAP_DSCP_TC_DSCP[2048];
	rand uvm_reg_field DSCP[2048];
	uvm_reg_field MAP_VPRI_TC_RSVD0[32];
	rand uvm_reg_field MAP_VPRI_TC_TC_BY_VPRI[32];
	rand uvm_reg_field TC_BY_VPRI[32];
	rand uvm_reg_field MAP_VPRI_VPRI_BY_VPRI[32];
	rand uvm_reg_field VPRI_BY_VPRI[32];
	uvm_reg_field MAP_PROFILE_KEY0__RSVD1_[96];
	rand uvm_reg_field MAP_PROFILE_KEY0_PTRS_ERR[96];
	rand uvm_reg_field MAP_PROFILE_KEY0_EX[96];
	rand uvm_reg_field MAP_PROFILE_KEY0_CSUM[96];
	uvm_reg_field MAP_PROFILE_KEY0__RSVD0_[96];
	rand uvm_reg_field MAP_PROFILE_KEY0_IHL_OK[96];
	rand uvm_reg_field MAP_PROFILE_KEY0_IHL_FITS[96];
	rand uvm_reg_field MAP_PROFILE_KEY0_FLAGS[96];
	rand uvm_reg_field MAP_PROFILE_KEY0_RSVD[96];
	uvm_reg_field MAP_PROFILE_KEY_INVERT0__RSVD1_[96];
	rand uvm_reg_field MAP_PROFILE_KEY_INVERT0_PTRS_ERR[96];
	rand uvm_reg_field MAP_PROFILE_KEY_INVERT0_EX[96];
	rand uvm_reg_field MAP_PROFILE_KEY_INVERT0_CSUM[96];
	uvm_reg_field MAP_PROFILE_KEY_INVERT0__RSVD0_[96];
	rand uvm_reg_field MAP_PROFILE_KEY_INVERT0_IHL_OK[96];
	rand uvm_reg_field MAP_PROFILE_KEY_INVERT0_IHL_FITS[96];
	rand uvm_reg_field MAP_PROFILE_KEY_INVERT0_FLAGS[96];
	rand uvm_reg_field MAP_PROFILE_KEY_INVERT0_RSVD[96];
	uvm_reg_field MAP_PROFILE_KEY1__RSVD1_[96];
	rand uvm_reg_field MAP_PROFILE_KEY1_PTYPE[96];
	rand uvm_reg_field MAP_PROFILE_KEY1_L2_DOMAIN[96];
	rand uvm_reg_field MAP_PROFILE_KEY1_L3_DOMAIN[96];
	uvm_reg_field MAP_PROFILE_KEY1__RSVD0_[96];
	rand uvm_reg_field MAP_PROFILE_KEY1_PORT_PROFILE[96];
	rand uvm_reg_field MAP_PROFILE_KEY1_DOMAIN_PROFILE[96];
	rand uvm_reg_field MAP_PROFILE_KEY1_MAC_ROUTABLE[96];
	rand uvm_reg_field MAP_PROFILE_KEY1_MAC_MBCAST[96];
	uvm_reg_field MAP_PROFILE_KEY_INVERT1__RSVD1_[96];
	rand uvm_reg_field MAP_PROFILE_KEY_INVERT1_PTYPE[96];
	rand uvm_reg_field MAP_PROFILE_KEY_INVERT1_L2_DOMAIN[96];
	rand uvm_reg_field MAP_PROFILE_KEY_INVERT1_L3_DOMAIN[96];
	uvm_reg_field MAP_PROFILE_KEY_INVERT1__RSVD0_[96];
	rand uvm_reg_field MAP_PROFILE_KEY_INVERT1_PORT_PROFILE[96];
	rand uvm_reg_field MAP_PROFILE_KEY_INVERT1_DOMAIN_PROFILE[96];
	rand uvm_reg_field MAP_PROFILE_KEY_INVERT1_MAC_ROUTABLE[96];
	rand uvm_reg_field MAP_PROFILE_KEY_INVERT1_MAC_MBCAST[96];
	uvm_reg_field MAP_PROFILE_ACTION_RSVD0[96];
	rand uvm_reg_field MAP_PROFILE_ACTION_PROFILE_VALID[96];
	rand uvm_reg_field PROFILE_VALID[96];
	rand uvm_reg_field MAP_PROFILE_ACTION_PROFILE[96];
	rand uvm_reg_field PROFILE[96];
	rand uvm_reg_field MAP_PROFILE_ACTION_REWRITE_PROFILE[96];
	rand uvm_reg_field REWRITE_PROFILE[96];
	rand uvm_reg_field MAP_PROFILE_ACTION_TRIG_VALID[96];
	rand uvm_reg_field TRIG_VALID[96];
	rand uvm_reg_field MAP_PROFILE_ACTION_PROFILE_TRIG[96];
	rand uvm_reg_field PROFILE_TRIG[96];
	rand uvm_reg_field MAP_PROFILE_ACTION_PARSER_ERROR[96];
	rand uvm_reg_field PARSER_ERROR[96];
	rand uvm_reg_field MAP_PROFILE_ACTION_IP_OPTIONS_MASK[96];
	rand uvm_reg_field IP_OPTIONS_MASK[96];
	rand uvm_reg_field MAP_PROFILE_ACTION_PRIOS_VALID[96];
	rand uvm_reg_field PRIOS_VALID[96];
	rand uvm_reg_field MAP_PROFILE_ACTION_VPRI_TGT[96];
	rand uvm_reg_field VPRI_TGT[96];
	rand uvm_reg_field MAP_PROFILE_ACTION_DSCP_TGT[96];
	rand uvm_reg_field DSCP_TGT[96];
	uvm_reg_field MAP_DOMAIN_POL_CFG_RSVD0;
	rand uvm_reg_field MAP_DOMAIN_POL_CFG_L3_COLOR_CFG;
	rand uvm_reg_field L3_COLOR_CFG;
	rand uvm_reg_field MAP_DOMAIN_POL_CFG_L2_COLOR_CFG;
	rand uvm_reg_field L2_COLOR_CFG;
	uvm_reg_field MAP_REWRITE_RSVD0[16][0:31];
	rand uvm_reg_field MAP_REWRITE_SRC_ID[16][0:31];
	rand uvm_reg_field SRC_ID[16][0:31];

	function new(string name = "mapper");
		super.new(name, build_coverage(UVM_NO_COVERAGE));
	endfunction: new

   virtual function void build();
      this.default_map = create_map("", 0, 8, UVM_LITTLE_ENDIAN, 0);
      foreach (this.MAP_PORT_CFG[i]) begin
         int J = i;
         this.MAP_PORT_CFG[J] = ral_reg_mapper_MAP_PORT_CFG::type_id::create($psprintf("MAP_PORT_CFG[%0d]",J),,get_full_name());
         this.MAP_PORT_CFG[J].configure(this, null, "");
         this.MAP_PORT_CFG[J].build();
         this.MAP_PORT_CFG[J].add_hdl_path('{

            '{$psprintf("MAP_PORT_CFG[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.MAP_PORT_CFG[J], `UVM_REG_ADDR_WIDTH'h0+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.MAP_PORT_CFG_RSVD0[J] = this.MAP_PORT_CFG[J].RSVD0;
			this.MAP_PORT_CFG_DEFAULT_SGLORT[J] = this.MAP_PORT_CFG[J].DEFAULT_SGLORT;
			this.DEFAULT_SGLORT[J] = this.MAP_PORT_CFG[J].DEFAULT_SGLORT;
			this.MAP_PORT_CFG_DEFAULT_SGLORT_EN[J] = this.MAP_PORT_CFG[J].DEFAULT_SGLORT_EN;
			this.DEFAULT_SGLORT_EN[J] = this.MAP_PORT_CFG[J].DEFAULT_SGLORT_EN;
			this.MAP_PORT_CFG_PORT_PROFILE[J] = this.MAP_PORT_CFG[J].PORT_PROFILE;
      end
      foreach (this.MAP_PORT_DEFAULT[i,j]) begin
         int J = i;
         int K = j;
         this.MAP_PORT_DEFAULT[J][K] = ral_reg_mapper_MAP_PORT_DEFAULT::type_id::create($psprintf("MAP_PORT_DEFAULT[%0d][%0d]",J,K),,get_full_name());
         this.MAP_PORT_DEFAULT[J][K].configure(this, null, "");
         this.MAP_PORT_DEFAULT[J][K].build();
         this.MAP_PORT_DEFAULT[J][K].add_hdl_path('{

            '{$psprintf("MAP_PORT_DEFAULT[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.MAP_PORT_DEFAULT[J][K], `UVM_REG_ADDR_WIDTH'h200+J*`UVM_REG_ADDR_WIDTH'h40+K*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.MAP_PORT_DEFAULT_RSVD0[J][K] = this.MAP_PORT_DEFAULT[J][K].RSVD0;
			this.MAP_PORT_DEFAULT_VALUE[J][K] = this.MAP_PORT_DEFAULT[J][K].VALUE;
			this.VALUE[J][K] = this.MAP_PORT_DEFAULT[J][K].VALUE;
			this.MAP_PORT_DEFAULT_RSVD1[J][K] = this.MAP_PORT_DEFAULT[J][K].RSVD1;
			this.RSVD1[J][K] = this.MAP_PORT_DEFAULT[J][K].RSVD1;
			this.MAP_PORT_DEFAULT_TARGET[J][K] = this.MAP_PORT_DEFAULT[J][K].TARGET;
			this.TARGET[J][K] = this.MAP_PORT_DEFAULT[J][K].TARGET;
      end
      foreach (this.MAP_DOMAIN_TCAM[i]) begin
         int J = i;
         this.MAP_DOMAIN_TCAM[J] = ral_reg_mapper_MAP_DOMAIN_TCAM::type_id::create($psprintf("MAP_DOMAIN_TCAM[%0d]",J),,get_full_name());
         this.MAP_DOMAIN_TCAM[J].configure(this, null, "");
         this.MAP_DOMAIN_TCAM[J].build();
         this.MAP_DOMAIN_TCAM[J].add_hdl_path('{

            '{$psprintf("MAP_DOMAIN_TCAM[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.MAP_DOMAIN_TCAM[J], `UVM_REG_ADDR_WIDTH'h10000+J*`UVM_REG_ADDR_WIDTH'h10, "RW", 0);
			this.MAP_DOMAIN_TCAM__RSVD1_[J] = this.MAP_DOMAIN_TCAM[J]._RSVD1_;
			this.MAP_DOMAIN_TCAM_PORT_KEY_INVERT[J] = this.MAP_DOMAIN_TCAM[J].PORT_KEY_INVERT;
			this.PORT_KEY_INVERT[J] = this.MAP_DOMAIN_TCAM[J].PORT_KEY_INVERT;
			this.MAP_DOMAIN_TCAM_VID2_VALID_INVERT[J] = this.MAP_DOMAIN_TCAM[J].VID2_VALID_INVERT;
			this.VID2_VALID_INVERT[J] = this.MAP_DOMAIN_TCAM[J].VID2_VALID_INVERT;
			this.MAP_DOMAIN_TCAM_VID2_KEY_INVERT[J] = this.MAP_DOMAIN_TCAM[J].VID2_KEY_INVERT;
			this.VID2_KEY_INVERT[J] = this.MAP_DOMAIN_TCAM[J].VID2_KEY_INVERT;
			this.MAP_DOMAIN_TCAM_VID1_VALID_INVERT[J] = this.MAP_DOMAIN_TCAM[J].VID1_VALID_INVERT;
			this.VID1_VALID_INVERT[J] = this.MAP_DOMAIN_TCAM[J].VID1_VALID_INVERT;
			this.MAP_DOMAIN_TCAM_VID1_KEY_INVERT[J] = this.MAP_DOMAIN_TCAM[J].VID1_KEY_INVERT;
			this.VID1_KEY_INVERT[J] = this.MAP_DOMAIN_TCAM[J].VID1_KEY_INVERT;
			this.MAP_DOMAIN_TCAM__RSVD0_[J] = this.MAP_DOMAIN_TCAM[J]._RSVD0_;
			this.MAP_DOMAIN_TCAM_PORT_KEY[J] = this.MAP_DOMAIN_TCAM[J].PORT_KEY;
			this.PORT_KEY[J] = this.MAP_DOMAIN_TCAM[J].PORT_KEY;
			this.MAP_DOMAIN_TCAM_VID2_VALID[J] = this.MAP_DOMAIN_TCAM[J].VID2_VALID;
			this.VID2_VALID[J] = this.MAP_DOMAIN_TCAM[J].VID2_VALID;
			this.MAP_DOMAIN_TCAM_VID2_KEY[J] = this.MAP_DOMAIN_TCAM[J].VID2_KEY;
			this.VID2_KEY[J] = this.MAP_DOMAIN_TCAM[J].VID2_KEY;
			this.MAP_DOMAIN_TCAM_VID1_VALID[J] = this.MAP_DOMAIN_TCAM[J].VID1_VALID;
			this.VID1_VALID[J] = this.MAP_DOMAIN_TCAM[J].VID1_VALID;
			this.MAP_DOMAIN_TCAM_VID1_KEY[J] = this.MAP_DOMAIN_TCAM[J].VID1_KEY;
			this.VID1_KEY[J] = this.MAP_DOMAIN_TCAM[J].VID1_KEY;
      end
      foreach (this.MAP_DOMAIN_ACTION0[i]) begin
         int J = i;
         this.MAP_DOMAIN_ACTION0[J] = ral_reg_mapper_MAP_DOMAIN_ACTION0::type_id::create($psprintf("MAP_DOMAIN_ACTION0[%0d]",J),,get_full_name());
         this.MAP_DOMAIN_ACTION0[J].configure(this, null, "");
         this.MAP_DOMAIN_ACTION0[J].build();
         this.MAP_DOMAIN_ACTION0[J].add_hdl_path('{

            '{$psprintf("MAP_DOMAIN_ACTION0[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.MAP_DOMAIN_ACTION0[J], `UVM_REG_ADDR_WIDTH'h20000+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.MAP_DOMAIN_ACTION0_RSVD0[J] = this.MAP_DOMAIN_ACTION0[J].RSVD0;
			this.MAP_DOMAIN_ACTION0_L2_DOMAIN[J] = this.MAP_DOMAIN_ACTION0[J].L2_DOMAIN;
			this.MAP_DOMAIN_ACTION0_L3_DOMAIN[J] = this.MAP_DOMAIN_ACTION0[J].L3_DOMAIN;
			this.MAP_DOMAIN_ACTION0_NAD[J] = this.MAP_DOMAIN_ACTION0[J].NAD;
			this.NAD[J] = this.MAP_DOMAIN_ACTION0[J].NAD;
			this.MAP_DOMAIN_ACTION0_UPDATE_DOMAINS[J] = this.MAP_DOMAIN_ACTION0[J].UPDATE_DOMAINS;
			this.UPDATE_DOMAINS[J] = this.MAP_DOMAIN_ACTION0[J].UPDATE_DOMAINS;
			this.MAP_DOMAIN_ACTION0_LEARN_EN[J] = this.MAP_DOMAIN_ACTION0[J].LEARN_EN;
			this.LEARN_EN[J] = this.MAP_DOMAIN_ACTION0[J].LEARN_EN;
			this.MAP_DOMAIN_ACTION0_LEARN_MODE[J] = this.MAP_DOMAIN_ACTION0[J].LEARN_MODE;
			this.LEARN_MODE[J] = this.MAP_DOMAIN_ACTION0[J].LEARN_MODE;
			this.MAP_DOMAIN_ACTION0_PRIORITY_PROFILE[J] = this.MAP_DOMAIN_ACTION0[J].PRIORITY_PROFILE;
			this.MAP_DOMAIN_ACTION0_PRI_SOURCE[J] = this.MAP_DOMAIN_ACTION0[J].PRI_SOURCE;
			this.PRI_SOURCE[J] = this.MAP_DOMAIN_ACTION0[J].PRI_SOURCE;
			this.MAP_DOMAIN_ACTION0_FORCE_DEFAULT_PRI[J] = this.MAP_DOMAIN_ACTION0[J].FORCE_DEFAULT_PRI;
			this.FORCE_DEFAULT_PRI[J] = this.MAP_DOMAIN_ACTION0[J].FORCE_DEFAULT_PRI;
			this.MAP_DOMAIN_ACTION0_DEFAULT_PRI[J] = this.MAP_DOMAIN_ACTION0[J].DEFAULT_PRI;
			this.DEFAULT_PRI[J] = this.MAP_DOMAIN_ACTION0[J].DEFAULT_PRI;
      end
      foreach (this.MAP_DOMAIN_ACTION1[i]) begin
         int J = i;
         this.MAP_DOMAIN_ACTION1[J] = ral_reg_mapper_MAP_DOMAIN_ACTION1::type_id::create($psprintf("MAP_DOMAIN_ACTION1[%0d]",J),,get_full_name());
         this.MAP_DOMAIN_ACTION1[J].configure(this, null, "");
         this.MAP_DOMAIN_ACTION1[J].build();
         this.MAP_DOMAIN_ACTION1[J].add_hdl_path('{

            '{$psprintf("MAP_DOMAIN_ACTION1[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.MAP_DOMAIN_ACTION1[J], `UVM_REG_ADDR_WIDTH'h28000+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.MAP_DOMAIN_ACTION1_RSVD0[J] = this.MAP_DOMAIN_ACTION1[J].RSVD0;
			this.MAP_DOMAIN_ACTION1_DOMAIN_PROFILE[J] = this.MAP_DOMAIN_ACTION1[J].DOMAIN_PROFILE;
			this.MAP_DOMAIN_ACTION1_L2_POLICER[J] = this.MAP_DOMAIN_ACTION1[J].L2_POLICER;
			this.L2_POLICER[J] = this.MAP_DOMAIN_ACTION1[J].L2_POLICER;
			this.MAP_DOMAIN_ACTION1_L3_POLICER[J] = this.MAP_DOMAIN_ACTION1[J].L3_POLICER;
			this.L3_POLICER[J] = this.MAP_DOMAIN_ACTION1[J].L3_POLICER;
			this.MAP_DOMAIN_ACTION1_VLAN_COUNTER[J] = this.MAP_DOMAIN_ACTION1[J].VLAN_COUNTER;
			this.VLAN_COUNTER[J] = this.MAP_DOMAIN_ACTION1[J].VLAN_COUNTER;
      end
      foreach (this.MAP_DOMAIN_PROFILE[i]) begin
         int J = i;
         this.MAP_DOMAIN_PROFILE[J] = ral_reg_mapper_MAP_DOMAIN_PROFILE::type_id::create($psprintf("MAP_DOMAIN_PROFILE[%0d]",J),,get_full_name());
         this.MAP_DOMAIN_PROFILE[J].configure(this, null, "");
         this.MAP_DOMAIN_PROFILE[J].build();
         this.MAP_DOMAIN_PROFILE[J].add_hdl_path('{

            '{$psprintf("MAP_DOMAIN_PROFILE[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.MAP_DOMAIN_PROFILE[J], `UVM_REG_ADDR_WIDTH'h30000+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.MAP_DOMAIN_PROFILE_RSVD0[J] = this.MAP_DOMAIN_PROFILE[J].RSVD0;
			this.MAP_DOMAIN_PROFILE_PRIORITY_PROFILE[J] = this.MAP_DOMAIN_PROFILE[J].PRIORITY_PROFILE;
      end
      foreach (this.MAP_PORT[i]) begin
         int J = i;
         this.MAP_PORT[J] = ral_reg_mapper_MAP_PORT::type_id::create($psprintf("MAP_PORT[%0d]",J),,get_full_name());
         this.MAP_PORT[J].configure(this, null, "");
         this.MAP_PORT[J].build();
         this.MAP_PORT[J].add_hdl_path('{

            '{$psprintf("MAP_PORT[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.MAP_PORT[J], `UVM_REG_ADDR_WIDTH'h31000+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.MAP_PORT_RSVD0[J] = this.MAP_PORT[J].RSVD0;
			this.MAP_PORT_MAP_PORT[J] = this.MAP_PORT[J].MAP_PORT;
      end
      foreach (this.MAP_MAC[i]) begin
         int J = i;
         this.MAP_MAC[J] = ral_reg_mapper_MAP_MAC::type_id::create($psprintf("MAP_MAC[%0d]",J),,get_full_name());
         this.MAP_MAC[J].configure(this, null, "");
         this.MAP_MAC[J].build();
         this.MAP_MAC[J].add_hdl_path('{

            '{$psprintf("MAP_MAC[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.MAP_MAC[J], `UVM_REG_ADDR_WIDTH'h31800+J*`UVM_REG_ADDR_WIDTH'h10, "RW", 0);
			this.MAP_MAC_RSVD0[J] = this.MAP_MAC[J].RSVD0;
			this.MAP_MAC_MAC_ROUTABLE[J] = this.MAP_MAC[J].MAC_ROUTABLE;
			this.MAP_MAC_MAP_MAC[J] = this.MAP_MAC[J].MAP_MAC;
			this.MAP_MAC_VALID[J] = this.MAP_MAC[J].VALID;
			this.MAP_MAC_IGNORE_LENGTH[J] = this.MAP_MAC[J].IGNORE_LENGTH;
			this.IGNORE_LENGTH[J] = this.MAP_MAC[J].IGNORE_LENGTH;
			this.MAP_MAC_MAC[J] = this.MAP_MAC[J].MAC;
			this.MAC[J] = this.MAP_MAC[J].MAC;
      end
      foreach (this.MAP_IP_LO[i]) begin
         int J = i;
         this.MAP_IP_LO[J] = ral_reg_mapper_MAP_IP_LO::type_id::create($psprintf("MAP_IP_LO[%0d]",J),,get_full_name());
         this.MAP_IP_LO[J].configure(this, null, "");
         this.MAP_IP_LO[J].build();
         this.MAP_IP_LO[J].add_hdl_path('{

            '{$psprintf("MAP_IP_LO[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.MAP_IP_LO[J], `UVM_REG_ADDR_WIDTH'h32080+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.MAP_IP_LO_IP_LO[J] = this.MAP_IP_LO[J].IP_LO;
			this.IP_LO[J] = this.MAP_IP_LO[J].IP_LO;
      end
      foreach (this.MAP_IP_HI[i]) begin
         int J = i;
         this.MAP_IP_HI[J] = ral_reg_mapper_MAP_IP_HI::type_id::create($psprintf("MAP_IP_HI[%0d]",J),,get_full_name());
         this.MAP_IP_HI[J].configure(this, null, "");
         this.MAP_IP_HI[J].build();
         this.MAP_IP_HI[J].add_hdl_path('{

            '{$psprintf("MAP_IP_HI[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.MAP_IP_HI[J], `UVM_REG_ADDR_WIDTH'h32100+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.MAP_IP_HI_IP_HI[J] = this.MAP_IP_HI[J].IP_HI;
			this.IP_HI[J] = this.MAP_IP_HI[J].IP_HI;
      end
      foreach (this.MAP_IP_CFG[i]) begin
         int J = i;
         this.MAP_IP_CFG[J] = ral_reg_mapper_MAP_IP_CFG::type_id::create($psprintf("MAP_IP_CFG[%0d]",J),,get_full_name());
         this.MAP_IP_CFG[J].configure(this, null, "");
         this.MAP_IP_CFG[J].build();
         this.MAP_IP_CFG[J].add_hdl_path('{

            '{$psprintf("MAP_IP_CFG[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.MAP_IP_CFG[J], `UVM_REG_ADDR_WIDTH'h32180+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.MAP_IP_CFG_RSVD0[J] = this.MAP_IP_CFG[J].RSVD0;
			this.MAP_IP_CFG_MATCH_LENGTH[J] = this.MAP_IP_CFG[J].MATCH_LENGTH;
			this.MATCH_LENGTH[J] = this.MAP_IP_CFG[J].MATCH_LENGTH;
			this.MAP_IP_CFG_VALID[J] = this.MAP_IP_CFG[J].VALID;
			this.MAP_IP_CFG_MAP_IP[J] = this.MAP_IP_CFG[J].MAP_IP;
			this.MAP_IP[J] = this.MAP_IP_CFG[J].MAP_IP;
			this.MAP_IP_CFG_IP_PROFILE[J] = this.MAP_IP_CFG[J].IP_PROFILE;
			this.IP_PROFILE[J] = this.MAP_IP_CFG[J].IP_PROFILE;
			this.MAP_IP_CFG_IS_IPV6[J] = this.MAP_IP_CFG[J].IS_IPV6;
			this.IS_IPV6[J] = this.MAP_IP_CFG[J].IS_IPV6;
      end
      foreach (this.MAP_PROT[i]) begin
         int J = i;
         this.MAP_PROT[J] = ral_reg_mapper_MAP_PROT::type_id::create($psprintf("MAP_PROT[%0d]",J),,get_full_name());
         this.MAP_PROT[J].configure(this, null, "");
         this.MAP_PROT[J].build();
         this.MAP_PROT[J].add_hdl_path('{

            '{$psprintf("MAP_PROT[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.MAP_PROT[J], `UVM_REG_ADDR_WIDTH'h32200+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.MAP_PROT_RSVD0[J] = this.MAP_PROT[J].RSVD0;
			this.MAP_PROT_MAP_PROT[J] = this.MAP_PROT[J].MAP_PROT;
			this.MAP_PROT_PROT[J] = this.MAP_PROT[J].PROT;
			this.PROT[J] = this.MAP_PROT[J].PROT;
      end
      foreach (this.MAP_L4_SRC[i]) begin
         int J = i;
         this.MAP_L4_SRC[J] = ral_reg_mapper_MAP_L4_SRC::type_id::create($psprintf("MAP_L4_SRC[%0d]",J),,get_full_name());
         this.MAP_L4_SRC[J].configure(this, null, "");
         this.MAP_L4_SRC[J].build();
         this.MAP_L4_SRC[J].add_hdl_path('{

            '{$psprintf("MAP_L4_SRC[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.MAP_L4_SRC[J], `UVM_REG_ADDR_WIDTH'h32400+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.MAP_L4_SRC_RSVD0[J] = this.MAP_L4_SRC[J].RSVD0;
			this.MAP_L4_SRC_MAP_L4_SRC[J] = this.MAP_L4_SRC[J].MAP_L4_SRC;
			this.MAP_L4_SRC_VALID[J] = this.MAP_L4_SRC[J].VALID;
			this.MAP_L4_SRC_MAP_PROT[J] = this.MAP_L4_SRC[J].MAP_PROT;
			this.MAP_L4_SRC_L4_SRC[J] = this.MAP_L4_SRC[J].L4_SRC;
			this.L4_SRC[J] = this.MAP_L4_SRC[J].L4_SRC;
      end
      foreach (this.MAP_L4_DST[i]) begin
         int J = i;
         this.MAP_L4_DST[J] = ral_reg_mapper_MAP_L4_DST::type_id::create($psprintf("MAP_L4_DST[%0d]",J),,get_full_name());
         this.MAP_L4_DST[J].configure(this, null, "");
         this.MAP_L4_DST[J].build();
         this.MAP_L4_DST[J].add_hdl_path('{

            '{$psprintf("MAP_L4_DST[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.MAP_L4_DST[J], `UVM_REG_ADDR_WIDTH'h32600+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.MAP_L4_DST_RSVD0[J] = this.MAP_L4_DST[J].RSVD0;
			this.MAP_L4_DST_MAP_L4_DST[J] = this.MAP_L4_DST[J].MAP_L4_DST;
			this.MAP_L4_DST_VALID[J] = this.MAP_L4_DST[J].VALID;
			this.MAP_L4_DST_MAP_PROT[J] = this.MAP_L4_DST[J].MAP_PROT;
			this.MAP_L4_DST_L4_DST[J] = this.MAP_L4_DST[J].L4_DST;
			this.L4_DST[J] = this.MAP_L4_DST[J].L4_DST;
      end
      foreach (this.MAP_EXP_TC[i]) begin
         int J = i;
         this.MAP_EXP_TC[J] = ral_reg_mapper_MAP_EXP_TC::type_id::create($psprintf("MAP_EXP_TC[%0d]",J),,get_full_name());
         this.MAP_EXP_TC[J].configure(this, null, "");
         this.MAP_EXP_TC[J].build();
         this.MAP_EXP_TC[J].add_hdl_path('{

            '{$psprintf("MAP_EXP_TC[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.MAP_EXP_TC[J], `UVM_REG_ADDR_WIDTH'h32800+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.MAP_EXP_TC_RSVD0[J] = this.MAP_EXP_TC[J].RSVD0;
			this.MAP_EXP_TC_TC_BY_EXP[J] = this.MAP_EXP_TC[J].TC_BY_EXP;
			this.TC_BY_EXP[J] = this.MAP_EXP_TC[J].TC_BY_EXP;
      end
      foreach (this.MAP_DSCP_TC[i]) begin
         int J = i;
         this.MAP_DSCP_TC[J] = ral_reg_mapper_MAP_DSCP_TC::type_id::create($psprintf("MAP_DSCP_TC[%0d]",J),,get_full_name());
         this.MAP_DSCP_TC[J].configure(this, null, "");
         this.MAP_DSCP_TC[J].build();
         this.MAP_DSCP_TC[J].add_hdl_path('{

            '{$psprintf("MAP_DSCP_TC[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.MAP_DSCP_TC[J], `UVM_REG_ADDR_WIDTH'h34000+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.MAP_DSCP_TC_RSVD0[J] = this.MAP_DSCP_TC[J].RSVD0;
			this.MAP_DSCP_TC_TC[J] = this.MAP_DSCP_TC[J].TC;
			this.TC[J] = this.MAP_DSCP_TC[J].TC;
			this.MAP_DSCP_TC_DSCP[J] = this.MAP_DSCP_TC[J].DSCP;
			this.DSCP[J] = this.MAP_DSCP_TC[J].DSCP;
      end
      foreach (this.MAP_VPRI_TC[i]) begin
         int J = i;
         this.MAP_VPRI_TC[J] = ral_reg_mapper_MAP_VPRI_TC::type_id::create($psprintf("MAP_VPRI_TC[%0d]",J),,get_full_name());
         this.MAP_VPRI_TC[J].configure(this, null, "");
         this.MAP_VPRI_TC[J].build();
         this.MAP_VPRI_TC[J].add_hdl_path('{

            '{$psprintf("MAP_VPRI_TC[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.MAP_VPRI_TC[J], `UVM_REG_ADDR_WIDTH'h38000+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.MAP_VPRI_TC_RSVD0[J] = this.MAP_VPRI_TC[J].RSVD0;
			this.MAP_VPRI_TC_TC_BY_VPRI[J] = this.MAP_VPRI_TC[J].TC_BY_VPRI;
			this.TC_BY_VPRI[J] = this.MAP_VPRI_TC[J].TC_BY_VPRI;
      end
      foreach (this.MAP_VPRI[i]) begin
         int J = i;
         this.MAP_VPRI[J] = ral_reg_mapper_MAP_VPRI::type_id::create($psprintf("MAP_VPRI[%0d]",J),,get_full_name());
         this.MAP_VPRI[J].configure(this, null, "");
         this.MAP_VPRI[J].build();
         this.MAP_VPRI[J].add_hdl_path('{

            '{$psprintf("MAP_VPRI[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.MAP_VPRI[J], `UVM_REG_ADDR_WIDTH'h38100+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.MAP_VPRI_VPRI_BY_VPRI[J] = this.MAP_VPRI[J].VPRI_BY_VPRI;
			this.VPRI_BY_VPRI[J] = this.MAP_VPRI[J].VPRI_BY_VPRI;
      end
      foreach (this.MAP_PROFILE_KEY0[i]) begin
         int J = i;
         this.MAP_PROFILE_KEY0[J] = ral_reg_mapper_MAP_PROFILE_KEY0::type_id::create($psprintf("MAP_PROFILE_KEY0[%0d]",J),,get_full_name());
         this.MAP_PROFILE_KEY0[J].configure(this, null, "");
         this.MAP_PROFILE_KEY0[J].build();
         this.MAP_PROFILE_KEY0[J].add_hdl_path('{

            '{$psprintf("MAP_PROFILE_KEY0[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.MAP_PROFILE_KEY0[J], `UVM_REG_ADDR_WIDTH'h38400+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.MAP_PROFILE_KEY0__RSVD1_[J] = this.MAP_PROFILE_KEY0[J]._RSVD1_;
			this.MAP_PROFILE_KEY0_PTRS_ERR[J] = this.MAP_PROFILE_KEY0[J].PTRS_ERR;
			this.MAP_PROFILE_KEY0_EX[J] = this.MAP_PROFILE_KEY0[J].EX;
			this.MAP_PROFILE_KEY0_CSUM[J] = this.MAP_PROFILE_KEY0[J].CSUM;
			this.MAP_PROFILE_KEY0__RSVD0_[J] = this.MAP_PROFILE_KEY0[J]._RSVD0_;
			this.MAP_PROFILE_KEY0_IHL_OK[J] = this.MAP_PROFILE_KEY0[J].IHL_OK;
			this.MAP_PROFILE_KEY0_IHL_FITS[J] = this.MAP_PROFILE_KEY0[J].IHL_FITS;
			this.MAP_PROFILE_KEY0_FLAGS[J] = this.MAP_PROFILE_KEY0[J].FLAGS;
			this.MAP_PROFILE_KEY0_RSVD[J] = this.MAP_PROFILE_KEY0[J].RSVD;
      end
      foreach (this.MAP_PROFILE_KEY_INVERT0[i]) begin
         int J = i;
         this.MAP_PROFILE_KEY_INVERT0[J] = ral_reg_mapper_MAP_PROFILE_KEY_INVERT0::type_id::create($psprintf("MAP_PROFILE_KEY_INVERT0[%0d]",J),,get_full_name());
         this.MAP_PROFILE_KEY_INVERT0[J].configure(this, null, "");
         this.MAP_PROFILE_KEY_INVERT0[J].build();
         this.MAP_PROFILE_KEY_INVERT0[J].add_hdl_path('{

            '{$psprintf("MAP_PROFILE_KEY_INVERT0[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.MAP_PROFILE_KEY_INVERT0[J], `UVM_REG_ADDR_WIDTH'h38800+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.MAP_PROFILE_KEY_INVERT0__RSVD1_[J] = this.MAP_PROFILE_KEY_INVERT0[J]._RSVD1_;
			this.MAP_PROFILE_KEY_INVERT0_PTRS_ERR[J] = this.MAP_PROFILE_KEY_INVERT0[J].PTRS_ERR;
			this.MAP_PROFILE_KEY_INVERT0_EX[J] = this.MAP_PROFILE_KEY_INVERT0[J].EX;
			this.MAP_PROFILE_KEY_INVERT0_CSUM[J] = this.MAP_PROFILE_KEY_INVERT0[J].CSUM;
			this.MAP_PROFILE_KEY_INVERT0__RSVD0_[J] = this.MAP_PROFILE_KEY_INVERT0[J]._RSVD0_;
			this.MAP_PROFILE_KEY_INVERT0_IHL_OK[J] = this.MAP_PROFILE_KEY_INVERT0[J].IHL_OK;
			this.MAP_PROFILE_KEY_INVERT0_IHL_FITS[J] = this.MAP_PROFILE_KEY_INVERT0[J].IHL_FITS;
			this.MAP_PROFILE_KEY_INVERT0_FLAGS[J] = this.MAP_PROFILE_KEY_INVERT0[J].FLAGS;
			this.MAP_PROFILE_KEY_INVERT0_RSVD[J] = this.MAP_PROFILE_KEY_INVERT0[J].RSVD;
      end
      foreach (this.MAP_PROFILE_KEY1[i]) begin
         int J = i;
         this.MAP_PROFILE_KEY1[J] = ral_reg_mapper_MAP_PROFILE_KEY1::type_id::create($psprintf("MAP_PROFILE_KEY1[%0d]",J),,get_full_name());
         this.MAP_PROFILE_KEY1[J].configure(this, null, "");
         this.MAP_PROFILE_KEY1[J].build();
         this.MAP_PROFILE_KEY1[J].add_hdl_path('{

            '{$psprintf("MAP_PROFILE_KEY1[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.MAP_PROFILE_KEY1[J], `UVM_REG_ADDR_WIDTH'h38C00+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.MAP_PROFILE_KEY1__RSVD1_[J] = this.MAP_PROFILE_KEY1[J]._RSVD1_;
			this.MAP_PROFILE_KEY1_PTYPE[J] = this.MAP_PROFILE_KEY1[J].PTYPE;
			this.MAP_PROFILE_KEY1_L2_DOMAIN[J] = this.MAP_PROFILE_KEY1[J].L2_DOMAIN;
			this.MAP_PROFILE_KEY1_L3_DOMAIN[J] = this.MAP_PROFILE_KEY1[J].L3_DOMAIN;
			this.MAP_PROFILE_KEY1__RSVD0_[J] = this.MAP_PROFILE_KEY1[J]._RSVD0_;
			this.MAP_PROFILE_KEY1_PORT_PROFILE[J] = this.MAP_PROFILE_KEY1[J].PORT_PROFILE;
			this.MAP_PROFILE_KEY1_DOMAIN_PROFILE[J] = this.MAP_PROFILE_KEY1[J].DOMAIN_PROFILE;
			this.MAP_PROFILE_KEY1_MAC_ROUTABLE[J] = this.MAP_PROFILE_KEY1[J].MAC_ROUTABLE;
			this.MAP_PROFILE_KEY1_MAC_MBCAST[J] = this.MAP_PROFILE_KEY1[J].MAC_MBCAST;
      end
      foreach (this.MAP_PROFILE_KEY_INVERT1[i]) begin
         int J = i;
         this.MAP_PROFILE_KEY_INVERT1[J] = ral_reg_mapper_MAP_PROFILE_KEY_INVERT1::type_id::create($psprintf("MAP_PROFILE_KEY_INVERT1[%0d]",J),,get_full_name());
         this.MAP_PROFILE_KEY_INVERT1[J].configure(this, null, "");
         this.MAP_PROFILE_KEY_INVERT1[J].build();
         this.MAP_PROFILE_KEY_INVERT1[J].add_hdl_path('{

            '{$psprintf("MAP_PROFILE_KEY_INVERT1[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.MAP_PROFILE_KEY_INVERT1[J], `UVM_REG_ADDR_WIDTH'h39000+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.MAP_PROFILE_KEY_INVERT1__RSVD1_[J] = this.MAP_PROFILE_KEY_INVERT1[J]._RSVD1_;
			this.MAP_PROFILE_KEY_INVERT1_PTYPE[J] = this.MAP_PROFILE_KEY_INVERT1[J].PTYPE;
			this.MAP_PROFILE_KEY_INVERT1_L2_DOMAIN[J] = this.MAP_PROFILE_KEY_INVERT1[J].L2_DOMAIN;
			this.MAP_PROFILE_KEY_INVERT1_L3_DOMAIN[J] = this.MAP_PROFILE_KEY_INVERT1[J].L3_DOMAIN;
			this.MAP_PROFILE_KEY_INVERT1__RSVD0_[J] = this.MAP_PROFILE_KEY_INVERT1[J]._RSVD0_;
			this.MAP_PROFILE_KEY_INVERT1_PORT_PROFILE[J] = this.MAP_PROFILE_KEY_INVERT1[J].PORT_PROFILE;
			this.MAP_PROFILE_KEY_INVERT1_DOMAIN_PROFILE[J] = this.MAP_PROFILE_KEY_INVERT1[J].DOMAIN_PROFILE;
			this.MAP_PROFILE_KEY_INVERT1_MAC_ROUTABLE[J] = this.MAP_PROFILE_KEY_INVERT1[J].MAC_ROUTABLE;
			this.MAP_PROFILE_KEY_INVERT1_MAC_MBCAST[J] = this.MAP_PROFILE_KEY_INVERT1[J].MAC_MBCAST;
      end
      foreach (this.MAP_PROFILE_ACTION[i]) begin
         int J = i;
         this.MAP_PROFILE_ACTION[J] = ral_reg_mapper_MAP_PROFILE_ACTION::type_id::create($psprintf("MAP_PROFILE_ACTION[%0d]",J),,get_full_name());
         this.MAP_PROFILE_ACTION[J].configure(this, null, "");
         this.MAP_PROFILE_ACTION[J].build();
         this.MAP_PROFILE_ACTION[J].add_hdl_path('{

            '{$psprintf("MAP_PROFILE_ACTION[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.MAP_PROFILE_ACTION[J], `UVM_REG_ADDR_WIDTH'h39400+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.MAP_PROFILE_ACTION_RSVD0[J] = this.MAP_PROFILE_ACTION[J].RSVD0;
			this.MAP_PROFILE_ACTION_PROFILE_VALID[J] = this.MAP_PROFILE_ACTION[J].PROFILE_VALID;
			this.PROFILE_VALID[J] = this.MAP_PROFILE_ACTION[J].PROFILE_VALID;
			this.MAP_PROFILE_ACTION_PROFILE[J] = this.MAP_PROFILE_ACTION[J].PROFILE;
			this.PROFILE[J] = this.MAP_PROFILE_ACTION[J].PROFILE;
			this.MAP_PROFILE_ACTION_REWRITE_PROFILE[J] = this.MAP_PROFILE_ACTION[J].REWRITE_PROFILE;
			this.REWRITE_PROFILE[J] = this.MAP_PROFILE_ACTION[J].REWRITE_PROFILE;
			this.MAP_PROFILE_ACTION_TRIG_VALID[J] = this.MAP_PROFILE_ACTION[J].TRIG_VALID;
			this.TRIG_VALID[J] = this.MAP_PROFILE_ACTION[J].TRIG_VALID;
			this.MAP_PROFILE_ACTION_PROFILE_TRIG[J] = this.MAP_PROFILE_ACTION[J].PROFILE_TRIG;
			this.PROFILE_TRIG[J] = this.MAP_PROFILE_ACTION[J].PROFILE_TRIG;
			this.MAP_PROFILE_ACTION_PARSER_ERROR[J] = this.MAP_PROFILE_ACTION[J].PARSER_ERROR;
			this.PARSER_ERROR[J] = this.MAP_PROFILE_ACTION[J].PARSER_ERROR;
			this.MAP_PROFILE_ACTION_IP_OPTIONS_MASK[J] = this.MAP_PROFILE_ACTION[J].IP_OPTIONS_MASK;
			this.IP_OPTIONS_MASK[J] = this.MAP_PROFILE_ACTION[J].IP_OPTIONS_MASK;
			this.MAP_PROFILE_ACTION_PRIOS_VALID[J] = this.MAP_PROFILE_ACTION[J].PRIOS_VALID;
			this.PRIOS_VALID[J] = this.MAP_PROFILE_ACTION[J].PRIOS_VALID;
			this.MAP_PROFILE_ACTION_VPRI_TGT[J] = this.MAP_PROFILE_ACTION[J].VPRI_TGT;
			this.VPRI_TGT[J] = this.MAP_PROFILE_ACTION[J].VPRI_TGT;
			this.MAP_PROFILE_ACTION_DSCP_TGT[J] = this.MAP_PROFILE_ACTION[J].DSCP_TGT;
			this.DSCP_TGT[J] = this.MAP_PROFILE_ACTION[J].DSCP_TGT;
      end
      this.MAP_DOMAIN_POL_CFG = ral_reg_mapper_MAP_DOMAIN_POL_CFG::type_id::create("MAP_DOMAIN_POL_CFG",,get_full_name());
      this.MAP_DOMAIN_POL_CFG.configure(this, null, "");
      this.MAP_DOMAIN_POL_CFG.build();
         this.MAP_DOMAIN_POL_CFG.add_hdl_path('{

            '{"MAP_DOMAIN_POL_CFG", -1, -1}
         });
      this.default_map.add_reg(this.MAP_DOMAIN_POL_CFG, `UVM_REG_ADDR_WIDTH'h39800, "RW", 0);
		this.MAP_DOMAIN_POL_CFG_RSVD0 = this.MAP_DOMAIN_POL_CFG.RSVD0;
		this.MAP_DOMAIN_POL_CFG_L3_COLOR_CFG = this.MAP_DOMAIN_POL_CFG.L3_COLOR_CFG;
		this.L3_COLOR_CFG = this.MAP_DOMAIN_POL_CFG.L3_COLOR_CFG;
		this.MAP_DOMAIN_POL_CFG_L2_COLOR_CFG = this.MAP_DOMAIN_POL_CFG.L2_COLOR_CFG;
		this.L2_COLOR_CFG = this.MAP_DOMAIN_POL_CFG.L2_COLOR_CFG;
      foreach (this.MAP_REWRITE[i,j]) begin
         int J = i;
         int K = j;
         this.MAP_REWRITE[J][K] = ral_reg_mapper_MAP_REWRITE::type_id::create($psprintf("MAP_REWRITE[%0d][%0d]",J,K),,get_full_name());
         this.MAP_REWRITE[J][K].configure(this, null, "");
         this.MAP_REWRITE[J][K].build();
         this.MAP_REWRITE[J][K].add_hdl_path('{

            '{$psprintf("MAP_REWRITE[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.MAP_REWRITE[J][K], `UVM_REG_ADDR_WIDTH'h3A000+J*`UVM_REG_ADDR_WIDTH'h100+K*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.MAP_REWRITE_RSVD0[J][K] = this.MAP_REWRITE[J][K].RSVD0;
			this.MAP_REWRITE_SRC_ID[J][K] = this.MAP_REWRITE[J][K].SRC_ID;
			this.SRC_ID[J][K] = this.MAP_REWRITE[J][K].SRC_ID;
      end
   endfunction : build

	`uvm_object_utils(ral_block_mapper)

endclass : ral_block_mapper



`endif
