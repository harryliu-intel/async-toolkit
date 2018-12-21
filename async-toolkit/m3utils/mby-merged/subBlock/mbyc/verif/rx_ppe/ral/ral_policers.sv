`ifndef RAL_POLICERS
`define RAL_POLICERS

import uvm_pkg::*;

class ral_reg_policers_POL_DIRECT_MAP_CTRL extends uvm_reg;
	rand uvm_reg_field GO_COMPL;
	rand uvm_reg_field STATUS;
	rand uvm_reg_field OP_TYPE;
	uvm_reg_field _RSVD0_;
	rand uvm_reg_field REG_ID;
	rand uvm_reg_field REG_SUB_ID;
	rand uvm_reg_field REG_INDX;

	function new(string name = "policers_POL_DIRECT_MAP_CTRL");
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

	`uvm_object_utils(ral_reg_policers_POL_DIRECT_MAP_CTRL)

endclass : ral_reg_policers_POL_DIRECT_MAP_CTRL


class ral_reg_policers_POL_DIRECT_MAP_CTR0 extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field DATA_CNTB;

	function new(string name = "policers_POL_DIRECT_MAP_CTR0");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 20, 44, "RO", 0, 20'h0, 1, 0, 0);
      this.DATA_CNTB = uvm_reg_field::type_id::create("DATA_CNTB",,get_full_name());
      this.DATA_CNTB.configure(this, 44, 0, "RW", 1, 44'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_policers_POL_DIRECT_MAP_CTR0)

endclass : ral_reg_policers_POL_DIRECT_MAP_CTR0


class ral_reg_policers_POL_DIRECT_MAP_CTR1 extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field DATA_CNTP;

	function new(string name = "policers_POL_DIRECT_MAP_CTR1");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 28, 36, "RO", 0, 28'h0, 1, 0, 0);
      this.DATA_CNTP = uvm_reg_field::type_id::create("DATA_CNTP",,get_full_name());
      this.DATA_CNTP.configure(this, 36, 0, "RW", 1, 36'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_policers_POL_DIRECT_MAP_CTR1)

endclass : ral_reg_policers_POL_DIRECT_MAP_CTR1


class ral_reg_policers_POL_DIRECT_MAP_POL0 extends uvm_reg;
	rand uvm_reg_field ETOK_HI;
	uvm_reg_field _RSVD_;
	rand uvm_reg_field ETOK_LO;
	uvm_reg_field TIME_STORED;

	function new(string name = "policers_POL_DIRECT_MAP_POL0");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.ETOK_HI = uvm_reg_field::type_id::create("ETOK_HI",,get_full_name());
      this.ETOK_HI.configure(this, 24, 40, "RW", 1, 24'h0, 1, 0, 1);
      this._RSVD_ = uvm_reg_field::type_id::create("_RSVD_",,get_full_name());
      this._RSVD_.configure(this, 3, 37, "RO", 1, 3'h0, 1, 0, 0);
      this.ETOK_LO = uvm_reg_field::type_id::create("ETOK_LO",,get_full_name());
      this.ETOK_LO.configure(this, 13, 24, "RW", 1, 13'h0, 1, 0, 0);
      this.TIME_STORED = uvm_reg_field::type_id::create("TIME_STORED",,get_full_name());
      this.TIME_STORED.configure(this, 24, 0, "RO", 1, 24'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_policers_POL_DIRECT_MAP_POL0)

endclass : ral_reg_policers_POL_DIRECT_MAP_POL0


class ral_reg_policers_POL_DIRECT_MAP_POL1 extends uvm_reg;
	rand uvm_reg_field CTOK_HI;
	uvm_reg_field _RSVD1_;
	rand uvm_reg_field CTOK_LO;
	uvm_reg_field _RSVD0_;
	rand uvm_reg_field CFG;

	function new(string name = "policers_POL_DIRECT_MAP_POL1");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.CTOK_HI = uvm_reg_field::type_id::create("CTOK_HI",,get_full_name());
      this.CTOK_HI.configure(this, 24, 40, "RW", 1, 24'h0, 1, 0, 1);
      this._RSVD1_ = uvm_reg_field::type_id::create("_RSVD1_",,get_full_name());
      this._RSVD1_.configure(this, 3, 37, "RO", 1, 3'h0, 1, 0, 0);
      this.CTOK_LO = uvm_reg_field::type_id::create("CTOK_LO",,get_full_name());
      this.CTOK_LO.configure(this, 13, 24, "RW", 1, 13'h0, 1, 0, 0);
      this._RSVD0_ = uvm_reg_field::type_id::create("_RSVD0_",,get_full_name());
      this._RSVD0_.configure(this, 20, 4, "RO", 1, 20'h0, 1, 0, 0);
      this.CFG = uvm_reg_field::type_id::create("CFG",,get_full_name());
      this.CFG.configure(this, 4, 0, "RW", 0, 4'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_policers_POL_DIRECT_MAP_POL1)

endclass : ral_reg_policers_POL_DIRECT_MAP_POL1


class ral_reg_policers_POL_DIRECT_MAP_POL2 extends uvm_reg;
	uvm_reg_field _RSVD1_;
	rand uvm_reg_field EIR_UNIT;
	rand uvm_reg_field EIR;
	uvm_reg_field _RSVD0_;
	rand uvm_reg_field EBS_UNIT;
	rand uvm_reg_field EBS;

	function new(string name = "policers_POL_DIRECT_MAP_POL2");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this._RSVD1_ = uvm_reg_field::type_id::create("_RSVD1_",,get_full_name());
      this._RSVD1_.configure(this, 34, 30, "RO", 1, 34'h0, 1, 0, 0);
      this.EIR_UNIT = uvm_reg_field::type_id::create("EIR_UNIT",,get_full_name());
      this.EIR_UNIT.configure(this, 3, 27, "RW", 0, 3'h0, 1, 0, 0);
      this.EIR = uvm_reg_field::type_id::create("EIR",,get_full_name());
      this.EIR.configure(this, 11, 16, "RW", 0, 11'h0, 1, 0, 0);
      this._RSVD0_ = uvm_reg_field::type_id::create("_RSVD0_",,get_full_name());
      this._RSVD0_.configure(this, 1, 15, "RO", 1, 1'h0, 1, 0, 0);
      this.EBS_UNIT = uvm_reg_field::type_id::create("EBS_UNIT",,get_full_name());
      this.EBS_UNIT.configure(this, 2, 13, "RW", 0, 2'h0, 1, 0, 0);
      this.EBS = uvm_reg_field::type_id::create("EBS",,get_full_name());
      this.EBS.configure(this, 13, 0, "RW", 0, 13'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_policers_POL_DIRECT_MAP_POL2)

endclass : ral_reg_policers_POL_DIRECT_MAP_POL2


class ral_reg_policers_POL_DIRECT_MAP_POL3 extends uvm_reg;
	uvm_reg_field _RSVD1_;
	rand uvm_reg_field CIR_UNIT;
	rand uvm_reg_field CIR;
	uvm_reg_field _RSVD0_;
	rand uvm_reg_field CBS_UNIT;
	rand uvm_reg_field CBS;

	function new(string name = "policers_POL_DIRECT_MAP_POL3");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this._RSVD1_ = uvm_reg_field::type_id::create("_RSVD1_",,get_full_name());
      this._RSVD1_.configure(this, 34, 30, "RO", 1, 34'h0, 1, 0, 0);
      this.CIR_UNIT = uvm_reg_field::type_id::create("CIR_UNIT",,get_full_name());
      this.CIR_UNIT.configure(this, 3, 27, "RW", 0, 3'h0, 1, 0, 0);
      this.CIR = uvm_reg_field::type_id::create("CIR",,get_full_name());
      this.CIR.configure(this, 11, 16, "RW", 0, 11'h0, 1, 0, 0);
      this._RSVD0_ = uvm_reg_field::type_id::create("_RSVD0_",,get_full_name());
      this._RSVD0_.configure(this, 1, 15, "RO", 1, 1'h0, 1, 0, 0);
      this.CBS_UNIT = uvm_reg_field::type_id::create("CBS_UNIT",,get_full_name());
      this.CBS_UNIT.configure(this, 2, 13, "RW", 0, 2'h0, 1, 0, 0);
      this.CBS = uvm_reg_field::type_id::create("CBS",,get_full_name());
      this.CBS.configure(this, 13, 0, "RW", 0, 13'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_policers_POL_DIRECT_MAP_POL3)

endclass : ral_reg_policers_POL_DIRECT_MAP_POL3


class ral_reg_policers_POL_CFG extends uvm_reg;
	uvm_reg_field _RSVD2_;
	rand uvm_reg_field UNPOLICE_DROP_CM;
	rand uvm_reg_field UNPOLICE_DROP_PRECM;
	rand uvm_reg_field CREDIT_FRAME_ERR;
	rand uvm_reg_field CREDIT_L3_LEN_ERR;
	uvm_reg_field _RSVD1_;
	rand uvm_reg_field CB;
	rand uvm_reg_field CF;
	rand uvm_reg_field COLOR_SELECT;
	rand uvm_reg_field PRECEDENCE;
	rand uvm_reg_field DEBIT_MODE;
	rand uvm_reg_field L3_LEN_MODE;
	uvm_reg_field _RSVD0_;

	function new(string name = "policers_POL_CFG");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this._RSVD2_ = uvm_reg_field::type_id::create("_RSVD2_",,get_full_name());
      this._RSVD2_.configure(this, 41, 23, "RO", 1, 41'h0, 1, 0, 0);
      this.UNPOLICE_DROP_CM = uvm_reg_field::type_id::create("UNPOLICE_DROP_CM",,get_full_name());
      this.UNPOLICE_DROP_CM.configure(this, 1, 22, "RW", 0, 1'h0, 1, 0, 0);
      this.UNPOLICE_DROP_PRECM = uvm_reg_field::type_id::create("UNPOLICE_DROP_PRECM",,get_full_name());
      this.UNPOLICE_DROP_PRECM.configure(this, 1, 21, "RW", 0, 1'h1, 1, 0, 0);
      this.CREDIT_FRAME_ERR = uvm_reg_field::type_id::create("CREDIT_FRAME_ERR",,get_full_name());
      this.CREDIT_FRAME_ERR.configure(this, 1, 20, "RW", 0, 1'h1, 1, 0, 0);
      this.CREDIT_L3_LEN_ERR = uvm_reg_field::type_id::create("CREDIT_L3_LEN_ERR",,get_full_name());
      this.CREDIT_L3_LEN_ERR.configure(this, 1, 19, "RW", 0, 1'h0, 1, 0, 0);
      this._RSVD1_ = uvm_reg_field::type_id::create("_RSVD1_",,get_full_name());
      this._RSVD1_.configure(this, 2, 17, "RO", 1, 2'h0, 1, 0, 0);
      this.CB = uvm_reg_field::type_id::create("CB",,get_full_name());
      this.CB.configure(this, 1, 16, "RW", 0, 1'h0, 1, 0, 0);
      this.CF = uvm_reg_field::type_id::create("CF",,get_full_name());
      this.CF.configure(this, 1, 15, "RW", 0, 1'h0, 1, 0, 0);
      this.COLOR_SELECT = uvm_reg_field::type_id::create("COLOR_SELECT",,get_full_name());
      this.COLOR_SELECT.configure(this, 1, 14, "RW", 0, 1'h0, 1, 0, 0);
      this.PRECEDENCE = uvm_reg_field::type_id::create("PRECEDENCE",,get_full_name());
      this.PRECEDENCE.configure(this, 1, 13, "RW", 0, 1'h0, 1, 0, 0);
      this.DEBIT_MODE = uvm_reg_field::type_id::create("DEBIT_MODE",,get_full_name());
      this.DEBIT_MODE.configure(this, 1, 12, "RW", 0, 1'h0, 1, 0, 0);
      this.L3_LEN_MODE = uvm_reg_field::type_id::create("L3_LEN_MODE",,get_full_name());
      this.L3_LEN_MODE.configure(this, 1, 11, "RW", 0, 1'h0, 1, 0, 0);
      this._RSVD0_ = uvm_reg_field::type_id::create("_RSVD0_",,get_full_name());
      this._RSVD0_.configure(this, 11, 0, "RO", 1, 11'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_policers_POL_CFG)

endclass : ral_reg_policers_POL_CFG


class ral_reg_policers_POL_DSCP extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field IR;
	rand uvm_reg_field IG;
	rand uvm_reg_field DROP_ER;
	rand uvm_reg_field TO_YELLOW;
	rand uvm_reg_field TO_RED;

	function new(string name = "policers_POL_DSCP");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 49, 15, "RO", 0, 49'h0, 1, 0, 0);
      this.IR = uvm_reg_field::type_id::create("IR",,get_full_name());
      this.IR.configure(this, 1, 14, "RW", 0, 1'h0, 1, 0, 0);
      this.IG = uvm_reg_field::type_id::create("IG",,get_full_name());
      this.IG.configure(this, 1, 13, "RW", 0, 1'h0, 1, 0, 0);
      this.DROP_ER = uvm_reg_field::type_id::create("DROP_ER",,get_full_name());
      this.DROP_ER.configure(this, 1, 12, "RW", 0, 1'h0, 1, 0, 0);
      this.TO_YELLOW = uvm_reg_field::type_id::create("TO_YELLOW",,get_full_name());
      this.TO_YELLOW.configure(this, 6, 6, "RW", 0, 6'h0, 1, 0, 0);
      this.TO_RED = uvm_reg_field::type_id::create("TO_RED",,get_full_name());
      this.TO_RED.configure(this, 6, 0, "RW", 0, 6'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_policers_POL_DSCP)

endclass : ral_reg_policers_POL_DSCP


class ral_reg_policers_POL_VPRI extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field IR;
	rand uvm_reg_field IG;
	rand uvm_reg_field DROP_ER;
	rand uvm_reg_field TO_YELLOW;
	rand uvm_reg_field TO_RED;

	function new(string name = "policers_POL_VPRI");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 53, 11, "RO", 0, 53'h0, 1, 0, 0);
      this.IR = uvm_reg_field::type_id::create("IR",,get_full_name());
      this.IR.configure(this, 1, 10, "RW", 0, 1'h0, 1, 0, 0);
      this.IG = uvm_reg_field::type_id::create("IG",,get_full_name());
      this.IG.configure(this, 1, 9, "RW", 0, 1'h0, 1, 0, 0);
      this.DROP_ER = uvm_reg_field::type_id::create("DROP_ER",,get_full_name());
      this.DROP_ER.configure(this, 1, 8, "RW", 0, 1'h0, 1, 0, 0);
      this.TO_YELLOW = uvm_reg_field::type_id::create("TO_YELLOW",,get_full_name());
      this.TO_YELLOW.configure(this, 4, 4, "RW", 0, 4'h0, 1, 0, 0);
      this.TO_RED = uvm_reg_field::type_id::create("TO_RED",,get_full_name());
      this.TO_RED.configure(this, 4, 0, "RW", 0, 4'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_policers_POL_VPRI)

endclass : ral_reg_policers_POL_VPRI


class ral_reg_policers_POL_TIME_UNIT extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field POL_TIME_UNIT;

	function new(string name = "policers_POL_TIME_UNIT");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 44, 20, "RO", 0, 44'h0, 1, 0, 0);
      this.POL_TIME_UNIT = uvm_reg_field::type_id::create("POL_TIME_UNIT",,get_full_name());
      this.POL_TIME_UNIT.configure(this, 20, 0, "RW", 0, 20'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_policers_POL_TIME_UNIT)

endclass : ral_reg_policers_POL_TIME_UNIT


class ral_reg_policers_POL_TIME extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field POL_TIME;

	function new(string name = "policers_POL_TIME");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 16, 48, "RO", 0, 16'h0, 1, 0, 1);
      this.POL_TIME = uvm_reg_field::type_id::create("POL_TIME",,get_full_name());
      this.POL_TIME.configure(this, 48, 0, "RW", 1, 48'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_policers_POL_TIME)

endclass : ral_reg_policers_POL_TIME


class ral_reg_policers_POL_SWEEP extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field ELAPSE_CYCLE;
	rand uvm_reg_field ELAPSE_SLOT;
	rand uvm_reg_field POL0_HI;
	rand uvm_reg_field POL1_HI;

	function new(string name = "policers_POL_SWEEP");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 16, 48, "RO", 0, 16'h0, 1, 0, 1);
      this.ELAPSE_CYCLE = uvm_reg_field::type_id::create("ELAPSE_CYCLE",,get_full_name());
      this.ELAPSE_CYCLE.configure(this, 16, 32, "RW", 0, 16'h0, 1, 0, 1);
      this.ELAPSE_SLOT = uvm_reg_field::type_id::create("ELAPSE_SLOT",,get_full_name());
      this.ELAPSE_SLOT.configure(this, 8, 24, "RW", 0, 8'h0, 1, 0, 1);
      this.POL0_HI = uvm_reg_field::type_id::create("POL0_HI",,get_full_name());
      this.POL0_HI.configure(this, 12, 12, "RW", 0, 12'h0, 1, 0, 0);
      this.POL1_HI = uvm_reg_field::type_id::create("POL1_HI",,get_full_name());
      this.POL1_HI.configure(this, 12, 0, "RW", 0, 12'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_policers_POL_SWEEP)

endclass : ral_reg_policers_POL_SWEEP


class ral_reg_policers_POL_SWEEP_LAST extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field POL_SWEEP_LAST;

	function new(string name = "policers_POL_SWEEP_LAST");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 16, 48, "RO", 0, 16'h0, 1, 0, 1);
      this.POL_SWEEP_LAST = uvm_reg_field::type_id::create("POL_SWEEP_LAST",,get_full_name());
      this.POL_SWEEP_LAST.configure(this, 48, 0, "RW", 1, 48'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_policers_POL_SWEEP_LAST)

endclass : ral_reg_policers_POL_SWEEP_LAST


class ral_reg_policers_POL_SWEEP_PERIOD_MAX extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field POL_SWEEP_PERIOD_MAX;

	function new(string name = "policers_POL_SWEEP_PERIOD_MAX");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 16, 48, "RO", 0, 16'h0, 1, 0, 1);
      this.POL_SWEEP_PERIOD_MAX = uvm_reg_field::type_id::create("POL_SWEEP_PERIOD_MAX",,get_full_name());
      this.POL_SWEEP_PERIOD_MAX.configure(this, 48, 0, "RW", 1, 48'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_policers_POL_SWEEP_PERIOD_MAX)

endclass : ral_reg_policers_POL_SWEEP_PERIOD_MAX


class ral_block_policers extends uvm_reg_block;
	rand ral_reg_policers_POL_DIRECT_MAP_CTRL POL_DIRECT_MAP_CTRL;
	rand ral_reg_policers_POL_DIRECT_MAP_CTR0 POL_DIRECT_MAP_CTR0;
	rand ral_reg_policers_POL_DIRECT_MAP_CTR1 POL_DIRECT_MAP_CTR1;
	rand ral_reg_policers_POL_DIRECT_MAP_POL0 POL_DIRECT_MAP_POL0;
	rand ral_reg_policers_POL_DIRECT_MAP_POL1 POL_DIRECT_MAP_POL1;
	rand ral_reg_policers_POL_DIRECT_MAP_POL2 POL_DIRECT_MAP_POL2;
	rand ral_reg_policers_POL_DIRECT_MAP_POL3 POL_DIRECT_MAP_POL3;
	rand ral_reg_policers_POL_CFG POL_CFG[2][0:15];
	rand ral_reg_policers_POL_DSCP POL_DSCP[16][0:63];
	rand ral_reg_policers_POL_VPRI POL_VPRI[16][0:15];
	rand ral_reg_policers_POL_TIME_UNIT POL_TIME_UNIT;
	rand ral_reg_policers_POL_TIME POL_TIME;
	rand ral_reg_policers_POL_SWEEP POL_SWEEP;
	rand ral_reg_policers_POL_SWEEP_LAST POL_SWEEP_LAST;
	rand ral_reg_policers_POL_SWEEP_PERIOD_MAX POL_SWEEP_PERIOD_MAX;
	rand uvm_reg_field POL_DIRECT_MAP_CTRL_GO_COMPL;
	rand uvm_reg_field GO_COMPL;
	rand uvm_reg_field POL_DIRECT_MAP_CTRL_STATUS;
	rand uvm_reg_field STATUS;
	rand uvm_reg_field POL_DIRECT_MAP_CTRL_OP_TYPE;
	rand uvm_reg_field OP_TYPE;
	uvm_reg_field POL_DIRECT_MAP_CTRL__RSVD0_;
	rand uvm_reg_field POL_DIRECT_MAP_CTRL_REG_ID;
	rand uvm_reg_field REG_ID;
	rand uvm_reg_field POL_DIRECT_MAP_CTRL_REG_SUB_ID;
	rand uvm_reg_field REG_SUB_ID;
	rand uvm_reg_field POL_DIRECT_MAP_CTRL_REG_INDX;
	rand uvm_reg_field REG_INDX;
	uvm_reg_field POL_DIRECT_MAP_CTR0_RSVD0;
	rand uvm_reg_field POL_DIRECT_MAP_CTR0_DATA_CNTB;
	rand uvm_reg_field DATA_CNTB;
	uvm_reg_field POL_DIRECT_MAP_CTR1_RSVD0;
	rand uvm_reg_field POL_DIRECT_MAP_CTR1_DATA_CNTP;
	rand uvm_reg_field DATA_CNTP;
	rand uvm_reg_field POL_DIRECT_MAP_POL0_ETOK_HI;
	rand uvm_reg_field ETOK_HI;
	uvm_reg_field POL_DIRECT_MAP_POL0__RSVD_;
	uvm_reg_field _RSVD_;
	rand uvm_reg_field POL_DIRECT_MAP_POL0_ETOK_LO;
	rand uvm_reg_field ETOK_LO;
	uvm_reg_field POL_DIRECT_MAP_POL0_TIME_STORED;
	uvm_reg_field TIME_STORED;
	rand uvm_reg_field POL_DIRECT_MAP_POL1_CTOK_HI;
	rand uvm_reg_field CTOK_HI;
	uvm_reg_field POL_DIRECT_MAP_POL1__RSVD1_;
	rand uvm_reg_field POL_DIRECT_MAP_POL1_CTOK_LO;
	rand uvm_reg_field CTOK_LO;
	uvm_reg_field POL_DIRECT_MAP_POL1__RSVD0_;
	rand uvm_reg_field POL_DIRECT_MAP_POL1_CFG;
	rand uvm_reg_field CFG;
	uvm_reg_field POL_DIRECT_MAP_POL2__RSVD1_;
	rand uvm_reg_field POL_DIRECT_MAP_POL2_EIR_UNIT;
	rand uvm_reg_field EIR_UNIT;
	rand uvm_reg_field POL_DIRECT_MAP_POL2_EIR;
	rand uvm_reg_field EIR;
	uvm_reg_field POL_DIRECT_MAP_POL2__RSVD0_;
	rand uvm_reg_field POL_DIRECT_MAP_POL2_EBS_UNIT;
	rand uvm_reg_field EBS_UNIT;
	rand uvm_reg_field POL_DIRECT_MAP_POL2_EBS;
	rand uvm_reg_field EBS;
	uvm_reg_field POL_DIRECT_MAP_POL3__RSVD1_;
	rand uvm_reg_field POL_DIRECT_MAP_POL3_CIR_UNIT;
	rand uvm_reg_field CIR_UNIT;
	rand uvm_reg_field POL_DIRECT_MAP_POL3_CIR;
	rand uvm_reg_field CIR;
	uvm_reg_field POL_DIRECT_MAP_POL3__RSVD0_;
	rand uvm_reg_field POL_DIRECT_MAP_POL3_CBS_UNIT;
	rand uvm_reg_field CBS_UNIT;
	rand uvm_reg_field POL_DIRECT_MAP_POL3_CBS;
	rand uvm_reg_field CBS;
	uvm_reg_field POL_CFG__RSVD2_[2][0:15];
	uvm_reg_field _RSVD2_[2][0:15];
	rand uvm_reg_field POL_CFG_UNPOLICE_DROP_CM[2][0:15];
	rand uvm_reg_field UNPOLICE_DROP_CM[2][0:15];
	rand uvm_reg_field POL_CFG_UNPOLICE_DROP_PRECM[2][0:15];
	rand uvm_reg_field UNPOLICE_DROP_PRECM[2][0:15];
	rand uvm_reg_field POL_CFG_CREDIT_FRAME_ERR[2][0:15];
	rand uvm_reg_field CREDIT_FRAME_ERR[2][0:15];
	rand uvm_reg_field POL_CFG_CREDIT_L3_LEN_ERR[2][0:15];
	rand uvm_reg_field CREDIT_L3_LEN_ERR[2][0:15];
	uvm_reg_field POL_CFG__RSVD1_[2][0:15];
	rand uvm_reg_field POL_CFG_CB[2][0:15];
	rand uvm_reg_field CB[2][0:15];
	rand uvm_reg_field POL_CFG_CF[2][0:15];
	rand uvm_reg_field CF[2][0:15];
	rand uvm_reg_field POL_CFG_COLOR_SELECT[2][0:15];
	rand uvm_reg_field COLOR_SELECT[2][0:15];
	rand uvm_reg_field POL_CFG_PRECEDENCE[2][0:15];
	rand uvm_reg_field PRECEDENCE[2][0:15];
	rand uvm_reg_field POL_CFG_DEBIT_MODE[2][0:15];
	rand uvm_reg_field DEBIT_MODE[2][0:15];
	rand uvm_reg_field POL_CFG_L3_LEN_MODE[2][0:15];
	rand uvm_reg_field L3_LEN_MODE[2][0:15];
	uvm_reg_field POL_CFG__RSVD0_[2][0:15];
	uvm_reg_field POL_DSCP_RSVD0[16][0:63];
	rand uvm_reg_field POL_DSCP_IR[16][0:63];
	rand uvm_reg_field POL_DSCP_IG[16][0:63];
	rand uvm_reg_field POL_DSCP_DROP_ER[16][0:63];
	rand uvm_reg_field POL_DSCP_TO_YELLOW[16][0:63];
	rand uvm_reg_field POL_DSCP_TO_RED[16][0:63];
	uvm_reg_field POL_VPRI_RSVD0[16][0:15];
	rand uvm_reg_field POL_VPRI_IR[16][0:15];
	rand uvm_reg_field POL_VPRI_IG[16][0:15];
	rand uvm_reg_field POL_VPRI_DROP_ER[16][0:15];
	rand uvm_reg_field POL_VPRI_TO_YELLOW[16][0:15];
	rand uvm_reg_field POL_VPRI_TO_RED[16][0:15];
	uvm_reg_field POL_TIME_UNIT_RSVD0;
	rand uvm_reg_field POL_TIME_UNIT_POL_TIME_UNIT;
	uvm_reg_field POL_TIME_RSVD0;
	rand uvm_reg_field POL_TIME_POL_TIME;
	uvm_reg_field POL_SWEEP_RSVD0;
	rand uvm_reg_field POL_SWEEP_ELAPSE_CYCLE;
	rand uvm_reg_field ELAPSE_CYCLE;
	rand uvm_reg_field POL_SWEEP_ELAPSE_SLOT;
	rand uvm_reg_field ELAPSE_SLOT;
	rand uvm_reg_field POL_SWEEP_POL0_HI;
	rand uvm_reg_field POL0_HI;
	rand uvm_reg_field POL_SWEEP_POL1_HI;
	rand uvm_reg_field POL1_HI;
	uvm_reg_field POL_SWEEP_LAST_RSVD0;
	rand uvm_reg_field POL_SWEEP_LAST_POL_SWEEP_LAST;
	uvm_reg_field POL_SWEEP_PERIOD_MAX_RSVD0;
	rand uvm_reg_field POL_SWEEP_PERIOD_MAX_POL_SWEEP_PERIOD_MAX;

	function new(string name = "policers");
		super.new(name, build_coverage(UVM_NO_COVERAGE));
	endfunction: new

   virtual function void build();
      this.default_map = create_map("", 0, 8, UVM_LITTLE_ENDIAN, 0);
      this.POL_DIRECT_MAP_CTRL = ral_reg_policers_POL_DIRECT_MAP_CTRL::type_id::create("POL_DIRECT_MAP_CTRL",,get_full_name());
      this.POL_DIRECT_MAP_CTRL.configure(this, null, "");
      this.POL_DIRECT_MAP_CTRL.build();
         this.POL_DIRECT_MAP_CTRL.add_hdl_path('{

            '{"POL_DIRECT_MAP_CTRL", -1, -1}
         });
      this.default_map.add_reg(this.POL_DIRECT_MAP_CTRL, `UVM_REG_ADDR_WIDTH'h0, "RW", 0);
		this.POL_DIRECT_MAP_CTRL_GO_COMPL = this.POL_DIRECT_MAP_CTRL.GO_COMPL;
		this.GO_COMPL = this.POL_DIRECT_MAP_CTRL.GO_COMPL;
		this.POL_DIRECT_MAP_CTRL_STATUS = this.POL_DIRECT_MAP_CTRL.STATUS;
		this.STATUS = this.POL_DIRECT_MAP_CTRL.STATUS;
		this.POL_DIRECT_MAP_CTRL_OP_TYPE = this.POL_DIRECT_MAP_CTRL.OP_TYPE;
		this.OP_TYPE = this.POL_DIRECT_MAP_CTRL.OP_TYPE;
		this.POL_DIRECT_MAP_CTRL__RSVD0_ = this.POL_DIRECT_MAP_CTRL._RSVD0_;
		this.POL_DIRECT_MAP_CTRL_REG_ID = this.POL_DIRECT_MAP_CTRL.REG_ID;
		this.REG_ID = this.POL_DIRECT_MAP_CTRL.REG_ID;
		this.POL_DIRECT_MAP_CTRL_REG_SUB_ID = this.POL_DIRECT_MAP_CTRL.REG_SUB_ID;
		this.REG_SUB_ID = this.POL_DIRECT_MAP_CTRL.REG_SUB_ID;
		this.POL_DIRECT_MAP_CTRL_REG_INDX = this.POL_DIRECT_MAP_CTRL.REG_INDX;
		this.REG_INDX = this.POL_DIRECT_MAP_CTRL.REG_INDX;
      this.POL_DIRECT_MAP_CTR0 = ral_reg_policers_POL_DIRECT_MAP_CTR0::type_id::create("POL_DIRECT_MAP_CTR0",,get_full_name());
      this.POL_DIRECT_MAP_CTR0.configure(this, null, "");
      this.POL_DIRECT_MAP_CTR0.build();
         this.POL_DIRECT_MAP_CTR0.add_hdl_path('{

            '{"POL_DIRECT_MAP_CTR0", -1, -1}
         });
      this.default_map.add_reg(this.POL_DIRECT_MAP_CTR0, `UVM_REG_ADDR_WIDTH'h8, "RW", 0);
		this.POL_DIRECT_MAP_CTR0_RSVD0 = this.POL_DIRECT_MAP_CTR0.RSVD0;
		this.POL_DIRECT_MAP_CTR0_DATA_CNTB = this.POL_DIRECT_MAP_CTR0.DATA_CNTB;
		this.DATA_CNTB = this.POL_DIRECT_MAP_CTR0.DATA_CNTB;
      this.POL_DIRECT_MAP_CTR1 = ral_reg_policers_POL_DIRECT_MAP_CTR1::type_id::create("POL_DIRECT_MAP_CTR1",,get_full_name());
      this.POL_DIRECT_MAP_CTR1.configure(this, null, "");
      this.POL_DIRECT_MAP_CTR1.build();
         this.POL_DIRECT_MAP_CTR1.add_hdl_path('{

            '{"POL_DIRECT_MAP_CTR1", -1, -1}
         });
      this.default_map.add_reg(this.POL_DIRECT_MAP_CTR1, `UVM_REG_ADDR_WIDTH'h10, "RW", 0);
		this.POL_DIRECT_MAP_CTR1_RSVD0 = this.POL_DIRECT_MAP_CTR1.RSVD0;
		this.POL_DIRECT_MAP_CTR1_DATA_CNTP = this.POL_DIRECT_MAP_CTR1.DATA_CNTP;
		this.DATA_CNTP = this.POL_DIRECT_MAP_CTR1.DATA_CNTP;
      this.POL_DIRECT_MAP_POL0 = ral_reg_policers_POL_DIRECT_MAP_POL0::type_id::create("POL_DIRECT_MAP_POL0",,get_full_name());
      this.POL_DIRECT_MAP_POL0.configure(this, null, "");
      this.POL_DIRECT_MAP_POL0.build();
         this.POL_DIRECT_MAP_POL0.add_hdl_path('{

            '{"POL_DIRECT_MAP_POL0", -1, -1}
         });
      this.default_map.add_reg(this.POL_DIRECT_MAP_POL0, `UVM_REG_ADDR_WIDTH'h18, "RW", 0);
		this.POL_DIRECT_MAP_POL0_ETOK_HI = this.POL_DIRECT_MAP_POL0.ETOK_HI;
		this.ETOK_HI = this.POL_DIRECT_MAP_POL0.ETOK_HI;
		this.POL_DIRECT_MAP_POL0__RSVD_ = this.POL_DIRECT_MAP_POL0._RSVD_;
		this._RSVD_ = this.POL_DIRECT_MAP_POL0._RSVD_;
		this.POL_DIRECT_MAP_POL0_ETOK_LO = this.POL_DIRECT_MAP_POL0.ETOK_LO;
		this.ETOK_LO = this.POL_DIRECT_MAP_POL0.ETOK_LO;
		this.POL_DIRECT_MAP_POL0_TIME_STORED = this.POL_DIRECT_MAP_POL0.TIME_STORED;
		this.TIME_STORED = this.POL_DIRECT_MAP_POL0.TIME_STORED;
      this.POL_DIRECT_MAP_POL1 = ral_reg_policers_POL_DIRECT_MAP_POL1::type_id::create("POL_DIRECT_MAP_POL1",,get_full_name());
      this.POL_DIRECT_MAP_POL1.configure(this, null, "");
      this.POL_DIRECT_MAP_POL1.build();
         this.POL_DIRECT_MAP_POL1.add_hdl_path('{

            '{"POL_DIRECT_MAP_POL1", -1, -1}
         });
      this.default_map.add_reg(this.POL_DIRECT_MAP_POL1, `UVM_REG_ADDR_WIDTH'h20, "RW", 0);
		this.POL_DIRECT_MAP_POL1_CTOK_HI = this.POL_DIRECT_MAP_POL1.CTOK_HI;
		this.CTOK_HI = this.POL_DIRECT_MAP_POL1.CTOK_HI;
		this.POL_DIRECT_MAP_POL1__RSVD1_ = this.POL_DIRECT_MAP_POL1._RSVD1_;
		this.POL_DIRECT_MAP_POL1_CTOK_LO = this.POL_DIRECT_MAP_POL1.CTOK_LO;
		this.CTOK_LO = this.POL_DIRECT_MAP_POL1.CTOK_LO;
		this.POL_DIRECT_MAP_POL1__RSVD0_ = this.POL_DIRECT_MAP_POL1._RSVD0_;
		this.POL_DIRECT_MAP_POL1_CFG = this.POL_DIRECT_MAP_POL1.CFG;
		this.CFG = this.POL_DIRECT_MAP_POL1.CFG;
      this.POL_DIRECT_MAP_POL2 = ral_reg_policers_POL_DIRECT_MAP_POL2::type_id::create("POL_DIRECT_MAP_POL2",,get_full_name());
      this.POL_DIRECT_MAP_POL2.configure(this, null, "");
      this.POL_DIRECT_MAP_POL2.build();
         this.POL_DIRECT_MAP_POL2.add_hdl_path('{

            '{"POL_DIRECT_MAP_POL2", -1, -1}
         });
      this.default_map.add_reg(this.POL_DIRECT_MAP_POL2, `UVM_REG_ADDR_WIDTH'h28, "RW", 0);
		this.POL_DIRECT_MAP_POL2__RSVD1_ = this.POL_DIRECT_MAP_POL2._RSVD1_;
		this.POL_DIRECT_MAP_POL2_EIR_UNIT = this.POL_DIRECT_MAP_POL2.EIR_UNIT;
		this.EIR_UNIT = this.POL_DIRECT_MAP_POL2.EIR_UNIT;
		this.POL_DIRECT_MAP_POL2_EIR = this.POL_DIRECT_MAP_POL2.EIR;
		this.EIR = this.POL_DIRECT_MAP_POL2.EIR;
		this.POL_DIRECT_MAP_POL2__RSVD0_ = this.POL_DIRECT_MAP_POL2._RSVD0_;
		this.POL_DIRECT_MAP_POL2_EBS_UNIT = this.POL_DIRECT_MAP_POL2.EBS_UNIT;
		this.EBS_UNIT = this.POL_DIRECT_MAP_POL2.EBS_UNIT;
		this.POL_DIRECT_MAP_POL2_EBS = this.POL_DIRECT_MAP_POL2.EBS;
		this.EBS = this.POL_DIRECT_MAP_POL2.EBS;
      this.POL_DIRECT_MAP_POL3 = ral_reg_policers_POL_DIRECT_MAP_POL3::type_id::create("POL_DIRECT_MAP_POL3",,get_full_name());
      this.POL_DIRECT_MAP_POL3.configure(this, null, "");
      this.POL_DIRECT_MAP_POL3.build();
         this.POL_DIRECT_MAP_POL3.add_hdl_path('{

            '{"POL_DIRECT_MAP_POL3", -1, -1}
         });
      this.default_map.add_reg(this.POL_DIRECT_MAP_POL3, `UVM_REG_ADDR_WIDTH'h30, "RW", 0);
		this.POL_DIRECT_MAP_POL3__RSVD1_ = this.POL_DIRECT_MAP_POL3._RSVD1_;
		this.POL_DIRECT_MAP_POL3_CIR_UNIT = this.POL_DIRECT_MAP_POL3.CIR_UNIT;
		this.CIR_UNIT = this.POL_DIRECT_MAP_POL3.CIR_UNIT;
		this.POL_DIRECT_MAP_POL3_CIR = this.POL_DIRECT_MAP_POL3.CIR;
		this.CIR = this.POL_DIRECT_MAP_POL3.CIR;
		this.POL_DIRECT_MAP_POL3__RSVD0_ = this.POL_DIRECT_MAP_POL3._RSVD0_;
		this.POL_DIRECT_MAP_POL3_CBS_UNIT = this.POL_DIRECT_MAP_POL3.CBS_UNIT;
		this.CBS_UNIT = this.POL_DIRECT_MAP_POL3.CBS_UNIT;
		this.POL_DIRECT_MAP_POL3_CBS = this.POL_DIRECT_MAP_POL3.CBS;
		this.CBS = this.POL_DIRECT_MAP_POL3.CBS;
      foreach (this.POL_CFG[i,j]) begin
         int J = i;
         int K = j;
         this.POL_CFG[J][K] = ral_reg_policers_POL_CFG::type_id::create($psprintf("POL_CFG[%0d][%0d]",J,K),,get_full_name());
         this.POL_CFG[J][K].configure(this, null, "");
         this.POL_CFG[J][K].build();
         this.POL_CFG[J][K].add_hdl_path('{

            '{$psprintf("POL_CFG[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.POL_CFG[J][K], `UVM_REG_ADDR_WIDTH'h40+J*`UVM_REG_ADDR_WIDTH'h80+K*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.POL_CFG__RSVD2_[J][K] = this.POL_CFG[J][K]._RSVD2_;
			this._RSVD2_[J][K] = this.POL_CFG[J][K]._RSVD2_;
			this.POL_CFG_UNPOLICE_DROP_CM[J][K] = this.POL_CFG[J][K].UNPOLICE_DROP_CM;
			this.UNPOLICE_DROP_CM[J][K] = this.POL_CFG[J][K].UNPOLICE_DROP_CM;
			this.POL_CFG_UNPOLICE_DROP_PRECM[J][K] = this.POL_CFG[J][K].UNPOLICE_DROP_PRECM;
			this.UNPOLICE_DROP_PRECM[J][K] = this.POL_CFG[J][K].UNPOLICE_DROP_PRECM;
			this.POL_CFG_CREDIT_FRAME_ERR[J][K] = this.POL_CFG[J][K].CREDIT_FRAME_ERR;
			this.CREDIT_FRAME_ERR[J][K] = this.POL_CFG[J][K].CREDIT_FRAME_ERR;
			this.POL_CFG_CREDIT_L3_LEN_ERR[J][K] = this.POL_CFG[J][K].CREDIT_L3_LEN_ERR;
			this.CREDIT_L3_LEN_ERR[J][K] = this.POL_CFG[J][K].CREDIT_L3_LEN_ERR;
			this.POL_CFG__RSVD1_[J][K] = this.POL_CFG[J][K]._RSVD1_;
			this.POL_CFG_CB[J][K] = this.POL_CFG[J][K].CB;
			this.CB[J][K] = this.POL_CFG[J][K].CB;
			this.POL_CFG_CF[J][K] = this.POL_CFG[J][K].CF;
			this.CF[J][K] = this.POL_CFG[J][K].CF;
			this.POL_CFG_COLOR_SELECT[J][K] = this.POL_CFG[J][K].COLOR_SELECT;
			this.COLOR_SELECT[J][K] = this.POL_CFG[J][K].COLOR_SELECT;
			this.POL_CFG_PRECEDENCE[J][K] = this.POL_CFG[J][K].PRECEDENCE;
			this.PRECEDENCE[J][K] = this.POL_CFG[J][K].PRECEDENCE;
			this.POL_CFG_DEBIT_MODE[J][K] = this.POL_CFG[J][K].DEBIT_MODE;
			this.DEBIT_MODE[J][K] = this.POL_CFG[J][K].DEBIT_MODE;
			this.POL_CFG_L3_LEN_MODE[J][K] = this.POL_CFG[J][K].L3_LEN_MODE;
			this.L3_LEN_MODE[J][K] = this.POL_CFG[J][K].L3_LEN_MODE;
			this.POL_CFG__RSVD0_[J][K] = this.POL_CFG[J][K]._RSVD0_;
      end
      foreach (this.POL_DSCP[i,j]) begin
         int J = i;
         int K = j;
         this.POL_DSCP[J][K] = ral_reg_policers_POL_DSCP::type_id::create($psprintf("POL_DSCP[%0d][%0d]",J,K),,get_full_name());
         this.POL_DSCP[J][K].configure(this, null, "");
         this.POL_DSCP[J][K].build();
         this.POL_DSCP[J][K].add_hdl_path('{

            '{$psprintf("POL_DSCP[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.POL_DSCP[J][K], `UVM_REG_ADDR_WIDTH'h2040+J*`UVM_REG_ADDR_WIDTH'h200+K*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.POL_DSCP_RSVD0[J][K] = this.POL_DSCP[J][K].RSVD0;
			this.POL_DSCP_IR[J][K] = this.POL_DSCP[J][K].IR;
			this.POL_DSCP_IG[J][K] = this.POL_DSCP[J][K].IG;
			this.POL_DSCP_DROP_ER[J][K] = this.POL_DSCP[J][K].DROP_ER;
			this.POL_DSCP_TO_YELLOW[J][K] = this.POL_DSCP[J][K].TO_YELLOW;
			this.POL_DSCP_TO_RED[J][K] = this.POL_DSCP[J][K].TO_RED;
      end
      foreach (this.POL_VPRI[i,j]) begin
         int J = i;
         int K = j;
         this.POL_VPRI[J][K] = ral_reg_policers_POL_VPRI::type_id::create($psprintf("POL_VPRI[%0d][%0d]",J,K),,get_full_name());
         this.POL_VPRI[J][K].configure(this, null, "");
         this.POL_VPRI[J][K].build();
         this.POL_VPRI[J][K].add_hdl_path('{

            '{$psprintf("POL_VPRI[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.POL_VPRI[J][K], `UVM_REG_ADDR_WIDTH'h4040+J*`UVM_REG_ADDR_WIDTH'h80+K*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.POL_VPRI_RSVD0[J][K] = this.POL_VPRI[J][K].RSVD0;
			this.POL_VPRI_IR[J][K] = this.POL_VPRI[J][K].IR;
			this.POL_VPRI_IG[J][K] = this.POL_VPRI[J][K].IG;
			this.POL_VPRI_DROP_ER[J][K] = this.POL_VPRI[J][K].DROP_ER;
			this.POL_VPRI_TO_YELLOW[J][K] = this.POL_VPRI[J][K].TO_YELLOW;
			this.POL_VPRI_TO_RED[J][K] = this.POL_VPRI[J][K].TO_RED;
      end
      this.POL_TIME_UNIT = ral_reg_policers_POL_TIME_UNIT::type_id::create("POL_TIME_UNIT",,get_full_name());
      this.POL_TIME_UNIT.configure(this, null, "");
      this.POL_TIME_UNIT.build();
         this.POL_TIME_UNIT.add_hdl_path('{

            '{"POL_TIME_UNIT", -1, -1}
         });
      this.default_map.add_reg(this.POL_TIME_UNIT, `UVM_REG_ADDR_WIDTH'h4840, "RW", 0);
		this.POL_TIME_UNIT_RSVD0 = this.POL_TIME_UNIT.RSVD0;
		this.POL_TIME_UNIT_POL_TIME_UNIT = this.POL_TIME_UNIT.POL_TIME_UNIT;
      this.POL_TIME = ral_reg_policers_POL_TIME::type_id::create("POL_TIME",,get_full_name());
      this.POL_TIME.configure(this, null, "");
      this.POL_TIME.build();
         this.POL_TIME.add_hdl_path('{

            '{"POL_TIME", -1, -1}
         });
      this.default_map.add_reg(this.POL_TIME, `UVM_REG_ADDR_WIDTH'h4848, "RW", 0);
		this.POL_TIME_RSVD0 = this.POL_TIME.RSVD0;
		this.POL_TIME_POL_TIME = this.POL_TIME.POL_TIME;
      this.POL_SWEEP = ral_reg_policers_POL_SWEEP::type_id::create("POL_SWEEP",,get_full_name());
      this.POL_SWEEP.configure(this, null, "");
      this.POL_SWEEP.build();
         this.POL_SWEEP.add_hdl_path('{

            '{"POL_SWEEP", -1, -1}
         });
      this.default_map.add_reg(this.POL_SWEEP, `UVM_REG_ADDR_WIDTH'h4850, "RW", 0);
		this.POL_SWEEP_RSVD0 = this.POL_SWEEP.RSVD0;
		this.POL_SWEEP_ELAPSE_CYCLE = this.POL_SWEEP.ELAPSE_CYCLE;
		this.ELAPSE_CYCLE = this.POL_SWEEP.ELAPSE_CYCLE;
		this.POL_SWEEP_ELAPSE_SLOT = this.POL_SWEEP.ELAPSE_SLOT;
		this.ELAPSE_SLOT = this.POL_SWEEP.ELAPSE_SLOT;
		this.POL_SWEEP_POL0_HI = this.POL_SWEEP.POL0_HI;
		this.POL0_HI = this.POL_SWEEP.POL0_HI;
		this.POL_SWEEP_POL1_HI = this.POL_SWEEP.POL1_HI;
		this.POL1_HI = this.POL_SWEEP.POL1_HI;
      this.POL_SWEEP_LAST = ral_reg_policers_POL_SWEEP_LAST::type_id::create("POL_SWEEP_LAST",,get_full_name());
      this.POL_SWEEP_LAST.configure(this, null, "");
      this.POL_SWEEP_LAST.build();
         this.POL_SWEEP_LAST.add_hdl_path('{

            '{"POL_SWEEP_LAST", -1, -1}
         });
      this.default_map.add_reg(this.POL_SWEEP_LAST, `UVM_REG_ADDR_WIDTH'h4858, "RW", 0);
		this.POL_SWEEP_LAST_RSVD0 = this.POL_SWEEP_LAST.RSVD0;
		this.POL_SWEEP_LAST_POL_SWEEP_LAST = this.POL_SWEEP_LAST.POL_SWEEP_LAST;
      this.POL_SWEEP_PERIOD_MAX = ral_reg_policers_POL_SWEEP_PERIOD_MAX::type_id::create("POL_SWEEP_PERIOD_MAX",,get_full_name());
      this.POL_SWEEP_PERIOD_MAX.configure(this, null, "");
      this.POL_SWEEP_PERIOD_MAX.build();
         this.POL_SWEEP_PERIOD_MAX.add_hdl_path('{

            '{"POL_SWEEP_PERIOD_MAX", -1, -1}
         });
      this.default_map.add_reg(this.POL_SWEEP_PERIOD_MAX, `UVM_REG_ADDR_WIDTH'h4860, "RW", 0);
		this.POL_SWEEP_PERIOD_MAX_RSVD0 = this.POL_SWEEP_PERIOD_MAX.RSVD0;
		this.POL_SWEEP_PERIOD_MAX_POL_SWEEP_PERIOD_MAX = this.POL_SWEEP_PERIOD_MAX.POL_SWEEP_PERIOD_MAX;
   endfunction : build

	`uvm_object_utils(ral_block_policers)

endclass : ral_block_policers



`endif
