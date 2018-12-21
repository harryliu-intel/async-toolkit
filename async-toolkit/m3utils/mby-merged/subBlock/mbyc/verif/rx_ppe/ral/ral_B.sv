`ifndef RAL_B
`define RAL_B

import uvm_pkg::*;

class ral_reg_B_EM_HASH_LOOKUP extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field PTR;
	uvm_reg_field RSVD1_;
	rand uvm_reg_field SELECT_4;
	rand uvm_reg_field SELECT_3;
	rand uvm_reg_field SELECT_2;
	rand uvm_reg_field SELECT_1;
	rand uvm_reg_field SELECT_0;
	rand uvm_reg_field MASK;

	function new(string name = "B_EM_HASH_LOOKUP");
		super.new(name, 128,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 44, 84, "RO", 0, 44'h0, 1, 0, 0);
      this.PTR = uvm_reg_field::type_id::create("PTR",,get_full_name());
      this.PTR.configure(this, 20, 64, "RW", 0, 20'h0, 1, 0, 0);
      this.RSVD1_ = uvm_reg_field::type_id::create("RSVD1_",,get_full_name());
      this.RSVD1_.configure(this, 12, 52, "RO", 1, 12'h0, 1, 0, 0);
      this.SELECT_4 = uvm_reg_field::type_id::create("SELECT_4",,get_full_name());
      this.SELECT_4.configure(this, 4, 48, "RW", 0, 4'h0, 1, 0, 0);
      this.SELECT_3 = uvm_reg_field::type_id::create("SELECT_3",,get_full_name());
      this.SELECT_3.configure(this, 4, 44, "RW", 0, 4'h0, 1, 0, 0);
      this.SELECT_2 = uvm_reg_field::type_id::create("SELECT_2",,get_full_name());
      this.SELECT_2.configure(this, 4, 40, "RW", 0, 4'h0, 1, 0, 0);
      this.SELECT_1 = uvm_reg_field::type_id::create("SELECT_1",,get_full_name());
      this.SELECT_1.configure(this, 4, 36, "RW", 0, 4'h0, 1, 0, 0);
      this.SELECT_0 = uvm_reg_field::type_id::create("SELECT_0",,get_full_name());
      this.SELECT_0.configure(this, 4, 32, "RW", 0, 4'h0, 1, 0, 0);
      this.MASK = uvm_reg_field::type_id::create("MASK",,get_full_name());
      this.MASK.configure(this, 32, 0, "RW", 0, 32'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_B_EM_HASH_LOOKUP)

endclass : ral_reg_B_EM_HASH_LOOKUP


class ral_reg_B_WCM_TCAM extends uvm_reg;
	uvm_reg_field _RSVD1_;
	rand uvm_reg_field KEY_TOP_INVERT;
	rand uvm_reg_field KEY_INVERT;
	uvm_reg_field _RSVD0_;
	rand uvm_reg_field KEY_TOP;
	rand uvm_reg_field KEY;

	function new(string name = "B_WCM_TCAM");
		super.new(name, 128,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this._RSVD1_ = uvm_reg_field::type_id::create("_RSVD1_",,get_full_name());
      this._RSVD1_.configure(this, 24, 104, "RO", 1, 24'h0, 1, 0, 1);
      this.KEY_TOP_INVERT = uvm_reg_field::type_id::create("KEY_TOP_INVERT",,get_full_name());
      this.KEY_TOP_INVERT.configure(this, 8, 96, "RW", 0, 8'hff, 1, 0, 1);
      this.KEY_INVERT = uvm_reg_field::type_id::create("KEY_INVERT",,get_full_name());
      this.KEY_INVERT.configure(this, 32, 64, "RW", 0, 32'hffffffff, 1, 0, 1);
      this._RSVD0_ = uvm_reg_field::type_id::create("_RSVD0_",,get_full_name());
      this._RSVD0_.configure(this, 24, 40, "RO", 1, 24'h0, 1, 0, 1);
      this.KEY_TOP = uvm_reg_field::type_id::create("KEY_TOP",,get_full_name());
      this.KEY_TOP.configure(this, 8, 32, "RW", 0, 8'hff, 1, 0, 1);
      this.KEY = uvm_reg_field::type_id::create("KEY",,get_full_name());
      this.KEY.configure(this, 32, 0, "RW", 0, 32'hffffffff, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_B_WCM_TCAM)

endclass : ral_reg_B_WCM_TCAM


class ral_reg_B_WCM_ACTION extends uvm_reg;
	rand uvm_reg_field ACTION1;
	rand uvm_reg_field ACTION0;

	function new(string name = "B_WCM_ACTION");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.ACTION1 = uvm_reg_field::type_id::create("ACTION1",,get_full_name());
      this.ACTION1.configure(this, 32, 32, "RW", 0, 32'h0, 1, 0, 1);
      this.ACTION0 = uvm_reg_field::type_id::create("ACTION0",,get_full_name());
      this.ACTION0.configure(this, 32, 0, "RW", 0, 32'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_B_WCM_ACTION)

endclass : ral_reg_B_WCM_ACTION


class ral_reg_B_WCM_TCAM_CFG extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field CHUNK_MASK;
	rand uvm_reg_field START_COMPARE;
	rand uvm_reg_field START_SET;
	rand uvm_reg_field SELECT_TOP;
	rand uvm_reg_field SELECT0;
	rand uvm_reg_field SELECT1;
	rand uvm_reg_field SELECT2;
	rand uvm_reg_field SELECT3;

	function new(string name = "B_WCM_TCAM_CFG");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 12, 52, "RO", 0, 12'h0, 1, 0, 0);
      this.CHUNK_MASK = uvm_reg_field::type_id::create("CHUNK_MASK",,get_full_name());
      this.CHUNK_MASK.configure(this, 16, 36, "RW", 0, 16'hffff, 1, 0, 0);
      this.START_COMPARE = uvm_reg_field::type_id::create("START_COMPARE",,get_full_name());
      this.START_COMPARE.configure(this, 1, 35, "RW", 0, 1'h1, 1, 0, 0);
      this.START_SET = uvm_reg_field::type_id::create("START_SET",,get_full_name());
      this.START_SET.configure(this, 1, 34, "RW", 0, 1'h1, 1, 0, 0);
      this.SELECT_TOP = uvm_reg_field::type_id::create("SELECT_TOP",,get_full_name());
      this.SELECT_TOP.configure(this, 6, 28, "RW", 0, 6'h0, 1, 0, 0);
      this.SELECT0 = uvm_reg_field::type_id::create("SELECT0",,get_full_name());
      this.SELECT0.configure(this, 7, 21, "RW", 0, 7'h0, 1, 0, 0);
      this.SELECT1 = uvm_reg_field::type_id::create("SELECT1",,get_full_name());
      this.SELECT1.configure(this, 7, 14, "RW", 0, 7'h0, 1, 0, 0);
      this.SELECT2 = uvm_reg_field::type_id::create("SELECT2",,get_full_name());
      this.SELECT2.configure(this, 7, 7, "RW", 0, 7'h0, 1, 0, 0);
      this.SELECT3 = uvm_reg_field::type_id::create("SELECT3",,get_full_name());
      this.SELECT3.configure(this, 7, 0, "RW", 0, 7'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_B_WCM_TCAM_CFG)

endclass : ral_reg_B_WCM_TCAM_CFG


class ral_reg_B_WCM_ACTION_CFG_EN extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field ENABLE;

	function new(string name = "B_WCM_ACTION_CFG_EN");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 40, 24, "RO", 0, 40'h0, 1, 0, 1);
      this.ENABLE = uvm_reg_field::type_id::create("ENABLE",,get_full_name());
      this.ENABLE.configure(this, 24, 0, "RW", 0, 24'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_B_WCM_ACTION_CFG_EN)

endclass : ral_reg_B_WCM_ACTION_CFG_EN


class ral_reg_B_WCM_ACTION_CFG extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field INDEX;

	function new(string name = "B_WCM_ACTION_CFG");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 4, 60, "RO", 0, 4'h0, 1, 0, 0);
      this.INDEX = uvm_reg_field::type_id::create("INDEX",,get_full_name());
      this.INDEX.configure(this, 60, 0, "RW", 0, 60'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_B_WCM_ACTION_CFG)

endclass : ral_reg_B_WCM_ACTION_CFG


class ral_reg_B_WCM_IP extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field HASH_ENTRY_RAM_C_ERR;
	rand uvm_reg_field HASH_ENTRY_RAM_U_ERR;
	rand uvm_reg_field FGHASH_ERR;
	rand uvm_reg_field FGRP_ERR;
	rand uvm_reg_field TCAM_SWEEP_ERR;
	rand uvm_reg_field FCMN_SHELL_CTRL_ERR;

	function new(string name = "B_WCM_IP");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 48, 16, "RO", 0, 48'h0, 1, 0, 1);
      this.HASH_ENTRY_RAM_C_ERR = uvm_reg_field::type_id::create("HASH_ENTRY_RAM_C_ERR",,get_full_name());
      this.HASH_ENTRY_RAM_C_ERR.configure(this, 4, 12, "W1C", 1, 4'h0, 1, 0, 0);
      this.HASH_ENTRY_RAM_U_ERR = uvm_reg_field::type_id::create("HASH_ENTRY_RAM_U_ERR",,get_full_name());
      this.HASH_ENTRY_RAM_U_ERR.configure(this, 4, 8, "W1C", 1, 4'h0, 1, 0, 0);
      this.FGHASH_ERR = uvm_reg_field::type_id::create("FGHASH_ERR",,get_full_name());
      this.FGHASH_ERR.configure(this, 3, 5, "W1C", 1, 3'h0, 1, 0, 0);
      this.FGRP_ERR = uvm_reg_field::type_id::create("FGRP_ERR",,get_full_name());
      this.FGRP_ERR.configure(this, 3, 2, "W1C", 1, 3'h0, 1, 0, 0);
      this.TCAM_SWEEP_ERR = uvm_reg_field::type_id::create("TCAM_SWEEP_ERR",,get_full_name());
      this.TCAM_SWEEP_ERR.configure(this, 1, 1, "W1C", 1, 1'h0, 1, 0, 0);
      this.FCMN_SHELL_CTRL_ERR = uvm_reg_field::type_id::create("FCMN_SHELL_CTRL_ERR",,get_full_name());
      this.FCMN_SHELL_CTRL_ERR.configure(this, 1, 0, "W1C", 1, 1'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_B_WCM_IP)

endclass : ral_reg_B_WCM_IP


class ral_reg_B_WCM_IM extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field HASH_ENTRY_RAM_C_ERR;
	rand uvm_reg_field HASH_ENTRY_RAM_U_ERR;
	rand uvm_reg_field FGHASH_ERR;
	rand uvm_reg_field FGRP_ERR;
	rand uvm_reg_field TCAM_SWEEP_ERR;
	rand uvm_reg_field FCMN_SHELL_CTRL_ERR;

	function new(string name = "B_WCM_IM");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 48, 16, "RO", 0, 48'h0, 1, 0, 1);
      this.HASH_ENTRY_RAM_C_ERR = uvm_reg_field::type_id::create("HASH_ENTRY_RAM_C_ERR",,get_full_name());
      this.HASH_ENTRY_RAM_C_ERR.configure(this, 4, 12, "RW", 0, 4'h0, 1, 0, 0);
      this.HASH_ENTRY_RAM_U_ERR = uvm_reg_field::type_id::create("HASH_ENTRY_RAM_U_ERR",,get_full_name());
      this.HASH_ENTRY_RAM_U_ERR.configure(this, 4, 8, "RW", 0, 4'h0, 1, 0, 0);
      this.FGHASH_ERR = uvm_reg_field::type_id::create("FGHASH_ERR",,get_full_name());
      this.FGHASH_ERR.configure(this, 3, 5, "RW", 0, 3'h0, 1, 0, 0);
      this.FGRP_ERR = uvm_reg_field::type_id::create("FGRP_ERR",,get_full_name());
      this.FGRP_ERR.configure(this, 3, 2, "RW", 0, 3'h0, 1, 0, 0);
      this.TCAM_SWEEP_ERR = uvm_reg_field::type_id::create("TCAM_SWEEP_ERR",,get_full_name());
      this.TCAM_SWEEP_ERR.configure(this, 1, 1, "RW", 0, 1'h0, 1, 0, 0);
      this.FCMN_SHELL_CTRL_ERR = uvm_reg_field::type_id::create("FCMN_SHELL_CTRL_ERR",,get_full_name());
      this.FCMN_SHELL_CTRL_ERR.configure(this, 1, 0, "RW", 0, 1'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_B_WCM_IM)

endclass : ral_reg_B_WCM_IM


class ral_block_B extends uvm_reg_block;
	rand ral_reg_B_EM_HASH_LOOKUP EM_HASH_LOOKUP[8192];
	rand ral_reg_B_WCM_TCAM WCM_TCAM[20][0:1023];
	rand ral_reg_B_WCM_ACTION WCM_ACTION[24][0:1023];
	rand ral_reg_B_WCM_TCAM_CFG WCM_TCAM_CFG[20][0:63];
	rand ral_reg_B_WCM_ACTION_CFG_EN WCM_ACTION_CFG_EN[64];
	rand ral_reg_B_WCM_ACTION_CFG WCM_ACTION_CFG[64][0:1];
	rand ral_reg_B_WCM_IP WCM_IP;
	rand ral_reg_B_WCM_IM WCM_IM;
	uvm_reg_field EM_HASH_LOOKUP_RSVD0[8192];
	rand uvm_reg_field EM_HASH_LOOKUP_PTR[8192];
	rand uvm_reg_field PTR[8192];
	uvm_reg_field EM_HASH_LOOKUP_RSVD1_[8192];
	uvm_reg_field RSVD1_[8192];
	rand uvm_reg_field EM_HASH_LOOKUP_SELECT_4[8192];
	rand uvm_reg_field SELECT_4[8192];
	rand uvm_reg_field EM_HASH_LOOKUP_SELECT_3[8192];
	rand uvm_reg_field SELECT_3[8192];
	rand uvm_reg_field EM_HASH_LOOKUP_SELECT_2[8192];
	rand uvm_reg_field SELECT_2[8192];
	rand uvm_reg_field EM_HASH_LOOKUP_SELECT_1[8192];
	rand uvm_reg_field SELECT_1[8192];
	rand uvm_reg_field EM_HASH_LOOKUP_SELECT_0[8192];
	rand uvm_reg_field SELECT_0[8192];
	rand uvm_reg_field EM_HASH_LOOKUP_MASK[8192];
	rand uvm_reg_field MASK[8192];
	uvm_reg_field WCM_TCAM__RSVD1_[20][0:1023];
	uvm_reg_field _RSVD1_[20][0:1023];
	rand uvm_reg_field WCM_TCAM_KEY_TOP_INVERT[20][0:1023];
	rand uvm_reg_field KEY_TOP_INVERT[20][0:1023];
	rand uvm_reg_field WCM_TCAM_KEY_INVERT[20][0:1023];
	rand uvm_reg_field KEY_INVERT[20][0:1023];
	uvm_reg_field WCM_TCAM__RSVD0_[20][0:1023];
	uvm_reg_field _RSVD0_[20][0:1023];
	rand uvm_reg_field WCM_TCAM_KEY_TOP[20][0:1023];
	rand uvm_reg_field KEY_TOP[20][0:1023];
	rand uvm_reg_field WCM_TCAM_KEY[20][0:1023];
	rand uvm_reg_field KEY[20][0:1023];
	rand uvm_reg_field WCM_ACTION_ACTION1[24][0:1023];
	rand uvm_reg_field ACTION1[24][0:1023];
	rand uvm_reg_field WCM_ACTION_ACTION0[24][0:1023];
	rand uvm_reg_field ACTION0[24][0:1023];
	uvm_reg_field WCM_TCAM_CFG_RSVD0[20][0:63];
	rand uvm_reg_field WCM_TCAM_CFG_CHUNK_MASK[20][0:63];
	rand uvm_reg_field CHUNK_MASK[20][0:63];
	rand uvm_reg_field WCM_TCAM_CFG_START_COMPARE[20][0:63];
	rand uvm_reg_field START_COMPARE[20][0:63];
	rand uvm_reg_field WCM_TCAM_CFG_START_SET[20][0:63];
	rand uvm_reg_field START_SET[20][0:63];
	rand uvm_reg_field WCM_TCAM_CFG_SELECT_TOP[20][0:63];
	rand uvm_reg_field SELECT_TOP[20][0:63];
	rand uvm_reg_field WCM_TCAM_CFG_SELECT0[20][0:63];
	rand uvm_reg_field SELECT0[20][0:63];
	rand uvm_reg_field WCM_TCAM_CFG_SELECT1[20][0:63];
	rand uvm_reg_field SELECT1[20][0:63];
	rand uvm_reg_field WCM_TCAM_CFG_SELECT2[20][0:63];
	rand uvm_reg_field SELECT2[20][0:63];
	rand uvm_reg_field WCM_TCAM_CFG_SELECT3[20][0:63];
	rand uvm_reg_field SELECT3[20][0:63];
	uvm_reg_field WCM_ACTION_CFG_EN_RSVD0[64];
	rand uvm_reg_field WCM_ACTION_CFG_EN_ENABLE[64];
	rand uvm_reg_field ENABLE[64];
	uvm_reg_field WCM_ACTION_CFG_RSVD0[64][0:1];
	rand uvm_reg_field WCM_ACTION_CFG_INDEX[64][0:1];
	rand uvm_reg_field INDEX[64][0:1];
	uvm_reg_field WCM_IP_RSVD0;
	rand uvm_reg_field WCM_IP_HASH_ENTRY_RAM_C_ERR;
	rand uvm_reg_field WCM_IP_HASH_ENTRY_RAM_U_ERR;
	rand uvm_reg_field WCM_IP_FGHASH_ERR;
	rand uvm_reg_field WCM_IP_FGRP_ERR;
	rand uvm_reg_field WCM_IP_TCAM_SWEEP_ERR;
	rand uvm_reg_field WCM_IP_FCMN_SHELL_CTRL_ERR;
	uvm_reg_field WCM_IM_RSVD0;
	rand uvm_reg_field WCM_IM_HASH_ENTRY_RAM_C_ERR;
	rand uvm_reg_field WCM_IM_HASH_ENTRY_RAM_U_ERR;
	rand uvm_reg_field WCM_IM_FGHASH_ERR;
	rand uvm_reg_field WCM_IM_FGRP_ERR;
	rand uvm_reg_field WCM_IM_TCAM_SWEEP_ERR;
	rand uvm_reg_field WCM_IM_FCMN_SHELL_CTRL_ERR;

	function new(string name = "B");
		super.new(name, build_coverage(UVM_NO_COVERAGE));
	endfunction: new

   virtual function void build();
      this.default_map = create_map("", 0, 8, UVM_LITTLE_ENDIAN, 0);
      foreach (this.EM_HASH_LOOKUP[i]) begin
         int J = i;
         this.EM_HASH_LOOKUP[J] = ral_reg_B_EM_HASH_LOOKUP::type_id::create($psprintf("EM_HASH_LOOKUP[%0d]",J),,get_full_name());
         this.EM_HASH_LOOKUP[J].configure(this, null, "");
         this.EM_HASH_LOOKUP[J].build();
         this.EM_HASH_LOOKUP[J].add_hdl_path('{

            '{$psprintf("EM_HASH_LOOKUP[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.EM_HASH_LOOKUP[J], `UVM_REG_ADDR_WIDTH'h0+J*`UVM_REG_ADDR_WIDTH'h10, "RW", 0);
			this.EM_HASH_LOOKUP_RSVD0[J] = this.EM_HASH_LOOKUP[J].RSVD0;
			this.EM_HASH_LOOKUP_PTR[J] = this.EM_HASH_LOOKUP[J].PTR;
			this.PTR[J] = this.EM_HASH_LOOKUP[J].PTR;
			this.EM_HASH_LOOKUP_RSVD1_[J] = this.EM_HASH_LOOKUP[J].RSVD1_;
			this.RSVD1_[J] = this.EM_HASH_LOOKUP[J].RSVD1_;
			this.EM_HASH_LOOKUP_SELECT_4[J] = this.EM_HASH_LOOKUP[J].SELECT_4;
			this.SELECT_4[J] = this.EM_HASH_LOOKUP[J].SELECT_4;
			this.EM_HASH_LOOKUP_SELECT_3[J] = this.EM_HASH_LOOKUP[J].SELECT_3;
			this.SELECT_3[J] = this.EM_HASH_LOOKUP[J].SELECT_3;
			this.EM_HASH_LOOKUP_SELECT_2[J] = this.EM_HASH_LOOKUP[J].SELECT_2;
			this.SELECT_2[J] = this.EM_HASH_LOOKUP[J].SELECT_2;
			this.EM_HASH_LOOKUP_SELECT_1[J] = this.EM_HASH_LOOKUP[J].SELECT_1;
			this.SELECT_1[J] = this.EM_HASH_LOOKUP[J].SELECT_1;
			this.EM_HASH_LOOKUP_SELECT_0[J] = this.EM_HASH_LOOKUP[J].SELECT_0;
			this.SELECT_0[J] = this.EM_HASH_LOOKUP[J].SELECT_0;
			this.EM_HASH_LOOKUP_MASK[J] = this.EM_HASH_LOOKUP[J].MASK;
			this.MASK[J] = this.EM_HASH_LOOKUP[J].MASK;
      end
      foreach (this.WCM_TCAM[i,j]) begin
         int J = i;
         int K = j;
         this.WCM_TCAM[J][K] = ral_reg_B_WCM_TCAM::type_id::create($psprintf("WCM_TCAM[%0d][%0d]",J,K),,get_full_name());
         this.WCM_TCAM[J][K].configure(this, null, "");
         this.WCM_TCAM[J][K].build();
         this.WCM_TCAM[J][K].add_hdl_path('{

            '{$psprintf("WCM_TCAM[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.WCM_TCAM[J][K], `UVM_REG_ADDR_WIDTH'h80000+J*`UVM_REG_ADDR_WIDTH'h4000+K*`UVM_REG_ADDR_WIDTH'h10, "RW", 0);
			this.WCM_TCAM__RSVD1_[J][K] = this.WCM_TCAM[J][K]._RSVD1_;
			this._RSVD1_[J][K] = this.WCM_TCAM[J][K]._RSVD1_;
			this.WCM_TCAM_KEY_TOP_INVERT[J][K] = this.WCM_TCAM[J][K].KEY_TOP_INVERT;
			this.KEY_TOP_INVERT[J][K] = this.WCM_TCAM[J][K].KEY_TOP_INVERT;
			this.WCM_TCAM_KEY_INVERT[J][K] = this.WCM_TCAM[J][K].KEY_INVERT;
			this.KEY_INVERT[J][K] = this.WCM_TCAM[J][K].KEY_INVERT;
			this.WCM_TCAM__RSVD0_[J][K] = this.WCM_TCAM[J][K]._RSVD0_;
			this._RSVD0_[J][K] = this.WCM_TCAM[J][K]._RSVD0_;
			this.WCM_TCAM_KEY_TOP[J][K] = this.WCM_TCAM[J][K].KEY_TOP;
			this.KEY_TOP[J][K] = this.WCM_TCAM[J][K].KEY_TOP;
			this.WCM_TCAM_KEY[J][K] = this.WCM_TCAM[J][K].KEY;
			this.KEY[J][K] = this.WCM_TCAM[J][K].KEY;
      end
      foreach (this.WCM_ACTION[i,j]) begin
         int J = i;
         int K = j;
         this.WCM_ACTION[J][K] = ral_reg_B_WCM_ACTION::type_id::create($psprintf("WCM_ACTION[%0d][%0d]",J,K),,get_full_name());
         this.WCM_ACTION[J][K].configure(this, null, "");
         this.WCM_ACTION[J][K].build();
         this.WCM_ACTION[J][K].add_hdl_path('{

            '{$psprintf("WCM_ACTION[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.WCM_ACTION[J][K], `UVM_REG_ADDR_WIDTH'h100000+J*`UVM_REG_ADDR_WIDTH'h2000+K*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.WCM_ACTION_ACTION1[J][K] = this.WCM_ACTION[J][K].ACTION1;
			this.ACTION1[J][K] = this.WCM_ACTION[J][K].ACTION1;
			this.WCM_ACTION_ACTION0[J][K] = this.WCM_ACTION[J][K].ACTION0;
			this.ACTION0[J][K] = this.WCM_ACTION[J][K].ACTION0;
      end
      foreach (this.WCM_TCAM_CFG[i,j]) begin
         int J = i;
         int K = j;
         this.WCM_TCAM_CFG[J][K] = ral_reg_B_WCM_TCAM_CFG::type_id::create($psprintf("WCM_TCAM_CFG[%0d][%0d]",J,K),,get_full_name());
         this.WCM_TCAM_CFG[J][K].configure(this, null, "");
         this.WCM_TCAM_CFG[J][K].build();
         this.WCM_TCAM_CFG[J][K].add_hdl_path('{

            '{$psprintf("WCM_TCAM_CFG[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.WCM_TCAM_CFG[J][K], `UVM_REG_ADDR_WIDTH'h130000+J*`UVM_REG_ADDR_WIDTH'h200+K*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.WCM_TCAM_CFG_RSVD0[J][K] = this.WCM_TCAM_CFG[J][K].RSVD0;
			this.WCM_TCAM_CFG_CHUNK_MASK[J][K] = this.WCM_TCAM_CFG[J][K].CHUNK_MASK;
			this.CHUNK_MASK[J][K] = this.WCM_TCAM_CFG[J][K].CHUNK_MASK;
			this.WCM_TCAM_CFG_START_COMPARE[J][K] = this.WCM_TCAM_CFG[J][K].START_COMPARE;
			this.START_COMPARE[J][K] = this.WCM_TCAM_CFG[J][K].START_COMPARE;
			this.WCM_TCAM_CFG_START_SET[J][K] = this.WCM_TCAM_CFG[J][K].START_SET;
			this.START_SET[J][K] = this.WCM_TCAM_CFG[J][K].START_SET;
			this.WCM_TCAM_CFG_SELECT_TOP[J][K] = this.WCM_TCAM_CFG[J][K].SELECT_TOP;
			this.SELECT_TOP[J][K] = this.WCM_TCAM_CFG[J][K].SELECT_TOP;
			this.WCM_TCAM_CFG_SELECT0[J][K] = this.WCM_TCAM_CFG[J][K].SELECT0;
			this.SELECT0[J][K] = this.WCM_TCAM_CFG[J][K].SELECT0;
			this.WCM_TCAM_CFG_SELECT1[J][K] = this.WCM_TCAM_CFG[J][K].SELECT1;
			this.SELECT1[J][K] = this.WCM_TCAM_CFG[J][K].SELECT1;
			this.WCM_TCAM_CFG_SELECT2[J][K] = this.WCM_TCAM_CFG[J][K].SELECT2;
			this.SELECT2[J][K] = this.WCM_TCAM_CFG[J][K].SELECT2;
			this.WCM_TCAM_CFG_SELECT3[J][K] = this.WCM_TCAM_CFG[J][K].SELECT3;
			this.SELECT3[J][K] = this.WCM_TCAM_CFG[J][K].SELECT3;
      end
      foreach (this.WCM_ACTION_CFG_EN[i]) begin
         int J = i;
         this.WCM_ACTION_CFG_EN[J] = ral_reg_B_WCM_ACTION_CFG_EN::type_id::create($psprintf("WCM_ACTION_CFG_EN[%0d]",J),,get_full_name());
         this.WCM_ACTION_CFG_EN[J].configure(this, null, "");
         this.WCM_ACTION_CFG_EN[J].build();
         this.WCM_ACTION_CFG_EN[J].add_hdl_path('{

            '{$psprintf("WCM_ACTION_CFG_EN[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.WCM_ACTION_CFG_EN[J], `UVM_REG_ADDR_WIDTH'h132800+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.WCM_ACTION_CFG_EN_RSVD0[J] = this.WCM_ACTION_CFG_EN[J].RSVD0;
			this.WCM_ACTION_CFG_EN_ENABLE[J] = this.WCM_ACTION_CFG_EN[J].ENABLE;
			this.ENABLE[J] = this.WCM_ACTION_CFG_EN[J].ENABLE;
      end
      foreach (this.WCM_ACTION_CFG[i,j]) begin
         int J = i;
         int K = j;
         this.WCM_ACTION_CFG[J][K] = ral_reg_B_WCM_ACTION_CFG::type_id::create($psprintf("WCM_ACTION_CFG[%0d][%0d]",J,K),,get_full_name());
         this.WCM_ACTION_CFG[J][K].configure(this, null, "");
         this.WCM_ACTION_CFG[J][K].build();
         this.WCM_ACTION_CFG[J][K].add_hdl_path('{

            '{$psprintf("WCM_ACTION_CFG[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.WCM_ACTION_CFG[J][K], `UVM_REG_ADDR_WIDTH'h132C00+J*`UVM_REG_ADDR_WIDTH'h10+K*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.WCM_ACTION_CFG_RSVD0[J][K] = this.WCM_ACTION_CFG[J][K].RSVD0;
			this.WCM_ACTION_CFG_INDEX[J][K] = this.WCM_ACTION_CFG[J][K].INDEX;
			this.INDEX[J][K] = this.WCM_ACTION_CFG[J][K].INDEX;
      end
      this.WCM_IP = ral_reg_B_WCM_IP::type_id::create("WCM_IP",,get_full_name());
      this.WCM_IP.configure(this, null, "");
      this.WCM_IP.build();
         this.WCM_IP.add_hdl_path('{

            '{"WCM_IP", -1, -1}
         });
      this.default_map.add_reg(this.WCM_IP, `UVM_REG_ADDR_WIDTH'h133000, "RW", 0);
		this.WCM_IP_RSVD0 = this.WCM_IP.RSVD0;
		this.WCM_IP_HASH_ENTRY_RAM_C_ERR = this.WCM_IP.HASH_ENTRY_RAM_C_ERR;
		this.WCM_IP_HASH_ENTRY_RAM_U_ERR = this.WCM_IP.HASH_ENTRY_RAM_U_ERR;
		this.WCM_IP_FGHASH_ERR = this.WCM_IP.FGHASH_ERR;
		this.WCM_IP_FGRP_ERR = this.WCM_IP.FGRP_ERR;
		this.WCM_IP_TCAM_SWEEP_ERR = this.WCM_IP.TCAM_SWEEP_ERR;
		this.WCM_IP_FCMN_SHELL_CTRL_ERR = this.WCM_IP.FCMN_SHELL_CTRL_ERR;
      this.WCM_IM = ral_reg_B_WCM_IM::type_id::create("WCM_IM",,get_full_name());
      this.WCM_IM.configure(this, null, "");
      this.WCM_IM.build();
         this.WCM_IM.add_hdl_path('{

            '{"WCM_IM", -1, -1}
         });
      this.default_map.add_reg(this.WCM_IM, `UVM_REG_ADDR_WIDTH'h133008, "RW", 0);
		this.WCM_IM_RSVD0 = this.WCM_IM.RSVD0;
		this.WCM_IM_HASH_ENTRY_RAM_C_ERR = this.WCM_IM.HASH_ENTRY_RAM_C_ERR;
		this.WCM_IM_HASH_ENTRY_RAM_U_ERR = this.WCM_IM.HASH_ENTRY_RAM_U_ERR;
		this.WCM_IM_FGHASH_ERR = this.WCM_IM.FGHASH_ERR;
		this.WCM_IM_FGRP_ERR = this.WCM_IM.FGRP_ERR;
		this.WCM_IM_TCAM_SWEEP_ERR = this.WCM_IM.TCAM_SWEEP_ERR;
		this.WCM_IM_FCMN_SHELL_CTRL_ERR = this.WCM_IM.FCMN_SHELL_CTRL_ERR;
   endfunction : build

	`uvm_object_utils(ral_block_B)

endclass : ral_block_B



`endif
