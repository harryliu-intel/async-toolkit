`ifndef RAL_ENTROPY
`define RAL_ENTROPY

import uvm_pkg::*;

class ral_reg_entropy_ENTROPY_HASH_CFG0 extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field KEY_MASK8;

	function new(string name = "entropy_ENTROPY_HASH_CFG0");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 32, 32, "RO", 0, 32'h0, 1, 0, 1);
      this.KEY_MASK8 = uvm_reg_field::type_id::create("KEY_MASK8",,get_full_name());
      this.KEY_MASK8.configure(this, 32, 0, "RW", 0, 32'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_entropy_ENTROPY_HASH_CFG0)

endclass : ral_reg_entropy_ENTROPY_HASH_CFG0


class ral_reg_entropy_ENTROPY_HASH_CFG1 extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field SYMMETRIC;
	rand uvm_reg_field SYM_PROFILE;
	rand uvm_reg_field KEY_MASK_PROFILE;
	rand uvm_reg_field KEY_MASK32;
	rand uvm_reg_field KEY_MASK16;

	function new(string name = "entropy_ENTROPY_HASH_CFG1");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 9, 55, "RO", 0, 9'h0, 1, 0, 0);
      this.SYMMETRIC = uvm_reg_field::type_id::create("SYMMETRIC",,get_full_name());
      this.SYMMETRIC.configure(this, 1, 54, "RW", 0, 1'h0, 1, 0, 0);
      this.SYM_PROFILE = uvm_reg_field::type_id::create("SYM_PROFILE",,get_full_name());
      this.SYM_PROFILE.configure(this, 2, 52, "RW", 0, 2'h0, 1, 0, 0);
      this.KEY_MASK_PROFILE = uvm_reg_field::type_id::create("KEY_MASK_PROFILE",,get_full_name());
      this.KEY_MASK_PROFILE.configure(this, 4, 48, "RW", 0, 4'h0, 1, 0, 0);
      this.KEY_MASK32 = uvm_reg_field::type_id::create("KEY_MASK32",,get_full_name());
      this.KEY_MASK32.configure(this, 16, 32, "RW", 0, 16'h0, 1, 0, 1);
      this.KEY_MASK16 = uvm_reg_field::type_id::create("KEY_MASK16",,get_full_name());
      this.KEY_MASK16.configure(this, 32, 0, "RW", 0, 32'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_entropy_ENTROPY_HASH_CFG1)

endclass : ral_reg_entropy_ENTROPY_HASH_CFG1


class ral_reg_entropy_ENTROPY_HASH_CFG2 extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field SYMMETRIC;
	rand uvm_reg_field SYM_PROFILE;
	rand uvm_reg_field KEY_MASK_PROFILE;
	rand uvm_reg_field KEY_MASK32;
	rand uvm_reg_field KEY_MASK16;

	function new(string name = "entropy_ENTROPY_HASH_CFG2");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 9, 55, "RO", 0, 9'h0, 1, 0, 0);
      this.SYMMETRIC = uvm_reg_field::type_id::create("SYMMETRIC",,get_full_name());
      this.SYMMETRIC.configure(this, 1, 54, "RW", 0, 1'h0, 1, 0, 0);
      this.SYM_PROFILE = uvm_reg_field::type_id::create("SYM_PROFILE",,get_full_name());
      this.SYM_PROFILE.configure(this, 2, 52, "RW", 0, 2'h0, 1, 0, 0);
      this.KEY_MASK_PROFILE = uvm_reg_field::type_id::create("KEY_MASK_PROFILE",,get_full_name());
      this.KEY_MASK_PROFILE.configure(this, 4, 48, "RW", 0, 4'h0, 1, 0, 0);
      this.KEY_MASK32 = uvm_reg_field::type_id::create("KEY_MASK32",,get_full_name());
      this.KEY_MASK32.configure(this, 16, 32, "RW", 0, 16'h0, 1, 0, 1);
      this.KEY_MASK16 = uvm_reg_field::type_id::create("KEY_MASK16",,get_full_name());
      this.KEY_MASK16.configure(this, 32, 0, "RW", 0, 32'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_entropy_ENTROPY_HASH_CFG2)

endclass : ral_reg_entropy_ENTROPY_HASH_CFG2


class ral_reg_entropy_ENTROPY_META_CFG extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field BYTE_DEFAULTS;
	rand uvm_reg_field HASH_START;
	rand uvm_reg_field HASH_SIZE;

	function new(string name = "entropy_ENTROPY_META_CFG");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 40, 24, "RO", 0, 40'h0, 1, 0, 1);
      this.BYTE_DEFAULTS = uvm_reg_field::type_id::create("BYTE_DEFAULTS",,get_full_name());
      this.BYTE_DEFAULTS.configure(this, 12, 12, "RW", 0, 12'h0, 1, 0, 0);
      this.HASH_START = uvm_reg_field::type_id::create("HASH_START",,get_full_name());
      this.HASH_START.configure(this, 6, 6, "RW", 0, 6'h0, 1, 0, 0);
      this.HASH_SIZE = uvm_reg_field::type_id::create("HASH_SIZE",,get_full_name());
      this.HASH_SIZE.configure(this, 6, 0, "RW", 0, 6'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_entropy_ENTROPY_META_CFG)

endclass : ral_reg_entropy_ENTROPY_META_CFG


class ral_reg_entropy_ENTROPY_HASH_SYM8 extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field KEY_PAIRS;

	function new(string name = "entropy_ENTROPY_HASH_SYM8");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 4, 60, "RO", 0, 4'h0, 1, 0, 0);
      this.KEY_PAIRS = uvm_reg_field::type_id::create("KEY_PAIRS",,get_full_name());
      this.KEY_PAIRS.configure(this, 60, 0, "RW", 0, 60'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_entropy_ENTROPY_HASH_SYM8)

endclass : ral_reg_entropy_ENTROPY_HASH_SYM8


class ral_reg_entropy_ENTROPY_HASH_SYM16 extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field KEY_PAIRS;

	function new(string name = "entropy_ENTROPY_HASH_SYM16");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 4, 60, "RO", 0, 4'h0, 1, 0, 0);
      this.KEY_PAIRS = uvm_reg_field::type_id::create("KEY_PAIRS",,get_full_name());
      this.KEY_PAIRS.configure(this, 60, 0, "RW", 0, 60'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_entropy_ENTROPY_HASH_SYM16)

endclass : ral_reg_entropy_ENTROPY_HASH_SYM16


class ral_reg_entropy_ENTROPY_HASH_SYM32 extends uvm_reg;
	rand uvm_reg_field KEY_PAIRS;

	function new(string name = "entropy_ENTROPY_HASH_SYM32");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.KEY_PAIRS = uvm_reg_field::type_id::create("KEY_PAIRS",,get_full_name());
      this.KEY_PAIRS.configure(this, 64, 0, "RW", 0, 64'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_entropy_ENTROPY_HASH_SYM32)

endclass : ral_reg_entropy_ENTROPY_HASH_SYM32


class ral_reg_entropy_ENTROPY_HASH_KEY_MASK extends uvm_reg;
	rand uvm_reg_field MASK;

	function new(string name = "entropy_ENTROPY_HASH_KEY_MASK");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.MASK = uvm_reg_field::type_id::create("MASK",,get_full_name());
      this.MASK.configure(this, 64, 0, "RW", 0, 64'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_entropy_ENTROPY_HASH_KEY_MASK)

endclass : ral_reg_entropy_ENTROPY_HASH_KEY_MASK


class ral_reg_entropy_ENTROPY_FWD_HASHING_CFG extends uvm_reg;
	uvm_reg_field _RSVD1_;
	rand uvm_reg_field ECMP_ROTATION;
	rand uvm_reg_field ROTATION_B;
	rand uvm_reg_field ROTATION_A;
	uvm_reg_field _RSVD0_;

	function new(string name = "entropy_ENTROPY_FWD_HASHING_CFG");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this._RSVD1_ = uvm_reg_field::type_id::create("_RSVD1_",,get_full_name());
      this._RSVD1_.configure(this, 51, 13, "RO", 1, 51'h0, 1, 0, 0);
      this.ECMP_ROTATION = uvm_reg_field::type_id::create("ECMP_ROTATION",,get_full_name());
      this.ECMP_ROTATION.configure(this, 1, 12, "RW", 0, 1'h0, 1, 0, 0);
      this.ROTATION_B = uvm_reg_field::type_id::create("ROTATION_B",,get_full_name());
      this.ROTATION_B.configure(this, 2, 10, "RW", 0, 2'h1, 1, 0, 0);
      this.ROTATION_A = uvm_reg_field::type_id::create("ROTATION_A",,get_full_name());
      this.ROTATION_A.configure(this, 2, 8, "RW", 0, 2'h0, 1, 0, 0);
      this._RSVD0_ = uvm_reg_field::type_id::create("_RSVD0_",,get_full_name());
      this._RSVD0_.configure(this, 8, 0, "RO", 1, 8'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_entropy_ENTROPY_FWD_HASHING_CFG)

endclass : ral_reg_entropy_ENTROPY_FWD_HASHING_CFG


class ral_block_entropy extends uvm_reg_block;
	rand ral_reg_entropy_ENTROPY_HASH_CFG0 ENTROPY_HASH_CFG0[3][0:63];
	rand ral_reg_entropy_ENTROPY_HASH_CFG1 ENTROPY_HASH_CFG1[3][0:63];
	rand ral_reg_entropy_ENTROPY_HASH_CFG2 ENTROPY_HASH_CFG2[3][0:63];
	rand ral_reg_entropy_ENTROPY_META_CFG ENTROPY_META_CFG[64];
	rand ral_reg_entropy_ENTROPY_HASH_SYM8 ENTROPY_HASH_SYM8[3][0:3];
	rand ral_reg_entropy_ENTROPY_HASH_SYM16 ENTROPY_HASH_SYM16[3][0:3];
	rand ral_reg_entropy_ENTROPY_HASH_SYM32 ENTROPY_HASH_SYM32[3][0:3];
	rand ral_reg_entropy_ENTROPY_HASH_KEY_MASK ENTROPY_HASH_KEY_MASK[3][0:127];
	rand ral_reg_entropy_ENTROPY_FWD_HASHING_CFG ENTROPY_FWD_HASHING_CFG[64];
	uvm_reg_field ENTROPY_HASH_CFG0_RSVD0[3][0:63];
	rand uvm_reg_field ENTROPY_HASH_CFG0_KEY_MASK8[3][0:63];
	rand uvm_reg_field KEY_MASK8[3][0:63];
	uvm_reg_field ENTROPY_HASH_CFG1_RSVD0[3][0:63];
	rand uvm_reg_field ENTROPY_HASH_CFG1_SYMMETRIC[3][0:63];
	rand uvm_reg_field ENTROPY_HASH_CFG1_SYM_PROFILE[3][0:63];
	rand uvm_reg_field ENTROPY_HASH_CFG1_KEY_MASK_PROFILE[3][0:63];
	rand uvm_reg_field ENTROPY_HASH_CFG1_KEY_MASK32[3][0:63];
	rand uvm_reg_field ENTROPY_HASH_CFG1_KEY_MASK16[3][0:63];
	uvm_reg_field ENTROPY_HASH_CFG2_RSVD0[3][0:63];
	rand uvm_reg_field ENTROPY_HASH_CFG2_SYMMETRIC[3][0:63];
	rand uvm_reg_field ENTROPY_HASH_CFG2_SYM_PROFILE[3][0:63];
	rand uvm_reg_field ENTROPY_HASH_CFG2_KEY_MASK_PROFILE[3][0:63];
	rand uvm_reg_field ENTROPY_HASH_CFG2_KEY_MASK32[3][0:63];
	rand uvm_reg_field ENTROPY_HASH_CFG2_KEY_MASK16[3][0:63];
	uvm_reg_field ENTROPY_META_CFG_RSVD0[64];
	rand uvm_reg_field ENTROPY_META_CFG_BYTE_DEFAULTS[64];
	rand uvm_reg_field BYTE_DEFAULTS[64];
	rand uvm_reg_field ENTROPY_META_CFG_HASH_START[64];
	rand uvm_reg_field HASH_START[64];
	rand uvm_reg_field ENTROPY_META_CFG_HASH_SIZE[64];
	rand uvm_reg_field HASH_SIZE[64];
	uvm_reg_field ENTROPY_HASH_SYM8_RSVD0[3][0:3];
	rand uvm_reg_field ENTROPY_HASH_SYM8_KEY_PAIRS[3][0:3];
	uvm_reg_field ENTROPY_HASH_SYM16_RSVD0[3][0:3];
	rand uvm_reg_field ENTROPY_HASH_SYM16_KEY_PAIRS[3][0:3];
	rand uvm_reg_field ENTROPY_HASH_SYM32_KEY_PAIRS[3][0:3];
	rand uvm_reg_field ENTROPY_HASH_KEY_MASK_MASK[3][0:127];
	rand uvm_reg_field MASK[3][0:127];
	uvm_reg_field ENTROPY_FWD_HASHING_CFG__RSVD1_[64];
	uvm_reg_field _RSVD1_[64];
	rand uvm_reg_field ENTROPY_FWD_HASHING_CFG_ECMP_ROTATION[64];
	rand uvm_reg_field ECMP_ROTATION[64];
	rand uvm_reg_field ENTROPY_FWD_HASHING_CFG_ROTATION_B[64];
	rand uvm_reg_field ROTATION_B[64];
	rand uvm_reg_field ENTROPY_FWD_HASHING_CFG_ROTATION_A[64];
	rand uvm_reg_field ROTATION_A[64];
	uvm_reg_field ENTROPY_FWD_HASHING_CFG__RSVD0_[64];
	uvm_reg_field _RSVD0_[64];

	function new(string name = "entropy");
		super.new(name, build_coverage(UVM_NO_COVERAGE));
	endfunction: new

   virtual function void build();
      this.default_map = create_map("", 0, 8, UVM_LITTLE_ENDIAN, 0);
      foreach (this.ENTROPY_HASH_CFG0[i,j]) begin
         int J = i;
         int K = j;
         this.ENTROPY_HASH_CFG0[J][K] = ral_reg_entropy_ENTROPY_HASH_CFG0::type_id::create($psprintf("ENTROPY_HASH_CFG0[%0d][%0d]",J,K),,get_full_name());
         this.ENTROPY_HASH_CFG0[J][K].configure(this, null, "");
         this.ENTROPY_HASH_CFG0[J][K].build();
         this.ENTROPY_HASH_CFG0[J][K].add_hdl_path('{

            '{$psprintf("ENTROPY_HASH_CFG0[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.ENTROPY_HASH_CFG0[J][K], `UVM_REG_ADDR_WIDTH'h0+J*`UVM_REG_ADDR_WIDTH'h200+K*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.ENTROPY_HASH_CFG0_RSVD0[J][K] = this.ENTROPY_HASH_CFG0[J][K].RSVD0;
			this.ENTROPY_HASH_CFG0_KEY_MASK8[J][K] = this.ENTROPY_HASH_CFG0[J][K].KEY_MASK8;
			this.KEY_MASK8[J][K] = this.ENTROPY_HASH_CFG0[J][K].KEY_MASK8;
      end
      foreach (this.ENTROPY_HASH_CFG1[i,j]) begin
         int J = i;
         int K = j;
         this.ENTROPY_HASH_CFG1[J][K] = ral_reg_entropy_ENTROPY_HASH_CFG1::type_id::create($psprintf("ENTROPY_HASH_CFG1[%0d][%0d]",J,K),,get_full_name());
         this.ENTROPY_HASH_CFG1[J][K].configure(this, null, "");
         this.ENTROPY_HASH_CFG1[J][K].build();
         this.ENTROPY_HASH_CFG1[J][K].add_hdl_path('{

            '{$psprintf("ENTROPY_HASH_CFG1[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.ENTROPY_HASH_CFG1[J][K], `UVM_REG_ADDR_WIDTH'h800+J*`UVM_REG_ADDR_WIDTH'h200+K*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.ENTROPY_HASH_CFG1_RSVD0[J][K] = this.ENTROPY_HASH_CFG1[J][K].RSVD0;
			this.ENTROPY_HASH_CFG1_SYMMETRIC[J][K] = this.ENTROPY_HASH_CFG1[J][K].SYMMETRIC;
			this.ENTROPY_HASH_CFG1_SYM_PROFILE[J][K] = this.ENTROPY_HASH_CFG1[J][K].SYM_PROFILE;
			this.ENTROPY_HASH_CFG1_KEY_MASK_PROFILE[J][K] = this.ENTROPY_HASH_CFG1[J][K].KEY_MASK_PROFILE;
			this.ENTROPY_HASH_CFG1_KEY_MASK32[J][K] = this.ENTROPY_HASH_CFG1[J][K].KEY_MASK32;
			this.ENTROPY_HASH_CFG1_KEY_MASK16[J][K] = this.ENTROPY_HASH_CFG1[J][K].KEY_MASK16;
      end
      foreach (this.ENTROPY_HASH_CFG2[i,j]) begin
         int J = i;
         int K = j;
         this.ENTROPY_HASH_CFG2[J][K] = ral_reg_entropy_ENTROPY_HASH_CFG2::type_id::create($psprintf("ENTROPY_HASH_CFG2[%0d][%0d]",J,K),,get_full_name());
         this.ENTROPY_HASH_CFG2[J][K].configure(this, null, "");
         this.ENTROPY_HASH_CFG2[J][K].build();
         this.ENTROPY_HASH_CFG2[J][K].add_hdl_path('{

            '{$psprintf("ENTROPY_HASH_CFG2[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.ENTROPY_HASH_CFG2[J][K], `UVM_REG_ADDR_WIDTH'h1000+J*`UVM_REG_ADDR_WIDTH'h200+K*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.ENTROPY_HASH_CFG2_RSVD0[J][K] = this.ENTROPY_HASH_CFG2[J][K].RSVD0;
			this.ENTROPY_HASH_CFG2_SYMMETRIC[J][K] = this.ENTROPY_HASH_CFG2[J][K].SYMMETRIC;
			this.ENTROPY_HASH_CFG2_SYM_PROFILE[J][K] = this.ENTROPY_HASH_CFG2[J][K].SYM_PROFILE;
			this.ENTROPY_HASH_CFG2_KEY_MASK_PROFILE[J][K] = this.ENTROPY_HASH_CFG2[J][K].KEY_MASK_PROFILE;
			this.ENTROPY_HASH_CFG2_KEY_MASK32[J][K] = this.ENTROPY_HASH_CFG2[J][K].KEY_MASK32;
			this.ENTROPY_HASH_CFG2_KEY_MASK16[J][K] = this.ENTROPY_HASH_CFG2[J][K].KEY_MASK16;
      end
      foreach (this.ENTROPY_META_CFG[i]) begin
         int J = i;
         this.ENTROPY_META_CFG[J] = ral_reg_entropy_ENTROPY_META_CFG::type_id::create($psprintf("ENTROPY_META_CFG[%0d]",J),,get_full_name());
         this.ENTROPY_META_CFG[J].configure(this, null, "");
         this.ENTROPY_META_CFG[J].build();
         this.ENTROPY_META_CFG[J].add_hdl_path('{

            '{$psprintf("ENTROPY_META_CFG[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.ENTROPY_META_CFG[J], `UVM_REG_ADDR_WIDTH'h1600+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.ENTROPY_META_CFG_RSVD0[J] = this.ENTROPY_META_CFG[J].RSVD0;
			this.ENTROPY_META_CFG_BYTE_DEFAULTS[J] = this.ENTROPY_META_CFG[J].BYTE_DEFAULTS;
			this.BYTE_DEFAULTS[J] = this.ENTROPY_META_CFG[J].BYTE_DEFAULTS;
			this.ENTROPY_META_CFG_HASH_START[J] = this.ENTROPY_META_CFG[J].HASH_START;
			this.HASH_START[J] = this.ENTROPY_META_CFG[J].HASH_START;
			this.ENTROPY_META_CFG_HASH_SIZE[J] = this.ENTROPY_META_CFG[J].HASH_SIZE;
			this.HASH_SIZE[J] = this.ENTROPY_META_CFG[J].HASH_SIZE;
      end
      foreach (this.ENTROPY_HASH_SYM8[i,j]) begin
         int J = i;
         int K = j;
         this.ENTROPY_HASH_SYM8[J][K] = ral_reg_entropy_ENTROPY_HASH_SYM8::type_id::create($psprintf("ENTROPY_HASH_SYM8[%0d][%0d]",J,K),,get_full_name());
         this.ENTROPY_HASH_SYM8[J][K].configure(this, null, "");
         this.ENTROPY_HASH_SYM8[J][K].build();
         this.ENTROPY_HASH_SYM8[J][K].add_hdl_path('{

            '{$psprintf("ENTROPY_HASH_SYM8[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.ENTROPY_HASH_SYM8[J][K], `UVM_REG_ADDR_WIDTH'h1800+J*`UVM_REG_ADDR_WIDTH'h20+K*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.ENTROPY_HASH_SYM8_RSVD0[J][K] = this.ENTROPY_HASH_SYM8[J][K].RSVD0;
			this.ENTROPY_HASH_SYM8_KEY_PAIRS[J][K] = this.ENTROPY_HASH_SYM8[J][K].KEY_PAIRS;
      end
      foreach (this.ENTROPY_HASH_SYM16[i,j]) begin
         int J = i;
         int K = j;
         this.ENTROPY_HASH_SYM16[J][K] = ral_reg_entropy_ENTROPY_HASH_SYM16::type_id::create($psprintf("ENTROPY_HASH_SYM16[%0d][%0d]",J,K),,get_full_name());
         this.ENTROPY_HASH_SYM16[J][K].configure(this, null, "");
         this.ENTROPY_HASH_SYM16[J][K].build();
         this.ENTROPY_HASH_SYM16[J][K].add_hdl_path('{

            '{$psprintf("ENTROPY_HASH_SYM16[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.ENTROPY_HASH_SYM16[J][K], `UVM_REG_ADDR_WIDTH'h1880+J*`UVM_REG_ADDR_WIDTH'h20+K*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.ENTROPY_HASH_SYM16_RSVD0[J][K] = this.ENTROPY_HASH_SYM16[J][K].RSVD0;
			this.ENTROPY_HASH_SYM16_KEY_PAIRS[J][K] = this.ENTROPY_HASH_SYM16[J][K].KEY_PAIRS;
      end
      foreach (this.ENTROPY_HASH_SYM32[i,j]) begin
         int J = i;
         int K = j;
         this.ENTROPY_HASH_SYM32[J][K] = ral_reg_entropy_ENTROPY_HASH_SYM32::type_id::create($psprintf("ENTROPY_HASH_SYM32[%0d][%0d]",J,K),,get_full_name());
         this.ENTROPY_HASH_SYM32[J][K].configure(this, null, "");
         this.ENTROPY_HASH_SYM32[J][K].build();
         this.ENTROPY_HASH_SYM32[J][K].add_hdl_path('{

            '{$psprintf("ENTROPY_HASH_SYM32[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.ENTROPY_HASH_SYM32[J][K], `UVM_REG_ADDR_WIDTH'h1900+J*`UVM_REG_ADDR_WIDTH'h20+K*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.ENTROPY_HASH_SYM32_KEY_PAIRS[J][K] = this.ENTROPY_HASH_SYM32[J][K].KEY_PAIRS;
      end
      foreach (this.ENTROPY_HASH_KEY_MASK[i,j]) begin
         int J = i;
         int K = j;
         this.ENTROPY_HASH_KEY_MASK[J][K] = ral_reg_entropy_ENTROPY_HASH_KEY_MASK::type_id::create($psprintf("ENTROPY_HASH_KEY_MASK[%0d][%0d]",J,K),,get_full_name());
         this.ENTROPY_HASH_KEY_MASK[J][K].configure(this, null, "");
         this.ENTROPY_HASH_KEY_MASK[J][K].build();
         this.ENTROPY_HASH_KEY_MASK[J][K].add_hdl_path('{

            '{$psprintf("ENTROPY_HASH_KEY_MASK[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.ENTROPY_HASH_KEY_MASK[J][K], `UVM_REG_ADDR_WIDTH'h2000+J*`UVM_REG_ADDR_WIDTH'h400+K*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.ENTROPY_HASH_KEY_MASK_MASK[J][K] = this.ENTROPY_HASH_KEY_MASK[J][K].MASK;
			this.MASK[J][K] = this.ENTROPY_HASH_KEY_MASK[J][K].MASK;
      end
      foreach (this.ENTROPY_FWD_HASHING_CFG[i]) begin
         int J = i;
         this.ENTROPY_FWD_HASHING_CFG[J] = ral_reg_entropy_ENTROPY_FWD_HASHING_CFG::type_id::create($psprintf("ENTROPY_FWD_HASHING_CFG[%0d]",J),,get_full_name());
         this.ENTROPY_FWD_HASHING_CFG[J].configure(this, null, "");
         this.ENTROPY_FWD_HASHING_CFG[J].build();
         this.ENTROPY_FWD_HASHING_CFG[J].add_hdl_path('{

            '{$psprintf("ENTROPY_FWD_HASHING_CFG[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.ENTROPY_FWD_HASHING_CFG[J], `UVM_REG_ADDR_WIDTH'h2C00+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.ENTROPY_FWD_HASHING_CFG__RSVD1_[J] = this.ENTROPY_FWD_HASHING_CFG[J]._RSVD1_;
			this._RSVD1_[J] = this.ENTROPY_FWD_HASHING_CFG[J]._RSVD1_;
			this.ENTROPY_FWD_HASHING_CFG_ECMP_ROTATION[J] = this.ENTROPY_FWD_HASHING_CFG[J].ECMP_ROTATION;
			this.ECMP_ROTATION[J] = this.ENTROPY_FWD_HASHING_CFG[J].ECMP_ROTATION;
			this.ENTROPY_FWD_HASHING_CFG_ROTATION_B[J] = this.ENTROPY_FWD_HASHING_CFG[J].ROTATION_B;
			this.ROTATION_B[J] = this.ENTROPY_FWD_HASHING_CFG[J].ROTATION_B;
			this.ENTROPY_FWD_HASHING_CFG_ROTATION_A[J] = this.ENTROPY_FWD_HASHING_CFG[J].ROTATION_A;
			this.ROTATION_A[J] = this.ENTROPY_FWD_HASHING_CFG[J].ROTATION_A;
			this.ENTROPY_FWD_HASHING_CFG__RSVD0_[J] = this.ENTROPY_FWD_HASHING_CFG[J]._RSVD0_;
			this._RSVD0_[J] = this.ENTROPY_FWD_HASHING_CFG[J]._RSVD0_;
      end
   endfunction : build

	`uvm_object_utils(ral_block_entropy)

endclass : ral_block_entropy



`endif
