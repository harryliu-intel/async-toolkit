`ifndef RAL_A
`define RAL_A

import uvm_pkg::*;

class ral_reg_A_EM_HASH_LOOKUP extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field PTR;
	uvm_reg_field RSVD1_;
	rand uvm_reg_field SELECT_4;
	rand uvm_reg_field SELECT_3;
	rand uvm_reg_field SELECT_2;
	rand uvm_reg_field SELECT_1;
	rand uvm_reg_field SELECT_0;
	rand uvm_reg_field MASK;

	function new(string name = "A_EM_HASH_LOOKUP");
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

	`uvm_object_utils(ral_reg_A_EM_HASH_LOOKUP)

endclass : ral_reg_A_EM_HASH_LOOKUP


class ral_reg_A_LPM_SUBTRIE_APTR extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field ACTION_BASE_PTR;

	function new(string name = "A_LPM_SUBTRIE_APTR");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 45, 19, "RO", 0, 45'h0, 1, 0, 0);
      this.ACTION_BASE_PTR = uvm_reg_field::type_id::create("ACTION_BASE_PTR",,get_full_name());
      this.ACTION_BASE_PTR.configure(this, 19, 0, "RW", 0, 19'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_A_LPM_SUBTRIE_APTR)

endclass : ral_reg_A_LPM_SUBTRIE_APTR


class ral_reg_A_LPM_SUBTRIE_CPTR extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field CHILD_PTR_LEN;
	rand uvm_reg_field CHILD_BASE_PTR;
	rand uvm_reg_field SUBTRIE_PTR;

	function new(string name = "A_LPM_SUBTRIE_CPTR");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 24, 40, "RO", 0, 24'h0, 1, 0, 1);
      this.CHILD_PTR_LEN = uvm_reg_field::type_id::create("CHILD_PTR_LEN",,get_full_name());
      this.CHILD_PTR_LEN.configure(this, 8, 32, "RW", 0, 8'h0, 1, 0, 1);
      this.CHILD_BASE_PTR = uvm_reg_field::type_id::create("CHILD_BASE_PTR",,get_full_name());
      this.CHILD_BASE_PTR.configure(this, 16, 16, "RW", 0, 16'h0, 1, 0, 1);
      this.SUBTRIE_PTR = uvm_reg_field::type_id::create("SUBTRIE_PTR",,get_full_name());
      this.SUBTRIE_PTR.configure(this, 16, 0, "RW", 0, 16'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_A_LPM_SUBTRIE_CPTR)

endclass : ral_reg_A_LPM_SUBTRIE_CPTR


class ral_reg_A_LPM_SUBTRIE_BITMAPS extends uvm_reg;
	rand uvm_reg_field BITMAP;

	function new(string name = "A_LPM_SUBTRIE_BITMAPS");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.BITMAP = uvm_reg_field::type_id::create("BITMAP",,get_full_name());
      this.BITMAP.configure(this, 64, 0, "RW", 0, 64'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_A_LPM_SUBTRIE_BITMAPS)

endclass : ral_reg_A_LPM_SUBTRIE_BITMAPS


class ral_reg_A_LPM_MATCH_TCAM extends uvm_reg;
	rand uvm_reg_field KEY_INVERT;
	rand uvm_reg_field KEY;

	function new(string name = "A_LPM_MATCH_TCAM");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.KEY_INVERT = uvm_reg_field::type_id::create("KEY_INVERT",,get_full_name());
      this.KEY_INVERT.configure(this, 32, 32, "RW", 0, 32'h0, 1, 0, 1);
      this.KEY = uvm_reg_field::type_id::create("KEY",,get_full_name());
      this.KEY.configure(this, 32, 0, "RW", 0, 32'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_A_LPM_MATCH_TCAM)

endclass : ral_reg_A_LPM_MATCH_TCAM


class ral_reg_A_LPM_MATCH_ACTION extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field CHILD_PTR_LEN;
	rand uvm_reg_field CHILD_BASE_PTR;
	rand uvm_reg_field ROOT_PTR;

	function new(string name = "A_LPM_MATCH_ACTION");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 24, 40, "RO", 0, 24'h0, 1, 0, 1);
      this.CHILD_PTR_LEN = uvm_reg_field::type_id::create("CHILD_PTR_LEN",,get_full_name());
      this.CHILD_PTR_LEN.configure(this, 8, 32, "RW", 0, 8'h0, 1, 0, 1);
      this.CHILD_BASE_PTR = uvm_reg_field::type_id::create("CHILD_BASE_PTR",,get_full_name());
      this.CHILD_BASE_PTR.configure(this, 16, 16, "RW", 0, 16'h0, 1, 0, 1);
      this.ROOT_PTR = uvm_reg_field::type_id::create("ROOT_PTR",,get_full_name());
      this.ROOT_PTR.configure(this, 16, 0, "RW", 0, 16'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_A_LPM_MATCH_ACTION)

endclass : ral_reg_A_LPM_MATCH_ACTION


class ral_reg_A_LPM_KEY_MASK extends uvm_reg;
	rand uvm_reg_field MASK;

	function new(string name = "A_LPM_KEY_MASK");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.MASK = uvm_reg_field::type_id::create("MASK",,get_full_name());
      this.MASK.configure(this, 64, 0, "RW", 0, 64'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_A_LPM_KEY_MASK)

endclass : ral_reg_A_LPM_KEY_MASK


class ral_reg_A_LPM_KEY_SEL0 extends uvm_reg;
	rand uvm_reg_field MD_KEY16_SEL;

	function new(string name = "A_LPM_KEY_SEL0");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.MD_KEY16_SEL = uvm_reg_field::type_id::create("MD_KEY16_SEL",,get_full_name());
      this.MD_KEY16_SEL.configure(this, 64, 0, "RW", 0, 64'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_A_LPM_KEY_SEL0)

endclass : ral_reg_A_LPM_KEY_SEL0


class ral_reg_A_LPM_KEY_SEL1 extends uvm_reg;
	rand uvm_reg_field ADDR_KEY8_SEL;
	rand uvm_reg_field MD_KEY8_SEL;

	function new(string name = "A_LPM_KEY_SEL1");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.ADDR_KEY8_SEL = uvm_reg_field::type_id::create("ADDR_KEY8_SEL",,get_full_name());
      this.ADDR_KEY8_SEL.configure(this, 32, 32, "RW", 0, 32'h0, 1, 0, 1);
      this.MD_KEY8_SEL = uvm_reg_field::type_id::create("MD_KEY8_SEL",,get_full_name());
      this.MD_KEY8_SEL.configure(this, 32, 0, "RW", 0, 32'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_A_LPM_KEY_SEL1)

endclass : ral_reg_A_LPM_KEY_SEL1


class ral_reg_A_LPM_KEY_SEL2 extends uvm_reg;
	rand uvm_reg_field ADDR_KEY16_SEL;

	function new(string name = "A_LPM_KEY_SEL2");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.ADDR_KEY16_SEL = uvm_reg_field::type_id::create("ADDR_KEY16_SEL",,get_full_name());
      this.ADDR_KEY16_SEL.configure(this, 64, 0, "RW", 0, 64'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_A_LPM_KEY_SEL2)

endclass : ral_reg_A_LPM_KEY_SEL2


class ral_reg_A_LPM_KEY_SEL3 extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field ADDR_KEY32_SEL;

	function new(string name = "A_LPM_KEY_SEL3");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 48, 16, "RO", 0, 48'h0, 1, 0, 1);
      this.ADDR_KEY32_SEL = uvm_reg_field::type_id::create("ADDR_KEY32_SEL",,get_full_name());
      this.ADDR_KEY32_SEL.configure(this, 16, 0, "RW", 0, 16'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_A_LPM_KEY_SEL3)

endclass : ral_reg_A_LPM_KEY_SEL3


class ral_block_A extends uvm_reg_block;
	rand ral_reg_A_EM_HASH_LOOKUP EM_HASH_LOOKUP[32768];
	rand ral_reg_A_LPM_SUBTRIE_APTR LPM_SUBTRIE_APTR[48][0:511];
	rand ral_reg_A_LPM_SUBTRIE_CPTR LPM_SUBTRIE_CPTR[48][0:511];
	rand ral_reg_A_LPM_SUBTRIE_BITMAPS LPM_SUBTRIE_BITMAPS[48][0:4095];
	rand ral_reg_A_LPM_MATCH_TCAM LPM_MATCH_TCAM[512];
	rand ral_reg_A_LPM_MATCH_ACTION LPM_MATCH_ACTION[512];
	rand ral_reg_A_LPM_KEY_MASK LPM_KEY_MASK[64][0:19];
	rand ral_reg_A_LPM_KEY_SEL0 LPM_KEY_SEL0[64];
	rand ral_reg_A_LPM_KEY_SEL1 LPM_KEY_SEL1[64];
	rand ral_reg_A_LPM_KEY_SEL2 LPM_KEY_SEL2[64];
	rand ral_reg_A_LPM_KEY_SEL3 LPM_KEY_SEL3[64];
	uvm_reg_field EM_HASH_LOOKUP_RSVD0[32768];
	rand uvm_reg_field EM_HASH_LOOKUP_PTR[32768];
	rand uvm_reg_field PTR[32768];
	uvm_reg_field EM_HASH_LOOKUP_RSVD1_[32768];
	uvm_reg_field RSVD1_[32768];
	rand uvm_reg_field EM_HASH_LOOKUP_SELECT_4[32768];
	rand uvm_reg_field SELECT_4[32768];
	rand uvm_reg_field EM_HASH_LOOKUP_SELECT_3[32768];
	rand uvm_reg_field SELECT_3[32768];
	rand uvm_reg_field EM_HASH_LOOKUP_SELECT_2[32768];
	rand uvm_reg_field SELECT_2[32768];
	rand uvm_reg_field EM_HASH_LOOKUP_SELECT_1[32768];
	rand uvm_reg_field SELECT_1[32768];
	rand uvm_reg_field EM_HASH_LOOKUP_SELECT_0[32768];
	rand uvm_reg_field SELECT_0[32768];
	rand uvm_reg_field EM_HASH_LOOKUP_MASK[32768];
	uvm_reg_field LPM_SUBTRIE_APTR_RSVD0[48][0:511];
	rand uvm_reg_field LPM_SUBTRIE_APTR_ACTION_BASE_PTR[48][0:511];
	rand uvm_reg_field ACTION_BASE_PTR[48][0:511];
	uvm_reg_field LPM_SUBTRIE_CPTR_RSVD0[48][0:511];
	rand uvm_reg_field LPM_SUBTRIE_CPTR_CHILD_PTR_LEN[48][0:511];
	rand uvm_reg_field LPM_SUBTRIE_CPTR_CHILD_BASE_PTR[48][0:511];
	rand uvm_reg_field LPM_SUBTRIE_CPTR_SUBTRIE_PTR[48][0:511];
	rand uvm_reg_field SUBTRIE_PTR[48][0:511];
	rand uvm_reg_field LPM_SUBTRIE_BITMAPS_BITMAP[48][0:4095];
	rand uvm_reg_field BITMAP[48][0:4095];
	rand uvm_reg_field LPM_MATCH_TCAM_KEY_INVERT[512];
	rand uvm_reg_field KEY_INVERT[512];
	rand uvm_reg_field LPM_MATCH_TCAM_KEY[512];
	rand uvm_reg_field KEY[512];
	uvm_reg_field LPM_MATCH_ACTION_RSVD0[512];
	rand uvm_reg_field LPM_MATCH_ACTION_CHILD_PTR_LEN[512];
	rand uvm_reg_field LPM_MATCH_ACTION_CHILD_BASE_PTR[512];
	rand uvm_reg_field LPM_MATCH_ACTION_ROOT_PTR[512];
	rand uvm_reg_field ROOT_PTR[512];
	rand uvm_reg_field LPM_KEY_MASK_MASK[64][0:19];
	rand uvm_reg_field LPM_KEY_SEL0_MD_KEY16_SEL[64];
	rand uvm_reg_field MD_KEY16_SEL[64];
	rand uvm_reg_field LPM_KEY_SEL1_ADDR_KEY8_SEL[64];
	rand uvm_reg_field ADDR_KEY8_SEL[64];
	rand uvm_reg_field LPM_KEY_SEL1_MD_KEY8_SEL[64];
	rand uvm_reg_field MD_KEY8_SEL[64];
	rand uvm_reg_field LPM_KEY_SEL2_ADDR_KEY16_SEL[64];
	rand uvm_reg_field ADDR_KEY16_SEL[64];
	uvm_reg_field LPM_KEY_SEL3_RSVD0[64];
	rand uvm_reg_field LPM_KEY_SEL3_ADDR_KEY32_SEL[64];
	rand uvm_reg_field ADDR_KEY32_SEL[64];

	function new(string name = "A");
		super.new(name, build_coverage(UVM_NO_COVERAGE));
	endfunction: new

   virtual function void build();
      this.default_map = create_map("", 0, 8, UVM_LITTLE_ENDIAN, 0);
      foreach (this.EM_HASH_LOOKUP[i]) begin
         int J = i;
         this.EM_HASH_LOOKUP[J] = ral_reg_A_EM_HASH_LOOKUP::type_id::create($psprintf("EM_HASH_LOOKUP[%0d]",J),,get_full_name());
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
      end
      foreach (this.LPM_SUBTRIE_APTR[i,j]) begin
         int J = i;
         int K = j;
         this.LPM_SUBTRIE_APTR[J][K] = ral_reg_A_LPM_SUBTRIE_APTR::type_id::create($psprintf("LPM_SUBTRIE_APTR[%0d][%0d]",J,K),,get_full_name());
         this.LPM_SUBTRIE_APTR[J][K].configure(this, null, "");
         this.LPM_SUBTRIE_APTR[J][K].build();
         this.LPM_SUBTRIE_APTR[J][K].add_hdl_path('{

            '{$psprintf("LPM_SUBTRIE_APTR[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.LPM_SUBTRIE_APTR[J][K], `UVM_REG_ADDR_WIDTH'h80000+J*`UVM_REG_ADDR_WIDTH'h1000+K*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.LPM_SUBTRIE_APTR_RSVD0[J][K] = this.LPM_SUBTRIE_APTR[J][K].RSVD0;
			this.LPM_SUBTRIE_APTR_ACTION_BASE_PTR[J][K] = this.LPM_SUBTRIE_APTR[J][K].ACTION_BASE_PTR;
			this.ACTION_BASE_PTR[J][K] = this.LPM_SUBTRIE_APTR[J][K].ACTION_BASE_PTR;
      end
      foreach (this.LPM_SUBTRIE_CPTR[i,j]) begin
         int J = i;
         int K = j;
         this.LPM_SUBTRIE_CPTR[J][K] = ral_reg_A_LPM_SUBTRIE_CPTR::type_id::create($psprintf("LPM_SUBTRIE_CPTR[%0d][%0d]",J,K),,get_full_name());
         this.LPM_SUBTRIE_CPTR[J][K].configure(this, null, "");
         this.LPM_SUBTRIE_CPTR[J][K].build();
         this.LPM_SUBTRIE_CPTR[J][K].add_hdl_path('{

            '{$psprintf("LPM_SUBTRIE_CPTR[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.LPM_SUBTRIE_CPTR[J][K], `UVM_REG_ADDR_WIDTH'hC0000+J*`UVM_REG_ADDR_WIDTH'h1000+K*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.LPM_SUBTRIE_CPTR_RSVD0[J][K] = this.LPM_SUBTRIE_CPTR[J][K].RSVD0;
			this.LPM_SUBTRIE_CPTR_CHILD_PTR_LEN[J][K] = this.LPM_SUBTRIE_CPTR[J][K].CHILD_PTR_LEN;
			this.LPM_SUBTRIE_CPTR_CHILD_BASE_PTR[J][K] = this.LPM_SUBTRIE_CPTR[J][K].CHILD_BASE_PTR;
			this.LPM_SUBTRIE_CPTR_SUBTRIE_PTR[J][K] = this.LPM_SUBTRIE_CPTR[J][K].SUBTRIE_PTR;
			this.SUBTRIE_PTR[J][K] = this.LPM_SUBTRIE_CPTR[J][K].SUBTRIE_PTR;
      end
      foreach (this.LPM_SUBTRIE_BITMAPS[i,j]) begin
         int J = i;
         int K = j;
         this.LPM_SUBTRIE_BITMAPS[J][K] = ral_reg_A_LPM_SUBTRIE_BITMAPS::type_id::create($psprintf("LPM_SUBTRIE_BITMAPS[%0d][%0d]",J,K),,get_full_name());
         this.LPM_SUBTRIE_BITMAPS[J][K].configure(this, null, "");
         this.LPM_SUBTRIE_BITMAPS[J][K].build();
         this.LPM_SUBTRIE_BITMAPS[J][K].add_hdl_path('{

            '{$psprintf("LPM_SUBTRIE_BITMAPS[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.LPM_SUBTRIE_BITMAPS[J][K], `UVM_REG_ADDR_WIDTH'h200000+J*`UVM_REG_ADDR_WIDTH'h8000+K*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.LPM_SUBTRIE_BITMAPS_BITMAP[J][K] = this.LPM_SUBTRIE_BITMAPS[J][K].BITMAP;
			this.BITMAP[J][K] = this.LPM_SUBTRIE_BITMAPS[J][K].BITMAP;
      end
      foreach (this.LPM_MATCH_TCAM[i]) begin
         int J = i;
         this.LPM_MATCH_TCAM[J] = ral_reg_A_LPM_MATCH_TCAM::type_id::create($psprintf("LPM_MATCH_TCAM[%0d]",J),,get_full_name());
         this.LPM_MATCH_TCAM[J].configure(this, null, "");
         this.LPM_MATCH_TCAM[J].build();
         this.LPM_MATCH_TCAM[J].add_hdl_path('{

            '{$psprintf("LPM_MATCH_TCAM[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.LPM_MATCH_TCAM[J], `UVM_REG_ADDR_WIDTH'h380000+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.LPM_MATCH_TCAM_KEY_INVERT[J] = this.LPM_MATCH_TCAM[J].KEY_INVERT;
			this.KEY_INVERT[J] = this.LPM_MATCH_TCAM[J].KEY_INVERT;
			this.LPM_MATCH_TCAM_KEY[J] = this.LPM_MATCH_TCAM[J].KEY;
			this.KEY[J] = this.LPM_MATCH_TCAM[J].KEY;
      end
      foreach (this.LPM_MATCH_ACTION[i]) begin
         int J = i;
         this.LPM_MATCH_ACTION[J] = ral_reg_A_LPM_MATCH_ACTION::type_id::create($psprintf("LPM_MATCH_ACTION[%0d]",J),,get_full_name());
         this.LPM_MATCH_ACTION[J].configure(this, null, "");
         this.LPM_MATCH_ACTION[J].build();
         this.LPM_MATCH_ACTION[J].add_hdl_path('{

            '{$psprintf("LPM_MATCH_ACTION[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.LPM_MATCH_ACTION[J], `UVM_REG_ADDR_WIDTH'h381000+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.LPM_MATCH_ACTION_RSVD0[J] = this.LPM_MATCH_ACTION[J].RSVD0;
			this.LPM_MATCH_ACTION_CHILD_PTR_LEN[J] = this.LPM_MATCH_ACTION[J].CHILD_PTR_LEN;
			this.LPM_MATCH_ACTION_CHILD_BASE_PTR[J] = this.LPM_MATCH_ACTION[J].CHILD_BASE_PTR;
			this.LPM_MATCH_ACTION_ROOT_PTR[J] = this.LPM_MATCH_ACTION[J].ROOT_PTR;
			this.ROOT_PTR[J] = this.LPM_MATCH_ACTION[J].ROOT_PTR;
      end
      foreach (this.LPM_KEY_MASK[i,j]) begin
         int J = i;
         int K = j;
         this.LPM_KEY_MASK[J][K] = ral_reg_A_LPM_KEY_MASK::type_id::create($psprintf("LPM_KEY_MASK[%0d][%0d]",J,K),,get_full_name());
         this.LPM_KEY_MASK[J][K].configure(this, null, "");
         this.LPM_KEY_MASK[J][K].build();
         this.LPM_KEY_MASK[J][K].add_hdl_path('{

            '{$psprintf("LPM_KEY_MASK[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.LPM_KEY_MASK[J][K], `UVM_REG_ADDR_WIDTH'h384000+J*`UVM_REG_ADDR_WIDTH'h100+K*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.LPM_KEY_MASK_MASK[J][K] = this.LPM_KEY_MASK[J][K].MASK;
      end
      foreach (this.LPM_KEY_SEL0[i]) begin
         int J = i;
         this.LPM_KEY_SEL0[J] = ral_reg_A_LPM_KEY_SEL0::type_id::create($psprintf("LPM_KEY_SEL0[%0d]",J),,get_full_name());
         this.LPM_KEY_SEL0[J].configure(this, null, "");
         this.LPM_KEY_SEL0[J].build();
         this.LPM_KEY_SEL0[J].add_hdl_path('{

            '{$psprintf("LPM_KEY_SEL0[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.LPM_KEY_SEL0[J], `UVM_REG_ADDR_WIDTH'h388000+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.LPM_KEY_SEL0_MD_KEY16_SEL[J] = this.LPM_KEY_SEL0[J].MD_KEY16_SEL;
			this.MD_KEY16_SEL[J] = this.LPM_KEY_SEL0[J].MD_KEY16_SEL;
      end
      foreach (this.LPM_KEY_SEL1[i]) begin
         int J = i;
         this.LPM_KEY_SEL1[J] = ral_reg_A_LPM_KEY_SEL1::type_id::create($psprintf("LPM_KEY_SEL1[%0d]",J),,get_full_name());
         this.LPM_KEY_SEL1[J].configure(this, null, "");
         this.LPM_KEY_SEL1[J].build();
         this.LPM_KEY_SEL1[J].add_hdl_path('{

            '{$psprintf("LPM_KEY_SEL1[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.LPM_KEY_SEL1[J], `UVM_REG_ADDR_WIDTH'h388200+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.LPM_KEY_SEL1_ADDR_KEY8_SEL[J] = this.LPM_KEY_SEL1[J].ADDR_KEY8_SEL;
			this.ADDR_KEY8_SEL[J] = this.LPM_KEY_SEL1[J].ADDR_KEY8_SEL;
			this.LPM_KEY_SEL1_MD_KEY8_SEL[J] = this.LPM_KEY_SEL1[J].MD_KEY8_SEL;
			this.MD_KEY8_SEL[J] = this.LPM_KEY_SEL1[J].MD_KEY8_SEL;
      end
      foreach (this.LPM_KEY_SEL2[i]) begin
         int J = i;
         this.LPM_KEY_SEL2[J] = ral_reg_A_LPM_KEY_SEL2::type_id::create($psprintf("LPM_KEY_SEL2[%0d]",J),,get_full_name());
         this.LPM_KEY_SEL2[J].configure(this, null, "");
         this.LPM_KEY_SEL2[J].build();
         this.LPM_KEY_SEL2[J].add_hdl_path('{

            '{$psprintf("LPM_KEY_SEL2[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.LPM_KEY_SEL2[J], `UVM_REG_ADDR_WIDTH'h388400+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.LPM_KEY_SEL2_ADDR_KEY16_SEL[J] = this.LPM_KEY_SEL2[J].ADDR_KEY16_SEL;
			this.ADDR_KEY16_SEL[J] = this.LPM_KEY_SEL2[J].ADDR_KEY16_SEL;
      end
      foreach (this.LPM_KEY_SEL3[i]) begin
         int J = i;
         this.LPM_KEY_SEL3[J] = ral_reg_A_LPM_KEY_SEL3::type_id::create($psprintf("LPM_KEY_SEL3[%0d]",J),,get_full_name());
         this.LPM_KEY_SEL3[J].configure(this, null, "");
         this.LPM_KEY_SEL3[J].build();
         this.LPM_KEY_SEL3[J].add_hdl_path('{

            '{$psprintf("LPM_KEY_SEL3[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.LPM_KEY_SEL3[J], `UVM_REG_ADDR_WIDTH'h388600+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.LPM_KEY_SEL3_RSVD0[J] = this.LPM_KEY_SEL3[J].RSVD0;
			this.LPM_KEY_SEL3_ADDR_KEY32_SEL[J] = this.LPM_KEY_SEL3[J].ADDR_KEY32_SEL;
			this.ADDR_KEY32_SEL[J] = this.LPM_KEY_SEL3[J].ADDR_KEY32_SEL;
      end
   endfunction : build

	`uvm_object_utils(ral_block_A)

endclass : ral_block_A



`endif
