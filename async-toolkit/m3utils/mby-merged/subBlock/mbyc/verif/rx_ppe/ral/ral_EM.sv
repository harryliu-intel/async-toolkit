`ifndef RAL_EM
`define RAL_EM

import uvm_pkg::*;

class ral_reg_EM_HASH_CAM extends uvm_reg;
	rand uvm_reg_field DATA;

	function new(string name = "EM_HASH_CAM");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.DATA = uvm_reg_field::type_id::create("DATA",,get_full_name());
      this.DATA.configure(this, 64, 0, "RW", 0, 64'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_EM_HASH_CAM)

endclass : ral_reg_EM_HASH_CAM


class ral_reg_EM_HASH_CAM_EN extends uvm_reg;
	rand uvm_reg_field MASK;

	function new(string name = "EM_HASH_CAM_EN");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.MASK = uvm_reg_field::type_id::create("MASK",,get_full_name());
      this.MASK.configure(this, 64, 0, "RW", 0, 64'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_EM_HASH_CAM_EN)

endclass : ral_reg_EM_HASH_CAM_EN


class ral_reg_EM_KEY_SEL0 extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field KEY8_MASK;

	function new(string name = "EM_KEY_SEL0");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 32, 32, "RO", 0, 32'h0, 1, 0, 1);
      this.KEY8_MASK = uvm_reg_field::type_id::create("KEY8_MASK",,get_full_name());
      this.KEY8_MASK.configure(this, 32, 0, "RW", 0, 32'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_EM_KEY_SEL0)

endclass : ral_reg_EM_KEY_SEL0


class ral_reg_EM_KEY_SEL1 extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field KEY_MASK_SEL;
	rand uvm_reg_field KEY32_MASK;
	rand uvm_reg_field KEY16_MASK;

	function new(string name = "EM_KEY_SEL1");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 12, 52, "RO", 0, 12'h0, 1, 0, 0);
      this.KEY_MASK_SEL = uvm_reg_field::type_id::create("KEY_MASK_SEL",,get_full_name());
      this.KEY_MASK_SEL.configure(this, 4, 48, "RW", 0, 4'h0, 1, 0, 0);
      this.KEY32_MASK = uvm_reg_field::type_id::create("KEY32_MASK",,get_full_name());
      this.KEY32_MASK.configure(this, 16, 32, "RW", 0, 16'h0, 1, 0, 1);
      this.KEY16_MASK = uvm_reg_field::type_id::create("KEY16_MASK",,get_full_name());
      this.KEY16_MASK.configure(this, 32, 0, "RW", 0, 32'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_EM_KEY_SEL1)

endclass : ral_reg_EM_KEY_SEL1


class ral_reg_EM_KEY_MASK extends uvm_reg;
	rand uvm_reg_field MASK;

	function new(string name = "EM_KEY_MASK");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.MASK = uvm_reg_field::type_id::create("MASK",,get_full_name());
      this.MASK.configure(this, 64, 0, "RW", 0, 64'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_EM_KEY_MASK)

endclass : ral_reg_EM_KEY_MASK


class ral_reg_EM_HASH_MISS extends uvm_reg;
	rand uvm_reg_field ACTION1;
	rand uvm_reg_field ACTION0;

	function new(string name = "EM_HASH_MISS");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.ACTION1 = uvm_reg_field::type_id::create("ACTION1",,get_full_name());
      this.ACTION1.configure(this, 32, 32, "RW", 0, 32'h0, 1, 0, 1);
      this.ACTION0 = uvm_reg_field::type_id::create("ACTION0",,get_full_name());
      this.ACTION0.configure(this, 32, 0, "RW", 0, 32'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_EM_HASH_MISS)

endclass : ral_reg_EM_HASH_MISS


class ral_reg_EM_HASH_CFG extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field HASH_HI;
	rand uvm_reg_field HASH_LO;
	rand uvm_reg_field MODE;
	rand uvm_reg_field BASE_PTR_0;
	rand uvm_reg_field BASE_PTR_1;
	rand uvm_reg_field HASH_SIZE_0;
	rand uvm_reg_field HASH_SIZE_1;
	rand uvm_reg_field ENTRY_SIZE_0;
	rand uvm_reg_field ENTRY_SIZE_1;

	function new(string name = "EM_HASH_CFG");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 15, 49, "RO", 0, 15'h0, 1, 0, 0);
      this.HASH_HI = uvm_reg_field::type_id::create("HASH_HI",,get_full_name());
      this.HASH_HI.configure(this, 1, 48, "RW", 0, 1'h0, 1, 0, 0);
      this.HASH_LO = uvm_reg_field::type_id::create("HASH_LO",,get_full_name());
      this.HASH_LO.configure(this, 1, 47, "RW", 0, 1'h0, 1, 0, 0);
      this.MODE = uvm_reg_field::type_id::create("MODE",,get_full_name());
      this.MODE.configure(this, 1, 46, "RW", 0, 1'h0, 1, 0, 0);
      this.BASE_PTR_0 = uvm_reg_field::type_id::create("BASE_PTR_0",,get_full_name());
      this.BASE_PTR_0.configure(this, 13, 33, "RW", 0, 13'h0, 1, 0, 0);
      this.BASE_PTR_1 = uvm_reg_field::type_id::create("BASE_PTR_1",,get_full_name());
      this.BASE_PTR_1.configure(this, 13, 20, "RW", 0, 13'h0, 1, 0, 0);
      this.HASH_SIZE_0 = uvm_reg_field::type_id::create("HASH_SIZE_0",,get_full_name());
      this.HASH_SIZE_0.configure(this, 5, 15, "RW", 0, 5'h0, 1, 0, 0);
      this.HASH_SIZE_1 = uvm_reg_field::type_id::create("HASH_SIZE_1",,get_full_name());
      this.HASH_SIZE_1.configure(this, 5, 10, "RW", 0, 5'h0, 1, 0, 0);
      this.ENTRY_SIZE_0 = uvm_reg_field::type_id::create("ENTRY_SIZE_0",,get_full_name());
      this.ENTRY_SIZE_0.configure(this, 5, 5, "RW", 0, 5'h0, 1, 0, 0);
      this.ENTRY_SIZE_1 = uvm_reg_field::type_id::create("ENTRY_SIZE_1",,get_full_name());
      this.ENTRY_SIZE_1.configure(this, 5, 0, "RW", 0, 5'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_EM_HASH_CFG)

endclass : ral_reg_EM_HASH_CFG


class ral_block_EM extends uvm_reg_block;
	rand ral_reg_EM_HASH_CAM HASH_CAM[32][0:7];
	rand ral_reg_EM_HASH_CAM_EN HASH_CAM_EN[2][0:31];
	rand ral_reg_EM_KEY_SEL0 KEY_SEL0[2][0:63];
	rand ral_reg_EM_KEY_SEL1 KEY_SEL1[2][0:63];
	rand ral_reg_EM_KEY_MASK KEY_MASK[2][0:31];
	rand ral_reg_EM_HASH_MISS HASH_MISS[2][0:63];
	rand ral_reg_EM_HASH_CFG HASH_CFG[64];
	rand uvm_reg_field HASH_CAM_DATA[32][0:7];
	rand uvm_reg_field DATA[32][0:7];
	rand uvm_reg_field HASH_CAM_EN_MASK[2][0:31];
	uvm_reg_field KEY_SEL0_RSVD0[2][0:63];
	rand uvm_reg_field KEY_SEL0_KEY8_MASK[2][0:63];
	rand uvm_reg_field KEY8_MASK[2][0:63];
	uvm_reg_field KEY_SEL1_RSVD0[2][0:63];
	rand uvm_reg_field KEY_SEL1_KEY_MASK_SEL[2][0:63];
	rand uvm_reg_field KEY_MASK_SEL[2][0:63];
	rand uvm_reg_field KEY_SEL1_KEY32_MASK[2][0:63];
	rand uvm_reg_field KEY32_MASK[2][0:63];
	rand uvm_reg_field KEY_SEL1_KEY16_MASK[2][0:63];
	rand uvm_reg_field KEY16_MASK[2][0:63];
	rand uvm_reg_field KEY_MASK_MASK[2][0:31];
	rand uvm_reg_field HASH_MISS_ACTION1[2][0:63];
	rand uvm_reg_field ACTION1[2][0:63];
	rand uvm_reg_field HASH_MISS_ACTION0[2][0:63];
	rand uvm_reg_field ACTION0[2][0:63];
	uvm_reg_field HASH_CFG_RSVD0[64];
	rand uvm_reg_field HASH_CFG_HASH_HI[64];
	rand uvm_reg_field HASH_HI[64];
	rand uvm_reg_field HASH_CFG_HASH_LO[64];
	rand uvm_reg_field HASH_LO[64];
	rand uvm_reg_field HASH_CFG_MODE[64];
	rand uvm_reg_field MODE[64];
	rand uvm_reg_field HASH_CFG_BASE_PTR_0[64];
	rand uvm_reg_field BASE_PTR_0[64];
	rand uvm_reg_field HASH_CFG_BASE_PTR_1[64];
	rand uvm_reg_field BASE_PTR_1[64];
	rand uvm_reg_field HASH_CFG_HASH_SIZE_0[64];
	rand uvm_reg_field HASH_SIZE_0[64];
	rand uvm_reg_field HASH_CFG_HASH_SIZE_1[64];
	rand uvm_reg_field HASH_SIZE_1[64];
	rand uvm_reg_field HASH_CFG_ENTRY_SIZE_0[64];
	rand uvm_reg_field ENTRY_SIZE_0[64];
	rand uvm_reg_field HASH_CFG_ENTRY_SIZE_1[64];
	rand uvm_reg_field ENTRY_SIZE_1[64];

	function new(string name = "EM");
		super.new(name, build_coverage(UVM_NO_COVERAGE));
	endfunction: new

   virtual function void build();
      this.default_map = create_map("", 0, 8, UVM_LITTLE_ENDIAN, 0);
      foreach (this.HASH_CAM[i,j]) begin
         int J = i;
         int K = j;
         this.HASH_CAM[J][K] = ral_reg_EM_HASH_CAM::type_id::create($psprintf("HASH_CAM[%0d][%0d]",J,K),,get_full_name());
         this.HASH_CAM[J][K].configure(this, null, "");
         this.HASH_CAM[J][K].build();
         this.HASH_CAM[J][K].add_hdl_path('{

            '{$psprintf("HASH_CAM[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.HASH_CAM[J][K], `UVM_REG_ADDR_WIDTH'h0+J*`UVM_REG_ADDR_WIDTH'h40+K*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.HASH_CAM_DATA[J][K] = this.HASH_CAM[J][K].DATA;
			this.DATA[J][K] = this.HASH_CAM[J][K].DATA;
      end
      foreach (this.HASH_CAM_EN[i,j]) begin
         int J = i;
         int K = j;
         this.HASH_CAM_EN[J][K] = ral_reg_EM_HASH_CAM_EN::type_id::create($psprintf("HASH_CAM_EN[%0d][%0d]",J,K),,get_full_name());
         this.HASH_CAM_EN[J][K].configure(this, null, "");
         this.HASH_CAM_EN[J][K].build();
         this.HASH_CAM_EN[J][K].add_hdl_path('{

            '{$psprintf("HASH_CAM_EN[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.HASH_CAM_EN[J][K], `UVM_REG_ADDR_WIDTH'h800+J*`UVM_REG_ADDR_WIDTH'h100+K*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.HASH_CAM_EN_MASK[J][K] = this.HASH_CAM_EN[J][K].MASK;
      end
      foreach (this.KEY_SEL0[i,j]) begin
         int J = i;
         int K = j;
         this.KEY_SEL0[J][K] = ral_reg_EM_KEY_SEL0::type_id::create($psprintf("KEY_SEL0[%0d][%0d]",J,K),,get_full_name());
         this.KEY_SEL0[J][K].configure(this, null, "");
         this.KEY_SEL0[J][K].build();
         this.KEY_SEL0[J][K].add_hdl_path('{

            '{$psprintf("KEY_SEL0[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.KEY_SEL0[J][K], `UVM_REG_ADDR_WIDTH'hC00+J*`UVM_REG_ADDR_WIDTH'h200+K*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.KEY_SEL0_RSVD0[J][K] = this.KEY_SEL0[J][K].RSVD0;
			this.KEY_SEL0_KEY8_MASK[J][K] = this.KEY_SEL0[J][K].KEY8_MASK;
			this.KEY8_MASK[J][K] = this.KEY_SEL0[J][K].KEY8_MASK;
      end
      foreach (this.KEY_SEL1[i,j]) begin
         int J = i;
         int K = j;
         this.KEY_SEL1[J][K] = ral_reg_EM_KEY_SEL1::type_id::create($psprintf("KEY_SEL1[%0d][%0d]",J,K),,get_full_name());
         this.KEY_SEL1[J][K].configure(this, null, "");
         this.KEY_SEL1[J][K].build();
         this.KEY_SEL1[J][K].add_hdl_path('{

            '{$psprintf("KEY_SEL1[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.KEY_SEL1[J][K], `UVM_REG_ADDR_WIDTH'h1000+J*`UVM_REG_ADDR_WIDTH'h200+K*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.KEY_SEL1_RSVD0[J][K] = this.KEY_SEL1[J][K].RSVD0;
			this.KEY_SEL1_KEY_MASK_SEL[J][K] = this.KEY_SEL1[J][K].KEY_MASK_SEL;
			this.KEY_MASK_SEL[J][K] = this.KEY_SEL1[J][K].KEY_MASK_SEL;
			this.KEY_SEL1_KEY32_MASK[J][K] = this.KEY_SEL1[J][K].KEY32_MASK;
			this.KEY32_MASK[J][K] = this.KEY_SEL1[J][K].KEY32_MASK;
			this.KEY_SEL1_KEY16_MASK[J][K] = this.KEY_SEL1[J][K].KEY16_MASK;
			this.KEY16_MASK[J][K] = this.KEY_SEL1[J][K].KEY16_MASK;
      end
      foreach (this.KEY_MASK[i,j]) begin
         int J = i;
         int K = j;
         this.KEY_MASK[J][K] = ral_reg_EM_KEY_MASK::type_id::create($psprintf("KEY_MASK[%0d][%0d]",J,K),,get_full_name());
         this.KEY_MASK[J][K].configure(this, null, "");
         this.KEY_MASK[J][K].build();
         this.KEY_MASK[J][K].add_hdl_path('{

            '{$psprintf("KEY_MASK[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.KEY_MASK[J][K], `UVM_REG_ADDR_WIDTH'h1400+J*`UVM_REG_ADDR_WIDTH'h100+K*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.KEY_MASK_MASK[J][K] = this.KEY_MASK[J][K].MASK;
      end
      foreach (this.HASH_MISS[i,j]) begin
         int J = i;
         int K = j;
         this.HASH_MISS[J][K] = ral_reg_EM_HASH_MISS::type_id::create($psprintf("HASH_MISS[%0d][%0d]",J,K),,get_full_name());
         this.HASH_MISS[J][K].configure(this, null, "");
         this.HASH_MISS[J][K].build();
         this.HASH_MISS[J][K].add_hdl_path('{

            '{$psprintf("HASH_MISS[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.HASH_MISS[J][K], `UVM_REG_ADDR_WIDTH'h1800+J*`UVM_REG_ADDR_WIDTH'h200+K*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.HASH_MISS_ACTION1[J][K] = this.HASH_MISS[J][K].ACTION1;
			this.ACTION1[J][K] = this.HASH_MISS[J][K].ACTION1;
			this.HASH_MISS_ACTION0[J][K] = this.HASH_MISS[J][K].ACTION0;
			this.ACTION0[J][K] = this.HASH_MISS[J][K].ACTION0;
      end
      foreach (this.HASH_CFG[i]) begin
         int J = i;
         this.HASH_CFG[J] = ral_reg_EM_HASH_CFG::type_id::create($psprintf("HASH_CFG[%0d]",J),,get_full_name());
         this.HASH_CFG[J].configure(this, null, "");
         this.HASH_CFG[J].build();
         this.HASH_CFG[J].add_hdl_path('{

            '{$psprintf("HASH_CFG[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.HASH_CFG[J], `UVM_REG_ADDR_WIDTH'h1C00+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.HASH_CFG_RSVD0[J] = this.HASH_CFG[J].RSVD0;
			this.HASH_CFG_HASH_HI[J] = this.HASH_CFG[J].HASH_HI;
			this.HASH_HI[J] = this.HASH_CFG[J].HASH_HI;
			this.HASH_CFG_HASH_LO[J] = this.HASH_CFG[J].HASH_LO;
			this.HASH_LO[J] = this.HASH_CFG[J].HASH_LO;
			this.HASH_CFG_MODE[J] = this.HASH_CFG[J].MODE;
			this.MODE[J] = this.HASH_CFG[J].MODE;
			this.HASH_CFG_BASE_PTR_0[J] = this.HASH_CFG[J].BASE_PTR_0;
			this.BASE_PTR_0[J] = this.HASH_CFG[J].BASE_PTR_0;
			this.HASH_CFG_BASE_PTR_1[J] = this.HASH_CFG[J].BASE_PTR_1;
			this.BASE_PTR_1[J] = this.HASH_CFG[J].BASE_PTR_1;
			this.HASH_CFG_HASH_SIZE_0[J] = this.HASH_CFG[J].HASH_SIZE_0;
			this.HASH_SIZE_0[J] = this.HASH_CFG[J].HASH_SIZE_0;
			this.HASH_CFG_HASH_SIZE_1[J] = this.HASH_CFG[J].HASH_SIZE_1;
			this.HASH_SIZE_1[J] = this.HASH_CFG[J].HASH_SIZE_1;
			this.HASH_CFG_ENTRY_SIZE_0[J] = this.HASH_CFG[J].ENTRY_SIZE_0;
			this.ENTRY_SIZE_0[J] = this.HASH_CFG[J].ENTRY_SIZE_0;
			this.HASH_CFG_ENTRY_SIZE_1[J] = this.HASH_CFG[J].ENTRY_SIZE_1;
			this.ENTRY_SIZE_1[J] = this.HASH_CFG[J].ENTRY_SIZE_1;
      end
   endfunction : build

	`uvm_object_utils(ral_block_EM)

endclass : ral_block_EM



`endif
