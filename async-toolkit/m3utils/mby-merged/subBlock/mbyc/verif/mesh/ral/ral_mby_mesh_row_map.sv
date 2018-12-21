`ifndef RAL_MBY_MESH_ROW_MAP
`define RAL_MBY_MESH_ROW_MAP

import uvm_pkg::*;

class ral_reg_mby_mesh_row_map_MESH_ARB_RREQ_Y extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field WB_TO_NB;
	rand uvm_reg_field EB_TO_NB;
	rand uvm_reg_field NB_TO_NB;
	rand uvm_reg_field WB_TO_SB;
	rand uvm_reg_field EB_TO_SB;
	rand uvm_reg_field SB_TO_SB;

	function new(string name = "mby_mesh_row_map_MESH_ARB_RREQ_Y");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 16, 48, "RO", 0, 16'h0, 1, 0, 1);
      this.WB_TO_NB = uvm_reg_field::type_id::create("WB_TO_NB",,get_full_name());
      this.WB_TO_NB.configure(this, 8, 40, "RW", 0, 8'h0, 1, 0, 1);
      this.EB_TO_NB = uvm_reg_field::type_id::create("EB_TO_NB",,get_full_name());
      this.EB_TO_NB.configure(this, 8, 32, "RW", 0, 8'h0, 1, 0, 1);
      this.NB_TO_NB = uvm_reg_field::type_id::create("NB_TO_NB",,get_full_name());
      this.NB_TO_NB.configure(this, 8, 24, "RW", 0, 8'h0, 1, 0, 1);
      this.WB_TO_SB = uvm_reg_field::type_id::create("WB_TO_SB",,get_full_name());
      this.WB_TO_SB.configure(this, 8, 16, "RW", 0, 8'h0, 1, 0, 1);
      this.EB_TO_SB = uvm_reg_field::type_id::create("EB_TO_SB",,get_full_name());
      this.EB_TO_SB.configure(this, 8, 8, "RW", 0, 8'h0, 1, 0, 1);
      this.SB_TO_SB = uvm_reg_field::type_id::create("SB_TO_SB",,get_full_name());
      this.SB_TO_SB.configure(this, 8, 0, "RW", 0, 8'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_mby_mesh_row_map_MESH_ARB_RREQ_Y)

endclass : ral_reg_mby_mesh_row_map_MESH_ARB_RREQ_Y


class ral_reg_mby_mesh_row_map_MESH_ARB_WREQ_Y extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field WB_TO_NB;
	rand uvm_reg_field EB_TO_NB;
	rand uvm_reg_field NB_TO_NB;
	rand uvm_reg_field WB_TO_SB;
	rand uvm_reg_field EB_TO_SB;
	rand uvm_reg_field SB_TO_SB;

	function new(string name = "mby_mesh_row_map_MESH_ARB_WREQ_Y");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 16, 48, "RO", 0, 16'h0, 1, 0, 1);
      this.WB_TO_NB = uvm_reg_field::type_id::create("WB_TO_NB",,get_full_name());
      this.WB_TO_NB.configure(this, 8, 40, "RW", 0, 8'h0, 1, 0, 1);
      this.EB_TO_NB = uvm_reg_field::type_id::create("EB_TO_NB",,get_full_name());
      this.EB_TO_NB.configure(this, 8, 32, "RW", 0, 8'h0, 1, 0, 1);
      this.NB_TO_NB = uvm_reg_field::type_id::create("NB_TO_NB",,get_full_name());
      this.NB_TO_NB.configure(this, 8, 24, "RW", 0, 8'h0, 1, 0, 1);
      this.WB_TO_SB = uvm_reg_field::type_id::create("WB_TO_SB",,get_full_name());
      this.WB_TO_SB.configure(this, 8, 16, "RW", 0, 8'h0, 1, 0, 1);
      this.EB_TO_SB = uvm_reg_field::type_id::create("EB_TO_SB",,get_full_name());
      this.EB_TO_SB.configure(this, 8, 8, "RW", 0, 8'h0, 1, 0, 1);
      this.SB_TO_SB = uvm_reg_field::type_id::create("SB_TO_SB",,get_full_name());
      this.SB_TO_SB.configure(this, 8, 0, "RW", 0, 8'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_mby_mesh_row_map_MESH_ARB_WREQ_Y)

endclass : ral_reg_mby_mesh_row_map_MESH_ARB_WREQ_Y


class ral_reg_mby_mesh_row_map_MESH_ARB_RRSP_Y extends uvm_reg;
	rand uvm_reg_field WB_TO_NB;
	rand uvm_reg_field EB_TO_NB;
	rand uvm_reg_field NB_TO_NB;
	rand uvm_reg_field MEM_TO_NB;
	rand uvm_reg_field WB_TO_SB;
	rand uvm_reg_field EB_TO_SB;
	rand uvm_reg_field SB_TO_SB;
	rand uvm_reg_field MEM_TO_SB;

	function new(string name = "mby_mesh_row_map_MESH_ARB_RRSP_Y");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.WB_TO_NB = uvm_reg_field::type_id::create("WB_TO_NB",,get_full_name());
      this.WB_TO_NB.configure(this, 8, 56, "RW", 0, 8'h0, 1, 0, 1);
      this.EB_TO_NB = uvm_reg_field::type_id::create("EB_TO_NB",,get_full_name());
      this.EB_TO_NB.configure(this, 8, 48, "RW", 0, 8'h0, 1, 0, 1);
      this.NB_TO_NB = uvm_reg_field::type_id::create("NB_TO_NB",,get_full_name());
      this.NB_TO_NB.configure(this, 8, 40, "RW", 0, 8'h0, 1, 0, 1);
      this.MEM_TO_NB = uvm_reg_field::type_id::create("MEM_TO_NB",,get_full_name());
      this.MEM_TO_NB.configure(this, 8, 32, "RW", 0, 8'h0, 1, 0, 1);
      this.WB_TO_SB = uvm_reg_field::type_id::create("WB_TO_SB",,get_full_name());
      this.WB_TO_SB.configure(this, 8, 24, "RW", 0, 8'h0, 1, 0, 1);
      this.EB_TO_SB = uvm_reg_field::type_id::create("EB_TO_SB",,get_full_name());
      this.EB_TO_SB.configure(this, 8, 16, "RW", 0, 8'h0, 1, 0, 1);
      this.SB_TO_SB = uvm_reg_field::type_id::create("SB_TO_SB",,get_full_name());
      this.SB_TO_SB.configure(this, 8, 8, "RW", 0, 8'h0, 1, 0, 1);
      this.MEM_TO_SB = uvm_reg_field::type_id::create("MEM_TO_SB",,get_full_name());
      this.MEM_TO_SB.configure(this, 8, 0, "RW", 0, 8'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_mby_mesh_row_map_MESH_ARB_RRSP_Y)

endclass : ral_reg_mby_mesh_row_map_MESH_ARB_RRSP_Y


class ral_reg_mby_mesh_row_map_MESH_ARB_RRSP_X extends uvm_reg;
	rand uvm_reg_field NB_TO_WB;
	rand uvm_reg_field SB_TO_WB;
	rand uvm_reg_field WB_TO_WB;
	rand uvm_reg_field MEM_TO_WB;
	rand uvm_reg_field NB_TO_EB;
	rand uvm_reg_field SB_TO_EB;
	rand uvm_reg_field EB_TO_EB;
	rand uvm_reg_field MEM_TO_EB;

	function new(string name = "mby_mesh_row_map_MESH_ARB_RRSP_X");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.NB_TO_WB = uvm_reg_field::type_id::create("NB_TO_WB",,get_full_name());
      this.NB_TO_WB.configure(this, 8, 56, "RW", 0, 8'h0, 1, 0, 1);
      this.SB_TO_WB = uvm_reg_field::type_id::create("SB_TO_WB",,get_full_name());
      this.SB_TO_WB.configure(this, 8, 48, "RW", 0, 8'h0, 1, 0, 1);
      this.WB_TO_WB = uvm_reg_field::type_id::create("WB_TO_WB",,get_full_name());
      this.WB_TO_WB.configure(this, 8, 40, "RW", 0, 8'h0, 1, 0, 1);
      this.MEM_TO_WB = uvm_reg_field::type_id::create("MEM_TO_WB",,get_full_name());
      this.MEM_TO_WB.configure(this, 8, 32, "RW", 0, 8'h0, 1, 0, 1);
      this.NB_TO_EB = uvm_reg_field::type_id::create("NB_TO_EB",,get_full_name());
      this.NB_TO_EB.configure(this, 8, 24, "RW", 0, 8'h0, 1, 0, 1);
      this.SB_TO_EB = uvm_reg_field::type_id::create("SB_TO_EB",,get_full_name());
      this.SB_TO_EB.configure(this, 8, 16, "RW", 0, 8'h0, 1, 0, 1);
      this.EB_TO_EB = uvm_reg_field::type_id::create("EB_TO_EB",,get_full_name());
      this.EB_TO_EB.configure(this, 8, 8, "RW", 0, 8'h0, 1, 0, 1);
      this.MEM_TO_EB = uvm_reg_field::type_id::create("MEM_TO_EB",,get_full_name());
      this.MEM_TO_EB.configure(this, 8, 0, "RW", 0, 8'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_mby_mesh_row_map_MESH_ARB_RRSP_X)

endclass : ral_reg_mby_mesh_row_map_MESH_ARB_RRSP_X


class ral_block_mby_mesh_row_map extends uvm_reg_block;
	rand ral_reg_mby_mesh_row_map_MESH_ARB_RREQ_Y MESH_ARB_RREQ_Y[8];
	rand ral_reg_mby_mesh_row_map_MESH_ARB_WREQ_Y MESH_ARB_WREQ_Y[8];
	rand ral_reg_mby_mesh_row_map_MESH_ARB_RRSP_Y MESH_ARB_RRSP_Y[8];
	rand ral_reg_mby_mesh_row_map_MESH_ARB_RRSP_X MESH_ARB_RRSP_X[8];
	uvm_reg_field MESH_ARB_RREQ_Y_RSVD0[8];
	rand uvm_reg_field MESH_ARB_RREQ_Y_WB_TO_NB[8];
	rand uvm_reg_field MESH_ARB_RREQ_Y_EB_TO_NB[8];
	rand uvm_reg_field MESH_ARB_RREQ_Y_NB_TO_NB[8];
	rand uvm_reg_field MESH_ARB_RREQ_Y_WB_TO_SB[8];
	rand uvm_reg_field MESH_ARB_RREQ_Y_EB_TO_SB[8];
	rand uvm_reg_field MESH_ARB_RREQ_Y_SB_TO_SB[8];
	uvm_reg_field MESH_ARB_WREQ_Y_RSVD0[8];
	rand uvm_reg_field MESH_ARB_WREQ_Y_WB_TO_NB[8];
	rand uvm_reg_field MESH_ARB_WREQ_Y_EB_TO_NB[8];
	rand uvm_reg_field MESH_ARB_WREQ_Y_NB_TO_NB[8];
	rand uvm_reg_field MESH_ARB_WREQ_Y_WB_TO_SB[8];
	rand uvm_reg_field MESH_ARB_WREQ_Y_EB_TO_SB[8];
	rand uvm_reg_field MESH_ARB_WREQ_Y_SB_TO_SB[8];
	rand uvm_reg_field MESH_ARB_RRSP_Y_WB_TO_NB[8];
	rand uvm_reg_field MESH_ARB_RRSP_Y_EB_TO_NB[8];
	rand uvm_reg_field MESH_ARB_RRSP_Y_NB_TO_NB[8];
	rand uvm_reg_field MESH_ARB_RRSP_Y_MEM_TO_NB[8];
	rand uvm_reg_field MEM_TO_NB[8];
	rand uvm_reg_field MESH_ARB_RRSP_Y_WB_TO_SB[8];
	rand uvm_reg_field MESH_ARB_RRSP_Y_EB_TO_SB[8];
	rand uvm_reg_field MESH_ARB_RRSP_Y_SB_TO_SB[8];
	rand uvm_reg_field MESH_ARB_RRSP_Y_MEM_TO_SB[8];
	rand uvm_reg_field MEM_TO_SB[8];
	rand uvm_reg_field MESH_ARB_RRSP_X_NB_TO_WB[8];
	rand uvm_reg_field NB_TO_WB[8];
	rand uvm_reg_field MESH_ARB_RRSP_X_SB_TO_WB[8];
	rand uvm_reg_field SB_TO_WB[8];
	rand uvm_reg_field MESH_ARB_RRSP_X_WB_TO_WB[8];
	rand uvm_reg_field WB_TO_WB[8];
	rand uvm_reg_field MESH_ARB_RRSP_X_MEM_TO_WB[8];
	rand uvm_reg_field MEM_TO_WB[8];
	rand uvm_reg_field MESH_ARB_RRSP_X_NB_TO_EB[8];
	rand uvm_reg_field NB_TO_EB[8];
	rand uvm_reg_field MESH_ARB_RRSP_X_SB_TO_EB[8];
	rand uvm_reg_field SB_TO_EB[8];
	rand uvm_reg_field MESH_ARB_RRSP_X_EB_TO_EB[8];
	rand uvm_reg_field EB_TO_EB[8];
	rand uvm_reg_field MESH_ARB_RRSP_X_MEM_TO_EB[8];
	rand uvm_reg_field MEM_TO_EB[8];

	function new(string name = "mby_mesh_row_map");
		super.new(name, build_coverage(UVM_NO_COVERAGE));
	endfunction: new

   virtual function void build();
      this.default_map = create_map("", 0, 8, UVM_LITTLE_ENDIAN, 0);
      foreach (this.MESH_ARB_RREQ_Y[i]) begin
         int J = i;
         this.MESH_ARB_RREQ_Y[J] = ral_reg_mby_mesh_row_map_MESH_ARB_RREQ_Y::type_id::create($psprintf("MESH_ARB_RREQ_Y[%0d]",J),,get_full_name());
         this.MESH_ARB_RREQ_Y[J].configure(this, null, "");
         this.MESH_ARB_RREQ_Y[J].build();
         this.MESH_ARB_RREQ_Y[J].add_hdl_path('{

            '{$psprintf("MESH_ARB_RREQ_Y[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.MESH_ARB_RREQ_Y[J], `UVM_REG_ADDR_WIDTH'h0+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.MESH_ARB_RREQ_Y_RSVD0[J] = this.MESH_ARB_RREQ_Y[J].RSVD0;
			this.MESH_ARB_RREQ_Y_WB_TO_NB[J] = this.MESH_ARB_RREQ_Y[J].WB_TO_NB;
			this.MESH_ARB_RREQ_Y_EB_TO_NB[J] = this.MESH_ARB_RREQ_Y[J].EB_TO_NB;
			this.MESH_ARB_RREQ_Y_NB_TO_NB[J] = this.MESH_ARB_RREQ_Y[J].NB_TO_NB;
			this.MESH_ARB_RREQ_Y_WB_TO_SB[J] = this.MESH_ARB_RREQ_Y[J].WB_TO_SB;
			this.MESH_ARB_RREQ_Y_EB_TO_SB[J] = this.MESH_ARB_RREQ_Y[J].EB_TO_SB;
			this.MESH_ARB_RREQ_Y_SB_TO_SB[J] = this.MESH_ARB_RREQ_Y[J].SB_TO_SB;
      end
      foreach (this.MESH_ARB_WREQ_Y[i]) begin
         int J = i;
         this.MESH_ARB_WREQ_Y[J] = ral_reg_mby_mesh_row_map_MESH_ARB_WREQ_Y::type_id::create($psprintf("MESH_ARB_WREQ_Y[%0d]",J),,get_full_name());
         this.MESH_ARB_WREQ_Y[J].configure(this, null, "");
         this.MESH_ARB_WREQ_Y[J].build();
         this.MESH_ARB_WREQ_Y[J].add_hdl_path('{

            '{$psprintf("MESH_ARB_WREQ_Y[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.MESH_ARB_WREQ_Y[J], `UVM_REG_ADDR_WIDTH'h40+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.MESH_ARB_WREQ_Y_RSVD0[J] = this.MESH_ARB_WREQ_Y[J].RSVD0;
			this.MESH_ARB_WREQ_Y_WB_TO_NB[J] = this.MESH_ARB_WREQ_Y[J].WB_TO_NB;
			this.MESH_ARB_WREQ_Y_EB_TO_NB[J] = this.MESH_ARB_WREQ_Y[J].EB_TO_NB;
			this.MESH_ARB_WREQ_Y_NB_TO_NB[J] = this.MESH_ARB_WREQ_Y[J].NB_TO_NB;
			this.MESH_ARB_WREQ_Y_WB_TO_SB[J] = this.MESH_ARB_WREQ_Y[J].WB_TO_SB;
			this.MESH_ARB_WREQ_Y_EB_TO_SB[J] = this.MESH_ARB_WREQ_Y[J].EB_TO_SB;
			this.MESH_ARB_WREQ_Y_SB_TO_SB[J] = this.MESH_ARB_WREQ_Y[J].SB_TO_SB;
      end
      foreach (this.MESH_ARB_RRSP_Y[i]) begin
         int J = i;
         this.MESH_ARB_RRSP_Y[J] = ral_reg_mby_mesh_row_map_MESH_ARB_RRSP_Y::type_id::create($psprintf("MESH_ARB_RRSP_Y[%0d]",J),,get_full_name());
         this.MESH_ARB_RRSP_Y[J].configure(this, null, "");
         this.MESH_ARB_RRSP_Y[J].build();
         this.MESH_ARB_RRSP_Y[J].add_hdl_path('{

            '{$psprintf("MESH_ARB_RRSP_Y[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.MESH_ARB_RRSP_Y[J], `UVM_REG_ADDR_WIDTH'h80+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.MESH_ARB_RRSP_Y_WB_TO_NB[J] = this.MESH_ARB_RRSP_Y[J].WB_TO_NB;
			this.MESH_ARB_RRSP_Y_EB_TO_NB[J] = this.MESH_ARB_RRSP_Y[J].EB_TO_NB;
			this.MESH_ARB_RRSP_Y_NB_TO_NB[J] = this.MESH_ARB_RRSP_Y[J].NB_TO_NB;
			this.MESH_ARB_RRSP_Y_MEM_TO_NB[J] = this.MESH_ARB_RRSP_Y[J].MEM_TO_NB;
			this.MEM_TO_NB[J] = this.MESH_ARB_RRSP_Y[J].MEM_TO_NB;
			this.MESH_ARB_RRSP_Y_WB_TO_SB[J] = this.MESH_ARB_RRSP_Y[J].WB_TO_SB;
			this.MESH_ARB_RRSP_Y_EB_TO_SB[J] = this.MESH_ARB_RRSP_Y[J].EB_TO_SB;
			this.MESH_ARB_RRSP_Y_SB_TO_SB[J] = this.MESH_ARB_RRSP_Y[J].SB_TO_SB;
			this.MESH_ARB_RRSP_Y_MEM_TO_SB[J] = this.MESH_ARB_RRSP_Y[J].MEM_TO_SB;
			this.MEM_TO_SB[J] = this.MESH_ARB_RRSP_Y[J].MEM_TO_SB;
      end
      foreach (this.MESH_ARB_RRSP_X[i]) begin
         int J = i;
         this.MESH_ARB_RRSP_X[J] = ral_reg_mby_mesh_row_map_MESH_ARB_RRSP_X::type_id::create($psprintf("MESH_ARB_RRSP_X[%0d]",J),,get_full_name());
         this.MESH_ARB_RRSP_X[J].configure(this, null, "");
         this.MESH_ARB_RRSP_X[J].build();
         this.MESH_ARB_RRSP_X[J].add_hdl_path('{

            '{$psprintf("MESH_ARB_RRSP_X[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.MESH_ARB_RRSP_X[J], `UVM_REG_ADDR_WIDTH'hC0+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.MESH_ARB_RRSP_X_NB_TO_WB[J] = this.MESH_ARB_RRSP_X[J].NB_TO_WB;
			this.NB_TO_WB[J] = this.MESH_ARB_RRSP_X[J].NB_TO_WB;
			this.MESH_ARB_RRSP_X_SB_TO_WB[J] = this.MESH_ARB_RRSP_X[J].SB_TO_WB;
			this.SB_TO_WB[J] = this.MESH_ARB_RRSP_X[J].SB_TO_WB;
			this.MESH_ARB_RRSP_X_WB_TO_WB[J] = this.MESH_ARB_RRSP_X[J].WB_TO_WB;
			this.WB_TO_WB[J] = this.MESH_ARB_RRSP_X[J].WB_TO_WB;
			this.MESH_ARB_RRSP_X_MEM_TO_WB[J] = this.MESH_ARB_RRSP_X[J].MEM_TO_WB;
			this.MEM_TO_WB[J] = this.MESH_ARB_RRSP_X[J].MEM_TO_WB;
			this.MESH_ARB_RRSP_X_NB_TO_EB[J] = this.MESH_ARB_RRSP_X[J].NB_TO_EB;
			this.NB_TO_EB[J] = this.MESH_ARB_RRSP_X[J].NB_TO_EB;
			this.MESH_ARB_RRSP_X_SB_TO_EB[J] = this.MESH_ARB_RRSP_X[J].SB_TO_EB;
			this.SB_TO_EB[J] = this.MESH_ARB_RRSP_X[J].SB_TO_EB;
			this.MESH_ARB_RRSP_X_EB_TO_EB[J] = this.MESH_ARB_RRSP_X[J].EB_TO_EB;
			this.EB_TO_EB[J] = this.MESH_ARB_RRSP_X[J].EB_TO_EB;
			this.MESH_ARB_RRSP_X_MEM_TO_EB[J] = this.MESH_ARB_RRSP_X[J].MEM_TO_EB;
			this.MEM_TO_EB[J] = this.MESH_ARB_RRSP_X[J].MEM_TO_EB;
      end
   endfunction : build

	`uvm_object_utils(ral_block_mby_mesh_row_map)

endclass : ral_block_mby_mesh_row_map



`endif
