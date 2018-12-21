`ifndef RAL_TRIG_USAGE
`define RAL_TRIG_USAGE

import uvm_pkg::*;

class ral_reg_trig_usage_TRIGGER_RATE_LIM_CFG_1 extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field RATE_EXPONENT;
	rand uvm_reg_field RATE_MANTISSA;
	rand uvm_reg_field CAPACITY;

	function new(string name = "trig_usage_TRIGGER_RATE_LIM_CFG_1");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 37, 27, "RO", 0, 37'h0, 1, 0, 0);
      this.RATE_EXPONENT = uvm_reg_field::type_id::create("RATE_EXPONENT",,get_full_name());
      this.RATE_EXPONENT.configure(this, 3, 24, "RW", 0, 3'h0, 1, 0, 0);
      this.RATE_MANTISSA = uvm_reg_field::type_id::create("RATE_MANTISSA",,get_full_name());
      this.RATE_MANTISSA.configure(this, 12, 12, "RW", 0, 12'h0, 1, 0, 0);
      this.CAPACITY = uvm_reg_field::type_id::create("CAPACITY",,get_full_name());
      this.CAPACITY.configure(this, 12, 0, "RW", 0, 12'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_trig_usage_TRIGGER_RATE_LIM_CFG_1)

endclass : ral_reg_trig_usage_TRIGGER_RATE_LIM_CFG_1


class ral_reg_trig_usage_TRIGGER_RATE_LIM_USAGE extends uvm_reg;
	uvm_reg_field RSVD0;
	uvm_reg_field LEVEL;

	function new(string name = "trig_usage_TRIGGER_RATE_LIM_USAGE");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 37, 27, "RO", 0, 37'h0, 1, 0, 0);
      this.LEVEL = uvm_reg_field::type_id::create("LEVEL",,get_full_name());
      this.LEVEL.configure(this, 27, 0, "RO", 1, 27'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_trig_usage_TRIGGER_RATE_LIM_USAGE)

endclass : ral_reg_trig_usage_TRIGGER_RATE_LIM_USAGE


class ral_block_trig_usage extends uvm_reg_block;
	rand ral_reg_trig_usage_TRIGGER_RATE_LIM_CFG_1 TRIGGER_RATE_LIM_CFG_1[16];
	rand ral_reg_trig_usage_TRIGGER_RATE_LIM_USAGE TRIGGER_RATE_LIM_USAGE[16];
	uvm_reg_field TRIGGER_RATE_LIM_CFG_1_RSVD0[16];
	rand uvm_reg_field TRIGGER_RATE_LIM_CFG_1_RATE_EXPONENT[16];
	rand uvm_reg_field RATE_EXPONENT[16];
	rand uvm_reg_field TRIGGER_RATE_LIM_CFG_1_RATE_MANTISSA[16];
	rand uvm_reg_field RATE_MANTISSA[16];
	rand uvm_reg_field TRIGGER_RATE_LIM_CFG_1_CAPACITY[16];
	rand uvm_reg_field CAPACITY[16];
	uvm_reg_field TRIGGER_RATE_LIM_USAGE_RSVD0[16];
	uvm_reg_field TRIGGER_RATE_LIM_USAGE_LEVEL[16];
	uvm_reg_field LEVEL[16];

	function new(string name = "trig_usage");
		super.new(name, build_coverage(UVM_NO_COVERAGE));
	endfunction: new

   virtual function void build();
      this.default_map = create_map("", 0, 8, UVM_LITTLE_ENDIAN, 0);
      foreach (this.TRIGGER_RATE_LIM_CFG_1[i]) begin
         int J = i;
         this.TRIGGER_RATE_LIM_CFG_1[J] = ral_reg_trig_usage_TRIGGER_RATE_LIM_CFG_1::type_id::create($psprintf("TRIGGER_RATE_LIM_CFG_1[%0d]",J),,get_full_name());
         this.TRIGGER_RATE_LIM_CFG_1[J].configure(this, null, "");
         this.TRIGGER_RATE_LIM_CFG_1[J].build();
         this.TRIGGER_RATE_LIM_CFG_1[J].add_hdl_path('{

            '{$psprintf("TRIGGER_RATE_LIM_CFG_1[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.TRIGGER_RATE_LIM_CFG_1[J], `UVM_REG_ADDR_WIDTH'h0+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.TRIGGER_RATE_LIM_CFG_1_RSVD0[J] = this.TRIGGER_RATE_LIM_CFG_1[J].RSVD0;
			this.TRIGGER_RATE_LIM_CFG_1_RATE_EXPONENT[J] = this.TRIGGER_RATE_LIM_CFG_1[J].RATE_EXPONENT;
			this.RATE_EXPONENT[J] = this.TRIGGER_RATE_LIM_CFG_1[J].RATE_EXPONENT;
			this.TRIGGER_RATE_LIM_CFG_1_RATE_MANTISSA[J] = this.TRIGGER_RATE_LIM_CFG_1[J].RATE_MANTISSA;
			this.RATE_MANTISSA[J] = this.TRIGGER_RATE_LIM_CFG_1[J].RATE_MANTISSA;
			this.TRIGGER_RATE_LIM_CFG_1_CAPACITY[J] = this.TRIGGER_RATE_LIM_CFG_1[J].CAPACITY;
			this.CAPACITY[J] = this.TRIGGER_RATE_LIM_CFG_1[J].CAPACITY;
      end
      foreach (this.TRIGGER_RATE_LIM_USAGE[i]) begin
         int J = i;
         this.TRIGGER_RATE_LIM_USAGE[J] = ral_reg_trig_usage_TRIGGER_RATE_LIM_USAGE::type_id::create($psprintf("TRIGGER_RATE_LIM_USAGE[%0d]",J),,get_full_name());
         this.TRIGGER_RATE_LIM_USAGE[J].configure(this, null, "");
         this.TRIGGER_RATE_LIM_USAGE[J].build();
         this.TRIGGER_RATE_LIM_USAGE[J].add_hdl_path('{

            '{$psprintf("TRIGGER_RATE_LIM_USAGE[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.TRIGGER_RATE_LIM_USAGE[J], `UVM_REG_ADDR_WIDTH'h80+J*`UVM_REG_ADDR_WIDTH'h8, "RO", 0);
			this.TRIGGER_RATE_LIM_USAGE_RSVD0[J] = this.TRIGGER_RATE_LIM_USAGE[J].RSVD0;
			this.TRIGGER_RATE_LIM_USAGE_LEVEL[J] = this.TRIGGER_RATE_LIM_USAGE[J].LEVEL;
			this.LEVEL[J] = this.TRIGGER_RATE_LIM_USAGE[J].LEVEL;
      end
   endfunction : build

	`uvm_object_utils(ral_block_trig_usage)

endclass : ral_block_trig_usage



`endif
