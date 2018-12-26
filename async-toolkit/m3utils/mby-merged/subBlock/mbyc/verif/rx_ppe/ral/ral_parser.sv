`ifndef RAL_PARSER
`define RAL_PARSER

import uvm_pkg::*;

class ral_reg_parser_PARSER_PORT_CFG extends uvm_reg;
	rand uvm_reg_field INITIAL_W0_OFFSET;
	rand uvm_reg_field INITIAL_W1_OFFSET;
	rand uvm_reg_field INITIAL_W2_OFFSET;
	rand uvm_reg_field INITIAL_PTR;
	rand uvm_reg_field INITIAL_STATE;
	rand uvm_reg_field INITIAL_OP_MASK;
	rand uvm_reg_field INITIAL_OP_ROT;

	function new(string name = "parser_PARSER_PORT_CFG");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.INITIAL_W0_OFFSET = uvm_reg_field::type_id::create("INITIAL_W0_OFFSET",,get_full_name());
      this.INITIAL_W0_OFFSET.configure(this, 8, 56, "RW", 0, 8'h0, 1, 0, 1);
      this.INITIAL_W1_OFFSET = uvm_reg_field::type_id::create("INITIAL_W1_OFFSET",,get_full_name());
      this.INITIAL_W1_OFFSET.configure(this, 8, 48, "RW", 0, 8'h0, 1, 0, 1);
      this.INITIAL_W2_OFFSET = uvm_reg_field::type_id::create("INITIAL_W2_OFFSET",,get_full_name());
      this.INITIAL_W2_OFFSET.configure(this, 8, 40, "RW", 0, 8'h0, 1, 0, 1);
      this.INITIAL_PTR = uvm_reg_field::type_id::create("INITIAL_PTR",,get_full_name());
      this.INITIAL_PTR.configure(this, 8, 32, "RW", 0, 8'h0, 1, 0, 1);
      this.INITIAL_STATE = uvm_reg_field::type_id::create("INITIAL_STATE",,get_full_name());
      this.INITIAL_STATE.configure(this, 16, 16, "RW", 0, 16'h0, 1, 0, 1);
      this.INITIAL_OP_MASK = uvm_reg_field::type_id::create("INITIAL_OP_MASK",,get_full_name());
      this.INITIAL_OP_MASK.configure(this, 12, 4, "RW", 0, 12'h0, 1, 0, 0);
      this.INITIAL_OP_ROT = uvm_reg_field::type_id::create("INITIAL_OP_ROT",,get_full_name());
      this.INITIAL_OP_ROT.configure(this, 4, 0, "RW", 0, 4'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_parser_PARSER_PORT_CFG)

endclass : ral_reg_parser_PARSER_PORT_CFG


class ral_reg_parser_PARSER_CSUM_CFG extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field VALIDATE_IPV4_HDR_CSUM;
	rand uvm_reg_field VALIDATE_IPV4_HDR_TRUNCATION;
	rand uvm_reg_field VALIDATE_IPV4_HDR_LENGTH;
	rand uvm_reg_field VALIDATE_IPV4_LENGTH;
	rand uvm_reg_field VALIDATE_L4_CSUM;
	rand uvm_reg_field STORE_L4_PARTIAL_CSUM;
	rand uvm_reg_field COMPUTE_L4_CSUM;
	rand uvm_reg_field VALIDATE_L3_LENGTH;

	function new(string name = "parser_PARSER_CSUM_CFG");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 54, 10, "RO", 0, 54'h0, 1, 0, 0);
      this.VALIDATE_IPV4_HDR_CSUM = uvm_reg_field::type_id::create("VALIDATE_IPV4_HDR_CSUM",,get_full_name());
      this.VALIDATE_IPV4_HDR_CSUM.configure(this, 1, 9, "RW", 0, 1'h0, 1, 0, 0);
      this.VALIDATE_IPV4_HDR_TRUNCATION = uvm_reg_field::type_id::create("VALIDATE_IPV4_HDR_TRUNCATION",,get_full_name());
      this.VALIDATE_IPV4_HDR_TRUNCATION.configure(this, 1, 8, "RW", 0, 1'h0, 1, 0, 0);
      this.VALIDATE_IPV4_HDR_LENGTH = uvm_reg_field::type_id::create("VALIDATE_IPV4_HDR_LENGTH",,get_full_name());
      this.VALIDATE_IPV4_HDR_LENGTH.configure(this, 1, 7, "RW", 0, 1'h0, 1, 0, 0);
      this.VALIDATE_IPV4_LENGTH = uvm_reg_field::type_id::create("VALIDATE_IPV4_LENGTH",,get_full_name());
      this.VALIDATE_IPV4_LENGTH.configure(this, 1, 6, "RW", 0, 1'h0, 1, 0, 0);
      this.VALIDATE_L4_CSUM = uvm_reg_field::type_id::create("VALIDATE_L4_CSUM",,get_full_name());
      this.VALIDATE_L4_CSUM.configure(this, 2, 4, "RW", 0, 2'h0, 1, 0, 0);
      this.STORE_L4_PARTIAL_CSUM = uvm_reg_field::type_id::create("STORE_L4_PARTIAL_CSUM",,get_full_name());
      this.STORE_L4_PARTIAL_CSUM.configure(this, 1, 3, "RW", 0, 1'h0, 1, 0, 0);
      this.COMPUTE_L4_CSUM = uvm_reg_field::type_id::create("COMPUTE_L4_CSUM",,get_full_name());
      this.COMPUTE_L4_CSUM.configure(this, 1, 2, "RW", 0, 1'h0, 1, 0, 0);
      this.VALIDATE_L3_LENGTH = uvm_reg_field::type_id::create("VALIDATE_L3_LENGTH",,get_full_name());
      this.VALIDATE_L3_LENGTH.configure(this, 2, 0, "RW", 0, 2'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_parser_PARSER_CSUM_CFG)

endclass : ral_reg_parser_PARSER_CSUM_CFG


class ral_reg_parser_PARSER_IP extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field MEM_ERROR;

	function new(string name = "parser_PARSER_IP");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 63, 1, "RO", 0, 63'h0, 1, 0, 0);
      this.MEM_ERROR = uvm_reg_field::type_id::create("MEM_ERROR",,get_full_name());
      this.MEM_ERROR.configure(this, 1, 0, "W1C", 1, 1'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_parser_PARSER_IP)

endclass : ral_reg_parser_PARSER_IP


class ral_reg_parser_PARSER_IM extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field MEM_ERROR;

	function new(string name = "parser_PARSER_IM");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 63, 1, "RO", 0, 63'h0, 1, 0, 0);
      this.MEM_ERROR = uvm_reg_field::type_id::create("MEM_ERROR",,get_full_name());
      this.MEM_ERROR.configure(this, 1, 0, "RW", 0, 1'h1, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_parser_PARSER_IM)

endclass : ral_reg_parser_PARSER_IM


class ral_reg_parser_PARSER_KEY_W extends uvm_reg;
	rand uvm_reg_field W1_VALUE;
	rand uvm_reg_field W1_MASK;
	rand uvm_reg_field W0_VALUE;
	rand uvm_reg_field W0_MASK;

	function new(string name = "parser_PARSER_KEY_W");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.W1_VALUE = uvm_reg_field::type_id::create("W1_VALUE",,get_full_name());
      this.W1_VALUE.configure(this, 16, 48, "RW", 0, 16'hffff, 1, 0, 1);
      this.W1_MASK = uvm_reg_field::type_id::create("W1_MASK",,get_full_name());
      this.W1_MASK.configure(this, 16, 32, "RW", 0, 16'h0, 1, 0, 1);
      this.W0_VALUE = uvm_reg_field::type_id::create("W0_VALUE",,get_full_name());
      this.W0_VALUE.configure(this, 16, 16, "RW", 0, 16'hffff, 1, 0, 1);
      this.W0_MASK = uvm_reg_field::type_id::create("W0_MASK",,get_full_name());
      this.W0_MASK.configure(this, 16, 0, "RW", 0, 16'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_parser_PARSER_KEY_W)

endclass : ral_reg_parser_PARSER_KEY_W


class ral_reg_parser_PARSER_KEY_S extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field STATE_VALUE;
	rand uvm_reg_field STATE_MASK;

	function new(string name = "parser_PARSER_KEY_S");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 32, 32, "RO", 0, 32'h0, 1, 0, 1);
      this.STATE_VALUE = uvm_reg_field::type_id::create("STATE_VALUE",,get_full_name());
      this.STATE_VALUE.configure(this, 16, 16, "RW", 0, 16'hffff, 1, 0, 1);
      this.STATE_MASK = uvm_reg_field::type_id::create("STATE_MASK",,get_full_name());
      this.STATE_MASK.configure(this, 16, 0, "RW", 0, 16'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_parser_PARSER_KEY_S)

endclass : ral_reg_parser_PARSER_KEY_S


class ral_reg_parser_PARSER_ANA_W extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field NEXT_W0_OFFSET;
	rand uvm_reg_field NEXT_W1_OFFSET;
	rand uvm_reg_field NEXT_W2_OFFSET;
	rand uvm_reg_field SKIP;

	function new(string name = "parser_PARSER_ANA_W");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 32, 32, "RO", 0, 32'h0, 1, 0, 1);
      this.NEXT_W0_OFFSET = uvm_reg_field::type_id::create("NEXT_W0_OFFSET",,get_full_name());
      this.NEXT_W0_OFFSET.configure(this, 8, 24, "RW", 0, 8'h0, 1, 0, 1);
      this.NEXT_W1_OFFSET = uvm_reg_field::type_id::create("NEXT_W1_OFFSET",,get_full_name());
      this.NEXT_W1_OFFSET.configure(this, 8, 16, "RW", 0, 8'h0, 1, 0, 1);
      this.NEXT_W2_OFFSET = uvm_reg_field::type_id::create("NEXT_W2_OFFSET",,get_full_name());
      this.NEXT_W2_OFFSET.configure(this, 8, 8, "RW", 0, 8'h0, 1, 0, 1);
      this.SKIP = uvm_reg_field::type_id::create("SKIP",,get_full_name());
      this.SKIP.configure(this, 8, 0, "RW", 0, 8'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_parser_PARSER_ANA_W)

endclass : ral_reg_parser_PARSER_ANA_W


class ral_reg_parser_PARSER_ANA_S extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field NEXT_STATE;
	rand uvm_reg_field NEXT_STATE_MASK;
	rand uvm_reg_field NEXT_OP;

	function new(string name = "parser_PARSER_ANA_S");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 16, 48, "RO", 0, 16'h0, 1, 0, 1);
      this.NEXT_STATE = uvm_reg_field::type_id::create("NEXT_STATE",,get_full_name());
      this.NEXT_STATE.configure(this, 16, 32, "RW", 0, 16'h0, 1, 0, 1);
      this.NEXT_STATE_MASK = uvm_reg_field::type_id::create("NEXT_STATE_MASK",,get_full_name());
      this.NEXT_STATE_MASK.configure(this, 16, 16, "RW", 0, 16'h0, 1, 0, 1);
      this.NEXT_OP = uvm_reg_field::type_id::create("NEXT_OP",,get_full_name());
      this.NEXT_OP.configure(this, 16, 0, "RW", 0, 16'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_parser_PARSER_ANA_S)

endclass : ral_reg_parser_PARSER_ANA_S


class ral_reg_parser_PARSER_EXC extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field EX_OFFSET;
	rand uvm_reg_field PARSING_DONE;

	function new(string name = "parser_PARSER_EXC");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 55, 9, "RO", 0, 55'h0, 1, 0, 0);
      this.EX_OFFSET = uvm_reg_field::type_id::create("EX_OFFSET",,get_full_name());
      this.EX_OFFSET.configure(this, 8, 1, "RW", 0, 8'h0, 1, 0, 0);
      this.PARSING_DONE = uvm_reg_field::type_id::create("PARSING_DONE",,get_full_name());
      this.PARSING_DONE.configure(this, 1, 0, "RW", 0, 1'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_parser_PARSER_EXC)

endclass : ral_reg_parser_PARSER_EXC


class ral_reg_parser_PARSER_EXT extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field PROTOCOL_ID;
	rand uvm_reg_field OFFSET;
	rand uvm_reg_field FLAG_NUM;
	rand uvm_reg_field FLAG_VALUE;
	rand uvm_reg_field PTR_NUM;

	function new(string name = "parser_PARSER_EXT");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 38, 26, "RO", 0, 38'h0, 1, 0, 0);
      this.PROTOCOL_ID = uvm_reg_field::type_id::create("PROTOCOL_ID",,get_full_name());
      this.PROTOCOL_ID.configure(this, 8, 18, "RW", 0, 8'hff, 1, 0, 0);
      this.OFFSET = uvm_reg_field::type_id::create("OFFSET",,get_full_name());
      this.OFFSET.configure(this, 8, 10, "RW", 0, 8'h0, 1, 0, 0);
      this.FLAG_NUM = uvm_reg_field::type_id::create("FLAG_NUM",,get_full_name());
      this.FLAG_NUM.configure(this, 6, 4, "RW", 0, 6'h0, 1, 0, 0);
      this.FLAG_VALUE = uvm_reg_field::type_id::create("FLAG_VALUE",,get_full_name());
      this.FLAG_VALUE.configure(this, 1, 3, "RW", 0, 1'h0, 1, 0, 0);
      this.PTR_NUM = uvm_reg_field::type_id::create("PTR_NUM",,get_full_name());
      this.PTR_NUM.configure(this, 3, 0, "RW", 0, 3'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_parser_PARSER_EXT)

endclass : ral_reg_parser_PARSER_EXT


class ral_reg_parser_PARSER_PTYPE_TCAM extends uvm_reg;
	rand uvm_reg_field KEY_INVERT;
	rand uvm_reg_field KEY;

	function new(string name = "parser_PARSER_PTYPE_TCAM");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.KEY_INVERT = uvm_reg_field::type_id::create("KEY_INVERT",,get_full_name());
      this.KEY_INVERT.configure(this, 32, 32, "RW", 0, 32'h0, 1, 0, 1);
      this.KEY = uvm_reg_field::type_id::create("KEY",,get_full_name());
      this.KEY.configure(this, 32, 0, "RW", 0, 32'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_parser_PARSER_PTYPE_TCAM)

endclass : ral_reg_parser_PARSER_PTYPE_TCAM


class ral_reg_parser_PARSER_PTYPE_RAM extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field EXTRACT_IDX;
	rand uvm_reg_field PTYPE;

	function new(string name = "parser_PARSER_PTYPE_RAM");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 50, 14, "RO", 0, 50'h0, 1, 0, 0);
      this.EXTRACT_IDX = uvm_reg_field::type_id::create("EXTRACT_IDX",,get_full_name());
      this.EXTRACT_IDX.configure(this, 4, 10, "RW", 0, 4'h0, 1, 0, 0);
      this.PTYPE = uvm_reg_field::type_id::create("PTYPE",,get_full_name());
      this.PTYPE.configure(this, 10, 0, "RW", 0, 10'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_parser_PARSER_PTYPE_RAM)

endclass : ral_reg_parser_PARSER_PTYPE_RAM


class ral_reg_parser_PARSER_EXTRACT_CFG extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field OFFSET;
	rand uvm_reg_field PROTOCOL_ID;

	function new(string name = "parser_PARSER_EXTRACT_CFG");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 48, 16, "RO", 0, 48'h0, 1, 0, 1);
      this.OFFSET = uvm_reg_field::type_id::create("OFFSET",,get_full_name());
      this.OFFSET.configure(this, 8, 8, "RW", 0, 8'h0, 1, 0, 1);
      this.PROTOCOL_ID = uvm_reg_field::type_id::create("PROTOCOL_ID",,get_full_name());
      this.PROTOCOL_ID.configure(this, 8, 0, "RW", 0, 8'hff, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_parser_PARSER_EXTRACT_CFG)

endclass : ral_reg_parser_PARSER_EXTRACT_CFG


class ral_reg_parser_PARSER_COUNTERS extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field EXT_DUP_PROTID;
	rand uvm_reg_field EXT_UNKNOWN_PROTID;
	rand uvm_reg_field EXT_SEG_BOUNDARY;

	function new(string name = "parser_PARSER_COUNTERS");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 40, 24, "RO", 0, 40'h0, 1, 0, 1);
      this.EXT_DUP_PROTID = uvm_reg_field::type_id::create("EXT_DUP_PROTID",,get_full_name());
      this.EXT_DUP_PROTID.configure(this, 8, 16, "W1C", 1, 8'h0, 1, 0, 1);
      this.EXT_UNKNOWN_PROTID = uvm_reg_field::type_id::create("EXT_UNKNOWN_PROTID",,get_full_name());
      this.EXT_UNKNOWN_PROTID.configure(this, 8, 8, "W1C", 1, 8'h0, 1, 0, 1);
      this.EXT_SEG_BOUNDARY = uvm_reg_field::type_id::create("EXT_SEG_BOUNDARY",,get_full_name());
      this.EXT_SEG_BOUNDARY.configure(this, 8, 0, "W1C", 1, 8'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_parser_PARSER_COUNTERS)

endclass : ral_reg_parser_PARSER_COUNTERS


class ral_block_parser extends uvm_reg_block;
	rand ral_reg_parser_PARSER_PORT_CFG PARSER_PORT_CFG[16];
	rand ral_reg_parser_PARSER_CSUM_CFG PARSER_CSUM_CFG[16];
	rand ral_reg_parser_PARSER_IP PARSER_IP;
	rand ral_reg_parser_PARSER_IM PARSER_IM;
	rand ral_reg_parser_PARSER_KEY_W PARSER_KEY_W[32][0:15];
	rand ral_reg_parser_PARSER_KEY_S PARSER_KEY_S[32][0:15];
	rand ral_reg_parser_PARSER_ANA_W PARSER_ANA_W[32][0:15];
	rand ral_reg_parser_PARSER_ANA_S PARSER_ANA_S[32][0:15];
	rand ral_reg_parser_PARSER_EXC PARSER_EXC[32][0:15];
	rand ral_reg_parser_PARSER_EXT PARSER_EXT[32][0:31];
	rand ral_reg_parser_PARSER_PTYPE_TCAM PARSER_PTYPE_TCAM[2][0:63];
	rand ral_reg_parser_PARSER_PTYPE_RAM PARSER_PTYPE_RAM[2][0:63];
	rand ral_reg_parser_PARSER_EXTRACT_CFG PARSER_EXTRACT_CFG[16][0:79];
	rand ral_reg_parser_PARSER_COUNTERS PARSER_COUNTERS;
	rand uvm_reg_field PARSER_PORT_CFG_INITIAL_W0_OFFSET[16];
	rand uvm_reg_field INITIAL_W0_OFFSET[16];
	rand uvm_reg_field PARSER_PORT_CFG_INITIAL_W1_OFFSET[16];
	rand uvm_reg_field INITIAL_W1_OFFSET[16];
	rand uvm_reg_field PARSER_PORT_CFG_INITIAL_W2_OFFSET[16];
	rand uvm_reg_field INITIAL_W2_OFFSET[16];
	rand uvm_reg_field PARSER_PORT_CFG_INITIAL_PTR[16];
	rand uvm_reg_field INITIAL_PTR[16];
	rand uvm_reg_field PARSER_PORT_CFG_INITIAL_STATE[16];
	rand uvm_reg_field INITIAL_STATE[16];
	rand uvm_reg_field PARSER_PORT_CFG_INITIAL_OP_MASK[16];
	rand uvm_reg_field INITIAL_OP_MASK[16];
	rand uvm_reg_field PARSER_PORT_CFG_INITIAL_OP_ROT[16];
	rand uvm_reg_field INITIAL_OP_ROT[16];
	uvm_reg_field PARSER_CSUM_CFG_RSVD0[16];
	rand uvm_reg_field PARSER_CSUM_CFG_VALIDATE_IPV4_HDR_CSUM[16];
	rand uvm_reg_field VALIDATE_IPV4_HDR_CSUM[16];
	rand uvm_reg_field PARSER_CSUM_CFG_VALIDATE_IPV4_HDR_TRUNCATION[16];
	rand uvm_reg_field VALIDATE_IPV4_HDR_TRUNCATION[16];
	rand uvm_reg_field PARSER_CSUM_CFG_VALIDATE_IPV4_HDR_LENGTH[16];
	rand uvm_reg_field VALIDATE_IPV4_HDR_LENGTH[16];
	rand uvm_reg_field PARSER_CSUM_CFG_VALIDATE_IPV4_LENGTH[16];
	rand uvm_reg_field VALIDATE_IPV4_LENGTH[16];
	rand uvm_reg_field PARSER_CSUM_CFG_VALIDATE_L4_CSUM[16];
	rand uvm_reg_field VALIDATE_L4_CSUM[16];
	rand uvm_reg_field PARSER_CSUM_CFG_STORE_L4_PARTIAL_CSUM[16];
	rand uvm_reg_field STORE_L4_PARTIAL_CSUM[16];
	rand uvm_reg_field PARSER_CSUM_CFG_COMPUTE_L4_CSUM[16];
	rand uvm_reg_field COMPUTE_L4_CSUM[16];
	rand uvm_reg_field PARSER_CSUM_CFG_VALIDATE_L3_LENGTH[16];
	rand uvm_reg_field VALIDATE_L3_LENGTH[16];
	uvm_reg_field PARSER_IP_RSVD0;
	rand uvm_reg_field PARSER_IP_MEM_ERROR;
	uvm_reg_field PARSER_IM_RSVD0;
	rand uvm_reg_field PARSER_IM_MEM_ERROR;
	rand uvm_reg_field PARSER_KEY_W_W1_VALUE[32][0:15];
	rand uvm_reg_field W1_VALUE[32][0:15];
	rand uvm_reg_field PARSER_KEY_W_W1_MASK[32][0:15];
	rand uvm_reg_field W1_MASK[32][0:15];
	rand uvm_reg_field PARSER_KEY_W_W0_VALUE[32][0:15];
	rand uvm_reg_field W0_VALUE[32][0:15];
	rand uvm_reg_field PARSER_KEY_W_W0_MASK[32][0:15];
	rand uvm_reg_field W0_MASK[32][0:15];
	uvm_reg_field PARSER_KEY_S_RSVD0[32][0:15];
	rand uvm_reg_field PARSER_KEY_S_STATE_VALUE[32][0:15];
	rand uvm_reg_field STATE_VALUE[32][0:15];
	rand uvm_reg_field PARSER_KEY_S_STATE_MASK[32][0:15];
	rand uvm_reg_field STATE_MASK[32][0:15];
	uvm_reg_field PARSER_ANA_W_RSVD0[32][0:15];
	rand uvm_reg_field PARSER_ANA_W_NEXT_W0_OFFSET[32][0:15];
	rand uvm_reg_field NEXT_W0_OFFSET[32][0:15];
	rand uvm_reg_field PARSER_ANA_W_NEXT_W1_OFFSET[32][0:15];
	rand uvm_reg_field NEXT_W1_OFFSET[32][0:15];
	rand uvm_reg_field PARSER_ANA_W_NEXT_W2_OFFSET[32][0:15];
	rand uvm_reg_field NEXT_W2_OFFSET[32][0:15];
	rand uvm_reg_field PARSER_ANA_W_SKIP[32][0:15];
	rand uvm_reg_field SKIP[32][0:15];
	uvm_reg_field PARSER_ANA_S_RSVD0[32][0:15];
	rand uvm_reg_field PARSER_ANA_S_NEXT_STATE[32][0:15];
	rand uvm_reg_field NEXT_STATE[32][0:15];
	rand uvm_reg_field PARSER_ANA_S_NEXT_STATE_MASK[32][0:15];
	rand uvm_reg_field NEXT_STATE_MASK[32][0:15];
	rand uvm_reg_field PARSER_ANA_S_NEXT_OP[32][0:15];
	rand uvm_reg_field NEXT_OP[32][0:15];
	uvm_reg_field PARSER_EXC_RSVD0[32][0:15];
	rand uvm_reg_field PARSER_EXC_EX_OFFSET[32][0:15];
	rand uvm_reg_field EX_OFFSET[32][0:15];
	rand uvm_reg_field PARSER_EXC_PARSING_DONE[32][0:15];
	rand uvm_reg_field PARSING_DONE[32][0:15];
	uvm_reg_field PARSER_EXT_RSVD0[32][0:31];
	rand uvm_reg_field PARSER_EXT_PROTOCOL_ID[32][0:31];
	rand uvm_reg_field PARSER_EXT_OFFSET[32][0:31];
	rand uvm_reg_field PARSER_EXT_FLAG_NUM[32][0:31];
	rand uvm_reg_field FLAG_NUM[32][0:31];
	rand uvm_reg_field PARSER_EXT_FLAG_VALUE[32][0:31];
	rand uvm_reg_field FLAG_VALUE[32][0:31];
	rand uvm_reg_field PARSER_EXT_PTR_NUM[32][0:31];
	rand uvm_reg_field PTR_NUM[32][0:31];
	rand uvm_reg_field PARSER_PTYPE_TCAM_KEY_INVERT[2][0:63];
	rand uvm_reg_field KEY_INVERT[2][0:63];
	rand uvm_reg_field PARSER_PTYPE_TCAM_KEY[2][0:63];
	rand uvm_reg_field KEY[2][0:63];
	uvm_reg_field PARSER_PTYPE_RAM_RSVD0[2][0:63];
	rand uvm_reg_field PARSER_PTYPE_RAM_EXTRACT_IDX[2][0:63];
	rand uvm_reg_field EXTRACT_IDX[2][0:63];
	rand uvm_reg_field PARSER_PTYPE_RAM_PTYPE[2][0:63];
	rand uvm_reg_field PTYPE[2][0:63];
	uvm_reg_field PARSER_EXTRACT_CFG_RSVD0[16][0:79];
	rand uvm_reg_field PARSER_EXTRACT_CFG_OFFSET[16][0:79];
	rand uvm_reg_field PARSER_EXTRACT_CFG_PROTOCOL_ID[16][0:79];
	uvm_reg_field PARSER_COUNTERS_RSVD0;
	rand uvm_reg_field PARSER_COUNTERS_EXT_DUP_PROTID;
	rand uvm_reg_field EXT_DUP_PROTID;
	rand uvm_reg_field PARSER_COUNTERS_EXT_UNKNOWN_PROTID;
	rand uvm_reg_field EXT_UNKNOWN_PROTID;
	rand uvm_reg_field PARSER_COUNTERS_EXT_SEG_BOUNDARY;
	rand uvm_reg_field EXT_SEG_BOUNDARY;

	function new(string name = "parser");
		super.new(name, build_coverage(UVM_NO_COVERAGE));
	endfunction: new

   virtual function void build();
      this.default_map = create_map("", 0, 8, UVM_LITTLE_ENDIAN, 0);
      foreach (this.PARSER_PORT_CFG[i]) begin
         int J = i;
         this.PARSER_PORT_CFG[J] = ral_reg_parser_PARSER_PORT_CFG::type_id::create($psprintf("PARSER_PORT_CFG[%0d]",J),,get_full_name());
         this.PARSER_PORT_CFG[J].configure(this, null, "");
         this.PARSER_PORT_CFG[J].build();
         this.PARSER_PORT_CFG[J].add_hdl_path('{

            '{$psprintf("PARSER_PORT_CFG[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.PARSER_PORT_CFG[J], `UVM_REG_ADDR_WIDTH'h0+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.PARSER_PORT_CFG_INITIAL_W0_OFFSET[J] = this.PARSER_PORT_CFG[J].INITIAL_W0_OFFSET;
			this.INITIAL_W0_OFFSET[J] = this.PARSER_PORT_CFG[J].INITIAL_W0_OFFSET;
			this.PARSER_PORT_CFG_INITIAL_W1_OFFSET[J] = this.PARSER_PORT_CFG[J].INITIAL_W1_OFFSET;
			this.INITIAL_W1_OFFSET[J] = this.PARSER_PORT_CFG[J].INITIAL_W1_OFFSET;
			this.PARSER_PORT_CFG_INITIAL_W2_OFFSET[J] = this.PARSER_PORT_CFG[J].INITIAL_W2_OFFSET;
			this.INITIAL_W2_OFFSET[J] = this.PARSER_PORT_CFG[J].INITIAL_W2_OFFSET;
			this.PARSER_PORT_CFG_INITIAL_PTR[J] = this.PARSER_PORT_CFG[J].INITIAL_PTR;
			this.INITIAL_PTR[J] = this.PARSER_PORT_CFG[J].INITIAL_PTR;
			this.PARSER_PORT_CFG_INITIAL_STATE[J] = this.PARSER_PORT_CFG[J].INITIAL_STATE;
			this.INITIAL_STATE[J] = this.PARSER_PORT_CFG[J].INITIAL_STATE;
			this.PARSER_PORT_CFG_INITIAL_OP_MASK[J] = this.PARSER_PORT_CFG[J].INITIAL_OP_MASK;
			this.INITIAL_OP_MASK[J] = this.PARSER_PORT_CFG[J].INITIAL_OP_MASK;
			this.PARSER_PORT_CFG_INITIAL_OP_ROT[J] = this.PARSER_PORT_CFG[J].INITIAL_OP_ROT;
			this.INITIAL_OP_ROT[J] = this.PARSER_PORT_CFG[J].INITIAL_OP_ROT;
      end
      foreach (this.PARSER_CSUM_CFG[i]) begin
         int J = i;
         this.PARSER_CSUM_CFG[J] = ral_reg_parser_PARSER_CSUM_CFG::type_id::create($psprintf("PARSER_CSUM_CFG[%0d]",J),,get_full_name());
         this.PARSER_CSUM_CFG[J].configure(this, null, "");
         this.PARSER_CSUM_CFG[J].build();
         this.PARSER_CSUM_CFG[J].add_hdl_path('{

            '{$psprintf("PARSER_CSUM_CFG[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.PARSER_CSUM_CFG[J], `UVM_REG_ADDR_WIDTH'h80+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.PARSER_CSUM_CFG_RSVD0[J] = this.PARSER_CSUM_CFG[J].RSVD0;
			this.PARSER_CSUM_CFG_VALIDATE_IPV4_HDR_CSUM[J] = this.PARSER_CSUM_CFG[J].VALIDATE_IPV4_HDR_CSUM;
			this.VALIDATE_IPV4_HDR_CSUM[J] = this.PARSER_CSUM_CFG[J].VALIDATE_IPV4_HDR_CSUM;
			this.PARSER_CSUM_CFG_VALIDATE_IPV4_HDR_TRUNCATION[J] = this.PARSER_CSUM_CFG[J].VALIDATE_IPV4_HDR_TRUNCATION;
			this.VALIDATE_IPV4_HDR_TRUNCATION[J] = this.PARSER_CSUM_CFG[J].VALIDATE_IPV4_HDR_TRUNCATION;
			this.PARSER_CSUM_CFG_VALIDATE_IPV4_HDR_LENGTH[J] = this.PARSER_CSUM_CFG[J].VALIDATE_IPV4_HDR_LENGTH;
			this.VALIDATE_IPV4_HDR_LENGTH[J] = this.PARSER_CSUM_CFG[J].VALIDATE_IPV4_HDR_LENGTH;
			this.PARSER_CSUM_CFG_VALIDATE_IPV4_LENGTH[J] = this.PARSER_CSUM_CFG[J].VALIDATE_IPV4_LENGTH;
			this.VALIDATE_IPV4_LENGTH[J] = this.PARSER_CSUM_CFG[J].VALIDATE_IPV4_LENGTH;
			this.PARSER_CSUM_CFG_VALIDATE_L4_CSUM[J] = this.PARSER_CSUM_CFG[J].VALIDATE_L4_CSUM;
			this.VALIDATE_L4_CSUM[J] = this.PARSER_CSUM_CFG[J].VALIDATE_L4_CSUM;
			this.PARSER_CSUM_CFG_STORE_L4_PARTIAL_CSUM[J] = this.PARSER_CSUM_CFG[J].STORE_L4_PARTIAL_CSUM;
			this.STORE_L4_PARTIAL_CSUM[J] = this.PARSER_CSUM_CFG[J].STORE_L4_PARTIAL_CSUM;
			this.PARSER_CSUM_CFG_COMPUTE_L4_CSUM[J] = this.PARSER_CSUM_CFG[J].COMPUTE_L4_CSUM;
			this.COMPUTE_L4_CSUM[J] = this.PARSER_CSUM_CFG[J].COMPUTE_L4_CSUM;
			this.PARSER_CSUM_CFG_VALIDATE_L3_LENGTH[J] = this.PARSER_CSUM_CFG[J].VALIDATE_L3_LENGTH;
			this.VALIDATE_L3_LENGTH[J] = this.PARSER_CSUM_CFG[J].VALIDATE_L3_LENGTH;
      end
      this.PARSER_IP = ral_reg_parser_PARSER_IP::type_id::create("PARSER_IP",,get_full_name());
      this.PARSER_IP.configure(this, null, "");
      this.PARSER_IP.build();
         this.PARSER_IP.add_hdl_path('{

            '{"PARSER_IP", -1, -1}
         });
      this.default_map.add_reg(this.PARSER_IP, `UVM_REG_ADDR_WIDTH'h100, "RW", 0);
		this.PARSER_IP_RSVD0 = this.PARSER_IP.RSVD0;
		this.PARSER_IP_MEM_ERROR = this.PARSER_IP.MEM_ERROR;
      this.PARSER_IM = ral_reg_parser_PARSER_IM::type_id::create("PARSER_IM",,get_full_name());
      this.PARSER_IM.configure(this, null, "");
      this.PARSER_IM.build();
         this.PARSER_IM.add_hdl_path('{

            '{"PARSER_IM", -1, -1}
         });
      this.default_map.add_reg(this.PARSER_IM, `UVM_REG_ADDR_WIDTH'h108, "RW", 0);
		this.PARSER_IM_RSVD0 = this.PARSER_IM.RSVD0;
		this.PARSER_IM_MEM_ERROR = this.PARSER_IM.MEM_ERROR;
      foreach (this.PARSER_KEY_W[i,j]) begin
         int J = i;
         int K = j;
         this.PARSER_KEY_W[J][K] = ral_reg_parser_PARSER_KEY_W::type_id::create($psprintf("PARSER_KEY_W[%0d][%0d]",J,K),,get_full_name());
         this.PARSER_KEY_W[J][K].configure(this, null, "");
         this.PARSER_KEY_W[J][K].build();
         this.PARSER_KEY_W[J][K].add_hdl_path('{

            '{$psprintf("PARSER_KEY_W[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.PARSER_KEY_W[J][K], `UVM_REG_ADDR_WIDTH'h1000+J*`UVM_REG_ADDR_WIDTH'h80+K*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.PARSER_KEY_W_W1_VALUE[J][K] = this.PARSER_KEY_W[J][K].W1_VALUE;
			this.W1_VALUE[J][K] = this.PARSER_KEY_W[J][K].W1_VALUE;
			this.PARSER_KEY_W_W1_MASK[J][K] = this.PARSER_KEY_W[J][K].W1_MASK;
			this.W1_MASK[J][K] = this.PARSER_KEY_W[J][K].W1_MASK;
			this.PARSER_KEY_W_W0_VALUE[J][K] = this.PARSER_KEY_W[J][K].W0_VALUE;
			this.W0_VALUE[J][K] = this.PARSER_KEY_W[J][K].W0_VALUE;
			this.PARSER_KEY_W_W0_MASK[J][K] = this.PARSER_KEY_W[J][K].W0_MASK;
			this.W0_MASK[J][K] = this.PARSER_KEY_W[J][K].W0_MASK;
      end
      foreach (this.PARSER_KEY_S[i,j]) begin
         int J = i;
         int K = j;
         this.PARSER_KEY_S[J][K] = ral_reg_parser_PARSER_KEY_S::type_id::create($psprintf("PARSER_KEY_S[%0d][%0d]",J,K),,get_full_name());
         this.PARSER_KEY_S[J][K].configure(this, null, "");
         this.PARSER_KEY_S[J][K].build();
         this.PARSER_KEY_S[J][K].add_hdl_path('{

            '{$psprintf("PARSER_KEY_S[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.PARSER_KEY_S[J][K], `UVM_REG_ADDR_WIDTH'h2000+J*`UVM_REG_ADDR_WIDTH'h80+K*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.PARSER_KEY_S_RSVD0[J][K] = this.PARSER_KEY_S[J][K].RSVD0;
			this.PARSER_KEY_S_STATE_VALUE[J][K] = this.PARSER_KEY_S[J][K].STATE_VALUE;
			this.STATE_VALUE[J][K] = this.PARSER_KEY_S[J][K].STATE_VALUE;
			this.PARSER_KEY_S_STATE_MASK[J][K] = this.PARSER_KEY_S[J][K].STATE_MASK;
			this.STATE_MASK[J][K] = this.PARSER_KEY_S[J][K].STATE_MASK;
      end
      foreach (this.PARSER_ANA_W[i,j]) begin
         int J = i;
         int K = j;
         this.PARSER_ANA_W[J][K] = ral_reg_parser_PARSER_ANA_W::type_id::create($psprintf("PARSER_ANA_W[%0d][%0d]",J,K),,get_full_name());
         this.PARSER_ANA_W[J][K].configure(this, null, "");
         this.PARSER_ANA_W[J][K].build();
         this.PARSER_ANA_W[J][K].add_hdl_path('{

            '{$psprintf("PARSER_ANA_W[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.PARSER_ANA_W[J][K], `UVM_REG_ADDR_WIDTH'h3000+J*`UVM_REG_ADDR_WIDTH'h80+K*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.PARSER_ANA_W_RSVD0[J][K] = this.PARSER_ANA_W[J][K].RSVD0;
			this.PARSER_ANA_W_NEXT_W0_OFFSET[J][K] = this.PARSER_ANA_W[J][K].NEXT_W0_OFFSET;
			this.NEXT_W0_OFFSET[J][K] = this.PARSER_ANA_W[J][K].NEXT_W0_OFFSET;
			this.PARSER_ANA_W_NEXT_W1_OFFSET[J][K] = this.PARSER_ANA_W[J][K].NEXT_W1_OFFSET;
			this.NEXT_W1_OFFSET[J][K] = this.PARSER_ANA_W[J][K].NEXT_W1_OFFSET;
			this.PARSER_ANA_W_NEXT_W2_OFFSET[J][K] = this.PARSER_ANA_W[J][K].NEXT_W2_OFFSET;
			this.NEXT_W2_OFFSET[J][K] = this.PARSER_ANA_W[J][K].NEXT_W2_OFFSET;
			this.PARSER_ANA_W_SKIP[J][K] = this.PARSER_ANA_W[J][K].SKIP;
			this.SKIP[J][K] = this.PARSER_ANA_W[J][K].SKIP;
      end
      foreach (this.PARSER_ANA_S[i,j]) begin
         int J = i;
         int K = j;
         this.PARSER_ANA_S[J][K] = ral_reg_parser_PARSER_ANA_S::type_id::create($psprintf("PARSER_ANA_S[%0d][%0d]",J,K),,get_full_name());
         this.PARSER_ANA_S[J][K].configure(this, null, "");
         this.PARSER_ANA_S[J][K].build();
         this.PARSER_ANA_S[J][K].add_hdl_path('{

            '{$psprintf("PARSER_ANA_S[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.PARSER_ANA_S[J][K], `UVM_REG_ADDR_WIDTH'h4000+J*`UVM_REG_ADDR_WIDTH'h80+K*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.PARSER_ANA_S_RSVD0[J][K] = this.PARSER_ANA_S[J][K].RSVD0;
			this.PARSER_ANA_S_NEXT_STATE[J][K] = this.PARSER_ANA_S[J][K].NEXT_STATE;
			this.NEXT_STATE[J][K] = this.PARSER_ANA_S[J][K].NEXT_STATE;
			this.PARSER_ANA_S_NEXT_STATE_MASK[J][K] = this.PARSER_ANA_S[J][K].NEXT_STATE_MASK;
			this.NEXT_STATE_MASK[J][K] = this.PARSER_ANA_S[J][K].NEXT_STATE_MASK;
			this.PARSER_ANA_S_NEXT_OP[J][K] = this.PARSER_ANA_S[J][K].NEXT_OP;
			this.NEXT_OP[J][K] = this.PARSER_ANA_S[J][K].NEXT_OP;
      end
      foreach (this.PARSER_EXC[i,j]) begin
         int J = i;
         int K = j;
         this.PARSER_EXC[J][K] = ral_reg_parser_PARSER_EXC::type_id::create($psprintf("PARSER_EXC[%0d][%0d]",J,K),,get_full_name());
         this.PARSER_EXC[J][K].configure(this, null, "");
         this.PARSER_EXC[J][K].build();
         this.PARSER_EXC[J][K].add_hdl_path('{

            '{$psprintf("PARSER_EXC[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.PARSER_EXC[J][K], `UVM_REG_ADDR_WIDTH'h5000+J*`UVM_REG_ADDR_WIDTH'h80+K*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.PARSER_EXC_RSVD0[J][K] = this.PARSER_EXC[J][K].RSVD0;
			this.PARSER_EXC_EX_OFFSET[J][K] = this.PARSER_EXC[J][K].EX_OFFSET;
			this.EX_OFFSET[J][K] = this.PARSER_EXC[J][K].EX_OFFSET;
			this.PARSER_EXC_PARSING_DONE[J][K] = this.PARSER_EXC[J][K].PARSING_DONE;
			this.PARSING_DONE[J][K] = this.PARSER_EXC[J][K].PARSING_DONE;
      end
      foreach (this.PARSER_EXT[i,j]) begin
         int J = i;
         int K = j;
         this.PARSER_EXT[J][K] = ral_reg_parser_PARSER_EXT::type_id::create($psprintf("PARSER_EXT[%0d][%0d]",J,K),,get_full_name());
         this.PARSER_EXT[J][K].configure(this, null, "");
         this.PARSER_EXT[J][K].build();
         this.PARSER_EXT[J][K].add_hdl_path('{

            '{$psprintf("PARSER_EXT[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.PARSER_EXT[J][K], `UVM_REG_ADDR_WIDTH'h6000+J*`UVM_REG_ADDR_WIDTH'h100+K*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.PARSER_EXT_RSVD0[J][K] = this.PARSER_EXT[J][K].RSVD0;
			this.PARSER_EXT_PROTOCOL_ID[J][K] = this.PARSER_EXT[J][K].PROTOCOL_ID;
			this.PARSER_EXT_OFFSET[J][K] = this.PARSER_EXT[J][K].OFFSET;
			this.PARSER_EXT_FLAG_NUM[J][K] = this.PARSER_EXT[J][K].FLAG_NUM;
			this.FLAG_NUM[J][K] = this.PARSER_EXT[J][K].FLAG_NUM;
			this.PARSER_EXT_FLAG_VALUE[J][K] = this.PARSER_EXT[J][K].FLAG_VALUE;
			this.FLAG_VALUE[J][K] = this.PARSER_EXT[J][K].FLAG_VALUE;
			this.PARSER_EXT_PTR_NUM[J][K] = this.PARSER_EXT[J][K].PTR_NUM;
			this.PTR_NUM[J][K] = this.PARSER_EXT[J][K].PTR_NUM;
      end
      foreach (this.PARSER_PTYPE_TCAM[i,j]) begin
         int J = i;
         int K = j;
         this.PARSER_PTYPE_TCAM[J][K] = ral_reg_parser_PARSER_PTYPE_TCAM::type_id::create($psprintf("PARSER_PTYPE_TCAM[%0d][%0d]",J,K),,get_full_name());
         this.PARSER_PTYPE_TCAM[J][K].configure(this, null, "");
         this.PARSER_PTYPE_TCAM[J][K].build();
         this.PARSER_PTYPE_TCAM[J][K].add_hdl_path('{

            '{$psprintf("PARSER_PTYPE_TCAM[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.PARSER_PTYPE_TCAM[J][K], `UVM_REG_ADDR_WIDTH'h8000+J*`UVM_REG_ADDR_WIDTH'h200+K*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.PARSER_PTYPE_TCAM_KEY_INVERT[J][K] = this.PARSER_PTYPE_TCAM[J][K].KEY_INVERT;
			this.KEY_INVERT[J][K] = this.PARSER_PTYPE_TCAM[J][K].KEY_INVERT;
			this.PARSER_PTYPE_TCAM_KEY[J][K] = this.PARSER_PTYPE_TCAM[J][K].KEY;
			this.KEY[J][K] = this.PARSER_PTYPE_TCAM[J][K].KEY;
      end
      foreach (this.PARSER_PTYPE_RAM[i,j]) begin
         int J = i;
         int K = j;
         this.PARSER_PTYPE_RAM[J][K] = ral_reg_parser_PARSER_PTYPE_RAM::type_id::create($psprintf("PARSER_PTYPE_RAM[%0d][%0d]",J,K),,get_full_name());
         this.PARSER_PTYPE_RAM[J][K].configure(this, null, "");
         this.PARSER_PTYPE_RAM[J][K].build();
         this.PARSER_PTYPE_RAM[J][K].add_hdl_path('{

            '{$psprintf("PARSER_PTYPE_RAM[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.PARSER_PTYPE_RAM[J][K], `UVM_REG_ADDR_WIDTH'h8400+J*`UVM_REG_ADDR_WIDTH'h200+K*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.PARSER_PTYPE_RAM_RSVD0[J][K] = this.PARSER_PTYPE_RAM[J][K].RSVD0;
			this.PARSER_PTYPE_RAM_EXTRACT_IDX[J][K] = this.PARSER_PTYPE_RAM[J][K].EXTRACT_IDX;
			this.EXTRACT_IDX[J][K] = this.PARSER_PTYPE_RAM[J][K].EXTRACT_IDX;
			this.PARSER_PTYPE_RAM_PTYPE[J][K] = this.PARSER_PTYPE_RAM[J][K].PTYPE;
			this.PTYPE[J][K] = this.PARSER_PTYPE_RAM[J][K].PTYPE;
      end
      foreach (this.PARSER_EXTRACT_CFG[i,j]) begin
         int J = i;
         int K = j;
         this.PARSER_EXTRACT_CFG[J][K] = ral_reg_parser_PARSER_EXTRACT_CFG::type_id::create($psprintf("PARSER_EXTRACT_CFG[%0d][%0d]",J,K),,get_full_name());
         this.PARSER_EXTRACT_CFG[J][K].configure(this, null, "");
         this.PARSER_EXTRACT_CFG[J][K].build();
         this.PARSER_EXTRACT_CFG[J][K].add_hdl_path('{

            '{$psprintf("PARSER_EXTRACT_CFG[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.PARSER_EXTRACT_CFG[J][K], `UVM_REG_ADDR_WIDTH'hC000+J*`UVM_REG_ADDR_WIDTH'h400+K*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.PARSER_EXTRACT_CFG_RSVD0[J][K] = this.PARSER_EXTRACT_CFG[J][K].RSVD0;
			this.PARSER_EXTRACT_CFG_OFFSET[J][K] = this.PARSER_EXTRACT_CFG[J][K].OFFSET;
			this.PARSER_EXTRACT_CFG_PROTOCOL_ID[J][K] = this.PARSER_EXTRACT_CFG[J][K].PROTOCOL_ID;
      end
      this.PARSER_COUNTERS = ral_reg_parser_PARSER_COUNTERS::type_id::create("PARSER_COUNTERS",,get_full_name());
      this.PARSER_COUNTERS.configure(this, null, "");
      this.PARSER_COUNTERS.build();
         this.PARSER_COUNTERS.add_hdl_path('{

            '{"PARSER_COUNTERS", -1, -1}
         });
      this.default_map.add_reg(this.PARSER_COUNTERS, `UVM_REG_ADDR_WIDTH'h10000, "RW", 0);
		this.PARSER_COUNTERS_RSVD0 = this.PARSER_COUNTERS.RSVD0;
		this.PARSER_COUNTERS_EXT_DUP_PROTID = this.PARSER_COUNTERS.EXT_DUP_PROTID;
		this.EXT_DUP_PROTID = this.PARSER_COUNTERS.EXT_DUP_PROTID;
		this.PARSER_COUNTERS_EXT_UNKNOWN_PROTID = this.PARSER_COUNTERS.EXT_UNKNOWN_PROTID;
		this.EXT_UNKNOWN_PROTID = this.PARSER_COUNTERS.EXT_UNKNOWN_PROTID;
		this.PARSER_COUNTERS_EXT_SEG_BOUNDARY = this.PARSER_COUNTERS.EXT_SEG_BOUNDARY;
		this.EXT_SEG_BOUNDARY = this.PARSER_COUNTERS.EXT_SEG_BOUNDARY;
   endfunction : build

	`uvm_object_utils(ral_block_parser)

endclass : ral_block_parser



`endif
