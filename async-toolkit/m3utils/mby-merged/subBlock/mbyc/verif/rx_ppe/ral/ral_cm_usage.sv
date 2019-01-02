`ifndef RAL_CM_USAGE
`define RAL_CM_USAGE

import uvm_pkg::*;

class ral_reg_cm_usage_CM_TX_TC_PRIVATE_WM extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field WATERMARK;

	function new(string name = "cm_usage_CM_TX_TC_PRIVATE_WM");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 49, 15, "RO", 0, 49'h0, 1, 0, 0);
      this.WATERMARK = uvm_reg_field::type_id::create("WATERMARK",,get_full_name());
      this.WATERMARK.configure(this, 15, 0, "RW", 0, 15'h7fff, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_cm_usage_CM_TX_TC_PRIVATE_WM)

endclass : ral_reg_cm_usage_CM_TX_TC_PRIVATE_WM


class ral_reg_cm_usage_CM_TX_TC_HOG_WM extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field WATERMARK;

	function new(string name = "cm_usage_CM_TX_TC_HOG_WM");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 49, 15, "RO", 0, 49'h0, 1, 0, 0);
      this.WATERMARK = uvm_reg_field::type_id::create("WATERMARK",,get_full_name());
      this.WATERMARK.configure(this, 15, 0, "RW", 0, 15'h7fff, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_cm_usage_CM_TX_TC_HOG_WM)

endclass : ral_reg_cm_usage_CM_TX_TC_HOG_WM


class ral_reg_cm_usage_CM_RX_SMP_PRIVATE_WM extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field WATERMARK;

	function new(string name = "cm_usage_CM_RX_SMP_PRIVATE_WM");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 49, 15, "RO", 0, 49'h0, 1, 0, 0);
      this.WATERMARK = uvm_reg_field::type_id::create("WATERMARK",,get_full_name());
      this.WATERMARK.configure(this, 15, 0, "RW", 0, 15'h7fff, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_cm_usage_CM_RX_SMP_PRIVATE_WM)

endclass : ral_reg_cm_usage_CM_RX_SMP_PRIVATE_WM


class ral_reg_cm_usage_CM_RX_SMP_HOG_WM extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field WATERMARK;

	function new(string name = "cm_usage_CM_RX_SMP_HOG_WM");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 49, 15, "RO", 0, 49'h0, 1, 0, 0);
      this.WATERMARK = uvm_reg_field::type_id::create("WATERMARK",,get_full_name());
      this.WATERMARK.configure(this, 15, 0, "RW", 0, 15'h7fff, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_cm_usage_CM_RX_SMP_HOG_WM)

endclass : ral_reg_cm_usage_CM_RX_SMP_HOG_WM


class ral_reg_cm_usage_CM_RX_SMP_PAUSE_WM extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field PAUSE_OFF;
	rand uvm_reg_field PAUSE_ON;

	function new(string name = "cm_usage_CM_RX_SMP_PAUSE_WM");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 34, 30, "RO", 0, 34'h0, 1, 0, 0);
      this.PAUSE_OFF = uvm_reg_field::type_id::create("PAUSE_OFF",,get_full_name());
      this.PAUSE_OFF.configure(this, 15, 15, "RW", 0, 15'h7fff, 1, 0, 0);
      this.PAUSE_ON = uvm_reg_field::type_id::create("PAUSE_ON",,get_full_name());
      this.PAUSE_ON.configure(this, 15, 0, "RW", 0, 15'h7fff, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_cm_usage_CM_RX_SMP_PAUSE_WM)

endclass : ral_reg_cm_usage_CM_RX_SMP_PAUSE_WM


class ral_reg_cm_usage_CM_SHARED_WM extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field WATERMARK;

	function new(string name = "cm_usage_CM_SHARED_WM");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 49, 15, "RO", 0, 49'h0, 1, 0, 0);
      this.WATERMARK = uvm_reg_field::type_id::create("WATERMARK",,get_full_name());
      this.WATERMARK.configure(this, 15, 0, "RW", 0, 15'h21c, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_cm_usage_CM_SHARED_WM)

endclass : ral_reg_cm_usage_CM_SHARED_WM


class ral_reg_cm_usage_CM_SOFTDROP_WM extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field HOG_SEGMENT_LIMIT;
	rand uvm_reg_field SOFT_DROP_SEGMENT_LIMIT;

	function new(string name = "cm_usage_CM_SOFTDROP_WM");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 34, 30, "RO", 0, 34'h0, 1, 0, 0);
      this.HOG_SEGMENT_LIMIT = uvm_reg_field::type_id::create("HOG_SEGMENT_LIMIT",,get_full_name());
      this.HOG_SEGMENT_LIMIT.configure(this, 15, 15, "RW", 0, 15'h7fff, 1, 0, 0);
      this.SOFT_DROP_SEGMENT_LIMIT = uvm_reg_field::type_id::create("SOFT_DROP_SEGMENT_LIMIT",,get_full_name());
      this.SOFT_DROP_SEGMENT_LIMIT.configure(this, 15, 0, "RW", 0, 15'h7fff, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_cm_usage_CM_SOFTDROP_WM)

endclass : ral_reg_cm_usage_CM_SOFTDROP_WM


class ral_reg_cm_usage_CM_SHARED_SMP_PAUSE_WM extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field PAUSE_OFF;
	rand uvm_reg_field PAUSE_ON;

	function new(string name = "cm_usage_CM_SHARED_SMP_PAUSE_WM");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 34, 30, "RO", 0, 34'h0, 1, 0, 0);
      this.PAUSE_OFF = uvm_reg_field::type_id::create("PAUSE_OFF",,get_full_name());
      this.PAUSE_OFF.configure(this, 15, 15, "RW", 0, 15'h7fff, 1, 0, 0);
      this.PAUSE_ON = uvm_reg_field::type_id::create("PAUSE_ON",,get_full_name());
      this.PAUSE_ON.configure(this, 15, 0, "RW", 0, 15'h7fff, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_cm_usage_CM_SHARED_SMP_PAUSE_WM)

endclass : ral_reg_cm_usage_CM_SHARED_SMP_PAUSE_WM


class ral_reg_cm_usage_CM_GLOBAL_WM extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field WATERMARK;

	function new(string name = "cm_usage_CM_GLOBAL_WM");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 49, 15, "RO", 0, 49'h0, 1, 0, 0);
      this.WATERMARK = uvm_reg_field::type_id::create("WATERMARK",,get_full_name());
      this.WATERMARK.configure(this, 15, 0, "RW", 0, 15'h5e20, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_cm_usage_CM_GLOBAL_WM)

endclass : ral_reg_cm_usage_CM_GLOBAL_WM


class ral_reg_cm_usage_CM_PAUSE_PHYS_PORT_CFG extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field PHYS_PORT;

	function new(string name = "cm_usage_CM_PAUSE_PHYS_PORT_CFG");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 59, 5, "RO", 0, 59'h0, 1, 0, 0);
      this.PHYS_PORT = uvm_reg_field::type_id::create("PHYS_PORT",,get_full_name());
      this.PHYS_PORT.configure(this, 5, 0, "RW", 0, 5'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_cm_usage_CM_PAUSE_PHYS_PORT_CFG)

endclass : ral_reg_cm_usage_CM_PAUSE_PHYS_PORT_CFG


class ral_reg_cm_usage_CM_FORCE_PAUSE_CFG extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field FORCE_OFF;
	rand uvm_reg_field FORCE_ON;

	function new(string name = "cm_usage_CM_FORCE_PAUSE_CFG");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 60, 4, "RO", 0, 60'h0, 1, 0, 0);
      this.FORCE_OFF = uvm_reg_field::type_id::create("FORCE_OFF",,get_full_name());
      this.FORCE_OFF.configure(this, 2, 2, "RW", 0, 2'h0, 1, 0, 0);
      this.FORCE_ON = uvm_reg_field::type_id::create("FORCE_ON",,get_full_name());
      this.FORCE_ON.configure(this, 2, 0, "RW", 0, 2'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_cm_usage_CM_FORCE_PAUSE_CFG)

endclass : ral_reg_cm_usage_CM_FORCE_PAUSE_CFG


class ral_reg_cm_usage_CM_AQM_EWMA_CFG extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field UPDATE_INTERVAL;
	rand uvm_reg_field W;
	rand uvm_reg_field TC;
	rand uvm_reg_field MIN_TH;
	rand uvm_reg_field MAX_TH;

	function new(string name = "cm_usage_CM_AQM_EWMA_CFG");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 19, 45, "RO", 0, 19'h0, 1, 0, 0);
      this.UPDATE_INTERVAL = uvm_reg_field::type_id::create("UPDATE_INTERVAL",,get_full_name());
      this.UPDATE_INTERVAL.configure(this, 8, 37, "RW", 0, 8'h0, 1, 0, 0);
      this.W = uvm_reg_field::type_id::create("W",,get_full_name());
      this.W.configure(this, 4, 33, "RW", 0, 4'h8, 1, 0, 0);
      this.TC = uvm_reg_field::type_id::create("TC",,get_full_name());
      this.TC.configure(this, 3, 30, "RW", 0, 3'h0, 1, 0, 0);
      this.MIN_TH = uvm_reg_field::type_id::create("MIN_TH",,get_full_name());
      this.MIN_TH.configure(this, 15, 15, "RW", 0, 15'h96, 1, 0, 0);
      this.MAX_TH = uvm_reg_field::type_id::create("MAX_TH",,get_full_name());
      this.MAX_TH.configure(this, 15, 0, "RW", 0, 15'h384, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_cm_usage_CM_AQM_EWMA_CFG)

endclass : ral_reg_cm_usage_CM_AQM_EWMA_CFG


class ral_reg_cm_usage_CM_AQM_DCTCP_CFG extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field TC;
	rand uvm_reg_field THRESHOLD;

	function new(string name = "cm_usage_CM_AQM_DCTCP_CFG");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 46, 18, "RO", 0, 46'h0, 1, 0, 0);
      this.TC = uvm_reg_field::type_id::create("TC",,get_full_name());
      this.TC.configure(this, 3, 15, "RW", 0, 3'h0, 1, 0, 0);
      this.THRESHOLD = uvm_reg_field::type_id::create("THRESHOLD",,get_full_name());
      this.THRESHOLD.configure(this, 15, 0, "RW", 0, 15'h7fff, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_cm_usage_CM_AQM_DCTCP_CFG)

endclass : ral_reg_cm_usage_CM_AQM_DCTCP_CFG


class ral_reg_cm_usage_CM_GLOBAL_CFG extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field SWEEPER_EN;
	rand uvm_reg_field NUM_SWEEPER_PORTS;

	function new(string name = "cm_usage_CM_GLOBAL_CFG");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 58, 6, "RO", 0, 58'h0, 1, 0, 0);
      this.SWEEPER_EN = uvm_reg_field::type_id::create("SWEEPER_EN",,get_full_name());
      this.SWEEPER_EN.configure(this, 1, 5, "RW", 0, 1'h0, 1, 0, 0);
      this.NUM_SWEEPER_PORTS = uvm_reg_field::type_id::create("NUM_SWEEPER_PORTS",,get_full_name());
      this.NUM_SWEEPER_PORTS.configure(this, 5, 0, "RW", 0, 5'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_cm_usage_CM_GLOBAL_CFG)

endclass : ral_reg_cm_usage_CM_GLOBAL_CFG


class ral_reg_cm_usage_CM_SHARED_SMP_PAUSE_CFG extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field ENABLE_MASK;

	function new(string name = "cm_usage_CM_SHARED_SMP_PAUSE_CFG");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 40, 24, "RO", 0, 40'h0, 1, 0, 1);
      this.ENABLE_MASK = uvm_reg_field::type_id::create("ENABLE_MASK",,get_full_name());
      this.ENABLE_MASK.configure(this, 24, 0, "RW", 0, 24'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_cm_usage_CM_SHARED_SMP_PAUSE_CFG)

endclass : ral_reg_cm_usage_CM_SHARED_SMP_PAUSE_CFG


class ral_reg_cm_usage_CM_SWEEPER_TC_TO_SMP extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field SMP_7;
	rand uvm_reg_field SMP_6;
	rand uvm_reg_field SMP_5;
	rand uvm_reg_field SMP_4;
	rand uvm_reg_field SMP_3;
	rand uvm_reg_field SMP_2;
	rand uvm_reg_field SMP_1;
	rand uvm_reg_field SMP_0;

	function new(string name = "cm_usage_CM_SWEEPER_TC_TO_SMP");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 56, 8, "RO", 0, 56'h0, 1, 0, 1);
      this.SMP_7 = uvm_reg_field::type_id::create("SMP_7",,get_full_name());
      this.SMP_7.configure(this, 1, 7, "RW", 0, 1'h0, 1, 0, 0);
      this.SMP_6 = uvm_reg_field::type_id::create("SMP_6",,get_full_name());
      this.SMP_6.configure(this, 1, 6, "RW", 0, 1'h0, 1, 0, 0);
      this.SMP_5 = uvm_reg_field::type_id::create("SMP_5",,get_full_name());
      this.SMP_5.configure(this, 1, 5, "RW", 0, 1'h0, 1, 0, 0);
      this.SMP_4 = uvm_reg_field::type_id::create("SMP_4",,get_full_name());
      this.SMP_4.configure(this, 1, 4, "RW", 0, 1'h0, 1, 0, 0);
      this.SMP_3 = uvm_reg_field::type_id::create("SMP_3",,get_full_name());
      this.SMP_3.configure(this, 1, 3, "RW", 0, 1'h0, 1, 0, 0);
      this.SMP_2 = uvm_reg_field::type_id::create("SMP_2",,get_full_name());
      this.SMP_2.configure(this, 1, 2, "RW", 0, 1'h0, 1, 0, 0);
      this.SMP_1 = uvm_reg_field::type_id::create("SMP_1",,get_full_name());
      this.SMP_1.configure(this, 1, 1, "RW", 0, 1'h0, 1, 0, 0);
      this.SMP_0 = uvm_reg_field::type_id::create("SMP_0",,get_full_name());
      this.SMP_0.configure(this, 1, 0, "RW", 0, 1'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_cm_usage_CM_SWEEPER_TC_TO_SMP)

endclass : ral_reg_cm_usage_CM_SWEEPER_TC_TO_SMP


class ral_reg_cm_usage_CM_GLOBAL_USAGE extends uvm_reg;
	uvm_reg_field RSVD0;
	uvm_reg_field COUNT;

	function new(string name = "cm_usage_CM_GLOBAL_USAGE");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 48, 16, "RO", 0, 48'h0, 1, 0, 1);
      this.COUNT = uvm_reg_field::type_id::create("COUNT",,get_full_name());
      this.COUNT.configure(this, 16, 0, "RO", 1, 16'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_cm_usage_CM_GLOBAL_USAGE)

endclass : ral_reg_cm_usage_CM_GLOBAL_USAGE


class ral_reg_cm_usage_CM_GLOBAL_USAGE_MAX extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field COUNT;

	function new(string name = "cm_usage_CM_GLOBAL_USAGE_MAX");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 48, 16, "RO", 0, 48'h0, 1, 0, 1);
      this.COUNT = uvm_reg_field::type_id::create("COUNT",,get_full_name());
      this.COUNT.configure(this, 16, 0, "RW", 1, 16'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_cm_usage_CM_GLOBAL_USAGE_MAX)

endclass : ral_reg_cm_usage_CM_GLOBAL_USAGE_MAX


class ral_reg_cm_usage_CM_MCAST_EPOCH_USAGE extends uvm_reg;
	uvm_reg_field RSVD0;
	uvm_reg_field COUNT;

	function new(string name = "cm_usage_CM_MCAST_EPOCH_USAGE");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 48, 16, "RO", 0, 48'h0, 1, 0, 1);
      this.COUNT = uvm_reg_field::type_id::create("COUNT",,get_full_name());
      this.COUNT.configure(this, 16, 0, "RO", 1, 16'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_cm_usage_CM_MCAST_EPOCH_USAGE)

endclass : ral_reg_cm_usage_CM_MCAST_EPOCH_USAGE


class ral_reg_cm_usage_CM_SHARED_SMP_USAGE extends uvm_reg;
	uvm_reg_field RSVD0;
	uvm_reg_field COUNT;

	function new(string name = "cm_usage_CM_SHARED_SMP_USAGE");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 48, 16, "RO", 0, 48'h0, 1, 0, 1);
      this.COUNT = uvm_reg_field::type_id::create("COUNT",,get_full_name());
      this.COUNT.configure(this, 16, 0, "RO", 1, 16'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_cm_usage_CM_SHARED_SMP_USAGE)

endclass : ral_reg_cm_usage_CM_SHARED_SMP_USAGE


class ral_reg_cm_usage_CM_SMP_USAGE extends uvm_reg;
	uvm_reg_field RSVD0;
	uvm_reg_field COUNT;

	function new(string name = "cm_usage_CM_SMP_USAGE");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 48, 16, "RO", 0, 48'h0, 1, 0, 1);
      this.COUNT = uvm_reg_field::type_id::create("COUNT",,get_full_name());
      this.COUNT.configure(this, 16, 0, "RO", 1, 16'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_cm_usage_CM_SMP_USAGE)

endclass : ral_reg_cm_usage_CM_SMP_USAGE


class ral_reg_cm_usage_CM_RX_SMP_USAGE extends uvm_reg;
	uvm_reg_field RSVD0;
	uvm_reg_field COUNT;

	function new(string name = "cm_usage_CM_RX_SMP_USAGE");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 48, 16, "RO", 0, 48'h0, 1, 0, 1);
      this.COUNT = uvm_reg_field::type_id::create("COUNT",,get_full_name());
      this.COUNT.configure(this, 16, 0, "RO", 1, 16'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_cm_usage_CM_RX_SMP_USAGE)

endclass : ral_reg_cm_usage_CM_RX_SMP_USAGE


class ral_reg_cm_usage_CM_RX_SMP_USAGE_MAX extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field COUNT;

	function new(string name = "cm_usage_CM_RX_SMP_USAGE_MAX");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 48, 16, "RO", 0, 48'h0, 1, 0, 1);
      this.COUNT = uvm_reg_field::type_id::create("COUNT",,get_full_name());
      this.COUNT.configure(this, 16, 0, "RW", 1, 16'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_cm_usage_CM_RX_SMP_USAGE_MAX)

endclass : ral_reg_cm_usage_CM_RX_SMP_USAGE_MAX


class ral_reg_cm_usage_CM_RX_SMP_USAGE_MAX_CTRL extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field PORT3;
	rand uvm_reg_field PORT2;
	rand uvm_reg_field PORT1;
	rand uvm_reg_field PORT0;

	function new(string name = "cm_usage_CM_RX_SMP_USAGE_MAX_CTRL");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 44, 20, "RO", 0, 44'h0, 1, 0, 0);
      this.PORT3 = uvm_reg_field::type_id::create("PORT3",,get_full_name());
      this.PORT3.configure(this, 5, 15, "RW", 0, 5'h0, 1, 0, 0);
      this.PORT2 = uvm_reg_field::type_id::create("PORT2",,get_full_name());
      this.PORT2.configure(this, 5, 10, "RW", 0, 5'h0, 1, 0, 0);
      this.PORT1 = uvm_reg_field::type_id::create("PORT1",,get_full_name());
      this.PORT1.configure(this, 5, 5, "RW", 0, 5'h0, 1, 0, 0);
      this.PORT0 = uvm_reg_field::type_id::create("PORT0",,get_full_name());
      this.PORT0.configure(this, 5, 0, "RW", 0, 5'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_cm_usage_CM_RX_SMP_USAGE_MAX_CTRL)

endclass : ral_reg_cm_usage_CM_RX_SMP_USAGE_MAX_CTRL


class ral_reg_cm_usage_CM_PAUSE_GEN_STATE extends uvm_reg;
	uvm_reg_field RSVD0;
	uvm_reg_field SMP1;
	uvm_reg_field SMP0;

	function new(string name = "cm_usage_CM_PAUSE_GEN_STATE");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 62, 2, "RO", 0, 62'h0, 1, 0, 0);
      this.SMP1 = uvm_reg_field::type_id::create("SMP1",,get_full_name());
      this.SMP1.configure(this, 1, 1, "RO", 1, 1'h0, 1, 0, 0);
      this.SMP0 = uvm_reg_field::type_id::create("SMP0",,get_full_name());
      this.SMP0.configure(this, 1, 0, "RO", 1, 1'h0, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_cm_usage_CM_PAUSE_GEN_STATE)

endclass : ral_reg_cm_usage_CM_PAUSE_GEN_STATE


class ral_reg_cm_usage_CM_TX_TC_USAGE extends uvm_reg;
	uvm_reg_field RSVD0;
	uvm_reg_field COUNT;

	function new(string name = "cm_usage_CM_TX_TC_USAGE");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 48, 16, "RO", 0, 48'h0, 1, 0, 1);
      this.COUNT = uvm_reg_field::type_id::create("COUNT",,get_full_name());
      this.COUNT.configure(this, 16, 0, "RO", 1, 16'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_cm_usage_CM_TX_TC_USAGE)

endclass : ral_reg_cm_usage_CM_TX_TC_USAGE


class ral_reg_cm_usage_CM_TX_EWMA extends uvm_reg;
	uvm_reg_field RSVD0;
	uvm_reg_field INTERVAL;
	uvm_reg_field EWMA_WHOLE;
	uvm_reg_field EWMA_FRAC;

	function new(string name = "cm_usage_CM_TX_EWMA");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 32, 32, "RO", 0, 32'h0, 1, 0, 1);
      this.INTERVAL = uvm_reg_field::type_id::create("INTERVAL",,get_full_name());
      this.INTERVAL.configure(this, 8, 24, "RO", 1, 8'h0, 1, 0, 1);
      this.EWMA_WHOLE = uvm_reg_field::type_id::create("EWMA_WHOLE",,get_full_name());
      this.EWMA_WHOLE.configure(this, 16, 8, "RO", 1, 16'h0, 1, 0, 1);
      this.EWMA_FRAC = uvm_reg_field::type_id::create("EWMA_FRAC",,get_full_name());
      this.EWMA_FRAC.configure(this, 8, 0, "RO", 1, 8'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_cm_usage_CM_TX_EWMA)

endclass : ral_reg_cm_usage_CM_TX_EWMA


class ral_block_cm_usage extends uvm_reg_block;
	rand ral_reg_cm_usage_CM_TX_TC_PRIVATE_WM CM_TX_TC_PRIVATE_WM[64][0:7];
	rand ral_reg_cm_usage_CM_TX_TC_HOG_WM CM_TX_TC_HOG_WM[64][0:7];
	rand ral_reg_cm_usage_CM_RX_SMP_PRIVATE_WM CM_RX_SMP_PRIVATE_WM[32][0:1];
	rand ral_reg_cm_usage_CM_RX_SMP_HOG_WM CM_RX_SMP_HOG_WM[32][0:1];
	rand ral_reg_cm_usage_CM_RX_SMP_PAUSE_WM CM_RX_SMP_PAUSE_WM[32][0:1];
	rand ral_reg_cm_usage_CM_SHARED_WM CM_SHARED_WM[8];
	rand ral_reg_cm_usage_CM_SOFTDROP_WM CM_SOFTDROP_WM[8];
	rand ral_reg_cm_usage_CM_SHARED_SMP_PAUSE_WM CM_SHARED_SMP_PAUSE_WM[2];
	rand ral_reg_cm_usage_CM_GLOBAL_WM CM_GLOBAL_WM;
	rand ral_reg_cm_usage_CM_PAUSE_PHYS_PORT_CFG CM_PAUSE_PHYS_PORT_CFG[32];
	rand ral_reg_cm_usage_CM_FORCE_PAUSE_CFG CM_FORCE_PAUSE_CFG;
	rand ral_reg_cm_usage_CM_AQM_EWMA_CFG CM_AQM_EWMA_CFG[64][0:1];
	rand ral_reg_cm_usage_CM_AQM_DCTCP_CFG CM_AQM_DCTCP_CFG[64][0:1];
	rand ral_reg_cm_usage_CM_GLOBAL_CFG CM_GLOBAL_CFG;
	rand ral_reg_cm_usage_CM_SHARED_SMP_PAUSE_CFG CM_SHARED_SMP_PAUSE_CFG[2];
	rand ral_reg_cm_usage_CM_SWEEPER_TC_TO_SMP CM_SWEEPER_TC_TO_SMP;
	rand ral_reg_cm_usage_CM_GLOBAL_USAGE CM_GLOBAL_USAGE;
	rand ral_reg_cm_usage_CM_GLOBAL_USAGE_MAX CM_GLOBAL_USAGE_MAX;
	rand ral_reg_cm_usage_CM_MCAST_EPOCH_USAGE CM_MCAST_EPOCH_USAGE[2];
	rand ral_reg_cm_usage_CM_SHARED_SMP_USAGE CM_SHARED_SMP_USAGE[2];
	rand ral_reg_cm_usage_CM_SMP_USAGE CM_SMP_USAGE[2];
	rand ral_reg_cm_usage_CM_RX_SMP_USAGE CM_RX_SMP_USAGE[32][0:1];
	rand ral_reg_cm_usage_CM_RX_SMP_USAGE_MAX CM_RX_SMP_USAGE_MAX[4][0:1];
	rand ral_reg_cm_usage_CM_RX_SMP_USAGE_MAX_CTRL CM_RX_SMP_USAGE_MAX_CTRL;
	rand ral_reg_cm_usage_CM_PAUSE_GEN_STATE CM_PAUSE_GEN_STATE[32];
	rand ral_reg_cm_usage_CM_TX_TC_USAGE CM_TX_TC_USAGE[64][0:7];
	rand ral_reg_cm_usage_CM_TX_EWMA CM_TX_EWMA[64][0:1];
	uvm_reg_field CM_TX_TC_PRIVATE_WM_RSVD0[64][0:7];
	rand uvm_reg_field CM_TX_TC_PRIVATE_WM_WATERMARK[64][0:7];
	uvm_reg_field CM_TX_TC_HOG_WM_RSVD0[64][0:7];
	rand uvm_reg_field CM_TX_TC_HOG_WM_WATERMARK[64][0:7];
	uvm_reg_field CM_RX_SMP_PRIVATE_WM_RSVD0[32][0:1];
	rand uvm_reg_field CM_RX_SMP_PRIVATE_WM_WATERMARK[32][0:1];
	uvm_reg_field CM_RX_SMP_HOG_WM_RSVD0[32][0:1];
	rand uvm_reg_field CM_RX_SMP_HOG_WM_WATERMARK[32][0:1];
	uvm_reg_field CM_RX_SMP_PAUSE_WM_RSVD0[32][0:1];
	rand uvm_reg_field CM_RX_SMP_PAUSE_WM_PAUSE_OFF[32][0:1];
	rand uvm_reg_field CM_RX_SMP_PAUSE_WM_PAUSE_ON[32][0:1];
	uvm_reg_field CM_SHARED_WM_RSVD0[8];
	rand uvm_reg_field CM_SHARED_WM_WATERMARK[8];
	uvm_reg_field CM_SOFTDROP_WM_RSVD0[8];
	rand uvm_reg_field CM_SOFTDROP_WM_HOG_SEGMENT_LIMIT[8];
	rand uvm_reg_field HOG_SEGMENT_LIMIT[8];
	rand uvm_reg_field CM_SOFTDROP_WM_SOFT_DROP_SEGMENT_LIMIT[8];
	rand uvm_reg_field SOFT_DROP_SEGMENT_LIMIT[8];
	uvm_reg_field CM_SHARED_SMP_PAUSE_WM_RSVD0[2];
	rand uvm_reg_field CM_SHARED_SMP_PAUSE_WM_PAUSE_OFF[2];
	rand uvm_reg_field CM_SHARED_SMP_PAUSE_WM_PAUSE_ON[2];
	uvm_reg_field CM_GLOBAL_WM_RSVD0;
	rand uvm_reg_field CM_GLOBAL_WM_WATERMARK;
	uvm_reg_field CM_PAUSE_PHYS_PORT_CFG_RSVD0[32];
	rand uvm_reg_field CM_PAUSE_PHYS_PORT_CFG_PHYS_PORT[32];
	rand uvm_reg_field PHYS_PORT[32];
	uvm_reg_field CM_FORCE_PAUSE_CFG_RSVD0;
	rand uvm_reg_field CM_FORCE_PAUSE_CFG_FORCE_OFF;
	rand uvm_reg_field FORCE_OFF;
	rand uvm_reg_field CM_FORCE_PAUSE_CFG_FORCE_ON;
	rand uvm_reg_field FORCE_ON;
	uvm_reg_field CM_AQM_EWMA_CFG_RSVD0[64][0:1];
	rand uvm_reg_field CM_AQM_EWMA_CFG_UPDATE_INTERVAL[64][0:1];
	rand uvm_reg_field UPDATE_INTERVAL[64][0:1];
	rand uvm_reg_field CM_AQM_EWMA_CFG_W[64][0:1];
	rand uvm_reg_field W[64][0:1];
	rand uvm_reg_field CM_AQM_EWMA_CFG_TC[64][0:1];
	rand uvm_reg_field CM_AQM_EWMA_CFG_MIN_TH[64][0:1];
	rand uvm_reg_field MIN_TH[64][0:1];
	rand uvm_reg_field CM_AQM_EWMA_CFG_MAX_TH[64][0:1];
	rand uvm_reg_field MAX_TH[64][0:1];
	uvm_reg_field CM_AQM_DCTCP_CFG_RSVD0[64][0:1];
	rand uvm_reg_field CM_AQM_DCTCP_CFG_TC[64][0:1];
	rand uvm_reg_field CM_AQM_DCTCP_CFG_THRESHOLD[64][0:1];
	rand uvm_reg_field THRESHOLD[64][0:1];
	uvm_reg_field CM_GLOBAL_CFG_RSVD0;
	rand uvm_reg_field CM_GLOBAL_CFG_SWEEPER_EN;
	rand uvm_reg_field SWEEPER_EN;
	rand uvm_reg_field CM_GLOBAL_CFG_NUM_SWEEPER_PORTS;
	rand uvm_reg_field NUM_SWEEPER_PORTS;
	uvm_reg_field CM_SHARED_SMP_PAUSE_CFG_RSVD0[2];
	rand uvm_reg_field CM_SHARED_SMP_PAUSE_CFG_ENABLE_MASK[2];
	rand uvm_reg_field ENABLE_MASK[2];
	uvm_reg_field CM_SWEEPER_TC_TO_SMP_RSVD0;
	rand uvm_reg_field CM_SWEEPER_TC_TO_SMP_SMP_7;
	rand uvm_reg_field SMP_7;
	rand uvm_reg_field CM_SWEEPER_TC_TO_SMP_SMP_6;
	rand uvm_reg_field SMP_6;
	rand uvm_reg_field CM_SWEEPER_TC_TO_SMP_SMP_5;
	rand uvm_reg_field SMP_5;
	rand uvm_reg_field CM_SWEEPER_TC_TO_SMP_SMP_4;
	rand uvm_reg_field SMP_4;
	rand uvm_reg_field CM_SWEEPER_TC_TO_SMP_SMP_3;
	rand uvm_reg_field SMP_3;
	rand uvm_reg_field CM_SWEEPER_TC_TO_SMP_SMP_2;
	rand uvm_reg_field SMP_2;
	rand uvm_reg_field CM_SWEEPER_TC_TO_SMP_SMP_1;
	rand uvm_reg_field SMP_1;
	rand uvm_reg_field CM_SWEEPER_TC_TO_SMP_SMP_0;
	rand uvm_reg_field SMP_0;
	uvm_reg_field CM_GLOBAL_USAGE_RSVD0;
	uvm_reg_field CM_GLOBAL_USAGE_COUNT;
	uvm_reg_field CM_GLOBAL_USAGE_MAX_RSVD0;
	rand uvm_reg_field CM_GLOBAL_USAGE_MAX_COUNT;
	uvm_reg_field CM_MCAST_EPOCH_USAGE_RSVD0[2];
	uvm_reg_field CM_MCAST_EPOCH_USAGE_COUNT[2];
	uvm_reg_field CM_SHARED_SMP_USAGE_RSVD0[2];
	uvm_reg_field CM_SHARED_SMP_USAGE_COUNT[2];
	uvm_reg_field CM_SMP_USAGE_RSVD0[2];
	uvm_reg_field CM_SMP_USAGE_COUNT[2];
	uvm_reg_field CM_RX_SMP_USAGE_RSVD0[32][0:1];
	uvm_reg_field CM_RX_SMP_USAGE_COUNT[32][0:1];
	uvm_reg_field CM_RX_SMP_USAGE_MAX_RSVD0[4][0:1];
	rand uvm_reg_field CM_RX_SMP_USAGE_MAX_COUNT[4][0:1];
	uvm_reg_field CM_RX_SMP_USAGE_MAX_CTRL_RSVD0;
	rand uvm_reg_field CM_RX_SMP_USAGE_MAX_CTRL_PORT3;
	rand uvm_reg_field PORT3;
	rand uvm_reg_field CM_RX_SMP_USAGE_MAX_CTRL_PORT2;
	rand uvm_reg_field PORT2;
	rand uvm_reg_field CM_RX_SMP_USAGE_MAX_CTRL_PORT1;
	rand uvm_reg_field PORT1;
	rand uvm_reg_field CM_RX_SMP_USAGE_MAX_CTRL_PORT0;
	rand uvm_reg_field PORT0;
	uvm_reg_field CM_PAUSE_GEN_STATE_RSVD0[32];
	uvm_reg_field CM_PAUSE_GEN_STATE_SMP1[32];
	uvm_reg_field SMP1[32];
	uvm_reg_field CM_PAUSE_GEN_STATE_SMP0[32];
	uvm_reg_field SMP0[32];
	uvm_reg_field CM_TX_TC_USAGE_RSVD0[64][0:7];
	uvm_reg_field CM_TX_TC_USAGE_COUNT[64][0:7];
	uvm_reg_field CM_TX_EWMA_RSVD0[64][0:1];
	uvm_reg_field CM_TX_EWMA_INTERVAL[64][0:1];
	uvm_reg_field INTERVAL[64][0:1];
	uvm_reg_field CM_TX_EWMA_EWMA_WHOLE[64][0:1];
	uvm_reg_field EWMA_WHOLE[64][0:1];
	uvm_reg_field CM_TX_EWMA_EWMA_FRAC[64][0:1];
	uvm_reg_field EWMA_FRAC[64][0:1];

	function new(string name = "cm_usage");
		super.new(name, build_coverage(UVM_NO_COVERAGE));
	endfunction: new

   virtual function void build();
      this.default_map = create_map("", 0, 8, UVM_LITTLE_ENDIAN, 0);
      foreach (this.CM_TX_TC_PRIVATE_WM[i,j]) begin
         int J = i;
         int K = j;
         this.CM_TX_TC_PRIVATE_WM[J][K] = ral_reg_cm_usage_CM_TX_TC_PRIVATE_WM::type_id::create($psprintf("CM_TX_TC_PRIVATE_WM[%0d][%0d]",J,K),,get_full_name());
         this.CM_TX_TC_PRIVATE_WM[J][K].configure(this, null, "");
         this.CM_TX_TC_PRIVATE_WM[J][K].build();
         this.CM_TX_TC_PRIVATE_WM[J][K].add_hdl_path('{

            '{$psprintf("CM_TX_TC_PRIVATE_WM[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.CM_TX_TC_PRIVATE_WM[J][K], `UVM_REG_ADDR_WIDTH'h0+J*`UVM_REG_ADDR_WIDTH'h40+K*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.CM_TX_TC_PRIVATE_WM_RSVD0[J][K] = this.CM_TX_TC_PRIVATE_WM[J][K].RSVD0;
			this.CM_TX_TC_PRIVATE_WM_WATERMARK[J][K] = this.CM_TX_TC_PRIVATE_WM[J][K].WATERMARK;
      end
      foreach (this.CM_TX_TC_HOG_WM[i,j]) begin
         int J = i;
         int K = j;
         this.CM_TX_TC_HOG_WM[J][K] = ral_reg_cm_usage_CM_TX_TC_HOG_WM::type_id::create($psprintf("CM_TX_TC_HOG_WM[%0d][%0d]",J,K),,get_full_name());
         this.CM_TX_TC_HOG_WM[J][K].configure(this, null, "");
         this.CM_TX_TC_HOG_WM[J][K].build();
         this.CM_TX_TC_HOG_WM[J][K].add_hdl_path('{

            '{$psprintf("CM_TX_TC_HOG_WM[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.CM_TX_TC_HOG_WM[J][K], `UVM_REG_ADDR_WIDTH'h1000+J*`UVM_REG_ADDR_WIDTH'h40+K*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.CM_TX_TC_HOG_WM_RSVD0[J][K] = this.CM_TX_TC_HOG_WM[J][K].RSVD0;
			this.CM_TX_TC_HOG_WM_WATERMARK[J][K] = this.CM_TX_TC_HOG_WM[J][K].WATERMARK;
      end
      foreach (this.CM_RX_SMP_PRIVATE_WM[i,j]) begin
         int J = i;
         int K = j;
         this.CM_RX_SMP_PRIVATE_WM[J][K] = ral_reg_cm_usage_CM_RX_SMP_PRIVATE_WM::type_id::create($psprintf("CM_RX_SMP_PRIVATE_WM[%0d][%0d]",J,K),,get_full_name());
         this.CM_RX_SMP_PRIVATE_WM[J][K].configure(this, null, "");
         this.CM_RX_SMP_PRIVATE_WM[J][K].build();
         this.CM_RX_SMP_PRIVATE_WM[J][K].add_hdl_path('{

            '{$psprintf("CM_RX_SMP_PRIVATE_WM[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.CM_RX_SMP_PRIVATE_WM[J][K], `UVM_REG_ADDR_WIDTH'h2000+J*`UVM_REG_ADDR_WIDTH'h10+K*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.CM_RX_SMP_PRIVATE_WM_RSVD0[J][K] = this.CM_RX_SMP_PRIVATE_WM[J][K].RSVD0;
			this.CM_RX_SMP_PRIVATE_WM_WATERMARK[J][K] = this.CM_RX_SMP_PRIVATE_WM[J][K].WATERMARK;
      end
      foreach (this.CM_RX_SMP_HOG_WM[i,j]) begin
         int J = i;
         int K = j;
         this.CM_RX_SMP_HOG_WM[J][K] = ral_reg_cm_usage_CM_RX_SMP_HOG_WM::type_id::create($psprintf("CM_RX_SMP_HOG_WM[%0d][%0d]",J,K),,get_full_name());
         this.CM_RX_SMP_HOG_WM[J][K].configure(this, null, "");
         this.CM_RX_SMP_HOG_WM[J][K].build();
         this.CM_RX_SMP_HOG_WM[J][K].add_hdl_path('{

            '{$psprintf("CM_RX_SMP_HOG_WM[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.CM_RX_SMP_HOG_WM[J][K], `UVM_REG_ADDR_WIDTH'h2200+J*`UVM_REG_ADDR_WIDTH'h10+K*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.CM_RX_SMP_HOG_WM_RSVD0[J][K] = this.CM_RX_SMP_HOG_WM[J][K].RSVD0;
			this.CM_RX_SMP_HOG_WM_WATERMARK[J][K] = this.CM_RX_SMP_HOG_WM[J][K].WATERMARK;
      end
      foreach (this.CM_RX_SMP_PAUSE_WM[i,j]) begin
         int J = i;
         int K = j;
         this.CM_RX_SMP_PAUSE_WM[J][K] = ral_reg_cm_usage_CM_RX_SMP_PAUSE_WM::type_id::create($psprintf("CM_RX_SMP_PAUSE_WM[%0d][%0d]",J,K),,get_full_name());
         this.CM_RX_SMP_PAUSE_WM[J][K].configure(this, null, "");
         this.CM_RX_SMP_PAUSE_WM[J][K].build();
         this.CM_RX_SMP_PAUSE_WM[J][K].add_hdl_path('{

            '{$psprintf("CM_RX_SMP_PAUSE_WM[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.CM_RX_SMP_PAUSE_WM[J][K], `UVM_REG_ADDR_WIDTH'h2400+J*`UVM_REG_ADDR_WIDTH'h10+K*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.CM_RX_SMP_PAUSE_WM_RSVD0[J][K] = this.CM_RX_SMP_PAUSE_WM[J][K].RSVD0;
			this.CM_RX_SMP_PAUSE_WM_PAUSE_OFF[J][K] = this.CM_RX_SMP_PAUSE_WM[J][K].PAUSE_OFF;
			this.CM_RX_SMP_PAUSE_WM_PAUSE_ON[J][K] = this.CM_RX_SMP_PAUSE_WM[J][K].PAUSE_ON;
      end
      foreach (this.CM_SHARED_WM[i]) begin
         int J = i;
         this.CM_SHARED_WM[J] = ral_reg_cm_usage_CM_SHARED_WM::type_id::create($psprintf("CM_SHARED_WM[%0d]",J),,get_full_name());
         this.CM_SHARED_WM[J].configure(this, null, "");
         this.CM_SHARED_WM[J].build();
         this.CM_SHARED_WM[J].add_hdl_path('{

            '{$psprintf("CM_SHARED_WM[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.CM_SHARED_WM[J], `UVM_REG_ADDR_WIDTH'h2600+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.CM_SHARED_WM_RSVD0[J] = this.CM_SHARED_WM[J].RSVD0;
			this.CM_SHARED_WM_WATERMARK[J] = this.CM_SHARED_WM[J].WATERMARK;
      end
      foreach (this.CM_SOFTDROP_WM[i]) begin
         int J = i;
         this.CM_SOFTDROP_WM[J] = ral_reg_cm_usage_CM_SOFTDROP_WM::type_id::create($psprintf("CM_SOFTDROP_WM[%0d]",J),,get_full_name());
         this.CM_SOFTDROP_WM[J].configure(this, null, "");
         this.CM_SOFTDROP_WM[J].build();
         this.CM_SOFTDROP_WM[J].add_hdl_path('{

            '{$psprintf("CM_SOFTDROP_WM[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.CM_SOFTDROP_WM[J], `UVM_REG_ADDR_WIDTH'h2640+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.CM_SOFTDROP_WM_RSVD0[J] = this.CM_SOFTDROP_WM[J].RSVD0;
			this.CM_SOFTDROP_WM_HOG_SEGMENT_LIMIT[J] = this.CM_SOFTDROP_WM[J].HOG_SEGMENT_LIMIT;
			this.HOG_SEGMENT_LIMIT[J] = this.CM_SOFTDROP_WM[J].HOG_SEGMENT_LIMIT;
			this.CM_SOFTDROP_WM_SOFT_DROP_SEGMENT_LIMIT[J] = this.CM_SOFTDROP_WM[J].SOFT_DROP_SEGMENT_LIMIT;
			this.SOFT_DROP_SEGMENT_LIMIT[J] = this.CM_SOFTDROP_WM[J].SOFT_DROP_SEGMENT_LIMIT;
      end
      foreach (this.CM_SHARED_SMP_PAUSE_WM[i]) begin
         int J = i;
         this.CM_SHARED_SMP_PAUSE_WM[J] = ral_reg_cm_usage_CM_SHARED_SMP_PAUSE_WM::type_id::create($psprintf("CM_SHARED_SMP_PAUSE_WM[%0d]",J),,get_full_name());
         this.CM_SHARED_SMP_PAUSE_WM[J].configure(this, null, "");
         this.CM_SHARED_SMP_PAUSE_WM[J].build();
         this.CM_SHARED_SMP_PAUSE_WM[J].add_hdl_path('{

            '{$psprintf("CM_SHARED_SMP_PAUSE_WM[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.CM_SHARED_SMP_PAUSE_WM[J], `UVM_REG_ADDR_WIDTH'h2680+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.CM_SHARED_SMP_PAUSE_WM_RSVD0[J] = this.CM_SHARED_SMP_PAUSE_WM[J].RSVD0;
			this.CM_SHARED_SMP_PAUSE_WM_PAUSE_OFF[J] = this.CM_SHARED_SMP_PAUSE_WM[J].PAUSE_OFF;
			this.CM_SHARED_SMP_PAUSE_WM_PAUSE_ON[J] = this.CM_SHARED_SMP_PAUSE_WM[J].PAUSE_ON;
      end
      this.CM_GLOBAL_WM = ral_reg_cm_usage_CM_GLOBAL_WM::type_id::create("CM_GLOBAL_WM",,get_full_name());
      this.CM_GLOBAL_WM.configure(this, null, "");
      this.CM_GLOBAL_WM.build();
         this.CM_GLOBAL_WM.add_hdl_path('{

            '{"CM_GLOBAL_WM", -1, -1}
         });
      this.default_map.add_reg(this.CM_GLOBAL_WM, `UVM_REG_ADDR_WIDTH'h2690, "RW", 0);
		this.CM_GLOBAL_WM_RSVD0 = this.CM_GLOBAL_WM.RSVD0;
		this.CM_GLOBAL_WM_WATERMARK = this.CM_GLOBAL_WM.WATERMARK;
      foreach (this.CM_PAUSE_PHYS_PORT_CFG[i]) begin
         int J = i;
         this.CM_PAUSE_PHYS_PORT_CFG[J] = ral_reg_cm_usage_CM_PAUSE_PHYS_PORT_CFG::type_id::create($psprintf("CM_PAUSE_PHYS_PORT_CFG[%0d]",J),,get_full_name());
         this.CM_PAUSE_PHYS_PORT_CFG[J].configure(this, null, "");
         this.CM_PAUSE_PHYS_PORT_CFG[J].build();
         this.CM_PAUSE_PHYS_PORT_CFG[J].add_hdl_path('{

            '{$psprintf("CM_PAUSE_PHYS_PORT_CFG[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.CM_PAUSE_PHYS_PORT_CFG[J], `UVM_REG_ADDR_WIDTH'h2700+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.CM_PAUSE_PHYS_PORT_CFG_RSVD0[J] = this.CM_PAUSE_PHYS_PORT_CFG[J].RSVD0;
			this.CM_PAUSE_PHYS_PORT_CFG_PHYS_PORT[J] = this.CM_PAUSE_PHYS_PORT_CFG[J].PHYS_PORT;
			this.PHYS_PORT[J] = this.CM_PAUSE_PHYS_PORT_CFG[J].PHYS_PORT;
      end
      this.CM_FORCE_PAUSE_CFG = ral_reg_cm_usage_CM_FORCE_PAUSE_CFG::type_id::create("CM_FORCE_PAUSE_CFG",,get_full_name());
      this.CM_FORCE_PAUSE_CFG.configure(this, null, "");
      this.CM_FORCE_PAUSE_CFG.build();
         this.CM_FORCE_PAUSE_CFG.add_hdl_path('{

            '{"CM_FORCE_PAUSE_CFG", -1, -1}
         });
      this.default_map.add_reg(this.CM_FORCE_PAUSE_CFG, `UVM_REG_ADDR_WIDTH'h2800, "RW", 0);
		this.CM_FORCE_PAUSE_CFG_RSVD0 = this.CM_FORCE_PAUSE_CFG.RSVD0;
		this.CM_FORCE_PAUSE_CFG_FORCE_OFF = this.CM_FORCE_PAUSE_CFG.FORCE_OFF;
		this.FORCE_OFF = this.CM_FORCE_PAUSE_CFG.FORCE_OFF;
		this.CM_FORCE_PAUSE_CFG_FORCE_ON = this.CM_FORCE_PAUSE_CFG.FORCE_ON;
		this.FORCE_ON = this.CM_FORCE_PAUSE_CFG.FORCE_ON;
      foreach (this.CM_AQM_EWMA_CFG[i,j]) begin
         int J = i;
         int K = j;
         this.CM_AQM_EWMA_CFG[J][K] = ral_reg_cm_usage_CM_AQM_EWMA_CFG::type_id::create($psprintf("CM_AQM_EWMA_CFG[%0d][%0d]",J,K),,get_full_name());
         this.CM_AQM_EWMA_CFG[J][K].configure(this, null, "");
         this.CM_AQM_EWMA_CFG[J][K].build();
         this.CM_AQM_EWMA_CFG[J][K].add_hdl_path('{

            '{$psprintf("CM_AQM_EWMA_CFG[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.CM_AQM_EWMA_CFG[J][K], `UVM_REG_ADDR_WIDTH'h2C00+J*`UVM_REG_ADDR_WIDTH'h10+K*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.CM_AQM_EWMA_CFG_RSVD0[J][K] = this.CM_AQM_EWMA_CFG[J][K].RSVD0;
			this.CM_AQM_EWMA_CFG_UPDATE_INTERVAL[J][K] = this.CM_AQM_EWMA_CFG[J][K].UPDATE_INTERVAL;
			this.UPDATE_INTERVAL[J][K] = this.CM_AQM_EWMA_CFG[J][K].UPDATE_INTERVAL;
			this.CM_AQM_EWMA_CFG_W[J][K] = this.CM_AQM_EWMA_CFG[J][K].W;
			this.W[J][K] = this.CM_AQM_EWMA_CFG[J][K].W;
			this.CM_AQM_EWMA_CFG_TC[J][K] = this.CM_AQM_EWMA_CFG[J][K].TC;
			this.CM_AQM_EWMA_CFG_MIN_TH[J][K] = this.CM_AQM_EWMA_CFG[J][K].MIN_TH;
			this.MIN_TH[J][K] = this.CM_AQM_EWMA_CFG[J][K].MIN_TH;
			this.CM_AQM_EWMA_CFG_MAX_TH[J][K] = this.CM_AQM_EWMA_CFG[J][K].MAX_TH;
			this.MAX_TH[J][K] = this.CM_AQM_EWMA_CFG[J][K].MAX_TH;
      end
      foreach (this.CM_AQM_DCTCP_CFG[i,j]) begin
         int J = i;
         int K = j;
         this.CM_AQM_DCTCP_CFG[J][K] = ral_reg_cm_usage_CM_AQM_DCTCP_CFG::type_id::create($psprintf("CM_AQM_DCTCP_CFG[%0d][%0d]",J,K),,get_full_name());
         this.CM_AQM_DCTCP_CFG[J][K].configure(this, null, "");
         this.CM_AQM_DCTCP_CFG[J][K].build();
         this.CM_AQM_DCTCP_CFG[J][K].add_hdl_path('{

            '{$psprintf("CM_AQM_DCTCP_CFG[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.CM_AQM_DCTCP_CFG[J][K], `UVM_REG_ADDR_WIDTH'h3000+J*`UVM_REG_ADDR_WIDTH'h10+K*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.CM_AQM_DCTCP_CFG_RSVD0[J][K] = this.CM_AQM_DCTCP_CFG[J][K].RSVD0;
			this.CM_AQM_DCTCP_CFG_TC[J][K] = this.CM_AQM_DCTCP_CFG[J][K].TC;
			this.CM_AQM_DCTCP_CFG_THRESHOLD[J][K] = this.CM_AQM_DCTCP_CFG[J][K].THRESHOLD;
			this.THRESHOLD[J][K] = this.CM_AQM_DCTCP_CFG[J][K].THRESHOLD;
      end
      this.CM_GLOBAL_CFG = ral_reg_cm_usage_CM_GLOBAL_CFG::type_id::create("CM_GLOBAL_CFG",,get_full_name());
      this.CM_GLOBAL_CFG.configure(this, null, "");
      this.CM_GLOBAL_CFG.build();
         this.CM_GLOBAL_CFG.add_hdl_path('{

            '{"CM_GLOBAL_CFG", -1, -1}
         });
      this.default_map.add_reg(this.CM_GLOBAL_CFG, `UVM_REG_ADDR_WIDTH'h3400, "RW", 0);
		this.CM_GLOBAL_CFG_RSVD0 = this.CM_GLOBAL_CFG.RSVD0;
		this.CM_GLOBAL_CFG_SWEEPER_EN = this.CM_GLOBAL_CFG.SWEEPER_EN;
		this.SWEEPER_EN = this.CM_GLOBAL_CFG.SWEEPER_EN;
		this.CM_GLOBAL_CFG_NUM_SWEEPER_PORTS = this.CM_GLOBAL_CFG.NUM_SWEEPER_PORTS;
		this.NUM_SWEEPER_PORTS = this.CM_GLOBAL_CFG.NUM_SWEEPER_PORTS;
      foreach (this.CM_SHARED_SMP_PAUSE_CFG[i]) begin
         int J = i;
         this.CM_SHARED_SMP_PAUSE_CFG[J] = ral_reg_cm_usage_CM_SHARED_SMP_PAUSE_CFG::type_id::create($psprintf("CM_SHARED_SMP_PAUSE_CFG[%0d]",J),,get_full_name());
         this.CM_SHARED_SMP_PAUSE_CFG[J].configure(this, null, "");
         this.CM_SHARED_SMP_PAUSE_CFG[J].build();
         this.CM_SHARED_SMP_PAUSE_CFG[J].add_hdl_path('{

            '{$psprintf("CM_SHARED_SMP_PAUSE_CFG[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.CM_SHARED_SMP_PAUSE_CFG[J], `UVM_REG_ADDR_WIDTH'h3410+J*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.CM_SHARED_SMP_PAUSE_CFG_RSVD0[J] = this.CM_SHARED_SMP_PAUSE_CFG[J].RSVD0;
			this.CM_SHARED_SMP_PAUSE_CFG_ENABLE_MASK[J] = this.CM_SHARED_SMP_PAUSE_CFG[J].ENABLE_MASK;
			this.ENABLE_MASK[J] = this.CM_SHARED_SMP_PAUSE_CFG[J].ENABLE_MASK;
      end
      this.CM_SWEEPER_TC_TO_SMP = ral_reg_cm_usage_CM_SWEEPER_TC_TO_SMP::type_id::create("CM_SWEEPER_TC_TO_SMP",,get_full_name());
      this.CM_SWEEPER_TC_TO_SMP.configure(this, null, "");
      this.CM_SWEEPER_TC_TO_SMP.build();
         this.CM_SWEEPER_TC_TO_SMP.add_hdl_path('{

            '{"CM_SWEEPER_TC_TO_SMP", -1, -1}
         });
      this.default_map.add_reg(this.CM_SWEEPER_TC_TO_SMP, `UVM_REG_ADDR_WIDTH'h3420, "RW", 0);
		this.CM_SWEEPER_TC_TO_SMP_RSVD0 = this.CM_SWEEPER_TC_TO_SMP.RSVD0;
		this.CM_SWEEPER_TC_TO_SMP_SMP_7 = this.CM_SWEEPER_TC_TO_SMP.SMP_7;
		this.SMP_7 = this.CM_SWEEPER_TC_TO_SMP.SMP_7;
		this.CM_SWEEPER_TC_TO_SMP_SMP_6 = this.CM_SWEEPER_TC_TO_SMP.SMP_6;
		this.SMP_6 = this.CM_SWEEPER_TC_TO_SMP.SMP_6;
		this.CM_SWEEPER_TC_TO_SMP_SMP_5 = this.CM_SWEEPER_TC_TO_SMP.SMP_5;
		this.SMP_5 = this.CM_SWEEPER_TC_TO_SMP.SMP_5;
		this.CM_SWEEPER_TC_TO_SMP_SMP_4 = this.CM_SWEEPER_TC_TO_SMP.SMP_4;
		this.SMP_4 = this.CM_SWEEPER_TC_TO_SMP.SMP_4;
		this.CM_SWEEPER_TC_TO_SMP_SMP_3 = this.CM_SWEEPER_TC_TO_SMP.SMP_3;
		this.SMP_3 = this.CM_SWEEPER_TC_TO_SMP.SMP_3;
		this.CM_SWEEPER_TC_TO_SMP_SMP_2 = this.CM_SWEEPER_TC_TO_SMP.SMP_2;
		this.SMP_2 = this.CM_SWEEPER_TC_TO_SMP.SMP_2;
		this.CM_SWEEPER_TC_TO_SMP_SMP_1 = this.CM_SWEEPER_TC_TO_SMP.SMP_1;
		this.SMP_1 = this.CM_SWEEPER_TC_TO_SMP.SMP_1;
		this.CM_SWEEPER_TC_TO_SMP_SMP_0 = this.CM_SWEEPER_TC_TO_SMP.SMP_0;
		this.SMP_0 = this.CM_SWEEPER_TC_TO_SMP.SMP_0;
      this.CM_GLOBAL_USAGE = ral_reg_cm_usage_CM_GLOBAL_USAGE::type_id::create("CM_GLOBAL_USAGE",,get_full_name());
      this.CM_GLOBAL_USAGE.configure(this, null, "");
      this.CM_GLOBAL_USAGE.build();
         this.CM_GLOBAL_USAGE.add_hdl_path('{

            '{"CM_GLOBAL_USAGE", -1, -1}
         });
      this.default_map.add_reg(this.CM_GLOBAL_USAGE, `UVM_REG_ADDR_WIDTH'h3428, "RO", 0);
		this.CM_GLOBAL_USAGE_RSVD0 = this.CM_GLOBAL_USAGE.RSVD0;
		this.CM_GLOBAL_USAGE_COUNT = this.CM_GLOBAL_USAGE.COUNT;
      this.CM_GLOBAL_USAGE_MAX = ral_reg_cm_usage_CM_GLOBAL_USAGE_MAX::type_id::create("CM_GLOBAL_USAGE_MAX",,get_full_name());
      this.CM_GLOBAL_USAGE_MAX.configure(this, null, "");
      this.CM_GLOBAL_USAGE_MAX.build();
         this.CM_GLOBAL_USAGE_MAX.add_hdl_path('{

            '{"CM_GLOBAL_USAGE_MAX", -1, -1}
         });
      this.default_map.add_reg(this.CM_GLOBAL_USAGE_MAX, `UVM_REG_ADDR_WIDTH'h3430, "RW", 0);
		this.CM_GLOBAL_USAGE_MAX_RSVD0 = this.CM_GLOBAL_USAGE_MAX.RSVD0;
		this.CM_GLOBAL_USAGE_MAX_COUNT = this.CM_GLOBAL_USAGE_MAX.COUNT;
      foreach (this.CM_MCAST_EPOCH_USAGE[i]) begin
         int J = i;
         this.CM_MCAST_EPOCH_USAGE[J] = ral_reg_cm_usage_CM_MCAST_EPOCH_USAGE::type_id::create($psprintf("CM_MCAST_EPOCH_USAGE[%0d]",J),,get_full_name());
         this.CM_MCAST_EPOCH_USAGE[J].configure(this, null, "");
         this.CM_MCAST_EPOCH_USAGE[J].build();
         this.CM_MCAST_EPOCH_USAGE[J].add_hdl_path('{

            '{$psprintf("CM_MCAST_EPOCH_USAGE[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.CM_MCAST_EPOCH_USAGE[J], `UVM_REG_ADDR_WIDTH'h3440+J*`UVM_REG_ADDR_WIDTH'h8, "RO", 0);
			this.CM_MCAST_EPOCH_USAGE_RSVD0[J] = this.CM_MCAST_EPOCH_USAGE[J].RSVD0;
			this.CM_MCAST_EPOCH_USAGE_COUNT[J] = this.CM_MCAST_EPOCH_USAGE[J].COUNT;
      end
      foreach (this.CM_SHARED_SMP_USAGE[i]) begin
         int J = i;
         this.CM_SHARED_SMP_USAGE[J] = ral_reg_cm_usage_CM_SHARED_SMP_USAGE::type_id::create($psprintf("CM_SHARED_SMP_USAGE[%0d]",J),,get_full_name());
         this.CM_SHARED_SMP_USAGE[J].configure(this, null, "");
         this.CM_SHARED_SMP_USAGE[J].build();
         this.CM_SHARED_SMP_USAGE[J].add_hdl_path('{

            '{$psprintf("CM_SHARED_SMP_USAGE[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.CM_SHARED_SMP_USAGE[J], `UVM_REG_ADDR_WIDTH'h3450+J*`UVM_REG_ADDR_WIDTH'h8, "RO", 0);
			this.CM_SHARED_SMP_USAGE_RSVD0[J] = this.CM_SHARED_SMP_USAGE[J].RSVD0;
			this.CM_SHARED_SMP_USAGE_COUNT[J] = this.CM_SHARED_SMP_USAGE[J].COUNT;
      end
      foreach (this.CM_SMP_USAGE[i]) begin
         int J = i;
         this.CM_SMP_USAGE[J] = ral_reg_cm_usage_CM_SMP_USAGE::type_id::create($psprintf("CM_SMP_USAGE[%0d]",J),,get_full_name());
         this.CM_SMP_USAGE[J].configure(this, null, "");
         this.CM_SMP_USAGE[J].build();
         this.CM_SMP_USAGE[J].add_hdl_path('{

            '{$psprintf("CM_SMP_USAGE[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.CM_SMP_USAGE[J], `UVM_REG_ADDR_WIDTH'h3460+J*`UVM_REG_ADDR_WIDTH'h8, "RO", 0);
			this.CM_SMP_USAGE_RSVD0[J] = this.CM_SMP_USAGE[J].RSVD0;
			this.CM_SMP_USAGE_COUNT[J] = this.CM_SMP_USAGE[J].COUNT;
      end
      foreach (this.CM_RX_SMP_USAGE[i,j]) begin
         int J = i;
         int K = j;
         this.CM_RX_SMP_USAGE[J][K] = ral_reg_cm_usage_CM_RX_SMP_USAGE::type_id::create($psprintf("CM_RX_SMP_USAGE[%0d][%0d]",J,K),,get_full_name());
         this.CM_RX_SMP_USAGE[J][K].configure(this, null, "");
         this.CM_RX_SMP_USAGE[J][K].build();
         this.CM_RX_SMP_USAGE[J][K].add_hdl_path('{

            '{$psprintf("CM_RX_SMP_USAGE[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.CM_RX_SMP_USAGE[J][K], `UVM_REG_ADDR_WIDTH'h3600+J*`UVM_REG_ADDR_WIDTH'h10+K*`UVM_REG_ADDR_WIDTH'h8, "RO", 0);
			this.CM_RX_SMP_USAGE_RSVD0[J][K] = this.CM_RX_SMP_USAGE[J][K].RSVD0;
			this.CM_RX_SMP_USAGE_COUNT[J][K] = this.CM_RX_SMP_USAGE[J][K].COUNT;
      end
      foreach (this.CM_RX_SMP_USAGE_MAX[i,j]) begin
         int J = i;
         int K = j;
         this.CM_RX_SMP_USAGE_MAX[J][K] = ral_reg_cm_usage_CM_RX_SMP_USAGE_MAX::type_id::create($psprintf("CM_RX_SMP_USAGE_MAX[%0d][%0d]",J,K),,get_full_name());
         this.CM_RX_SMP_USAGE_MAX[J][K].configure(this, null, "");
         this.CM_RX_SMP_USAGE_MAX[J][K].build();
         this.CM_RX_SMP_USAGE_MAX[J][K].add_hdl_path('{

            '{$psprintf("CM_RX_SMP_USAGE_MAX[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.CM_RX_SMP_USAGE_MAX[J][K], `UVM_REG_ADDR_WIDTH'h3800+J*`UVM_REG_ADDR_WIDTH'h10+K*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.CM_RX_SMP_USAGE_MAX_RSVD0[J][K] = this.CM_RX_SMP_USAGE_MAX[J][K].RSVD0;
			this.CM_RX_SMP_USAGE_MAX_COUNT[J][K] = this.CM_RX_SMP_USAGE_MAX[J][K].COUNT;
      end
      this.CM_RX_SMP_USAGE_MAX_CTRL = ral_reg_cm_usage_CM_RX_SMP_USAGE_MAX_CTRL::type_id::create("CM_RX_SMP_USAGE_MAX_CTRL",,get_full_name());
      this.CM_RX_SMP_USAGE_MAX_CTRL.configure(this, null, "");
      this.CM_RX_SMP_USAGE_MAX_CTRL.build();
         this.CM_RX_SMP_USAGE_MAX_CTRL.add_hdl_path('{

            '{"CM_RX_SMP_USAGE_MAX_CTRL", -1, -1}
         });
      this.default_map.add_reg(this.CM_RX_SMP_USAGE_MAX_CTRL, `UVM_REG_ADDR_WIDTH'h3840, "RW", 0);
		this.CM_RX_SMP_USAGE_MAX_CTRL_RSVD0 = this.CM_RX_SMP_USAGE_MAX_CTRL.RSVD0;
		this.CM_RX_SMP_USAGE_MAX_CTRL_PORT3 = this.CM_RX_SMP_USAGE_MAX_CTRL.PORT3;
		this.PORT3 = this.CM_RX_SMP_USAGE_MAX_CTRL.PORT3;
		this.CM_RX_SMP_USAGE_MAX_CTRL_PORT2 = this.CM_RX_SMP_USAGE_MAX_CTRL.PORT2;
		this.PORT2 = this.CM_RX_SMP_USAGE_MAX_CTRL.PORT2;
		this.CM_RX_SMP_USAGE_MAX_CTRL_PORT1 = this.CM_RX_SMP_USAGE_MAX_CTRL.PORT1;
		this.PORT1 = this.CM_RX_SMP_USAGE_MAX_CTRL.PORT1;
		this.CM_RX_SMP_USAGE_MAX_CTRL_PORT0 = this.CM_RX_SMP_USAGE_MAX_CTRL.PORT0;
		this.PORT0 = this.CM_RX_SMP_USAGE_MAX_CTRL.PORT0;
      foreach (this.CM_PAUSE_GEN_STATE[i]) begin
         int J = i;
         this.CM_PAUSE_GEN_STATE[J] = ral_reg_cm_usage_CM_PAUSE_GEN_STATE::type_id::create($psprintf("CM_PAUSE_GEN_STATE[%0d]",J),,get_full_name());
         this.CM_PAUSE_GEN_STATE[J].configure(this, null, "");
         this.CM_PAUSE_GEN_STATE[J].build();
         this.CM_PAUSE_GEN_STATE[J].add_hdl_path('{

            '{$psprintf("CM_PAUSE_GEN_STATE[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.CM_PAUSE_GEN_STATE[J], `UVM_REG_ADDR_WIDTH'h3900+J*`UVM_REG_ADDR_WIDTH'h8, "RO", 0);
			this.CM_PAUSE_GEN_STATE_RSVD0[J] = this.CM_PAUSE_GEN_STATE[J].RSVD0;
			this.CM_PAUSE_GEN_STATE_SMP1[J] = this.CM_PAUSE_GEN_STATE[J].SMP1;
			this.SMP1[J] = this.CM_PAUSE_GEN_STATE[J].SMP1;
			this.CM_PAUSE_GEN_STATE_SMP0[J] = this.CM_PAUSE_GEN_STATE[J].SMP0;
			this.SMP0[J] = this.CM_PAUSE_GEN_STATE[J].SMP0;
      end
      foreach (this.CM_TX_TC_USAGE[i,j]) begin
         int J = i;
         int K = j;
         this.CM_TX_TC_USAGE[J][K] = ral_reg_cm_usage_CM_TX_TC_USAGE::type_id::create($psprintf("CM_TX_TC_USAGE[%0d][%0d]",J,K),,get_full_name());
         this.CM_TX_TC_USAGE[J][K].configure(this, null, "");
         this.CM_TX_TC_USAGE[J][K].build();
         this.CM_TX_TC_USAGE[J][K].add_hdl_path('{

            '{$psprintf("CM_TX_TC_USAGE[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.CM_TX_TC_USAGE[J][K], `UVM_REG_ADDR_WIDTH'h4000+J*`UVM_REG_ADDR_WIDTH'h40+K*`UVM_REG_ADDR_WIDTH'h8, "RO", 0);
			this.CM_TX_TC_USAGE_RSVD0[J][K] = this.CM_TX_TC_USAGE[J][K].RSVD0;
			this.CM_TX_TC_USAGE_COUNT[J][K] = this.CM_TX_TC_USAGE[J][K].COUNT;
      end
      foreach (this.CM_TX_EWMA[i,j]) begin
         int J = i;
         int K = j;
         this.CM_TX_EWMA[J][K] = ral_reg_cm_usage_CM_TX_EWMA::type_id::create($psprintf("CM_TX_EWMA[%0d][%0d]",J,K),,get_full_name());
         this.CM_TX_EWMA[J][K].configure(this, null, "");
         this.CM_TX_EWMA[J][K].build();
         this.CM_TX_EWMA[J][K].add_hdl_path('{

            '{$psprintf("CM_TX_EWMA[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.CM_TX_EWMA[J][K], `UVM_REG_ADDR_WIDTH'h5000+J*`UVM_REG_ADDR_WIDTH'h10+K*`UVM_REG_ADDR_WIDTH'h8, "RO", 0);
			this.CM_TX_EWMA_RSVD0[J][K] = this.CM_TX_EWMA[J][K].RSVD0;
			this.CM_TX_EWMA_INTERVAL[J][K] = this.CM_TX_EWMA[J][K].INTERVAL;
			this.INTERVAL[J][K] = this.CM_TX_EWMA[J][K].INTERVAL;
			this.CM_TX_EWMA_EWMA_WHOLE[J][K] = this.CM_TX_EWMA[J][K].EWMA_WHOLE;
			this.EWMA_WHOLE[J][K] = this.CM_TX_EWMA[J][K].EWMA_WHOLE;
			this.CM_TX_EWMA_EWMA_FRAC[J][K] = this.CM_TX_EWMA[J][K].EWMA_FRAC;
			this.EWMA_FRAC[J][K] = this.CM_TX_EWMA[J][K].EWMA_FRAC;
      end
   endfunction : build

	`uvm_object_utils(ral_block_cm_usage)

endclass : ral_block_cm_usage



`endif
