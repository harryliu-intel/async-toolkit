`ifndef RAL_MBY_RX_PB_MAP
`define RAL_MBY_RX_PB_MAP

import uvm_pkg::*;

class ral_reg_mby_rx_pb_map_RX_PB_PORT_CFG extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field PORT;

	function new(string name = "mby_rx_pb_map_RX_PB_PORT_CFG");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 32, 32, "RO", 0, 32'h0, 1, 0, 1);
      this.PORT = uvm_reg_field::type_id::create("PORT",,get_full_name());
      this.PORT.configure(this, 32, 0, "RW", 0, 32'h0, 1, 0, 1);
   endfunction: build

	`uvm_object_utils(ral_reg_mby_rx_pb_map_RX_PB_PORT_CFG)

endclass : ral_reg_mby_rx_pb_map_RX_PB_PORT_CFG


class ral_reg_mby_rx_pb_map_RX_PB_WM extends uvm_reg;
	uvm_reg_field RSVD0;
	rand uvm_reg_field LOSSLESS;
	rand uvm_reg_field XON;
	rand uvm_reg_field XOFF_OR_DROP;

	function new(string name = "mby_rx_pb_map_RX_PB_WM");
		super.new(name, 64,build_coverage(UVM_NO_COVERAGE));
	endfunction: new
   virtual function void build();
      this.RSVD0 = uvm_reg_field::type_id::create("RSVD0",,get_full_name());
      this.RSVD0.configure(this, 43, 21, "RO", 0, 43'h0, 1, 0, 0);
      this.LOSSLESS = uvm_reg_field::type_id::create("LOSSLESS",,get_full_name());
      this.LOSSLESS.configure(this, 1, 20, "RW", 0, 1'h0, 1, 0, 0);
      this.XON = uvm_reg_field::type_id::create("XON",,get_full_name());
      this.XON.configure(this, 10, 10, "RW", 0, 10'h200, 1, 0, 0);
      this.XOFF_OR_DROP = uvm_reg_field::type_id::create("XOFF_OR_DROP",,get_full_name());
      this.XOFF_OR_DROP.configure(this, 10, 0, "RW", 0, 10'h200, 1, 0, 0);
   endfunction: build

	`uvm_object_utils(ral_reg_mby_rx_pb_map_RX_PB_WM)

endclass : ral_reg_mby_rx_pb_map_RX_PB_WM


class ral_block_mby_rx_pb_map extends uvm_reg_block;
	rand ral_reg_mby_rx_pb_map_RX_PB_PORT_CFG RX_PB_PORT_CFG;
	rand ral_reg_mby_rx_pb_map_RX_PB_WM RX_PB_WM[17][0:7];
	uvm_reg_field RX_PB_PORT_CFG_RSVD0;
	rand uvm_reg_field RX_PB_PORT_CFG_PORT;
	rand uvm_reg_field PORT;
	uvm_reg_field RX_PB_WM_RSVD0[17][0:7];
	rand uvm_reg_field RX_PB_WM_LOSSLESS[17][0:7];
	rand uvm_reg_field LOSSLESS[17][0:7];
	rand uvm_reg_field RX_PB_WM_XON[17][0:7];
	rand uvm_reg_field XON[17][0:7];
	rand uvm_reg_field RX_PB_WM_XOFF_OR_DROP[17][0:7];
	rand uvm_reg_field XOFF_OR_DROP[17][0:7];

	function new(string name = "mby_rx_pb_map");
		super.new(name, build_coverage(UVM_NO_COVERAGE));
	endfunction: new

   virtual function void build();
      this.default_map = create_map("", 0, 8, UVM_LITTLE_ENDIAN, 0);
      this.RX_PB_PORT_CFG = ral_reg_mby_rx_pb_map_RX_PB_PORT_CFG::type_id::create("RX_PB_PORT_CFG",,get_full_name());
      this.RX_PB_PORT_CFG.configure(this, null, "");
      this.RX_PB_PORT_CFG.build();
         this.RX_PB_PORT_CFG.add_hdl_path('{

            '{"RX_PB_PORT_CFG", -1, -1}
         });
      this.default_map.add_reg(this.RX_PB_PORT_CFG, `UVM_REG_ADDR_WIDTH'h0, "RW", 0);
		this.RX_PB_PORT_CFG_RSVD0 = this.RX_PB_PORT_CFG.RSVD0;
		this.RX_PB_PORT_CFG_PORT = this.RX_PB_PORT_CFG.PORT;
		this.PORT = this.RX_PB_PORT_CFG.PORT;
      foreach (this.RX_PB_WM[i,j]) begin
         int J = i;
         int K = j;
         this.RX_PB_WM[J][K] = ral_reg_mby_rx_pb_map_RX_PB_WM::type_id::create($psprintf("RX_PB_WM[%0d][%0d]",J,K),,get_full_name());
         this.RX_PB_WM[J][K].configure(this, null, "");
         this.RX_PB_WM[J][K].build();
         this.RX_PB_WM[J][K].add_hdl_path('{

            '{$psprintf("RX_PB_WM[%0d]", J), -1, -1}
         });
         this.default_map.add_reg(this.RX_PB_WM[J][K], `UVM_REG_ADDR_WIDTH'h800+J*`UVM_REG_ADDR_WIDTH'h40+K*`UVM_REG_ADDR_WIDTH'h8, "RW", 0);
			this.RX_PB_WM_RSVD0[J][K] = this.RX_PB_WM[J][K].RSVD0;
			this.RX_PB_WM_LOSSLESS[J][K] = this.RX_PB_WM[J][K].LOSSLESS;
			this.LOSSLESS[J][K] = this.RX_PB_WM[J][K].LOSSLESS;
			this.RX_PB_WM_XON[J][K] = this.RX_PB_WM[J][K].XON;
			this.XON[J][K] = this.RX_PB_WM[J][K].XON;
			this.RX_PB_WM_XOFF_OR_DROP[J][K] = this.RX_PB_WM[J][K].XOFF_OR_DROP;
			this.XOFF_OR_DROP[J][K] = this.RX_PB_WM[J][K].XOFF_OR_DROP;
      end
   endfunction : build

	`uvm_object_utils(ral_block_mby_rx_pb_map)

endclass : ral_block_mby_rx_pb_map



`endif
