// -----------------------------------------------------------------------------
// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :  Raghu P Gudla
// Created On   :  09/22/2018
// Description  :  Derived Cadence AXI VIP agent
// -----------------------------------------------------------------------------

class cdn_axi_agent extends cdnAxiUvmAgent;
  
  `uvm_component_utils_begin(cdn_axi_agent)        
  `uvm_component_utils_end

`ifndef CDN_AXI_USING_CLOCKING_BLOCK
	`cdnAxiDeclareVif(virtual interface cdnAxi4Interface #(.DATA_WIDTH(128)))
`endif
	
  // ***************************************************************
  // Method : new
  // Desc.  : Call the constructor of the parent class.
  // ***************************************************************
  function new (string name = "cdn_axi_agent", uvm_component parent = null);
    super.new(name, parent);
  endfunction : new      
  
  
  virtual function integer memoryRead8Bytes(reg [63:0] addr, ref reg [7:0] data []);
  	inst.memoryRead8Bytes(addr,data);
  endfunction
  
  virtual function integer memoryWrite8Bytes(reg [63:0] addr, reg [7:0] data []);
	inst.memoryWrite8Bytes(addr,data);
  endfunction

endclass : cdn_axi_agent

class cdn_axi_active_master_agent extends cdn_axi_agent;
  
  `uvm_component_utils_begin(cdn_axi_active_master_agent)        
  `uvm_component_utils_end
   
`ifdef CDN_AXI_USING_CLOCKING_BLOCK
	`cdnAxiDeclareVif(virtual interface cdnAxi4ActiveMasterInterface#(.DATA_WIDTH(128)))
`endif

  // ***************************************************************
  // Method : new
  // Desc.  : Call the constructor of the parent class.
  // ***************************************************************
  function new (string name = "cdn_axi_active_master_agent", uvm_component parent = null);
    super.new(name, parent);
  endfunction : new      

endclass : cdn_axi_active_master_agent

class cdn_axi_active_slave_agent extends cdn_axi_agent;
  
  `uvm_component_utils_begin(cdn_axi_active_slave_agent)        
  `uvm_component_utils_end

`ifdef CDN_AXI_USING_CLOCKING_BLOCK
	`cdnAxiDeclareVif(virtual interface cdnAxi4ActiveSlaveInterface#(.DATA_WIDTH(128)))
`endif

  // ***************************************************************
  // Method : new
  // Desc.  : Call the constructor of the parent class.
  // ***************************************************************
  function new (string name = "cdn_axi_active_slave_agent", uvm_component parent = null);
    super.new(name, parent);
  endfunction : new      

endclass : cdn_axi_active_slave_agent

class cdn_axi_passive_agent extends cdn_axi_agent;
  
  `uvm_component_utils_begin(cdn_axi_passive_agent)        
  `uvm_component_utils_end

`ifdef CDN_AXI_USING_CLOCKING_BLOCK
	`cdnAxiDeclareVif(virtual interface cdnAxi4PassiveInterface#(.DATA_WIDTH(128)))
`endif

  // ***************************************************************
  // Method : new
  // Desc.  : Call the constructor of the parent class.
  // ***************************************************************
  function new (string name = "cdn_axi_passive_agent", uvm_component parent = null);
    super.new(name, parent);
  endfunction : new      

endclass : cdn_axi_passive_agent
