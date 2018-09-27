// -----------------------------------------------------------------------------
// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :  Raghu P Gudla
// Created On   :  09/22/2018
// Description  :  Cadence Axi System Config object derived from pureview obj
// -----------------------------------------------------------------------------

class cdn_axi_system_cfg extends cdnAxiUvmConfig;
    
  `uvm_object_utils_begin(cdn_axi_system_cfg)  
  `uvm_object_utils_end
  
  function new(string name = "cdn_axi_system_cfg");
    super.new(name);

    // set feature values
    spec_ver = CDN_AXI_CFG_SPEC_VER_AMBA4;
    spec_subtype = CDN_AXI_CFG_SPEC_SUBTYPE_BASE;
    spec_interface = CDN_AXI_CFG_SPEC_INTERFACE_FULL;  
  endfunction : new    
  
endclass
