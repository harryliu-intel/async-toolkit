// -------------------------------------------------------------------
// -- Intel Proprietary
// -- Copyright (C) 2015 Intel Corporation
// -- All Rights Reserved
// -------------------------------------------------------------------
// -- Author : Nelson Crumbaker
// -- Project Name : Madison Bay
// -- Description :
// --
// -------------------------------------------------------------------

`ifndef __INSIDE_CDN_PCIE_PKG__
** ERROR: This file is meant to be used only through cdn_pcie_pkg.sv.  Do not include it individually.;
`endif  /* __INSIDE_CDN_PCIE_PKG__ */

`ifndef __PCIE_MONITOR_SV__
`define __PCIE_MONITOR_SV__

class pcie_monitor extends cdnPcieUvm::cdnPcieUvmMonitor;

   bit coverageEnable = 0;

   `uvm_component_utils_begin(pcie_monitor)
      `uvm_field_int(coverageEnable, UVM_ALL_ON)
   `uvm_component_utils_end
  
   // Coverage model
   pcie_cover coverModel;
    
   function new(string name = "pcie_monitor", uvm_component parent = null);
      super.new(name, parent);
   endfunction : new
  
   virtual function void build();
      super.build();

      if (coverageEnable == 1)
         coverModel = pcie_cover::type_id::create("coverModel", this);
   endfunction : build

   virtual function void connect();
      if (coverageEnable == 1) begin
         TL_RX_packetCbPort.connect(coverModel.CoverEndedCbPortImp);
         TL_TX_packetCbPort.connect(coverModel.CoverEndedCbPortImp); 
      end
   endfunction

endclass : pcie_monitor

`endif /* __PCIE_MONITOR_SV__ */

// -------------------------------------------------------------------
//
