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

`ifndef __PCIE_SVE_SV__
`define __PCIE_SVE_SV__

class pcie_sve extends uvm_env;

   pcie_env pcie_env_inst;
   
   `uvm_component_utils_begin(pcie_sve)
      `uvm_field_object(pcie_env_inst, UVM_ALL_ON);
   `uvm_component_utils_end

   function new(string name = "pcie_sve", uvm_component parent);
      super.new(name,parent);
      factory.set_type_override_by_type(cdnPcieUvm::cdnPcieUvmSequencer::get_type(),   pcie_sequencer::get_type());
      factory.set_type_override_by_type(cdnPcieUvm::cdnPcieUvmDriver::get_type(),      pcie_driver::get_type());
      factory.set_type_override_by_type(cdnPcieUvm::cdnPcieUvmInstance::get_type(),    pcie_instance::get_type());
      factory.set_type_override_by_type(cdnPcieUvm::cdnPcieUvmMonitor::get_type(),     pcie_monitor::get_type());
      factory.set_type_override_by_type(cdnPcieUvm::cdnPcieUvmMemInstance::get_type(), pcie_mem_instance::get_type());
      `uvm_info(get_type_name(),"End of Sve new",UVM_LOW);
   endfunction // new

   virtual function void build();
      super.build();
      pcie_env_inst = pcie_env::type_id::create("pcie_env_inst", this);
   endfunction : build

   virtual function void connect();
      super.connect();
   endfunction 
   
endclass

`endif /* __PCIE_SVE_SV__ */

// -------------------------------------------------------------------
//
