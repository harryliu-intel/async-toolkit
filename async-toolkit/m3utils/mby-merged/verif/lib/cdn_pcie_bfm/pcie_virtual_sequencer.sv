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

`ifndef __PCIE_VIRTUAL_SEQUENCER_SV__
`define __PCIE_VIRTUAL_SEQUENCER_SV__

class pcie_virtual_sequencer extends uvm_sequencer;
  
   pcie_sequencer pSeqr0;
   // pcie_env       pEnv;
   uvm_env        pEnv;
   pcie_agent     pAgent;

   `uvm_component_utils_begin(pcie_virtual_sequencer)
      `uvm_field_object(pSeqr0, UVM_ALL_ON)
      `uvm_field_object(pEnv, UVM_ALL_ON)
      `uvm_field_object(pAgent, UVM_ALL_ON)
   `uvm_component_utils_end

   function new(string name = "pcie_virtual_sequencer", uvm_component parent = null);
      super.new(name, parent);
   endfunction : new

   virtual function void connect();
      super.connect();
   endfunction : connect

endclass : pcie_virtual_sequencer

`endif /* __PCIE_VIRTUAL_SEQUENCER_SV__ */

// -------------------------------------------------------------------
//
