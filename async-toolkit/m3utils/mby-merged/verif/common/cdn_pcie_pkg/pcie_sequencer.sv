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

`ifndef __PCIE_SEQUENCER_SV__
`define __PCIE_SEQUENCER_SV__

typedef class pcie_env;

class pcie_sequencer extends cdnPcieUvm::cdnPcieUvmSequencer;
   
   DenaliSvPcie::denaliPcieConfig srcConfig;
   DenaliSvPcie::denaliPcieConfig dstConfig;
   pcie_env         pEnv;
   bit csr_swap_disabled = 'b1;

   `uvm_component_utils(pcie_sequencer)    

   function new (string name = "pcie_sequencer" , uvm_component parent = null);
      super.new(name, parent);
   endfunction : new

   virtual function void build();
      super.build();
      // Get instances of the denaliPcieConfig class for both the
      // Source and Destination configuration objects.
      srcConfig = DenaliSvPcie::denaliPcieConfig::type_id::create("srcConfig", this);
      dstConfig = DenaliSvPcie::denaliPcieConfig::type_id::create("dstConfig", this);
   endfunction : build
   
   virtual function void connect();
       super.connect();
   endfunction : connect

endclass

`endif /* __PCIE_SEQUENCER_SV__ */

// -------------------------------------------------------------------
//
