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

`ifndef __PCIE_AGENT_SV__
`define __PCIE_AGENT_SV__

class pcie_agent extends cdnPcieUvm::cdnPcieUvmAgent;

   `uvm_component_utils(pcie_agent)

   function new (string name = "pcie_agent", uvm_component parent = null);
      super.new(name, parent);
   endfunction : new

   virtual function void build();
      super.build();
`ifdef CDN_PCIE_COVERAGE
      void'(inst.has_cover());
`endif
      //if(is_active == UVM_ACTIVE) begin
         //adapter = pcie_adapter::type_id::create("adapter", this);
         // Driver will send a response to the sequencer.
         // Adapter uses this response (important for read operations)
         //adapter.provides_responses = 1;
      //end
   endfunction

   virtual function void connect();
       super.connect();
   endfunction : connect
   
   function integer setCallback(DenaliSvPcie::denaliPcieCbReasonT cbRsn);
      void'(inst.setCallback(cbRsn));
   endfunction : setCallback

endclass : pcie_agent

`endif /* __PCIE_AGENT_SV__ */

// -------------------------------------------------------------------
//
