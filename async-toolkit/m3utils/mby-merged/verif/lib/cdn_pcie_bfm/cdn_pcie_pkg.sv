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

`ifndef __CDN_PCIE_PKG_SV__
`define __CDN_PCIE_PKG_SV__

package cdn_pcie_pkg;
   import uvm_pkg::*;

   `define __INSIDE_CDN_PCIE_PKG__
   typedef class pcie_cover;  
   typedef class pcie_virtual_sequencer;  
    
   `include "pcie_mem_instance.sv"
   `include "pcie_instance.sv"
   `include "pcie_driver.sv"
   `include "pcie_monitor.sv"
   `include "pcie_sequencer.sv"
   `include "mby_cp_pcie_cfg.sv"
   `include "pcie_agent.sv"
   `include "pcie_env.sv"
   `include "pcie_ep_env.sv"
   `include "pcie_sve.sv"
   `include "pcie_cover.sv"
   `include "pcie_seq_lib.sv"
   `include "pcie_report_server.sv"
   `include "pcie_virtual_sequencer.sv"
   `include "mby_cp_pcie_coverage.sv"

endpackage

`endif  /* __CDN_PCIE_PKG_SV__ */

// -------------------------------------------------------------------
//
