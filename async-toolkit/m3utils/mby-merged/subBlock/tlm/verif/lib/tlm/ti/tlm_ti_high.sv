/* ----------------------------------------------------------------------


   ----------------------------------------------------------------------
   file:    tlm_ti_high.sv
   Date Created  : 25/7/2016
   Author        : dbenita
   Project       : TLM1 IP
   ----------------------------------------------------------------------
 
 IP Test island
 
 This moudle will hold all the "shared" TB content between the IP and the integartion level.

 
 TLM1_TOP_RTL define should be use to monitor internal signals. This define will be override in integration level.


*/

`ifndef TLM1_TOP_RTL
 `define TLM1_TOP_RTL tlm_tb.tlm_top
`endif

module tlm_ti_high #(parameter string IP_ENV = "*.env*"
            )
  (
   tlm_env_if tlm_if
   );

  import uvm_pkg::*;
  
`ifdef XVM
   import ovm_pkg::*;
   import xvm_pkg::*;
   `include "ovm_macros.svh"
   `include "sla_macros.svh"
`endif

import sla_pkg::*;
  
  // IP parameter file for IOSF primary and sideband signal widths, etc.
`include "tlm_params.sv"
`include "tlm_defines.sv"
  


    // ===============================================
    // Test Island LOW instance
    // ===============================================
    tlm_ti_low #(
            )
            u_tlm_ti_low (
                     .tlm_if          (tlm_if)
                 );


  
endmodule
