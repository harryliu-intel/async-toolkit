/* ----------------------------------------------------------------------


   ----------------------------------------------------------------------
   file:    tlm_ti_low.sv
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

module tlm_ti_low #(parameter string IP_ENV = "*.env*"

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

  import tlm_env_pkg::*;
  
  // IP parameter file for IOSF primary and sideband signal widths, etc.
`include "tlm_params.sv"
`include "tlm_defines.sv"
  
   
   //---------------------------------------------------------------
   // Connect the tlm DUT pins to the tlm_iosf_pri_if
   // using METHOD 1 or 2 below:
   //
   // METHOD 1: pin connections
   // tlm #(tlm_params) 
   // tlm  (.req_chid[4:0] (tlm_iosf_pri_if.req_chid), 
   //       .gnt           (tlm_iosf_pri_if.gnt) ...
   //
   // METHOD 2: intermediate wire connections
   // wire [4:0] req_chid;
   // wire       gnt; ...
   // assign tlm_iosf_pri_if.req_chid = req_chid;
   // assign gnt                      = tlm_iosf_pri_if.gnt; ...
   // tlm #(tlm_params) 
   // tlm  (.req_chid[4:0] (req_chid), 
   //       .gnt           (gnt) ...
   //
   // The advantage of pin connections (method 1) is that you don't 
   // have to worry about the direction of the assign statements in
   // method 2.  The disadvantage is that the compiler will NOT warn 
   // about bugs in DUT port direction. 
   //------------------------------------------------------------------


  // Adding TLM1 if to Saola container

  initial begin
    sla_pkg::slu_resource_db#(virtual tlm_env_if)::add("tlm_if",tlm_if,`__FILE__,`__LINE__);
  end

  tlm_rtl_config rtl_config;

  initial begin  
    rtl_config = new();
   //set ti path for env
    rtl_config.tlm_ti_low_path = $psprintf("%m");
   
    uvm_config_object::set(null, IP_ENV,"tlm_rtl_config",rtl_config);
  end 


  
endmodule
