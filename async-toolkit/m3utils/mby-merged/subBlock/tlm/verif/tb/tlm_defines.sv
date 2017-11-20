/* ----------------------------------------------------------------------


   ----------------------------------------------------------------------
   file:    tlm_defines.sv
   Date Created  : 25/7/2016
   Author        : dbenita
   Project       : TLM1 IP
   ----------------------------------------------------------------------
 
  

 This file contain all the IP macros (`define)
 
 each define must be protected with "ifndef" to be able to be overide from the command line



*/


`ifndef TLM1_TOP
 `define TLM1_TOP tlm_tb.tlm_top
`endif
`ifndef TLM1_TOP_PATH
 `define TLM1_TOP_PATH tlm_tb
`endif

