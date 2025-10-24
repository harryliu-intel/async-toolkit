/* ----------------------------------------------------------------------


   ----------------------------------------------------------------------
   file:    mby_defines.sv
   Date Created  : 25/7/2016
   Author        : dbenita
   Project       : MBY IP
   ----------------------------------------------------------------------
 
  

 This file contain all the IP macros (`define)
 
 each define must be protected with "ifndef" to be able to be overide from the command line



*/


`ifndef MBY_TOP
 `define MBY_TOP mby_tb.mby_top
`endif
`ifndef MBY_TOP_PATH
 `define MBY_TOP_PATH mby_tb
`endif

