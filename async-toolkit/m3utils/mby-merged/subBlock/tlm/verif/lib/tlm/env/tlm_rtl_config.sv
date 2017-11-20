
/* ----------------------------------------------------------------------


   ----------------------------------------------------------------------
   file:    tlm_rtl_config.sv
   Date Created  : 25/7/2016
   Author        : dbenita
   Project       : TLM1 IP
   ----------------------------------------------------------------------
 
  TLM1 rtl config object

 This is the configuration object to control the TLM1 rtl env.
 

*/

class tlm_rtl_config extends uvm_object;

 string tlm_ti_low_path;
 int tlm_has_reset_pkg;

  `uvm_object_utils_begin(tlm_rtl_config)
     `uvm_field_string(tlm_ti_low_path, UVM_ALL_ON)
     `uvm_field_int(tlm_has_reset_pkg, UVM_ALL_ON)
   `uvm_object_utils_end

  //function: new 
  function new( string         name = "");
    super.new(name);

  endfunction
  
endclass
