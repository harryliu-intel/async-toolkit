
/* ----------------------------------------------------------------------


   ----------------------------------------------------------------------
   file:    mby_ti_config.sv
   Date Created  : 25/7/2016
   Author        : dbenita
   Project       : MBY IP
   ----------------------------------------------------------------------
 
  MBY rtl config object

 This is the configuration object to control the MBY rtl env.
 

*/

class mby_ti_config extends uvm_object;

 string mby_ti_low_path;
 int mby_has_reset_pkg;

  `uvm_object_utils_begin(mby_ti_config)
     `uvm_field_string(mby_ti_low_path, UVM_ALL_ON)
     `uvm_field_int(mby_has_reset_pkg, UVM_ALL_ON)
   `uvm_object_utils_end

  //function: new 
  function new( string         name = "");
    super.new(name);

  endfunction
  
endclass
