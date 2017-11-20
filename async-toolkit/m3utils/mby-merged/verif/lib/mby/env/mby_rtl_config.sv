
/* ----------------------------------------------------------------------


   ----------------------------------------------------------------------
   file:    mby_rtl_config.sv
   Date Created  : 25/7/2016
   Author        : dbenita
   Project       : MBY IP
   ----------------------------------------------------------------------
 
  MBY rtl config object

 This is the configuration object to control the MBY rtl env.
 

*/

class mby_rtl_config extends ovm_object;

 string mby_ti_low_path;
 int mby_has_reset_pkg;

  `ovm_object_utils_begin(mby_rtl_config)
     `ovm_field_string(mby_ti_low_path, OVM_ALL_ON)
     `ovm_field_int(mby_has_reset_pkg, OVM_ALL_ON)
   `ovm_object_utils_end

  //function: new 
  function new( string         name = "");
    super.new(name);

  endfunction
  
endclass
