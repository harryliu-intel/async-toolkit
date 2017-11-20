
/* ----------------------------------------------------------------------


   ----------------------------------------------------------------------
   file:    tlm_config.sv
   Date Created  : 25/7/2016
   Author        : dbenita
   Project       : TLM1 IP
   ----------------------------------------------------------------------
 
  TLM1 config object

 This is the configuration object to control the TLM1 env.
 
 This Class contain all the switches to control the ENV seeting.
 
 By default it contain Saola config oobject:
 
 checkers_enabled - default 1
 
 monitors_enabled - default 1
 
 trackers_enabled  - default 1
 
 coverage_enabled  - default 1
 
 By default Saola will be build this object unless Upper env 
 or a test override it using set_config ...
 
 

*/

class tlm_config extends slu_config_object;

    string tlm_primary_access = "primary";
    string tlm_sideband_access = "sideband";
    bit tlm_chassis_rst_verbose_dbg;
    //Variable: tlm_has_reset_pkg
    // This bit control the build of the reset pkg
    // It gets it value fromthe TI TLM1_HAS_RESET_PKG parameter
    bit tlm_has_reset_pkg = 0;


  `uvm_object_utils_begin(tlm_config)
     `uvm_field_string(tlm_primary_access, UVM_ALL_ON)
     `uvm_field_string(tlm_sideband_access, UVM_ALL_ON)
     `uvm_field_string(tlm_chassis_rst_verbose_dbg, UVM_ALL_ON)
   `uvm_object_utils_end

  //function: new 
  function new( string         name = "");
    super.new(name);
    set_parent_name(name);


  endfunction


  
endclass
