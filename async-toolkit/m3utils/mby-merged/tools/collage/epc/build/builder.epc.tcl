##########################################################################
# IP info settings
###########################################################################

set ::collage_ip_info::ip_name "mby_ec_top"
set ::collage_ip_info::ip_top_module_name "mby_ec_top"
set ::collage_ip_info::ip_version "1.0"
set ::collage_ip_info::ip_intent_sp ""
# update this pointer once we can use non-temp shell
set ::collage_ip_info::ip_rtl_inc_dirs "./rtl/"

set ::collage_ip_info::ip_input_language SystemVerilog

set ::collage_ip_info::ip_input_files "../../../subIP/eth_port/src/mby_ec_top/rtl/mby_ec_top.sv"

set ::collage_ip_info::ip_plugin_dir "" ; # Directories - space separated list - with tcl plugin files
set ::collage_ip_info::ip_ifc_def_hook "epc_create_ifc_instances" ; # Set this to procedure to add IP interfaces - defined below

######################################################################
# Procedure to create IP interfaces
######################################################################
proc epc_create_ifc_instances {} {

  # insert std interfaces to be used by epc here...

  return
}

######################################################################
# Lines below are generic and should not include design specific info
######################################################################
# pointers to std interface definitions here...


collage_simple_build_flow -exit -copy_corekit
