##########################################################################
# IP info settings
###########################################################################

set ::collage_ip_info::ip_name "mby_gmm_n"
set ::collage_ip_info::ip_top_module_name "mby_gmm_n_top"
set ::collage_ip_info::ip_version "1.0"
set ::collage_ip_info::ip_intent_sp ""
# update this pointer once we can use non-temp shell
set ::collage_ip_info::ip_rtl_inc_dirs "$::env(MODEL_ROOT)/subBlock/mbyc/src/gmm/gmm_n/rtl"

set ::collage_ip_info::ip_input_language SystemVerilog

#set ::collage_ip_info::ip_input_files "$::env(MODEL_ROOT)/subBlock/mbyc/src/shared/rtl/mby_gmm_pkg.sv $::env(MODEL_ROOT)/subBlock/mbyc/src/shared/rtl/shared_pkg.sv $::env(MODEL_ROOT)/subBlock/mbyc/src/gmm/gmm_n/top/rtl/mby_gmm_n_top.sv"
### TEMP solution until sla_pkg is removed from mby_gmm_n_top.sv
set ::collage_ip_info::ip_input_files "$::env(MODEL_ROOT)/tools/collage/corekit_stubs/sla_pkg_stub.sv $::env(MODEL_ROOT)/subBlock/mbyc/src/shared/rtl/mby_gmm_pkg.sv $::env(MODEL_ROOT)/subBlock/mbyc/src/shared/rtl/shared_pkg.sv $::env(MODEL_ROOT)/subBlock/mbyc/src/gmm/gmm_n/top/rtl/mby_gmm_n_top.sv"

set ::collage_ip_info::ip_plugin_dir "" ; # Directories - space separated list - with tcl plugin files
set ::collage_ip_info::ip_ifc_def_hook "gmm_n_create_ifc_instances" ; # Set this to procedure to add IP interfaces - defined below

######################################################################
# Procedure to create IP interfaces
######################################################################
proc gmm_n_create_ifc_instances {} {

  # insert std interfaces to be used here...

  return
}

######################################################################
# Lines below are generic and should not include design specific info
######################################################################
# pointers to std interface definitions here...


collage_simple_build_flow -exit -copy_corekit
