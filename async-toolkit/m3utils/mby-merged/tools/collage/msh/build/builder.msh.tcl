##########################################################################
# IP info settings
###########################################################################

set ::collage_ip_info::ip_name "mesh"
set ::collage_ip_info::ip_top_module_name "mesh"
set ::collage_ip_info::ip_version "1.0"
set ::collage_ip_info::ip_intent_sp ""
# update this pointer once we can use non-temp shell
set ::collage_ip_info::ip_rtl_inc_dirs "$::env(MODEL_ROOT)/subBlock/mbyc/src/mesh/rtl/"

set ::collage_ip_info::ip_input_language SystemVerilog

set ::collage_ip_info::ip_input_files "$::env(MODEL_ROOT)/subBlock/mbyc/src/mesh/rtl/mesh_pkg.sv $::env(MODEL_ROOT)/subBlock/mbyc/src/mesh/rtl/mesh.sv"

set ::collage_ip_info::ip_plugin_dir "" ; # Directories - space separated list - with tcl plugin files
set ::collage_ip_info::ip_ifc_def_hook "mesh_create_ifc_instances" ; # Set this to procedure to add IP interfaces - defined below

######################################################################
# Procedure to create IP interfaces
######################################################################
proc mesh_create_ifc_instances {} {

  # insert std interfaces to be used by mesh here...

  return
}

######################################################################
# Lines below are generic and should not include design specific info
######################################################################
# pointers to std interface definitions here...


collage_simple_build_flow -exit -copy_corekit
