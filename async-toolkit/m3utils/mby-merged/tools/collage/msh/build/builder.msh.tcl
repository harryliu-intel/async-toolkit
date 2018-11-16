###################################################################################
# pre corekit script to fixup rtl due to collage/coretool issue parsing typedef bus
# https://hsdes.intel.com/resource/1909279599
###################################################################################
set ip_src_path "$::env(MODEL_ROOT)/subBlock/mbyc/src/msh/rtl"
set ip_src_file_name "mby_msh.sv"
# if running modscripts succeeds, then reset the src path to local area; comment out  this if statement if don't need this workaround
# if any typedefs used in src code outside of shared rtl area, then will need to support additional paths in script
if {[catch {exec $::env(MODEL_ROOT)/tools/collage/scripts/typedef_mods.sh $ip_src_path $ip_src_file_name $::env(MODEL_ROOT)/subBlock/mbyc/src/shared/rtl} result] == 0} {
  set ip_src_path "./"
} else {
  echo "ERROR attempting to fix typedefs in src rtl: $ip_src_file_name"
  exit -1
}

# use this pointer to file in the ip_input_files list below
set ip_src_file "$ip_src_path/$ip_src_file_name"


##########################################################################
# IP info settings
###########################################################################

set ::collage_ip_info::ip_name "mby_msh"
set ::collage_ip_info::ip_top_module_name "mby_msh"
set ::collage_ip_info::ip_version "1.0"
set ::collage_ip_info::ip_intent_sp ""
# update this pointer once we can use non-temp shell
set ::collage_ip_info::ip_rtl_inc_dirs "$::env(MODEL_ROOT)/subBlock/mbyc/src/msh/rtl/ $::env(MODEL_ROOT)/target/mby/mgm_run/rtl/ $::env(MODEL_ROOT)/target/mby/mgm_run/msh/src/"

set ::collage_ip_info::ip_input_language SystemVerilog

set ::collage_ip_info::ip_input_files "$::env(MODEL_ROOT)/subBlock/mbyc/src/shared/rtl/shared_pkg.sv $::env(MODEL_ROOT)/subBlock/mbyc/src/shared/rtl/mby_msh_pkg.sv $::env(MODEL_ROOT)/subBlock/mbyc/src/shared/rtl/mby_egr_pkg.sv $ip_src_file"

set ::collage_ip_info::ip_plugin_dir "" ; # Directories - space separated list - with tcl plugin files
set ::collage_ip_info::ip_ifc_def_hook "msh_create_ifc_instances" ; # Set this to procedure to add IP interfaces - defined below

######################################################################
# Procedure to create IP interfaces
######################################################################
proc msh_create_ifc_instances {} {

  # insert std interfaces to be used by msh here...

  return
}

######################################################################
# Lines below are generic and should not include design specific info
######################################################################
# pointers to std interface definitions here...


collage_simple_build_flow -exit -copy_corekit
