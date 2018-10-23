#################################################################################
# This file contains tcl code and is sourced as part of the -init stage of 
# reading a subsystem
#
# It can include (optionally) installation of IP kits as well as tcl utility 
# procedures that are used by the subsequent stages of this subsystem
# 
#################################################################################

### Wrapper stuff is temp, until wrapper output from ngen removes the 2d arrays
#set epc_wrap_name "mby_ec_top_wrapper"
#set mpp_wrap_name "mby_mpp_wrapper"

#set ip_wrapper_dir $workspace_root/wrappers
#set epc_kit_dir $ip_wrapper_dir
#set mpp_kit_dir $ip_wrapper_dir
#file mkdir $ip_wrapper_dir

# need to preserve and restore this env var, since wrapper generation overwrites it and does not restore
#set tmp_collage_design   $::env(COLLAGE_DESIGN)

#collage_generate_wrapper_corekit_rtl \
#  -top_wrap_name $epc_wrap_name \
#  -ip_wrap_kit_src_dir $ip_wrapper_dir \
#  -ip_name mby_ec_top \
#  -ip_kit_src_dir "$::env(MODEL_ROOT)/tools/collage/epc/ip_kits" \
#  -blast_adhoc_2D_ports
## release rtl and generate hdl to be used in ace;
#collage_release_rtl  -dest_rtl_dir $::env(BUILD_TARGET_RELATIVE)/$::env(DUT)/collage/work/soc/${gen_rtl_dir} -gen_hdl -file_extension sv

#collage_generate_wrapper_corekit_rtl \
#  -top_wrap_name $mpp_wrap_name \
#  -ip_wrap_kit_src_dir $ip_wrapper_dir \
#  -ip_name mby_mpp \
#  -ip_kit_src_dir "$::env(MODEL_ROOT)/tools/collage/mpp/ip_kits" \
#  -blast_adhoc_2D_ports
## release rtl and generate hdl to be used in ace;
#collage_release_rtl  -dest_rtl_dir $::env(BUILD_TARGET_RELATIVE)/$::env(DUT)/collage/work/soc/${gen_rtl_dir} -gen_hdl -file_extension sv

## restore this var
#set ::env(COLLAGE_DESIGN) $tmp_collage_design
### End temp wrapper stuff; 

### Note that ip/kit/src names will need to change below once wrappers are removed
# use these names without above wrappers; else use wrapper names defined above
set epc_wrap_name "mby_ec_top"
set mpp_wrap_name "mby_mpp"
set epc_kit_dir "$::env(MODEL_ROOT)/tools/collage/epc/ip_kits"
set mpp_kit_dir "$::env(MODEL_ROOT)/tools/collage/mpp/ip_kits"
collage_install_ip_kit -ip_name  $epc_wrap_name \
                       -kit_name $epc_wrap_name \
                       -src_dir  $epc_kit_dir \
                       -dest_dir $install_kits_root

collage_install_ip_kit -ip_name  $mpp_wrap_name \
                       -kit_name $mpp_wrap_name \
                       -src_dir  $mpp_kit_dir \
                       -dest_dir $install_kits_root

