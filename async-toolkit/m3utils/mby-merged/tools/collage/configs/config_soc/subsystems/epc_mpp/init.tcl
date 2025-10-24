#################################################################################
# This file contains tcl code and is sourced as part of the -init stage of 
# reading a subsystem
#
# It can include (optionally) installation of IP kits as well as tcl utility 
# procedures that are used by the subsequent stages of this subsystem
# 
#################################################################################

set epc_wrap_name "mby_ec_top"
set mpp_wrap_name "mby_mpp"
collage_install_ip_kit -ip_name  $epc_wrap_name \
                       -kit_name $epc_wrap_name \
                       -src_dir  $src_kits_root \
                       -dest_dir $install_kits_root

collage_install_ip_kit -ip_name  $mpp_wrap_name \
                       -kit_name $mpp_wrap_name \
                       -src_dir  $src_kits_root \
                       -dest_dir $install_kits_root

