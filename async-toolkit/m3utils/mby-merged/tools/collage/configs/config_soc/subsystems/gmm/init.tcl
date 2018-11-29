#################################################################################
# This file contains tcl code and is sourced as part of the -init stage of 
# reading a subsystem
#
# It can include (optionally) installation of IP kits as well as tcl utility 
# procedures that are used by the subsequent stages of this subsystem
# 
#################################################################################

collage_install_ip_kit -ip_name  "mby_gmn_par" \
                       -kit_name "mby_gmn_par" \
                       -src_dir  $src_kits_root \
                       -dest_dir $install_kits_root

collage_install_ip_kit -ip_name  "mby_gms_par" \
                       -kit_name "mby_gms_par" \
                       -src_dir  $src_kits_root \
                       -dest_dir $install_kits_root

