#################################################################################
# This file contains tcl code and is sourced as part of the -init stage of 
# reading a subsystem
#
# It can include (optionally) installation of IP kits as well as tcl utility 
# procedures that are used by the subsequent stages of this subsystem
# 
#################################################################################

collage_install_ip_kit -ip_name  "mby_gmm_n" \
                       -kit_name "mby_gmm_n" \
                       -src_dir  "$::env(MODEL_ROOT)/tools/collage/gmm_n/ip_kits" \
                       -dest_dir $install_kits_root

collage_install_ip_kit -ip_name  "mby_gmm_s" \
                       -kit_name "mby_gmm_s" \
                       -src_dir  "$::env(MODEL_ROOT)/tools/collage/gmm_s/ip_kits" \
                       -dest_dir $install_kits_root

