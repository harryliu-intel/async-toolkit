#################################################################################
# This file contains tcl code and is sourced as part of the -init stage of 
# reading a subsystem
#
# It can include (optionally) installation of IP kits as well as tcl utility 
# procedures that are used by the subsequent stages of this subsystem
# 
#################################################################################

collage_install_ip_kit -ip_name  "mpp" \
                       -kit_name "mpp" \
                       -src_dir  "$::env(MODEL_ROOT)/subBlock/mbyc/src/mpp/collage/ip_kits" \
                       -dest_dir $install_kits_root
