##############################################################################
# This file contains tcl code and is sourced as part of the -init stage of 
# reading a subsystem
# It can include (optionally) installation of IP kits as well as tcl utility 
# procedures that are used by the subsequent stages of this subsystem
##############################################################################

#set gpio_kit_src_dir  /p/hdk/rtl/ip_releases/cpghdk/mst/sip/gpio/MSTA0P00RTL1IFC1V1_HACK3/tools/collage/work/ip_kits/
#set gpio_kit_src_dir  /nfs/pdx/proj/acd/fe.205/work/akau/MST/IRR/GPIO_Transformer_ALL_RTL1P0_PIC12_V1/ip-gpio-transformer-rls-pic12_OLD/tools/collage/work/ip_kits/


#collage_install_ip_kit -ip_name gpio -kit_name gpcommstunit_wrapper -src_dir $gpio_kit_src_dir  -dest_dir $install_kits_root

#set locked_kits_dir $::env(gpio_COREKIT_DIR)
#collage_install_ip_kit -ip_name "ip74xecfiodlchvmtp0family" -kit_name "ip74xecfiodlchvmtp0family" -src_dir $locked_kits_dir7 -dest_dir $install_kits_root



################################
# Text editor settings
# Local Variables:
# mode: tcl
# tcl-indent-level: 4
# End:
# vim: set expandtab tabstop=4 shiftwidth=4 softtabstop=4 :
