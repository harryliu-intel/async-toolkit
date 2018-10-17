##############################################################################
# This file is tcl code that is sourced during the "-inst" stage of reading a 
# subsystem
# This file will typically contain instantiation of one or more IPs
# It should NOT include any connectivity specs
##############################################################################


collage_import_component -component "mby_parclk" -files $::env(MODEL_ROOT)/src/rtl/clkstub/mby_parclk.sv -name "mby_parclk" -use_hier
collage_import_component -component "xtal_rcvr" -files $::env(MODEL_ROOT)/src/rtl/clkstub/xtal_rcvr.sv -name "xtal_rcvr" -use_hier
#eval_in_component [collage_get_ip_hier_par -ip_name "clkstub_imc"] {
#set_configuration_parameter -component "gpio_mux2"  NUMBER_MUXINOUT 3
#}


################################
# Text editor settings
# Local Variables:
# mode: tcl
# tcl-indent-level: 4
# End:
# vim: set expandtab tabstop=4 shiftwidth=4 softtabstop=4 :
