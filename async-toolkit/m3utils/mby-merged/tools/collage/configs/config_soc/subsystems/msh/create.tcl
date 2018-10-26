#################################################################################
# This file is tcl code that is sourced during the "-inst" stage of reading a 
# subsystem
#
# This file will typically contain instantiation of one or more IPs
# 
# It should NOT include any connectivity specs
#################################################################################

set inst_name "mby_msh_0"
set mod_name "mby_msh"
collage_instantiate_component $mod_name -name $inst_name -noauto -use_hier

#
#collage_eval_in_component [collage_get_ip_hier_par -ip_name $inst_name] {
#    set_configuration_parameter -component $inst_name <module parameter> <value> 
#    set_configuration_parameter -component $inst_name FP_SAI  0x26
#}
