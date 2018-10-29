#################################################################################
# This file is tcl code that is sourced during the "-inst" stage of reading a 
# subsystem
#
# This file will typically contain instantiation of one or more IPs
# 
# It should NOT include any connectivity specs
#################################################################################

# using wrapper for now, to blast the 2d arrays; leaving instance name same as if not wrapper
#collage_instantiate_component mby_ec_top_wrapper -name mby_ec_top_{%inst_num} -noauto -use_hier
#collage_instantiate_component mby_mpp_wrapper -name mby_mpp_{%inst_num} -noauto -use_hier

collage_instantiate_component mby_ec_top -name mby_ec_top_{%inst_num} -noauto -use_hier
collage_instantiate_component mby_mpp -name mby_mpp_{%inst_num} -noauto -use_hier



#
#collage_eval_in_component [collage_get_ip_hier_par -ip_name $inst_name] {
#    set_configuration_parameter -component $inst_name <module parameter> <value> 
#    set_configuration_parameter -component $inst_name FP_SAI  0x26
#}

