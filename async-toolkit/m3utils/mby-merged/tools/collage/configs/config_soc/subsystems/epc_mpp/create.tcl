#################################################################################
# This file is tcl code that is sourced during the "-inst" stage of reading a 
# subsystem
#
# This file will typically contain instantiation of one or more IPs
# 
# It should NOT include any connectivity specs
#################################################################################

## SERDES_ORIENTATION
#   0 = NS
#   1 = EW
set inst_name mby_ec_top_{%inst_num}
collage_instantiate_component mby_ec_top -name $inst_name -noauto -use_hier
collage_eval_in_component -use_hier_par $inst_name {
 # note... serdes orientation might need to change since die rotated; I'm not sure if NS/EW is from SD layout perspective...
 # i'm assuming it is, so, epc 0 starts with EW (1), epc 1 & 2 are NS (0) and so on...
 # is there an easier way to calculate this based on inst_num?
 collage_set_configuration_parameter -component $inst_name SERDES_ORIENTATION [expr ((({%inst_num}+3)/2)%2)]
}

collage_instantiate_component mby_mpp -name mby_mpp_{%inst_num} -noauto -use_hier


#
#collage_eval_in_component [collage_get_ip_hier_par -ip_name $inst_name] {
#    set_configuration_parameter -component $inst_name <module parameter> <value> 
#    set_configuration_parameter -component $inst_name FP_SAI  0x26
#}

