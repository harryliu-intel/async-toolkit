# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0

#################################################################################
# This file is tcl code that is sourced during the "-inst" stage of reading a 
# subsystem
#
# This file will typically contain instantiation of one or more IPs
# 
# It should NOT include any connectivity specs
#################################################################################

## temp dummy syscon to drive synch deassertion of reset
collage_import_component -component "mby_syscon" -files $::env(MODEL_ROOT)/src/rtl/imcstub/mby_syscon.sv -name "mby_syscon" -use_hier

#set inst_name "<ip instance name>1"
#set mod_name "<module name>"
#collage_instantiate_component $mod_name -name $inst_name -noauto -use_hier
#
#collage_eval_in_component [collage_get_ip_hier_par -ip_name $inst_name] {
#    set_configuration_parameter -component $inst_name <module parameter> <value> 
#    set_configuration_parameter -component $inst_name FP_SAI  0x26
#}
