##
## GTR Generator Tools Release v1.0.0
##
##------------------------------------------------------------------------------
##
## INTEL CONFIDENTIAL
##
## Copyright 2021 - 2022 Intel Corporation All Rights Reserved.
##
## The source code contained or described herein and all documents related
## to the source code ("Material") are owned by Intel Corporation or its
## suppliers or licensors. Title to the Material remains with Intel
## Corporation or its suppliers and licensors. The Material contains trade
## secrets and proprietary and confidential information of Intel or its
## suppliers and licensors. The Material is protected by worldwide copyright
## and trade secret laws and treaty provisions. No part of the Material may
## be used, copied, reproduced, modified, published, uploaded, posted,
## transmitted, distributed, or disclosed in any way without Intel's prior
## express written permission.
##
## No license under any patent, copyright, trade secret or other intellectual
## property right is granted to or conferred upon you by disclosure or
## delivery of the Materials, either expressly, by implication, inducement,
## estoppel or otherwise. Any license under such intellectual property rights
## must be express and approved by Intel in writing.
##
##------------------------------------------------------------------------------

if { ! [info exists ::env(GTR_ROOT) ] } {
    error "You must set ::env(GTR_ROOT), or setenv GTR_ROOT to \"/nfs/sc/disks/tfc_be_01/pdonehue/gtr\""
}

source $::env(GTR_ROOT)/tcl/gtr_util.tcl
source $::env(GTR_ROOT)/tcl/gtr_tech.tcl
source $::env(GTR_ROOT)/tcl/gtr_read_lib.tcl
source $::env(GTR_ROOT)/tcl/gtr_lamb_area.tcl
source $::env(GTR_ROOT)/tcl/gtr_lamb_power.tcl
source $::env(GTR_ROOT)/tcl/gtr_lamb_gen_sv.tcl
source $::env(GTR_ROOT)/tcl/gtr_lamb_gen_lef.tcl
source $::env(GTR_ROOT)/tcl/gtr_lamb_gen_lib.tcl
source $::env(GTR_ROOT)/tcl/gtr_gen_ndm.tcl
source $::env(GTR_ROOT)/tcl/gtr_lblocks_gen_spice.tcl

### Notes on Synopsys arg handler package                                                                                        
# define_proc_attributes argHandler \                                                                                            
#    -info "Arguments processor" \                                                                                               
#    -define_args {                                                                                                              
#        {-Oos "oos help"       AnOos   one_of_string {required value_help {values {a b}}}}                                      
#        {-Int "int help"       AnInt   int     optional}                                                                        
#        {-Float "float help"   AFloat  float   optional}                                                                        
#        {-Bool "bool help"     ""      boolean optional}                                                                        
#        {-String "string help" AString string  optional}                                                                        
#        {-List "list help"     AList   list    optional}                                                                        
#        {-IDup "int dup help"  AIDup   int     {optional merge_duplicates}}                                                     
#      } \                                                                                                                       
#    -define_arg_groups {                                                                                                        
#       {exclusive {-Int -Float -Bool}}                                                                                          
# }                                                                                                                              

