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

proc gtr_lamb_gen_db { args } {
   parse_proc_arguments -args $args arg
   set proc_name [lindex [regsub "::" [info level 0]  "" ] 0 ]
   set libfname $arg(-lib_file)
   set block_name $arg(-block_name)
   if { ! [file exists $libfname] } {
      error "$proc_name, $libfname does not exist"
   }
   
   set dbfname timing_snps/${block_name}_[gtr_cornerSuffix $arg(-oc_type) $arg(-voltage)].db
   set dbdir [file dirname $dbfname]
   if { [file exists $dbfname] } {
      puts "INFO: $proc_name, $dbfname already exists, deleting to create new content"
      file delete $dbfname
   }
   file mkdir $dbdir
   puts "INFO: $proc_name, Generating Compiled DB Liberty view: $dbfname from $libfname"
   global properties
   
   set lc_shell_exec $::env(LIBRARYCOMPILER_DIR)/bin/lc_shell
   exec $lc_shell_exec -no_home_init -output_log_file ${dbfname}.log -batch -x "read_lib $libfname; write_lib $block_name -output $dbfname"
   if { [info exists arg(-filelist_var) ] } {
        upvar $arg(-filelist_var) fileList
        set thisEntry [dict create]
        dict set thisEntry path $dbfname
        dict set thisEntry nda_protection_level front_end
        dict set thisEntry type db_ccs_filelist
        dict set thisEntry voltage $arg(-voltage)
        dict set thisEntry rc_type all_rc_types
        dict set thisEntry reliability_condition client
        dict set thisEntry design_view timing
        dict set thisEntry variation_modeling pocv
        dict set thisEntry liberty_ccs_type ccs
        dict set thisEntry consuming_vendor synopsys
        dict set thisEntry fusion_enablement dependent
        dict set thisEntry oc_type $arg(-oc_type)
        dict set thisEntry feol_process_corner [dict get $properties oc_type $arg(-oc_type) feol_process_corner]
        dict set thisEntry temperature [dict get $properties oc_type $arg(-oc_type) temperature]        
        lappend fileList $thisEntry
   }
   if { ![file exists $dbfname]} {
      error "$dbfname not created by DB compilation step!"
   }
   return $dbfname
}
define_proc_attributes gtr_lamb_gen_db \
    -info "Utility to generate Synopsys .DB Memory collaterals" \
    -define_args {
   {-block_name "Specify memory name" "<block_name>" string required}	
   {-lib_file "Specify liberty file name" "<liberty_name>" string required}
   {-oc_type "Operating condition" "AnOos" string required }
   {-voltage "Voltage condition" "voltage" string required}
   {-filelist_var "Update filelist for manifest.xml" "" string optional}
   {-debug "Report additional logging for debug purposes" "" boolean optional}
}
