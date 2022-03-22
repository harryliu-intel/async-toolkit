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


proc gtr_gen_ndm { args } {
    set proc_name [lindex [regsub "::" [info level 0]  "" ] 0 ]
    set date [date ]
    parse_proc_arguments -args $args arg
    set tool icc2_lm_shell

    if { [info exists arg(-verbose) ] } {
        set verbose 1
    } else {
        set verbose 0
    }
    set block_name $arg(-block_name)
    set lib_file $arg(-lib_file)
    set process_label $arg(-process_label)
    set lef_file $arg(-lef_file)
    set tech_node_uc [string toupper $arg(-tech_node) ]
    set ndmdir collateral/ebb/lib
    file mkdir $ndmdir
    set out_ndm "${ndmdir}/${block_name}.ndm"

    if { [info exists arg(-filelistVar) ] } {
        upvar $arg(-filelistVar) fileList
        set thisEntry [dict create]
        dict set thisEntry path $out_ndm
        dict set thisEntry is_dir true
        dict set thisEntry consuming_tool icc2
        dict set thisEntry consuming_vendor synopsys
        dict set thisEntry layout_type both
        dict set thisEntry timing_included true
        dict set thisEntry type ndm_reflist
        lappend fileList $thisEntry
    } 

    echo "INFO: $proc_name, Generating NDM Model for $block_name"
    if { [info exists ::env(GTR_NDM_TECH_FILE_${tech_node_uc})] && $::env(GTR_NDM_TECH_FILE_${tech_node_uc}) != "" } {
        set ndm_tech_file $::env(GTR_NDM_TECH_FILE_${tech_node_uc})
        puts "INFO: $proc_name, using GTR_NDM_TECH_FILE_N<X>:\n     $ndm_tech_file"
    } else {
        error "$proc_name, need to setenv GTR_NDM_TECH_FILE_${tech_node_uc}"
    }
    if { $tool == "new" } {
        #create_physical_lib $block_name -scale_factor 2000                                                                      
        create_physical_lib $block_name
        read_tech_file $ndm_tech_file
    } else {
        remove_workspace
        create_workspace -flow normal -technology $ndm_tech_file $block_name
    }
    read_lef $lef_file

    # the following line needs to loop over process labels and lib files
    read_lib -process_label $process_label $lib_file
    
    if { $tool == "lc_shell" } {
        report_app_options > ./${block_name}_frame_report_app_options.rep
        create_frame
        #set_attribute [get_physical_lib_cells */*/frame ] design_type macro                                                     
        write_physical_lib -force -output $out_ndm
        close_physical_lib [get_physical_lib ]
    } else {
        check_workspace
        commit_workspace -output $out_ndm -force
    }
}


define_proc_attributes gtr_gen_ndm \
    -info "Utility to generate LAMB NDM" \
    -define_args {
	{-block_name "Specify memory name" "<block_name>" string required}
	{-lib_file "Path to the .lib file" "./<block_name>.lib" string required}
	{-process_label "Process label" "e.g., ssgnp" string required}
	{-lef_file "Path to the .LEF file" "./<block_name>.LEF" string required}
	{-tech_node "Specify tech node (default n3b)" "AnOos" one_of_string {required {values {"n3b" "n3e" "n5"}}}}
   {-filelistVar "Update filelist for manifest.xml" "" string optional}
	{-verbose "Verbose Reporting" "" boolean optional}
    }

