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

proc gtr_lamb_gen_upf { args } {
    set proc_name [lindex [regsub "::" [info level 0]  "" ] 0 ]
    set date [date ]
    parse_proc_arguments -args $args arg
    if { [info exists arg(-verbose) ] } {
        set verbose 1
    } else {
        set verbose 0
    }
    set dir upf
    file mkdir $dir
    set block_name $arg(-block_name)
    set fname ${dir}/${block_name}.upf

    echo "INFO: $proc_name, Generating UPF view: $block_name -> $fname"

    if { [info exists arg(-filelistVar) ] } {
        upvar $arg(-filelistVar) fileList
        set thisEntry [dict create]
        dict set thisEntry path $fname
        dict set thisEntry nda_protection_level front_end 
        dict set thisEntry pg_included false 
        dict set thisEntry standard_name upf
        dict set thisEntry type upf_filelist 
        lappend fileList $thisEntry
    } 
    
    set of [open $fname w ]

    puts $of "# Template UPF for LAMBs"
    puts $of ""
    puts $of "#Define All Design Level UPF Attributes"
    puts $of "set_design_attributes -elements {.} -attribute terminal_boundary TRUE"
    puts $of "set_design_attributes -elements { reinit_msg_blk reinit_top_blk} -models {${block_name}} -attribute SNPS_reinit TRUE"
    puts $of "set_design_attributes -elements { reinit_uut_blkA reinit_uut_blkB  reinit_fault_blk } -models {generic_behav_${block_name}} -attribute SNPS_reinit TRUE"
    puts $of "set_design_attributes -elements { mes_all_valid_old } -models {${block_name}} -attribute UPF_dont_touch TRUE"
    puts $of "set_design_attributes -elements { mes_all_valid } -models {generic_behav_${block_name}} -attribute UPF_dont_touch TRUE"
    puts $of ""
    puts $of "# Define power intent for top level scope that corresponds to memory periphery "
    puts $of "# ****************************************************************************** "
    puts $of "# Create Supply Sets"
    puts $of "create_supply_set SS_Peri_AO"
    puts $of ""
    puts $of "# Supply Port, Supply Net Connections"
    puts $of "create_supply_port VSS"
    puts $of "create_supply_port VDD"
    puts $of ""
    puts $of "create_supply_net VSS"
    puts $of "create_supply_net VDD"
    puts $of ""
    puts $of "connect_supply_net VSS -ports VSS"
    puts $of "connect_supply_net VDD -ports VDD"
    puts $of ""
    puts $of ""
    puts $of "# Supply Sets update"
    puts $of "create_supply_set SS_Peri_AO -function {power VDD} -function {ground VSS} -update"
    puts $of ""
    puts $of "#Define Power Domains"
    puts $of "create_power_domain PDP -elements {.}   -supply { primary SS_Peri_AO }"
    puts $of ""
    puts $of ""
    puts $of ""
    puts $of "#Define Supply States"
    puts $of ""
    puts $of "add_port_state VDD -state {ON  0.675 0.75 0.825} -state {OFF off}"
    puts $of "add_port_state VSS -state {GND 0.0 }"
    puts $of ""
    puts $of "create_pst inst_pst -supplies                           {VDD  VSS }"
    puts $of "add_pst_state normal -pst inst_pst -state               {ON     GND}"
    puts $of "add_pst_state allOFF -pst inst_pst -state               {OFF    GND}"
    puts $of "#Define Boundary Supplies"
    puts $of "set_port_attributes -elements {.} -attribute related_supply_default_primary true"
    puts $of ""
    puts $of ""

    close $of
}

define_proc_attributes gtr_lamb_gen_upf \
    -info "Utility to generate Lamb UPF" \
    -define_args {
      {-block_name "Specify memory block name" "String" string required}
      {-verbose "Verbose Reporting" "" boolean optional}
      {-filelistVar "Update filelist for manifest.xml" "" string optional}
}
