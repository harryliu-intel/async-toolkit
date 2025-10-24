# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0



set ::env(GTR_NDM_TECH_FILE_N5) /nfs/site/proj/tech1/n5/tech-release/v1.1.9p2/snpstech/1P15M_1X_h_1Xb_v_1Xe_h_1Ya_v_1Yb_h_5Y_vhvhv_2Yy2R/TechFile/PRTF_ICC2_N5_15M_1X1Xb1Xe1Ya1Yb5Y2Yy2R_UTRDL_M1P34_M2P35_M3P42_M4P42_M5P76_M6P76_M7P76_M8P76_M9P76_M10P76_M11P76_H210_SHDMIM.11_2a.tf

set ::env(GTR_NDM_TECH_FILE_N3B) /nfs/site/proj/tech1/n3/tech-release/v1.0.4p1/snpstech/1P17M_1X_h_1Xb_v_1Xc_h_1Xd_v_1Ya_h_1Yb_v_6Y_hvhvhv_2Yy2R_shdmim_ut-alrdl/PR_tech/Synopsys/TechFile/Standard/VHV/PRTF_ICC2_N3_17M_1X1Xb1Xc1Xd1Ya1Yb6Y2Yy2R_UTRDL_M1P30_M2P26_M3P35_M4P35_M5P42_M6P76_M7P76_M8P76_M9P76_M10P76_M11P76_M12P76_M13P76_143H_SHDMIM.10_1b.tf

set ::env(GTR_NDM_TECH_FILE_N3E) /nfs/site/proj/tech1/n3/tech-release/v1.0.4p1/snpstech/1P17M_1X_h_1Xb_v_1Xc_h_1Xd_v_1Ya_h_1Yb_v_6Y_hvhvhv_2Yy2R_shdmim_ut-alrdl/PR_tech/Synopsys/TechFile/Standard/VHV/PRTF_ICC2_N3_17M_1X1Xb1Xc1Xd1Ya1Yb6Y2Yy2R_UTRDL_M1P30_M2P26_M3P35_M4P35_M5P42_M6P76_M7P76_M8P76_M9P76_M10P76_M11P76_M12P76_M13P76_143H_SHDMIM.10_1b.tf

set ::env(GTR_FC_ROOT) /p/hdk/cad/fusioncompiler/S-2021.06-SP5

set wash_groups "user,soc,n7,n7fe,n5,n5fe,n3,n3e,tfc"
if { [info exists ::env(NB_WASH_GROUPS) ] } {
    if { "$::env(NB_WASH_GROUPS)" != $wash_groups } {
	puts "INFO: gtr_tech, overwriting envar NB_WASH_GROUPS"
	puts "      Previous Setting: $::env(NB_WASH_GROUPS)"
	set ::env(NB_WASH_GROUPS) $wash_groups
	puts "      New Setting: $::env(NB_WASH_GROUPS)"
    }
} else {
    set ::env(NB_WASH_GROUPS) $wash_groups
    puts "INFO: gtr_tech, setting envar NB_WASH_GROUPS"
    puts "      Setting: $::env(NB_WASH_GROUPS)"
}
