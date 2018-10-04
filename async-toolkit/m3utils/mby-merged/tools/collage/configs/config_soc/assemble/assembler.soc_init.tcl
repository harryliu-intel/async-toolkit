#################################################################################
# This is a template file provided for setting up Collage assembly initialization.
# This includes activities such as
#  - search paths for kits
#  - subsystem registration
#  - workspace and work directory creation
#  - etc.
# 
# This file should be copied and modified per SoC.
#################################################################################
suppress_message [list DES-40]
catch {set ::use_vcs_parser 0}
set chassis_config_root [file normalize $::env(MODEL_ROOT)/tools/collage/configs/config_soc]
set chassis_config_id ""
set chassis_config_dir "${chassis_config_root}"

set script_dir [file dirname [info script]]

source ${chassis_config_dir}/assemble/configuration.tcl
source ${chassis_config_dir}/assemble/assembler.soc_utils.tcl
source ${chassis_config_dir}/assemble/assembler.soc_tb_utils.tcl
source ${chassis_config_dir}/patches/assembler.chassis_patches.tcl
source ${chassis_config_dir}/patches/assembler.collage_patches.tcl
source ${chassis_config_dir}/patches/assembler.collage_clock_patches.tcl
source ${chassis_config_dir}/patches/collage_add_ifdef_supply_ground_patch.tcl
source ${chassis_config_dir}/integ_specs/stub_override.tcl
source ${chassis_config_dir}/patches/insert_mcp_cell.tcl
source ${chassis_config_dir}/patches/insert_module.tcl
source ${chassis_config_dir}/patches/mi_patches.tcl


##########################################################################################
# SOC integration specs
##########################################################################################
set gen_specs_dir "gen"
set fp_stubs_dir ${gen_specs_dir}/stubs/floorplan
set vp_stubs_dir ${gen_specs_dir}/stubs/verification
set nl_stubs_dir ${gen_specs_dir}/stubs/fp_netlist
set nl_leaf_stubs_dir ${gen_specs_dir}/stubs/fp_leaf_netlist
set gen_rtl_dir  ${gen_specs_dir}/source/rtl
set auto_conn_dir  ${gen_specs_dir}/connectivity
set gen_clock_collaterals_dir  ${gen_specs_dir}/clock_collaterals

file mkdir ${fp_stubs_dir}
file mkdir ${vp_stubs_dir}
file mkdir ${gen_rtl_dir}
file mkdir ${auto_conn_dir}
file mkdir ${nl_stubs_dir}
file mkdir ${nl_leaf_stubs_dir}
file mkdir ${gen_clock_collaterals_dir}

## To save/open workspace post specify sub-system
set save_workspace_post_specify_subsystem 0
set open_workspace_post_specify_subsystem 0

set workspace_root $::env(COLLAGE_WORK)
set install_kits_root ${workspace_root}/installed_kits

##############################################################################

catch {set ::use_vcs_parser 0}
