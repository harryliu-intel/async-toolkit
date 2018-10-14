#############################
# SoC initialization
#############################
set script_dir [file dirname [info script]]


set ::collage_tb::enable_split_tb_sv 1


# -------------------------------
# Configure TB generation options
# -------------------------------
# Register the subsystems for TB generation
::collage_subsystem::update_subsys_info
#collage_unregister_subsystem -subsystem_id  "p2sb"
# rtl does not match corekit 
#collage_unregister_subsystem -subsystem_id  "iosf_pri" 



# -------------------------------
# Generate Testbench
# -------------------------------
set  ::_chs_gen_tb_specs_dir "gen/tb_specs"
set  tb_dest [file join $::env(COLLAGE_WORK) "gen/tb_specs"]
file mkdir $tb_dest

# TODO: SHOULD THIS BE REMOVED?
set iosf_pri_ss_dir [collage_get_subsystem_spec_dir -subsystem_id  "iosf_pri"]
set iosf_pri_si_dir [collage_get_subsystem_integ_dir -subsystem_id  "iosf_pri"]
set iosf_sb_ss_dir [collage_get_subsystem_spec_dir -subsystem_id  "iosf_sb"]
set iosf_sb_si_dir [collage_get_subsystem_integ_dir -subsystem_id  "iosf_sb"]

if {!$::collage_tb::reduce_fabric_enable} {
   
    set sbr_post_ti "${::_chs_gen_tb_specs_dir}/sbr_post_ti_include.sv"
    set sbr_tb_override "${::_chs_gen_tb_specs_dir}/sbr_tb_overrides.txt"
    set sbr_ti_inst "chs_sideband_ti"
    set router_prefix chs
    #This procedure generates the clkreq/ack connectivity
    #proc collage_tb_generate_sbr_collaterals { design_inst sbr_post_ti sbr_tb_override sbr_ti_inst sbr_names} {}
    collage_tb_generate_sbr_collaterals $design_inst $sbr_post_ti $sbr_tb_override $sbr_ti_inst $sbr_names 
}

#To generate active passive configuration and pass to env, add API collage_tb_write_sbr_active before collage_tb_assemb    le API. The API expects a string arguement. The arguement represents the IP_ENV be used to pass config setting via ovm     set_config_int. The set_config_int is guarded with the same IP_ENABLE around the sbr interfaces, thus when the enable g    ets set, the passive setting is passed the the sideband env. The API generates a configuration settings file in $COLLAG    E_WORK/gen/tb_specs/soc_tb_sbr_active.sv and `includes the file automatically into the generated tb.
collage_tb_write_sbr_active "*.sbr_env*"

## Set fabric info for tb
#collage_tb_set_fabric_inst_list PSF "$psf_names psf_rpt psf0_orgate psf1_orgate psf2_orgate psf3_orgate psf3_orgate ps    f3_orgate_a psf3_orgate_b psf4_orgate"

collage_tb_set_fabric_inst_list PSF "psf.*"


set _log_file_ $::env(COLLAGE_WORK)/log/collage.${::design}_tb.assembler.log
file delete -force $_log_file_
file link -symbolic $_log_file_ $env(COLLAGE_WORK)/log/collage.${collage_pid}.${shell_activity_mode}.log 

##. Installation of TB related kits in /tmp disk can cause random failure as specific directory might be taken..
set  tb_install_kits_root $::env(COLLAGE_WORK)/tb_installed_kits
file delete -force $tb_install_kits_root

## Just to keep backward compatibility before merging collage and chassis
if {[info exists $::env(CHASSIS_ROOT)/configs/common/design/globals/ip_kits/iosf_sbc_intf.coreKit]} {
    set kits_dir $::env(CHASSIS_ROOT)/configs/common/design/globals/ip_kits
} else {
    set kits_dir $::env(COLLAGE_INTF_DEF)/tb_interface/sv_interface_packaging/design/ip_kits
}

collage_tb_assemble -design_name "${::design}_tb" \
    -dut_inst_name $design_inst \
    -dut_mod_name $design \
    -dut_ws [file join $::env(COLLAGE_WORK) $design] \
    -user_ti_if_file [file join $soc_tb_specs_dir "${design}.ii_ti_spec.txt"] \
    -user_tb_conn_file "" \
    -iosf_sb_poks_file "${::_chs_gen_tb_specs_dir}/sbr_only_tb_poks.txt" \
    -kits_dir $kits_dir \
    -kits_install_dir $tb_install_kits_root \
    -iosf_psf_oob_file "" \
    -ti_def_files "" 
#    -user_tb_conn_file "$::env(COLLAGE_WORK)/gen/tb_specs/auto_wdu_tb_conn.txt" \

#collage_tb_generate_ip_enable
set  gen_tb_dir "gen/verif"
set  gen_tb_path [file join $::env(COLLAGE_WORK) $gen_tb_dir]
file mkdir ${gen_tb_path}
collage_release_tb  -root_tb_dir $::env(COLLAGE_WORK) -dest_tb_dir $gen_tb_dir -gen_hdl -add_hdl_incdirs $gen_tb_dir -file_extension sv


# ----------------------------------------------------------------------------------------
# Copy TB to repository
# ----------------------------------------------------------------------------------------
collage_post_process_tb
#file copy -force  $::env(COLLAGE_WORK)/gen/tb_specs/soc_ip_hier_defines.sv $::env(MODEL_ROOT)/verif/tb/include


################################
# Text editor settings
# Local Variables:
# mode: tcl
# tcl-indent-level: 2
# End:
# vim: set expandtab tabstop=2 shiftwidth=2 softtabstop=2 :
