setenv CONFIG_GEN $::env(COLLAGE_WORK)/gen
source $::env(REPO_ROOT)/collage/patches/gen_fp_netlist_procs.tcl
source $::env(REPO_ROOT)/collage/patches/create_upf_supply_net_info.tcl
set ::sh_output_log_file upf_supply_net_dc_load.log
set out_file $::env(COLLAGE_WORK)/upf/outputs/soc_supply_net_info.txt
set run_file $::env(COLLAGE_WORK)/upf/power_spec/create_supply_net_info.tcl
catch {
soc_upf_dc_read_rtl_stub
current_design soc
if { [ file exists $run_file ] } {
source $run_file
} else {
puts "\n Could not find the run file in the output area. Skipping creating the soc supply net info"
}
}

exit
