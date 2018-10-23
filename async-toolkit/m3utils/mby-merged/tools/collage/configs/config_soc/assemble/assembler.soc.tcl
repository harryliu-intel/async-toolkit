#======================#
# START OF CATCH BLOCK #
#======================#
if { [catch {
#======================#

##########################################################################################
# Pre-initialize hook
# - use to set directory variables such as sc_repo_dir and sc_integ_specs
##########################################################################################
if {[info proc soc_pre_hook_initialize] != ""} {
  soc_pre_hook_initialize
}

#############################
# SoC initialization
#############################
set script_dir [file dirname [info script]]
source ${script_dir}/soc_assemble.tcl
source ${script_dir}/assembler.soc_init.tcl

#############################
# 
#############################
set _log_file_ $::env(COLLAGE_WORK)/log/collage.${design}.assembler.log 
file delete -force $_log_file_
#djo tempfile link -symbolic $_log_file_ $env(COLLAGE_WORK)/log/collage.${collage_pid}.${shell_activity_mode}.log 

# -----------------------------------------------------------------------
# SoC Subsystem Registration
# -----------------------------------------------------------------------
if {[info proc soc_register_subsystems] != ""} {
  soc_register_subsystems
}

if { !$open_workspace_post_specify_subsystem } {
  ########## ######################################################################
  # Install IPs (if pre-installing IPs, this step is optional)
  ################################################################################
  file delete -force $install_kits_root
  file mkdir $install_kits_root

  #-----------------------------------------------------------------------------
  # Subsystem init
  #-----------------------------------------------------------------------------
  collage_read_subsystems -stage init

  ::insert_mod::process_module_insertion -stage init
  source ${::auto_conn_dir}/insert_module_init.tcl

  # Do NOT checkin with the below uncommented
  #collage_continue_on_error_std_connect_stage
  #collage_continue_on_error_adhoc_connect_stage

  #################################################################################
  # Create "gen" directory for outputs 
  # Create workspace and read global knowledge bases
  #################################################################################
  chassis_create_collage_gen_dir "gen" "integ_specs tb_specs"

  set workspace [file join $workspace_root $design]
  print_info "Collage Workspace: $workspace"

  if {[file isdirectory $workspace] == 1} {
      exec /bin/rm -fr $workspace
  }
  print_info "create_workspace now"
  create_workspace -name $design -root $workspace_root -language SystemVerilog
  load_interface_definitions $::env(COLLAGE_INTF_DEF)/rtl_interface_defs/internal/clock_req_ack_interface.tcl


  #################################################################################
  # Set search path for IPs
  # Note: install_kits_root variable below is set in previous step above and 
  #       should be set appropriately if IPs are pre-installed
  #################################################################################
  set_activity_parameter AddSubsystemComponents SearchPath \
      "$::synopsys_root/auxx/dware/glue \
     $install_kits_root"

 

  if {[info proc soc_hook_initialize] != ""} {
    soc_hook_initialize
  }

  set_current_component

  #set_design_attribute SpiritName ${chassis_config_id}
  #set_design_attribute SpiritVersion 1.4

  ##########################################################################################
  # Clocks & Multicast & Global Decode
  ##########################################################################################
  if [file readable ${chassis_config_dir}/integ_specs/soc_clocks.tcl] {
    source ${chassis_config_dir}/integ_specs/soc_clocks.tcl
  }
  autocomplete_activity GClocks
  set_current_component

  ##########################################################################################
  # RTL bookkeeping
  ##########################################################################################

  # --------------------------------------------------------------------------------------------
  # SoC Registered Subsystems: instantiate & configure
  # --------------------------------------------------------------------------------------------
  print_time "Read Subsystems: START"
  collage_read_subsystems -stage inst


  ::insert_mod::process_module_insertion -stage inst


  if {[info proc soc_post_instantiate_components] != ""} {
    soc_post_instantiate_components
  }


  collage_write_registered_subsystem_info -output_file "subsystems.txt"

 ## read in overrides
  ## REVIEW move back to create.tcl files
  print_time "Read overrides.tcl"
  source ${::soc_integ_specs_dir}/overrides.tcl

 
  # --------------------------------------------------------------------------------------------
  # SoC Registered Subsystems: connect standard interfaces
  # --------------------------------------------------------------------------------------------
  print_time "Subsystems Standard Connections: START"
  collage_read_subsystems -stage std_conn

  if {[info proc soc_specify_subsystem] != ""} {
    soc_specify_subsystem
  }

  ::insert_mod::process_module_insertion -stage std_conn
  collage_process_ifc_conn_file -hier_lookup -file ${::auto_conn_dir}/insert_intf_module.cfg


  # -------------------------------------------------------------------------------------------
  # Process PSF orgates
  # -------------------------------------------------------------------------------------------
#djo temp  chassis_optimize_orgate_ports -orgate_template psf2_orgate

  # -------------------------------------------------------------------------------------------
  # Processing steps to unlink unused std interface ports to prevent tie off at partition
  # -------------------------------------------------------------------------------------------
#djo temp  collage_unlink_unused_interface_port -use_internal_logic -skip_fabric_ifcs "IOSF::Primary"

  # --------------------------------------------------------------------------------------------
  #  Processing steps to merge / unlink etc on standard interfaces
  # --------------------------------------------------------------------------------------------
#djo temp  collage_merge_common_ifc_ports -ifc_defn IOSF::DFX::SCAN
#djo temp  collage_merge_common_ifc_ports -ifc_defn SPID
#djo temp  collage_merge_common_ifc_ports -ifc_defn COL::GPIO

  # --------------------------------------------------------------------------------------------
  # Export parameters
  # --------------------------------------------------------------------------------------------
  foreach s [collage_get_registered_subsystems] {
    set subsys_dir "[collage_get_subsystem_spec_dir -subsystem_id $s]"
    if { [file exists ${subsys_dir}/parameters.txt] } {
       collage_process_parameter_propagation_file -file ${subsys_dir}/parameters.txt
    }
  }

  # -------------------------------------------------------------------------------------------
  # Interface Reports
  # -------------------------------------------------------------------------------------------
  collage_report_unused_interfaces -file "reports/${design}.open_interfaces.pre_process.report"
  collage_process_open_interfaces -verbose

  ###############################################################################
  autocomplete_activity SpecifySubsystem
  collage_report_unused_interfaces -file "reports/${design}.std_interfaces.report"
}

###############################################################################

#---------------------------------------
# Subsystems: Specify ad-hoc connections
#---------------------------------------
print_time "Subsystems Adhoc Connections: START"

collage_read_subsystems -stage adhoc_conn
#create_power_pins
#create_clk_pins
#create_rst_pins

if {[info proc soc_complete_connections] != ""} {
  soc_complete_connections
}

#################################################################################
# Reports, temporary tieoffs, and complete Open connections
#################################################################################
if {[info proc soc_complete_temporary_connections] != ""} {
  soc_complete_temporary_connections
}

::insert_mod::process_module_insertion -stage adhoc_conn
soc_collage_tar_ws "soc.b4_insert_module"
collage_process_conn_file -file ${::auto_conn_dir}/adhoc_insert_module.txt


#################################################################################
# Process open connections
#################################################################################

collage_report_connections -file "reports/${design}.unconnected.pre_process.report" -unconnected -format text
#print_info "Interactive mode"; save_workspace; gui_start; return
collage_process_open_connections -all_hier -in_comment {} -out_comment {}

###############################################################################
#################################################################################
autocomplete_activity CompleteConnections
#################################################################################
###############################################################################

print_time "Generate RTL and Reports: START"
if {[info proc soc_hook_generate_subsystem_rtl] != ""} {
  soc_hook_generate_subsystem_rtl
}


###############################################################################
# Generate and Release/Copy RTL and HDL
###############################################################################
#generates RTL with unconnected ports
collage_generate_rtl -param_value {DontTieOffInputs 1} -language SystemVerilog
#generates tied-off RTL
#collage_generate_rtl -language SystemVerilog

collage_release_rtl  -dest_rtl_dir $::env(BUILD_TARGET_RELATIVE)/$::env(DUT)/collage/work/soc/${gen_rtl_dir} -gen_hdl -file_extension sv

# Generate hierarchy defines file
gen_hier_defines ${gen_rtl_dir} $script_dir/fc_hier_defines.txt $script_dir/fc_hier_defines_filter.txt 

#publish generated rtl to $MODEL_ROOT/srcgen/collage. This may not be needed but 
#putting it in here to match EBG for now.
set model_root  $::env(MODEL_ROOT)

################################
# Generate collaterals
################################
if {[info proc soc_hook_generate_collaterals] != ""} {
  soc_hook_generate_collaterals
}

#stubs generation
set fp_stub_cells {}
set vp_stub_cells {}
foreach_in_collection c [find_item -quiet -type cell] {
  set cname [get_attribute -attrs UserName $c]
  append fp_stub_cells " $cname"
  set is_hier [get_attribute -attrs IsHierarchicalComponent [get_attribute -attrs ReferenceDesign $c]]
  if {$is_hier} {
    append vp_stub_cells " $cname"
  }
}

chassis_generate_collaterals -syn_defs "DC" \
    -gen_fp_stubs $fp_stub_cells -fp_stub_dir $fp_stubs_dir \
    -gen_vp_stubs $vp_stub_cells -vp_stub_dir $vp_stubs_dir


#collage_gen_parameterized_ip_stubs  -out_dir gen/stubs/verification -file_extension sv
#collage_gen_parameterized_ip_stubs  -out_dir $::nl_leaf_stubs_dir -file_extension sv -only_interface -skip_weak_stubs

# Hierarchy report
collage_report_hierarchy -rep_fn "reports/${design}.hierarchy.report"
#collage_write_registered_subsystem_info -output_file "reports/${design}.subsystems.report"

# in obviate init to not initialize clocks
obviate reset_clock_spec

  print_time "Start connectivity report"

# Generate connectivity reports
### TEMP something in these reports is taking a long time, commenting out for now
#collage_report_pin_connectivity -rep_fn "reports/${design}.ip2ip_pin_connectivity.report"
##soc_populate_obv_clocks 1
#collage_report_interface_connectivity -rep_fn "reports/${design}.ip2ip_std_connectivity.report"
##soc_collage_report_opens_stats  "reports/${design}.unconnected.pre_process.report" reports/subsystem_opens
#collage_report_sub_optimal_connectivity -rep_fn "reports/${design}.duplicate_ports.report"

##########################################################################################
# Finalize hook
# - use to copy all generated files (RTL, collaterals etc.) to the final location 
#   as well as any reports / post-processing
##########################################################################################
if {[info proc soc_hook_finalize] != ""} {
  soc_hook_finalize
}

print_time "RTL Assembly : END"

################################
# exit unless debugging
################################
if {![info exists ::env(CHASSIS_DEBUG_MODE)] } {
  exit 0
}

#####################################################
#####################################################
#====================#
# END OF CATCH BLOCK #
#====================#
} ] } {
  puts stderr "\n\nERROR (Code = ${::errorCode}):\n${::errorInfo}\n"
  if {![info exists ::env(CHASSIS_DEBUG_MODE)] } {
    exit 1
  }
}
