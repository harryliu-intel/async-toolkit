##############################################################################
# 
# NOTE
# 
# The procs and actions in this file are specific to SOC RTL & TB generation
#
##############################################################################

##############################################################################
# Hook to Register Subsystems
proc soc_register_subsystems {} {

  #set in configurations.tcl
  set queried_subsys_list $::soc_subsystems

  foreach "ssid ssdir total_num_inst" $queried_subsys_list {
    set specdir [file join ${::soc_subsys_specs_dir} $ssdir]
    print_info "Auto-register subsystem: -subsystem_id $ssid -subsystem_spec_dir $specdir"
    # do loop for each instance of subsystem, will replace inst_num variable passed into subsystem with iteration num
    for {set i 1} {$i <= $total_num_inst} {incr i} {
       set subst_list [list [list inst_num $i]]
       collage_register_subsystem -subsystem_id ${ssid}_$i -subsystem_spec_dir $specdir -subsystem_copy $subst_list -subsystem_target_dir $::env(COLLAGE_WORK)/subsystem
    }
  }
}

proc soc_post_instantiate_components {} {
  source ${::auto_conn_dir}/insert_module_create.tcl
}



# Specify subsystem
proc soc_specify_subsystem {} {
  #Process global std connection file from integ area
  #collage_process_ifc_conn_file -hier_lookup -file ${::soc_integ_specs_dir}/std_clk_connection.cfg
  #Process global std connection file from subsystem areas
  set queried_subsys_list $::soc_subsystems
  foreach "ssid ssdir numinst" $queried_subsys_list {
    set specdir [file join ${::soc_subsys_specs_dir} $ssdir]
    if { [file exists ${specdir}/global_std_connection.cfg] } {
      print_info "Processing global std_connections: -subsystem_id $ssid"
      collage_process_ifc_conn_file -hier_lookup -file ${specdir}/global_std_connection.cfg
    }
  }
  #Process stubs overrides, adding pass-through
  foreach "ssid ssdir numinst" $queried_subsys_list {
    set specdir [file join ${::soc_subsys_specs_dir} $ssdir]
    if { [file exists ${specdir}/stub_override.tcl] } {
      print_info "Sourcing stubs override: -subsystem_id $ssid"
      source ${specdir}/stub_override.tcl 
    }
  }
}


################################################################
# Complete adhoc connectivity
proc soc_complete_connections {} {
  collage_process_conn_file -merge_conn -file ${::soc_integ_specs_dir}/adhoc_clk_connection.txt
  collage_process_conn_file -merge_conn -file ${::soc_integ_specs_dir}/adhoc_dfx_connection.txt
  collage_process_conn_file -merge_conn -file ${::soc_integ_specs_dir}/adhoc_soc_connection.txt
  collage_process_conn_file -merge_conn -file ${::soc_integ_specs_dir}/adhoc_pm_connection.txt
  collage_process_conn_file -merge_conn -file ${::soc_integ_specs_dir}/soc_resets.txt
  collage_process_conn_file -merge_conn -file ${::soc_integ_specs_dir}/adhoc_temporary.txt

  #same as above lines, filename is subsytem/global_adhoc_connection.txt
  #should contain IP connections to globals
  set global_ss_adhocs ""
  set queried_subsys_list $::soc_subsystems
  foreach "ssid ssdir numinst" $queried_subsys_list {
    set subsys_dir "[collage_get_subsystem_spec_dir -subsystem_id $ssdir]"
    if { [file exists ${subsys_dir}/global_adhoc_connection.txt] } {
      print_info "Adding global adhoc: -subsystem_id $ssid"
      append global_ss_adhocs " ${subsys_dir}/global_adhoc_connection.txt"
    }
  }

  if {$global_ss_adhocs != ""} {
    print_info "Processing global adhoc from all subsystems."
    collage_process_conn_file -merge_conn -file $global_ss_adhocs
  }


  #foreach par [concat [collage_get_par_names] "/"] {
  #  set name [collage_get_hier_name -ip_name $par]
  #  collage_export_unconnected_ports -context $name -prefix ""
  #}
  #foreach par [concat [collage_get_par_names] "/"] {
  #  set name [collage_get_hier_name -ip_name $par]
  #  collage_export_unconnected_pins -context $name -prefix ""
  #}
}


################################################################
# Complete temporary connections
proc soc_complete_temporary_connections {} {
}

################################################################
# Pre-initialize hook
proc soc_pre_hook_initialize {} {
}

################################################################
# Post-initialize hook
proc soc_hook_initialize {} {
  #read_kb ${::install_kits_root}/IOSFSideband/gplugins/GMultiCast/GMultiCast.kb
  read_kb $::env(COLLAGE_ROOT)/comps/rtl_integ/templates/gplugins/GClocks.kb
  
  collage_process_instance_define_file -file ${::soc_integ_specs_dir}/${::design}_inst_alias.txt
  collage_set_hier_spec -file ${::soc_integ_specs_dir}/${::design}_par.txt
}

# ----------------------------------------------------------------------------------------
# Items to be run only on nightly / model releases and not with 
# each make_soc
# ----------------------------------------------------------------------------------------
proc soc_nightly_release {} {
}


# ----------------------------------------------------------------------------------------
# Finalize hook
# ----------------------------------------------------------------------------------------
proc soc_hook_finalize { {assign_soc_check 0} } {
}

# ----------------------------------------------------------------------------------------
# Generate subsystem RTL hook
# ----------------------------------------------------------------------------------------
proc soc_hook_generate_subsystem_rtl {} {
  print_info "SoC Hook: GenerateSubsystemRTL"
}

# ----------------------------------------------------------------------------------------
# Generate collaterals hook
# ----------------------------------------------------------------------------------------
proc soc_hook_generate_collaterals {} {
  update_insert_modules_hdl

  collage_write_hier_defines -file [file join $::env(COLLAGE_WORK) "gen/tb_specs/${::design}_ip_hier_defines.sv"] -prefix ${::design}
  #collage_write_hier_defines -file [file join $::env(MODEL_ROOT) "src/gen/collage/${::design}_ip_hier_defines.sv"] -prefix ${::design}
}

################################
# Text editor settings
# Local Variables:
# mode: tcl
# tcl-indent-level: 2
# End:
# vim: set expandtab tabstop=2 shiftwidth=2 softtabstop=2 :
