namespace eval collage_tb {} {

  variable fabric_ip_map ; # SV interface to fabric blocks mapping
  variable fabric_iosf_pri_hier;
  variable fabric_iosf_sb_hier;
  variable fabric_pnd_hier; 
  variable enable_suffix;
  variable ipenable_ifc_name_map;

  # SV Interface to fabric IPs name map
  array set fabric_ip_map {} 
  set fabric_iosf_pri_hier ""
  set fabric_iosf_sb_hier ""
  set fabric_pnd_hier ""

  array set ipenable_ifc_name_map {}
  set enable_suffix "_ENABLE"

  array set fabric_uspec_map {}

  proc _isa_fabric_generic { sv_inst intf_component} {
    set ti_name [collage_tb_get_svinst_to_tiinst $sv_inst]
    set ti_comp [::collage_tb::_collage_get_ti_module_name $ti_name]
    if {$ti_comp == ""} {
      return 0
    }
    set ti_intf_name [_collage_tb_get_ti_intf_name $ti_name $sv_inst]
    if {[info exists ::collage_tb::collage_tb_misc_arr($intf_component,$ti_comp)]} {
      if {$::collage_tb::collage_tb_misc_arr($intf_component,$ti_comp) == 1} {
        return 1
      }
      if {[lsearch $::collage_tb::collage_tb_misc_arr($intf_component,$ti_comp) $ti_intf_name] != -1} {
        return 1
      }
    }
    return 0
  }

  proc _isa_rtl_intf_def {intf_comp rtl_intf_defs} {
    foreach rtl_intf_def $rtl_intf_defs {
      foreach "k vs" [array get ::collage_interface_lists intfdef2intf,$rtl_intf_def,*] {
        foreach v $vs {
          if {$v == $intf_comp} {
            return 1
          }
        }
      }
    }
    return 0
  }

  proc _isa_inst_prop {ti_inst instlist} {
    foreach inst $instlist {
      set inst [regsub "^\\*" $inst ".*"]
      if {[regexp "^$inst$" $ti_inst]} {
        return 1
      }
    }
    return 0
  }

  # -----------------------------------------------------
  # Given an SV interface, check if it IOSF::Primary
  # -----------------------------------------------------
  proc _isa_iosf_primary_sv_ifc {intf_component} {
    # is iosf primary intf
    set iosf_primary_sv_ifcs ""
    foreach "k v" [array get ::collage_interface_lists intfdef2intf,IOSF::Primary,*] {
      append iosf_primary_sv_ifcs " $v"
    }
    set iosf_primary_sv_ifcs [lsort -uniq $iosf_primary_sv_ifcs]
    if {[lsearch $iosf_primary_sv_ifcs $intf_component] != -1} {
      return 1
    }
    return 0
  }

  # -----------------------------------------------------
  # Given an SV interface, check if it IOSF::SB
  # -----------------------------------------------------
  proc _isa_iosf_sb_sv_ifc {intf_component} {
    # is iosf sb intf
    set iosf_sb_sv_ifcs ""
    foreach "k v" [array get ::collage_interface_lists intfdef2intf,IOSF::SB,*] {
      append iosf_sb_sv_ifcs " $v"
    }
    set iosf_sb_sv_ifcs [lsort -uniq $iosf_sb_sv_ifcs]
    if {[lsearch $iosf_sb_sv_ifcs $intf_component] != -1} {
      return 1
    }
    return 0
  }
  # -----------------------------------------------------
  # Given an SV interface, check if it PND2 IDI
  # -----------------------------------------------------
  proc _isa_pnd2_idi_sv_ifc {intf_component} {
    # is PND2::IDI
    return [::collage_tb::_isa_typeof_sv_ifc $intf_component PND2::IDI]
  }

  # -----------------------------------------------------
  # Given an SV interface, check if it PND2 PMI
  # -----------------------------------------------------
  proc _isa_pnd_pmi_sv_ifc {intf_component} {
    # is PND::PMI
    set res [::collage_tb::_isa_typeof_sv_ifc $intf_component PND::PMI]
    if {$res} {
      return 1
    }
    return [::collage_tb::_isa_typeof_sv_ifc $intf_component PND2::PMI]
  }

  # -----------------------------------------------------
  # Generic proc to check if an SV interface is of a certain RTL interface type
  # -----------------------------------------------------
  proc _isa_typeof_sv_ifc {intf_component typeof} {
    set sv_ifcs ""
    foreach "k v" [array get ::collage_interface_lists intfdef2intf,$typeof,*] {
      append sv_ifcs " $v"
    }
    set sv_ifcs [lsort -uniq $sv_ifcs]
    if {[lsearch $sv_ifcs $intf_component] != -1} {
      return 1
    }
    return 0
  }

  # -----------------------------------------------------
  # Given a dut_intf, determine if a fabric IP
  # TODO: the algorithm in this procedure needs to be generalized into a policy mechanism
  #       to scale across multiple interfaces
  # Example dut_intf : par_psf.psf0
  #         intf_component: iosf_sbc_intf
  # -----------------------------------------------------
  proc _isa_fabric_ip_by_dutintf {dut_inst intf_component {sv_inst ""} dut_inst_hier} {
    set is_fabric 0
    
    #ADITI: Need to be fixed when one sv interface is mapped to multiple dut inst
    if {$::collage_tb::ip_tb || $::collage_tb::custom_tb} {
      #generating an ip testbench, always set to fabric to get all interface capabilities (ie monitor/driver)
      set is_fabric 1
    } elseif {[lsearch $::collage_tb::fabric_iosf_pri_hier [collage_get_hier_to_flat_name -hier_name $dut_inst_hier]] != -1 && [_isa_iosf_primary_sv_ifc $intf_component]} {
      # is PSF
      set is_fabric 1
    } elseif {[lsearch $::collage_tb::fabric_iosf_sb_hier [collage_get_hier_to_flat_name -hier_name $dut_inst_hier]] != -1 && [_isa_iosf_sb_sv_ifc $intf_component]} {
      # is SBR
      set is_fabric 1
    } elseif {[lsearch $::collage_tb::fabric_pnd_hier [collage_get_hier_to_flat_name -hier_name $dut_inst_hier]] != -1 && ([_isa_pnd2_idi_sv_ifc $intf_component] || [_isa_pnd_pmi_sv_ifc $intf_component])} {
      # is System Agent
      set is_fabric 1
    } else {
      if {$sv_inst != ""} {
          set is_fabric [_isa_fabric_generic $sv_inst $intf_component]
      }
    }
    if {$collage_tb::debug_mode} {
      echo "_COLLAGE_TB: is_fabric $is_fabric  dut_inst: $dut_inst intf_component $intf_component "
    }
    return $is_fabric

  }

  # -----------------------------------------------------
  # Check if SV II is to be guarded by IP_ENABLE ifdef or generate
  # - yes: if user specified enable name
  #        if IP
  # - no: if not yes, and is_fabric
  # -----------------------------------------------------
  proc _isa_ifdef_sv_ifc {intf_inst dut_intf is_fabric} {
  }

  # -----------------------------------------------------
  # Get IP Enable name (given interface instance name)
  # -----------------------------------------------------
  proc _get_ip_enable_name {intf_inst dut_intf is_fabric {fabric_enable 0}} {
    set suffix $::collage_tb::enable_suffix

    # Lookup in user specified IP enable name map
    set uspec_name [::collage_tb::_get_user_ip_enable_name_by_ifc_inst $intf_inst]
    if {$uspec_name != ""} {
      if {$::collage_tb::reduce_fabric_enable && $fabric_enable} {
        set fspec_name [::collage_tb::_get_fabric_ip_enable_name $intf_inst $dut_intf]
        if {$fspec_name != ""} {
          set ::collage_tb::fabric_uspec_map($fspec_name) $uspec_name
        }
      }
      return $uspec_name
    }

    # If does not exist in user specified, then lookup from fabric spec
    set fspec_name [::collage_tb::_get_fabric_ip_enable_name $intf_inst $dut_intf]
    if {$fspec_name != ""} {
      return $fspec_name
    }

    # If still not found, then determine based on instance name
    set ret_val ""
    set tiname [::collage_tb::_collage_get_ti_inst_name [collage_tb_get_svinst_to_tiinst $intf_inst]]
    if {$is_fabric || $tiname == ""} {
      set tmpstr [::collage_tb::_collage_get_ti_inst_name $intf_inst]
      if {$::collage_tb::enable_reconfig_tb} {
        if {[info exists ::collage_tb::sv_interface_map($tmpstr,tiinst)]} {
          set subs_val $::collage_tb::sv_interface_map($tmpstr,tiinst)
          set subs_val [string toupper [::collage_tb::_collage_get_ti_inst_name $subs_val]]
        } else {
          collage_message_print "TB-019" "RECONFIG TB - tiinst name $tmpstr doesn't exist for generated names"
        }
        set tmpstr [string toupper $tmpstr]
        set tmpstr [regsub $subs_val $tmpstr "${subs_val}${::collage_tb::general_ti_str}"]
        set ret_val ${tmpstr}${suffix}
        #set ret_val [string toupper $tmpstr]${::collage_tb::general_ti_str}${suffix}
      } else {
        set ret_val [string toupper $tmpstr]${suffix}
      }
    } else {
      set ret_val [_get_ip_enable_name_by_ti_inst $tiname]
    }
    return $ret_val
  }


  # -----------------------------------------------------
  # Lookup IP enable name automatically from fabric spec
  # -----------------------------------------------------
  proc _get_fabric_ip_enable_name {intf_inst dut_inst} {
    set rtlport [collage_tb_get_sv_inst_rtlport_name $intf_inst]
    if {$rtlport != ""} {
      if {$::collage_tb::enable_reconfig_tb} {
        return "[string toupper $rtlport]${::collage_tb::general_ti_str}$::collage_tb::enable_suffix"
      } else {
        return "[string toupper $rtlport]$::collage_tb::enable_suffix"
      }
    } else {
      return ""
    }
    #set ret_val ""
    # Extract the ip and rtl collage ifc name (to lookup from the param info for sbr & psf)
    #     set dut_inst_intf_list [collage_tb_get_svinst_to_dut_list $intf_inst]
    #     if {$dut_inst_intf_list != ""} {
    #       set ifc_name [lindex [lindex $dut_inst_intf_list 0] 1]
    #       set rtl_port_number [lindex [split $ifc_name "_"] end]
    
    # Check in SB 
    #       set dut_cell [find_item -quiet -type cell $dut_inst]
    #       set param_info_key ${dut_inst},per_port,${rtl_port_number},P${rtl_port_number}_NAME
    #       if {[info exists ::collage_tb::sb_param_info($param_info_key)]} {
    #         set ret_val $::collage_tb::sb_param_info($param_info_key)
    #         print_info "COLLAGE_TB returning for $intf_inst $ifc_name $ret_val"
    #       }
    
    #       if {$ret_val != ""} {
    #         set ret_val [string toupper $ret_val]
    #         set suffix $::collage_tb::enable_suffix
    #         set ret_val ${ret_val}${suffix}
    #       }
    #    }

    #return $ret_val
  }

  # -----------------------------------------------------
  # Get IP Enable name (given test island intance name)
  # -----------------------------------------------------
  proc _get_ip_enable_name_by_ti_inst {ti_inst} {
    set suffix $::collage_tb::enable_suffix

    # Lookup in user specified IP enable name map
    if {[info exists ::collage_tb::ipenable_ti_name_map($ti_inst)]} {
      return $::collage_tb::ipenable_ti_name_map($ti_inst)${suffix}
    } else {
      regsub "_ti$" $ti_inst "" ti_inst
      set ipname_uppercase [string toupper $ti_inst]
      if {$collage_tb::debug_mode} {
        print_info "COLLAGE_TB: _get_ip_enable_name_by_ti_inst ti_inst $ti_inst ipname_uppercase $ipname_uppercase"
      }
      if {$::collage_tb::enable_reconfig_tb} {
        return ${ipname_uppercase}${::collage_tb::general_ti_str}${suffix}
      }
      return ${ipname_uppercase}${suffix}
    }
  }


  # -----------------------------------------------------
  # IP Enable related internal APIs (get by ifc inst)
  # -----------------------------------------------------
  proc _get_user_ip_enable_name_by_ifc_inst {intf_inst} {
    if {[info exists ::collage_tb::ipenable_ifc_name_map($intf_inst)]} {
      set suffix $::collage_tb::enable_suffix
      return $::collage_tb::ipenable_ifc_name_map($intf_inst)${suffix}
    }
    return ""
  }


  ###############################################################################
  # process IP enable spec file
  ###############################################################################
  proc _collage_tb_process_en_ti_line {inst_name en_name} {
    set inst_name [::collage_tb::_collage_get_ti_inst_name $inst_name]
    set ::collage_tb::ipenable_ti_name_map($inst_name) $en_name
  }

  proc _collage_tb_process_en_ii_line {inst_name ifc_name en_name} {
    set ifc_name ${inst_name}_${ifc_name}
    set ::collage_tb::ipenable_ifc_name_map($ifc_name) $en_name
  }

  #GET it from new code
  proc _collage_tb_process_ip_enable_file {fn} {
    set line_num 0
    sv_for_file l $fn {
      incr line_num
      set l [string trim $l]
      if {$l == ""} {continue} 
      if {[regexp {^\#} $l]} { continue }
      
      set l_rest [sv_lassign $l kw]
      switch $kw {
        "EN_TI" {
          sv_lassign $l_rest inst_name en_name
          _collage_tb_process_en_ti_line $inst_name $en_name
        }
        "EN_II" {
          sv_lassign $l_rest inst_name rtl_ifc_name en_name
          _collage_tb_process_en_ii_line $inst_name $rtl_ifc_name $en_name
        }
        "default" {
          set err_msg "Illegal line \"$l\" in the adhoc connection file $fn at line number $line_num"
          collage_message_print "TB-048" $err_msg
          collage_message_print "TB-091" $err_msg	
        }
      }
    }
  }
}

###############################################################################
# Set ifdef name for IP enable used in generated testbench
###############################################################################
proc collage_tb_process_ipenable_names {args} {
  parse_proc_arguments -args $args opts

  # ------------
  # Parse options
  # ------------
  set fabric_spec_dirs ""
  if {[info exists opts(-fabric_spec_dirs)]} {
    set fabric_spec_dirs $opts(-fabric_spec_dirs)
  }

  set fabric_ifc_name ""
  if {[info exists opts(-fabric_ifc_name)]} {
    set fabric_ifc_name $opts(-fabric_ifc_name)
  }

  if {($fabric_ifc_name == "" && $fabric_spec_dirs != "") || ($fabric_ifc_name != "" && $fabric_spec_dirs == "") } {
    collage_message_print "TB-092" "Fabric IFC name and fabric spec dirs must be specified together"
  }
  

  # ------------
  # Error checks
  # ------------
  foreach fabric_spec_dir "$fabric_spec_dirs" {
    if {![file isdirectory $fabric_spec_dir]} {
      collage_message_print "TB-093" "Fabric directory for fabric ${fabric_id} does not exist - $fabric_spec_dir"
      if {![file readable $fabric_spec_dir]} {
        collage_message_print "TB-094" "Fabric directory for fabric ${fabric_id} is not readable - $fabric_spec_dir"
      }
    }
  }

  #print_info "Fabric specs: $fabric_spec_dirs Fabric ID:"

  foreach fabric_spec_dir "$fabric_spec_dirs" {
    collage_message_print "TB-020" "Processing fabric spec: $fabric_spec_dir"
    set ipconfig_specs_dir [::collage_fabric::_get_ipconfig_specs_dir $fabric_spec_dir]
    set ipparam_files [glob -nocomplain ${ipconfig_specs_dir}/[::collage_fabric::_get_param_file_glob]]
    foreach ipparam $ipparam_files {
      set inst_name [string range [file tail $ipparam] 0 end-[string length [::collage_fabric::_get_param_file_ext]]]
      set line_num 0
      sv_for_file l $ipparam {
        incr line_num
        set l [string trim $l]
        if {$l == ""} {continue}
        if {[regexp {^\#} $l]} { continue }
        sv_lassign $l pname pval
        # ----------------------------------
        # Check if per-port or per instance param
        # P<n>_NAME
        # ----------------------------------
        if {($fabric_ifc_name == "IOSF::SB") || ($fabric_ifc_name == "IOSF::Primary" && $::collage_fabric::psf_version == 1)} {
          if {[regexp {^P([0-9]).*} $pname]} {
            set port_num [string range [lindex [split $pname "_"] 0] 1 end]
            set ::collage_tb::sb_param_info(${inst_name},per_port,$port_num,${pname}) $pval
          } else {
            set ::collage_tb::sb_param_info(${inst_name},per_inst,all,$pname) $pval
          }
        }
      }
    }
  }
}

define_proc_attributes collage_tb_process_ipenable_names \
    -info "Collage - set IP enable names for TB interface instances. If specified, port name parameters from fabric specs will be used." \
    -define_args {
      {"-fabric_spec_dirs"   "Sideband fabric spec dirs (space separated list)"   "" string optional}
      {"-fabric_ifc_name"    "Fabric interface definition name (example, IOSF::SB)"   "" string optional}
    }

