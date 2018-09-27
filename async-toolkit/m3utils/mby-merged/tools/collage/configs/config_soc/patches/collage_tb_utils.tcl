#################################################################################
# Main procedure for assembling a testbench
#################################################################################
package require fileutil
#set tb_interface_defs_dir [file join [file dirname [info script]] "tb_interface_defs"]
namespace eval collage_tb {} {
  variable ipenable_ifc_name_map;
  variable ipenable_ti_name_map;
  variable ip_tb;
  variable psf_param_info;
  variable use_generate;
  variable add_ti_adhoc_file;
  set add_ti_adhoc_file 0;
  set use_custom_dut_writer 1
  variable tb_use_one_to_n;
  set ip_tb 0;
  set use_generate 0;
  set tb_use_one_to_n 1
  set dut_prefix {}
  array set psf_param_info {}
  set sbr_active 0
  set sbr_active_env ""
  set sbr_config_mod_name "sbr_generic"
  set pmi_ver 1.0
  set idi_ver "2.0"
  set sbr_version ""
  set bfm_ver 1.0
  #used for 14ww04 bfm option
  set sv_intf_dis_compmon ""
  #multi-sbr network support
  array set sbr_design_inst {}
  array set sbr_subsystems_map {}
  array set sbr_subsystems_prefix [list "iosf_sideband" "chs"]
  set sbr_subsystems ""

  array set ::collage_tb::num_rtl_ports {}
  set ::collage_tb::disable_params ""
  set ::collage_tb::exclude_rtb_ti_files ""
  set ::collage_tb::skip_ti_inst 0
  set ::collage_tb::dont_generate 0
  set ::collage_tb::prim_rtl_intf_ver "1.1.prec"
  set ::collage_tb::prim_intf_def_file "iosf_interface.1.1.prec.tcl"
  set ::collage_tb::ifdef_var "`ifdef"
  set ::collage_tb::aggr_ti_file ""
  set ::collage_tb::aggr_ti_fp ""
  set ::collage_tb::f2f_list ""
  set ::collage_tb::use_hier_conn 0
  set ::collage_tb::add_ti_adhoc_file 0
  set ::collage_tb::enable_auto_wdu 0
  set ::collage_tb::enable_auto_wdu_include 0
  set ::collage_tb::enable_split_tb_sv 0
  set ::collage_tb::general_ti_str "{%%x}"
  set ::collage_tb::tb_top_define "`TB_TOP"
  set ::collage_tb::enable_hier_alias 0
  set ::collage_tb::enable_reconfig_tb 0
  set ::collage_tb::multi_rtb_consumer 0
  set ::collage_tb::unused_param_check 0
  set ::collage_tb::assign_open 0
  set ::collage_tb::allow_multiple_conns 0
  set ::collage_tb::custom_tb 0
  set ::collage_tb::gen_param_file 1
  set ::collage_tb::active_en_suffix "_ACTIVE"
  set ::collage_tb::pre_post_include_prefix "soc"
  set enable_emulation_ifdef 0
  set disable_auto_install 0
  set mi_corekit_flow 0
  set ::collage_tb::emu_sbr_ver 1
  set ::collage_tb::emu_psf_ver 1
  set ::collage_tb::emu_map 0
  set ::collage_tb::emu_tb 0
  set ::collage_tb::enable_mi_corekit 0
  set ::collage_tb::reduce_fabric_enable 0
  set ::collage_tb::stop_on_error 0
  set ::collage_tb::tb_interface_defs_dir ""

  proc init_data_structure { } {
    global env
    global collage_interface_lists
    ::array unset w_map
    ::array unset collage_interface_lists
    ::array unset ti_def_map
    ::array unset sv_interface_map
    ::array unset sv_interface_attrs_map
    ::array unset collage_tb_misc_arr
    if {[array names ::env "CHASSIS_DEBUG_MODE"] eq ""} {
      set ::collage_tb::debug_mode 0
      suppress_message "DES-4 DES-16 DES-40 CMDS-58 CMDS-243 CMDS-224 CMDS-225 CMDS-117 INTF-13 INTF-88 INTF-164 INSTALL-11"
    } else {
      set ::collage_tb::debug_mode 1
    }
    if {[array name ::env COLLAGE_INTF_DEF] eq ""} {
    } else {
      set ::collage_tb::tb_interface_defs_dir $::env(COLLAGE_INTF_DEF)/tb_interface/interface_map
    }
  }

  ###############################################################################
  # Look up rtl component by module or instance name
  ###############################################################################
  #proc _collage_tb_check_rtl_component_type {ip_cellname {old_proc 0}} {}
  proc _collage_tb_check_rtl_component_type {ip_cellname} {

    set is_instance_name 0
    set inst_not_found 0
    set rtl_instance ""
    set rtl_instance_hier ""
    set rtl_instance_real_hier ""
    set hier_inst ""
    
    #check to see if mapped to rtl instance name or module name
    set fields [split $ip_cellname ":"]
    #check for instance syntax
    if {[llength $fields] == 2 && [lindex $fields 0] == "inst"} {
      set is_instance_name 1
      set my_cellname [::col::get_ip_real_name [lindex $fields 1]]
    } else {
      #module syntax
      set is_instance_name 0
      set my_cellname $ip_cellname 
    }

#    if {[collage_is_mi_inst -name $my_cellname] && $old_proc} {
#      error "collage_ti_definition is not supported for MI. Please use collage_tb_add_ti_inst instead and rerun."
#    }
    if {$::collage_tb::enable_mi_corekit && [collage_is_mi_inst -name $my_cellname]} {
      #Dump MI info
    } elseif {$::collage_tb::enable_mi_corekit && ![collage_is_mi_inst -name $my_cellname]} {
      return "0 0 0 0 0"
    } elseif {!$::collage_tb::enable_mi_corekit && [collage_is_mi_inst -name $my_cellname] && $::collage_tb::mi_corekit_flow} {
      return "0 0 0 0 0"
    } 

    if {[collage_is_mi_inst -name $my_cellname]} {
      set hier_inst [collage_get_hier_name -ip_name $my_cellname]
    }

    #search for instance name and verify it exists
    if {$is_instance_name} {
      #ADDING FOR MI
      #for corekit flow, my_cellname based on folded_instance_map is needed before setting hierarchy
      if {[info exists ::col_mi::folded_instance_map($my_cellname)] && $::collage_tb::enable_mi_corekit} {
        set hier_inst [collage_get_hier_name -ip_name $my_cellname]
        #set my_cellname $::col_mi::folded_instance_map($my_cellname)
        #set my_cellname [lindex [split $my_cellname "/"] 1]
        #set rtl_instance_real_hier [join "[string range $hier_inst 0 [expr [string last "/" $hier_inst] -1]] $my_cellname" "/"] 
      }
      eval_in_component [collage_get_ip_hier_par -ip_name $my_cellname] {
        set cell [find_item -type cell -quiet [collage_get_folded_name -name $my_cellname]]
        if {$cell != ""} {
          set rtl_instance [collage_get_folded_name -name $my_cellname]
          #set rtl_instance_hier [string trimleft [get_attribute -attr ComponentOfItem $cell] "/"]
          set rtl_instance_hier [collage_get_hier_name -ip_name $my_cellname]
          set inst_not_found 0
        } else {
          set inst_not_found 1
        }
      }
    } else {
      #map rtl instance ptr, get rid of module names and verify inst exists
      #find collage instances in workspace
      #get all instance of the module
      set design [find_item -quiet -type design -filter "IsHierarchicalComponent==0 && (UnelabName==$my_cellname || Name==$my_cellname)"]

      set num [sizeof_collection $design]
      echo "collage_tb_check_rtl_component_type module:$ip_cellname cellname:$my_cellname num $num"
      if {$num == 0} {
        set inst_not_found 1
      } else {
        #if more than one we cannot map this module
        if {$num > 1} {
          collage_message_print "TB-049" "More than one instance of $ip_cellname, cannot map to TI"
        } else {
          #convert coretools instance ptr to module name
          
          set rtl_instance [get_attribute -attrs "Name" [index_collection [get_attribute -attrs CellReferences $design]  0]]
          set rtl_instance_hier [string trimleft [get_attribute -attr ComponentOfItem $design] "/"]

        }
      }
    }
    
    return "$is_instance_name $inst_not_found $rtl_instance $rtl_instance_hier $hier_inst"
  }

  proc _collage_tb_set_rtl_conn {dut_inst_conn dut_intf_conn dut_inst} {
    set ::collage_tb::collage_tb_misc_arr($dut_inst_conn,ADD_ENABLE_NAME,$dut_intf_conn) $dut_inst
  }

  proc _collage_tb_set_ip_enable_rtl {dut_inst dut_intf enable_name} {
    if {[info exists ::collage_tb::collage_tb_misc_arr($dut_inst,ENABLE_NAME_RTL,$dut_intf)]} {
      lappend ::collage_tb::collage_tb_misc_arr($dut_inst,ENABLE_NAME_RTL,$dut_intf) $enable_name
    } else {
      set ::collage_tb::collage_tb_misc_arr($dut_inst,ENABLE_NAME_RTL,$dut_intf) $enable_name
    }
  }

  proc _collage_tb_get_ip_enable_rtl {dut_inst dut_intf} {
    if {[info exists ::collage_tb::collage_tb_misc_arr($dut_inst,ENABLE_NAME_RTL,$dut_intf)]} {
      return $::collage_tb::collage_tb_misc_arr($dut_inst,ENABLE_NAME_RTL,$dut_intf)
    }
    return ""
  }

  proc _collage_tb_add_enable_name_conn {} {
    foreach rtl_intf_conn [lsort [array names ::collage_tb::collage_tb_misc_arr *,ADD_ENABLE_NAME,*]] {
      set dut_inst_conn [lindex [split $rtl_intf_conn ,] 0]
      set dut_intf_conn [lindex [split $rtl_intf_conn ,] 2]
      set dut_inst $::collage_tb::collage_tb_misc_arr($dut_inst_conn,ADD_ENABLE_NAME,$dut_intf_conn) 
      set enable_name [_collage_tb_get_ip_enable_rtl $dut_inst_conn $dut_intf_conn]
      if {$enable_name != "" && !$::collage_tb::enable_reconfig_tb} {
        _collage_tb_set_ip_enable_names $dut_inst $enable_name
      }
    }
  }

  proc _collage_tb_set_ip_enable_names {dut_inst_list enable_name} {
    foreach dut_inst $dut_inst_list {
      if {[info exists ::collage_tb::collage_tb_misc_arr($dut_inst,ENABLE_NAME)]} {
        lappend ::collage_tb::collage_tb_misc_arr($dut_inst,ENABLE_NAME) $enable_name
      } else {
        set ::collage_tb::collage_tb_misc_arr($dut_inst,ENABLE_NAME) $enable_name
      }
    }
  }
  ###############################################################################
  # Testbench verif development file. 
  # _collage_generate_intf_assigns intf_comp intf_inst dut_intf intf_type
  # creates the assign statements for an intf instance
  # dut_intf is hierarchical path separated by . 
  # intf_comp hierarchical path separated by _
  # dut_interface_list(intf_inst) generates the hierarchical path + interface name
  ###############################################################################
  proc _collage_generate_intf_assigns {intf_component intf_inst dut_inst dut_intf intf_dir key_value dut_inst_hier} {
    global param_names
    global collage_interface_lists
    #set is_fabric [::collage_tb::_isa_fabric_generic $intf_inst $intf_component]
    set is_fabric [::collage_tb::_isa_fabric_ip_by_dutintf $dut_inst $intf_component $intf_inst $dut_inst_hier]
    set assign_line ""
    set intf_inst_no_idx $intf_inst
    
    if {[regexp {\[(\d+)\]$} $intf_inst tmp inst_idx]} {
      set intf_inst_no_idx [string trim $intf_inst  $tmp]
    } else {
      set inst_idx -1
    }
    set ip_enable_name [::collage_tb::_get_ip_enable_name  $intf_inst $dut_inst $is_fabric]
    if {!$is_fabric && !$::collage_tb::enable_reconfig_tb} {
      _collage_tb_set_ip_enable_names $dut_inst $ip_enable_name
    }

    foreach key $key_value { 
      sv_lassign $key dut_inst_t dut_intf_t intf_type_t dut_dir_t dut_inst_hier_conn_t intf_role_t di_exp_from_t
      #_collage_tb_set_ip_enable_rtl $dut_inst_t $dut_intf_t $ip_enable_name
      if {$is_fabric && $dut_inst_hier_conn_t != "<undriven>"} {
      #if { $dut_inst_hier_conn_t != "<undriven>"} {}
        set dut_inst_conn [lindex [split $dut_inst_hier_conn_t "/"] end-1]
        set dut_intf_conn [lindex [split $dut_inst_hier_conn_t "/"] end]
        if {!$::collage_tb::enable_reconfig_tb} {
          _collage_tb_set_ip_enable_names $dut_inst_conn $ip_enable_name
        }
        #_collage_tb_set_rtl_conn $dut_inst_conn $dut_intf_conn $dut_inst
      }
    }
    set db_idx [collage_tb_get_intf_index $intf_inst_no_idx]
    if {![collage_get_and_set_instantiated $intf_inst_no_idx]} {
      append assign_line [_collage_generate_intf_instantiation $intf_component $intf_inst $intf_inst_no_idx $ip_enable_name $is_fabric $db_idx $inst_idx]
    } else {
      if {!$is_fabric} {
        if {$::collage_tb::use_generate} {
          if {$::collage_tb::skip_ti_inst && $::collage_tb::dont_generate} {
            append assign_line "  if($ip_enable_name == 1) begin : ${intf_inst_no_idx}_${inst_idx}\n"
          } else {
            append assign_line "generate\n  if($ip_enable_name == 1) begin : ${intf_inst_no_idx}_${inst_idx}\n"
          }
          lappend param_names $ip_enable_name
        } else {
          append assign_line "${::collage_tb::ifdef_var} $ip_enable_name\n"
        }
      }
    }
    if {!$is_fabric && ($inst_idx != -1) && ($::collage_tb::use_generate== 1)} {
	  set preinststr "${intf_inst_no_idx}."
    } else {
	  set preinststr ""
    }
    
    set assign_line_mon "   // START: custom assign for $intf_inst of $intf_component with type M\n"
    set assign_line_drv "   // START: custom assign for $intf_inst of $intf_component with type D\n"
    set dutintfdefs [collage_tb_get_sv_intf_dutintfdefs $intf_inst]
    foreach intf_key [array names collage_interface_lists $intf_component,*,*,interface,*] { 
      foreach {icomp idutintfdef idut_dir interface ipindef} [split $intf_key ,]  {}
      set val $collage_interface_lists($intf_key)
      if {[lindex $val 0] == "open"} {
        if {![info exists ::collage_tb::sv_interface_attrs_map($intf_inst,isused,$ipindef)]} {
          collage_tb_set_sv_intf_open $intf_inst $ipindef [lindex $val 1]
        }
        continue
      }

      set dut_dir [collage_tb_get_sv_intf_pin_dir $intf_inst $ipindef]
      if {($dut_dir != "" || $idut_dir != $intf_dir)  && $dut_dir != $idut_dir} continue
      if {[lsearch -exact $dutintfdefs $idutintfdef] == -1} continue

      set ipin [lindex $collage_interface_lists($intf_key) 0]
      set dir [lindex $collage_interface_lists($intf_key) 1]
      set dim [lindex $collage_interface_lists($intf_key) 2]
      set M_isrev 0
      set D_isrev 0
      
      # create a verif port through append of intf_inst + pinname (as defined in interface_defs)
      # if I instantiate the dut in the same level of hierarchy, and replace verif_port with a hier. signal,
      # direct connection to dut is enabled.

      # change to correct interface index name(intead of default module_port name) if used in interface array
      set assign_sig "${preinststr}$intf_inst"
      append assign_sig "."  $ipin
      ###ADITI: What is dut_intf "E"
      if {$dut_inst == "E"} {
        collage_message_print "TB-050" "exported interfaces on dut are not supported in this version of tb"
        set verif_sig $intf_inst
        set verif_sig $intf_inst
        append verif_sig  "_"  $ipindef
        set collage_tb_verif_portlist($verif_sig,dim) $dim
        set collage_tb_verif_portlist($verif_sig,dir) $dir
        set collage_tb_verif_portlist($verif_sig,inst) $intf_inst
        set inst_name $intf_inst
      } else {
        set my_dut_inst [collage_tb_get_sv_intf_pin_dut_inst $intf_inst $ipindef]
        if {$my_dut_inst == ""} {
          collage_message_print "TB-022" "COLLAGE_TB: can not find dut instance for $intf_inst $ipindef"
        }
        set verif_sigM_t "[collage_tb_get_sv_intf_conn_all $intf_inst $ipindef $ipin M]"
        set verif_sigD_t "[collage_tb_get_sv_intf_conn_all $intf_inst $ipindef $ipin D]"
        set verif_sigM [lindex $verif_sigM_t 0]
        set M_isrev [lindex $verif_sigM_t 1]
        set verif_sigD [lindex $verif_sigD_t 0]
        set D_isrev [lindex $verif_sigD_t 1]
        set inst_name $intf_inst
      }

      # Generate assigns depending on whether in monitor or driver mode
      # updated on May 5th, 2012
      set dut_sigM ${verif_sigM}
      set dut_sigD ${verif_sigD}

      # Bug Fix: when no dut signal, the SV interface needs to be driven
      if {[regexp -all {<open>} $dut_sigM] || [regexp {\.$} $dut_sigM] || ($dut_dir == "" && [regexp -all {<open>} $dut_sigM])} {
        set tmp_line_mon "   assign $assign_sig = '0;\n" ; #  // RTL <open> or optional (todo, add this comment to the generated tb)
      } else {
        if {$M_isrev == 1 && $M_isrev != ""} {
          set tmp_line_mon "   assign $dut_sigM = $assign_sig;\n"
        } else {
          set tmp_line_mon "   assign $assign_sig = $dut_sigM;\n"
        }
      }
      if {[regexp -all {<open>} $dut_sigD] || [regexp {\.$} $dut_sigD] || $dut_dir == ""} {
        if {$::collage_tb::assign_open == 0} { 
          if {$::collage_fabric::psf_version != 1 && $intf_component == "iosf_primary_intf"} {
            if {[_iosf_pri_intf_assign $intf_inst $assign_sig]} {
              set tmp_line_drv "   assign $assign_sig = '0; //tie-off non-existent rtl signal \n" ; #  // RTL <open> or optional (todo, add this comment to the generated tb)
            } else {
              set tmp_line_drv  "   // $assign_sig = <open>  \n"
            }
          } else {
            set tmp_line_drv "   assign $assign_sig = '0;\n" ; #  // RTL <open> or optional (todo, add this comment to the generated tb)
          }
        } else {
          set dut_is_lhs 0
          if {$dut_dir == "consumer"} {
            if {$dir == "in"} {
              # output of consumer
              set dut_is_lhs 0
            } else {
              set dut_is_lhs 1
            }
          } else {
            if {$dir == "in"} {
              set dut_is_lhs 1
            } else {
              set dut_is_lhs 0
            }
          }

          if {$dut_is_lhs} {
            set tmp_line_drv  "   // $assign_sig = <open>  \n"
          } else {
            set tmp_line_drv "   assign $assign_sig = '0;\n" ; #  // RTL <open> or optional (todo, add this comment to the generated tb)
          }
        }
      } else {

        set dut_is_lhs 0

        # --------------------------------------------------
        # "in" - fromConsumer (output of consumer)
        # "out" - fromProvider (output of provider)
        # 
        # dut_is_lhs 
        # - consumer && out && not switchy
        # - provider && in && not switchy
        # --------------------------------------------------
        if {$dut_dir == "consumer"} {
          if {$dir == "in"} {
            # output of consumer
            set dut_is_lhs 0
          } else {
            set dut_is_lhs 1
          }
        } else {
          if {$dir == "in"} {
            set dut_is_lhs 1
          } else {
            set dut_is_lhs 0
          }
        }

        if {$dut_is_lhs} {
          if {[collage_tb_get_sv_intf_force $intf_inst]} {
            if {$D_isrev == 1 && $D_isrev != ""} {
              set tmp_line_drv "   initial force $assign_sig = $dut_sigD;\n"
            } else {
              set tmp_line_drv "   initial force $dut_sigD = $assign_sig;\n"
            }
          } else {
            if {$D_isrev == 1 && $D_isrev != ""} {
              set tmp_line_drv "   assign $assign_sig = $dut_sigD;\n"
            } else {
              set tmp_line_drv "   assign $dut_sigD = $assign_sig;\n"
            }
          }
        } else {
          set flop_sim [collage_tb_get_sv_intf_flop_sim $intf_inst]  
          if {$flop_sim != ""} {
            sv_lassign $flop_sim flop_clk delay
            set tmp_line_drv "   assign $assign_sig = \$past($dut_sigD,$delay,$flop_clk);\n"
          } else {
            set tmp_line_drv "   assign $assign_sig = $dut_sigD;\n"
          }
        }

      }

      append assign_line_mon $tmp_line_mon
      append assign_line_drv $tmp_line_drv

    }

    append assign_line_mon "[collage_generate_user_assigns $intf_inst 1 M $preinststr]"
    append assign_line_drv "[collage_generate_user_assigns $intf_inst 1 D $preinststr]"

    append assign_line_mon "   // END: custom assign for $intf_inst of $intf_component with type M\n"
    append assign_line_drv "   // END: custom assign for $intf_inst of $intf_component with type D\n"

    # For non-fabric IPs, the testbench will only have a monitor (when the IP is enabled)
    if {$is_fabric} {
      set pre_passive_val ""
      set pre_active_val ""
      set post_passive_val ""
      set post_active_val ""
      if {$::collage_tb::custom_tb} {
        if {[info proc ::collage_tb_custom_passive_assigns] != ""} {
          set val [::collage_tb_custom_passive_assigns $intf_inst $ip_enable_name]
          set pre_passive_val [lindex $val 0]
          set post_passive_val [lindex $val 1]
          if {($pre_passive_val eq "" && $post_passive_val ne "") || ($pre_passive_val ne "" && $post_passive_val eq "")} {
            collage_message_print "TB-051" "COLLAGE CUSTOM TB - One of the Pre or Post line in collage_tb_custom_passive_assigns is empty which is not allowed. Please fix and rerun."
          }
          if {$pre_passive_val ne "" && $post_passive_val ne ""} {
            append assign_line "${pre_passive_val}\n"
            set ::collage_tb::gen_param_file 0
          }
        } else {
          append assign_line "  if(${ip_enable_name}) begin : ${intf_inst}_PASSIVE\n"
          lappend param_names $ip_enable_name
        }
      } else {
        if {$::collage_tb::use_generate} {
          if {$::collage_tb::skip_ti_inst && $::collage_tb::dont_generate} {
            append assign_line "  if($ip_enable_name == 1) begin\n"
          } else {
            append assign_line "generate\n  if($ip_enable_name == 1) begin\n"
          }
          lappend param_names $ip_enable_name
        } else {
          append assign_line "${::collage_tb::ifdef_var} ${ip_enable_name}\n"
        }
      }

      if {$::collage_tb::custom_tb && [info proc ::collage_tb_custom_passive_assigns] != ""} {
        if {$pre_passive_val ne "" && $post_passive_val ne ""} {
          append assign_line "$assign_line_mon"
        }
      } else {
        append assign_line "$assign_line_mon"
      }  

      if {$::collage_tb::custom_tb} {
        #PASSIVE END
        if {[info proc ::collage_tb_custom_passive_assigns] != ""} {
          if {$pre_passive_val ne "" && $post_passive_val ne ""} {
            append assign_line "${post_passive_val}\n\n"
          }
        } else {
          append assign_line "  end : ${intf_inst}_PASSIVE\n\n"
        }

        #ACTIVE start
        #default enable name for ACTIVE assigns
        set active_enable_name [regsub "${::collage_tb::enable_suffix}$" $ip_enable_name "${::collage_tb::active_en_suffix}${::collage_tb::enable_suffix}"]
        if {[info proc ::collage_tb_custom_active_assigns] != ""} {
          set val [::collage_tb_custom_active_assigns $intf_inst $active_enable_name]
          set pre_active_val [lindex $val 0]
          set post_active_val [lindex $val 1]
          if {($pre_active_val eq "" && $post_active_val ne "") || ($pre_active_val ne "" && $post_active_val eq "")} {
            collage_message_print "TB-052" "COLLAGE CUSTOM TB - One of the Pre or Post line in collage_tb_custom_active_assigns is empty which is not allowed. Please fix and rerun."
          }
          if {$pre_active_val ne "" && $post_active_val ne ""} {
            append assign_line "${pre_active_val}\n"
            set ::collage_tb::gen_param_file 0
          }
        } else {
          append assign_line "  if(${active_enable_name}) begin : ${intf_inst}_ACTIVE\n"
          lappend param_names $active_enable_name
        }
      } else {
        if {$::collage_tb::use_generate} {
          append assign_line "  end\n  else begin\n"
        } else {
          append assign_line "`else\n"
        }
      }

      if {$::collage_tb::custom_tb && [info proc ::collage_tb_custom_active_assigns] != ""} {
        if {$pre_active_val ne "" && $post_active_val ne ""} {
          append assign_line "$assign_line_drv"
        }
      } else {
        append assign_line "$assign_line_drv"
      }

      if {$::collage_tb::custom_tb} {
        #ACTIVE end
        if {[info proc ::collage_tb_custom_active_assigns] != ""} {
          if {$pre_active_val ne "" && $post_active_val ne ""} {
            append assign_line "${post_active_val}\n"
          } 
        } else {
          append assign_line "  end : ${intf_inst}_ACTIVE\n"
        }
      } else {
        if {$::collage_tb::use_generate} {
          if {$::collage_tb::skip_ti_inst && $::collage_tb::dont_generate} {
            append assign_line "  end\n"
          } else {
            append assign_line "  end\nendgenerate\n"
          }
        } else {
          append assign_line "`endif\n"
        }
      }
    } else {
      append assign_line "$assign_line_mon"
      if {$::collage_tb::use_generate} {
        collage_tb_set_sv_intf_generate ${intf_inst_no_idx} ${intf_inst_no_idx}
        if {$::collage_tb::skip_ti_inst && $::collage_tb::dont_generate} {
          append assign_line "  end\n"
        } else {
          append assign_line "  end\nendgenerate\n"
        }
      } else {
        append assign_line "`endif\n"
      }
    }
    if {!$is_fabric && $::collage_tb::use_generate} {
      set prestr "$intf_inst_no_idx."
    } else {
      set prestr ""
    }
    append assign_line "\n[collage_generate_user_assigns $intf_inst 0 "" $prestr]"
    
    set_cell_attribute [list $intf_inst_no_idx ] {CustomizedTestbenchCode[Verilog_Additional_Pre_Instance]} "`ifndef THIS_TO_BE_STRIPPED_OUT"
    set_cell_attribute [list $intf_inst_no_idx ] {CustomizedTestbenchCode[Verilog_Additional_Post_Instance]} "`endif"
    # shove into some global var or file, prevents re-ordering by coretools.  This will break interface array instantiations
    set ti_inst_name [collage_tb_get_svinst_to_tiinst $intf_inst]
    set ti_inst_name [_collage_get_ti_inst_name $ti_inst_name]
    set assign_line [collage_tb_reconfig_sv_intf_inst $assign_line $ti_inst_name]
    return $assign_line
  }

  proc collage_generate_user_assigns {intf_inst writenable type {prestr ""}} {
    set assign_line ""
    #puts "intf_inst $intf_inst"
    foreach pin [collage_tb_get_sv_intf_userpins $intf_inst] {
      if {$writenable} {
        append assign_line [collage_generate_single_user_assign $intf_inst $pin 1 $type $prestr]
      } else {
        append assign_line [collage_generate_single_user_assign $intf_inst $pin 0 M $prestr]
        append assign_line [collage_generate_single_user_assign $intf_inst $pin 0 D $prestr]
        
      }
    }
    return $assign_line
  }
  
  proc collage_generate_single_user_assign {intf_inst pin writenable type prestr} {
    if {$writenable} {
      set sp "   "
    } else {
      set sp ""
    }
    set assign_line ""

    set pinstr [collage_tb_get_sv_intf_userconn $intf_inst $pin $type]
    if {$pinstr == ""} {
      return ""
    }
    sv_lassign $pinstr usrconn isenable isforce isrev
    if {$isenable != $writenable} {
      return ""
    }
    collage_tb_reset_sv_intf_open $intf_inst $pin
    if {$type == "D"} {
      if {$isrev} {
        if {$isforce} {
          set assign_line "${sp}initial force ${prestr}${intf_inst}.$pin = $usrconn;\n"
        } else {
          set assign_line "${sp}assign ${prestr}${intf_inst}.$pin = $usrconn;\n"
        }
      } else {
        if {$isforce} {
          set assign_line "${sp}initial force $usrconn = ${prestr}${intf_inst}.$pin;\n"
        } else {
          set assign_line "${sp}assign $usrconn = ${prestr}${intf_inst}.$pin;\n"
        }
      }
    } else {
      if {$isforce} {
        set assign_line "${sp}initial force ${prestr}${intf_inst}.$pin = $usrconn;\n" 
      } else {
        set assign_line "${sp}assign ${prestr}${intf_inst}.$pin = $usrconn;\n" 
      }
    }
    return $assign_line
  }

  ###############################################################################
  # Custom netlist of SV ifc instantiation
  # - necessary to write out the ports of the SV interface
  # ASSUMPTIONS: these assumptions need to be fixed, can be incorrect
  # - the SV interface has at least one parameter and at least one port
  # - the ports on the SV interface are all single bit
  ###############################################################################
  proc _collage_generate_intf_instantiation {module_name inst_name_full inst_name ip_enable_name is_fabric {instidx -1} {curidx -1}} {
    global interface_inst_list
    global param_names

    array set inst_params {}
    set inst_pins [list]
  
    # ---------------------------------
    # Find the pins of the SV interface
    # ---------------------------------
    foreach_in_collection pin [find_item -quiet -type pin ${inst_name}/* -filter "SVUsageInfo==FromPort"] {
      sv_lassign [get_attribute -attrs "InterfacePortName RangeDecoratedName" $pin] p_name rd_name
      lappend inst_pins $p_name
    }
    #parray inst_pins ; # debug only

    # ---------------------------------------
    # Find the parameters of the SV interface
    # ---------------------------------------
    foreach_in_collection p [find_item -quiet -type param ${inst_name}/* -filter "FromArchitecture==0"] {
      sv_lassign [split [get_attribute -attrs Name $p]  "/"] comp pname
      # Note: intentional to get the parameter value from the array (instead of the component) as the 
      #       code is being run before SpecifySubsystem is complete
      set comp1 $comp
      if {$instidx != -1} {
        set comp1 "${comp}\[$instidx\]"
      }
      if {![collage_tb_is_ignore_sv_intfdef_param $module_name $pname]} {
        set pval [collage_tb_get_sv_intf_pval $comp1 $pname]
        if {$pval == ""} {
          set pval [get_configuration_parameter -component $comp $pname]
          if {[regexp {^0x} $pval]} {
            set pval [_collage_tb_get_hdl_value $comp $pname]
          }
        }
      
        set inst_params($pname) $pval
      }
    }

    # ----------------------------------------------------------------------------------
    # Generate the RTL, used by post-processing to strip out coretool interface instance 
    # ----------------------------------------------------------------------------------
    #append ret_val "`endif\n"

    # For non-fabric IPs, the SV interface and assign are enabled together under the same ifdef
    set ip_enable_name_intf $ip_enable_name
    if {!$is_fabric} {
      if {$instidx != -1} {
        set tiname [::collage_tb::_collage_get_ti_inst_name [collage_tb_get_svinst_to_tiinst $inst_name_full]]
        if {$tiname != ""} {
          set ip_enable_name_intf [_get_ip_enable_name_by_ti_inst $tiname]
        }
      } 
      if {$::collage_tb::use_generate} {
        if {$::collage_tb::skip_ti_inst && $::collage_tb::dont_generate} {
          append ret_val "  if($ip_enable_name_intf == 1) begin : ${inst_name}\n"
        } else {
          append ret_val "generate\n  if($ip_enable_name_intf == 1) begin : ${inst_name}\n"
        }
        lappend param_names $ip_enable_name_intf

      } else {
        append ret_val "${::collage_tb::ifdef_var} ${ip_enable_name_intf}\n"
      }
    }

    append ret_val "   // START: custom instantiation of SV interface\n"

    # -- logic wires for the ports
    foreach pname $inst_pins {
      #append ret_val "   logic ${inst_name}_${pname};\n"
    }
    append ret_val "\n"

    # ----------------
    # write parameters
    # ----------------

    if {$instidx == -1} {
      set if_inst_name $inst_name
    } else {
      set instarr [expr $instidx+1]
      set if_inst_name "$inst_name\[$instarr\]"
    }

    set num_params [array size inst_params]
    if {$num_params > 0} {
      append ret_val "   ${module_name}\n"
      append ret_val "      #("
      set idx 0
      # TODO: sort the parameter names before writing. Using array names makes output change
      foreach pname [lsort [array names inst_params]] {
        set pval $inst_params($pname)
        incr idx
        if {$idx == 1} {
          # this is the first param
          append ret_val ".${pname}($pval)"
        } else {
          append ret_val "        .${pname}($pval)"
        }

        if {$idx == $num_params} {
          # this is the last param
          append ret_val ") ${if_inst_name}\n"
        } else {
          append ret_val ",\n"
        }
      }
    } else {
      append ret_val "   ${module_name}        ${if_inst_name}\n"
    }

    # -----------
    # write ports
    # -----------
    set num_ports [sizeof_collection [find_item -quiet -type pin ${inst_name}/* -filter "SVUsageInfo==FromPort"]]
    if {$num_ports > 0} {
      set idx 0
      foreach pname [lsort $inst_pins] {
        set pname_wire [::collage_tb::_get_w ${inst_name}/$pname]
        incr idx
        if {$idx == 1} {
          append ret_val "      (.${pname}($pname_wire)"
        } else {
          append ret_val "       .${pname}($pname_wire)"
        }
        if {$idx == $num_ports} {
          # this is the last port
          append ret_val ");\n"
        } else {
          append ret_val ",\n"
        }
      }
    } else {
      append ret_val "      ();\n"
    }

    append ret_val "   // END: custom instantiation of SV interface\n\n"
    if {!$is_fabric && $instidx != -1} {
      if {$::collage_tb::use_generate} {
        if {$::collage_tb::skip_ti_inst && $::collage_tb::dont_generate} {
          append ret_val "  end\n"
        } else {
          append ret_val "  end\nendgenerate\n"
        }
        if {$curidx != -1} {
          if {$::collage_tb::skip_ti_inst && $::collage_tb::dont_generate} {
	    append ret_val "  if($ip_enable_name == 1) begin : ${inst_name}_${curidx}\n"
          } else {
	    append ret_val "generate\n  if($ip_enable_name == 1) begin : ${inst_name}_${curidx}\n"
          }
        } else {
          if {$::collage_tb::skip_ti_inst && $::collage_tb::dont_generate} {
            append ret_val "  if($ip_enable_name == 1) begin : ${inst_name}\n"
          } else {
            append ret_val "generate\n  if($ip_enable_name == 1) begin : ${inst_name}\n"
          }
        }
        lappend param_names $ip_enable_name
        
      } else {
        append ret_val "`endif\n"
        append ret_val "${::collage_tb::ifdef_var} ${ip_enable_name}\n"
      }
      
    }
    return $ret_val
  }

  #################################################################################
  # Write Test island
  # _collage_tb_verif_write_test_island
  #################################################################################
  proc _collage_tb_set_rtb_files {full_path} {
    if {![info exists ::collage_tb::collage_tb_misc_arr(reconfig_tb_files,TI)]} {
      set ::collage_tb::collage_tb_misc_arr(reconfig_tb_files,TI) $full_path
    } elseif {[lsearch $::collage_tb::collage_tb_misc_arr(reconfig_tb_files,TI) $full_path] == -1} {
      lappend ::collage_tb::collage_tb_misc_arr(reconfig_tb_files,TI) $full_path
    }
  }

  proc _collage_tb_verif_write_test_island {} {
    global collage_interface_lists
    global interface_inst_list
    global ti_inst_list
    global collage_tb_addons
    global collage_tb_verif_portlist
    global param_names


    ###############################################################################
    # WRITE test_island instance
    ###############################################################################
    puts $collage_tb_addons "\n"
    if {$::collage_tb::enable_emulation_ifdef} {
      puts $collage_tb_addons "\`ifndef EMULATION"
    }
    puts $collage_tb_addons "\n"
    if {!$::collage_tb::enable_split_tb_sv} {
      puts $collage_tb_addons "   `include \"${::collage_tb::pre_post_include_prefix}_pre_ti_include.sv\"\n"
    }

    if {($::collage_tb::enable_split_tb_sv && !$::collage_tb::enable_reconfig_tb) || $::collage_tb::multi_rtb_consumer} {
      if {[info exists ::collage_tb::collage_tb_misc_arr(reconfig_include_files)]} {
        set inc_files $::collage_tb::collage_tb_misc_arr(reconfig_include_files)
        foreach inc_file $inc_files {
          puts $collage_tb_addons "   `include \"$inc_file\""
        }
      } 
    }

    ## Adds no value for PSF2, thus removing
    if {$::collage_fabric::psf_version == 1} {
      puts $collage_tb_addons "   `include \"psf_only_tb_oob.sv\"\n\n"
    }

    if {!$::collage_tb::enable_split_tb_sv} {
      puts $collage_tb_addons "   // START: custom test island instantiation"
    }
    # ----------------------------------
    # Loop through and create test islands
    # ----------------------------------
    foreach ti_entries [lsort [::array names ::collage_tb::ti_intf_map *,dut_inst]] {
      foreach {ti_name tmp} [split $ti_entries ,] {break}
      set component_list [collage_tb_get_dut_insts_from_ti $ti_name]
      # string of concatenated ports
      set ports ""
      set ti_comp $ti_name
      set ti_comp_inst [::collage_tb::_collage_get_ti_inst_name $ti_name]
      set ti_comp_mod [::collage_tb::_collage_get_ti_module_name $ti_name]
      set ip_enable_name [::collage_tb::_get_ip_enable_name_by_ti_inst $ti_comp_inst]
      
      if {$::collage_tb::enable_split_tb_sv} {
        set f "${::collage_tb::collage_design}_${ti_comp_inst}.sv"
        if {[lsearch -exact $::collage_tb::exclude_rtb_ti_files $f] == -1} {
          puts $collage_tb_addons "   `include \"$f\"";
        } else {
          collage_message_print "TB-099" "COLLAGE RECONFIG_TB - Excluding file $f while reconfiguration."
        }
        if {$::collage_tb::aggr_ti_fp ne ""} {
          puts $::collage_tb::aggr_ti_fp "   `include \"$f\"";
        }
        set full_path "$::env(COLLAGE_WORK)/${::collage_tb::collage_design}/src/$f"
        _collage_tb_set_rtb_files $full_path
        set instantiation_file [open "$full_path" "a+"]
        if {!$::collage_tb::custom_tb} {  
          puts $instantiation_file "   // START: custom test island instantiation"
        }
      } else {
        set instantiation_file $collage_tb_addons
      }
            
      if {!$::collage_tb::enable_reconfig_tb} {
        _collage_tb_set_ip_enable_names [collage_tb_get_dut_insts_from_ti $ti_name] $ip_enable_name
      }
      set ti_comp_inst [collage_tb_reconfig_ti_inst $ti_comp_inst]

      if {$::collage_tb::custom_tb} {
        if {[info proc ::collage_tb_custom_pre_ti_inst] != ""} {
          set val [::collage_tb_custom_pre_ti_inst $ti_comp_inst]
          if {$::collage_tb::add_ti_adhoc_file} {
             puts $instantiation_file  "${val}\n"
          }
        } else {
          if {$::collage_tb::add_ti_adhoc_file} {
             puts $instantiation_file  "\`include \"${ti_comp_inst}_adhoc.sv\"\n"
          }
        }
        puts $instantiation_file "   // START: custom test island instantiation"
      } else {
        if {$::collage_tb::use_generate} {
          puts $instantiation_file  "generate\n if($ip_enable_name == 1) begin: $ti_comp_inst\n"
          #DHRUBA do this only if the ti has adhoc port and a special setting is applied
          #to keep bkward compatibility
          if {$::collage_tb::add_ti_adhoc_file} {
             puts $instantiation_file  "\`include \"${ti_comp_inst}_adhoc.sv\"\n"
          }
          lappend param_names $ip_enable_name
        } else {
          puts $instantiation_file  "${::collage_tb::ifdef_var} ${ip_enable_name}"
        }
      }

      # ----------------------------------
      # Write test island parameters
      # ----------------------------------
      set num ""

      set test_island_inst_str "   $ti_comp_mod "
      set test_island_param_str ""
      if {$::collage_tb::debug_mode} {
        print_info "COLLAGE_TB: TI_COMP:$ti_comp_mod TI_NAME:$ti_comp  TI_INST:$ti_comp_inst ti_str:$test_island_inst_str"
      }
      foreach key [lsort [array names ::collage_tb::ti_intf_map *,params,* ]] { 
        foreach {t params pname} [split $key ,] {break}
        if {$t == $ti_comp} {
          set pval [collage_tb_get_ti_param $t $pname]
          if {$::collage_tb::enable_reconfig_tb} {
            set new_pval "\{${t}${::collage_tb::general_ti_str}_${pname}\}\{${pval}\}"
            append test_island_param_str "      .$pname\($new_pval\),\n"
          } else {
            append test_island_param_str "      .$pname\($pval\),\n"
          }
        } 
      } 

      set test_island_param_str [string trimright $test_island_param_str ",\n"]
      if {$test_island_param_str != ""} {
        set test_island_param_str  " #( \n${test_island_param_str}" 
        append test_island_inst_str $test_island_param_str
        append test_island_inst_str "\n   )"
      }
      #instance name mapping
      #more than one of the same ti instantiated
      append test_island_inst_str " $ti_comp_inst ( "
      puts $instantiation_file $test_island_inst_str
      set idx 0
      if {[info exists ::collage_tb::ti_intf_map($ti_comp,interfacelist)]} { 
        foreach ti_sv_port_pair [lsort $::collage_tb::ti_intf_map($ti_comp,interfacelist)] {
          set ti_intf_def [lindex $ti_sv_port_pair 0]
          set sv_intf_inst [lindex $ti_sv_port_pair 1]
          #verify that the pin is for this test island, 
          #can have more than one test island/rtl component
          #or is pok interface
          # if unconnected pin, then write only ()
          if {[regexp "^<open>" $sv_intf_inst]} {
            set  sv_intf_inst ""
          }
          if {[collage_tb_get_sv_intf_generate $sv_intf_inst] != "" && $::collage_tb::use_generate} {
            set prestr "[collage_tb_get_sv_intf_generate $sv_intf_inst]." 
          } else {
            set prestr ""
          }
          set ti_comp_inst_t [::collage_tb::_collage_get_ti_inst_name $ti_comp]
          set prestr [collage_tb_reconfig_sv_intf_inst $prestr $ti_comp_inst_t]
          set sv_intf_inst [collage_tb_reconfig_sv_intf_inst $sv_intf_inst $ti_comp_inst_t]
          #how indexing on definitions would work
          if {[info exists ::collage_tb::sv_interface_attrs_map(aggr_exp_ports)]} {
            if {[lsearch -exact $::collage_tb::sv_interface_attrs_map(aggr_exp_ports) $sv_intf_inst] != -1} {
              set ti_port_conn $ti_intf_def
              if {[info exists ::collage_tb::ti_intf_map($sv_intf_inst,ti_port_ovr)]} {
                set ti_port_conn $::collage_tb::ti_intf_map($sv_intf_inst,ti_port_ovr)
              }
              append ports "      .$ti_intf_def\(${ti_port_conn}\),\n"
            } else {
              append ports "      .$ti_intf_def\(${prestr}$sv_intf_inst\),\n"
            }
          } else {
            append ports "      .$ti_intf_def\(${prestr}$sv_intf_inst\),\n"
          }
        }; # iterate through the pins of the test island
      }; # iterate through ti components	
      #strip off comma and put to print port instances
      set ports [string trimright $ports ",\n"]
      # write ports to file
      puts $instantiation_file $ports
      # end of test island instantiation
      puts $instantiation_file "   );\n"
      if {$::collage_tb::custom_tb} {
        puts $instantiation_file "   // END: custom test island instantiation"
        if {[info proc ::collage_tb_custom_ti] != ""} {
          set val [::collage_tb_custom_ti $ti_comp_inst $ip_enable_name]
          set print_val [lindex $val 1]
          puts $instantiation_file "${print_val}\n"
        } else {
          puts $instantiation_file "  end : ${ti_comp_inst}_BLOCK\n"
        }
        if {[info proc ::collage_tb_custom_outer_ti] != ""} {
          set val [::collage_tb_custom_outer_ti $ti_comp_inst]
          set print_val [lindex $val 1]
          puts $instantiation_file "${print_val}\n"
        } else {
          puts $instantiation_file "endgenerate\n"
        }
      } else {
        if {$::collage_tb::use_generate} {
          puts $instantiation_file "  end\nendgenerate\n"
        } else {
          puts $instantiation_file "`endif\n"
        }
      }
      if {$::collage_tb::enable_split_tb_sv} {
        if {!$::collage_tb::custom_tb} {
          puts $instantiation_file "   // END: custom test island instantiation"
        }
        close $instantiation_file
      }
    }
    
    if {!$::collage_tb::enable_split_tb_sv} {
      puts $collage_tb_addons "   // END: custom test island instantiation"
    }
    puts $collage_tb_addons ""
    puts $collage_tb_addons "   `include \"${::collage_tb::pre_post_include_prefix}_post_ti_include.sv\"\n"

  }
  
  proc ::collage_tb::_generate_num_intf_pins {} {
##    foreach elems [lsort [array names ::collage_interface_lists intf2intfdef,*,provider]] {
##      set sv_intf_type_t [lindex [split $elems ","] 1]
##      set rtl_intf_types $::collage_interface_lists(intf2intfdef,$sv_intf_type_t,provider)
##puts "elems = $elems and $sv_intf_type_t"
##      foreach rtl_intf_type_t [lsort -u $rtl_intf_types] {
##        set cntr 0
##        foreach key [lsort [array names ::collage_interface_lists $sv_intf_type_t,$rtl_intf_type_t,provider,interface,*]] {
##          set rtl_intf_type [lindex [split $key ","] 1]
##          incr cntr
##        }
##        set ::collage_interface_lists($rtl_intf_type_t,num_rtl_ports) $cntr
##      }
##    }
    foreach leaf [collage_get_leaf_ips] {
      set hier_name [collage_get_hier_name  -ip_name $leaf]
      foreach_in_collection intf [find_interface_instances -component $hier_name] {
        set intf_ports_cntr 0
        foreach_in_collection ac [get_attribute $intf -attr AllChildren] {
          if {[get_attribute $ac -attr TypeName] == "interfacePort"} {
            incr intf_ports_cntr
          }
        }
        set ::collage_tb::num_rtl_ports($leaf,[get_attribute $intf -attr Name]) $intf_ports_cntr
      }
    } 
  }

  proc ::collage_tb::_generate_fabric_EN_II {} {
    set file_name "$::env(COLLAGE_WORK)/collage_tb_fabric_EN_II.txt"
    set fh [open $file_name "w"]
    foreach sv_map_name  [lsort [array names ::collage_tb::sv_interface_map *,svintfinst,1]] {
      set sv_intf_inst [lindex [split $sv_map_name ,] 0]
      set key [lindex $::collage_tb::sv_interface_map($sv_map_name) 0]
      set dut_inst [lindex  $key 0]
      set dut_intf [lindex $key 1]
      set intf_type [lindex $key 2]
      set dut_dir [lindex $key 3]
      set dut_inst_hier [lindex $key 7]
      set key_value $::collage_tb::sv_interface_map($sv_map_name)
      set intf_component $::collage_tb::sv_interface_map($sv_intf_inst,svintfdef)
      if {[info exists ::collage_tb::sv_interface_map($sv_intf_inst,USER_SV_INST_TYPE)]} {
        set sv_intf_type $::collage_tb::sv_interface_map($sv_intf_inst,USER_SV_INST_TYPE)
        set rtl_intf_type [lindex $::collage_tb::sv_interface_map($sv_intf_inst,dutintfdefs) 0]
        set all_sv_intf_types $::collage_interface_lists(intfdef2intf,$rtl_intf_type,$dut_dir)
        if {[llength $all_sv_intf_types] > 1 && [regexp "$sv_intf_type" $all_sv_intf_types]} {
          set intf_component $sv_intf_type
        }
      }
      set is_fabric [::collage_tb::_isa_fabric_ip_by_dutintf $dut_inst $intf_component $sv_intf_inst $dut_inst_hier]
      set dut_inst_conn ""
      set dut_intf_conn ""
      set num_rtl_ports ""
      set key_cntr 0
      foreach key $key_value {
        sv_lassign $key dut_inst_t dut_intf_t intf_type_t dut_dir_t dut_inst_hier_conn_t intf_role_t di_exp_from_t
        if {$is_fabric && $dut_inst_hier_conn_t != "<undriven>"} {
          # select dut with max. # of interface ports
          if {$key_cntr == 0} {
            set dut_inst_conn [lindex [split $dut_inst_hier_conn_t "/"] end-1]
            set dut_intf_conn [lindex [split $dut_inst_hier_conn_t "/"] end]
            set num_rtl_ports $::collage_tb::num_rtl_ports($dut_inst_conn,$dut_intf_conn)
            incr key_cntr
          }
          if {$key_cntr > 0 && $::collage_tb::num_rtl_ports($dut_inst_conn,$dut_intf_conn) > $num_rtl_ports} {
            set dut_inst_conn [lindex [split $dut_inst_hier_conn_t "/"] end-1]
            set dut_intf_conn [lindex [split $dut_inst_hier_conn_t "/"] end]
            set num_rtl_ports $::collage_tb::num_rtl_ports($dut_inst_conn,$dut_intf_conn)
          } 
        } else {
          #puts "no connection exist for $sv_intf_inst"
        }
      }
      set ti_name_EN_II ""
      set enable_name_EN_II ""
      if {$dut_inst_conn != "" & [info exists ::collage_tb::ti_intf_map($dut_inst_conn,$dut_intf_conn,dut_ti_comp)]} {
        # get ti_name for rtl IP and interface
        set dut_ti_name $::collage_tb::ti_intf_map($dut_inst_conn,$dut_intf_conn,dut_ti_comp)
        #format of dut_ti_name to be passed  - _ti_1
        set dut_ti_name [::collage_tb::_collage_get_ti_inst_name $dut_ti_name]
        set enable_name_EN_II [_get_ip_enable_name_by_ti_inst $dut_ti_name]
        set enable_name_EN_II [regsub {_ENABLE$} $enable_name_EN_II {}]
      } 
      set ti_inst_name_EN_II $::collage_tb::sv_interface_map($sv_intf_inst,tiinst)
      set sv_intf_EN_II [regsub "${ti_inst_name_EN_II}_" $sv_intf_inst ""]
      if {$enable_name_EN_II != ""} {
         puts $fh "EN_II $ti_inst_name_EN_II $sv_intf_EN_II $enable_name_EN_II"
      }
    }
    close $fh
    _collage_tb_process_user_ii_ti $file_name "build"
  }

  proc _collage_tb_verif_write_emu_instances { } {
    array set emu_par {}
    foreach emu_val [lsort [array names ::collage_tb::sv_interface_map *,emu_svintfinst,1]] {
      set inst_name [lindex [split $emu_val ,] 0]
      set mod_name [collage_emu_get_emu_type $inst_name]
      set inst [collage_emu_get_emu_name $inst_name]
      set values $::collage_tb::sv_interface_map($emu_val)
      set par [lindex [lindex $values 0] 0]
      set print_lines [_collage_generate_emu_instantiation $mod_name $inst_name $inst $values]
      append emu_par(emu_${par}) $print_lines "\n"
    }
    if {![file isdirectory $::env(COLLAGE_WORK)/gen/emu_tb]} {
      file mkdir $::env(COLLAGE_WORK)/gen/emu_tb
    } else {
      file delete {*}[glob -nocomplain $::env(COLLAGE_WORK)/gen/emu_tb/*]
    }
    set emu_tb_dir $::env(COLLAGE_WORK)/gen/emu_tb
    foreach par [lsort [array names emu_par]] {
      set fp [open "$emu_tb_dir/${par}.sv" "w"]
      puts $fp $emu_par($par)
      close $fp
    }
  }

  proc _collage_generate_emu_instantiation {module_name inst_name inst rtl_conns} {

    global collage_interface_lists
    global interface_inst_list
    global param_names

    set ret_val ""
    array set inst_params {}
    array set inst_ports {}
    set inst_pins [list]
  
    foreach rtl_conn $rtl_conns {
      sv_lassign $rtl_conn par dut_inst ifc_name rtl_type dir 
        foreach pname_val [array names collage_interface_lists $module_name,$rtl_type,$dir,$inst,emu_parameter,* ] {
        set emu_pname [lindex [split $pname_val ,] 5]
        set pval_pair $collage_interface_lists($module_name,$rtl_type,$dir,$inst,emu_parameter,$emu_pname)
        set pval [lindex $pval_pair 1]
        set pname [lindex $pval_pair 0]
        if {$pname eq "open"} {set inst_params($emu_pname) $pval ; continue}
        set query_param [collage_emu_get_params $inst_name $emu_pname]
        if {$query_param ne ""} {
          set pval $query_param
        } else {
          collage_message_print "TB-053" "COLLAGE_EMU_TB: There is no interface parameter with name $pname. Please fix mapping file and rerun!!"
        }
        set inst_params($emu_pname) $pval
      }

      foreach pname_val [array names collage_interface_lists $module_name,$rtl_type,$dir,$inst,emu_interface,* ] {
        set emu_port [lindex [split $pname_val ,] 5]
        set pname [lindex $collage_interface_lists($module_name,$rtl_type,$dir,$inst,emu_interface,$emu_port) 0]
        set def_val [lindex $collage_interface_lists($module_name,$rtl_type,$dir,$inst,emu_interface,$emu_port) 1]
        if {$pname eq "open"} {
          set inst_ports($emu_port) "${def_val}"
          continue
        }
        set conn_port [collage_emu_get_conns $inst_name $pname]
        if {$conn_port eq "NA"} {
          collage_message_print "TB-054" "COLLAGE_EMU_TB: There is no interface port with name $pname. Please fix mapping file and rerun!!"
        }
        if {$conn_port eq "<open>" || $conn_port eq ""} {
          set inst_ports($emu_port) ""
          continue
        } else {
          set ip_inst_name $dut_inst
          set inst_ports($emu_port) "${ip_inst_name}.${conn_port}" 
        } 
      }
    }
      
    #append ret_val "\`ifdef USE_SLA_RTL_TLM \n parameter tlm_mon_${inst_name}_enable = iosfsbm_rtl_tlm_common_pkg::IOSF_SB_HW_MON_ENABLE\;\n generate\n  if (tlm_mon_${inst_name}_enable != 0) begin: ${inst_name}\n\n"
    set num_params [array size inst_params]
    set mon_enable_name [_collage_tb_emu_get_mon_param $module_name]
    if {$mon_enable_name eq ""} {
      collage_message_print "TB-055" "COLLAGE_EMU_TB: Emulation monitor parameter is missing for $module_name. Please provide EMP shorthand for it and rerun."
    }
    #set mon_enable_name "[string toupper $module_name]_ENABLE"
    if {$num_params > 0} {
      append ret_val "\`SLA_RTL_TLM_MONITOR_INST(${mon_enable_name},${module_name}\n"
      append ret_val "      #("
      set idx 0
      foreach pname [lsort [array names inst_params]] {
        set pval $inst_params($pname)
        if {[info exists ::collage_tb::sv_interface_attrs_map($inst_name,$pname,emu_user_param)]} {
          set pval $::collage_tb::sv_interface_attrs_map($inst_name,$pname,emu_user_param)
        } 
        incr idx
        if {$idx == 1} {
          # this is the first param
          append ret_val ".${pname}($pval)"
        } else {
          append ret_val "        .${pname}($pval)"
        }
  
        if {$idx == $num_params} {
          # this is the last param
          append ret_val "),${inst_name},\n"
        } else {
          append ret_val ",\n"
        }
      }
    } else {
      append ret_val "\`SLA_RTL_TLM_MONITOR_INST(${mon_enable_name},${module_name},${inst_name},\n"
    }
  
    # -----------
    # write ports
    # -----------
    set num_ports [array size inst_ports]
    if {$num_ports > 0} {
      set idx 0
      foreach pname [lsort [array names inst_ports]] {
        set ifc_port ""
        set pname_wire ""
        set pname_wire $inst_ports($pname)
        if {[info exists ::collage_tb::sv_interface_attrs_map($inst_name,$pname,emu_user_port)]} {
          set pname_wire $::collage_tb::sv_interface_attrs_map($inst_name,$pname,emu_user_port)
        }
        incr idx
        if {$idx == 1} {
          append ret_val "      (.${pname}($pname_wire)"
        } else {
          append ret_val "       .${pname}($pname_wire)"
        }
        if {$idx == $num_ports} {
          # this is the last port
          append ret_val "))\n"
        } else {
          append ret_val ",\n"
        }
      }
    } else {
      append ret_val "     ())\n"
    }
    #append ret_val "\n  end\n endgenerate\n\`endif"
    append ret_val "\n"

    return $ret_val
  }

  proc _collage_tb_verif_write_ifc_instances { } {
    global interface_inst_list
    global param_names
    # WRITE the interface instances
    # intf_component intf_inst dut_intf intf_type
    
    puts $::collage_tb_assigns "`ifndef THIS_TO_BE_STRIPPED_OUT\n`endif\n\n"
    
    array set ti_assigns {}
    foreach sv_map_name  [lsort [array names ::collage_tb::sv_interface_map *,svintfinst,1]] { 
      set sv_intf_inst [lindex [split $sv_map_name ,] 0]
      if {[info exists ::collage_tb::sv_interface_attrs_map(aggr_exp_ports)]} {
        if {[lsearch -exact $::collage_tb::sv_interface_attrs_map(aggr_exp_ports) $sv_intf_inst] != -1} {collage_message_print "TB-001" "COLLAGE_TB: AGGR TI - exported interface instances - $sv_intf_inst"; continue}
      }
      set key [lindex $::collage_tb::sv_interface_map($sv_map_name) 0] 
      set dut_inst [lindex  $key 0] 
      set dut_intf [lindex $key 1] 
      set intf_type [lindex $key 2]
      set dut_dir [lindex $key 3]
      set dut_inst_hier [lindex $key 7]
      set key_value $::collage_tb::sv_interface_map($sv_map_name)
      set intf_component $::collage_tb::sv_interface_map($sv_intf_inst,svintfdef)
      if {[info exists ::collage_tb::sv_interface_map($sv_intf_inst,USER_SV_INST_TYPE)]} {
        set sv_intf_type $::collage_tb::sv_interface_map($sv_intf_inst,USER_SV_INST_TYPE)
        set rtl_intf_type [lindex $::collage_tb::sv_interface_map($sv_intf_inst,dutintfdefs) 0]
        set all_sv_intf_types $::collage_interface_lists(intfdef2intf,$rtl_intf_type,$dut_dir)
        if {[llength $all_sv_intf_types] > 1 && [regexp "$sv_intf_type" $all_sv_intf_types]} {
          set intf_component $sv_intf_type
        }
      }
      set assigns [_collage_generate_intf_assigns $intf_component $sv_intf_inst $dut_inst $dut_intf $dut_dir $key_value $dut_inst_hier]
      
      if {$::collage_tb::enable_split_tb_sv} {
        set tiinst [collage_tb_get_svinst_to_tiinst $sv_intf_inst]
        set tiinst [_collage_get_ti_inst_name $tiinst]
        append ti_assigns($tiinst) $assigns "\n"
      } else {      
        puts $::collage_tb_assigns $assigns 
        puts $::collage_tb_assigns "\n"
      }    
    }  
    
    if {$::collage_tb::enable_split_tb_sv} {
      foreach tiinst [lsort [array names ti_assigns]] {
        set f "$::env(COLLAGE_WORK)/$::env(COLLAGE_DESIGN)/src/${::collage_tb::collage_design}_${tiinst}.sv"
        set fp [open $f "w"]
        if {$::collage_tb::skip_ti_inst} {
          _collage_tb_set_rtb_files $f
        }
        set ti_en_name [::collage_tb::_get_ip_enable_name_by_ti_inst $tiinst]
        if {$::collage_tb::custom_tb} {
          if {[info proc ::collage_tb_custom_outer_ti] != ""} {
            set val [::collage_tb_custom_outer_ti $tiinst]
            set print_val [lindex $val 0]
            set post_val [lindex $val 1]
            if {($print_val eq "" && $post_val ne "") || ($print_val ne "" && $post_val eq "")} {
              collage_message_print "TB-056" "COLLAGE CUSTOM TB - One of the Pre or Post line in collage_tb_custom_outer_ti is empty which is not allowed. Please fix and rerun."
            } 
            puts $fp "${print_val}\n"
          } else {
            puts $fp "generate\n"
          }
        }
        if {$::collage_tb::custom_tb} {
          if {[info proc ::collage_tb_custom_ti] != ""} {
            set val [::collage_tb_custom_ti $tiinst $ti_en_name]
            set print_val [lindex $val 0]
            set post_val [lindex $val 1]
            if {($print_val eq "" && $post_val ne "") || ($print_val ne "" && $post_val eq "")} {
              collage_message_print "TB-057" "COLLAGE CUSTOM TB - One of the Pre or Post line in collage_tb_custom_ti is empty which is not allowed. Please fix and rerun."
            } 
            puts $fp "${print_val}\n"
            set ::collage_tb::gen_param_file 0
          } else {
            puts $fp "  if(${ti_en_name}) begin : ${tiinst}_BLOCK\n"
            lappend param_names $ti_en_name
          }
        }
        puts $fp $ti_assigns($tiinst)
        close $fp
      }; # end foreach [lsort [array names ti_assigns]]      
    }
    
    if {$::collage_tb::enable_emulation_ifdef} {
      puts $::collage_tb_assigns "\`endif \/\/EMULATION \n"
    }
    #Add ENABLE names based on RTL connection
    #::collage_tb::_collage_tb_add_enable_name_conn
  }

  proc _collage_tb_verif_build_svcifc_instances {} {
    
    # Istantiate the interface instances
    # intf_component intf_inst dut_intf intf_type
    foreach sv_map_name [array names ::collage_tb::sv_interface_map *,svintfinst,*] { 
      set sv_intf_inst [lindex [split $sv_map_name ,] 0] 
      set intf_component $::collage_tb::sv_interface_map($sv_intf_inst,svintfdef)

      set key [lindex $::collage_tb::sv_interface_map($sv_map_name) 0]

      set intf_dir [lindex $key 3]
      _collage_tb_intf_create $intf_component $sv_intf_inst  $intf_dir 
    }
  }
  ###############################################################################
  # Testbench verif development file. 
  # _collage_tb_intf_create intf_comp intf_inst dut_intf intf_type
  # creates the interface instance and calls the intf_assign routine
  ###############################################################################
  proc _collage_tb_intf_create {intf_component intf_inst intf_type } {
    set_current_component -quiet [::col::get_ip_par $intf_inst]
    set intf_inst_no_idx $intf_inst
    
    if {[regexp {\[(\d+)\]$} $intf_inst tmp inst_idx]} {
      set intf_inst_no_idx [string trim $intf_inst  $tmp]
      if {[find_item -type cell -quiet  $intf_inst_no_idx] != ""} {
        return
      }
    }
      
    instantiate_component [list Intel Intel $intf_component 1.0] -name ${intf_inst_no_idx} -noauto
    
    if {[string equal $intf_component "iosf_sbc_intf"] && ![regexp {\d+_np$} $::collage_tb::sbr_version]} {
      #ADITI: CHECK WHAT SHOULD IT BE
      if {$intf_type == "consumer"} {
        set_configuration_parameter -component $intf_inst_no_idx AGENT_MASTERING_SB_IF 0
      } else {
        set_configuration_parameter -component $intf_inst_no_idx AGENT_MASTERING_SB_IF 1
      }
    }
    set_current_component -quiet
    
  }

  ###############################################################################
  # Testbench verif development file. 
  # _collage_tb_intf_parameter_configure
  # - global var interface_inst_list has f1,parameter,f2 [f1 = instance, f2 = pname and value is pvalue]
  # calls the configuration parameter command 
  ###############################################################################
  proc _collage_tb_intf_parameter_configure { } {
    global interface_inst_list
    
    foreach key [array names ::collage_tb::sv_interface_map *,params,*] { 
      foreach {inst temp  pname} [split $key ,]  {}
      set inst_no_idx $inst
      if {[regexp {\[(\d+)\]$} $inst tmp inst_idx]} {
        set inst_no_idx [string trim $inst $tmp]
      }
      set val [collage_tb_get_sv_intf_pval $inst $pname] 
      set sv_intf_inst [find_item -quiet -type cell $inst_no_idx]
      if {[sizeof_collection $sv_intf_inst] == 0} {
        echo "COLLAGE_TB: did not find SV interface inst: $inst"
      } else {
        set_configuration_parameter -component $inst_no_idx $pname $val
      }
    }
  }



  proc _open_tb_workspace { } {
    #################################################################################
    # Close the design workspace
    #################################################################################
    #gui_start; print_info "interactive debug"; return
    close_workspace
    catch {set_editing_mode -readwrite}


    #################################################################################
    # Create testbench workspace
    #################################################################################
    #ADITI: debug remove -replace
    create_workspace -name $::env(COLLAGE_DESIGN) -root $::env(COLLAGE_WORK) -replace
    #preload to 1.1, latest version
    if {![file exists $::env(COLLAGE_INTF_DEF)/rtl_interface_defs/iosf/${::collage_tb::prim_intf_def_file}]} {
      collage_message_print "TB-095" "File $::env(COLLAGE_INTF_DEF)/rtl_interface_defs/iosf/${::collage_tb::prim_intf_def_file} does not exist. File name provided to option -prim_intf_def_file of API collage_tb_init is incorrect. Please fix and rerun."
    }

    load_interface_definitions $::env(COLLAGE_INTF_DEF)/rtl_interface_defs/iosf/${::collage_tb::prim_intf_def_file}

    # --------
    # init
    # pull in all the corekits for the SV interfaces
    # --------
    _collage_tb_assemble_init $::collage_tb::collage_sv_ifc_kits $::collage_tb::collage_sv_ifc_kits_install_dir

    # --------
    # set design name and pull in top-level DUT for instantiation into TB
    # --------
    set filepaths ""
    if {$::collage_tb::corekit_mode} {
      set cur_search_path [get_activity_parameter AddSubsystemComponents SearchPath]
      set_activity_parameter AddSubsystemComponents SearchPath \
          "$::collage_tb::collage_sv_ifc_kits_install_dir ::collage_tb::collage_sv_ifc_kits $cur_search_path"
      instantiate_component $::collage_tb::dut_mod_name -name $::collage_tb::dut_inst_name -noauto      
    } elseif {![string equal "" $::collage_tb::partition_tb]} {
      set filepath ${::collage_tb::dut_ws}/components/${::collage_tb::partition_tb}/src/${::collage_tb::partition_tb}.$::collage_tb::dut_file_ext
      lappend filepaths $filepath
      if {[info exists ::collage_tb::dut_inc_dirs]} {
        import_component -name ${::collage_tb::partition_tb} -design ${::collage_tb::partition_tb} -language SystemVerilog -file $filepaths -include $::collage_tb::dut_inc_dirs
      } else {    
        import_component -name ${::collage_tb::partition_tb} -design ${::collage_tb::partition_tb} -language SystemVerilog -file $filepaths
      } 
    } else {    
      if {[info exists ::collage_tb::import_files]} {
        foreach file $::collage_tb::import_files {
          lappend filepaths $file
        }
      }
      lappend filepaths "${::collage_tb::dut_ws}/src/${::collage_tb::dut_mod_name}.$::collage_tb::dut_file_ext"
      if {[info exists ::collage_tb::dut_inc_dirs]} {
        collage_message_print "TB-002" "Importing : import_component -name ${::collage_tb::dut_inst_name} -design ${::collage_tb::dut_mod_name} -language SystemVerilog -file $filepaths -include $::collage_tb::dut_inc_dirs"
        import_component -name ${::collage_tb::dut_inst_name} -design ${::collage_tb::dut_mod_name} -language SystemVerilog -file $filepaths -include $::collage_tb::dut_inc_dirs
      } else {    
        collage_message_print "TB-002" "Importing : import_component -name ${::collage_tb::dut_inst_name} -design ${::collage_tb::dut_mod_name} -language SystemVerilog -file $filepaths"
        import_component -name ${::collage_tb::dut_inst_name} -design ${::collage_tb::dut_mod_name} -language SystemVerilog -file $filepaths
      } 
    }
  }

  ######################################################################
  # debug proc to dump collage_tb global variables and arrays
  ######################################################################
  proc _collage_tb_new_debug_globals {{reset 0}} {
    global collage_interface_lists
    echo ""
    echo "# -------------------------------------------"
    echo "# ARRAY: collage_interface_default_parameter"
    echo "# -------------------------------------------"
    if {[array exists ::collage_interface_default_parameter]} {
      parray ::collage_interface_default_parameter
    }

    echo ""
    echo "# -------------------------------------------"
    echo "# ARRAY: ::collage_tb::collage_tb_misc_arr"
    echo "# -------------------------------------------"
    if {[array exists ::collage_tb::collage_tb_misc_arr]} {
      parray ::collage_tb::collage_tb_misc_arr
    }
    echo ""
    echo "# -------------------------------------------"
    echo "# ARRAY: collage_interface_lists"
    echo "# -------------------------------------------"
    if {[array exists ::collage_interface_lists]} {
      parray collage_interface_lists
    }

    echo ""
    echo "# -------------------------------------------"
    echo "# ARRAY (unused?): collage_tb_verif_portlist"
    echo "# -------------------------------------------"


    echo ""
    echo "# -------------------------------------------"
    echo "# ARRAY: sv_interface_map"
    echo "# -------------------------------------------"
    if {[array exists ::collage_tb::sv_interface_map]} {
      parray ::collage_tb::sv_interface_map
    }

    echo ""
    echo "# -------------------------------------------"
    echo "# ARRAY: sv_interface_attrs_map"
    echo "# -------------------------------------------"
    if {[array exists  ::collage_tb::sv_interface_attrs_map]} {
      parray   ::collage_tb::sv_interface_attrs_map
    }

    echo ""
    echo "# -------------------------------------------"
    echo "# ARRAY: ::collage_tb::ti_intf_map"
    echo "# -------------------------------------------"
    if {[array exists ::collage_tb::ti_intf_map]} {
      parray ::collage_tb::ti_intf_map
    }
    echo "# -------------------------------------------"
    echo "# VARIABLE: w_map"
    echo "# -------------------------------------------"
    if {[array exists ::collage_tb::w_map]} {
      parray ::collage_tb::w_map
    }

    echo "# -------------------------------------------"
    echo "# VARIABLE: ::collage_tb::sb_param_info"
    echo "# -------------------------------------------"
    if {[array exists ::collage_tb::sb_param_info]} {
      parray ::collage_tb::sb_param_info
    }
    
    echo ""
    echo "# -------------------------------------------"
    echo "# VARIABLE: collage_tb_addons"
    echo "# -------------------------------------------"
    if {[info exists ::collage_tb_addons]} {
      puts $::collage_tb_addons
    }
  }


  proc collage_tb_generate_open_file {file_name} {
    set fptr [open $file_name "w"]
    puts $fptr [format "%-40s %-20s %-30s"  "#SV_intf_inst" "sv_intf_def" "pin"]    
    foreach key [lsort [array names ::collage_tb::sv_interface_attrs_map *,open,*]] {
      set isopen $::collage_tb::sv_interface_attrs_map($key)
      if {$isopen != 0} {
        foreach {inst temp  svpinname} [split $key ,]  {}
        set dutpinname [lindex $isopen 1]
        if {[_get_w ${inst}/$svpinname] != ""} {
          continue
        }
        if {[regexp chs_sideband_ti_ $inst] && $svpinname == "side_pok"} { continue }
        if {($dutpinname == "") || [collage_tb_get_sv_intf_conn $inst $dutpinname] == "<open>"} {
          puts $fptr [format "%-40s %-20s %-30s" $inst [collage_tb_get_sv_intfdef $inst] $svpinname]
        }
        }
    }
    close $fptr
  }
  


#  proc _collage_tb_add_ti_ip_map {ti_component ip_cellname old_proc} {}
#    foreach {is_instance_name inst_not_found dut_instance dut_instance_hier rtl_instance_real_hier} [::collage_tb::_collage_tb_check_rtl_component_type $ip_cellname $old_proc] break
  proc _collage_tb_add_ti_ip_map {ti_component ip_cellname {ip_intf ""}} {
    foreach {is_instance_name inst_not_found dut_instance dut_instance_hier rtl_instance_real_hier} [::collage_tb::_collage_tb_check_rtl_component_type $ip_cellname ] break
    if {$is_instance_name == "0" && $inst_not_found == "0" && $dut_instance == "0" && $dut_instance_hier == "0" && $rtl_instance_real_hier == "0"} {return "NA"}
    
    #found a valid TI instance mapping
    if {$inst_not_found == 1} {
      #No valid dut instance found for ip_cellname so returning"
      return ""
    }
    collage_tb_add_ti_dut_map $ti_component $dut_instance $ip_intf
    return "$dut_instance_hier $rtl_instance_real_hier"
  }
  
  proc _collage_get_ti_inst_name {ti_comp} {
    if {[regexp {\[(\d+)\]$} $ti_comp tmp idx]} {
      set base_name [string trim $ti_comp "\[$idx\]"]
      return "${base_name}_$idx"
    } else {
      return $ti_comp
    }  
  }

  proc _collage_get_ti_module_name {ti_comp} {
    if {[regexp {\[(\d+)\]$} $ti_comp tmp idx]} {
      return [string trim $ti_comp "\[$idx\]"]
    } else {
      set ret_val $ti_comp
      if {[info exists ::collage_tb::ti_intf_map($ti_comp,TI_module)]} {
        set ret_val $::collage_tb::ti_intf_map($ti_comp,TI_module) 
      }
      return $ret_val
    }  
  }

  proc _collage_tb_set_ti_module_name {ti_module ti_instance} {
    if {![info exists ::collage_tb::ti_intf_map($ti_instance,TI_module)]} {
      set ::collage_tb::ti_intf_map($ti_instance,TI_module) $ti_module
      if {$::collage_tb::enable_mi_corekit} {
        puts $::collage_tb::fmi_corekit "set ::collage_tb::ti_intf_map($ti_instance,TI_module) $ti_module"
      }
    } else {
      collage_message_print "TB-023" "COLLAGE_TB: Trying to set multiple module name - $ti_module to the same TI instance name - $ti_instance"
    }
  }

  proc _collage_tb_get_ti_intf_name {ti_comp sv_intf_name} {
    if {![info exists ::collage_tb::ti_intf_map($ti_comp,interfacelist)]} {
      return ""
    }
    set sv_intf_name [_collage_get_ti_module_name $sv_intf_name]
    foreach elem $::collage_tb::ti_intf_map($ti_comp,interfacelist) {
      set svname [lindex $elem 1]
      if {$svname == $sv_intf_name} {
        return [lindex $elem 0]
      }
    }
    return ""
  }
 
  
  proc _collage_tb_ti_interface_exists {ti_comp ti_pin_name} {
    if {![info exists ::collage_tb::ti_intf_map($ti_comp,interfacelist)]} {
      return 0
    }
    foreach elem $::collage_tb::ti_intf_map($ti_comp,interfacelist) {
      set pinname [lindex $elem 0]
      if {$pinname == $ti_pin_name} {
        return 1
      }
    }
    return 0
  }

  proc _collage_tb_find_hier_conn {intf_obj context} {

    set curintfhier [get_attribute -attr ComponentUserName $intf_obj ]
    set cxtintfhier [get_attribute -attr ComponentUserName $context]
    set intfname [get_attribute -attr Name $context]
    set cxtinsthier [join [lrange [split $cxtintfhier "/"] 0 [expr [llength [split $cxtintfhier "/"]] - 3]] "/"]
    set common_ancestor [_collage_get_common_ancestor_name "$curintfhier $cxtintfhier"]
                   
    set intf $context
    if { $common_ancestor != $cxtinsthier} {
      set len  [string length $common_ancestor]
      set ip_rltv_hier [string range $cxtinsthier $len end]
      set ip_abs_hier $cxtinsthier
      #pin: /a/b/c/p1, /a/c/d/p2 ip_tltv_hier /a/c /a
      while {$ip_rltv_hier != "" && $ip_rltv_hier != "/"} {
        set intftype [get_attribute $intf -attr InterfaceType ]
        if {$intftype == "consumer"} { 
          set reqintftype "Consumers"
        } elseif {$intftype == "consumer"} { 
          set reqintftype "Provider"
        } else {
          return $context
        }
        set tmpintf [get_attribute $intf -attr $reqintftype]
        if {$tmpintf == ""} {
          return $context
        }
        set intfname [get_attribute $tmpintf -attr Name]
        set intf [find_interface_instance -component $ip_abs_hier -name $intfname]
        if {$intf == ""} {return $context}
        set lastidx [expr [string last "/" $ip_rltv_hier] -1]
        set ip_rltv_hier [string range $ip_rltv_hier 0 $lastidx]
        set lastidx [expr [string last "/" $ip_abs_hier] -1]
        set ip_abs_hier [string range $ip_abs_hier 0 $lastidx]
      }
      return $intf
     }
    return $context
  }

  proc collage_tb_create_sv_interface_attrs {dut_instance dut_inst_no_hier ip_name sv_intf_inst_name rtl_instance_real_hier {precollinf2sv ""} {ti_intf_def ""} {dut_inst_mi ""} {emu_tb 0}} {
    global collage_interface_lists
    set contexts ""

    set dut_intf_obj [find_interface_instances -component $dut_instance -name $ip_name -filter {VisibleInterface!=0}]
    if {[sizeof_collection $dut_intf_obj] == 0} {
      if {$emu_tb} {
        collage_message_print "TB-058" "COLLAGE_EMU_TB: Did not find interface $ip_name in $dut_instance for ti mapping"
      } else {
        collage_message_print "TB-024" "COLLAGE_TB: Did not find interface $ip_name in $dut_instance for ti mapping"
      }
      return ""
    } 
    sv_lassign [get_attribute $dut_intf_obj -attrs "Name HierUserName InterfaceType InterfaceDefinitionName Unused IntfRole ExportedFrom"] di_name di_hier_user_name di_direction di_defname di_unused di_intf_role di_exp_from
    if {!$emu_tb} {
      collage_tb_add_sv_intf_dutintfdefs $sv_intf_inst_name $di_defname
    } 
    if {$emu_tb} {
      if {![info exists collage_interface_lists(emu_intfdef2intf,$di_defname,$di_direction)]} {
        collage_message_print "TB-059" "COLLAGE_EMU_TB: Did not find definition for $di_defname,$di_direction during $di_hier_user_name. Please fix mapping file and rerun"
      }
    }
    if {$ti_intf_def == ""} {
      if {$emu_tb} {
        set collintf2sv [lindex $collage_interface_lists(emu_intfdef2intf,$di_defname,$di_direction) 0]
      } else {
        if {![info exists collage_interface_lists(intfdef2intf,$di_defname,$di_direction)]} {
          collage_message_print "TB-025" "COLLAGE_TB: Did not find SV interface definition for $di_defname,$di_direction"
          return ""
        } 
        set collintf2sv [lindex $collage_interface_lists(intfdef2intf,$di_defname,$di_direction) 0]
      }
    } else {
      if {$emu_tb} {
        if {![info exists collage_interface_lists(emu_intf2intfdef,$ti_intf_def,$di_direction)]} {
          collage_message_print "TB-060" "COLLAGE_EMU_TB: Type provided $ti_intf_def does not match with the one provided in mapping file. Please fix type in ti_def file and rerun"
        }
      }
      set collintf2sv $ti_intf_def
    }
    set par ""
    if {$emu_tb} {
      set par [lindex [split $rtl_instance_real_hier "/"] end-1]
      #what if directly under soc - check below code later
      if {$par eq ""} {set par $::collage_tb::dut_inst_name}
      collage_emu_set_partitions $par
    }
    if {($precollinf2sv != "") && ($collintf2sv != $precollinf2sv) && !$emu_tb} {
      collage_message_print "TB-026" "COLLAGE_TB: mapping m collage interface to n sv interface is not supported"
      return ""
      }
    if {$di_direction == "consumer"} {set direction Provider; set direction_rev Consumers}
    if {$di_direction == "provider"} {set direction AllConsumers; set direction_rev Provider}
    if {[info exists direction]} {
      set connection [get_attribute  $dut_intf_obj  -attr TargetConnection -subscript ${direction}]
      if {[sizeof_collection $connection] > 1} {
        if {!$::collage_tb::allow_multiple_conns} {
          foreach_in_collection coll $connection {
            append ifcs "[get_attribute -attrs HierUserName $coll] "
          }
          collage_message_print "TB-061" "Multiple interfaces - $ifcs are connected to single interface $di_hier_user_name . If this is intended then please use option -allow_multiple_conns to the API collage_tb_use_custom_dut_writer"
        }
        set connection [index_collection $connection 0]
      }
      set contexts   $connection
      if {[sizeof_collection $contexts] == 0} {
        set contexts [get_attribute $dut_intf_obj -attr TargetConnection -subscript ${direction_rev}]
        #puts "got rev context: $contexts"
      }
      if {[sizeof_collection $connection] == 1 && $::collage_tb::aggr_ti_file ne ""} {
        set target_exp [get_attribute $connection -attr ExportedFrom]
        if {$target_exp ne ""} {
          collage_message_print "TB-003" "COLLAGE_TB: AGGR TI - exported rtl intf = $di_name and corresponding TI sv intf = $sv_intf_inst_name"
          collage_tb_set_exported_intf $sv_intf_inst_name
        }
      }
    } else {
      set connection ""
    }
    set intf_type "M"	
    if {$connection == ""} {
      if {$::collage_tb::enable_mi_corekit && $dut_inst_mi != ""} {
        set dut_inst_no_hier_mi [lindex [split $dut_inst_mi "/"] end]
        sv_lassign [::col::get_mi_intf_hier_name $dut_inst_mi $ip_name] exp_component exp_interface
        if {!$emu_tb} {
          puts $::collage_tb::fmi_corekit "set ::collage_tb::sv_interface_map($dut_inst_no_hier_mi,$ip_name,mi_expconn) \"$exp_component $exp_interface\""
          set ::collage_tb::sv_interface_map($dut_inst_no_hier_mi,$ip_name,mi_expconn) "$exp_component $exp_interface"
        }
      }
      set used_state "<undriven>"
      set intf_type "D"
    } else {
      set used_state [get_attribute -attrs HierUserName $connection]
    }
    
    if {$di_intf_role == ""} {
      set intf_role "on_ip"
    } else {
      set intf_role "on_hier"
    }
    ##Aditi: why is this??
    # Skip "top-level" interfaces on pars
    if {$di_exp_from != "" && !$emu_tb} {
      continue
    } 
    if {!$emu_tb} {
      if {$::collage_tb::use_hier_conn && [sizeof_collection $contexts] == 1} {
        set dut_intf_obj [_collage_tb_find_hier_conn $dut_intf_obj $contexts]
        set intf_hier_temp [get_attribute -attr ComponentUserName $dut_intf_obj]
        set intf_hier [string range $intf_hier_temp 1 [expr [string last "/" $intf_hier_temp] -1]]
        set intf_hier [regsub -all {/} $intf_hier {.}]
      } else {
        if {$::collage_tb::enable_mi_corekit} {
          set intf_hier [regsub -all {/} $rtl_instance_real_hier {.}]
        } else {
          set intf_hier [regsub -all {/} $dut_instance {.}]
          if {$rtl_instance_real_hier ne ""} {
            set intf_hier [regsub -all {/} $rtl_instance_real_hier {.}]
          }
        }
      }
    }  
    foreach_in_collection pin [get_attr -attr AllChildren $dut_intf_obj] {
      set sigtype [get_attr -attr "TypeName" $pin]
      if {$sigtype == "interfacePort"} {
        sv_lassign [get_attr -attr "InterfaceLink InterfaceSize Name" $pin] conn buswidth pindef
        if {[sizeof_collection $contexts] != 0} {
          set conn [get_interface_link $pin -context $contexts]
        }
        if {$emu_tb} {
          #store port info
          foreach pair [collage_emu_get_inst_names $sv_intf_inst_name $collintf2sv] {
            sv_lassign $pair if_inst_name inst
            collage_emu_set_conns $if_inst_name $pindef $conn
            collage_emu_set_intf_conn_props $if_inst_name $pindef $dut_inst_no_hier $ip_name $di_direction
          }
        } else {
          if {$conn != "<open>"} {
            set conn "${intf_hier}.$conn"
          } 
          collage_tb_add_sv_intf_conn $sv_intf_inst_name $pindef "${::collage_tb::dut_prefix}$conn"
          collage_tb_add_sv_intf_conn_props $sv_intf_inst_name $pindef $dut_inst_no_hier $ip_name $intf_type $di_direction $used_state $intf_role $di_exp_from
        }  
      }
    }
    if {$di_direction == "provider" && $connection != ""} {
      set intf_for_params $connection
    } else {
      set intf_for_params $dut_intf_obj
    }
    foreach_in_collection intf_param [get_interface_parameter $intf_for_params  -param -inst *] {
      # ---------------------------------------------------------------------
      # HACK to workaround mistake in collage interface defn in IOSF 1.0
      # ---------------------------------------------------------------------
      sv_lassign [get_attr -attr "Name Value" $intf_param] pname pval
      if {$pname == "AGENT_WIDTH" && $di_defname == "IOSF::Primary" && $::collage_fabric::psf_version == 1} {
        set pval [expr $pval + 1]
      }
      if {$::collage_tb::unused_param_check} {
        set used_attr [get_attribute $intf_param -attr UsedOnInstance]
        set used_val [eval_param $used_attr -context $intf_for_params]
        if {!$used_val} {continue}
      }
      if {$emu_tb} {
        #store param info
        foreach pair [collage_emu_get_inst_names $sv_intf_inst_name $collintf2sv] {
          sv_lassign $pair if_inst_name inst
          collage_emu_set_params $if_inst_name $collintf2sv $di_defname $di_direction $inst $pname $pval
        } 
      } else {
        collage_tb_add_sv_intf_params $collintf2sv $sv_intf_inst_name $pname $pval
      }
    }
    if {$emu_tb} {
      set restr "$par $di_defname $di_direction"
    } else { 
      set restr "$intf_type $di_direction $used_state $intf_role $di_exp_from"
    }
    return "$collintf2sv $restr"
  }

  proc collage_tb_add_intf_ti_dut_pinlist {ti_component dut_inst_opt interface_list {rtl_instance_real_hier ""} {dut_inst_mi ""}} {
    foreach {ti_name_str ip_name_pair_list} $interface_list {
      set collintf2sv_pre ""
      set collintf2sv ""
      set dut_inst_intf_pair_list [list]
      set sv_intf_inst_name ""
      set isopen 0
      if {[string first ":" $ti_name_str] > 0} {
        sv_lassign [split $ti_name_str ":"] ti_intf_def ti_name 
      } else {
        set ti_name $ti_name_str
        set ti_intf_def ""
      }
      set ti_inst_name [_collage_get_ti_inst_name $ti_component]
      set ti_comp [_collage_get_ti_component $ti_component]
      if {$ti_comp ne ""} {
        set ti_component $ti_comp
        set ti_inst_name $ti_comp
      }
      set ti_intf_name [_collage_get_ti_inst_name $ti_name]
      set sv_intf_inst_name ${ti_inst_name}_${ti_name}
      set isadhoc 0
      set dut_inst "NA"
      foreach ip_name_pair $ip_name_pair_list {
        if {[string first "adhoc_port:" $ip_name_pair] >= 0} {
          set ip_name [lindex [split $ip_name_pair :] 1]
          collage_tb_add_one_ti_pin $ti_component $ti_name $ip_name
          set isadhoc 1
          break
        } elseif {[regexp "<open>" $ip_name_pair]} {
          set ip_name "<open>"
          collage_tb_add_one_ti_pin $ti_component $ti_name "<open>"
        } elseif {![regexp {\.} $ip_name_pair]} {
          if {$dut_inst_opt == ""} {
            collage_message_print "TB-027" "COLLAGE_TB: need to have rtl instance with ip_name $ip_name_pair"
            return 0
          } else {
            set ip_name $ip_name_pair
            set dut_inst $dut_inst_opt
          }
        } else {
          if {$dut_inst_opt != ""} {
            collage_message_print "TB-028" "COLLAGE_TB: multiple mapping is not supported for collage_ti_definition"
            return 0
          }
          foreach {d_inst_name ip_name} [split $ip_name_pair "."] break
          if {$::collage_tb::enable_mi_corekit && [collage_is_mi_inst -name $d_inst_name]} {
            set dut_inst_mi $d_inst_name
          }
          #set dut_inst_t [::collage_tb::_collage_tb_add_ti_ip_map $ti_component "inst:$dut_inst" 0]
          set dut_inst_t [::collage_tb::_collage_tb_add_ti_ip_map $ti_component "inst:$d_inst_name" $ip_name]
          sv_lassign $dut_inst_t dut_inst rtl_instance_real_hier
        }
        if {$dut_inst == "NA"} {collage_message_print "TB-004" "COLLAGE_TB: Skipping for $ip_name_pair" ; continue}
        if {$dut_inst == ""} {
          if {$::collage_tb::skip_ti_inst} {
            collage_message_print "TB-096" "COLLAGE_TB: Did not find inst $d_inst_name for inst mapping for $ti_component"
            continue
          } else {
            collage_message_print "TB-063" "COLLAGE_TB: Did not find inst $d_inst_name for inst mapping for $ti_component"
          }
        }
        if {![string equal "" $::collage_tb::partition_tb]} {
          if {![regexp "(^|/)${::collage_tb::partition_tb}(/|$)" $dut_inst]} {
            collage_message_print "TB-064" "trying to connect TI ${ti_inst_name} to IP ${dut_inst} while in partition mode with $::collage_tb::partition_tb"
          }
        }
        set dut_inst_no_hier [lindex [split $dut_inst "/"] end]
        if {[regexp "<open>" $ip_name]} {
          set isopen 1
        } else {
          #set collintf2sv ""
          foreach {collintf2sv di_intf_type di_direction used_state intf_role di_exp_from} [::collage_tb::collage_tb_create_sv_interface_attrs $dut_inst $dut_inst_no_hier $ip_name $sv_intf_inst_name $rtl_instance_real_hier $collintf2sv_pre $ti_intf_def $dut_inst_mi] break
          if {$collintf2sv == ""} {
            set isopen 1
          }
          set collintf2sv_pre $collintf2sv
          if {$collintf2sv != ""} {
            if {$dut_inst_mi != "" && $::collage_tb::enable_mi_corekit} {
              set dut_inst_no_hier_mi [lindex [split $dut_inst_mi "/"] end]
              lappend dut_inst_intf_pair_list [list $dut_inst_no_hier $ip_name $di_intf_type $di_direction $used_state $intf_role $di_exp_from "orig:$dut_inst_no_hier_mi"]
            } else {
              lappend dut_inst_intf_pair_list [list $dut_inst_no_hier $ip_name $di_intf_type $di_direction $used_state $intf_role $di_exp_from $dut_inst]
            }
            collage_tb_add_sv_inst_rtlport_name $sv_intf_inst_name $collintf2sv $dut_inst $dut_inst_no_hier $ip_name
            collage_tb_add_sv_inst_to_dut_map $sv_intf_inst_name $dut_inst_no_hier $ip_name
          }
          collage_tb_add_inst_to_hier_map $dut_inst_no_hier $dut_inst $rtl_instance_real_hier
        }
      }
      if {$dut_inst == "NA" || $dut_inst == ""} {continue}
      if {!$isopen && $sv_intf_inst_name != "" && !$isadhoc} {
        #ADITI: even if one interface is missing its marked as open in ti interface
        collage_tb_add_sv_intf_inst_dut $sv_intf_inst_name $collintf2sv $dut_inst_intf_pair_list $ti_component
        collage_tb_add_one_ti_pin $ti_component $ti_name $sv_intf_inst_name
      } 
    }
  }


  # -----------------------------------------------------
  # set wire connection (SV Interface pin)
  # -----------------------------------------------------
  proc _set_w {wire inst_name pins comment} {
#    set pintofind ${inst_name}_${pins}
#    set pincol [find_item -quiet -type pin $pintofind]
#    if {[sizeof_collection $pincol] == 0} {
#      collage_message_print "TB-029" "COLLAGE_TB: did not find any pins matching $pintofind"
#      #error
#    }
#    foreach_in_collection p $pincol {
#      sv_lassign [get_attribute -attrs "RTLName" $p] rtl_name
#      set ::collage_tb::w_map($rtl_name) $wire
#    }
    set ::collage_tb::w_map(${inst_name}_${pins}) $wire
  }

  # -----------------------------------------------------
  # get wire connection
  # -----------------------------------------------------
  proc _get_w {pin} {
    if {[info exists ::collage_tb::w_map($pin)]} {
      return $::collage_tb::w_map($pin)
    }
    return ""
  }

  # -----------------------------------------------------
  # set wire connection (DUT pin)
  # -----------------------------------------------------
  proc _set_wdu {wire inst_name pin comment} {
    set ::collage_tb::wdu_map(${inst_name}/${pin}) $wire
  }

  # -----------------------------------------------------
  # get DUT pin's wire connection
  # -----------------------------------------------------
  proc _get_wdu {pin} {
    if {[info exists ::collage_tb::wdu_map($pin)]} {
      return $::collage_tb::wdu_map($pin)
    }
    return ""
  }

  proc _collage_tb_find_insts {prefix} {
    set hier_insts [_collage_find_design_insts $prefix]
    set ret_insts [list]
    foreach hier_inst $hier_insts {
      lappend ret_insts [collage_get_hier_to_flat_name -hier_name $hier_inst]
    }
    return $ret_insts
  }

  proc _write_sbr_active { fname } {
    if {![file isdirectory $::env(COLLAGE_WORK)/gen/tb_specs]} {
      file mkdir $::env(COLLAGE_WORK)/gen/tb_specs
    }
    set filename $::env(COLLAGE_WORK)/gen/tb_specs/$fname
    set fh [open $filename w]
    variable str_value

    set str_value "/*********************************************\n"
    append str_value "* This is a collage generated file\n"
    append str_value "*********************************************/\n\n"
    if {$::collage_tb::use_generate} {
      append str_value "generate\n"
    } else {
      append str_value "initial begin\n\n"
    }
    foreach sv_inst [array names ::collage_tb::sv_interface_map *,rtlportname] {
      set ifc_inst [lindex [split $sv_inst ','] 0]
      set rtlport [string toupper $::collage_tb::sv_interface_map($sv_inst)]
      set enname "${rtlport}$::collage_tb::enable_suffix"
      #override enable names
      set uspec_name [::collage_tb::_get_user_ip_enable_name_by_ifc_inst $ifc_inst]
      if {$uspec_name ne ""} {
        set rtlport [regsub "$::collage_tb::enable_suffix$" $uspec_name {}]
        set enname $uspec_name
      }
      if {[info exists ::collage_tb::fabric_uspec_map($enname)]} {
        set enname $::collage_tb::fabric_uspec_map($enname)
      }
      
      if {$::collage_tb::use_generate} {
        append str_value "  if ($enname == 1) begin\n"
        append str_value "    initial ovm_pkg::set_config_int (\"$::collage_tb::sbr_active_env\", \"SBNODE_$rtlport\",0);\n"
        append str_value "  end\n"
      } else {
        append str_value "${::collage_tb::ifdef_var} $enname\n"
        append str_value "  ovm_pkg::set_config_int (\"$::collage_tb::sbr_active_env\", \"SBNODE_$rtlport\",0);\n"
        append str_value "`endif\n"

      }
      
    }
    if {$::collage_tb::use_generate} {
      append str_value "endgenerate\n"
    } else {
      append str_value "end\n"
    }
    puts $fh $str_value
    close $fh

  }
}

proc collage_tb_set_aggr_exp_adhocs {aggr_exp_adhocs} {
  set ::collage_tb::sv_interface_attrs_map(aggr_exp_ports) $aggr_exp_adhocs
}

proc collage_tb_emu_f2f_inst {f2f_list} {
  foreach f2f_inst $f2f_list {
    lappend ::collage_tb::f2f_list $f2f_inst
  }
}

proc collage_tb_init { args } {
  parse_proc_arguments -args $args opts

  if {[info exists opts(-aggr_ti_file)]} {
    set ::collage_tb::aggr_ti_file $opts(-aggr_ti_file)
  }
  if {[info exists opts(-aggr_exp_adhocs)]} {
    collage_tb_set_aggr_exp_adhocs $opts(-aggr_exp_adhocs)
  }
  if {[info exists opts(-flip_dir)]} {
    set ::collage_tb::enable_suffix "_DISABLE"
    set ::collage_tb::ifdef_var "`ifndef"
  }
  if {[info exists opts(-assign_open)]} {
    set ::collage_tb::assign_open 1
  }
  set dont_add_ti_adhoc_file 0
  if {[info exists opts(-custom_tb)]} {
    set ::collage_tb::custom_tb 1
    set ::collage_tb::enable_split_tb_sv 1
    if {[info exists opts(-dont_add_ti_adhoc_file)]} {
      set dont_add_ti_adhoc_file 1
    }
    if {!$dont_add_ti_adhoc_file} {
      set ::collage_tb::add_ti_adhoc_file 1
    }
  }
  if {[info exists opts(-prim_rtl_intf_ver)]} {
    set ::collage_tb::prim_rtl_intf_ver $opts(-prim_rtl_intf_ver)
  }
  if {[info exists opts(-prim_intf_def_file)]} {
    set ::collage_tb::prim_intf_def_file $opts(-prim_intf_def_file)
  }
  if {[info exists opts(-skip_ti_inst)]} {
    set ::collage_tb::skip_ti_inst 1
  }
  if {[info exists opts(-dont_generate)]} {
    set ::collage_tb::dont_generate 1
  }
  if {[info exists opts(-disable_params)]} {
    set ::collage_tb::disable_params $opts(-disable_params)
  }
  if {[info exists opts(-exclude_rtb_ti_files)]} {
    set ::collage_tb::exclude_rtb_ti_files $opts(-exclude_rtb_ti_files)
  }
}

define_proc_attributes collage_tb_init \
    -info "Collage TB - Used for initialization" \
    -define_args {
      {"-assign_open"     "Handles TI pins assignments for non-existing DUT ports. Based on direction either connected to 0 or left unassigned."   "" boolean optional}
      {"-custom_tb"     "Generates Customizable TB with new structure"   "" boolean optional}
      {"-dont_add_ti_adhoc_file"     "To remove pre_ti include file generation"   "" boolean optional}
      {"-aggr_ti_file"     "File containing includes of lower TI files"   "" string optional}
      {"-aggr_exp_adhocs"  "SV Instance names for exported adhocs"   "" string optional}
      {"-flip_dir"  "Flip default direction to driver mode for sv interface assigns and TI instantiations"   "" boolean optional}
      {"-prim_rtl_intf_ver"  "IOSF Primary RTL Interface Version"   "" string optional}
      {"-prim_intf_def_file"  "IOSF Primary RTL Interface definition file name"   "" string optional}
      {"-skip_ti_inst"     "Skip generation of TI instantiation"   "" boolean optional}
      {"-dont_generate"     "Skip generation of generate block"   "" boolean optional}
      {"-disable_params"     "Generate parameter value as 0"   "" string optional}
      {"-exclude_rtb_ti_files" "Don't include given files as part of reconfiguration" "" string optional}
    }

proc collage_tb_reconfig_sv_intf_inst {sv_intf_inst ti_inst} {
  if {$::collage_tb::enable_reconfig_tb} {
    #ti_inst is already expanded
    if {[regexp "$ti_inst" $sv_intf_inst]} {
      set ret_val [regsub -all $ti_inst $sv_intf_inst "${ti_inst}${::collage_tb::general_ti_str}"]
      return $ret_val
    }
  }
  return $sv_intf_inst
}

proc collage_tb_reconfig_ti_inst {ti_comp_inst} {
  if {$::collage_tb::enable_reconfig_tb} {
    return "${ti_comp_inst}${::collage_tb::general_ti_str}"
  }
  return $ti_comp_inst
}

proc collage_tb_post_process_sbr_post_ti_include {} {
  set sbr_post_ti "$::env(COLLAGE_WORK)/sbr_post_ti_include.sv"
  set sbr_post_ti_out "$::env(REPO_ROOT)/verif/subsystems/iosf_sideband/includes/sbr_post_ti_include.sv"
  set fh [open $sbr_post_ti r]
  set fout [open $sbr_post_ti_out w]
  while {[gets $fh line] >= 0} {
    foreach fabric_spec [array names ::collage_tb::fabric_uspec_map] {
      if {[regexp "if \\($fabric_spec == 1\\) begin" $line]} {
        regsub "$fabric_spec" $line "$::collage_tb::fabric_uspec_map($fabric_spec)" line
      }
      if {[regexp "ifdef $fabric_spec" $line]} {
        regsub "$fabric_spec" $line "$::collage_tb::fabric_uspec_map($fabric_spec)" line
      }
    }
    puts $fout "$line"
  }
  close $fh
  close $fout
}

proc collage_tb_use_hier_intf { } {
  set ::collage_tb::use_hier_conn 1
}

proc collage_tb_stop_on_error { } {
  set ::collage_tb::stop_on_error 1
}

proc collage_tb_write_sbr_active { env } {
  set ::collage_tb::sbr_active 1
  set ::collage_tb::sbr_active_env $env
}

proc collage_tb_set_bfm_ver { bfm_ver } {
  set ::collage_tb::bfm_ver $bfm_ver
}

proc collage_tb_get_bfm_ver {} {
  return $::collage_tb::bfm_ver 
}

proc collage_tb_use_custom_dut_writer { args } {
  parse_proc_arguments -args $args opts

  set ::collage_tb::use_custom_dut_writer 1

  if {[info exists opts(-auto_wdu)]} {
    set ::collage_tb::enable_auto_wdu 1
  }
  if {[info exists opts(-add_auto_wdu_include)]} {
    set ::collage_tb::enable_auto_wdu_include 1
  }
  if {[info exists opts(-unused_param_check)]} {
    set ::collage_tb::unused_param_check 1
  }
  if {[info exists opts(-allow_multiple_conns)]} {
    set ::collage_tb::allow_multiple_conns 1
  }
  if {[info exists opts(-pre_post_include_prefix)]} {
    set ::collage_tb::pre_post_include_prefix $opts(-pre_post_include_prefix)
  }

}

define_proc_attributes collage_tb_use_custom_dut_writer \
    -info "Collage TB - Use custom DUT writer" \
    -define_args {
      {"-auto_wdu"     "Auto generate Wdu lines for DUT"   "" boolean optional}
      {"-add_auto_wdu_include"     "Include file containing Auto generated Wdu lines for DUT"   "" boolean optional}
      {"-unused_param_check"     "Don't get value of unused RTL interface param"   "" boolean optional}
      {"-allow_multiple_conns"     "Allow multiple RTL connections"   "" boolean optional}
      {"-pre_post_include_prefix"    "Prefix for pre-ti and post-ti include files. Default is soc" "" string optional}
    }

proc collage_tb_generate_emulation_ifdef {} {
  set ::collage_tb::enable_emulation_ifdef 1
}

proc collage_tb_assemble {args} {
  global param_names
  collage_message_print "TB-005" "COLLAGE_TB: new code version with ::collage_tb::tb_use_one_to_n $::collage_tb::tb_use_one_to_n"
  parse_proc_arguments -args $args ::collage_tb::tb_opts
  ::collage_tb::init_data_structure
  namespace eval :: {
    source $::collage_tb::tb_interface_defs_dir/tb_fabric_info.tcl
  }
  read_and_init_args $args
  if {[info proc soc_tb_pre_hook_init] != ""} {
    soc_tb_pre_hook_init
  }

  if {$::collage_tb::corekit_mode} {
    create_workspace -name $::env(COLLAGE_DESIGN) -root $::env(COLLAGE_WORK) -replace
    collage_install_ip_kit -ip_name $::collage_tb::dut_mod_name -kit_name $::collage_tb::dut_mod_name -src_dir $::collage_tb::collage_tb_corekit_path -dest_dir $::collage_tb::collage_sv_ifc_kits_install_dir
    set cur_search_path [get_activity_parameter AddSubsystemComponents SearchPath]
    set_activity_parameter AddSubsystemComponents SearchPath \
        "$::collage_tb::collage_sv_ifc_kits_install_dir ::collage_tb::collage_sv_ifc_kits $cur_search_path"
    instantiate_component $::collage_tb::dut_mod_name -name $::collage_tb::dut_inst_name -noauto    
  }
  
  if {!$::collage_tb::enable_mi_corekit} {
    if {$::collage_tb::fabric_iosf_pri_hier == ""} {
      set ::collage_tb::fabric_iosf_pri_hier [::collage_tb::_collage_tb_find_insts $::collage_tb::psf_mod_name_pfx]
    }

    _chassis_iosf_psf_write_oob_ifc $::collage_tb::fabric_iosf_pri_hier $::collage_tb::tb_oob_fn $::collage_fabric::psf_version

    if {$::collage_tb::fabric_iosf_sb_hier == ""} {
      set ::collage_tb::fabric_iosf_sb_hier [::collage_tb::_collage_tb_find_insts $::collage_tb::sbr_config_mod_name]
    }
    if {$::collage_tb::fabric_pnd_hier == ""} {
      set ::collage_tb::fabric_pnd_hier [::collage_tb::_collage_tb_find_insts "bt_top"]
    }
    if {$collage_tb::debug_mode} {
      print_info "COLLAGE_TB: ::collage_tb::fabric_pnd_hier after $::collage_tb::fabric_pnd_hier"
    }
  }

  # source MI corekit data structure
  if {$::collage_tb::mi_corekit_flow && !$::collage_tb::enable_mi_corekit} {
    set mi_corekit_files [fileutil::findByPattern $::collage_tb::dut_ws MI_corekit_data.tcl]
    if {[llength $mi_corekit_files] == 0} {
      collage_message_print "TB-065" "MI corekit flow - MI_corekit_data.tcl does not exist in the workspace $::collage_tb::dut_ws"
    }
    set mi_corekit_file [lindex $mi_corekit_files 0]
    collage_message_print "TB-006" "COLLAGE_TB: sourcing MI data - $mi_corekit_file"
    source $mi_corekit_file

    #handle undriven connections
    foreach conn [array names ::collage_tb::sv_interface_map *,svintfinst,1] {
      set values $::collage_tb::sv_interface_map($conn)
      set temp_values ""
      foreach val $values {
        set orig_ip_name ""
        if {[regexp {orig:} $val]} {
          set orig_ip_name [lindex $val end]
          regsub {orig:} $orig_ip_name {} orig_ip_name
          set val [lreplace $val end end]
        }
        if {[regexp {<undriven>} $val]} {
          set val_t [split $val " "]
          #set e_ip [string trim [lindex $val_t 0] "\{"]
          set e_ip $orig_ip_name
          set e_ifc [lindex $val_t 1]
          if {[info exists ::collage_tb::sv_interface_map($e_ip,$e_ifc,mi_expconn)]} {
            sv_lassign $::collage_tb::sv_interface_map($e_ip,$e_ifc,mi_expconn) exp_ip exp_intf
            set intf_obj [find_interface_instances -component [collage_get_hier_name -ip_name $exp_ip] -name $exp_intf -filter {VisibleInterface!=0}]
            if {[sizeof_collection $intf_obj] == 0} {
              collage_message_print "TB-030" "COLLAGE_TB: Did not find exported interface $exp_intf in $exp_ip"
              continue 
            }
            sv_lassign [get_attribute $intf_obj -attrs "Name HierUserName InterfaceType InterfaceDefinitionName Unused IntfRole ExportedFrom"] di_name di_hier_user_name di_direction di_defname di_unused di_intf_role di_exp_from
            if {$di_direction == "consumer"} {set direction Provider; set direction_rev Consumers}
            if {$di_direction == "provider"} {set direction AllConsumers; set direction_rev Provider}
            if {[info exists direction]} {
              set connection [get_attribute  $intf_obj  -attr TargetConnection -subscript ${direction}]
            } else {
              set connection ""
            }
            if {$connection != ""} {
              #don't substitute for exported interfaces - exp interface return username without ip_name
              if {[regexp {/} [get_attribute $connection -attr UserName]]} {
                regsub {<undriven>} $val [get_attribute $connection -attr UserName] val
              }
            }
          } else {
            #do nothing
          }
        }
        if {$temp_values eq ""} {
          set temp_values [list $val]
        } else {
          lappend temp_values $val
        }
      }
      if {$temp_values ne $values} {
        set ::collage_tb::sv_interface_map($conn) $temp_values
      }
    }
  }
  
  #Useful for collecting all partition names
  collage_set_partitions

  collage_read_subsystems -stage "ti_def"

  foreach ti_defs_fn $::collage_tb::ti_def_files {
    source $ti_defs_fn
  }

  if {$::collage_tb::enable_mi_corekit} {
    return
  }
  if {$::collage_tb::reduce_fabric_enable} {
    ::collage_tb::_generate_num_intf_pins
    ::collage_tb::_generate_fabric_EN_II
  }
  collage_read_subsystems -stage "ti_inst"
  
  foreach user_if_ti_file $::collage_tb::user_if_ti_fn {
    _collage_tb_process_user_ii_ti $user_if_ti_file "build"
  }  

  if {$::collage_tb::reduce_fabric_enable} {
    foreach sv_map_name  [lsort [array names ::collage_tb::sv_interface_map *,svintfinst,1]] { 
      set sv_intf_inst [lindex [split $sv_map_name ,] 0]
      set key [lindex $::collage_tb::sv_interface_map($sv_map_name) 0] 
      set dut_inst [lindex  $key 0] 
      set dut_inst_hier [lindex  $key 7] 
      set intf_component $::collage_tb::sv_interface_map($sv_intf_inst,svintfdef)
      if {[info exists ::collage_tb::sv_interface_map($sv_intf_inst,USER_SV_INST_TYPE)]} {
        set sv_intf_type $::collage_tb::sv_interface_map($sv_intf_inst,USER_SV_INST_TYPE)
        set rtl_intf_type [lindex $::collage_tb::sv_interface_map($sv_intf_inst,dutintfdefs) 0]
        set all_sv_intf_types $::collage_interface_lists(intfdef2intf,$rtl_intf_type,$dut_dir)
        if {[llength $all_sv_intf_types] > 1 && [regexp "$sv_intf_type" $all_sv_intf_types]} {
          set intf_component $sv_intf_type
        }
      }
      set is_fabric [::collage_tb::_isa_fabric_ip_by_dutintf $dut_inst $intf_component $sv_intf_inst $dut_inst_hier]
      set ip_enable_name [::collage_tb::_get_ip_enable_name  $sv_intf_inst $dut_inst $is_fabric 1]
    }

    set sbr_post_ti "$::env(REPO_ROOT)/verif/subsystems/iosf_sideband/includes/sbr_post_ti_include.sv"
    set sbr_tb_override "$::env(REPO_ROOT)/collage/subsystems/iosf_sideband/tb_overrides.txt"
    set sbr_ti_inst "chs_sideband_ti"
    #This procedure generates the clkreq/ack connectivity
    collage_tb_generate_sbr_collaterals $::design_inst $sbr_post_ti $sbr_tb_override $sbr_ti_inst $::sbr_names
  }
  
  ::collage_tb::_collage_tb_new_debug_globals > tb_globs.find_intf_insts
  #temporary storage for assigns in tb, do not change
  set tb_addons_file /tmp/$::env(COLLAGE_DESIGN)_tb_addons.[pid].sv
  set ::collage_tb_addons [open $tb_addons_file "w"]

  set tb_assigns_file /tmp/$::env(COLLAGE_DESIGN)_tb_assigns.[pid].sv
  set ::collage_tb_assigns [open $tb_assigns_file "w"]
  if {$::collage_tb::aggr_ti_file ne ""} {
    set ::collage_tb::aggr_ti_fp [open $::collage_tb::aggr_ti_file "w"]
  }
  
  collage_read_subsystems -stage "tb_conn"
  foreach tb_conn_fn $::collage_tb::tb_conn_files {
    collage_tb_process_conn_file -file $tb_conn_fn
  }
  if {[info proc soc_tb_b4_open_tb_ws] != ""} {
    soc_tb_b4_open_tb_ws
  }
  
  ::collage_tb::_open_tb_workspace
  collage_message_print "TB-007" "COLLAGE_TB: creating TI and sv interface instances"
  ::collage_tb::_collage_tb_verif_build_svcifc_instances
  # Give a callback to add "pins" to a test island instantiation
  if {[info proc soc_tb_pre_hook_configure_subsystem] != ""} {
    soc_tb_pre_hook_configure_subsystem
  }
  ::collage_tb::_collage_tb_intf_parameter_configure
  
  if {!$::collage_tb::tb_use_one_to_n} {
    #set sb_fabric_pok_ifcs [_collage_tb_sb_fabric_instantiate_poks_new $::collage_tb::tb_poks_fn "*sbr*port*" 1 $::collage_tb::dut_inst_name]
    set sb_fabric_pok_ifcs [_collage_tb_sb_fabric_instantiate_poks_new $::collage_tb::tb_poks_fn "chs_sideband_ti*if" 1 $::collage_tb::dut_inst_name]
  }

  # Give a callback hack to add "pins" to a test island instantiation
  if {[info proc soc_tb_hook_specify_subsystem] != ""} {
    soc_tb_hook_specify_subsystem
  }
  #3 steps to coretools
  #specify subsystem, complete connection, generate rtl
  ###############################################################################
  # Complete adding and configuring components
  # Configure SV interfaces, mapping parameters
  ###############################################################################

  if {$::collage_tb::corekit_mode} { collage_process_open_interfaces -verbose }

  autocomplete_activity SpecifySubsystem
  print_time "Collage Build - SpecifySubsystem End"

  #################################################################################
  # Reports and complete Open connections
  #################################################################################
  #collage_read_subsystems -stage "tb_conn"
  #foreach tb_conn_fn $::collage_tb::tb_conn_files {
  #  collage_tb_process_conn_file -file $tb_conn_fn
  #}
  
  if {$::collage_tb::enable_auto_wdu} {  
    _collage_tb_autogen_wdu ${::collage_tb::dut_inst_name}
  }; # end if {$::collage_tb::enable_auto_wdu}
   
  ::collage_tb::_collage_tb_new_debug_globals > tb_globs.after_tb_conn

    
  if {[info proc soc_tb_hook_complete_connections] != ""} {
    soc_tb_hook_complete_connections $::collage_tb::dut_inst_name
  } else {
    foreach_in_collection p [find_item -quiet -type pin ${::collage_tb::dut_inst_name}/* -filter "PortDirection==in"] {
      set name [get_attribute $p -attrs UserName]
      set pname [lindex [split $name "/"] end]
      
      export_pin -port $pname -auto_dim $name
      #create_connection -constant open [get_attribute -attrs Name $p]
    }
  }
  collage_process_open_connections -in_comment {} -out_comment {}
  autocomplete_activity CompleteConnections

  if {![string equal "" $::collage_tb::partition_tb]} {
    foreach conn [array names ::collage_tb::sv_interface_attrs_map *,conns,*] {
      set val $::collage_tb::sv_interface_attrs_map($conn)
      regsub [format {\S*\.%s\.} $::collage_tb::partition_tb] $val "$::collage_tb::partition_tb." ::collage_tb::sv_interface_attrs_map($conn)
    }
    foreach conn [array names ::collage_tb::w_map *] {
      set val $::collage_tb::w_map($conn)
      regsub [format {\S*\.%s\.} $::collage_tb::partition_tb] $val "$::collage_tb::partition_tb." ::collage_tb::w_map($conn)
    }
    foreach conn [array names ::collage_tb::sv_interface_attrs_map {*,[MD],*}] {
      set val $::collage_tb::sv_interface_attrs_map($conn)
      regsub [format {\S*\.%s\.} $::collage_tb::partition_tb] $val "$::collage_tb::partition_tb." ::collage_tb::sv_interface_attrs_map($conn)
   }
  }
  
  # -----------------------------------------------------------------------------
  # Custom Netlisting: Instantiate test islands (in addons file)
  # Custom Netlisting: Generate assigns and set attribute on test island instance
  # -----------------------------------------------------------------------------
  if {[info exists ::collage_tb_user_force_list]} {
    collage_user_create_force_intf_list $::collage_tb_user_force_list
  }
  collage_tb_verif_generate_rtl -output_dir "unused_option"

  #To support reduction in fabric enable names
#  if {$::collage_tb::reduce_fabric_enable} {
#    collage_tb_post_process_sbr_post_ti_include
#  }

  if {$::collage_tb::sbr_active} {
    set fname "${::collage_tb::collage_design}_sbr_active.sv"
    puts $::collage_tb_addons "   `include \"$fname\"\n"
    ::collage_tb::_write_sbr_active $fname
  }

  close $::collage_tb_addons
  close $::collage_tb_assigns
  if {$::collage_tb::aggr_ti_file ne ""} {
    close $::collage_tb::aggr_ti_fp 
  }

  #set_design_attribute {CustomizedTestbenchCode[Verilog_Additional_Wire]}
  set_design_attribute {CustomizedTestbenchCode[Verilog_Additional_Instance]} \
      "=exec cat  $tb_addons_file"

  if {!$::collage_tb::tb_use_one_to_n} {
    set sb_fabric_poks [_collage_tb_sb_fabric_instantiate_poks_new $::collage_tb::tb_poks_fn "chs_sideband_ti_*if" 0 $::collage_tb::dut_inst_name]

    # want to avoid soc.soc.pok1
    if {$::collage_tb::corekit_mode} {
      regsub -all "$::collage_tb::dut_inst_name.$::collage_tb::dut_inst_name" $sb_fabric_poks $::collage_tb::dut_inst_name sb_fabric_poks
    }

    set_design_attribute {CustomizedTestbenchCode[Verilog_Additional_Instance]} \
        "${sb_fabric_poks}\n[get_design_attribute {CustomizedTestbenchCode[Verilog_Additional_Instance]}]\n"
  }

  set topcell $::collage_tb::dut_inst_name
  if {![string equal "" $::collage_tb::partition_tb]} {
    set topcell $::collage_tb::partition_tb
  }
  #append interface assignments to tb file
  set_cell_attribute [list $topcell] {CustomizedTestbenchCode[Verilog_Additional_Pre_Instance]} "=exec cat $tb_assigns_file"
  # If using custom DUT writer, then remove the dut sv written by assembler
  if {$::collage_tb::use_custom_dut_writer} {
    set cur_cell_attrib [get_cell_attribute [list $topcell] {CustomizedTestbenchCode[Verilog_Additional_Pre_Instance]}]
    set_cell_attribute  [list $topcell] {CustomizedTestbenchCode[Verilog_Additional_Pre_Instance]} "${cur_cell_attrib}\n`ifndef THIS_TO_BE_STRIPPED_OUT"
    set_cell_attribute  [list $topcell] {CustomizedTestbenchCode[Verilog_Additional_Post_Instance]} "`endif"
  }
  if {$::collage_tb::sbr_active} {
    #Importing ovm_pkg is no longer needed in soc_tb
    #set_design_attribute {CustomizedTestbenchCode[Verilog_Additional_Wire]} \
        "[get_design_attribute {CustomizedTestbenchCode[Verilog_Additional_Wire]}]\n   import ovm_pkg::*;\n"
  }
  if {[info proc soc_tb_pre_hook_generate_subsystem_rtl] != ""} {
    soc_tb_pre_hook_generate_subsystem_rtl
  }
  if {$::collage_tb::enable_emulation_ifdef} {
    set_design_attribute {CustomizedTestbenchCode[Verilog_Additional_Instance]} \
      "[get_design_attribute {CustomizedTestbenchCode[Verilog_Additional_Instance]}]\n\n\`endif \/\/EMULATION \n"
    set_design_attribute {CustomizedTestbenchCode[Verilog_Additional_Wire]} \
      "[get_design_attribute {CustomizedTestbenchCode[Verilog_Additional_Wire]}]\n\n\`ifndef EMULATION\n"
  }
  ::collage_tb::_collage_tb_new_debug_globals > tb_globs.last
  if {$::collage_tb::use_generate} {
    if {![file isdirectory $::env(COLLAGE_WORK)/gen/tb_specs]} {
      file mkdir $::env(COLLAGE_WORK)/gen/tb_specs
    }
    set file_name $::env(COLLAGE_WORK)/gen/tb_specs/${::collage_tb::collage_design}_params.sv
    if {$::collage_tb::gen_param_file} {
      collage_tb_generate_params_file $file_name $param_names
    }
  }

  #generate open report
  set file_name $::env(COLLAGE_WORK)/gen/tb_specs/${::collage_tb::collage_design}_opens.txt
  #::collage_tb::collage_tb_generate_open_file $file_name
  collage_tb_report_open_connections -file $file_name




  #################################################################################
  # Set RTL Generation options
  #################################################################################
  set_activity_parameter GenerateSubsystemRTL UseUniqueLogicNets 0
  set_activity_parameter GenerateSubsystemRTL LanguageChoice SystemVerilog
  set_activity_parameter GenerateSubsystemRTL UserComment "Testbench Generated using Collage"
  set_activity_parameter GenerateSubsystemRTL DontWriteHeader 1
  set_activity_parameter GenerateSubsystemRTL AllowReadingOfOutputPorts 1
  set_activity_parameter GenerateSubsystemRTL IncludeSVIncludeFiles 0
  #################################################################################
  # Generate RTL
  # - generated RTL will be in workspace under $COLLAGE_DESIGN/src and $COLLAGE_DESIGN/components
  #################################################################################
  set_activity_parameter GenerateSubsystemRTL UseUniqueLogicNets 1
  autocomplete_activity GenerateSubsystemRTL

  # remove temporary files: collage_tb_addons
  if {[file writable $tb_addons_file]} {
    file delete $tb_addons_file
    file delete $tb_assigns_file
  }
  ###ADITI: Calling from old code
  _collage_tb_process_rtl $topcell
  
  if {[info proc soc_tb_post_hook_generate_subsystem_rtl] != ""} {
    soc_tb_post_hook_generate_subsystem_rtl
  }
  close_workspace 
}

define_proc_attributes collage_tb_assemble \
    -info "Collage - process SOC Testbench design   file" \
    -define_args {
      {"-design_name"      "Design name"   "" string required}
      {"-dut_inst_name"    "Instance name of DUT"   "" string required}
      {"-dut_mod_name"     "Module name of DUT"   "" string optional}
      {"-dut_inc_dirs"     "Directory to search in for verilog include files"   "" string optional}
      {"-import_files"     "Files to be imported along with top level dut"   "" string optional}
      {"-dut_file_ext"     "File extension of DUT RTL (sv or v)"   "" string optional}
      {"-dut_ws"           "DUT Workspace"   "" string required}
      {"-ti_def_files"     "Test Island Definition Files (space separated file list)"   "" string optional}
      {"-user_ti_if_file"  "User Test Island and SV Interface Instantiation Directives"   "" string optional}
      {"-user_tb_conn_files" "User Connectivity Spec for ad-hoc connections in testbench (space separated file list)"   "" string optional}
      {"-iosf_sb_poks_file"  "POK interface specs for sideband (generated during RTL assembly)"   "" string optional}
      {"-iosf_psf_oob_file"  "OOB interface specs for PSF (generated during RTL assembly)"   "" string optional}
      {"-kits_dir" "Directory of coreKits to install for use in testbench (global kits such as iosf_sbc_intf)"   "" string optional}
      {"-kits_install_dir"  "Directory to install global kits into"   "" string optional}
      {"-fabric_ti_names" "Names of Test Islands for fabric IPs such as SBR & PSF"   "" string optional}
      {"-multi_ti_names"  "Deprecated"   "" string optional}
      {"-corekit_mode"    "Corekit flow - path to a corekit dir"  "" string optional}
      {"-partition_tb"    "only generate TB for a partition" "" string optional}
      {"-reconfig_tb"    "Generates reconfigurable Testbench" "" boolean optional}
    }


proc read_and_init_args {args} {
  # Depending on the psf_version supported, source the appropriate
  # interface definition mapping files
  
  # Add the known modules to elab_name use 
  # TODO: Move this into central chassis initialization code
  append ::_collage_internal::hardcode_mods_to_skip_unelab_name " psf2_orgate "

  if {$::collage_tb::emu_map} {
    collage_tb_load_map_file EMU_SBR $::collage_tb::emu_sbr_ver
    collage_tb_load_map_file EMU_PSF $::collage_tb::emu_psf_ver
  }
  
  if {!$::collage_tb::disable_auto_install} {
    collage_tb_load_map_file PSF $::collage_fabric::psf_version 
    collage_tb_load_map_file SBR $::collage_tb::sbr_version
    collage_tb_load_map_file IDI $::collage_tb::idi_ver
    collage_tb_load_map_file  PMI $::collage_tb::pmi_ver 
    collage_tb_load_map_file PowerGating ""
  }
#   if {$::collage_fabric::psf_version == 1} {
#     namespace eval :: {
#       source $::tb_interface_defs_dir/tb_iosf_primary_agent.tcl
#       source $::tb_interface_defs_dir/tb_iosf_primary_fabric.tcl
#     }
#   } elseif {$::collage_fabric::psf_version == 2} {
#     namespace eval :: {
#       source $::tb_interface_defs_dir/tb_iosf_primary_intf.tcl  
#     }
#   } else {
#     error "the value of collage_fabric::psf_version must be 1 or 2"
#   }
#   if {$::collage_tb::tb_use_one_to_n} {
#     namespace eval :: {
#       source $::tb_interface_defs_dir/tb_iosf_sb_pok.tcl  
#     }
#   }
#   # IDI (PND2)
#   namespace eval :: {
#     source $::tb_interface_defs_dir/tb_caidi_if.tcl
#   }


#   # PMI 2.5
#   if {$::collage_tb::pmi_ver == "2.5"} {
#     namespace eval :: {
#       source $::tb_interface_defs_dir/tb_pmi_if.tcl
#     }
#   } else {
#     namespace eval :: {
#       source $::tb_interface_defs_dir/tb_req_if.tcl
#     }
#   }

#   # PGCB
#   namespace eval :: {
#     source $::tb_interface_defs_dir/tb_power_gating_if.tcl 
#   }

  #parse_proc_arguments -args $args opts
  set ::collage_tb::collage_design $::collage_tb::tb_opts(-design_name)
  set ::collage_tb::dut_inst_name  $::collage_tb::tb_opts(-dut_inst_name)
  if {[info exists ::collage_tb::tb_opts(-dut_mod_name)]} {
    set ::collage_tb::dut_mod_name  $::collage_tb::tb_opts(-dut_mod_name)
  } else {
    set ::collage_tb::dut_mod_name  $::collage_tb::dut_inst_name
  }
  if {[info exists ::collage_tb::tb_opts(-dut_inc_dirs)]} {
    set ::collage_tb::dut_inc_dirs  $::collage_tb::tb_opts(-dut_inc_dirs)
  }
  if {[info exists ::collage_tb::tb_opts(-import_files)]} {
    set ::collage_tb::import_files  $::collage_tb::tb_opts(-import_files)
  }
  if {[info exists ::collage_tb::tb_opts(-dut_file_ext)]} {
    set ::collage_tb::dut_file_ext  $opts(-dut_file_ext)
  } else {
    set ::collage_tb::dut_file_ext "sv"
  }

  set ::collage_tb::dut_ws $::collage_tb::tb_opts(-dut_ws)
  set ws_lock "$::collage_tb::dut_ws/.lock"
  if {[file exists $ws_lock]} {
    file delete -force $ws_lock
  }
  set ::collage_tb::ti_def_files ""
  if {[info exists ::collage_tb::tb_opts(-ti_def_files)]} {
    set ::collage_tb::ti_def_files $::collage_tb::tb_opts(-ti_def_files)
  }
  if {[info exists ::collage_tb::tb_opts(-user_ti_if_file)]} {
    set ::collage_tb::user_if_ti_fn $::collage_tb::tb_opts(-user_ti_if_file)
  } else {
    set ::collage_tb::user_if_ti_fn ""
  }
  set ::collage_tb::tb_conn_files ""
  if {[info exists ::collage_tb::tb_opts(-user_tb_conn_files)]} {
    set ::collage_tb::tb_conn_files $::collage_tb::tb_opts(-user_tb_conn_files)
  }
  set ::collage_tb::tb_poks_fn $::collage_tb::tb_opts(-iosf_sb_poks_file)
  set ::collage_tb::tb_oob_fn ""
  if {[info exists ::collage_tb::tb_opts(-iosf_psf_oob_file)]} {
    set ::collage_tb::tb_oob_fn $::collage_tb::tb_opts(-iosf_psf_oob_file)
  }

  if {[info exists ::collage_tb::tb_opts(-fabric_ti_names)]} {
    set ::collage_fabric::test_island_names $::collage_tb::tb_opts(-fabric_ti_names)
  }

  set ::collage_tb::collage_sv_ifc_kits "$::env(COLLAGE_INTF_DEF)/tb_interface/sv_interface_packaging/design/ip_kits"
  if {[info exists ::collage_tb::tb_opts(-kits_dir)]} {
    set ::collage_tb::collage_sv_ifc_kits $::collage_tb::tb_opts(-kits_dir)
  }
  set ::collage_tb::collage_sv_ifc_kits_install_dir ""
  if {[info exists ::collage_tb::tb_opts(-kits_install_dir)]} {
    set ::collage_tb::collage_sv_ifc_kits_install_dir $::collage_tb::tb_opts(-kits_install_dir)
  }

  set ::collage_tb::corekit_mode 0
  if {[info exists ::collage_tb::tb_opts(-corekit_mode)]} {
    set ::collage_tb::corekit_mode 1
    set ::collage_tb::collage_tb_corekit_path $::collage_tb::tb_opts(-corekit_mode)
  }

  set ::collage_tb::partition_tb ""
  if {[info exists ::collage_tb::tb_opts(-partition_tb)]} {
    set ::collage_tb::partition_tb $::collage_tb::tb_opts(-partition_tb)
  }

  if {[info exists ::collage_tb::tb_opts(-reconfig_tb)]} {
    set ::collage_tb::enable_split_tb_sv 1
    set ::collage_tb::enable_hier_alias 1
    set ::collage_tb::enable_reconfig_tb 1
  }
  
  if {!$::collage_tb::enable_mi_corekit} {
    set ::env(COLLAGE_DESIGN) $::collage_tb::collage_design
  }
  
  
  if {$::collage_tb::corekit_mode} {
    set ::collage_tb::dut_prefix ""
  } else {
    if {$::collage_tb::enable_hier_alias} {
      set ::collage_tb::dut_prefix "`[string toupper ${::collage_tb::dut_inst_name}]${::collage_tb::general_ti_str}."
    } else {
      set ::collage_tb::dut_prefix "${::collage_tb::dut_inst_name}."
    }
  }

  set ::collage_tb::psf_mod_name_pfx ""

  #look for suffix name of modules for psf to understand the module is a fabric
  if {$::collage_fabric::psf_version == 1} {
    set ::collage_tb::psf_mod_name_pfx "psf_wrapper"
    if {$::collage_tb::tb_oob_fn == ""} { 
      if {!$::collage_tb::enable_mi_corekit} {
        collage_message_print "TB-066" "For PSF 1 version, OOB ifc file must be specified"
      }
    }
  }
  if {$::collage_fabric::psf_version != 1} {
    set ::collage_tb::psf_mod_name_pfx "_psf2"
  }
}

proc collage_tb_add_ti_dut_map {ti_component dut_inst {dut_intf ""}} {
  if {[info exists ::collage_tb::ti_intf_map($dut_inst,ti_comp)]} {
    set found 0
    foreach comp $::collage_tb::ti_intf_map($dut_inst,ti_comp) {
      if {$comp == $ti_component} {
        set found 1
        break
      }
    }
    if {!$found} {
      if {$::collage_tb::enable_mi_corekit} {
        puts $::collage_tb::fmi_corekit "lappend ::collage_tb::ti_intf_map($dut_inst,ti_comp) $ti_component"
      } 
      lappend ::collage_tb::ti_intf_map($dut_inst,ti_comp) $ti_component
    }
  } else {
    if {$::collage_tb::enable_mi_corekit} {
      puts $::collage_tb::fmi_corekit "set ::collage_tb::ti_intf_map($dut_inst,ti_comp) $ti_component"
    } 
    set ::collage_tb::ti_intf_map($dut_inst,ti_comp) $ti_component
  } 

  if {[info exists ::collage_tb::ti_intf_map($dut_inst,$dut_intf,dut_ti_comp)]} {
    set found 0
    foreach comp $::collage_tb::ti_intf_map($dut_inst,$dut_intf,dut_ti_comp) {
      if {$comp == $ti_component} {
        set found 1
        break
      }
    }
    if {!$found} {
      if {$::collage_tb::enable_mi_corekit} {
        if {$dut_intf != ""} {
          puts $::collage_tb::fmi_corekit "lappend ::collage_tb::ti_intf_map($dut_inst,$dut_intf,dut_ti_comp) $ti_component"
        }
      }
      if {$dut_intf != ""} {
        lappend ::collage_tb::ti_intf_map($dut_inst,$dut_intf,dut_ti_comp) $ti_component
      }
    }
  } else {
    if {$::collage_tb::enable_mi_corekit} {
      if {$dut_intf != ""} {
        puts $::collage_tb::fmi_corekit "set ::collage_tb::ti_intf_map($dut_inst,$dut_intf,dut_ti_comp) $ti_component"
      }
    }
    if {$dut_intf != ""} {
      set ::collage_tb::ti_intf_map($dut_inst,$dut_intf,dut_ti_comp) $ti_component
    }
  }
  
  if {[info exists ::collage_tb::ti_intf_map($ti_component,dut_inst)]} {
    set found 0
    foreach inst $::collage_tb::ti_intf_map($ti_component,dut_inst) {
      if {$inst == $dut_inst} {
        set found 1
        break
      }
    }
    if {!$found} {
      if {$::collage_tb::enable_mi_corekit} {
        puts $::collage_tb::fmi_corekit "lappend ::collage_tb::ti_intf_map($ti_component,dut_inst) $dut_inst"
      } 
      lappend ::collage_tb::ti_intf_map($ti_component,dut_inst) $dut_inst
    }
  } else {
    if {$::collage_tb::enable_mi_corekit} {
      puts $::collage_tb::fmi_corekit "set ::collage_tb::ti_intf_map($ti_component,dut_inst) $dut_inst"
    } 
    set ::collage_tb::ti_intf_map($ti_component,dut_inst) $dut_inst
  }
}

######################################################################
proc _collage_get_ti_component {ti_component} {
  set ti_comp ""
  #user provided TI module and instance name
  if {[string first ":" $ti_component] > 0} {
    sv_lassign [split $ti_component ":"] ti_module_name ti_inst_name
    if {[regexp {\[\d+\]$} $ti_inst_name]} {
      collage_message_print "TB-062" "COLLAGE_TB: Array instances are not allowed in TI instance name - $ti_inst_name"
    }
    set ti_comp $ti_inst_name
    ::collage_tb::_collage_tb_set_ti_module_name $ti_module_name $ti_inst_name
  }

  return $ti_comp
}

proc collage_tb_add_ti_pin {ti_component ip_cellname interface_list} {
  
  set ti_comp [_collage_get_ti_component $ti_component]
  if {$ti_comp ne ""} {
    set ti_component $ti_comp
  }
  #should really call collage_tb_add_ti_ip_map in case this is not rtl name but we do not have that info any longer since workspace is closed
  collage_tb_add_ti_dut_map $ti_component $ip_cellname 
  foreach {port_name ifc_inst_name} $interface_list {
    collage_tb_add_one_ti_pin $ti_component $port_name $ifc_inst_name
  }
}

proc collage_tb_add_one_ti_pin {ti_component ti_intf_name sv_intf_inst} {
  if {[regexp {\[(\d+)\]$} $sv_intf_inst tmp idx]} {
    set sv_intf_inst [string trim $sv_intf_inst $tmp]
  }
  if {$sv_intf_inst == "" } {
    return
  }
  if {$sv_intf_inst == "<open>"} {
    collage_tb_update_sv_intf_inst_index "[::collage_tb::_collage_get_ti_inst_name $ti_component]_$ti_intf_name"
    return
  }
  if {[regexp {\[(\d+)\]$} $ti_intf_name tmp idx]} {
    set ti_intf_name [string trim $ti_intf_name $tmp]
  }

  if {[info exists ::collage_tb::ti_intf_map($ti_component,interfacelist)]} {
    if {![::collage_tb::_collage_tb_ti_interface_exists $ti_component $ti_intf_name]} {
      if {$::collage_tb::enable_mi_corekit} {
        puts $::collage_tb::fmi_corekit "lappend ::collage_tb::ti_intf_map($ti_component,interfacelist) \"$ti_intf_name $sv_intf_inst\""
      } 
      lappend ::collage_tb::ti_intf_map($ti_component,interfacelist) "$ti_intf_name $sv_intf_inst"
    }
  } else {
    if {$::collage_tb::enable_mi_corekit} {
      puts $::collage_tb::fmi_corekit "set ::collage_tb::ti_intf_map($ti_component,interfacelist) \"{$ti_intf_name $sv_intf_inst}\""
    } 
    set ::collage_tb::ti_intf_map($ti_component,interfacelist) "{$ti_intf_name $sv_intf_inst}"
  }
  
  #::collage_tb::_collage_tb_add_ti_pin $ti_comp $ti_intf_name $sv_intf_inst

}

proc collage_tb_add_ti_param {ti_inst param_name param_val} {
  set ::collage_tb::ti_intf_map($ti_inst,params,$param_name) $param_val
}

proc collage_tb_get_dut_insts_from_ti  {ti_inst} {
  if {[info exists ::collage_tb::ti_intf_map($ti_inst,dut_inst)]} {
    return $::collage_tb::ti_intf_map($ti_inst,dut_inst)
  } else {
    return ""
  }
}

proc collage_get_ti_name_from_inst {inst_name} {
  if {[info exists ::collage_tb::ti_intf_map($inst_name,ti_comp)]} {
    return $::collage_tb::ti_intf_map($inst_name,ti_comp)
  } else {
    return ""
  }
}

proc  collage_tb_get_ti_param {ti_inst param_name} {
  if {[info exists ::collage_tb::ti_intf_map($ti_inst,params,$param_name)]} {
    return $::collage_tb::ti_intf_map($ti_inst,params,$param_name)
  } else {
    return ""
  }
}

proc collage_tb_add_sv_intf_inst_user {sv_intf_inst sv_intf_def} {
  set ::collage_tb::sv_interface_map($sv_intf_inst,svintfinst,0) "" 
  set ::collage_tb::sv_interface_map($sv_intf_inst,svintfdef) $sv_intf_def 
}

proc collage_emu_set_replay_info {emu_name par em_type} {
  set ::collage_tb::sv_interface_map($emu_name,$em_type,emu_replay) $par
}

proc collage_emu_add_intf_inst {if_inst_name emu_type inst val} {
  if {$if_inst_name == ""} {
    return
  }
  set ::collage_tb::sv_interface_map($if_inst_name,emu_svintfdef) $emu_type
  set ::collage_tb::sv_interface_map($if_inst_name,emu_svintfinst,1) "$val"
  set ::collage_tb::sv_interface_map($if_inst_name,emu_svintfname) $inst
}

proc collage_emu_get_emu_type {if_inst_name} {
  if {[info exists ::collage_tb::sv_interface_map($if_inst_name,emu_svintfdef)]} {
    return $::collage_tb::sv_interface_map($if_inst_name,emu_svintfdef)
  } else {
    return ""
  }
}

proc collage_emu_get_emu_name {if_inst_name} {
  if {[info exists ::collage_tb::sv_interface_map($if_inst_name,emu_svintfname)]} {
    return $::collage_tb::sv_interface_map($if_inst_name,emu_svintfname)
  } else {
    return ""
  }
}

proc collage_tb_add_sv_intf_inst_dut {sv_intf_inst  sv_intf_def val {ti_inst ""}} {
  if {$sv_intf_inst == ""} {
    return
  }
  collage_tb_update_sv_intf_inst_index $sv_intf_inst
  if {$::collage_tb::enable_mi_corekit} {
    puts $::collage_tb::fmi_corekit "set ::collage_tb::sv_interface_map($sv_intf_inst,svintfdef) $sv_intf_def"
    puts $::collage_tb::fmi_corekit "set ::collage_tb::sv_interface_map($sv_intf_inst,svintfinst,1) \"$val\""
    puts $::collage_tb::fmi_corekit "set ::collage_tb::sv_interface_map($sv_intf_inst,tiinst) $ti_inst"
  } 
  set ::collage_tb::sv_interface_map($sv_intf_inst,svintfdef) $sv_intf_def 
  set ::collage_tb::sv_interface_map($sv_intf_inst,svintfinst,1) "$val"
  set ::collage_tb::sv_interface_map($sv_intf_inst,tiinst) $ti_inst
}

proc collage_tb_update_sv_intf_inst_index {sv_intf_inst} {
  if {[regexp {\[(\d+)\]$} $sv_intf_inst tmp idx]} {
    set sv_intf_inst_no_arr [string trim $sv_intf_inst $tmp]
    if {[info exists ::collage_tb::sv_interface_map($sv_intf_inst_no_arr,index)]} {
      set oldidx  $::collage_tb::sv_interface_map($sv_intf_inst_no_arr,index)
      if {$oldidx < $idx} {
        if {$::collage_tb::enable_mi_corekit} {
          puts $::collage_tb::fmi_corekit "set ::collage_tb::sv_interface_map($sv_intf_inst_no_arr,index) $idx"
        } 
        set ::collage_tb::sv_interface_map($sv_intf_inst_no_arr,index) $idx
      }
    } else {
      if {$::collage_tb::enable_mi_corekit} {
        puts $::collage_tb::fmi_corekit "set ::collage_tb::sv_interface_map($sv_intf_inst_no_arr,index) $idx"
      } 
      set ::collage_tb::sv_interface_map($sv_intf_inst_no_arr,index) $idx
    }
  }
}

proc collage_tb_add_sv_inst_rtlport_name {sv_inst_name sv_intf_name dut_inst_hier dut_inst dut_intf} {
  if {[info exists ::collage_tb::sv_interface_map($sv_inst_name,rtlportname)]} {
    return
  }
  if {[lsearch $::collage_tb::fabric_iosf_sb_hier $dut_inst] != -1 && [::collage_tb::_isa_iosf_sb_sv_ifc $sv_intf_name]} {
    set rtl_port_number [lindex [split $dut_intf "_"] end]
    if {![string is integer $rtl_port_number]} {
      return
    }
    eval_in_component $dut_inst_hier {
      if {! [catch {set is_route [get_configuration_parameter P${rtl_port_number}ROUTER2ROUTER]}] } {
        if {!$is_route} {
          if {! [catch {set rtlportname [get_configuration_parameter P${rtl_port_number}_NAME]}] } {
            set attr_val [_collage_get_sbr_attr_val $dut_inst $rtl_port_number P${rtl_port_number}_NAME]
            if {$attr_val ne ""} {
              set rtlportname $attr_val
            }
            if {$::collage_tb::enable_mi_corekit} {
              puts $::collage_tb::fmi_corekit "set ::collage_tb::sv_interface_map($sv_inst_name,rtlportname) $rtlportname"
            }
            set ::collage_tb::sv_interface_map($sv_inst_name,rtlportname) $rtlportname
          }
        }
      }
    }
  }
}

proc collage_tb_get_sv_inst_rtlport_name {sv_inst_name} {
  if {[info exists ::collage_tb::sv_interface_map($sv_inst_name,rtlportname)]} {
    return $::collage_tb::sv_interface_map($sv_inst_name,rtlportname)
  } else {
    return ""
  }
}

proc collage_tb_add_sv_inst_to_dut_map {sv_inst_name dut_inst dut_intf} {
  if {$::collage_tb::enable_mi_corekit} {
    puts $::collage_tb::fmi_corekit "set ::collage_tb::sv_interface_map(${dut_inst}_$dut_intf,duttosvinst) $sv_inst_name"
  } 
  set ::collage_tb::sv_interface_map(${dut_inst}_$dut_intf,duttosvinst) $sv_inst_name
  if {[info exists ::collage_tb::sv_interface_map($sv_inst_name,svinsttodut)]} {
    if {$::collage_tb::enable_mi_corekit} {
      puts $::collage_tb::fmi_corekit "lappend ::collage_tb::sv_interface_map($sv_inst_name,svinsttodut) \"$dut_inst $dut_intf\""
    } 
    lappend ::collage_tb::sv_interface_map($sv_inst_name,svinsttodut) "$dut_inst $dut_intf"
  } else {
    if {$::collage_tb::enable_mi_corekit} {
      puts $::collage_tb::fmi_corekit "set ::collage_tb::sv_interface_map($sv_inst_name,svinsttodut) \"{$dut_inst $dut_intf}\""
    } 
    set ::collage_tb::sv_interface_map($sv_inst_name,svinsttodut) "{$dut_inst $dut_intf}"
  }
}

proc collage_set_partitions {} {
  set par_names [collage_get_par_names]
  foreach par $par_names {
    set inst_par [collage_get_folded_name -name $par]
    if {![info exists ::collage_tb::collage_tb_misc_arr(ALL_PARS)]} {
      set ::collage_tb::collage_tb_misc_arr(ALL_PARS) $inst_par
    } else {
      if {[lsearch -exact $::collage_tb::collage_tb_misc_arr(ALL_PARS) $inst_par] == -1} {
        lappend ::collage_tb::collage_tb_misc_arr(ALL_PARS) $inst_par
      }
    }
  }
}

proc collage_emu_set_partitions {par} {
  if {![info exists ::collage_tb::collage_tb_misc_arr(EMU_PARS)]} {
    set ::collage_tb::collage_tb_misc_arr(EMU_PARS) $par
  } else {
    if {[lsearch -exact $::collage_tb::collage_tb_misc_arr(EMU_PARS) $par] == -1} {
      lappend ::collage_tb::collage_tb_misc_arr(EMU_PARS) $par
    }
  }
}

proc collage_emu_pars_empty {} {
  if {[info exists ::collage_tb::collage_tb_misc_arr(ALL_PARS)]} {
    foreach par $::collage_tb::collage_tb_misc_arr(ALL_PARS) {
      if {[lsearch -exact $::collage_tb::collage_tb_misc_arr(EMU_PARS) $par] == -1} {
        exec touch "$::env(COLLAGE_WORK)/gen/emu_tb/emu_${par}.sv"
      } 
    }
  }
  exec touch "$::env(COLLAGE_WORK)/gen/emu_tb/emu_${::collage_tb::dut_inst_name}.sv"
}

proc collage_emu_gen_replay {args} {
  parse_proc_arguments -args $args opts
  set emu_type $opts(-emu_type)
  set replay_file $opts(-replay_file)
  #set replay_file "$::env(COLLAGE_WORK)/gen/tb_specs/iosf_sbr_hw_mon_instances.txt"
  if {![info exists ::collage_tb::collage_tb_misc_arr(reconfig_tb_files,emu)]} {
    set ::collage_tb::collage_tb_misc_arr(reconfig_tb_files,emu) $replay_file
  } elseif {[lsearch $::collage_tb::collage_tb_misc_arr(reconfig_tb_files,emu) $replay_file] == -1} {
    lappend ::collage_tb::collage_tb_misc_arr(reconfig_tb_files,emu) $replay_file
  }
  set ofptr [open $replay_file w]
  collage_message_print "TB-008" "COLLAGE_EMU_TB: Fabric to fabric names are $::collage_tb::f2f_list"
  foreach val [array names ::collage_tb::sv_interface_map *,$emu_type,emu_replay] {
    set emu_inst [lindex [split $val ,] 0]
    set emu_hier $::collage_tb::sv_interface_map($emu_inst,$emu_type,emu_replay)

    if {$::collage_tb::enable_reconfig_tb} {
      set emu_hier "${::collage_tb::tb_top_define}.`[string toupper ${::collage_tb::dut_inst_name}]${::collage_tb::general_ti_str}.${emu_hier}"
    } else {
      set emu_hier "${::collage_tb::collage_design}.${::collage_tb::dut_inst_name}.${emu_hier}"
    }

    if {[lsearch -exact $::collage_tb::f2f_list $emu_inst] != -1} {
      set agent_mastering_sb_if "false"
    } else {
      set agent_mastering_sb_if "true"
    }
    puts $ofptr "$emu_inst $emu_hier $agent_mastering_sb_if"
  }
  close $ofptr
}

define_proc_attributes collage_emu_gen_replay \
    -info "Collage EMU TB - Generate replay file for emulation " \
    -define_args {
      {"-emu_type"        "Emulation type"   "" string required}
      {"-replay_file"        "Replay file name"   "" string required}
    }

proc collage_emu_set_conns {emu_inst_name ifc_port rtl_port} {
  set ::collage_tb::sv_interface_attrs_map($emu_inst_name,emu_conn,$ifc_port) $rtl_port
}

proc collage_emu_get_conns {emu_inst_name ifc_port} {
  if {[info exists ::collage_tb::sv_interface_attrs_map($emu_inst_name,emu_conn,$ifc_port)]} {
    return $::collage_tb::sv_interface_attrs_map($emu_inst_name,emu_conn,$ifc_port)
  } else {
    return "NA"
  }
}

proc collage_emu_set_params {emu_inst_name hw_type rtl_type rtl_dir inst ifc_param param_val} {
  global collage_interface_lists
  foreach pname_val [array names collage_interface_lists $hw_type,$rtl_type,$rtl_dir,$inst,emu_parameter,* ] { 
    set emu_pname [lindex [split $pname_val ,] 5]
    set pval_pair $collage_interface_lists($hw_type,$rtl_type,$rtl_dir,$inst,emu_parameter,$emu_pname)
    set ifc_pname [lindex $pval_pair 0]
    if {$ifc_pname eq "open"} {continue}
    if {$ifc_pname eq $ifc_param} {
      set ::collage_tb::sv_interface_attrs_map($emu_inst_name,emu_param,$emu_pname) $param_val
    }
  }    
}

proc collage_emu_get_params {emu_inst_name emu_param} {
  if {[info exists ::collage_tb::sv_interface_attrs_map($emu_inst_name,emu_param,$emu_param)]} {
    return $::collage_tb::sv_interface_attrs_map($emu_inst_name,emu_param,$emu_param)
  } else {
    return ""
  }
}

proc collage_tb_get_svinst_to_tiinst {sv_inst_name} {
  if {[info exists ::collage_tb::sv_interface_map($sv_inst_name,tiinst)]} {
    return $::collage_tb::sv_interface_map($sv_inst_name,tiinst)
  } else {
    return ""
  }
}
proc collage_tb_get_svinst_to_dut_list {sv_inst_name} {
  if {[info exists ::collage_tb::sv_interface_map($sv_inst_name,svinsttodut)]} {
    return $::collage_tb::sv_interface_map($sv_inst_name,svinsttodut)
  } else {
    return ""
  }
}

proc collage_tb_get_dut_to_svinst {dut_inst_intf} {
  if {[info exists ::collage_tb::sv_interface_map($dut_inst_intf,duttosvinst)]} {
    return $::collage_tb::sv_interface_map($dut_inst_intf,duttosvinst)
  } else {
    return ""
  }
}



proc collage_tb_get_sv_intfdef {sv_intf_inst} {
  if {[info exists  ::collage_tb::sv_interface_map($sv_intf_inst,svintfdef)]} {
    return $::collage_tb::sv_interface_map($sv_intf_inst,svintfdef)
  } else {
    return ""
  }
}

proc collage_tb_get_dut_inst {sv_intf_inst}  {
  if {[info exists ::collage_tb::sv_interface_map($sv_intf_inst,svintfinst,1)]} {
    set dut_inst [lindex [lindex $::collage_tb::sv_interface_map($sv_intf_inst,svintfinst,1) 0] 0]
    return $dut_inst
  } else {
    return ""
  }
  
}

proc collage_tb_is_intf_inst_exists {sv_intf_inst} {
  if {[info exists  ::collage_tb::sv_interface_map($sv_intf_inst,svintfdef)]} {
    return 1
  } else {
    return 0
  }
}


proc collage_tb_get_intf_index {intf_name_no_idx} {
  if {[info exists ::collage_tb::sv_interface_map($intf_name_no_idx,index)]} {
    return $::collage_tb::sv_interface_map($intf_name_no_idx,index)
  } else {
    return -1
  }
}

proc collage_tb_set_sv_intf_generate {sv_intf_inst name} {
  set ::collage_tb::sv_interface_map($sv_intf_inst,genname) $name
}
proc collage_tb_get_sv_intf_generate {sv_intf_inst } {
  if {[info exists ::collage_tb::sv_interface_map($sv_intf_inst,genname)]} {
    return $::collage_tb::sv_interface_map($sv_intf_inst,genname)
  }
  return ""
}

proc collage_get_and_set_instantiated {intf_name_no_idx} {
  if {[info exists ::collage_tb::sv_interface_map($intf_name_no_idx,instantiated)]} {
    return 1
  } else {
    set ::collage_tb::sv_interface_map($intf_name_no_idx,instantiated) 1
    return 0
  }
}


proc collage_tb_get_sv_inst_pin_dir {sv_intf_inst} {
  if {[info exists ::collage_tb::sv_interface_map($sv_intf_inst,svintfinst,1)]} {
    return [lindex [lindex $::collage_tb::sv_interface_map($sv_intf_inst,svintfinst,1) 0] 3]
  }
  return ""
}

proc collage_tb_add_inst_to_hier_map {dut_inst dut_inst_hier rtl_instance_real_hier} {
  if {$::collage_tb::enable_mi_corekit} {
    puts $::collage_tb::fmi_corekit "set ::collage_tb::sv_interface_map($dut_inst,hier) $rtl_instance_real_hier"
  } 
  set ::collage_tb::sv_interface_map($dut_inst,hier) $dut_inst_hier
}

proc collage_tb_get_dut_inst_hier {dut_inst} {
  if {$dut_inst == ""} {
    return $dut_inst
  } else {
    if {[info exists ::collage_tb::sv_interface_map($dut_inst,hier)]} {
      return $::collage_tb::sv_interface_map($dut_inst,hier)
    } else {
      return $dut_inst
    }
  }
}

proc collage_tb_set_exported_intf {sv_intf_inst} {
  if {![info exists ::collage_tb::sv_interface_attrs_map(aggr_exp_ports)]} {
    set ::collage_tb::sv_interface_attrs_map(aggr_exp_ports) $sv_intf_inst
  } else {
    if {[lsearch -exact $::collage_tb::sv_interface_attrs_map(aggr_exp_ports) $sv_intf_inst] == -1} {
      lappend ::collage_tb::sv_interface_attrs_map(aggr_exp_ports) $sv_intf_inst
    }
  }
}
proc collage_tb_add_sv_intf_conn {sv_intf_inst pindef conn } {
  if {$::collage_tb::enable_mi_corekit} {
    puts $::collage_tb::fmi_corekit "set ::collage_tb::sv_interface_attrs_map($sv_intf_inst,conns,$pindef) $conn"
  } 
  set ::collage_tb::sv_interface_attrs_map($sv_intf_inst,conns,$pindef) $conn
}

proc collage_tb_get_sv_intf_conn_all {sv_intf_inst rtlpin svpin type} {
  set conn [collage_tb_get_sv_intf_userconn $sv_intf_inst $svpin $type]
  if {$conn != ""} {
    set ::collage_tb::sv_interface_attrs_map($sv_intf_inst,isused,$svpin) 1
    #return [lindex [split $conn] 0]
    return "\{[lindex [split $conn] 0]\} \{[lindex [split $conn] 3]\}"
  }
  return [collage_tb_get_sv_intf_conn $sv_intf_inst $rtlpin]
}

proc collage_tb_get_sv_intf_conn {sv_intf_inst pindef } {
  if {[info exists ::collage_tb::sv_interface_attrs_map($sv_intf_inst,conns,$pindef)]} {
    return $::collage_tb::sv_interface_attrs_map($sv_intf_inst,conns,$pindef)
  }
  return "<open>"
}

proc collage_tb_add_sv_intf_userconn {sv_intf_inst pin conn type isenable force revconn} {
  set ::collage_tb::sv_interface_attrs_map($sv_intf_inst,isused,$pin) 0
  if {$type == "M"} {
    set ::collage_tb::sv_interface_attrs_map($sv_intf_inst,M,$pin) "$conn $isenable $force $revconn"
  } elseif {$type == "D"} {
    set ::collage_tb::sv_interface_attrs_map($sv_intf_inst,D,$pin) "$conn $isenable $force $revconn"
  } elseif {$type == "MD"} {
    set ::collage_tb::sv_interface_attrs_map($sv_intf_inst,M,$pin) "$conn $isenable $force $revconn"
    set ::collage_tb::sv_interface_attrs_map($sv_intf_inst,D,$pin) "$conn $isenable $force $revconn"
  }
}

proc _collage_tb_emu_process_adhoc {wire inst pin} {
  set ::collage_tb::sv_interface_attrs_map($inst,$pin,emu_user_port) $wire
}

proc _collage_tb_emu_process_param {inst param pval} {
  set ::collage_tb::sv_interface_attrs_map($inst,$param,emu_user_param) $pval
}

proc _collage_tb_emu_process_mon_param {module_name param} {
  set ::collage_tb::sv_interface_attrs_map($module_name,emu_user_mon_param) $param
}

proc _collage_tb_emu_get_mon_param {module_name} {
  if {[info exists ::collage_tb::sv_interface_attrs_map($module_name,emu_user_mon_param)]} {
    return $::collage_tb::sv_interface_attrs_map($module_name,emu_user_mon_param)
  } else {
    return ""
  }
}

proc _collage_tb_aggr_set_ti_port_override {ti_inst ti_port new_ti_port} {
  set ti_inst [::collage_tb::_collage_get_ti_inst_name $ti_inst]
  set ::collage_tb::ti_intf_map(${ti_inst}_${ti_port},ti_port_ovr) $new_ti_port
}

proc collage_tb_get_sv_intf_userpins {sv_intf_inst} {
  set pins [list]
  regsub -all {[][*?\\]} $sv_intf_inst {\\&} sv_intf_inst
  foreach key [array names ::collage_tb::sv_interface_attrs_map $sv_intf_inst,isused,*] {
    if {$::collage_tb::sv_interface_attrs_map($key) == 0} {
      lappend pins "[lindex [split $key ,] 2]"
    }
  }
  return $pins
}


proc collage_tb_get_sv_intf_userconn {sv_intf_inst pin type} {
  if {[info exists ::collage_tb::sv_interface_attrs_map($sv_intf_inst,$type,$pin)]} {
    return $::collage_tb::sv_interface_attrs_map($sv_intf_inst,$type,$pin)
  } else {
    return ""
  }
}


proc collage_tb_set_sv_intf_open {sv_intf_inst pin {dut_intf_pin ""}} {
  set ::collage_tb::sv_interface_attrs_map($sv_intf_inst,open,$pin) "1 $dut_intf_pin"
  
}

proc collage_tb_reset_sv_intf_open {sv_intf_inst pin} {
  if {[info exists ::collage_tb::sv_interface_attrs_map($sv_intf_inst,open,$pin)]} {
    set ::collage_tb::sv_interface_attrs_map($sv_intf_inst,open,$pin) 0
  }
}

proc collage_emu_set_intf_conn_props {if_inst_name pindef dut_inst_no_hier ip_name di_direction} {
  set ::collage_tb::sv_interface_attrs_map($if_inst_name,emu_props,$pindef) "$dut_inst_no_hier $ip_name $di_direction" 
}

proc collage_tb_add_sv_intf_conn_props {sv_intf_inst pindef dut_inst dut_intf intf_type di_direction used_state intf_role di_exp_from} {
  if {$::collage_tb::enable_mi_corekit} {
    puts $::collage_tb::fmi_corekit "set ::collage_tb::sv_interface_attrs_map($sv_intf_inst,props,$pindef) \"$dut_inst $dut_intf $intf_type $di_direction $used_state $intf_role $di_exp_from\""
  } 
  set ::collage_tb::sv_interface_attrs_map($sv_intf_inst,props,$pindef) "$dut_inst $dut_intf $intf_type $di_direction $used_state $intf_role $di_exp_from"
}

proc collage_tb_get_sv_intf_pin_dut_inst {sv_intf_inst pindef} {
  if {[info exists ::collage_tb::sv_interface_attrs_map($sv_intf_inst,props,$pindef)]} {
    return [lindex $::collage_tb::sv_interface_attrs_map($sv_intf_inst,props,$pindef) 0]
  } else {
    return ""
  }
}

proc collage_tb_get_sv_intf_pin_dir {sv_intf_inst pindef} {
  if {[info exists ::collage_tb::sv_interface_attrs_map($sv_intf_inst,props,$pindef)]} {
    return [lindex $::collage_tb::sv_interface_attrs_map($sv_intf_inst,props,$pindef) 3]
  }
  return ""
}

proc collage_tb_set_sv_intf_force {intf val} {
  set ::collage_tb::sv_interface_map($intf,force) $val
}

proc collage_tb_get_sv_intf_force {intf} {
  if {[info exists ::collage_tb::sv_interface_map($intf,force)]} {
    if {$::collage_tb::sv_interface_map($intf,force) == "t"} {
      return 1
    }
  } 
  return 0
}

proc collage_tb_get_sv_intf_flop_sim {intf } {
  if {[info exists ::collage_tb::sv_interface_map($intf,flop)]} {
    return  $::collage_tb::sv_interface_map($intf,flop) 
  } else {
    return ""
  }
}

proc collage_tb_set_sv_intf_flop_sim {intf clk {delay ""}} {
  set ::collage_tb::sv_interface_map($intf,flop) "$clk $delay"
}

proc collage_tb_add_sv_intf_dutintfdefs {intf dutintfdef} {
  if {[info exists ::collage_tb::sv_interface_map($intf,dutintfdefs)]} {
    if {$::collage_tb::enable_mi_corekit} {
      puts $::collage_tb::fmi_corekit "lappend ::collage_tb::sv_interface_map($intf,dutintfdefs) $dutintfdef"
    } 
    lappend ::collage_tb::sv_interface_map($intf,dutintfdefs) $dutintfdef
  } else {
    if {$::collage_tb::enable_mi_corekit} {
      puts $::collage_tb::fmi_corekit "set ::collage_tb::sv_interface_map($intf,dutintfdefs) $dutintfdef"
    } 
    set ::collage_tb::sv_interface_map($intf,dutintfdefs) $dutintfdef
  }
}

proc collage_tb_get_sv_intf_dutintfdefs {intf} {
  if {[info exists ::collage_tb::sv_interface_map($intf,dutintfdefs)]} {
    return $::collage_tb::sv_interface_map($intf,dutintfdefs)
  }
  return ""
}

proc collage_tb_add_sv_intf_params_user {sv_intf_inst  pname pval} {
  set ::collage_tb::sv_interface_map($sv_intf_inst,params,$pname) "$pval"
}

proc collage_tb_ignore_sv_intfdef_param {sv_intf_inst  pname } {
  set ::collage_tb::sv_interface_map($sv_intf_inst,ignoreparam,$pname) 1
}

proc collage_tb_is_ignore_sv_intfdef_param {sv_intf_inst  pname } {
  if {[info exists ::collage_tb::sv_interface_map($sv_intf_inst,ignoreparam,$pname)]} {
    return 1
  }
  return 0
}

proc collage_tb_get_sv_intf_pval {sv_intf_inst  pname } {
  if {[info exists ::collage_tb::sv_interface_map($sv_intf_inst,params,$pname)]} {
    return $::collage_tb::sv_interface_map($sv_intf_inst,params,$pname)
  } else {
    return ""
  }
}

proc collage_tb_add_sv_intf_params {sv_intf_def sv_intf_inst pname pval} {
  global collage_interface_lists
  if {[info exists collage_interface_lists($sv_intf_def,parameter,$pname)]}   {
    set pname [lindex $collage_interface_lists($sv_intf_def,parameter,$pname) 0]
    if {$::collage_tb::enable_mi_corekit} {
      puts $::collage_tb::fmi_corekit "set ::collage_tb::sv_interface_map($sv_intf_inst,params,$pname) \"$pval\""
    } 
    set ::collage_tb::sv_interface_map($sv_intf_inst,params,$pname) "$pval"
  } else {
    if {$collage_tb::debug_mode} {
      print_info "param: $pname does not exist in collage_interface_lists for $sv_intf_def"
    }
  }
}

###############################################################################
# Testbench verif development file. 
# ti_interface_definition
# routine that reads the ti builder. 
# logic mapping between testisland interfaces to the ip interfaces (collage interface instantiation)
###############################################################################
proc collage_ti_definition { ti_component ip_cell interface_list} {
  collage_message_print "TB-098" "COLLAGE_TB: collage_ti_definition API is deprecated. Please replace it with collage_tb_add_ti_inst calls."
  global collage_interface_lists
  set dut_inst ""
  set dut_inst_mi ""
  if {$::collage_tb::enable_mi_corekit && [collage_is_mi_inst -name [regsub {inst:} $ip_cell {}]]} {
    set dut_inst_mi [regsub {inst:} $ip_cell {}]
  }
  if {$ip_cell != ""} {
    #set dut_inst_t [::collage_tb::_collage_tb_add_ti_ip_map $ti_component $ip_cell 1]
    set dut_inst_t [::collage_tb::_collage_tb_add_ti_ip_map $ti_component $ip_cell]
    sv_lassign $dut_inst_t dut_inst rtl_instance_real_hier
    if {$dut_inst == "NA"} {return}
    if {$dut_inst == ""} {
      collage_message_print "TB-031" "COLLAGE_TB: Did not find inst $ip_cell for inst mapping for $ti_component"
      return
    }
  }
  ::collage_tb::collage_tb_add_intf_ti_dut_pinlist $ti_component $dut_inst $interface_list $rtl_instance_real_hier $dut_inst_mi
}

proc collage_tb_add_ti_inst {ti_component interface_list} {
  global collage_interface_lists
  set dut_inst ""
  ::collage_tb::collage_tb_add_intf_ti_dut_pinlist $ti_component $dut_inst $interface_list
}

proc collage_tb_add_emu_inst {interface_list} {
  global collage_interface_lists
  set ::collage_tb::emu_tb 1
  foreach {emu_name_str ip_name_pair_list} $interface_list {
    set emu_type ""
    set emu_name $emu_name_str
    set dut_inst_intf_pair_list [list]
    set par ""
    set dut_inst "NA"
    set rtl_instance_real_hier ""
    if {[string first ":" $emu_name_str] > 0} {
      sv_lassign [split $emu_name_str ":"] emu_type emu_name
    } 
    set em_type ""
    set map_cntr 1
    foreach ip_name_pair $ip_name_pair_list {
      set tmp_emu_type ""
      if {![regexp {\.} $ip_name_pair]} {
        collage_message_print "TB-067" "COLLAGE_EMU_TB: need to have rtl instance with ip_name $ip_name_pair"
      } else {
        foreach {dut_inst ifc_name} [split $ip_name_pair "."] break
        set dut_inst_mi ""
        if {$::collage_tb::enable_mi_corekit && [collage_is_mi_inst -name $dut_inst]} {
          set dut_inst_mi $dut_inst
        }
        set dut_inst_orig $dut_inst
        foreach {is_instance_name inst_not_found dut_inst dut_instance_hier rtl_instance_real_hier} [::collage_tb::_collage_tb_check_rtl_component_type "inst:$dut_inst"] break
        if {$is_instance_name == "0" && $inst_not_found == "0" && $dut_inst == "0" && $dut_instance_hier == "0" && $rtl_instance_real_hier == "0"} {set dut_inst "NA" ; continue}
        if {$rtl_instance_real_hier eq ""} {set rtl_instance_real_hier $dut_instance_hier}
        if {$inst_not_found} {
          collage_message_print "TB-068" "COLLAGE_EMU_TB: Incorrect instance name $dut_inst_orig provided in ti_def to API collage_tb_add_emu_inst"
        }
        foreach {tmp_emu_type par rtl_type di_direction} [::collage_tb::collage_tb_create_sv_interface_attrs $dut_instance_hier $dut_inst $ifc_name $emu_name $rtl_instance_real_hier "" $emu_type $dut_inst_mi 1] break
        lappend dut_inst_intf_pair_list [list $par $dut_inst $ifc_name $rtl_type $di_direction]
      }
      if {$map_cntr > 1 && $em_type ne $tmp_emu_type} {
        collage_message_print "TB-069" "COLLAGE_EMU_TB: M RTL conns found during $dut_inst,$ifc_name which cannot be connected to N hw monitor types - $em_type and $tmp_emu_type"
      }
      incr map_cntr
      set em_type $tmp_emu_type
    }
    if {$dut_inst eq "NA"} {continue}
    foreach pair [collage_emu_get_inst_names $emu_name $em_type] {
      sv_lassign $pair if_inst_name inst
       if {[info exists ::collage_tb::sv_interface_map($if_inst_name,emu_svintfinst,1)]} {
         collage_message_print "TB-070" "COLLAGE_EMU_TB: $if_inst_name already exists!! Please fix instance name provided and rerun."
       } 
       collage_emu_add_intf_inst $if_inst_name $em_type $inst $dut_inst_intf_pair_list
    }
    set emu_real_hier [join "[string range $rtl_instance_real_hier 0 [expr [string last "/" $rtl_instance_real_hier] -1]] $emu_name" "/"]
    set emu_real_hier [regsub -all {/+} $emu_real_hier {.}]
    collage_emu_set_replay_info $emu_name $emu_real_hier $em_type
  }
}

proc collage_emu_get_inst_names {emu_name emu_type} {
  global collage_interface_lists
  set emu_names [list]
  foreach val [lsort [array names collage_interface_lists emu_inst_names,$emu_type,*]] {
    set inst [lindex [split $val ,] 2]
    set if_inst_name ${emu_name}_${inst}
    if {$inst eq "NONE"} {
      set if_inst_name ${emu_name}
    }
    lappend emu_names "$if_inst_name $inst"
  }
  return $emu_names
}

###############################################################################
# 
###############################################################################
proc collage_tb_verif_generate_rtl  {args} {
  parse_proc_arguments -args $args opts

  # ---------------------------------
  # write the SV interface instances
  # ---------------------------------
  ::collage_tb::_collage_tb_verif_write_ifc_instances
  if {$::collage_tb::emu_tb} {
    ::collage_tb::_collage_tb_verif_write_emu_instances
    #Create empty files 
    collage_emu_pars_empty
    if {[info proc soc_tb_emu_post_hook] != ""} {
      soc_tb_emu_post_hook
    }
    #collage_emu_gen_replay
  }

  # ---------------------------------
  # write the test islands
  # ---------------------------------
  # Merge the multiple ti values in ti_inst_list array
  #_collage_tb_debug_globals > tb_globs.merge_ti_insts
  if {$::collage_tb::use_custom_dut_writer} {
    if {![string equal "" $::collage_tb::partition_tb]} {
      _collage_tb_verif_write_dut $::collage_tb::partition_tb
    } else {
      _collage_tb_verif_write_dut $::collage_tb::dut_inst_name
    }
  }
  if {!$::collage_tb::skip_ti_inst} {
    ::collage_tb::_collage_tb_verif_write_test_island  
  }
}

define_proc_attributes collage_tb_verif_generate_rtl \
    -info "Collage - Generate TestBench RTL " \
    -define_args {
      {"-output_dir"        "File name"   "" string optional}
    }

proc ::collage_tb::expand_inst_alias {wire_name} {
  set return_val $wire_name
  set prefix ""
  if {[regexp "^(.*)`" $wire_name temp prefix]} {
    set wire_name [string trimleft $wire_name $prefix]
  }
  if { [string index $wire_name 0] == "`" } {
    set wire_name_split [split $wire_name "."]
    set inst_name [lindex $wire_name_split 0]
    if { ![info exists ::col::instance_name($inst_name)] } {
      return $return_val
    }

    set ip_name $::col::instance_name($inst_name)
    set ip_hier_name [collage_get_hier_name -ip_name $ip_name]
    #To handle case of IP directly under soc
    if {[regexp {^//} $ip_hier_name]} {
      regsub {^//} $ip_hier_name {} ip_hier_name
    }
    set ip_hier_name_dot [regsub -all {\/} $ip_hier_name {.}]
    set hier_name ""
    if {$::design == $ip_hier_name_dot} {
      if {$::collage_tb::enable_reconfig_tb || $::collage_tb::multi_rtb_consumer} {
        set hier_name "`[string toupper ${ip_hier_name_dot}]${::collage_tb::general_ti_str}."
      } else { 
        set hier_name "${ip_hier_name_dot}."
      }
    } else {
      if {$::collage_tb::enable_reconfig_tb || $::collage_tb::multi_rtb_consumer} {
        set hier_name "`[string toupper ${::design}]${::collage_tb::general_ti_str}.${ip_hier_name_dot}."
      } else {
        set hier_name "${::design}.${ip_hier_name_dot}."
      }
    }
    set w_name_rem [lrange $wire_name_split 1 end]
    set wire_name_rem [regsub -all {\{|\}} $w_name_rem {}]
    set wire_name_rem [regsub -all " " $wire_name_rem "."]
    set return_val "${prefix}${hier_name}${wire_name_rem}"
  } 

  return $return_val
}

######################################################################
# Collage TB process connectivity file
# - these are connections in the testbench, and can be 
#   . between modules in the testbench (including the DUT)
#   . from a testbench module to a pin inside the DUT
######################################################################
proc collage_tb_process_conn_file  {args} {
  global ti_intf_map
  parse_proc_arguments -args $args opts
  
  set fn $opts(-file)
  
  collage_message_print "TB-009" "Collage_TB: processing connection file: $fn"
  
  if {[info exists opts(-dont_save_ws)] && [info exists opts(-restore_ws_on_error)]} {
    collage_message_print "TB-071" "Cannot use option -restore_ws_on_error with -dont_save_ws. Workspace must be saved to enable restore"
  }
  
  if {![info exists opts(-dont_save_ws)]} {
    save_workspace
  }
  
  set line_num 0
  sv_for_file l $fn {
    incr line_num
    set l [string trim $l]
    if {$l == ""} {continue} 
    if {[regexp {^\#} $l]} { continue }
    set l_rest [sv_lassign $l kw]
    # ii interface instance
    # ti test island instance
    # du test island instance
    # W wire
    # P parameter
    # T tie-off 
    # O open
    # V evaluate, proc
    # F force
    switch $kw {
      "W" {
        collage_message_print "TB-032" ""
        if {$::collage_tb::debug_mode} {
          print_info "found shorthand W $l_rest"
        }
        sv_lassign $l_rest wire inst_name pins comment
        _collage_tb_process_conn_w $wire $inst_name $pins $comment
      }
      "Wdu" {
        sv_lassign $l_rest wire inst_name pins comment
        _collage_tb_process_conn_wdu $wire $inst_name $pins $comment
      }
      "Aii" {
        sv_lassign $l_rest wire inst_name pin type isenable isforce isrevconn
        if {$::collage_tb::debug_mode} {
          print_info "found shorthand Aii $l_rest inst_name $inst_name pin $pin type isenable isforce "
        }
        if {![info exists ::collage_tb::sv_interface_map($inst_name,svintfinst,1)] && $::collage_tb::stop_on_error} {
          collage_message_print "TB-072" "COLLAGE_TB: incorrect inst_name \"$inst_name\" specified in line \"Aii $l_rest\" in file - \"$fn\""
        }
        if {$isenable == ""} {
          set isenable 0
        }
        if {$isforce == ""} {
          set isforce 0
        }
        if {$isrevconn == ""} {
          set isrevconn 0
        }
        if {$isenable == 0 && $type == "MD"} {
          collage_message_print "TB-038" "Can not have Aii $l_rest with type MD and isenable unset"
          continue
        }
        set wire [::collage_tb::expand_inst_alias $wire]
        _collage_tb_process_adhoc_assign $wire $inst_name $pin  $type $isenable $isforce $isrevconn
      }
      "EA" {
        sv_lassign $l_rest wire inst_name pin 
        if {$::collage_tb::debug_mode} {
          print_info "found shorthand EA $l_rest inst_name $inst_name pin $pin"
        }
        if {![info exists ::collage_tb::sv_interface_map($inst_name,emu_svintfinst,1)]} {
          collage_message_print "TB-073" "COLLAGE_EMU_TB: inst_name $inst_name provided in EA shorthand - \"EA $l_rest\" in file - \"$fn\" doesn't exist. Please fix and rerun."
        }
        set wire [regsub "/" $wire "."]
        _collage_tb_emu_process_adhoc $wire $inst_name $pin 
      }
      "EP" {
        sv_lassign $l_rest inst_name param pval
        if {$::collage_tb::debug_mode} {
          print_info "found shorthand EP $l_rest inst_name $inst_name param $param"
        }
        if {![info exists ::collage_tb::sv_interface_map($inst_name,emu_svintfinst,1)]} {
          collage_message_print "TB-074" "COLLAGE_EMU_TB: inst_name $inst_name provided in EP shorthand - \"EP $l_rest\" in file - \"$fn\" doesn't exist. Please fix and rerun."
        }
        _collage_tb_emu_process_param $inst_name $param $pval
      }
      "EMP" {
        sv_lassign $l_rest module_name pkg
        if {$::collage_tb::debug_mode} {
          print_info "found shorthand EMP $l_rest module_name $module_name pkg $pkg"
        }
        _collage_tb_emu_process_mon_param $module_name $pkg
      }
      "Wii" {
        if {$::collage_tb::debug_mode} {
          print_info "found shorthand Wii $l_rest"
        }
        sv_lassign $l_rest wire inst_name pins comment
        set wire [::collage_tb::expand_inst_alias $wire]
        _collage_tb_process_conn_w $wire $inst_name $pins $comment
      }
      "Wti" {
        collage_message_print "TB-033" ""

        #will only work with one test island component for one rtl instance
        #else use collage_tb_add_ti_pin
        sv_lassign $l_rest wire inst_name pins comment

        _collage_tb_process_conn_wti $wire $inst_name $pins $comment
      }
      "C" {
        
      }
      # force
      "F" {
        collage_message_print "TB-034" ""
        sv_lassign $l_rest inst_name intf_name
        collage_user_create_force_intf $inst_name $intf_name
      }
      "Fii" {
        if {$::collage_tb::debug_mode} {
          print_info "found shorthand Fii $l_rest"
        }
        sv_lassign $l_rest inst_name intf_name
        collage_user_create_force_intf $inst_name $intf_name
      }
      "Rii" {
        set delay ""
        sv_lassign $l_rest inst_name intf_name clk_name delay
        collage_message_print "TB-035" "COLLAGE_TB: Shorthand Rii is being processed inst_name $inst_name clk_name $clk_name delay $delay"
        collage_user_create_flop_sim $inst_name $intf_name $clk_name $delay
      }
      "T" {
      }
      "O" {
      }
      "V" {
        collage_message_print "TB-010" "evaluating inline command: $l_rest"
        eval $l_rest
      }
      "default" {
        set err_msg "illegal line \"$l\" in the adhoc connection file $fn at line number $line_num"
        collage_message_print "TB-039" $err_msg
        collage_message_print "TB-075" $err_msg	
      }
    }
  }
}

define_proc_attributes collage_tb_process_conn_file \
    -info "collage tb - process adhoc connectivity file" \
    -define_args {
      {"-file"                "File name"   "" string required}
      {"-dont_save_ws"        "Do not save the workspace before processing the file (default is to save)"   "" boolean optional}
      {"-restore_ws_on_error"  "On error in connections, close and reopen the worspace (cannot be used with -dont_save_ws option)"  "" boolean optional}
    }

######################################################################
# process "x" line from collage_tb conn file (ad hoc / manual connections)
######################################################################
proc _collage_tb_process_conn_w {wire inst_name pins comment} {
  set inst_name [::collage_tb::_collage_get_ti_inst_name $inst_name ]
  ::collage_tb::_set_w $wire $inst_name $pins $comment
}

proc _collage_tb_process_adhoc_assign {wire inst_name pin type isenable force revconn} {
  collage_tb_add_sv_intf_userconn $inst_name $pin $wire $type $isenable $force $revconn
}


######################################################################
# process "x" line from collage_tb conn file (ad hoc / manual connections)
######################################################################
proc _collage_tb_process_conn_wti {wire inst_name pins comment} {
  #ADITI: what if this this instance is connected to multiple comp
  set ti_component [collage_get_ti_name_from_inst $inst_name]
  if {$ti_component != ""} {
    collage_tb_add_one_ti_pin $ti_component $pins $wire
  } else {
    collage_message_print "TB-036" "COLLAGE_TB: Wti could find test island component name for $inst_name"
  }
}
######################################################################
# process "Wdu" line from collage_tb conn file (ad hoc / manual connections)
######################################################################
proc _collage_tb_process_conn_wdu {wire inst_name pins comment} {
  ::collage_tb::_set_wdu $wire $inst_name $pins $comment
}



###############################################################################
# process user ii ti file
###############################################################################
proc _collage_tb_process_user_ii_ti {fn stage} {
  sv_for_file l $fn {
    set cmd {}
    set l [string trim $l]
    if {$l == ""} {continue} 
    if {[regexp {^\#} $l]} { continue }

    set l_rest [sv_lassign $l kw]
    switch $kw {
      "II" {
        sv_lassign $l_rest ip_inst ip_intf
        foreach {is_instance_name inst_not_found dut_inst_no_hier dut_inst rtl_instance_real_hier} [::collage_tb::_collage_tb_check_rtl_component_type "inst:$ip_inst"] break
        set sv_intf_inst_name "${dut_inst_no_hier}_${ip_intf}"
        foreach {collintf2sv di_intf_type di_direction used_state intf_role di_exp_from} [::collage_tb::collage_tb_create_sv_interface_attrs $dut_inst $dut_inst_no_hier $ip_intf $sv_intf_inst_name $rtl_instance_real_hier] break
        if {$collintf2sv != ""} {
          lappend dut_inst_intf_pair_list [list $dut_inst_no_hier $ip_intf $di_intf_type $di_direction $used_state $intf_role $di_exp_from]
          collage_tb_add_sv_inst_rtlport_name $sv_intf_inst_name $collintf2sv $dut_inst $dut_inst_no_hier $ip_intf
          collage_tb_add_inst_to_hier_map $dut_inst_no_hier $dut_inst $rtl_instance_real_hier
          collage_tb_add_sv_inst_to_dut_map $sv_intf_inst_name $dut_inst_no_hier $ip_name
          collage_tb_add_sv_intf_inst_dut $sv_intf_inst_name  $collintf2sv $dut_inst_intf_pair_list

        }
       
      }
      "PI" {
        if {$::collage_tb::debug_mode} {
          print_info "found shorthand PI $l_rest"
        }
        # Parameter to instance: PI intf_inst parameter_name parameter_value, do not think need to check if interface instance exists
        # whole idea is to get the parameter from the dut
        sv_lassign $l_rest inst intf param_name param_val

        collage_tb_add_sv_intf_params_user "${inst}_$intf" $param_name $param_val
      }
      "PT" {
        # Parameter to instance: PI ti_inst parameter_name parameter_value
        sv_lassign $l_rest ti_inst param_name param_val
        #map ti_inst to ti_inst_list name
        collage_tb_add_ti_param $ti_inst $param_name $param_val
      } 
      "EN_TI" {
        sv_lassign $l_rest inst_name en_name
        ::collage_tb::_collage_tb_process_en_ti_line $inst_name $en_name
      }
      "EN_II" {
        if {$::collage_tb::debug_mode} {
          print_info "found shorthand EN_II $l_rest"
        }
        sv_lassign $l_rest inst_name rtl_ifc_name en_name
        ::collage_tb::_collage_tb_process_en_ii_line $inst_name $rtl_ifc_name $en_name
      }
      "TIp" {
        sv_lassign $l_rest ti_inst ti_port new_ti_port
        _collage_tb_aggr_set_ti_port_override $ti_inst $ti_port $new_ti_port
      }
      "default" {
        set err_msg "Illegal line \"$l\" in the interface connection file $fn"
        collage_message_print "TB-040" $err_msg
        collage_message_print "TB-076" $err_msg	
      }
    }
  }
  ###############################################################################
}

###############################################################################
# Testbench verif development file. 
# _collage_tb_ti_parameter_list ti_instance param_name param_val
# creates the array to be used by _collage_tb_ti_parameter_configure.
# routine is called after all the interfaces are placed
###############################################################################
# proc collage_tb_ti_set_parameter {ti_inst param_name param_val {skip_hier_lookup 0}} {
#   if {!$skip_hier_lookup} {
#     set ti_inst [_collage_tb_get_ti_hier_name $ti_inst]
#   }
#   _collage_tb_ti_parameter_list $ti_inst $param_name $param_val
# }

proc _collage_tb_ti_parameter_list {ti_inst param_name param_val} {
  collage_tb_add_ti_param $ti_inst $param_name $param_val

}



###############################################################################
# _iosf_pri_intf_assign intf_component intf_sig
# creates the assign statements for an intf instance
# intf_comp hierarchical path separated by _
# checks to see if the signal and interface are consumer/provider 
# sets assign if the interface direction is an output of the interface
###############################################################################
proc _iosf_pri_intf_assign {intf_inst intf_sig} {
  global dut_intf_list
  set ret_val 1
  set sig_name [string toupper [lindex [split $intf_sig "."] end]]
  if {[info exists ::collage_tb::sv_interface_map($intf_inst,svintfinst,1)]} {
    #set ac [index_collection [find_item -type interfaceDefn IOSF::Primary] 0]
    set ac [find_item -type interfaceDefn IOSF::Primary -filter "InterfaceVersion==${::collage_tb::prim_rtl_intf_ver}"] 
    set ifc_sig_coll [find_item -quiet -filter "Name==$sig_name" [get_attribute $ac -attrs AllChildren]]
    if {[sizeof_collection $ifc_sig_coll] != 1} {
      # unknown signal (can happen if we get iosf 1.1 port on 1.0 ifc def)
      return 1
    }
    set direction [get_attribute $ifc_sig_coll -attrs LinkDirection]
    set direction [string range $direction 4 end]; # remove the from

    #echo "DEBUG WL: sig_name = $sig_name" >> /tmp/x
    #::sSys::dump_attrs $ac >> /tmp/x
    #::sSys::dump_attrs $ifc_sig_coll >> /tmp/x
    #echo "DEBUG WL: $intf_sig $sig_name $direction [get_attribute -attrs UserName $ifc_sig_coll] [get_attribute -attrs ComponentName $ifc_sig_coll] [get_attribute -quiet -attrs LinkDirection $ifc_sig_coll]" >> /tmp/x
    set type [lindex [lindex $::collage_tb::sv_interface_map($intf_inst,svintfinst,1) 0] 3]
    set ret_val [string equal -nocase $type $direction]
    echo "$intf_inst $intf_sig $type $direction $ret_val" >> dir
  }

  return $ret_val
}

proc collage_user_create_force_intf {inst_name intf_name} {
  collage_tb_set_sv_intf_force ${inst_name}_${intf_name} "t"
}
proc collage_user_create_flop_sim {inst_name intf_name clk_name {delay ""}} {
  collage_tb_set_sv_intf_flop_sim ${inst_name}_${intf_name} $clk_name $delay
}


proc collage_user_create_force_intf_list { force_list } {
  foreach i $force_list {
    regsub -all {/} $force_list "_" force_list
    collage_tb_set_sv_intf_force $i "t"
    #echo "DEBUG: CREATE_FORCE $i => $::dut_intf_list($i,force)"
  }
}


###############################################################################
# Testbench verif development file. 
# collage_interface_definition
# routine that reads the tb builder. since interfaces do not have ports but internal wires.
# logic mapping between collage interface_defs and the interface object's wires
###############################################################################
proc collage_interface_definition { interface_name collage_intf_def intf_types interface_list } {
  global collage_interface_lists
  global collage_interface_default_parameter

  #   puts "interface_name:   $interface_name"
  #   puts "collage_intg_def: $collage_intf_def"
  #   puts "intf_role: $intf_role"
  #   puts "interface_list: $interface_list"

  # satish note (jan 29, 2011): need to check why ,interface is not added for the two below
  foreach it $intf_types {
    if {[info exists collage_interface_lists(intfdef2intf,${collage_intf_def},${it})]} {
      lappend collage_interface_lists(intfdef2intf,${collage_intf_def},${it}) [list $interface_name]
    } else {
      set collage_interface_lists(intfdef2intf,${collage_intf_def},${it}) [list $interface_name]
    }
    if {[info exists collage_interface_lists(intf2intfdef,${interface_name},${it})]} {
      lappend collage_interface_lists(intf2intfdef,${interface_name},${it}) [list $collage_intf_def]
    } else {
      set collage_interface_lists(intf2intfdef,${interface_name},${it}) [list $collage_intf_def]
    }
  }

  foreach {iname idef porttype dimension} $interface_list {
    #	echo "$iname $idef $porttype"
    if {$iname == "OPEN"} {
      set collage_interface_lists($interface_name,$collage_intf_def,$it,interface,$idef)  "open $porttype"
    } elseif {$iname == "TIEOFF"} {
      set collage_interface_lists($interface_name,$collage_intf_def,$it,interface,$idef)  [list $idef $porttype $dimension]
    } elseif {$iname != "PARAMETER"} {
      foreach it $intf_types {
        set collage_interface_lists($interface_name,$collage_intf_def,$it,interface,$idef)  [list $iname $porttype $dimension]
      }
    } else {
      # default parameters: PARAMETER <ip param name> <collage param def> <default val>
      set collage_interface_lists($interface_name,parameter,$idef) [list $porttype $dimension]
      set collage_interface_lists($interface_name,parameter,$porttype) [list $idef $dimension]
    }
  }
}

proc collage_emu_interface_definition { interface_name collage_intf_def intf_types interface_list } {
  global collage_interface_lists

  foreach it $intf_types {
    set itype $it
    set i_name "NONE"
    if {[regexp ":" $it]} {
      set itype [lindex [split $it ":"] 0]
      set i_name [lindex [split $it ":"] 1]
    }
    if {[info exists collage_interface_lists(emu_intfdef2intf,${collage_intf_def},${itype})]} {
      lappend collage_interface_lists(emu_intfdef2intf,${collage_intf_def},${itype}) [list $interface_name]
    } else {
      set collage_interface_lists(emu_intfdef2intf,${collage_intf_def},${itype}) [list $interface_name]
    }
    if {[info exists collage_interface_lists(emu_intf2intfdef,${interface_name},${itype})]} {
      lappend collage_interface_lists(emu_intf2intfdef,${interface_name},${itype}) [list $collage_intf_def]
    } else {
      set collage_interface_lists(emu_intf2intfdef,${interface_name},${itype}) [list $collage_intf_def]
    }
    
    set collage_interface_lists(emu_inst_names,${interface_name},${i_name}) 1
  }

  foreach {iname idef porttype pval} $interface_list {
    if {$iname eq ""} {
      collage_message_print "TB-077" "COLLAGE_EMU_TB: First arguement in mapping file can't be empty. Please fix it and rerun."
    }
    foreach it $intf_types {
      set itype $it
      set i_name "NONE"
      if {[regexp ":" $it]} {
        set itype [lindex [split $it ":"] 0]
        set i_name [lindex [split $it ":"] 1]
      }
      if {$iname != "PARAMETER"} {
        if {$idef eq ""} {
          collage_message_print "TB-078" "COLLAGE_EMU_TB: Interface port name can't be empty. Please put correct mapping if exist or mark it OPEN."
        }
        if {$idef == "OPEN"} {
          set collage_interface_lists($interface_name,$collage_intf_def,$itype,$i_name,emu_interface,$iname)  "open $porttype"
        } else {
          set collage_interface_lists($interface_name,$collage_intf_def,$itype,$i_name,emu_interface,$iname) "$idef" 
        }
      } else {
        if {$idef eq ""} {
          collage_message_print "TB-079" "COLLAGE_EMU_TB: After PARAMETER needs to provide monitor parameter name and it can't be empty. Please fix and rerun."
        }
        if {$porttype eq ""} {
          collage_message_print "TB-080" "COLLAGE_EMU_TB: Interface parameter name can't be empty. Please put correct mapping if exist or mark it OPEN."
        }
        if {$porttype == "OPEN"} {
          set collage_interface_lists($interface_name,$collage_intf_def,$itype,$i_name,emu_parameter,$idef) "open $pval"
        } else {
          set collage_interface_lists($interface_name,$collage_intf_def,$itype,$i_name,emu_parameter,$idef) "$porttype $pval"
        }
      }
    }
  }
}

proc collage_tb::get_fabric_ip_list { } {
  set iplist [list]
  foreach ipstr [::array names ::collage_tb::collage_tb_misc_arr *,INTFDEF] {
    lappend iplist [lindex [split $ipstr ,] 0]
  }
  return $iplist
}

proc collage_tb_set_fabric_inst_list {ip_name  instlist} {
  set ::collage_tb::collage_tb_misc_arr($ip_name,INSTLIST) $instlist
}

proc collage_tb::get_tb_fabric_inst_list {ip} {
  if {[info exists ::collage_tb::collage_tb_misc_arr($ip,INSTLIST)]} {
    return $::collage_tb::collage_tb_misc_arr($ip,INSTLIST)
  } else {
    return ""
  }
}

proc collage_tb_set_fabric_intf_name {ip_name  rtl_intf_def} {
  set ::collage_tb::collage_tb_misc_arr($ip_name,INTFDEF) $rtl_intf_def
}

proc collage_tb::get_tb_fabric_intf_name {ip} {
  if {[info exists ::collage_tb::collage_tb_misc_arr($ip,INTFDEF)]} {
    return $::collage_tb::collage_tb_misc_arr($ip,INTFDEF)
  } else {
    return ""
  }
}

proc set_tb_ip_corekit_map {ip_name ver corekits maps} {
  if {$ver == ""} {
    set ver "NA"
  }
  set ::collage_tb::collage_tb_misc_arr($ip_name,INSTALL,$ver) "[list $corekits] [list $maps]"
}

###############################################################################################
# when tb uses generate instead of ifdef, create file with list of paramaters
###############################################################################################
proc collage_tb_generate_params_file {file_name params_list} {
  set fh [open $file_name w]
  variable str_value

  set str_value "/*********************************************\n"
  append str_value "* This is a collage generated file\n"
  append str_value "*********************************************/\n\n"
  foreach param [lsort -u $params_list] {
    if {[lsearch -exact $::collage_tb::disable_params $param] != -1} {
      append str_value "  parameter $param = 0;\n"
    } else {
      append str_value "  parameter $param = 1;\n"
    }
  }
  puts $fh $str_value
  close $fh
}

proc collage_tb_load_map_file {ipname ipver} {
  if {$ipver == ""} {
    set ipver "NA"
  }
  if {![info exists ::collage_tb::collage_tb_misc_arr($ipname,INSTALL,$ipver)]} {
    collage_message_print "TB-041" "Could not load map file for ip $ipname ipver $ipver"
    return
  }
  set mapfiles [lindex $::collage_tb::collage_tb_misc_arr($ipname,INSTALL,$ipver) 1]
  foreach mapfile $mapfiles {
    if {[file exists $::collage_tb::tb_interface_defs_dir/$mapfile]} {
      collage_message_print "TB-011" "COLLAGE_TB: Sourcing file $mapfile from the path $::collage_tb::tb_interface_defs_dir"
      source $::collage_tb::tb_interface_defs_dir/$mapfile
    } else {
      collage_message_print "TB-037" "COLLAGE_TB: File $mapfile does not exist in the path $::collage_tb::tb_interface_defs_dir"
    } 
  }
  
}
proc collage_tb_install_svifc_corekit {ipname ipver install_kits_root} {
  if {$ipver == ""} {
    set ipver "NA"
  }
  if {![info exists ::collage_tb::collage_tb_misc_arr($ipname,INSTALL,$ipver)]} {
    collage_message_print "TB-042" "Could not install corekit for ip $ipname ipver $ipver"
    return
  }
  set corekits [lindex $::collage_tb::collage_tb_misc_arr($ipname,INSTALL,$ipver) 0]

  foreach corekit $corekits {
    sv_lassign $corekit ip_name corekit_name
    collage_install_ip_kit -ip_name $ip_name -kit_name $corekit_name \
        -src_dir $::collage_tb::collage_sv_ifc_kits -dest_dir $install_kits_root
  }
}

#################################################################################
# 
#################################################################################
proc _collage_tb_assemble_init {collage_sv_ifc_kits collage_sv_ifc_kits_install_dir} {
  # ----------------------------------------------------------
  # Install IPs (if pre-installing IPs, this step is optional)
  # ----------------------------------------------------------
  if {$collage_sv_ifc_kits_install_dir != ""} {
    set install_kits_root $collage_sv_ifc_kits_install_dir
  } else {
    set install_kits_root /tmp/$::env(USER)/ipkits/[pid]
    file mkdir $install_kits_root
  }
  if {!$::collage_tb::disable_auto_install} {
    collage_tb_install_svifc_corekit PSF $::collage_fabric::psf_version $install_kits_root
    collage_tb_install_svifc_corekit SBR $::collage_tb::sbr_version $install_kits_root
    collage_tb_install_svifc_corekit IDI $::collage_tb::idi_ver $install_kits_root
    collage_tb_install_svifc_corekit PMI $::collage_tb::pmi_ver $install_kits_root
    collage_tb_install_svifc_corekit PowerGating "" $install_kits_root
  }
#   if {$::collage_fabric::psf_version == 1} {
#     collage_install_ip_kit -ip_name "iosf_pri_agent_if" -kit_name "iosf_pri_agent_if" \
#         -src_dir ${collage_sv_ifc_kits} -dest_dir $install_kits_root
#     collage_install_ip_kit -ip_name "iosf_pri_fabric_if" -kit_name "iosf_pri_fabric_if" \
#         -src_dir ${collage_sv_ifc_kits} -dest_dir $install_kits_root
#   } else {
#     collage_install_ip_kit -ip_name "iosf_primary_intf" -kit_name "iosf_primary_intf" \
#         -src_dir ${collage_sv_ifc_kits} -dest_dir $install_kits_root
#   }
#   collage_install_ip_kit -ip_name "iosf_sbc_intf" -kit_name "iosf_sbc_intf" \
#       -src_dir ${collage_sv_ifc_kits} -dest_dir $install_kits_root
#   collage_install_ip_kit -ip_name "iosf_sb_pok_if" -kit_name "iosf_sb_pok_if" \
#       -src_dir ${collage_sv_ifc_kits} -dest_dir $install_kits_root

#   collage_install_ip_kit -ip_name "caidi_if" -kit_name "caidi_if" \
#       -src_dir ${collage_sv_ifc_kits} -dest_dir $install_kits_root

#   if {$::collage_tb::pmi_ver == "2.5"} {
#     collage_install_ip_kit -ip_name "pmi_ifc" -kit_name "pmi_ifc" \
#         -src_dir ${collage_sv_ifc_kits} -dest_dir $install_kits_root
#   } else {
#     collage_install_ip_kit -ip_name "pcs_req_if" -kit_name "pcs_req_if" \
#         -src_dir ${collage_sv_ifc_kits} -dest_dir $install_kits_root
#   }
#   collage_install_ip_kit -ip_name "PowerGatingNoParamIF" -kit_name "PowerGatingNoParamIF" \
#       -src_dir ${collage_sv_ifc_kits} -dest_dir $install_kits_root

  # Give a callback to add "pins" to a test island instantiation
  if {[info proc soc_tb_hook_init] != ""} {
    soc_tb_hook_init
  }

  #################################################################################
  # Set search path for IPs
  # Note: install_kits_root variable below is set in previous step above and 
  #       should be set appropriately if IPs are pre-installed
  #################################################################################
  set cur_search_path [get_activity_parameter AddSubsystemComponents SearchPath]
  set_activity_parameter AddSubsystemComponents SearchPath "$install_kits_root $cur_search_path" 
}

proc _collage_tb_process_rtl {dut_inst_name} {
  global param_names

  foreach full_path [get_hdl_file_list] {
    set d [file dir $full_path]
    set f [file tail $full_path]
    set orig_copy ${d}/${f}.orig
    file rename -force $full_path $orig_copy
    set fh [open $full_path w]
    set skip 0
    set this_ti_name ""
    set dut_clk_assign 0
    set skip_blank 0


    set line_num 0
    sv_for_file l $orig_copy {
      incr line_num
      # Remove code to be stripped out
      if {[regexp {^`ifndef THIS_TO_BE_STRIPPED_OUT} $l]} {
        set skip 1
      }

      # Remove logic_dc
      if {[regexp {(wire ).*( logic_dc_)} $l]} {
        set is_logic_dc 1
      } else {
        set is_logic_dc 0
      }

      # Remove logic_dc in pin
      set l [regsub {(\(logic_dc[^\)]+\))} $l {()}] ; # this makes it empty
      #set l [regsub {(\(logic_dc_soc_)} $l {(}] ; # this leaves the pin name in there

      # Remove the IO lines coming from processing opens on SV interfaces
      if {[regexp "inout  logic" $l] || [regexp "input " $l] || [regexp "output " $l] || [regexp "inout " $l] || [regexp "(assign ).*(gated_side_clk ).*" $l] || [regexp "(logic ).*(gated_side_clk;).*" $l]} {
        set is_port 1
      } else {
        set is_port 0
      }

      # Write line to file
      if {!$skip && !$is_logic_dc && !$is_port} {
        if {!$skip_blank} {
          puts $fh $l
        } elseif {$skip_blank && $l ne ""} {
          puts $fh $l
        }
      }

      set skip_blank 0
      # Reset skip
      if {[regexp {^`endif} $l]} {
        set skip 0
        set skip_blank 1
      }
      if {[regexp {^  end\nendgenerate} $l]} {
        set skip 0
      }

      # end of the loop
    }


    close $fh
  }
}

proc _collage_tb_get_svifc_hier_name {ip {suffix ""}} {
  set ip_par [::col::get_ip_hier_par ${ip} 0]
  if {$suffix != ""} {
    set suffix "_$suffix"
  }
  if {$ip_par == ""} {
    set hier_name ${ip}${suffix}
  } else {
    set hier_name [regsub -all {/} ${ip_par}_${ip}${suffix} {_}]
  }

  return $hier_name
}

#{{{ _collage_tb_get_dut_pins
proc _collage_tb_get_dut_pins { dut_inst_name } { 
  set_current_component -quiet $dut_inst_name
  set dut_pins [list]
  foreach p_dir "in inout out" {
    foreach_in_collection p [find_item -quiet -type port * -sort Name -filter "PortDirection==${p_dir}&&ParentTypeName==design"] {
      sv_lassign [get_attribute -attrs UserName $p] p_name
      set pwidth [lindex [get_attribute $p -attr PortWidth] 0]
      set width ""
      if {$pwidth > 1} {
        set msb [get_attribute $p -attr StartBits]
        set lsb [get_attribute $p -attr EndBits]
        #set width "\[${msb}:${lsb}\]"
        if { [llength $msb] > 0 } {
          for {set j 0} { $j < [llength $msb] } { incr j } {
            set mbit  [regsub -all {@} [lindex $msb $j] {}]
            set lbit  [regsub -all {@} [lindex $lsb $j] {}]
            
            set mbit_trim [regsub -all {@|\(|\)} [lindex $msb $j] {}]
            set lbit_trim [regsub -all {@|\(|\)} [lindex $lsb $j] {}]
            append width "\[$mbit:$lbit\]"
          }
        }
      }
      lappend dut_pins [list $p_name $width]
    }
  }
  set_current_component -quiet
  return $dut_pins;
  
}; # end proc _collage_tb_get_dut_pins
#}}}

#{{{ _collage_tb_autogen_wdu
proc _collage_tb_autogen_wdu { dut_inst_name} { 
  array set auto_wdu {}
  
  foreach p [_collage_tb_get_dut_pins $dut_inst_name] {
    sv_lassign $p pname width
    set pname_wire [::collage_tb::_get_wdu ${dut_inst_name}/$pname]
    if {![string match "" $pname_wire]} { continue }
    set auto_wdu($pname) [list $pname $width]
  }
  
  set auto_wdu_conn_file "$::env(COLLAGE_WORK)/gen/tb_specs/${::collage_tb::dut_inst_name}_tb_autowdu.txt"
  set wdufp [open $auto_wdu_conn_file w]
  foreach p [array names auto_wdu] {
    puts $wdufp [format "Wdu %s %s %s" [lindex $auto_wdu($p) 0] ${::collage_tb::dut_inst_name} $p]
  }
  close $wdufp
  
  set auto_wdu_wire_file "$::env(COLLAGE_WORK)/gen/tb_specs/${::collage_tb::dut_inst_name}_tb_autowdu.sv"
  set wdufp [open $auto_wdu_wire_file w]
  foreach p [array names auto_wdu] {
    puts $wdufp [format "  logic %-3s %s;" [lindex $auto_wdu($p) 1] [lindex $auto_wdu($p) 0]]
  }
  close $wdufp

  collage_tb_process_conn_file -file $auto_wdu_conn_file   
}; # end proc _collage_tb_autogen_wdu
#}}}

#################################################################################
# Write DUT
# _collage_tb_verif_write_dut
#################################################################################
proc _collage_tb_get_hdl_value {comp pname} {
  set language "systemverilog"
  set param [find_item -type param $comp/$pname]
  set pval [get_attribute $param -attr HdlValue -subscript $language]
  return $pval
}

proc _collage_tb_verif_write_dut {dut_inst_name} {
  global collage_tb_addons

  if {$::collage_tb::enable_split_tb_sv} {
    puts $collage_tb_addons "   `include \"${::collage_tb::collage_design}_module.sv\"\n"
    set dut_module "$::env(COLLAGE_WORK)/$::collage_tb::collage_design/src/${::collage_tb::collage_design}_module.sv"
    set ::collage_tb::collage_tb_misc_arr(reconfig_tb_files,MODULE) $dut_module
    puts $collage_tb_addons "   `include \"${::collage_tb::pre_post_include_prefix}_pre_ti_include.sv\"\n"
    set module_inst_file [open "$dut_module" "w"]
  } else {
    set module_inst_file $collage_tb_addons
  }

  if {$::collage_tb::enable_auto_wdu && $::collage_tb::enable_auto_wdu_include} {  
    puts $module_inst_file "`include ${::collage_tb::dut_inst_name}_autowdu.sv"
  }
  
  # Collect DUT data
  set dut_inst_ref [find_item -quiet -type cell $dut_inst_name]
  if {[sizeof_collection $dut_inst_ref] == 0} { 
    set emsg "COLLAGE_TB: write DUT - unable to find dut instance: $dut_inst_name"
    collage_message_print "TB-043" $emsg
    collage_message_print "TB-081" $emsg
  }
  set dut_design_ref [get_attribute -attrs ReferenceDesign $dut_inst_ref]
  set dut_module_name [get_attribute -attrs UnelabName $dut_design_ref]
  
  # DUT Parameters
  array set dut_params {}
  foreach_in_collection p [find_item -quiet -type param ${dut_inst_name}/* -filter "FromArchitecture==0"] {
    sv_lassign [split [get_attribute -attrs Name $p]  "/"] comp pname
    set pval [get_configuration_parameter -component $comp $pname]
    if {[regexp {^0x} $pval]} {
      set pval [_collage_tb_get_hdl_value $comp $pname]
    }
    set dut_params($pname) $pval
  }


  # DUT Ports : Get the pins by direction and in sorted order
  set dut_pins [_collage_tb_get_dut_pins ${dut_inst_name}]
  set longest_pin_name 0
  foreach p $dut_pins {
    set this_pin_len [string length [lindex $p 0]]
    if {$this_pin_len > $longest_pin_name} {
      set longest_pin_name $this_pin_len
    }
  }
  
  incr longest_pin_name 2
  set num_ports [llength $dut_pins]

  # Write the DUT instantiation
  set dut_sv_string ""
  if {[array size dut_params] != 0} {
    append dut_sv_string "   ${dut_module_name}\n"
    append dut_sv_string "      #("
    set dut_params_list [lsort [array names dut_params]]
    set num_params [llength $dut_params_list]
    set idx 0
    foreach pname $dut_params_list {
      set pval $dut_params($pname)
      incr idx
      if {$idx == 1} {
        # this is the first param
        append dut_sv_string ".${pname}($pval)"
      } else {
        append dut_sv_string "        .${pname}($pval)"
      }

      if {$idx == $num_params} {
        # this is the last param
        append dut_sv_string ") ${dut_inst_name}\n"
      } else {
        append dut_sv_string ",\n"
      }
    }
  } else {
    append dut_sv_string "   ${dut_module_name} ${dut_inst_name}\n"
  }
  
  if {$num_ports > 0} {
    set idx 0
    foreach p $dut_pins {
      set pname [lindex $p 0]
      set pname_wire [::collage_tb::_get_wdu ${dut_inst_name}/$pname]
      incr idx
      if {$idx == 1} {
        # this is the first port
        append dut_sv_string [format "      (.%-${longest_pin_name}s(%s)" ${pname} $pname_wire]
      } else {
        append dut_sv_string [format "       .%-${longest_pin_name}s(%s)" ${pname} $pname_wire]
      }

      if {$idx == $num_ports} {
        # this is the last port
        append dut_sv_string ");\n"
      } else {
        append dut_sv_string ",\n"
      }
    }
  } else {
    append dut_sv_string "      ();\n"
  }

  puts $module_inst_file $dut_sv_string

  if {$::collage_tb::enable_split_tb_sv} {
    close $module_inst_file
  }
}

proc collage_tb_generate_ip_enable_file {file_name} {
  set fptr [open $file_name "w"]
  puts $fptr "<enable-profile>"
  foreach key [lsort [::array names ::collage_tb::collage_tb_misc_arr *,ENABLE_NAME]] {
    foreach {ip_name tmp} [split $key ,] {break}
    puts $fptr "   <if-enable key=\"$ip_name\" type=\"instance\">"
    foreach enname [lsort -u $::collage_tb::collage_tb_misc_arr($key)] {
      set enname [regsub {_ENABLE$}  $enname ""]
      puts $fptr "      $enname"
    }
    puts $fptr "   </if-enable>"
    
  }
  puts $fptr "</enable-profile>"
  close $fptr
}
proc collage_tb_ignore_svdef_param {intf_def param} {
  collage_tb_ignore_sv_intfdef_param $intf_def $param
}

proc collage_tb_set_fabric_cond {sv_intf_def ti_comp_list} {
  foreach ti_comp $ti_comp_list {
    if {[string first ":" $ti_comp] > 0} {
      sv_lassign [split $ti_comp ":"] ti_comp ti_intf_inst
      if {[info exists ::collage_tb::collage_tb_misc_arr($sv_intf_def,$ti_comp)]} {
        if {$::collage_tb::collage_tb_misc_arr($sv_intf_def,$ti_comp) == 1} {
          collage_message_print "TB-044" "Can not set fabric property on ti_component as well as specific interfaces of ti"
          return
        }
        lappend ::collage_tb::collage_tb_misc_arr($sv_intf_def,$ti_comp) $ti_intf_inst
      } else {
        set ::collage_tb::collage_tb_misc_arr($sv_intf_def,$ti_comp) $ti_intf_inst
      }
    } else {
      set ::collage_tb::collage_tb_misc_arr($sv_intf_def,$ti_comp) 1
    }
  }
}
