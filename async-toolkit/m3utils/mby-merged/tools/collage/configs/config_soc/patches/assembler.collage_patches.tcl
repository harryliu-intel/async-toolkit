############################################################################
###JPK MI TI to RTL PATCH
############################################################################

#  proc ::collage_tb::_collage_tb_check_rtl_component_type {ip_cellname} {
#
#    set is_instance_name 0
#    set inst_not_found 0
#    set rtl_instance ""
#    set rtl_instance_hier ""
#    set rtl_instance_real_hier ""
#    set hier_inst ""
#    
#    #check to see if mapped to rtl instance name or module name
#    set fields [split $ip_cellname ":"]
#    #check for instance syntax
#    if {[llength $fields] == 2 && [lindex $fields 0] == "inst"} {
#      set is_instance_name 1
#      set my_cellname [lindex $fields 1]
#      set my_cellname [::col::get_ip_real_name $my_cellname]
#    } else {
#      #module syntax
#      set is_instance_name 0
#      set my_cellname $ip_cellname 
#    }
#
##    if {[collage_is_mi_inst -name $my_cellname] && $old_proc} {
##      error "collage_ti_definition is not supported for MI. Please use collage_tb_add_ti_inst instead and rerun."
##    }
#    if {$::collage_tb::enable_mi_corekit && [collage_is_mi_inst -name $my_cellname]} {
#      #Dump MI info
#    } elseif {$::collage_tb::enable_mi_corekit && ![collage_is_mi_inst -name $my_cellname]} {
#      return "0 0 0 0 0"
#    } elseif {!$::collage_tb::enable_mi_corekit && [collage_is_mi_inst -name $my_cellname] && $::collage_tb::mi_corekit_flow} {
#      return "0 0 0 0 0"
#    } 
#
#    if {[collage_is_mi_inst -name $my_cellname]} {
#      set hier_inst [collage_get_hier_name -ip_name $my_cellname]
#    }
#
#    #search for instance name and verify it exists
#    if {$is_instance_name} {
#      #ADDING FOR MI
#      #for corekit flow, my_cellname based on folded_instance_map is needed before setting hierarchy
#      if {[info exists ::col_mi::folded_instance_map($my_cellname)] && $::collage_tb::enable_mi_corekit} {
#        set hier_inst [collage_get_hier_name -ip_name $my_cellname]
#        #set my_cellname $::col_mi::folded_instance_map($my_cellname)
#        #set my_cellname [lindex [split $my_cellname "/"] 1]
#        #set rtl_instance_real_hier [join "[string range $hier_inst 0 [expr [string last "/" $hier_inst] -1]] $my_cellname" "/"] 
#      }
#      eval_in_component [collage_get_ip_hier_par -ip_name $my_cellname] {
#        set cell [find_item -type cell -quiet [collage_get_folded_name -name $my_cellname]]
#        set tempcell [collage_get_folded_name -name $my_cellname]
#        if {$cell != ""} {
#          set rtl_instance [collage_get_folded_name -name $my_cellname]
#          set rtl_instance_hier [string trimleft [get_attribute -attr ComponentOfItem $cell] "/"]
#          #set rtl_instance_hier [string trimleft [collage_get_hier_name -ip_name $my_cellname]  "/"]
#          set inst_not_found 0
#        } else {
#          set inst_not_found 1
#        }
#      }
#    } else {
#      #map rtl instance ptr, get rid of module names and verify inst exists
#      #find collage instances in workspace
#      #get all instance of the module
#      set design [find_item -quiet -type design -filter "IsHierarchicalComponent==0 && (UnelabName==$my_cellname || Name==$my_cellname)"]
#
#      set num [sizeof_collection $design]
#      echo "collage_tb_check_rtl_component_type module:$ip_cellname cellname:$my_cellname num $num"
#      if {$num == 0} {
#        set inst_not_found 1
#      } else {
#        #if more than one we cannot map this module
#        if {$num > 1} {
#          collage_message_print "TB-049" "More than one instance of $ip_cellname, cannot map to TI"
#        } else {
#          #convert coretools instance ptr to module name
#          
#          set rtl_instance [get_attribute -attrs "Name" [index_collection [get_attribute -attrs CellReferences $design]  0]]
#          set rtl_instance_hier [string trimleft [get_attribute -attr ComponentOfItem $design] "/"]
#
#        }
#      }
#    }
#    
#    return "$is_instance_name $inst_not_found $rtl_instance $rtl_instance_hier $hier_inst"
#  }

###JPK END OF MI TI to RTL PATCH
############################################################################

proc collage_mark_open_ifc {intfName type signallist} {
  collage_eval_in_comp -use_hier_par -edit_mi [collage_get_leaf_ips ] {
    foreach_in_collection intf [ find_item -quiet -type interfaceInstance -filter "InterfaceDefinitionName==$intfName && InterfaceType==$type"] {
      foreach sig $signallist {
        set port [get_children -name $sig $intf]
        if {$port != ""} {
          print_info "MARK OPEN [get_current_component] [get_attribute -attr Name $intf] $sig"
          set_interface_port_attribute $intf $sig InterfaceLink "<open>"
        }  
      }    
    }      
  }        
}



proc collage_tag_sbifc_portid {args} {
  parse_proc_arguments -args $args opts

  set inst_name $opts(-inst_name)
  set ifc_name $opts(-ifc_name)
  set pname    $opts(-pname)

  set type port
  if { [info exists opts(-type)] } {
     set type     $opts(-type)
   }

  set value 1
  if { [info exists opts(-value)] } {
     set value     $opts(-value)
   }

  eval_in_component [collage_get_hier_name -ip_name $inst_name] {
    # FIXME
    #collage_set_descriptor_tag -$type -descriptor_tag_name SBID -descriptor_tag_value $value
    if {$type == "port"} {
    } 
    collage_set_descriptor_tag -interface $ifc_name    -descriptor_tag_name SBID -descriptor_tag_value $pname
  }
}

define_proc_attributes collage_tag_sbifc_portid \
   -info "Collage - associate a SB interface  with its corresponding portid param or port " \
    -define_args {
      {"-inst_name"   "Name of RTL instance"                             "" string required}
      {"-ifc_name"    "Name of interface instance"                             "" string required}
      {"-type"        "Type" "port"  one_of_string {optional {values {"port" "param" }}}}
      {"-value"       "SBID value "   "1" string optional}
      {"-pname"       "name of the SBID port or param"  "" string required}
    }

###############################################################################
# Bug fix: added usage of -type
# 
###############################################################################
proc collage_set_interface_strap {args} {
  parse_proc_arguments -args $args opts

  set interface_inst $opts(-ifc_inst_name)
  set type $opts(-type)
  set strap_name $opts(-strap_name)
  set value $opts(-value)

  if { ($type != "port") && ($type != "param") } {
    error "Illegal type specified for strap object"
  }

  #FIXME - add a check for whethe
  if {$type == "port"} {
  }

  collage_set_descriptor_tag  -interface $interface_inst -descriptor_tag_name $strap_name -descriptor_tag_value "${type}:${value}"
}

define_proc_attributes collage_set_interface_strap \
    -info "Collage - Set Interface Strap" \
    -define_args {
      {"-ifc_inst_name"    "Interface Instance Name"   "" string required}
      {"-strap_name"       "Architectural name of the strap"   "" string required}
      {"-type"             "Type of strap: port or param"   "" string required}
      {"-value"            "Asscociated port or parameter"   "" string required}
    }



proc collage_tag_ifc {args} {
  parse_proc_arguments -args $args opts

  set inst_name $opts(-inst_name)
  set ifc_name $opts(-ifc_name)
  set pname    $opts(-pname)
  set tagval    $opts(-tagval)

  # only attach on reference instance
  if {[collage_is_mi_inst -name $inst_name] &&  [::col::is_readonly_inst  $inst_name  1] } {
      return
  }

  collage_eval_in_component  -use_hier_name  $inst_name {
    # FIXME
    # check anc make sure that the portname is indeed a port name on
      # the ip instance
    #collage_set_descriptor_tag -port $pname descriptor_tag_name POK -descriptor_tag_value 1
    collage_set_descriptor_tag  -interface $ifc_name    -descriptor_tag_name $tagval -descriptor_tag_value $pname
  }
}
define_proc_attributes collage_tag_ifc \
   -info "Collage - associate a IOSF SB/PRIMARY interface  with its corresponding pok  port " \
    -define_args {
      {"-inst_name"   "Name of RTL instance"                             "" string required}
      {"-ifc_name"    "Name of interface instance"                       "" string required}
      {"-pname"       "name of the  port"  "" string required}
      {"-tagval"      "TAG - POK CGEN "  "" string required}
    }


proc collage_tag_sbifc_pok {args} {
  parse_proc_arguments -args $args opts

  set inst_name $opts(-inst_name)
  set ifc_name $opts(-ifc_name)
  set pname    $opts(-pname)

   collage_tag_ifc -inst_name $inst_name -ifc_name $ifc_name -pname $pname -tagval POK
}

define_proc_attributes collage_tag_sbifc_pok \
   -info "Collage - associate a IOSF SB/PRIMARY interface  with its corresponding pok  port " \
    -define_args {
      {"-inst_name"   "Name of RTL instance"                             "" string required}
      {"-ifc_name"    "Name of interface instance"                       "" string required}
      {"-pname"       "name of the  port"  "" string required}
    }

# connect ifcs
# for all leaf ips
# get all provider ifcs of type iosf::sb with descriptor tag  sink_instance  sinkifc  sbid_value and pok for these also
# provider_ifcs{source_instance/sbifc_name} {sink_instance, sink ifc,sbid_value,  pok_portname}
# get all consumer ifcs of type iosf::sb with descriptor tag  sbid   
# consumer_ifcs (ip_instance/sbifc_name} (port/param, param/portname, pok_portname)
# 
#
# connect portids
# connect poks


proc collage_gen_params_from_intf_to_remove { args } {
  parse_proc_arguments -args $args opts
  set strap_file $opts(-param_file)
  set param_tag $opts(-param_tag)
  set value_tag ""
  if {[info exists opts(-value_tag)]} {
    set value_tag $opts(-value_tag)
  }
  set strapfptr [open $strap_file "w"]
  foreach  intf_pair [collage_get_objects_with_descriptor_tag -descriptor_tag_name $param_tag -object_type interface] {
    set sourceip [lindex [split [lindex $intf_pair 0] "/"] 0]
    set intfname [lindex [split [lindex $intf_pair 0] "/"] 1]
    set param_value  [lindex $intf_pair 1]
    collage_eval_in_component -use_hier_par $sourceip {
      set srcintf [find_interface_instances -component $sourceip -name $intfname]
      if {$srcintf == ""} {
        print_error "Could not find ip $sourceip or interface $intfname in ip $sourceip"
      }
      if {$value_tag != ""} {
        set param_value [get_attribute $srcintf -attr GenericAttr -subscript $value_tag]
      }
    }
    if {[string is integer $param_value]} {
      set param_value [format "0x%x" $param_value]
    }
    foreach param [collage_get_objects_with_descriptor_tag -descriptor_tag_name  $param_tag -object_type param] {
      puts $strapfptr "P $param_value $port"
    }
    foreach port [collage_get_objects_with_descriptor_tag -descriptor_tag_name  $param_tag -object_type port] {
      puts $strapfptr "T $param_value $port"
    }
  }
  close $strapfptr
}

define_proc_attributes collage_gen_params_from_intf  \
    -info "Collage - collage create connnections from interface descriptor tags" \
    -define_args {
      {"-param_file"      "File for params"   "" string required}
      {"-param_tag"       "Paramter Tag"   "" string required}
      {"-value_tag"       "Value Tag"   "" string optional}
    }

#########################################################################
# 
# Usage example:
# 
#  collage_gen_connections_from_intf \
#      -std_conn_file ${::auto_conn_dir}/fabric_iosf_sideband.cfg \
#      -adhoc_conn_file ${::auto_conn_dir}/adhoc_iosf_sideband.txt \
#      -tieoff_file ${::auto_conn_dir}/straps_iosf_sideband.txt \
#      -inst_tag SINK_INSTANCE \
#      -intf_tag SINK_IFC \
#      -port_tags "POK" -tieoff_tags "SBID" -intf_name "IOSF::SB" 
# 
# Algorithm:
#
# Find all interface instance objects with descriptor "$inst_tag"
# for each such ifc inst
#   - the value is expected to be a list of pairs as below
#    {source_inst/intf sink_inst}
#    {sbr0/port_0 fuse_top} {sbr0/port_1 fuse_top} {sbr0/port_4 pmc} {sbr0/port_6 pmc} {sbr0/port_8 cci_top}
#   - In each sink_inst find all interfaces of type "$intf_name"
#     If there is more than one interface instance of that type on the sink, then need to filter by tag "$intg_tag"
#########################################################################
proc collage_gen_connections_from_intf_to_remove { args } {
  parse_proc_arguments -args $args opts
  set inst_tag $opts(-inst_tag)
  set std_conn_file $opts(-std_conn_file)
  set adhocfptr ""
  set tieofffptr ""
  set intf_tag ""
  set port_tags ""
  set tieoff_tags ""
  set target_intfname ""

  if {[info exists opts(-adhoc_conn_file)]} {
    set adhocfptr [open $opts(-adhoc_conn_file) "w"]
  }
  if {[info exists opts(-tieoff_file)]} {
    set tieofffptr [open $opts(-tieoff_file) "w"]
  }
  if {[info exists opts(-intf_tag)]} {
    set intf_tag $opts(-intf_tag)
  }
  if {[info exists opts(-intf_defn)]} {
    set intf_defn $opts(-intf_defn)
  }
  if {[info exists opts(-port_tags)]} {
    set port_tags $opts(-port_tags)
    if {$adhocfptr == ""} {
      print_error "please provide file name to write adhoc connection with -port_tags option"
      return
    }
  }
  if {[info exists opts(-tieoff_tags)]} {
    set tieoff_tags $opts(-tieoff_tags)
    if {$tieofffptr == ""} {
      print_error "please provide file name to write tieff connection with -tieoff_tags option"
      return
    }
  }

  set stdfptr [open $std_conn_file "w"]

  foreach  intf_pair [collage_get_objects_with_descriptor_tag -descriptor_tag_name $inst_tag -object_type interface] {
    #puts "DBG: Processing $intf_pair"
    set sourceip [lindex [split [lindex $intf_pair 0] "/"] 0]
    set intfname [lindex [split [lindex $intf_pair 0] "/"] 1]
    set targetip  [lindex $intf_pair 1]
    set target_intfname ""
    ## FIXME_SV: assumption that the value is the instance name (use inst alias?)
    ## note: the -quiet is needed below to allow for same descriptor tag to be used for multiple interface types such as IOSF::SB
    ##       IOSF::Primary, etc
    set skip_by_ifc_type_check 0
    collage_eval_in_component -use_hier_par $sourceip {
      set srcintf [find_interface_instances -quiet -component $sourceip -name $intfname -filter InterfaceDefinitionName==$intf_defn]
      if {$srcintf == ""} {
	## FIXME? need to determine how to distinguish between erroneous data vs. bad input
        #print_warning "Could not find ip $sourceip or interface $intfname in ip $sourceip"
	set skip_by_ifc_type_check 1
      } else {
	if {$intf_tag != ""} {
	  set target_intfname [get_attribute $srcintf -attr GenericAttr -subscript $intf_tag]
	} else {
	  set target_intfname ""
	}
      }
    }

    if {$skip_by_ifc_type_check} {
      continue
    }

    ## if instance does not exist, give a warning and continue
    if {![_collage_inst_exists_in_any_hier $targetip]} {
      print_warning "Auto gen: did not find target $targetip for source ${sourceip}/$intfname"
      continue
    }

    # if target instance exists, then find the sink interface on it
    collage_eval_in_component -use_hier_par $targetip {
      if {$target_intfname != ""} {
	# interface name was specified in the value of the source tag
        set targetintf [find_interface_instances -component $targetip -name $target_intfname]
        if {$targetintf == ""} {
          print_error "Could not find targetip $targetip or interface $target_intfname in $targetip"
        }
      } else {
        set targetintf  [find_interface_instances  -component $targetip -filter "InterfaceDefinitionName==$intf_defn"]
        if {[sizeof_collection $targetintf] != 1} {
          print_error "found [sizeof_collection $targetintf] interface of type $intf_defn in instance $targetip, can not generate connect $targetintf"
          return
        }
      }
    }

    puts $stdfptr "C $sourceip $intfname $targetip [get_attribute -attr Name $targetintf]"
    foreach port_tag $port_tags {
      set sport [get_attribute $srcintf -attr GenericAttr -subscript $port_tag]
      if {$sport != ""} {
        set tport [get_attribute $targetintf -attr GenericAttr -subscript $port_tag]
        if {$tport != ""} {
          puts $adhocfptr "C $targetip/$tport $sourceip/$sport"
        }
      }
    }
    foreach  tieoff_tag $tieoff_tags {
      ## add a check to find which is value and which is port"
      set value [get_attribute $srcintf -attr GenericAttr -subscript $tieoff_tag]
      if {$value == ""} {
        print_warning "... value of attribute  $tieoff_tag on [get_attribute -attrs UserName $srcintf] is empty"
      }
      if {$value != "" && [string is integer $value]} {
        set value [format "0x%x" $value]
      }
      if {$value != ""} {
        set tport [get_attribute $targetintf -attr GenericAttr -subscript $tieoff_tag]
        if {$tport !=  ""} {
          puts $tieofffptr "T $value $targetip/$tport"

        }
      }
    }
  }
  close $stdfptr
  if {$adhocfptr != ""} {
    close $adhocfptr
  }
  if {$tieofffptr != ""} {
    close $tieofffptr
  }
}

define_proc_attributes  collage_gen_connections_from_intf_to_remove \
    -info "Collage - collage create connnections from interface descriptor tags" \
    -define_args {
      {"-std_conn_file"        "File for std connections"    "" string required}
      {"-adhoc_conn_file"       "File for std connections"   "" string optional}
      {"-inst_conn_file"        "File for std connections"   "" string optional}
      {"-tieoff_file"           "File for straps connection" "" string optional}
      {"-inst_tag"              "Instance Tags"              "" string required}
      {"-intf_tag"              "Interface Tags"             "" string optional}
      {"-port_tags"             "Port Tags"                  "" string optional}
      {"-tieoff_tags"           "Tieoff Tags"                "" string optional}
      {"-intf_defn"             "Interface definition name such as IOSF::SB"              "" string required}
    }

#########################################################################
#########################################################################
collage_register_descriptor_tag  -descriptor_tag_name POK_TIEOFF  -descriptor_tag_description  {}
collage_register_descriptor_tag  -descriptor_tag_name CGEN        -descriptor_tag_description  {}
collage_register_descriptor_tag  -descriptor_tag_name CONN_NAME_IFC -descriptor_tag_description  { Specifies name of the interfaces for the connection }

    
#########################################################################
# Internal procedure to find if an instance exists in any hierarchy
#########################################################################
proc _collage_inst_exists_in_any_hier_to_remove {instname} {
  set ret_val xx
  eval_in_component [collage_get_ip_hier_par -ip_name $instname] {
    set cell [find_item -type cell -quiet $instname]
    if {$cell != ""} {
      set ret_val 1
    } else {
      set ret_val 0
    }
  }
  return $ret_val
}

#########################################################################
###############################################################################
# Insert IDVP fublets into partitions and connect chain including controller
###############################################################################

proc collage_connect_idvp_chain {args} {
  parse_proc_arguments -args $args opts

  set idv_spec_file $opts(-idv_spec_file)
  set module_name "c69pxidv_x9idvwmif4"
  set idv_in_name "idv_in"
  set idv_out_name "idv_out"

  if {[info exists opts(-module_name)]} {
    set module_name $opts(-module_name)
  }

  if {[info exists opts(-idv_in_name)]} {
    set idv_in_name $opts(-idv_in_name)
  }

  if {[info exists opts(-idv_out_name)]} {
    set idv_out_name $opts(-idv_out_name)
  }

  set inst_name_prefix "i_idvp_fublet"
  if {[info exists opts(-inst_name_prefix)]} {
    set inst_name_prefix $opts(-inst_name_prefix)
  }

  set chain {}
  array set par_chain {}
  set idx 0
  
  sv_for_file l $idv_spec_file {
    set cmd {}
    set l [string trim $l]
    if {$l == ""} {continue} 
    if {[regexp {^\#} $l]} { continue }

    # Insert fublets into the soft paritions
    if { [lindex $l 0] == "S" } {
      sv_lassign $l par_type par_name num_fublets idv_module
      set chain [concat $chain " $par_name"]
     
      set par_name_parent [::col::get_ip_hier_par [lindex [split $par_name /] 0]]
	set_current_component ${par_name_parent}/$par_name
      if { $idv_module != "" } {
	print_info "Inserting user specified fublet $idv_module inside the partition $par_name"
	set sub_chain [::col::dfx_insert_idvp_fublet $num_fublets $idv_module $idv_in_name $idv_out_name $inst_name_prefix]
      } else {
	set sub_chain [::col::dfx_insert_idvp_fublet $num_fublets $module_name $idv_in_name $idv_out_name $inst_name_prefix]
      }

      set par_chain($idx) [list [list [lindex $sub_chain 0] $idv_in_name] [list [lindex $sub_chain 1] $idv_out_name]]      
      incr idx
      set_current_component /
    } else {
      sv_lassign $l par_type par_name ifc_in ifc_out
      set chain [concat $chain " [::col::get_ip_hier_par $par_name]"]

      set component "[::col::get_ip_hier_par $par_name]/$par_name"
      set par_chain($idx) [list [list $component $ifc_in] [list $component $ifc_out]]
      incr idx
    }
  }


  set idx_minus_one [expr $idx - 1]
  for {set i 0} {$i < $idx_minus_one} {incr i} {
    sv_lassign [lindex $par_chain($i) 1] from_inst from_out_ifc
    sv_lassign [lindex $par_chain([expr $i + 1]) 0] to_inst to_in_ifc

    set common_ancestor [_collage_get_common_ancestor_name [list $from_inst/hpin $to_inst/hpin]]
    if { $common_ancestor != "" } {
      eval_in_component $common_ancestor {
	set from_inst [string range $from_inst [expr [string length $common_ancestor] + 1] [string length $from_inst]]
	set to_inst [string range $to_inst [expr [string length $common_ancestor] + 1] [string length $to_inst]]
	
	set cmd "connect_interface -from_component $from_inst -from_interface $from_out_ifc -to_component $to_inst -to_interface $to_in_ifc"
	#. Use hierarchy option only when IP is some level down
	  if { [regexp {\/} $from_inst] || [regexp {\/} $to_inst] } {
	    append cmd " -hierarchy"
	}

	print_info "Evaluating comamnd: $cmd"
	eval $cmd
      }
    } else {
      set cmd "connect_interface -hierarchy -from_component $from_inst -from_interface $from_out_ifc -to_component $to_inst -to_interface $to_in_ifc"
      print_info "Evaluating comamnd: $cmd"
      eval $cmd
    }
  }

  return $chain
}
define_proc_attributes collage_connect_idvp_chain \
    -info "Collage - " \
    -define_args {
      {"-idv_spec_file"      "File name"   "" string required}
      {"-module_name"        "Name of IDVP fublet module"   "" string optional}
      {"-idv_in_name"        "Name of IDV in interface"   "" string optional}
      {"-idv_out_name"       "Name of IDV out interface"   "" string optional}
      {"-inst_name_prefix"   "Prefix to use on insert fublet"   "" string optional}
    }

#################################################################
# Generate stubs (new procedure)
#################################################################
proc collage_gen_stubs {args} {
  parse_proc_arguments -args $args opts

  set out_dir $opts(-out_dir)

  set extension "v"; if {[info exists opts(-file_extension)]} { set extension $opts(-file_extension) }
  set ifc_only 0;    if {[info exists opts(-interface_only)]} { set ifc_only 1 }
  set depth_val 0;   if {[info exists opts(-depth)]}          { set depth_val $opts(-depth) }

  set suffix ""
  set stub_cells ""
  foreach v [_collage_get_depth_names $depth_val] {
    lappend stub_cells $v
    collage_gen_ip_stub $v $suffix $extension $out_dir $ifc_only 1
  }

}

define_proc_attributes collage_gen_stubs \
    -info "Collage - generate RTL stub files for designs" \
    -define_args {
      {"-out_dir"          "Output directory for stub files"       "" string required}
      {"-file_extension"   "RTL filename extenstion"               "" string optional}
      {"-interface_only"   "Write ports only (no output assigns)"  "" boolean optional}
      {"-format"           "Verilog or System Verilog"             "" string optional}
      {"-depth"             "Generate stub at specified depth"     "" string optional}
    }


###############################################################################
# Utility procedure to attach a clock req ack interface during assembly as well 
# as set the descriptor tags
###############################################################################
proc collage_add_cra_spec {args} {
  parse_proc_arguments -args $args opts

  #inst_name ifc_name c_pin r_pin a_pin {clk_type IOSFPRI_CLK} {max_f 400} {min_f 100}

  set inst_name $opts(-inst_name)
  
  set is_full_cra 0 ; # Full CRA interface - triplet is specified
  set is_clk_only 0 ; # Only clk pin is specified, no req and ack for this clock, 
                      # but we still want to use the clock automation

  if {[info exists opts(-cra_pins)]} {
    set is_full_cra 1
  }

  if {[info exists opts(-clk_pin)]} {
    set is_clk_only 1
  }

  if {[info exists opts(-ifc_name)]} {
    set ifc_name $opts(-ifc_name)
  } else {
    if {$is_full_cra} {
      error "Interface name is a required argument when specifying the full set of clk-req-ack triplet"
    }
  }



  if {!$is_full_cra && ! $is_clk_only} {
    error "One of -clk_pin or -cra_pins must be specified"
  }

  if {$is_full_cra} {
    sv_lassign $opts(-cra_pins) c_pin r_pin a_pin
  }

  if {$is_clk_only} {
    set c_pin $opts(-clk_pin)
  }

  set u_pin ""
  if {[info exists opts(-usync_pin)]} {
    set u_pin $opts(-usync_pin)
  }


  set clk_type $opts(-clk_type)

  set clk_min_freq 10
  if { [info exists opts(-clk_min_freq)] } {
    set clk_min_freq $opts(-clk_min_freq)
  }

  set clk_max_freq 200
  if { [info exists opts(-clk_max_freq)] } {
    set clk_max_freq $opts(-clk_max_freq)
  }
    
  set clk_type "FUNC_CLK"
  if { [info exists opts(-clk_type)] } {
    set clk_type $opts(-clk_type)
  }

  # only attach on reference instance
  if {[collage_is_mi_inst -name $inst_name] &&  [::col::is_readonly_inst  $inst_name  1] } {
      return
  }
  if {$is_full_cra} {
    set port_map "CLK_REQ $r_pin  CLK_ACK $a_pin"
    puts "CMD is collage_attach_interface -ifc_name $ifc_name -interface CLOCK_REQ_ACK -version 1.0 -component $inst_name -type consumer -port_map $port_map  -param_map {}"
    collage_attach_interface -ifc_name $ifc_name -interface CLOCK_REQ_ACK -version 1.0 -component $inst_name -type consumer -port_map $port_map  -param_map {}
  }

  collage_eval_in_component -use_hier_name $inst_name {
    print_info "Adding cra spec: Inst=$inst_name"
    collage_create_ip_clock -port $c_pin -clk_type $clk_type -clk_max_freq $clk_max_freq -clk_min_freq $clk_min_freq
    if {[info exists opts(-skip_pcg)]} {
      set_port_attribute $c_pin {GenericBooleanAttr[PcgSkip]} 1
    }
    if {$is_full_cra} {
      collage_set_descriptor_tag  -interface $ifc_name -descriptor_tag_name INTERFACE_CLKS -descriptor_tag_value $c_pin
    }
    if {$u_pin != ""} {
      collage_set_descriptor_tag  -port $c_pin -descriptor_tag_name USYNC_PIN -descriptor_tag_value $u_pin
    }
 }


}

define_proc_attributes collage_add_cra_spec \
    -info "Collage - instantiate clock req ack interface and set clock attributes (for during soc assembly)" \
    -define_args {
      {"-inst_name"   "Name of RTL instance"                             "" string required}
      {"-ifc_name"    "Name of interface instance"                             "" string optional}
      {"-cra_pins"    "Pin Name triplet: clock clock_req clock_ack"  "" string optional}
      {"-usync_pin"   "Pin Name: usync"  "" string optional}
      {"-clk_pin"     "Pin Name: clock (use when there is no req/ack, but only clock pin)"  "" string optional}
      {"-clk_min_freq"           "Clock Min Frequency in MHZ"   "10" string optional}
      {"-clk_max_freq"           "Clock Max Frequency in MHZ"   "200" string optional}
      {"-clk_type"                        "Clock Type"   "FUNC_CLK" one_of_string {optional {values {"FUNC_CLK" "DFX_CLK" "IOSFPRI_CLK" "IOSFSB_CLK"}}}}
      {"-skip_pcg"           "Skip this port for Partition Clock Gating"   "" boolean optional}
    }

#########################################################################
# A temporary utility to load in interface protocol to clock association
# 
# sv_collage_load_ifc_clocks /nfs/pdx/disks/hdk73.lghrtl.02/satish/glgh/sbx_3.0/config_soc-chs-a0.ltc/test_ifc_clock.txt 0.2
#########################################################################
proc sv_collage_load_ifc_clocks {fn int_delay_mult} {

  # FIXME: quick hack to force creation of all the obviate ifc objects
  foreach i [::obv_ip_inst::get_all] {eval $i get_interfaces}

  sv_for_file l $fn {
    set cmd {}
    set l [string trim $l]
    if {$l == ""} {continue} 
    if {[regexp {^\#} $l]} { continue }

    sv_lassign $l i_name ifc_name clock_name

    puts "Processing - $i_name : $ifc_name : $clock_name"
    set clock_name_lc [string tolower $clock_name]

    # Check if clock is valid
    if {[obv_soc_clock::get_all ::${clock_name}] == ""} {
      print_error "No such clock: $clock_name"
      continue
    }
    
    set clk_freq [eval $clock_name get_generated_freq]
    set clk_period [expr (1 / $clk_freq) * 1000.0] ; # freq in MHz, period in ns

    # Check if instance & interface are valid
    set inst_name [::col::get_ip_real_name $i_name]
    set inst_par [::col::get_ip_hier_par $inst_name]
    eval_in_component $inst_par {
      set ifc_obj [find_interface_instances -quiet -component $inst_name -name $ifc_name]
      if {[sizeof_collection $ifc_obj] == 0} {
	# The instance does not exist
	print_error "No such interface instance: $i_name $ifc_name"
      }
    }
    
    # construct the obviate interface object name (should really be a lookup - FIXME)
    set obv_ip_obj [obviate get_ip_obj ${inst_name}]
    set ip_hier_name [$obv_ip_obj get_full_hier_name]
    set obv_ifc_obj ofi_${ip_hier_name}/${ifc_name}
   
    
    foreach "arch_pin rtl_pin" [eval $obv_ifc_obj get_ports] {
      if {[string equal -nocase $rtl_pin "<open>"] || $rtl_pin == ""} {
	continue
      }
      #set cmd "$obv_ip_obj set_io_pin_delay $rtl_pin $clock_name_lc  [expr $clk_period * $int_delay_mult]"
      foreach rtl_bit_blast_pin [obviate bit_blast $rtl_pin] { 
	  set cmd "$obv_ip_obj set_io_pin_delay $rtl_bit_blast_pin $clock_name_lc  [expr $clk_period * $int_delay_mult * 1000.0]" ; # obv in ps
        puts "Evaluating: $cmd"
        eval $cmd
      }
    }
  }

}

proc collage_message_unsuppress { code_list } {
  global ::collage_msg_handler::msg_array  
  global ::collage_msg_handler::suppress_array
  global ::collage_msg_handler::fp
  
  foreach code $code_list {
    if { ! [info exists ::collage_msg_handler::msg_array($code)] } {
      puts "-I-: \[$code\] is not recognized. wrong code to unsuppress."    
      continue
    }
    set ::collage_msg_handler::suppress_array($code) 0 
    puts "-I-: message $code unsupressed"
  }
}


# ------------------------------------------------------------------------
# fix bug (original source of this proc - /p/com/eda/intel/collage/3.7/comps/packages/upf/upf.tcl)
# ------------------------------------------------------------------------
proc ::col::my_get_par_ips {par_name {ismodule 0}} {
  set ret_val {}
  set par_name1 $par_name
  if {$ismodule} {
    set par_name [::col::get_ref_inst $par_name]
  }
  set par_hier_name [collage_get_hier_name -ip_name $par_name]
  set ip_names [list]
  

  if {[string equal $par_hier_name $par_name]} {
    if {[sizeof_collection [find_item -quiet -type cell $par_name]] == 0} {
      return $ip_names
    }
  }


  #Soma temporary workaround until update_hier is fixed to return existing components 
  # Workaround starts
  eval_in_component /${par_hier_name} {
    if {[get_attribute -attrs IsHierarchicalComponent [current_design -quiet ]]} {
      foreach_in_collection ip [find_item -quiet -type cell] {
	set ip_name [get_attribute -attrs "Name" $ip];
	set comp_name [get_current_component];
	lappend ip_names [collage_get_hier_to_flat_name -hier_name $comp_name/$ip_name]
      }
    }
  }

  return ${ip_names}
  # Workaround ends
  foreach ip [array names ::col::ip_hier] {
    if {[string equal -nocase $::col::ip_hier($ip) $par_name]} {
      set ret_val [concat $ret_val $ip]
    }
  }
  return $ret_val
}

proc collage_tag_sbifc_pok_tieoff {args} {
  parse_proc_arguments -args $args opts

  set inst_name $opts(-inst_name)
  set ifc_name $opts(-ifc_name)
  set pname    $opts(-pname)

  eval_in_component [collage_get_hier_name -ip_name $inst_name] {
    # FIXME
    # check anc make sure that the portname is indeed a port name on
      # the ip instance
    #collage_set_descriptor_tag -port $pname descriptor_tag_name POK_TIEOFF -descriptor_tag_value 1
    collage_set_descriptor_tag  -interface $ifc_name    -descriptor_tag_name POK_TIEOFF -descriptor_tag_value $pname
  }
}

define_proc_attributes collage_tag_sbifc_pok_tieoff \
   -info "Collage - associate a IOSF SB/PRIMARY interface  with its corresponding pok  port " \
    -define_args {
      {"-inst_name"   "Name of RTL instance"                             "" string required}
      {"-ifc_name"    "Name of interface instance"                       "" string required}
      {"-pname"       "name of the pok port"  "" string required}
}


###############################################################################
# workaround proc to update the pars hierarchy when wrappers are added
###############################################################################
proc soc_collage_update_wrap_db {} {
  #::soc_wrapper_spec
}


##. Override to handle cases where repeaters are inserted between routers
proc _collage_sbr_is_router2router_link {port} {

  set router2router [_collage_sbr_param_val "P${port}ROUTER2ROUTER"]
  
  #if router2router is set, then we know that the router is intended to be connected
  #to another router.  
  #TODO:  does it matter whether it actually is connected?
  
  if {$router2router} {
   # print_info "::collage_sbr::is_router2router_link detected a router2router link (agent side) for port $port"
    return 1
  }
  
  set connection [_collage_sbr_get_port_connection $port $router2router]
  
  if {$connection == ""} {
    #if agent is empty, this means that there is nothing connected to this particular port
    #print_info "::collage_sbr::is_router2router_link detected an agent(empty) link for port $port"
    return 0
  }
  
  ##. Port it back
  set component [get_attribute -attrs ComponentOfItem $connection]
  set d_name [set d_unelab_name ""]
  set is_locked_sbr 0
  eval_in_component $component {
    set cur_d [current_design -quiet]
    sv_lassign [get_attribute -attrs "Name UnelabName"  $cur_d] d_name d_unelab_name
    set is_locked_sbr [get_attribute -attrs GenericBooleanAttr -subscript Sbr_IsLocked $cur_d]
  }
  
  set cur_comp_is_rpt 0
  if {$d_unelab_name == "sbr_rpt"} {set cur_comp_is_rpt 1}
  
  while {$cur_comp_is_rpt} {
    set direction Consumers
    set ifc_name rpt_agt_side

    set ifc [find_interface_instances -component $component -name $ifc_name -quiet]
    set connection [get_attribute $ifc -attr TargetConnection -subscript ${direction}]
    
    if {$connection == ""} {return 0}
    
    set component [get_attribute -attrs ComponentOfItem $connection]
    eval_in_component $component {
      set cur_d [current_design -quiet]
      sv_lassign [get_attribute -attrs "Name UnelabName"  $cur_d] d_name d_unelab_name
      set is_locked_sbr [get_attribute -attrs GenericBooleanAttr -subscript Sbr_IsLocked $cur_d]
    }

    if {$d_unelab_name != "sbr_rpt"} {set cur_comp_is_rpt 0}
  }

  if {$d_name == $::collage_tb::sbr_config_mod_name || $d_unelab_name == $::collage_tb::sbr_config_mod_name || $is_locked_sbr} {
    #print_info "::collage_sbr::is_router2router_link detected a router2router link (fabric side) for port $port"
    return 1
  } else {
    #print_info "::collage_sbr::is_router2router_link detected an agent link for port $port"
    return 0
  }
}


::itcl::body ::obv_ip_inst::set_interfaces {{ifc_names ""}} {
  if {![llength $ifc_names]} {
    set d_ip_inst_data(interfaces) 1
    eval_in_component $d_obj {
      foreach_in_collection ii [find_item -quiet -type "InterfaceInstance"] {
	set name [get_attribute -attrs "UserName" $ii]
	lappend d_interfaces [obv_ifc_inst ::ofi_${name} $ii $this]
      }
    }
  } else {
    set d_interfaces ${ifc_names}
  }
}



###############################################################################
# kbpatil : Proc fix for MI
###############################################################################
namespace eval ::visa {}

proc ::visa::write_ip_interface_file { visa_interface_file } {

  set top_design_name [get_attribute -attrs Name [current_design -quiet]]

  set fp [open $visa_interface_file "w"]
  puts $fp "array unset ip_visa_interface"
  puts $fp "array set ip_visa_interface {}"
  puts $fp "set ip_visa_interface(TOP_DESIGN_NAME) $top_design_name"
  foreach leaf [collage_get_leaf_ips] {
    if { [regexp {plm|clm} $leaf] } { continue }

    puts $fp "set ip_visa_interface($leaf,HIERARCHY) /$top_design_name/[collage_get_hier_name -ip_name $leaf]"
    collage_eval_in_component -edit_mi -use_hier_par $leaf {
      set leaf [collage_get_folded_name -name $leaf]
      
      set visa_interfaces [list]
      foreach_in_collection ifc [find_interface_instances -sort Name -component $leaf -filter "InterfaceDefinitionName==IOSF::DFX::VISA && InterfaceType==consumer"] {
        set interface_name [get_attribute $ifc -attr Name]
        set lane_width [get_interface_parameter_attribute -instance $leaf/$interface_name VISA_LANE_WIDTH Value]
        
        lappend visa_interfaces [list $interface_name $lane_width]
      }
      
      puts $fp "set ip_visa_interface($leaf,IOSF::DFX::VISA) \{[lsort -decreasing -integer -index 1 $visa_interfaces]\}"

      
      set visa_cfg_interfaces [list]
      foreach_in_collection cfg_ifc [find_interface_instances -sort Name -component $leaf -filter "InterfaceDefinitionName==IOSF::DFX::VISA_CFG && InterfaceType==consumer"] {
        set interface_name [get_attribute $cfg_ifc -attr Name]
        
        lappend visa_cfg_interfaces $interface_name
      }

      puts $fp "set ip_visa_interface($leaf,IOSF::DFX::VISA_CFG) \{$visa_cfg_interfaces\}"
    }
  }
  
  close $fp
}


############################################################################
# collage_mark_tie_high_ifc  IOSF::DFX::SCAN consumer "FSCAN_BYPRST_B FSCAN_LATCHCLOSED_B FSCAN_BYPLATRST_B"
############################################################################
proc collage_mark_tie_high_ifc {intfName type signallist} {
  collage_eval_in_comp -use_hier_par -edit_mi [collage_get_leaf_ips ] {
    foreach_in_collection intf [ find_item -quiet -type interfaceInstance -filter "InterfaceDefinitionName==$intfName && InterfaceType==$type"] {
      foreach sig $signallist {
        set port [get_children -name $sig $intf]
        if {$port != ""} {
          set rtl_port [get_interface_port_attribute $intf $sig InterfaceLink]
	  if {$rtl_port != "" && $rtl_port != "<open>"} {
	    #print_info "MARK tie high : [get_attribute -attr ComponentUserName $intf] $sig: $rtl_port"
	    set comp [get_attribute -attr ComponentOfItem $intf]
	    eval_in_component $comp {
	      if {![regexp {\[} $rtl_port]} {
		print_info "Auto mark tie high on - $comp set_attribute $rtl_port -attr IfUnconnected -value one"
		set_attribute [find_item -type port $rtl_port] -attr IfUnconnected -value one
	      } else {
		print_info "SKIPPING auto mark tie high on bussed port $rtl_port"
	      }
	    }
	  }
        }
      }
    }
  }
}


define_proc_attributes collage_process_partition_file \
    -info "Collage - process SOC hierarchy file with MI support" \
    -define_args {
      {"-file"         "Partition file(s)"   "" string required}
      {"-debug"         "debug info"   "" boolean optional}
      {"-dont_create"         "debug info"   "" boolean optional}
      {"-skip_exist_check"         "debug info"   "" boolean optional}
    }


# -----------------------------------------------------------------------
# Fix for error during multiply instantiated design
# -----------------------------------------------------------------------
proc _collage_get_depth_names {depth_val {depth_val_t 0}} {

  if {$depth_val_t == $depth_val} {
    set depth_names [list]
    foreach_in_collection c [find_item -quiet -type cell] {
      set curr_hier [get_current_component]
      set comp_name [get_attribute -attrs UserName $c]
      set cname [collage_get_hier_to_flat_name -hier_name $curr_hier/$comp_name]
      lappend depth_names $cname
    }
    return $depth_names
  } else {
    set depth_val_t [expr $depth_val_t+1]
    set depth_names [list]
    foreach_in_collection c [find_item -quiet -type cell] {
      set is_hier [get_attribute -attrs IsHierarchicalComponent [get_attribute -attrs ReferenceDesign $c]]
      if {$is_hier} {
        eval_in_component $c {
          set values [_collage_get_depth_names $depth_val $depth_val_t]
          if {$values ne ""} {
            foreach v $values {
              lappend depth_names $v
            }
          }
        }
      }
    }
    return $depth_names
  }
}

# -----------------------------------------------------------------------
# 
# -----------------------------------------------------------------------
proc collage_report_empty_hiers {args} {
  parse_proc_arguments -args $args opts

  set out_fn $opts(-rep_fn)

  set out_fh [open $out_fn w]
  puts $out_fh "##########################################################"
  puts $out_fh "# Hierarchies without any content (no instances)          "
  puts $out_fh "##########################################################"

  set curd [current_design -quiet]
  set num 0
  set total 0
  foreach_in_collection d [filter_collection [all_designs] -filter "IsHierarchicalComponent==1"] {
    incr total
    current_design -quiet $d
    if {[sizeof_collection [find_item -quiet -type cell]] == 0} {
      puts $out_fh [get_attribute -attrs UserName $d]
      incr num
    }
    current_design -quiet $curd
  }

  puts $out_fh ""
  puts $out_fh "# Number of empty hiers: $num  out of $total"
  close $out_fh
}

define_proc_attributes collage_report_empty_hiers \
    -info "Collage - report hierarchical components with nohing instantiated under them" \
    -define_args {
      {"-rep_fn"   "Name of report file"                             "" string required}
    }




# ----------------------------------------------------
# Merge Ports 
# ----------------------------------------------------
proc ::col::get_folded_pname {pname} {
  if {![string match -nocase "*/*" $pname ]} {
    return $pname
  }
  sv_lassign [split $pname "/"] inst_name pin_name
  return "[::col::get_folded_name $inst_name]/$pin_name"
}

proc _collage_merge_port_skip_list {skip_ifc_port skip_adhoc_pin} {
  array set skip_port_list ""
  if {$skip_ifc_port != 0} {
    foreach_in_collection p [find_item -quiet -type port *] {
      set ifc_def_name [get_attribute $p -attr InterfaceDefnName]
      if {$ifc_def_name != ""} {
	if {$skip_ifc_port == 1 || [regexp $skip_ifc_port $ifc_def_name]} {
	  set skip_port_list([get_attribute -attr UserName $p]) 1
	}
      }
    }
  } else {
    # foreach_in_collection p [find_item -quiet -type pin *] { 
    #   if {[get_attribute $p -attr InterfaceDefnName] == ""} {
    # 	continue
    #   }    
      
    #   lassign [get_attribute $p -attr {HasCnctChildren StartBit}] chcnt startbit
    #   if {$chcnt > 0 || $startbit != ""} {
    # 	set_attribute  $p -attr BlastBus -value 1
    #   }    
    #   set pin_bits [get_attribute $p -attr Children]
    #   if {$pin_bits != {} } {
    # 	set skip_port_list([get_attribute -attr UserName $p]) 1
    #   }    
    # }
  }
  set ports [find_item -quiet -type port -filter "GenericBooleanAttr\[$::col_attr::user_port\]==true"]
  foreach_in_collection port $ports {
    set skip_port_list([get_attribute -attr UserName $port]) 1
  }
  set exclude_list [concat [join [array names skip_port_list] " "] $skip_adhoc_pin]
  print_info "CALLING merge_ports at [get_current_component]"
  merge_ports -ignore_attrs -exclude $exclude_list
}


proc _collage_merge_ports_dfs {cell {skip_ifc_port 0} {skip_adhoc_pin ""}} {

  if {[::col::is_readonly_inst $cell 1]} {
    return
  }
  set hier ""
  set_current_component -quiet
  collage_eval_in_component -use_hier_name "$cell" {
    set hier [get_current_component]
    set isHier [get_attribute -attrs IsHierarchicalComponent [current_design -quiet]]
  }	
  if {!$isHier} {
    return
  }	
  print_info "MERGE cell DFS $cell hier $hier"
  set_current_component -quiet "/$hier"
  foreach_in_collection c [find_item -quiet -type cell ] {
      if {[get_attribute -attrs IsHierarchicalComponent [get_attribute -attr ReferenceDesign $c]]} {
	  set hier_cell_name "$hier/[get_attribute -attr Name $c]"
	  set inst_name [::col::get_hier_to_flat_name $hier_cell_name]
	  _collage_merge_ports_dfs $inst_name $skip_ifc_port $skip_adhoc_pin
      }
  }
  set_current_component -quiet "/$hier"
  _collage_merge_port_skip_list $skip_ifc_port $skip_adhoc_pin
  set_current_component -quiet
}

proc _collage_merge_ports_bfs {cell {skip_ifc_port 0} {skip_adhoc_pin ""}} {
  set inst_list [list]
  lappend inst_list $cell
  while {[llength $inst_list] > 0} {
    set new_list ""
    foreach c $inst_list {
      if {[::col::is_readonly_inst $c 1]} {
        continue
      }	
      set hier ""
      collage_eval_in_component -use_hier_name $c {
        set hier [get_current_component]
        set isHier [get_attribute -attrs IsHierarchicalComponent [current_design -quiet]]
      }	
      if {!$isHier} {
        continue
      }	
      set_current_component -quiet $hier
  print_info "MERGE cell BFS $cell hier $hier"
      _collage_merge_port_skip_list $skip_ifc_port $skip_adhoc_pin
      foreach_in_collection nextcell [find_item -quiet -type cell ] {
        set hier_cell_name "$hier/[get_attribute -attr Name $nextcell]"
        set inst_name [::col::get_hier_to_flat_name $hier_cell_name]
        lappend new_list $inst_name
      }	
      set_current_component -quiet
    }
    set inst_list $new_list
  }
  set_current_component  -quiet
}
proc _collage_merge_ports_legacy {cell {skip_ifc_port 0} {skip_adhoc_pin ""}} {
  set_current_component $cell
  foreach  c [all_components -exclude_nonhier] {
    _collage_merge_ports_legacy $c $skip_ifc_port $skip_adhoc_pin
  }
  array set skip_port_list ""
  if {$skip_ifc_port} {
    foreach_in_collection p [find_item -quiet -type port *] {
      if {[get_attribute $p -attr InterfaceDefnName] != ""} {
        set skip_port_list([get_attribute -attr UserName $p]) 1
      }
    }
  } else {
    foreach_in_collection p [find_item -quiet -type pin *] {
      if {[get_attribute $p -attr InterfaceDefnName] == ""} {
        continue
      }

      set chcnt [get_attribute $p -attr HasCnctChildren]
      if {$chcnt > 0} {
        set_attribute  $p -attr BlastBus -value 1
      }
      set pin_bits [get_attribute $p -attr Children]
      if {$pin_bits == {} } {
        continue
      }
      while {$pin_bits != {}} {
        set new_children {}
        foreach_in_collection upin_bit $pin_bits {
          set upin_sub_bits [get_attribute $upin_bit -attr Children]
          if { $upin_sub_bits != {} } {
            #. Dive down
            append_to_collection new_children $upin_sub_bits
          } else {
            foreach_in_collection port [get_connections $upin_bit] {
              if {[get_attribute -attr TypeName $port] == "port"} {
                set skip_port_list([get_attribute -attr UserName $port]) 1
              }
            }
          }
        }
        set pin_bits $new_children
      }
    }
  }
  set ports [find_item -quiet -type port -filter "GenericBooleanAttr\[$::col_attr::user_port\]==true"]
  foreach_in_collection port $ports {
    set skip_port_list([get_attribute -attr UserName $port]) 1
  }
  set exclude_list [concat [join [array names skip_port_list] " "] $skip_adhoc_pin]
  merge_ports -ignore_attrs -exclude $exclude_list
  set_current_component ..
}

proc collage_merge_ports {args} {
  parse_proc_arguments -args $args opts
  set cell ""
  set skip_ifc_port 0
  set skip_adhoc_pin ""
  if {[info exists opts(-cell)]} {
    set cell $opts(-cell)
    if {[llength [split $cell "/"]] > 1} {
        collage_message_print CRT-030 "-cell $cell should be instance name not hierarchy"
    }
  }
  if {[info exists opts(-skip_ifc_port)]} {
    set skip_ifc_port $opts(-skip_ifc_port)
  }
  if {[info exists opts(-skip_adhoc_pin_list)]} {
    set skip_adhoc_pin $opts(-skip_adhoc_pin_list)
  }
  set skip_adhoc_pin_folded ""
  foreach pin $skip_adhoc_pin {
    lappend skip_adhoc_pin_folded [::col::get_folded_pname $pin]
  }	
  if {[info exists opts(-nolegacy)]} {
    _collage_merge_ports_bfs $cell $skip_ifc_port $skip_adhoc_pin_folded
    _collage_merge_ports_dfs $cell $skip_ifc_port $skip_adhoc_pin_folded
  } else {
    _collage_merge_ports_legacy $cell $skip_ifc_port $skip_adhoc_pin_folded
  }	
  set_current_component -quiet
}



define_proc_attributes collage_merge_ports \
    -info "Collage - merge duplicate ports, can only be used with coretools K version and newer!!!" \
    -define_args {
      {"-cell"        "cell name"     ""      string         optional}
      {"-nolegacy"        "Old algorithm for backward compatibility"     ""      boolean         {hidden optional}}
      {"-skip_ifc_port"        "Regular expression of interface def names to skip rtl ports if linked to these"     ""      string         optional}
      {"-skip_adhoc_pin_list"        "Skip adhoc rtl pins"     ""      string         optional}
    }

# --------------------------------------------------------------------------------
# ADDED by ADITI: Needs to be removed once 3.18 collage release is available
# --------------------------------------------------------------------------------

proc _collage_rename_ports_on_hier {pin newname pars input_only debug_mode} {
  set cmds ""
  foreach {par_ports leaf_pins} [collage_get_par_ports_of_pin $pin] {
    foreach par_port $par_ports {
      lassign [split $par_port "/"] p par_clk_port_name
      if {[lsearch -exact $pars $p] == -1} {
		puts "Skipping hierarchy $p"
		continue
      }
      foreach {par_clock_port_pin is_bus} [_collage_split_pin $par_clk_port_name] {}
      if {$is_bus} {
	#append par_clk_port_name "\[${par_clk_port_bit}\]"
      }
      if {($par_clock_port_pin != "") && ($par_clock_port_pin != $newname) && ($par_clock_port_pin != "logic.zero") && ($par_clock_port_pin != "logic.one")} {
		collage_eval_in_component -edit_mi -use_hier_name $p {
		  set port_ref [find_item -quiet -type port $par_clock_port_pin]
		  set new_port_ref [find_item -quiet -type port $newname]
			  set pdir "unknown"
		  if {[sizeof_collection $port_ref] == 1} {
			set pdir [get_attribute -attrs PortDirection $port_ref]
		  }
		  if {$input_only && $pdir!="in"} {
			echo "skipping $p $par_clock_port_pin with direction $pdir"
		  } else {
			# Check that newname doesn't already exist
			if {[sizeof_collection $new_port_ref] != 0} {
			  print_warning "Skipping rename - from $par_clock_port_pin - to $newname on $p - as $newname already exists"
			} else {
			  if {![info exists rename_port($p/$newname)]} { 
					lappend cmds "_collage_process_rename_port $p $par_clock_port_pin $newname"
					set rename_port($p/$newname) $par_clock_port_pin
				  } else {
					print_warning "Skipping rename - from $par_clock_port_pin - to $newname on $p - as $newname is already used for renaming port $rename_port($p/$newname) "
			  }
			}
		  }
		}  
      }
    }
  }
  return $cmds
}  

proc _collage_rename_ports_from_pin { pin newname {addtl_hiers ""} {input_only 0} {debug_mode 0}} {
  set pars "[_soc_get_pars] $addtl_hiers"
  puts "Processing pin $pin" 
  set cmds [_collage_rename_ports_on_hier $pin $newname $pars $input_only $debug_mode]
  foreach cmd $cmds {
    if {$debug_mode} {
      echo $cmd
    } else {
      echo "Evaluating: $cmd"
      eval $cmd
    }
  }
}
proc _collage_rename_all_hier_ports_from_pin { pin newname  {input_only 0} {debug_mode 0}} {
  print_info "Rename all hier ports: $pin $newname $input_only $debug_mode"
  set pars "[collage_get_par_names]"
  set cmds [_collage_rename_ports_on_hier $pin $newname $pars $input_only $debug_mode]

  foreach cmd $cmds {
    if {$debug_mode} {
      echo $cmd
    } else {
      echo "Evaluating: $cmd"
      eval $cmd
    }
  }

}

proc _collage_rename_port_from_pin { pin newname context} {
  set portinfo [_collage_get_port_name  $pin $context]
  if {$portinfo == ""} {
    collage_message_print CRT-061 " $pin in context $context"
    return
  }
  lassign $portinfo portname bits
    collage_message_print CRT-001 " rename_port $context $portname $newname"
  _collage_process_rename_port $context $portname $newname
}

proc _collage_get_port_name {pin context} {
  if [ string match -nocase "*/*" $pin ] {
    sv_lassign [split $pin "/"] inst pname
  } else {
    collage_message_print CRT-062 " $pin"
    return
  }	
  set retlist ""
  if {$context != "" && [::col::isa_par $context] == 0} {
    print_info "context $context needs to be hierarchy cann't work on ip"
    return  
  }	
  set isBus 0
  set inst [collage_get_ip_real_name -ip_name $inst]
  set pin [collage_get_folded_name -name $inst]/$pname
  set same_hier 0
  set bits ""
  if { [regexp {\[} $pname] } {
    set isBus 1
    set elements [regexp  -inline {(\S+)\[(\S+)\]} $pname] 
    set pname [lindex $elements 1]
    set bits [lindex $elements 2]
  }	
  set hierpath "[::col::get_ip_hier_par $inst 1 0 0]/$inst"
  if {$context == ""} {
    set same_hier 2
  } elseif {[lsearch [split $hierpath "/"] $context] >= 0} {
    set same_hier 1
  }

  lappend inst_list [list $inst $pname $bits]
  set ct 0
  array set pinsvisted {}
  array set pinsdone {}
  while {[llength $inst_list] > 0} {
    if {$ct > 40} {
      collage_message_print CRT-061 " $pin in hierarchy $context"
      return ""
    }
    incr ct
    set newlist ""
    foreach inst_array $inst_list {
      lassign $inst_array inst pname bits
      set folded_inst [collage_get_folded_name -name $inst]
      set pintotrace [_collage_find_pin_to_trace $inst $pname $bits]
      set pinsvisted($folded_inst/$pname) 1
      if {$pintotrace == ""} {
        continue
      }
      collage_eval_in_comp -use_hier_par $inst {
        if {$same_hier == 2} {
            set connstotrace [get_connections $pintotrace]
            append_to_collection connstotrace [get_connections -hier $pintotrace]
        } elseif {$same_hier == 1} {
            set connstotrace [get_connections $pintotrace]
        } else {
            set connstotrace [get_connections -hier $pintotrace]
        }
      }
      set hierpathinst "[::col::get_ip_hier_par $inst 1 0 0]/$inst"

      #print_info "inst_array $inst_array  [sizeof_collection $connstotrace] ct $ct same_hier $same_hier" 
      foreach_in_collection conn $connstotrace {
        set pinUserName [get_attribute -attr UserName $conn]
	#puts "checking $pinUserName"
        set pinbits ""
        if { [regexp {\[} $pinUserName] } {
          set elements [regexp  -inline {(\S+)\[(\S+)\]} $pinUserName] 
          set pinUserName [lindex $elements 1]
          set pinbits [lindex $elements 2]
        }	
        if {![info exists pinsvisted($pinUserName)]} {
          if {[string match -nocase "*/*" $pinUserName ]} {
            sv_lassign [split $pinUserName "/"] inst_name pin_name
            set insttemp [::col::col_get_component_name $conn]
            set instnew [::col::get_hier_to_flat_name $insttemp/$inst_name]
            set pnamenew [lindex [split $pinUserName "/"] end]
          } else {
            set pnamenew $pinUserName
            set instnew [::col::col_get_component_name $conn 1 1]
          }
          
          if {$instnew == ""} {
            continue 
          }	
	  if {$context == ""} {
	    if {$same_hier == 1 && [lsearch [split $hierpathinst "/"] $instnew] < 0} {
	      continue
	    }
	    if {[::col::isa_par $instnew] != 0 && ![info exists pinsdone(${pinUserName},$pinbits,$instnew)]} {
	      lappend retlist [list ${pinUserName} $pinbits $instnew]
	      set pinsdone(${pinUserName},$pinbits,$instnew) 1
	    }
	  } elseif {$instnew == $context} {
	    return [list ${pinUserName} $pinbits]
	  }
	  set hierpath "[::col::get_ip_hier_par $instnew 1 0 0]/$instnew"
	  if {$context != "" && [lsearch [split $hierpath "/"] $context]  < 0} {
	    continue
	  }
	  lappend newlist [list $instnew $pnamenew $pinbits]
        }
      }
    }
    set same_hier 1
    set inst_list $newlist
  }
  return $retlist
}

proc _collage_find_pin_to_trace {inst pname bits} {
  if {$pname == "logic.zero" || $pname == "logic.one"} {
    return
  }
  collage_eval_in_component -use_hier_par $inst {
    set p [find_item -type pin -quiet [collage_get_folded_name -name $inst]/$pname] 
    set pintotrace ""  
    if {$p == ""} {
      collage_message_print CRT-015 " $inst/$pname"
    } else {
    lassign [get_attribute $p -attr {HasCnctChildren StartBit}] chcnt startbit
    if {$chcnt > 0 || $startbit != ""} {
      set_attribute  $p -attr BlastBus -value 1
    }

    set children [get_children $p]
    if {$children != ""} {
      if {$bits == ""} {
        set pintotrace [index_collection $children 0]
      } else {
        set pintotrace [get_children $p -name $bits]
      }	
    } else {
      set pintotrace $p
    }	
  }	 
  }
# set c [get_attribute $p -attr "ElectricalConnections"]
# set port [get_attribute $c -attrs "ConnectedPinNames"]
# set port [get_attribute $c -attrs "ConnectedPortNames"]

  return $pintotrace
}
proc ::col::col_get_component_name {item {inst_name 0} {is_flat 0}} {
  if {[compare_versions $::sh_product_version "K-2015.06-SP7-3"] >= 1 || [compare_versions $::sh_product_version "L-2016.09-1"] >= 1} {
    set hier_name [get_current_component -name $item -unfolded]
  } else {
    if {[get_attribute -attr TypeName $item] == "interfaceInstance"} {
      set hier_name "/[regsub -all "%" [get_attribute $item -attr InterfaceComponent] "/"]"
    } else {
      set hier_name [get_component_name $item]
    }
  }	
  if {$hier_name == "/" || $hier_name == ""} {
    return ""
  }	
  if {$inst_name} {
    if {$is_flat} {
      return [::col::get_hier_to_flat_name $hier_name]
    } else {
      return [lindex [split $hier_name "/"] end]
    }	
  } else {
    return $hier_name
  }
}


# ----------------------------------------------------------------------------------
# Proc patched for conn_name_tag update
# ----------------------------------------------------------------------------------
#if {[file exists $::env(MODEL_ROOT)/collage/patches/fii_patch.tbc] && [ compare_versions $::sh_product_version K-2015.06-SP7] >= 1 && [compare_versions $::sh_product_version L-2016.09] <= 0 && $shell_activity_mode == "assembler"} {
 # puts "VIDHAN - sourcing $::env(MODEL_ROOT)/collage/patches/fii_patch.tbc"
 #  source $::env(MODEL_ROOT)/collage/patches/fii_patch.tbc
#}

proc collage_get_objects_with_descriptor_tag {args} {
  parse_proc_arguments -args $args opts

  set descriptor_tag_id $opts(-descriptor_tag_name)
  set object_type $opts(-object_type)

  set value 1
  if { [info exists opts(-descriptor_tag_value)] } {
    set value $opts(-descriptor_tag_value)
  }

  set flat_inst_tag 0
  if { [info exists opts(-flat_inst_tag)] } {
    set flat_inst_tag 1
  }

  set objects [list]
  if { $::shell_activity_mode != "builder" } {
    set leafs [collage_get_leaf_ips]
    if { [info exists opts(-component)] } {
      set leafs  $opts(-component)
    }
    
    foreach leaf $leafs {
      collage_eval_in_component -use_hier_name  $leaf {
        if { $object_type == "design" } {
          foreach clk [get_attribute [current_design -quiet] -attr GenericAttr -subscript $descriptor_tag_id] {
	    lappend objects $clk
	  }
	}
	
	if { $object_type == "port" } {
	  foreach_in_collection port [find_item -quiet -type port -sort Name -filter "GenericAttr\[$descriptor_tag_id\]!=0" ] {
	    lappend objects [list $leaf/[get_attribute $port -attr Name] [get_attribute $port -attr GenericAttr -subscript $descriptor_tag_id]]
	  }
	}
	
	if { $object_type == "param" } {
	  foreach_in_collection param [find_item -quiet -type param -sort Name -filter "GenericAttr\[$descriptor_tag_id\]!=0" ] {
	    lappend objects [list $leaf/[get_attribute $param -attr Name] [get_attribute $param -attr GenericAttr -subscript $descriptor_tag_id]]
	  }
	}
	
	if { $object_type == "interface" } {
          #fix for descriptor_tag_id
          if {$flat_inst_tag} {
            set new_descriptor_tag_id [collage_get_new_tag_id $leaf $descriptor_tag_id]
	    foreach_in_collection interface [find_interface_instances -sort Name -filter "GenericAttr\[$new_descriptor_tag_id\]!=0"] {
	      lappend objects [list $leaf/[get_attribute $interface -attr Name] [get_attribute $interface -attr GenericAttr -subscript $new_descriptor_tag_id]]
	    }
          } else {
	    foreach_in_collection interface [find_interface_instances -sort Name -filter "GenericAttr\[$descriptor_tag_id\]!=0"] {
	      lappend objects [list $leaf/[get_attribute $interface -attr Name] [get_attribute $interface -attr GenericAttr -subscript $descriptor_tag_id]]
	    }

          }
	}
      }
    }
  } else {
    set design [current_design -quiet]
    set leaf [get_attribute $design -attr Name]

    if { $object_type == "design" } {
      foreach clk [get_attribute [current_design -quiet] -attr GenericAttr -subscript $descriptor_tag_id] {
	lappend objects $clk
      }
    }
    
    if { $object_type == "port" } {
      foreach_in_collection port [find_item -quiet -type port -sort Name -filter "GenericAttr\[$descriptor_tag_id\]!=0" ] {
	lappend objects [list $leaf/[get_attribute $port -attr Name] [get_attribute $port -attr GenericAttr -subscript $descriptor_tag_id]]
      }
    }
	
    if { $object_type == "param" } {
      foreach_in_collection param [find_item -quiet -type param -sort Name -filter "GenericAttr\[$descriptor_tag_id\]!=0" ] {
	lappend objects [list $leaf/[get_attribute $param -attr Name] [get_attribute $param -attr GenericAttr -subscript $descriptor_tag_id]]
      }
    }
    
    if { $object_type == "interface" } {
      foreach_in_collection interface [find_interface_instances -sort Name -filter "GenericAttr\[$descriptor_tag_id\]!=0"] {
	lappend objects [list $leaf/[get_attribute $interface -attr Name] [get_attribute $interface -attr GenericAttr -subscript $descriptor_tag_id]]
      }
    }
  }

  return $objects
}

define_proc_attributes collage_get_objects_with_descriptor_tag \
    -info "Collage - Get objects tagged with a given description tag" \
    -define_args {
      {"-component"          "Name of the component(s). Default is all leaf components"  "" string optional}
      {"-object_type"        "Type of object"   "" string required}
      {"-descriptor_tag_name"        "Identifier / Name of descriptor_tag"   "" string required}
      {"-flat_inst_tag"          "Provide tagging based on flat instance name"  "" boolean optional}
    }

proc collage_set_descriptor_tag {args} {
  parse_proc_arguments -args $args opts

  set descriptor_tag_id $opts(-descriptor_tag_name)
  set value 1
  if { [info exists opts(-descriptor_tag_value)] } {
    set value $opts(-descriptor_tag_value)
  }

  if { ![::collage_descriptor_tag::_is_descriptor_tag_registered $descriptor_tag_id] } {
    error "Descriptor TAG $descriptor_tag_id is not registered"
  }


  if { [info exists opts(-port)] } {
    set port [find_item -quiet -type port $opts(-port)]
    if { [sizeof_collection $port] == 0 } {
      error "Invalid port $opts(-port) specified for tagging"
    }

    set_attribute $port -attr GenericAttr -subscript $descriptor_tag_id -value $value
  }

  if { [info exists opts(-param)] } {
    set param [find_item -quiet -type param $opts(-param)]
    if { [sizeof_collection $param] == 0 } {
      error "Invalid param $opts(-param) specified for tagging"
    }

    set_attribute $param -attr GenericAttr -subscript $descriptor_tag_id -value $value
  }

  if { [info exists opts(-interface)] } {
    set interface [find_interface_instances -quiet -name $opts(-interface)]
    if { [sizeof_collection $interface] == 0 } {
      error "Invalid interface $opts(-interface) specified for tagging"
    }
    #Fixing descriptor_tag_id
    if { $::shell_activity_mode != "builder" } {
      set flat_inst [::col::get_hier_to_flat_name [::col::col_get_component_name $interface]]
      set new_descriptor_tag_id [collage_get_new_tag_id $flat_inst $descriptor_tag_id]
      set_attribute $interface -attr GenericAttr -subscript $new_descriptor_tag_id -value $value
    }

    set_attribute $interface -attr GenericAttr -subscript $descriptor_tag_id -value $value
  }

  if { ![info exists opts(-interface)] && ![info exists opts(-port)] && ![info exists opts(-param)] } {
    set design [current_design -quiet]
    
    set_attribute $design -attr GenericAttr -subscript $descriptor_tag_id -value $value
  }
}

define_proc_attributes collage_set_descriptor_tag \
    -info "Collage - set descriptor tap on port/param" \
    -define_args {
      {"-port"        "Name of the port"   "" string optional}
      {"-param"        "Name of the param"   "" string optional}
      {"-interface"        "Name of the interface"   "" string optional}
      {"-descriptor_tag_name"        "Identifier / Name of descriptor_tag"   "" string required}
      {"-descriptor_tag_value"        "Value of the tag"   "" string optional}
    }

proc collage_gen_connections_from_intf { args } {
  parse_proc_arguments -args $args opts
  set inst_tag $opts(-inst_tag)
  set std_conn_file $opts(-std_conn_file)
  set adhocfptr ""
  set tieofffptr ""
  set intf_tag ""
  set port_tags ""
  set tieoff_tags ""
  set target_intfname ""
  set conn_name_tag ""

  if {[info exists opts(-adhoc_conn_file)]} {
    set adhocfptr [open $opts(-adhoc_conn_file) "w"]
  }
  if {[info exists opts(-tieoff_file)]} {
    set tieofffptr [open $opts(-tieoff_file) "w"]
  }
  if {[info exists opts(-intf_tag)]} {
    set intf_tag $opts(-intf_tag)
  }
  if {[info exists opts(-conn_name_tag)]} {
    set conn_name_tag $opts(-conn_name_tag)
  }
  if {[info exists opts(-intf_defn)]} {
    set intf_defn $opts(-intf_defn)
  }
  if {[info exists opts(-port_tags)]} {
    set port_tags $opts(-port_tags)
    if {$adhocfptr == ""} {
      print_error "please provide file name to write adhoc connection with -port_tags option"
      return
    }
  }
  if {[info exists opts(-tieoff_tags)]} {
    set tieoff_tags $opts(-tieoff_tags)
    if {$tieofffptr == ""} {
      print_error "please provide file name to write tieff connection with -tieoff_tags option"
      return
    }
  }

  set stdfptr [open $std_conn_file "w"]

  foreach  intf_pair [collage_get_objects_with_descriptor_tag -descriptor_tag_name $inst_tag -object_type interface -flat_inst_tag] {
    #puts "DBG: Processing $intf_pair"
    set sourceip [lindex [split [lindex $intf_pair 0] "/"] 0]
    set intfname [lindex [split [lindex $intf_pair 0] "/"] 1]
    set targetip  [lindex $intf_pair 1]
    set targetip_f [collage_get_folded_name -name $targetip]
    set target_intfname ""
    ## FIXME_SV: assumption that the value is the instance name (use inst alias?)
    ## note: the -quiet is needed below to allow for same descriptor tag to be used for multiple interface types such as IOSF::SB
    ##       IOSF::Primary, etc
    set skip_by_ifc_type_check 0
    collage_eval_in_component -use_hier_par $sourceip {
      set sourceip_f [collage_get_folded_name -name $sourceip]
      set srcintf [find_interface_instances -quiet -component $sourceip_f -name $intfname -filter InterfaceDefinitionName==$intf_defn]
      if {$srcintf == ""} {
    ## FIXME? need to determine how to distinguish between erroneous data vs. bad input
        #print_warning "Could not find ip $sourceip or interface $intfname in ip $sourceip"
        set skip_by_ifc_type_check 1
      } else {
        if {$intf_tag != ""} {
          set new_intf_tag [collage_get_new_tag_id $sourceip $intf_tag]
          set target_intfname [get_attribute $srcintf -attr GenericAttr -subscript $new_intf_tag]
        } else {
          set target_intfname ""
        }
        if {$conn_name_tag != ""} {
          set conn_name [get_attribute $srcintf -attr GenericAttr -subscript $conn_name_tag]
        } else {
          set conn_name ""
        }
      }
    }
    if {$skip_by_ifc_type_check} {
      continue
    }

    ## if instance does not exist, give a warning and continue
    if {![::col::inst_exists_in_any_hier $targetip]} {
      print_warning "Auto gen: did not find target $targetip for source ${sourceip}/$intfname"
      continue
    }

    # if target instance exists, then find the sink interface on it
    collage_eval_in_component -use_hier_par $targetip {
      if {$target_intfname != ""} {
    # interface name was specified in the value of the source tag
        set targetintf [find_interface_instances -component $targetip_f -name $target_intfname]
        if {$targetintf == ""} {
          print_error "Could not find targetip $targetip or interface $target_intfname in $targetip"
        }
      } else {
        set targetintf  [find_interface_instances  -component $targetip_f -filter "InterfaceDefinitionName==$intf_defn"]
        if {[sizeof_collection $targetintf] != 1} {
          print_error "found [sizeof_collection $targetintf] interface of type $intf_defn in instance $targetip, can not generate connect $targetintf"
          return
        }
      }
    }
    puts $stdfptr "C $sourceip $intfname $targetip [get_attribute -attr Name $targetintf] $conn_name" 
    foreach port_tag $port_tags {
      set new_port_tag [collage_get_new_tag_id $sourceip $port_tag]
      set sport [get_attribute $srcintf -attr GenericAttr -subscript $new_port_tag]
      if {$sport != ""} {
        set new_port_tag [collage_get_new_tag_id $targetip $port_tag]
        set tport [get_attribute $targetintf -attr GenericAttr -subscript $new_port_tag]
        if {$tport != ""} {
          puts $adhocfptr "C $targetip/$tport $sourceip/$sport"
        }
      }
    }
    foreach  tieoff_tag $tieoff_tags {
      ## add a check to find which is value and which is port"
      set new_tieoff_tag [collage_get_new_tag_id $sourceip $tieoff_tag]
      set value [get_attribute $srcintf -attr GenericAttr -subscript $new_tieoff_tag]
      if {$value == ""} {
        print_warning "... value of attribute  $tieoff_tag on [get_attribute -attrs UserName $srcintf] is empty"
      }
      if {$value != "" && [string is integer $value]} {
        set value [format "0x%x" $value]
      }

      if {$value != ""} {
        set new_tieoff_tag [collage_get_new_tag_id $targetip $tieoff_tag]
        set tport [get_attribute $targetintf -attr GenericAttr -subscript $new_tieoff_tag]
        if {$tport !=  ""} {
          puts $tieofffptr "T $value $targetip/$tport"

        }
      }
    }
  }
  close $stdfptr
  if {$adhocfptr != ""} {
    close $adhocfptr
  }
  if {$tieofffptr != ""} {
    close $tieofffptr
  }
}

define_proc_attributes  collage_gen_connections_from_intf \
    -info "Collage - collage create connnections from interface descriptor tags" \
    -define_args {
      {"-std_conn_file"        "File for std connections"    "" string required}
      {"-adhoc_conn_file"       "File for std connections"   "" string optional}
      {"-inst_conn_file"        "File for std connections"   "" string optional}
      {"-tieoff_file"           "File for straps connection" "" string optional}
      {"-inst_tag"              "Instance Tags"              "" string required}
      {"-intf_tag"              "Interface Tags"             "" string optional}
      {"-conn_name_tag"         "Interface Tags connection name" "" string optional}
      {"-port_tags"             "Port Tags"                  "" string optional}
      {"-tieoff_tags"           "Tieoff Tags"                "" string optional}
      {"-intf_defn"             "Interface definition name such as IOSF::SB"              "" string required}
}

proc collage_get_new_tag_id {flat_inst old_tag_id} {
  return "[::col::get_ip_real_name ${flat_inst}]:${old_tag_id}"
}

proc collage_set_inst_desc_tag_ids {} {
  foreach leaf [collage_get_leaf_ips] {
    eval_in_component [collage_get_ip_hier_par -ip_name $leaf] {
      foreach_in_collection ifc [find_interface_instances -component [collage_get_folded_name -name $leaf]] {
        set subscripts [get_subscripts -attr GenericAttr $ifc]
        set subscripts_t [list]
        foreach subscript $subscripts {
          if {[regexp {:} $subscript] || [regexp "$leaf:$subscript" $subscripts]} {continue}
          lappend subscripts_t $subscript
        }
        foreach subscript $subscripts_t {
          set value [get_attribute $ifc -attr GenericAttr -subscript $subscript]
            set new_descriptor_tag_id [collage_get_new_tag_id $leaf $subscript]
            set_attribute $ifc -attr GenericAttr -subscript $new_descriptor_tag_id -value $value
        }
      }
    }
  }
}


# ---------------------------------
# - for MI port merging
# ---------------------------------
proc sv_compare_list {list1 list2} {
  if {[llength $list1] != [llength $list2]} {
    return 0
  }	
  foreach elem $list1 {
    if {[lsearch -exact  $list2 $elem] == -1} {
      return 0
    }
  }
  foreach elem $list2 {
    if {[lsearch -exact  $list1 $elem] == -1} {
      return 0
    }
  }
  return 1
}

## Merge PORTS for receiver, runs DFS for all hierarchies. skip_ifc_port and skip_adhoc_pin are being ignored. 
proc collage_mi_merge_ports {cell {skip_ifc_port 0} {skip_adhoc_pin ""}} {
  set inst_list [list]
  lappend inst_list $cell
  while {[llength $inst_list] > 0} {
    set new_list ""
    foreach c $inst_list {
      if {[::col::is_readonly_inst $c 1]} {
        continue
      }
      set_current_component [collage_get_hier_name -ip_name $c] -quiet
      set hier [get_current_component]
      set isHier [get_attribute -attrs IsHierarchicalComponent [current_design -quiet]]
      if {!$isHier} {
        set_current_component -quiet
        continue
      }
      _collage_merge_ports_local $c $skip_ifc_port $skip_adhoc_pin
      set_current_component "/$hier" -quiet
      foreach_in_collection nextcell [find_item -quiet -type cell ] {
        set hier_cell_name "$hier/[get_attribute -attr Name $nextcell]"
        set inst_name [::col::get_hier_to_flat_name $hier_cell_name]
        lappend new_list $inst_name
      }
      set_current_component -quiet
    }
    set inst_list $new_list
  }
  set_current_component  -quiet
}



proc _collage_merge_ports_local {cell skip_ifc_port skip_adhoc_pin} {
  if {$cell == "" || $cell == "/"} {
    return
  }
  set folded_cell [collage_get_folded_name -name $cell]
  set_current_component -quiet ..
  foreach_in_collection pin [find_item -type pin $folded_cell/* -quiet] {
    lassign [get_attribute $pin -attr {HasCnctChildren StartBit}] chcnt startbit
    if {$chcnt > 0 || $startbit != ""} {
      continue
    }
      set port [get_attribute $pin -attr RefPort ]
      if {[get_attribute $port -attr GenericBooleanAttr -subscript $::col_attr::user_port] == 1} {
          continue
      }
     
    set origcons [get_connections -name $pin]
    if {[llength $origcons]  > 2} {
      #print_info "folded_cell $folded_cell origcons $origcons"
    }	
    set dups [lsearch  -regexp -all -inline $origcons ^${folded_cell}\/.*]
    if {[llength $origcons]  > 2} {
      #print_info "dups $dups"
    }
    #     set dups [lsearch  -regexp -all -inline [get_connections -name $pin] ^${folded_cell}\/.*]
    set dupsize [llength $dups]
    if {$dupsize <= 1  || [regexp {\[} $dups] || [llength $origcons] == $dupsize} {
      if {$dupsize > 1} {
      }	
      #print_info "returning dup $dupsize [llength $origcons]"
      continue
    }
    set pinname [get_attribute -attr UserName $pin]
    set pname [lindex [split [get_attribute -attr UserName $pin] "/"] 1]
      set zero_pins [list]
      set pz [find_item -quiet -type port -hidden logic.zero]
      #FIXME - check why logic.one is not added
      #append_to_collection pz [find_item -quiet -type port -hidden logic.one]
      set isconst 0
      if {[sizeof_collection $pz] > 0} {
      foreach_in_collection ec [get_attribute -attrs ElectricalConnections $pz] {
        if {[lsearch  [get_attribute -attrs ConnectedPinNames $ec] $pinname] > -1} {
          set isconst 1
          break
        }	
      }
    }
      print_info "isconst $isconst pin $pinname"
    if {$isconst} {
      continue
    }
    foreach_in_collection lo [find_item -quiet -type net -hidden logic.open.*] {
      foreach_in_collection ec [get_attribute -attrs ElectricalConnections $lo] {
        if {[lsearch [get_attribute -attrs ConnectedPinNames $ec] $pinname] > -1} {
          set isconst 1
          break
        }	
      }
    }
    if {$isconst} {
      continue
    }
    
    print_info "Potentially Merging duplicates $dups"
      set plist [list]
      set predir ""
      set isdup 1
      if {([lsearch -all -inline -regexp $dups "memss/npkpll_npkclk_mc1_main"] < 0 ) && ([lsearch -all -inline -regexp $dups "par_fivr_4c_right/fivrclk_c2_00_1_gtfwd"] < 0 )  } {
      foreach conn $dups {
          set dir [get_attribute -attr PortDirection [find_item -type pin $conn -quiet ]]
          if {$predir != "" && $predir != $dir} {
              set isdup 0
              break
          }	
          set predir $dir
          lappend plist [lindex [split $conn "/"] 1]
      }
      if {!$isdup} {
          continue
      }	
      if {[::col::is_mi_inst $cell]} {
        set all_insts [::col::get_all_insts_of_mi_hier $cell]
        foreach inst $all_insts {
          if {$inst == $cell} {
            continue
          }	
          set finst [collage_get_folded_name -name $inst]
          eval_in_comp "/[collage_get_ip_hier_par -ip_name $inst]" {
            set opin [find_item -type pin  $finst/$pname -quiet]
            if {$opin != ""} {
              set conns [lsearch  -regexp -all -inline [get_connections -name $opin] ^${finst}\/.*]
             # print_info "[get_connections -name $opin] conns $conns dups $dups"
              set newplist [list]
              foreach conn $conns {
                lappend newplist [lindex [split $conn "/"] 1]
              }	
              if {[sv_compare_list $plist $newplist] == 0} {
                  print_info "not matching"
		  set isdup 0
              }
            } else {
              print_warning "Could not find pin $inst/$pname, please check!!"
            }	
          }
          if {!$isdup} {
            ## don't compare other MI hierarchies
            break
          }	
        }
      }
    if {$isdup} {
      set portlist [list]
      foreach pin $dups {
        lappend portlist [lindex [split $pin "/"] end]
      }	
      set portlist [lsort $portlist]
      print_info "current_comp [get_current_component] cell $cell folded_cell $folded_cell portlist $portlist"
      set_current_component -quiet $folded_cell
      collage_force_merge_ports $portlist
      set_current_component -quiet ..
    }	
  }
 } 
}

# Force a merge of ports for current component. Ports are
# specified as names or list of names, e.g.
#
#    force_merge_ports  portA portB 
#
proc collage_force_merge_ports { args } {
  # Check that the current component is not a shared component.
  set cell [get_current_component -cell]
  if {![sizeof_collection $cell]} {
    set masterComp [get_attribute $cell -attr MasterComponentName]
    if {$masterComp ne ""} {
      print_warning " Cannot merge ports from a shared component. MasterComponent is $masterComp"
      return
    }
  }

  # Make a list of port names from args and check that the ports exist.
  set ports $args
  if {[llength $ports] == 1} {
    set ports [join $ports]
  }
  foreach portName $ports {
    set port [find_item -quiet -type port $portName]
    if {![sizeof_collection $port]} {
        print_warning " Port '$portName' not found - merge terminated"
      return
    }
  }

  # Merge ports to first port in args.
  set firstPort [lindex $ports 0]

  # Merge the ports.
  foreach portName [lrange $ports 1 end] { 
    # Get connected pins.
    set port [find_item -type port $portName]
    set conns [get_attribute $port -attr ElectricalConnections]
    if {[sizeof_collection $conns] > 1} {
      print_warning " Port $portName has partial connections - merge terminated."
      return
    }
    if {[sizeof_collection $conns] < 1} {
      print_warning " Port $portName has no connections - merge terminated."
      return
    }
    set pinConns [get_attribute $conns -attr ConnectedPinNames] 
    if {[llength $pinConns] < 1} {
        print_warning "Could not find any connections for port $portName in hierarchy [get_current_component]"
        return
    }	
    # Remove connection to port 
    remove_connection $portName

    # Reconnect pins to first port.
    set pinConns [join $pinConns]
    set cmd "create_connection {$firstPort $pinConns}"
    print_info $cmd
    eval $cmd
  } 
}



proc collage_find_duplicate_ports { fname} {
    set fn [open $fname w]
  #collage_update_and_write_spec_files
  collage_obv_wrapper init 1
    set leafips [collage_get_leaf_ips]
  foreach net  [collage_obv_wrapper get_nets] {
      if {[regexp {logic\.zero} $net] || [regexp {logic\.one} $net]} {
          continue
      }
      array unset hier_pin_map
      array set hier_pin_map {} 
      array set hier_pin_map_debug {} 
      foreach {pin dir} [collage_obv_wrapper get_pins_of_net $net] {
	  set w [split $pin /]
	  set pname [lindex [split $pin /] end]
	  if {[regexp {logic\.zero} $pname] || [regexp {logic\.one} $pname]} {
	      break
	  } 
	  set pintmpbase [lindex [split $pname "\["] 0]
	  if {[llength $w] == 1} {
	      set hier [obviate get_top_hier]
	  } else {
	      set hier [join [lrange $w 0 end-1] "/"]
	      set hier [collage_get_hier_to_flat_name -hier_name $hier ]
	  }
	  set_current_component [collage_get_ip_hier_par -ip_name $hier] -quiet
	  if {[llength $w] == 1} {
	      set port [find_item -type port $pintmpbase -quiet]
	  } else {
	      set p [find_item -type pin [collage_get_folded_name -name $hier]/$pintmpbase -quiet]
	      #print_info "pin is $hier/$pintmpbase $p pin $pin hier $hier"
	      set port [get_attribute $p -attr RefPort]
	  }
	  ###need to test more not sure its its giving correct width
	  set width [lindex [get_attribute $port -attr PortWidth] 0]
	  set_current_component -quiet 
	  if {[lsearch -exact $leafips $hier] < 0} {
	      if {[info exists hier_pin_map($hier)]} {
		  lappend hier_pin_map($hier) "$pname:$width"
	      } else {
		  set hier_pin_map($hier) "$pname:$width"
	      }
	  }

	  
      }
      foreach hier [array names hier_pin_map] {
	  if {[llength $hier_pin_map($hier)] > 1} {
	      if {[::col::is_mi_inst $hier] && [::col::is_readonly_inst  $hier  1]} {
		  ##No need to print it for all MI instances
		  continue
	      }
	      # ##CHECK MI
	      # if {[::col::is_mi_inst $hier] && ![::col::is_readonly_inst  $hier  1]} {
	      # 	  set all_insts [::col::get_all_insts_of_mi_hier $hier]
	      # }
	      
	      set prewidth 0
	      set duppinlist [list]
	      foreach namewidth $hier_pin_map($hier) {
		  sv_lassign [split $namewidth ":"]  name width
		  if {$prewidth != 0 && $width != $prewidth} {
		      set prewidth -1
		      print_info "width for pin $namewidth are not same $hier_pin_map($hier)"
		      break
		  }
		  lappend duppinlist $name
		  set prewidth $width
	      }
	      if {$prewidth != -1} {
		  print_info "DUPLICATES for net $net on hier $hier pins $hier_pin_map($hier)"
		  puts $fn "DUPLICATES for hier $hier pins $duppinlist allpins [collage_obv_wrapper get_pins_of_net $net]"
	      }
	  }
      }
  }
    close $fn
}


# ----------------------------------------------------------------------------------
# Check if pin is connected to a constant
# ----------------------------------------------------------------------------------
proc _collage_is_pin_tied_off {pin} {
  set pinname [get_attribute -attr UserName $pin]
  set pname [lindex [split $pinname "/"] 1]
  set zero_pins [list]
  set pz [find_item -quiet -type port -hidden logic.zero]
  append_to_collection pz [find_item -quiet -type port -hidden logic.one]
  set isconst 0
  if {[sizeof_collection $pz] > 0} {
    foreach_in_collection p $pz {
      foreach_in_collection ec [get_attribute -attrs ElectricalConnections $p] {
	if {[lsearch  [get_attribute -attrs ConnectedPinNames $ec] $pinname] > -1} {
	  set isconst 1
          break
	}
      }
    }
  }
  return $isconst
}


proc ::collage_subsystem::_set_subsys_attrib_on_ip {subsys_id subsys_ip} {
    collage_eval_in_component -use_hier_par -edit_mi $subsys_ip  {
      set cell_ip_found [find_item -quiet -type cell [collage_get_folded_name -name $subsys_ip]]
      set num_found [sizeof_collection $cell_ip_found]
      if { $num_found == 0 } {
        print_warning "No IP with name $subsys_ip found during subsystem inst."
      } elseif { $num_found ==1 } {
        set_attribute $cell_ip_found -attr GenericAttr -subscript SubsysName -value $subsys_id
      } else {
        print_warning "Multiple IP's with name $subsys_ip found during $subsys_id subsystem inst."
      }
    }
  }


##############################################################################
# Install IP Kit
###############################################################################
proc collage_install_ip_kit {args} {
  parse_proc_arguments -args $args opts

  set ok 1
  if {![info exists opts(-skip_exist_check)]} {
    if {[file exists $opts(-dest_dir)/$opts(-kit_name)]} { 
      print_warning "Corekit $opts(-kit_name) already installed"
      set ok 0
    }
  }
  if {$ok} {
    collage_sip_kit_inst $opts(-ip_name) $opts(-kit_name) $opts(-src_dir) $opts(-dest_dir)
  }
}

define_proc_attributes collage_install_ip_kit \
    -info "Collage - install IP kits" \
    -define_args {
      {"-ip_name"   "IP name"   "" string required}
      {"-kit_name"  "Kit Name"   "" string required}
      {"-src_dir"   "Location of coreKits"   "" string required}
      {"-dest_dir"  "Location to install"   "" string required}
      {"-skip_exist_check"  ""   "" boolean optional}
    }




# ----------------------------------------------------------------------------------
# 
# ----------------------------------------------------------------------------------

###############################################################################
#Add the below two procs to enable collage upf 4.1.Can be removed once we move t to collage 4.1 completely.
###############################################################################
proc ::col::collage_get_leaf_cells {comp {ismi 0} {empty_pars 0}} {
  eval_in_component $comp {
    set ret_val {}
    set cell_coll [find_item -quiet -type cell]
    if {[sizeof_collection $cell_coll] >0} {
      foreach_in_collection c $cell_coll {
        set is_hier [get_attribute -attrs IsHierarchicalComponent [get_attribute -attrs ReferenceDesign $c]]
        set ipname [get_attribute -attrs Name $c]
        set hier_cell_name "[get_current_component]/$ipname"
        if {$is_hier} {

          set ret_val [concat $ret_val [::col::collage_get_leaf_cells "\/$hier_cell_name" $ismi $empty_pars]]
        } else {
          set inst_name [::col::get_hier_to_flat_name $hier_cell_name]
          if {[::col::are_ips_registered $inst_name]} {
            set ret_val [concat $ret_val $inst_name]
          }
        }
      }
    } elseif {$empty_pars} {
      set hier_cell_name "[get_current_component]"
      set inst_name [::col::get_hier_to_flat_name $hier_cell_name]
      set ret_val [concat $ret_val $inst_name]
    }
  }

  return [lsort -uniq $ret_val]
}
###############################################################################
# 
###############################################################################
proc collage_get_leaf_ips { args } {
  parse_proc_arguments -args $args opts

  if { [info exists opts(-hierarchy)] } {
    set hierarchy $opts(-hierarchy)
  } else {
    set hierarchy "/"
  }
  if {[info exists opts(-mi)] } {
    set mileaf $opts(-mi)
  } else {
    set mileaf 0
  }

  if {[info exists opts(-include_empty_pars)] } {
    set empty_pars $opts(-include_empty_pars)
  } else {
    set empty_pars 0
  }

  return [::col::collage_get_leaf_cells $hierarchy $mileaf $empty_pars]
}

define_proc_attributes collage_get_leaf_ips \
    -info "Return list of leaf IPs" \
    -define_args {
      {"-hierarchy"      "Name of the hierarchy" "/" string optional}
      {"-mi"             "Treat MI as leaf" "" boolean {hidden optional}}
      {"-include_empty_pars" "Includes empty pars as part of return list" "" boolean optional}
    }

###############################################################################

proc soc_collage_tar_ws {fn} {
  collage_save_partition_info
  set tarfile ${fn}.tar.gz
  set cmd "tar hzcf $tarfile [get_workspace_name]"
  print_info "Starting save and tar of workspace to $tarfile"
  save_workspace
  eval exec $cmd
  print_info "Completed tar of workspace to $tarfile"

}

proc soc_collage_un_tar_ws {fn dest_dir} {
  set tarfile ${fn}.tar.gz
  if {[file exists $tarfile]} {
	set cmd "tar xf $tarfile -C $dest_dir "
	print_info "Starting untar of workspace to $tarfile"
	eval exec $cmd
	if {[file exists $dest_dir/soc/.lock]} {
	  eval exec "rm $dest_dir/soc/.lock" 
	}
	print_info "Completed untar of workspace to $tarfile"
    soc_collage_remove_tmp_ws
  } else {
    print_info "tar file $tarfile not found to untar"
  }
}
proc soc_collage_mv_install_dir {src dest} {
  print_info "Starting move of installed_kits from $src to $dest"
  set only_dir [file tail $src]
  file delete -force $dest/$only_dir
  ::fileutil::install $src $dest
  print_info "Completed move of installed_kits from $src to $dest"
}
proc soc_collage_remove_tmp_ws {} {
  if {[info exists ::tmp_workspace_root]} {
    if {[file isdir $::tmp_workspace_root]} {
	  print_info "Starting removal of temporary workspace $::tmp_workspace_root ."
	  file delete -force $::tmp_workspace_root
	  print_info "Completed removal of temporary workspace $::tmp_workspace_root ."
    }
  } else {
    print_info "Temporary workspace variable ::tmp_workspace_root doesnt exist ."
  }
}
proc ::col::isa_par {par} {
  if {[lsearch -exact $::col::pars $par]> -1} {return 1}
    return 0
}

###############################################################################
proc collage_set_ifunconn_attr {inst port_name val} {
  eval_in_component [::col::get_hier_name  $inst] {
    set_attribute [find_item -type port $port_name] -attr IfUnconnected -value $val
  }
}

