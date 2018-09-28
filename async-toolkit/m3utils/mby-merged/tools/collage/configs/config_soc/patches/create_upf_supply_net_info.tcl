namespace eval ::soc_power_net {

}
proc ::soc_power_net::get_ip_list  { file_list } {
  set ip_list [list]
  foreach spec_file [join $file_list] {
    set ip_name [file tail $spec_file]
    set ip [lindex [split $ip_name .] 0]
    lappend ip_list $ip
  }
  return $ip_list
}


proc ::soc_power_net::get_ip_name  { args } {
  set ip_name ""
  set ip_name [file tail $args]
  set ip_name [lindex [split $ip_name .] 0]
  return $ip_name
}

proc ::soc_power_net::get_hier_ip_module_cell_name { ip } {
  set module_cell_list [list]
  foreach each_cell [get_object_name [get_cells -quiet -hierarchical]] {
    set leaf_cell [lindex [join [split $each_cell /]] end]
    if { $each_cell eq "U1"  || $each_cell eq "U2" } {
      continue
    }
    if { $leaf_cell eq "U1"  || $leaf_cell eq "U2" } {
      continue
    }
    if { $leaf_cell eq $ip } { 
      set module_name  [get_attribute -quiet $each_cell ref_name]
      lappend module_cell_list $module_name 
      lappend module_cell_list $each_cell 
      lappend module_cell_list $leaf_cell 
    }
  }
  return $module_cell_list
}

proc ::soc_power_net::create_blackbox { ip } {
  set orig_design ""
  set orig_design [current_design]
  set orig_design_name [current_design_name]
  current_design $ip
  if {[current_design_name] != $orig_design_name} {
    remove_cell -all
  }
  current_design $orig_design
}

proc ::soc_power_net::is_tie_off { signal } {
  set ret_val 0
  if {[lsearch -exact {"**logic_0**" "**logic_1**"} $signal] > -1} {
    set ret_val 1
  }
  return $ret_val
}

proc ::soc_power_net::write_blackbox_rtl { } {
  set bb_rtl [pwd]/bb_rtl	
  if { [file exists $bb_rtl ] } { 
    file delete -force $bb_rtl
  } 
  file mkdir $bb_rtl
  write_file  -format ddc -hierarchy -output $bb_rtl/design_blackbox_rtl.ddc
}

proc ::soc_power_net::read_blackbox_rtl {} {
  set bb_rtl [pwd]/bb_rtl	
  read_ddc $bb_rtl/design_blackbox_rtl.ddc
  current_design soc
}


proc ::soc_power_net::remove_blackbox_rtl { args } {
  set bb_rtl [pwd]/bb_rtl	
  file delete -force $bb_rtl
}

proc ::soc_power_net::print_time {} {
  set systemTime [clock seconds]
  set sys_time [clock format $systemTime -format %H:%M:%S]
  return $sys_time
}

proc ::soc_power_net::net_info {net} {
  set net_name $net
  regsub -all {\[} $net_name "\\\[" net_name
  regsub -all {\]} $net_name "\\\]" net_name
  return $net_name
}


proc ::soc_power_net::gen_vnn_vnnaon {args} {

  set pin_spec_files  ""
  set pwr_algorithm ""
  set all_pars ""
  set error 0
  set missing_par_files [list]
  set ip_list [list]
  set POWER_NET_TYPES [list]
  array unset d_vars_info
  array unset ip_pwr_map
  array unset ip_missing_pwr_map
  array unset signal_pwr_map
  array unset signal_missing_pwr_map
  array unset unit_hier_map
  array unset par_signal_pwr_map 
  array unset LISTED_POWER
  array unset d_leaf_spec_file_map
  array unset d_soc_pwr_map 

  parse_proc_arguments -args $args options
  foreach file_name $options(-pin_spec_file) {
    if {[file exists $file_name]} {
      lappend pin_spec_files $file_name
    }
  }
  
  if {[info exists options(-set_vars)]} {
    foreach {var var_value} [join $options(-set_vars)] {
      set ${var} ${var_value}
      set d_vars_info($var) $var_value
    }
  }

  foreach {pwr_key pwr_value} [join $options(-pwr_map)] {
    foreach pwr_supply [ join $pwr_value] {
        set LISTED_POWER($pwr_supply) $pwr_key
    }
    lappend POWER_NET_TYPES ${pwr_key}_nets
    set ${pwr_key}_nets [list] 
  }


  if {[info exists options(-algorithm)]} {
    set pwr_algorithm $options(-algorithm)
    set sorted_pwr_algorithm [list]
    foreach {drv_pwr rec_pwr res_pwr} [join ${pwr_algorithm}] {
      lappend sorted_pwr_algorithm [list $drv_pwr [join [lsort -dictionary [split $rec_pwr "&"]] "&"] $res_pwr]
    }
    set pwr_algorithm $sorted_pwr_algorithm
  } else {
    set pwr_algorithm [list [list vnn aon vnn] [list vnn vnn vnn] [list aon vnn vnn] [list aon aon aon] [list aon aon&vnn aon] [list vnn aon&vnn vnn]]
  }
 foreach {drv_pwr rec_pwr res_pwr} [join $pwr_algorithm] {
   set d_soc_pwr_map($drv_pwr,$rec_pwr) $res_pwr
 }
#need for inout as inout are treated as rcv_pins. Therefore the drv_pins are empty and drv_pwr is empty
 foreach pwr [lsort -unique [join $pwr_algorithm]] {
   set d_soc_pwr_map(,$pwr) $pwr
 }


  if {[info exists options(-par_names)]} {
    set all_pars $options(-par_names)
  } else {
    if {[info exists ::synopsys_program_name]} { 
      if {[string equal $::synopsys_program_name "icc_shell"]} {
        if {[sizeof_collection [get_plan_groups -quiet]]} {
          set all_pars [get_attribute [get_plan_groups -quiet] name]
        } else {
          set all_pars [get_attribute [get_cells -filter "is_soft_macro==true"] name]
        }
      } elseif {[string equal $::synopsys_program_name "dc_shell"]} {
      set all_pars [get_attribute [get_cells ] name]
      }
    }
  }
 
  set pwr_list [join [array names LISTED_POWER]]
  set ip_list [get_ip_list $pin_spec_files]
   
 # Initialize the signal_pwr_map   
  set all_cells [list]
  set leaf_list [list]
  set leaf_list $ip_list
  set all_cells  [get_object_name [get_cells -quiet -hierarchical]]
  foreach each_cell $all_cells {
    set all_pins [list]
    set leaf_cell ""
    set leaf_found 0
    set leaf_cell [lindex [join [split $each_cell /]] end]

    if { [lsearch $leaf_list $leaf_cell] > -1 } {
      set leaf_found 1
      set idx [lsearch $leaf_list $leaf_cell]
      set leaf_list [lreplace $leaf_list $idx $idx]
    }
    set all_pins [get_object_name [get_pins -quiet -of_objects $each_cell ]]
    foreach each_pin $all_pins {
      if {  $leaf_found == 1 } {
        set ip_pin ""
        set ip_pin [lindex [join [split $each_pin /]] end]
        set ip_pwr_map($leaf_cell/$ip_pin) ""
      }
      set signal_pwr_map($each_pin) ""
    }
    set leaf_found 0 
  }

  # set leaf pin power spec files pointers
  foreach spec_file $pin_spec_files {
    set ip_name ""
    set ip_name [::soc_power_net::get_ip_name $spec_file]
    set d_leaf_spec_file_map($ip_name) $spec_file
  }

#Read spec files into a array

  foreach spec_file $pin_spec_files {
    set soc_power_pin_data ""
    set each_ip [::soc_power_net::get_ip_name $spec_file]
    set fp_rd [open ${spec_file} r]
    set file_data [read $fp_rd]
    close $fp_rd
    set file_data [split ${file_data} "\n"]
    append soc_power_pin_data " [lrange ${file_data} 1 end]"
  
    foreach line $soc_power_pin_data {
      foreach { leaf pin power power_src} $line {}
      if {[lsearch -exact $pwr_list $power] >= 0} { 
        set ip_pwr_map($each_ip/$pin) [list $power $LISTED_POWER($power)]
      } else {
        set ip_missing_pwr_map($each_ip/$pin) [list $power ""]
      }
    }
  }

# set the signal_pwr_map for given ip list
  foreach ip $ip_list {
    set ip_hier_list [list]
    set module_list [list]
    set cell_list [list]
    set leaf_list [list]

    set ip_hier_list [::soc_power_net::get_hier_ip_module_cell_name $ip]
    
    foreach {mod cell leaf} $ip_hier_list {
      lappend module_list $mod
      lappend cell_list $cell
      lappend leaf_list $leaf
    }
  
    foreach each_cell $cell_list {
      set cell_index [lsearch $cell_list $each_cell]
      set each_leaf ""
      set each_leaf [lindex $leaf_list $cell_index]
      set all_pins [list]
      set all_pins [get_object_name [get_pins -quiet -of_objects $each_cell]]
      foreach each_pin $all_pins {
        set ip_pin ""
        set ip_pin [lindex [join [split $each_pin /]] end]
        if { [ lsearch $ip_list $each_leaf] > -1 } {
          set signal_pwr_map($each_pin) $ip_pwr_map($each_leaf/$ip_pin)
        }
      }
    }
  }
 
  foreach ip $ip_list {
    set ip_hier_list [list]
    set module_list [list]
    set cell_list [list]
    set leaf_list [list]
    set ip_hier_list [::soc_power_net::get_hier_ip_module_cell_name $ip]
    
    foreach {mod cell leaf} $ip_hier_list {
      lappend module_list $mod
      lappend cell_list $cell
      lappend leaf_list $leaf
    }
    set module_list [lsort -unique $module_list]
    foreach ip_blackbox $module_list {
      ::soc_power_net::create_blackbox $ip_blackbox
    }
    
  }
#  ::soc_power_net::write_blackbox_rtl

# Read modified blackbox rtl and return the original search path
#  ::soc_power_net::read_blackbox_rtl


###### Algorithm from BXT
#Power Receiver  --->	VNN	AON	VNN	AON	mixed receivers
#Driver	Fanout   --->	1	1	2+	2+	2+
#VNN 	         --->	VNN	VNN	VNN	VNN	VNN
#AON	         --->	VNN	AON	VNN	AON	AON
  
  if {![sizeof [current_design]]} {
    puts "Unable to open mw_cel, or no open mw_cel found !. Please open the design and rerun the proc."
    return
  }


  set nets_missing_reciever_power [list]
  set nets_missing_driver_power [list]
  set nets_missing_power [list]
  set all_nets [get_attribute [get_nets -quiet] name]
  set skipped_nets 0
  set multi_term_nets 0
  foreach net $all_nets {
    set skip_net 0
    set multi_term_net 0
    foreach c [get_attribute [get_cells -quiet -of_objects $net] name] {
      if {[lsearch -exact $all_pars $c] == -1} {
        puts "Skipping net $net because it is connected to a cell $c that is not in the list of cells provided as part of $all_pars"
        set skip_net 1; break
      }
    }

    if {$skip_net} {
      incr skipped_nets
      continue ; # Not one of the pars
    }
    set pins [get_pins -quiet -of_objects [get_nets -quiet $net]]
    set pin_pars [get_attribute [get_cells -quiet -of_objects ${pins}] name]
    set pin_directions [lsort -unique [get_attribute [get_pins -quiet $pins] direction]]
    if {[lsearch -exact {{in out} {inout out} {in inout} {inout}} $pin_directions] == -1} {
      incr skipped_nets
      puts "Skipping net \"$net\" as the pins associated with net are not either \"in out\" or \"inout out\" or \"in inout\" or \"inout\""
      continue
    }
    set total_pins [sizeof_collection $pins]
    if { $total_pins < 2} {
      incr skipped_nets
      puts "Skipping net \"$net\" as it is a 1 terminal net (Pins = $total_pins)"
      continue
    } elseif {$total_pins > 2} {
      set multi_term_net 1
      incr multi_term_nets
      #print_info "Multi terminal net \"[get_object_name $net]\" (Pins = [sizeof_collection $pins])"
    }
    if {[llength ${pin_pars}] <2} {
      incr skipped_nets
      puts "Skipping net \"$net\" as the pins are associated with only [lsort -unique $pin_pars] parition"
      continue
    }
    set leaf_pins [list]
    set leaf_pins [all_connected -leaf $net]
    set driver_pins "" 
    set rec_pins ""
    set driver_pwr [list]
    set reciever_pwr [list]
    set error 0
    if {[info exists ::synopsys_program_name]} {
      if {[string equal $::synopsys_program_name "icc_shell"]} {
        if {[llength $pins]} {
          set out_pins [filter_collection $pins "direction==out"]
          set in_out_pins [filter_collection $pins "direction==inout"]
          set in_pins [filter_collection $pins "direction==in"]
        } else {
          set out_pins [filter_collection $leaf_pins "direction==out"]
          set in_out_pins [filter_collection $leaf_pins "direction==inout"]
          set in_pins [filter_collection $leaf_pins "direction==in"]
        }
      } elseif {[string equal $::synopsys_program_name "dc_shell"]} { 
        if {[llength $pins]} {
          set out_pins [filter_collection $pins "pin_direction==out"]
          set in_out_pins [filter_collection $pins "pin_direction==inout"]
          set in_pins [filter_collection $pins "pin_direction==in"]
        } else {
          set out_pins [filter_collection $leaf_pins "pin_direction==out"]
          set in_out_pins [filter_collection $leaf_pins "pin_direction==inout"]
          set in_pins [filter_collection $leaf_pins "pin_direction==in"]

        }
      }
    }
    if {[sizeof_collection $out_pins]} {
      append driver_pins " [get_attribute $out_pins full_name]"
    }
    if {[sizeof_collection $in_out_pins]} {
      append rec_pins " [get_attribute $in_out_pins full_name]" 
    }
    if {[sizeof_collection $in_pins]} {
      append rec_pins " [get_attribute $in_pins full_name]"
    }


    foreach {pin} [join $driver_pins] {
#Detecting if the pin is output of soc. We want internal nets. so Skipping
      if {[llength [split $pin /]] > 1 } {
        if { [llength $signal_pwr_map($pin)]} {
          lappend driver_pwr [lindex $signal_pwr_map($pin) end]
        } else {
          lappend nets_missing_driver_power [list $net ${pin}]
          set error 1
        }
      } else {
        puts "\n $pin is output port and not a wire. So skipping"
      }
    }



    foreach {pin} [join $rec_pins] {
      if {[llength [split $pin /]] > 1 } {
        if {[llength $signal_pwr_map($pin)]} {
          set signal_pwr_map($pin) [join [lsort -unique -dictionary [lindex $signal_pwr_map($pin) end ]]]
          lappend reciever_pwr $signal_pwr_map($pin)
        } else {
          lappend nets_missing_reciever_power [list $net ${pin}]
          set error 1
        }
      } else {
         puts "\n $pin is input or inout port and not a wire. So skipping"
      }
    }


    set reciever_pwr [join [lsort -unique -dictionary $reciever_pwr] &]
   if {$error} {
      continue
    }
    if {[llength $d_soc_pwr_map($driver_pwr,$reciever_pwr)]} {
      set res_pwr $d_soc_pwr_map($driver_pwr,$reciever_pwr)
      lappend ${res_pwr}_nets ${net}
    } else {
      puts "Unable to determine the supply_net associated $net $pin $driver_pwr $reciever_pwr"
      lappend nets_missing_power [list $net ${pin} $driver_pwr $reciever_pwr]
    }

#  puts "RUNTIME DEBUG: net  :$net compare supply nets : [::soc_power_net::print_time]" 
#    foreach {drv_pwr rec_pwr res_pwr} [join $pwr_algorithm] {
#      if {($drv_pwr == $driver_pwr) && ($reciever_pwr == $rec_pwr)} {
#        lappend ${res_pwr}_nets ${net}
#      }
#    }
  }

#  puts "RUNTIME DEBUG: Finished to compare nets and its supplies : [::soc_power_net::print_time]" 
  puts "\n Writing the VNN AON INFO to $options(-out_file)"
  if {[file exists $options(-out_file)]} {
    file delete -force $options(-out_file)
  }
  set fp_wr [open $options(-out_file) w]
  foreach net_type $POWER_NET_TYPES {
    puts ${fp_wr} "set PROCESSED_${net_type} \[list \\"
    foreach net_name [set $net_type] {
      set net_sub_name  [::soc_power_net::net_info $net_name]
      puts ${fp_wr} "$net_sub_name \\"
    }
    puts ${fp_wr} "\]"
  }
  puts ${fp_wr}  "set NETS_MISSING_DRIVER_POWER \[list \\"
  foreach {net_name pin_name} [join $nets_missing_driver_power] {
    set net_sub_name [::soc_power_net::net_info $net_name]
    set pin_sub_name [::soc_power_net::net_info $pin_name]
    puts ${fp_wr} "\[list $net_sub_name $pin_sub_name \] \\"
  }
  puts ${fp_wr} "\]"
  puts ${fp_wr}  "set NETS_MISSING_RECIEVER_POWER \[list \\"
  foreach {net_name pin_name} [join $nets_missing_reciever_power] {
    set net_sub_name [::soc_power_net::net_info $net_name]
    set pin_sub_name [::soc_power_net::net_info $pin_name]
    puts ${fp_wr} "\[list $net_sub_name $pin_sub_name \] \\"
  }
  puts ${fp_wr} "\]"
  puts ${fp_wr}  "set NETS_MISSING_POWER \[list \\"
  foreach {net_name pin_name drv_pwr rcv_pwr} [join $nets_missing_power] {
    set net_sub_name [::soc_power_net::net_info $net_name]
    set pin_sub_name [::soc_power_net::net_info $pin_name]
    puts ${fp_wr} "\[list $net_sub_name $pin_sub_name $drv_pwr $rcv_pwr\] \\"
  }
  puts ${fp_wr} "\]"
 close ${fp_wr}

#  puts "\n Removing the black box rtl database"
 # ::soc_power_net::remove_blackbox_rtl
}


define_proc_attributes "::soc_power_net::gen_vnn_vnnaon " -info "Lattice command to generate vnn and vnnaon net list files which can be later used as an input to Lattice." \
    -hide_body \
    -define_args {
      {"-verbose" "Verbose mode of operation" "" "boolean" "optional"}
      {"-set_vars" "Set variables" "" "string" "optional"}
     {"-algorithm" "specifies the list of driver reciever and output power. Ex [list [list vnn aon vnn] [list vnn vnn vnn] [list aon vnn vnn] [list aon aon aon] [list aon vnn&aon aon] [list vnn vnn&aon vnn]]" "" "string" "optional"}
      {"-pin_spec_file"  "SOC pin spec file" "file name" "string" "required"}
      {"-pwr_map" "Specified the power mapping of POWER supply names with names used in algorithm. Ex [list [list vnn NAME_OF_VNN_SUPPLY] [list aon NAME_OF_AON_SUPPLY]]" "" "string" "required"}
      {"-par_names" "Provides the list of all paritition pin spec files that need to be processed" "" "string" "optional"}
      {"-out_file" "This is the output file generated which has the list of vnn and vnnaon nets" "" "string" "optional"}
  }
    

