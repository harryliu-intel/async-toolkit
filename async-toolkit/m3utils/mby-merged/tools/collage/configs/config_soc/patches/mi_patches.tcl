
proc _collage_split_pin {pin} {
  set is_bus 0
  if {[regexp {(\S+)\[(\S+)\]} $pin]} {
    set is_bus 1
    set elements [regexp  -inline {(\S+)\[(\S+)\]} $pin]
    set pin [lindex $elements 1]
  }
  return [list $pin $is_bus]
}
proc _collage_get_child_pin_connections {inst pin} {
  set connstotrace ""
  set cpins_master [list] 
  collage_eval_in_comp -use_hier_name $inst {
    foreach {split_pin is_bus} [_collage_split_pin $pin] {}
    set pin_coll [find_item -quiet -type port $split_pin]
    if {[sizeof_collection $pin_coll] > 0} { 
      set pin_decorated_name [get_attribute -attrs RangeDecoratedName $pin_coll]
      if {[regexp {.*\[.*} $pin_decorated_name] && !$is_bus} {
      } else { 
        foreach_in_collection econn [get_attribute $pin_coll -attr "ElectricalConnections"] {
          set cports_master [get_attribute $econn -attrs "ConnectedPortNames"]
          set cpins_master [get_attribute $econn -attrs "ConnectedPinNames"]
          if {[lsearch -exact $cports_master $pin] >= 0} { 
            #puts "Connections are $cports_master"
	    break;
	  }
        }
      }
    }
  }
  set ret_val [list]
  foreach pin $cpins_master {
    if {([lsearch -exact $pin {logic.one}] > -1) || ([lsearch -exact $pin {logic.zero}] > -1)} {continue }
    sv_lassign [split $pin "/"] fold_inst pname 
    set h_name "[string trimleft [get_current_component]/${fold_inst} /]"
    set inst_hier_flat [::col::get_hier_to_flat_name $h_name]
    set pin "$inst_hier_flat/$pname"
    foreach {leaf_pin inst pin pname} [_collage_get_pin_hier_data $pin] {}
    lappend ret_val $leaf_pin 
    lappend ret_val [_collage_get_child_pin_connections $inst_hier_flat $pname]
  }
  #puts "Returning pins $ret_val"
  return $ret_val
}
proc _collage_get_parent_pin_connections {inst pin} {
  set connstotrace ""
  set cports_master [list] 
  set inst_hier_flat [::col::get_hier_to_flat_name [collage_get_ip_hier_par -ip_name $inst]]
  collage_eval_in_comp -use_hier_par $inst {
    foreach {split_pin is_bus} [_collage_split_pin $pin] {}
    set pin_coll [find_item -quiet -type pin $split_pin]
    if {[sizeof_collection $pin_coll] > 0} { 
      set pin_decorated_name [get_attribute -attrs RangeDecoratedName $pin_coll]
      if {[regexp {.*\[.*} $pin_decorated_name] && !$is_bus} {
        collage_message_print CRT-015 "$leaf_pin"
      } else { 
        foreach_in_collection econn [get_attribute $pin_coll -attr "ElectricalConnections"] {
          set cports_master [get_attribute $econn -attrs "ConnectedPortNames"]
          set cpins_master [get_attribute $econn -attrs "ConnectedPinNames"]
          if {[lsearch -exact $cpins_master $pin] >= 0} { 
            #puts "Connections are $cports_master"
	    break;
	  }
        }
      }
    }
  }
  set ret_val [list]
  foreach port $cports_master {
    if {([lsearch -exact $port {logic.one}] > -1) || ([lsearch -exact $port {logic.zero}] > -1)} {continue }
    set pin "$inst_hier_flat/$port"
    foreach {leaf_pin inst pin pname} [_collage_get_pin_hier_data $pin] {}
    lappend ret_val $leaf_pin 
    lappend ret_val [_collage_get_parent_pin_connections $inst $pin]
  }
  #puts "Returning pins $ret_val"
  return $ret_val
}
proc _collage_get_pin_connections {inst pin leaf_pin} {
  set connstotrace ""
  set ret_val ""
  set is_hier 0
  set inst_hier [collage_get_ip_hier_par -ip_name $inst]
  collage_eval_in_comp -use_hier_par $inst {
    foreach {split_pin is_bus} [_collage_split_pin $pin] {}
    set pin_coll [find_item -quiet -type pin $split_pin]
    sv_lassign [split $pin "/"] fold_inst pname 
    set cell_coll [find_item -quiet -type cell $fold_inst]
    if {[sizeof_collection $cell_coll] > 0} {
      set is_hier [get_attribute -attrs "IsHierarchicalComponent" $cell_coll]
    }
    if {[sizeof_collection $pin_coll] > 0} { 
      set pin_decorated_name [get_attribute -attrs RangeDecoratedName $pin_coll]
      if {[regexp {.*\[.*} $pin_decorated_name] && !$is_bus} {
        collage_message_print CRT-015 "$leaf_pin" 
      } else { 
        set connstotrace [get_connections -names -hier $pin]
        #puts "Connections are $connstotrace"
        ## Following is a workaround to convert the folded names to leaf names from the get_connections command
        #puts "Connection hier is $get_connections_contexts"
        set connection_elements $get_connections_contexts
      }
    } else {
      collage_message_print CRT-015 " $leaf_pin"
    }
  }
  if {[llength $connstotrace]} {
    set ret_val [_collage_get_flat_names_from_folded $connstotrace $connection_elements]
  } else {
    if {$is_hier} {
      set child_pins [_collage_get_child_pin_connections $inst $pin]
      set child_pins [join [regsub -all {\{|\}} $child_pins {}]]
      foreach child_pin $child_pins {  
        foreach {child_leaf_pin child_inst child_pin child_pname} [_collage_get_pin_hier_data $child_pin] {}
        _collage_get_pin_connections $child_inst $child_pin $child_leaf_pin
        break
      }
      
    }
  }
  return $ret_val
}
proc _collage_get_flat_names_from_folded {connstotrace connection_elements} {
  set con_length [llength $connstotrace]
  set idx 0
  set ret_val [list]
  while {$idx < $con_length} {
    set folded_pin [lindex $connstotrace $idx]
    sv_lassign [split $folded_pin "/"] fold_inst pname
    set conn_hier [lindex $connection_elements $idx]
    set leaf_ip [::col::get_hier_to_flat_name $conn_hier/$fold_inst]
    set leaf_pin "${leaf_ip}/${pname}"
    lappend ret_val $leaf_pin
    incr idx
  }
  return $ret_val
}
proc _collage_get_pin_info {inst pin} {
  set total_pins 0
  set all_pins [list]
  collage_eval_in_comp -use_hier_par $inst {
    set pin_coll [find_item -quiet -type pin $pin]
    set total_pins [sizeof_collection $pin_coll] 
    if {$total_pins > 0} {
      set pin_name [get_attribute -attrs RangeDecoratedName $pin_coll]
      set all_bits [obviate bit_blast $pin_name]
      foreach bit $all_bits {
        sv_lassign [split $bit "/"] fold_inst pname
        lappend all_pins $pname
      }
    }
  }
  return $all_pins
}
proc _collage_get_pin_direction {inst pin} {
  foreach {pin is_bus} [_collage_split_pin $pin] {}
  set is_out 0
  collage_eval_in_comp -use_hier_par $inst {
    if { ([get_attribute -attrs PortDirection [find_item -type pin $pin]] == "out")} {
      set is_out 1
    }
  }
  return $is_out
}
proc _collage_get_pin_hier_data {leaf_pin} {
  sv_lassign [split $leaf_pin "/"] inst pname
  set inst [collage_get_ip_real_name -ip_name $inst]
  set pin [collage_get_folded_name -name $inst]/$pname
  return [list $leaf_pin $inst $pin $pname] 
}
proc collage_native_trace_pin_connection2driver {pin} {
  set is_out 0
  if [ string match -nocase "*/*" $pin ] {
    foreach {leaf_pin inst pin pname} [_collage_get_pin_hier_data $pin] {}
  } else {
    collage_message_print CRT-062 " $pin"
    return
  }	
  set connections [_collage_get_pin_connections $inst $pin $leaf_pin]
  foreach pin $connections {
    foreach {leaf_pin inst pin pname} [_collage_get_pin_hier_data $pin] {}
    if {$pname == "logic.zero" || $pname == "logic.one"} {
      continue 
    }
    if {[::col::isa_par $inst]} {
      puts "Coretools is returning hierarchical pin $pin for get_connections operation on pin $leaf_pin"  
      continue 
    }
    set is_out [_collage_get_pin_direction $inst $pin] 
    if {$is_out} {
      return $leaf_pin
    }
  }
  return
}

proc collage_get_par_ports_of_pin {pin} {
  if [ string match -nocase "*/*" $pin ] {
    foreach {leaf_pin inst pin pname} [_collage_get_pin_hier_data $pin] {}
  } else {
    collage_message_print CRT-062 " $pin"
    return
  }	
  set leaf_connections [_collage_get_pin_connections $inst $pin $leaf_pin]
  set ret_val [list]
  foreach pin $leaf_connections {
    foreach {leaf_pin inst pin pname} [_collage_get_pin_hier_data $pin] {}
    if {$pname == "logic.zero" || $pname == "logic.one"} {
      continue 
    }
    if {[::col::isa_par $inst]} {
      lappend ret_val $leaf_pin
    }
    lappend ret_val [_collage_get_parent_pin_connections $inst $pin]
    set ret_val [lsort -unique [regsub -all {\{|\}} $ret_val {}]]
  }
  return [list $ret_val $leaf_connections]
} 

proc collage_trace_pin_connection2driver { pin hierarchy all_hier_names} {
  foreach {hier_name only_pin} [join [obv_get_hier_pin $pin]] {} 
  set hier_pin "$hier_name/$only_pin"
  set soc_net [obviate get_net_of_pin $hier_pin]
  set soc_pins [obviate get_pins_of_net ${soc_net}]
  set drv_pin [list]
  foreach {conn_pin pin_dir} $soc_pins {
    if {[string equal ${conn_pin} ${hier_pin}] || [string equal $pin_dir in]} {
      continue
    } else {
    }
    set split_pin [split ${conn_pin} "/"]
    set conn_ip [join [lrange ${split_pin} 0 end-1] "/"]
    set pin_name [lindex $split_pin end]
    set flat_conn_ip [collage_get_hier_to_flat_name -hier_name $conn_ip]
    set flat_conn_pin "$flat_conn_ip/$pin_name"
    set ip_clk_found 0;
    set matching_ips [obviate get_ip_obj_of_hier ${conn_ip}]
    if {([llength ${matching_ips}]) && ([lsearch -exact ${all_hier_names} ${conn_ip}] == -1)} {
       if {[string equal ${pin_dir} "out"]} {
         lappend drv_pin $flat_conn_pin
       }
    }
  }
  return $drv_pin
}



## Algorithm ( per clock connection)

## Get the hierarchy of the clock driver
## Get the hierarchy of the 
## Get the hierarchy of the SCC 
## Find the CP point for the SCC input
## Create C lines and D lines for all SCC outputs, as all outputs from SCC are crossing the 
## Create Ci line from CP port to SCC input
## Create C line from CP port to driver


## Algorithm 2 
 ## Assumption Each SCC is wrapped around a wrapper 
## Get the list of SCC's in the SOC
## Check if they have a wrapper around them 
## Check the number of bits on the input bus and the number of bits on the output bus
## Create a CP line for each input bit and each output bit on the wrapper
## Create a Ci line from the wrapper port to SCC bits
## Use the wrapper input ports and output ports to make the connections through SCC.
proc find_scc {{pattern *clst_*scan*} {leaf_ips ""}} {
  set all_scc [lsearch -all -inline $leaf_ips ${pattern}]
  return ${all_scc}
}

proc find_scc_wrapper {{scc_pattern *clst_*scan*} {pattern cdu*} {leaf_ips ""}} {
  set all_scc [find_scc $scc_pattern $leaf_ips] 
  set scc_info [list]
  foreach scc ${all_scc} {
    set scc_wrapper [collage_get_hier_to_flat_name -hier_name [collage_get_ip_hier_par -ip_name ${scc}]]
    
    if {[collage_is_mi_inst -name $scc] && [llength $scc_wrapper] && [string match ${pattern} ${scc_wrapper} ]} {
      lappend scc_info [list $scc $scc_wrapper]
    }
  }
  return $scc_info
}

proc gen_cp_for_sccwrapper {out_file {input_pattern "pre_clocks*"} {output_pattern "post_clocks*"} {scan_pattern *clst_*scan*} {cdu_pattern cdu*}} {
  set leaf_ips [collage_get_leaf_ips]
  set scc_info [find_scc_wrapper $scan_pattern $cdu_pattern $leaf_ips]
  foreach scc_elem $scc_info {
    foreach {scc scc_wrap} $scc_elem {}  
    collage_message_print CRT-003 "Processing scan element $scc with wrapper $scc_wrap"
    set cp_ip_cmds($scc_wrap) [list]
    set cp_op_cmds($scc_wrap) [list]
    foreach {leaf_pin inst pin pname} [_collage_get_pin_hier_data $scc/$input_pattern] {}
    set scc_input_ports [_collage_get_pin_info $inst $pin]

    foreach {leaf_pin inst pin pname} [_collage_get_pin_hier_data $scc/$output_pattern] {}
    set scc_output_ports [_collage_get_pin_info $inst $pin]
    collage_message_print CRT-003 "Total input pins on SCC that matched the input pattern $input_pattern are [llength $scc_input_ports]"
    collage_message_print CRT-003 "Total output pins on SCC that matched the output pattern $output_pattern are [llength $scc_output_ports]\n"
    if {[llength $scc_input_ports] != [llength $scc_output_ports]} {
      collage_message_print CVAL-003 "Input bus width on SCC doesn't match output bus width. SCC $scc" 
    }
    foreach scc_ip $scc_input_ports {
      set sccwrap_ip [string trimright [regsub -all {\[|\]} $scc_ip _] _]
      lappend cp_ip_ports($scc_wrap) [list ${sccwrap_ip} ${scc_ip} ${scc}]
    }
    foreach scc_op $scc_output_ports {
      set sccwrap_op [string trimright [regsub -all {\[|\]} $scc_op _] _]
      lappend cp_op_ports($scc_wrap) [list ${sccwrap_op} ${scc_op} ${scc}]
    }

  }
  set c_file_strng ""
  foreach scc_wrap [lsort -unique -dictionary [array names cp_ip_ports]] {
    set start 1
    foreach elem [set cp_ip_ports($scc_wrap)] {
      foreach {sccwrap_ip scc_ip scc} $elem {}
      if {$start} {
        append c_file_strng "#IN Conn info from $scc_wrap to $scc\n"
        set start 0
      }
      collage_message_print CRT-003 "Processing input pin $scc_ip on SCC $scc" 
      append c_file_strng "CP ${sccwrap_ip} in {} ${scc_wrap}\n" 
      append c_file_strng "Ci ${scc_wrap}/${sccwrap_ip} ${scc}/${scc_ip}\n" 
    }
    set start 1
    foreach elem [set cp_op_ports($scc_wrap)] {
      foreach {sccwrap_op scc_op scc} $elem {}
      if {$start} {
        append c_file_strng "#OUT Conn info from $scc_wrap to $scc\n"
        set start 0
      }
      append c_file_strng "CP ${sccwrap_op} out {} ${scc_wrap}\n" 
      append c_file_strng "Ci ${scc_wrap}/${sccwrap_op} ${scc}/${scc_op}\n" 
    }
  }
  set fp_wr [open $out_file w]
  puts $fp_wr ${c_file_strng}
  close $fp_wr
}

proc gen_ci_from_sccwrapper_to_scc {} {
}

## This is to overwrite the procedure written earlier
proc collage_generate_scan_clock_specs { args } {

  # Populating connectivity information
  set all_hier_names [list]
  set leaf_ips [collage_get_leaf_ips]
  foreach hier [obviate get_hiers] {
    lappend all_hier_names [$hier get_full_hier_name]
  }
  parse_proc_arguments -args $args opts
  set fn $opts(-scan_spec_file)

  set debug_only 0
  if {[info exists opts(-debug_only)]} {
    set debug_only 1
  }
  
  set sink 0
  if {[info exists opts(-sink_side)]} {
    set sink 1
  }

  # ------------------------------------------------------
  # naming conventions (to be made options to this procedure 
  # with default values as used below
  # ------------------------------------------------------
  set in_clk_wire_suffix "_fscan_func_preclk"
  set out_clk_wire_suffix "_ascan_func_postclk"

  set fscan_clk_pin "fscan_func_preclk"
  set ascan_clk_pin "ascan_func_postclk"  

  if {[info exists opts(-fscan_clk_pin)]} {
    set fscan_clk_pin $opts(-fscan_clk_pin) 
  }

  if {[info exists opts(-ascan_clk_pin)]} {
    set ascan_clk_pin $opts(-ascan_clk_pin) 
  }

  set scan_ip_pattern "_scan"
  if {[info exists opts(-scan_ip_pattern)]} {
    set scan_ip_pattern $opts(-scan_ip_pattern)
  }

  set in_mux_clk_wire_suffix "_fscan_func_genclk"
  set fscan_mux_clk_pin "scan_clk_mux_fscan_func_genclk"
  set ascan_mux_clk_pin "scan_clk_mux_ascan_func_postclk"


  set fscan_mux_sel_pin "scan_clk_mux_fscan_func_genclk_enb"
  set fscan_mux_scc_pin "scan_clk_mux_fscan_scc_clk"
  set fscan_mode_pin "clstr_fscan_mode"

  set in_rst_wire_suffix "_fscan_pre_src"
  set fscan_rst_pin "fscan_src_byprst_b"
  if {[info exists opts(-fscan_rst_pin)]} {
    set fscan_rst_pin $opts(-fscan_rst_pin)
  }
  
  set ascan_rst_pin "ascan_rst_b"
  if {[info exists opts(-ascan_rst_pin)]} {
    set ascan_rst_pin $opts(-ascan_rst_pin)
  }

  set scan_system_sfx "_scan"

  # ------------------------------------------------------
  # Generic code below, no name assumptions should be made
  # ------------------------------------------------------
  array unset clock_roots 
  array set clock_roots {}

  array unset scc_override
  array set scc_override {}

  set out_fn scan_spec_file_debug.txt
  if {[info exists opts(-out_fn)]} {
    set out_fn $opts(-out_fn)
  }
  
  set debug_fn scc_errors.txt
  if {[info exists opts(-out_debug_fn)]} {
    set debug_fn $opts(-out_debug_fn)
  }
  set out_fh [open $out_fn w]
  set debug_fh [open $debug_fn w]

  set cnt 0
  sv_for_file l $fn {
    incr cnt
    set l [string trim $l]
    if {$l == ""} {continue} 
    if {[regexp {^\#} $l]} { continue }
    
    set l_rest [sv_lassign $l kw]
    if { ($kw == "CLK") || ($kw == "GEN_CLK") || ($kw == "RST") }  {
      sv_lassign $l_rest src_pins idx scan_ip ip op comment
      
      ##. More than one endpoint pin can be driven by same SCC index
      foreach src_pin $src_pins {
	#. Assuming partitions are immediately below SoC
	sv_lassign [split $src_pin "/"] src_inst_name src_pin_name
	set partition [lindex [split [collage_get_ip_hier_par -ip_name $src_inst_name] "/"] 0]
      
	#. Trace driver pin for the receiver within the partition
	puts "Processing source pin $src_pin on par $partition"
        
        set root_pin [collage_native_trace_pin_connection2driver $src_pin]
	print_info "Root pin of $src_pin is $root_pin"
      
	if { $root_pin == "" } {
	  print_warning "Not able to trace the clock pin. Check if the clock pin was connected earlier"
	  puts $debug_fh "Not able to trace existing connection on clock pin $src_pin"
	} else {	
	  if { [info exists clock_roots($root_pin)] } {
	    lappend clock_roots($root_pin) [list $src_pin $idx]
	  } else {
	    set clock_roots($root_pin) [list [list $src_pin $idx]]
	  }
	}
      
	if { $scan_ip != "" } {
	  set scc_override($src_pin) $scan_ip
	}
      }
    }  
  }

  ##. Caching partition to SCC information for speedup
  array unset par2scc 
  array set par2scc {}

  ##. Caching connected SCC info for error checking
  array unset connectedsccpin 
  array set connectedsccpin {}
  set C_commands [list]
  foreach clock_root [array names clock_roots] {
    ##. Assuming driver and receivers are in same partition
    puts "Clock root is $clock_root"  
    set clk_child [lindex [lindex $clock_roots($clock_root) 0] 0]
    #set hier_clk_child [collage_get_hier_name -ip_name $clk_child]
    #set sp_clk_root [split [string trimleft $hier_clk_child "/"] "/"]
    #set clk_child_ip_name [lrange $sp_clk_root 0 end-1]
    #set clk_hier_pin_name [join [lrange $sp_clk_root 1 end] "/"]
    set clk_pin_name $clock_root
    set split_clk_child [split $clk_child /]
    set partition [lindex [split [collage_get_hier_name -ip_name [lindex $split_clk_child 0]] /] 0]
    #sv_lassign [split $clock_root "/"] clk_inst_name clk_pin_name
    #set partition [lindex [split [string trimleft [collage_get_hier_name -ip_name $clk_inst_name] /] "/"] 0]
    #if {[llength $clk_ip_name] > 1 } {
    #  set clk_pin_name $clk_hier_pin_name
      #set clk_pin_name [join [lrange [split [string trimleft [collage_get_hier_name -ip_name $clk_inst_name] /] "/"] 1 end] "/"]/$clk_pin_name
    #}

    if { $partition == "" } {
      print_warning "No root for $clock_roots($clock_root)"
      continue
    }

    puts "Connecting $clk_pin_name --> $clock_roots($clock_root)"
    puts "Partition of clk $clk_pin_name is $partition $clk_child"

    ##. Find the SCC MUX
    if { ![info exists par2scc($partition)] } {
      set leafs [collage_get_leaf_ips -hierarchy $partition]
      set scan_ip [lindex $leafs [lsearch -regexp $leafs $scan_ip_pattern] 0]
      if { $scan_ip == "" } {
	error "No scan subsystem exists for the pin $clk_pin_name"
      }
      set par2scc($partition) $scan_ip
    } else {
      set scan_ip $par2scc($partition)
    }
 
    set relative_scan_path [join [lrange [split [collage_get_hier_name -ip_name $scan_ip] "/"] 1 end] "/"]

    ##Soma puts $out_fh "set_current_component -quiet $partition"
    ##Soma set_current_component $partition 
    foreach endpoint $clock_roots($clock_root) {
      set endpoint_pin [lindex $endpoint 0]
      set endpoint_ip [lindex [split $endpoint_pin /] 0]
      set clk_ip [lindex [split $clk_pin_name /] 0]
      puts "Endpoint pin is $endpoint_pin"
      set idx [lindex $endpoint 1]
      if {([collage_is_mi_inst -name $endpoint_ip] && ![::col::is_readonly_inst $endpoint_ip]) || (![collage_is_mi_inst -name $endpoint_ip])} {
      puts $out_fh "D $endpoint_pin" 
      }
      ##. user has provided SCC mapping
      if { [info exists scc_override($endpoint_pin)] } {
	set scan_ip $scc_override($endpoint_pin)
	#Soma set relative_scan_path [join [lrange [split [collage_get_hier_name -ip_name $scan_ip] "/"] 1 end] "/"]
      }
      # Make the connections to SCC wrapper
      if {[collage_is_mi_inst -name $scan_ip]} {
        foreach {scan_ip scanwrap} [join [find_scc_wrapper ${scan_ip} cdu* $leaf_ips]] {}
        set ip ${scanwrap}/${fscan_clk_pin}_${idx}
        set op ${scanwrap}/${ascan_clk_pin}_${idx}
      } else {
        set ip ${scan_ip}/${fscan_clk_pin}\[${idx}\] 
        set op ${scan_ip}/${ascan_clk_pin}\[${idx}\]
      }
      set c_command "C $clk_pin_name $ip"
      if {[lsearch -exact $C_commands $c_command] == -1} {
        puts $out_fh $c_command 
        lappend C_commands $c_command
      }
      if {([collage_is_mi_inst -name $endpoint_ip] && ![::col::is_readonly_inst $endpoint_ip]) || (![collage_is_mi_inst -name $endpoint_ip])} {
        set c_command "C $op $endpoint_pin"
	if {[lsearch -exact $C_commands $c_command] == -1} {
	  puts $out_fh $c_command 
	  lappend C_commands $c_command
	}
      }
      continue 
      unset -nocomplain relative_endpoint_path 
      collage_eval_in_comp "/" {
        set endpont_ip [lindex [split $endpoint_pin "/"] 0]
        set only_pin [lindex [split $endpoint_pin "/"] end]
        set h [collage_get_hier_name -ip_name $endpont_ip]
        set rp [join [lrange [split $h "/"] 1 end] "/"]
        set rp "$rp/$only_pin"
        set relative_endpoint_path $rp
        #set relative_endpoint_path [join [lrange [split [::col::get_pin_hier_name $endpoint_pin] "/"] 1 end] "/"]
        puts "Relative end point path0 is $relative_endpoint_path with $endpoint_pin and $rp" 
      }
      puts "Relative end point path1 is $relative_endpoint_path" 
      set ip ${relative_scan_path}/${fscan_clk_pin}\[${idx}\]
      set op ${relative_scan_path}/${ascan_clk_pin}\[${idx}\]
      puts "Ip is $ip and OP is $op"

      set sccpinconnected 0
      if { [info exists connectedsccpin($ip)] } {
	if { $connectedsccpin($ip) != $clk_pin_name } {
	  print_warning "SCC pin $ip already associated to root clock $connectedsccpin($ip)"
	  puts $debug_fh "SCC pin $ip already associated to root clock $connectedsccpin($ip) and now being associated to $clk_pin_name"
	  continue
	}

	set sccpinconnected 1
      }

      set connectedsccpin($ip) $clk_pin_name

      if { !$sccpinconnected } {
	puts $out_fh "create_connection -hierarchy \"$clk_pin_name  $ip\""      
	create_connection -hierarchy "$clk_pin_name  $ip"
      }

      puts $out_fh "remove_connection -hierarchy $relative_endpoint_path"
      remove_connection -hierarchy $relative_endpoint_path

      ##. CoreTools expect that you dive to the common ancestor or use full path 
      if {([lindex [split $relative_endpoint_path "/"] 0] != [lindex [split $op "/"] 0]) || ([lindex [split $h "/"] 0] == $partition)} {
	puts "Wrong !! "
        puts $out_fh "create_connection -hierarchy \"$relative_endpoint_path  $op\""
	create_connection -hierarchy "$relative_endpoint_path  $op"
      } else {
	puts "Correct !! "
	puts $out_fh "create_connection -hierarchy \"$partition/$relative_endpoint_path  $partition/$op\""
	create_connection -hierarchy "$partition/$relative_endpoint_path  $partition/$op"
      }
    }

    #puts $out_fh "set_current_component -quiet "
    puts $out_fh "\n"

    set_current_component -quiet 
  }

  close $out_fh
  close $debug_fh
}

define_proc_attributes collage_generate_scan_clock_specs \
    -info "Collage - Process scan spec file and route scan clk/rst via scan blocks" \
    -define_args {
      {"-scan_spec_file"       "File in which tap network is specified"   "" string required}
      {"-scan_ip_pattern"       "Name of scan ip pattern"   "_scan" string optional}
      {"-fscan_clk_pin"       "Name of scan clk pin (pre-scan)"   "fscan_func_preclk" string optional}
      {"-ascan_clk_pin"       "Name of scan clk pin (post-scan)"   "ascan_func_postclk" string optional}
      {"-fscan_rst_pin"       "Name of scan rst pin (pre-scan)"   "fscan_func_preclk" string optional}
      {"-ascan_rst_pin"       "Name of scan rst pin (post-scan)"   "ascan_func_postclk" string optional}
      {"-sink_side" "" "" boolean optional}
      {"-debug_only" "" "" boolean optional}
      {"-out_debug_fn"      "Output debug file which captures all error messages during SCC insertion"   "scc_errors.txt" string optional}
      {"-out_fn"      "Output file which captures all SCC insertion execution commands."   "scan_spec_file_debug.txt" string optional}
    }

proc scc_pre_steps {} {
  #collage_obv_wrapper init 1
  puts "Generating CP CI lines from scan IP's to Scan Wrappers for the entire SOC starts"
  gen_cp_for_sccwrapper scc_wrapper_spec_file_debug.txt
  puts "Generating CP CI lines from scan IP's to Scan Wrappers for the entire SOC ends"
  collage_process_conn_file -file scc_wrapper_spec_file_debug.txt
}
proc scc_post_steps {} {
  #collage_obv_wrapper init 1
}
