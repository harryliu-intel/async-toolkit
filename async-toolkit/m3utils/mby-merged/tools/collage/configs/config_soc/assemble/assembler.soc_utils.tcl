proc collage_merge_common_ifc_ports { args } {
  parse_proc_arguments -args $args opts
  
  set interface_defn $opts(-ifc_defn)

  ##. Use smaller RTL port names on intermediate hierarchies
  ##. when different interfaces are driven from same ports on driver IP (example: RTDR CTRL)
  set one_to_many 0
  if { [info exists opts(-one_to_many)] } {
    set one_to_many 1
  }

  foreach par [concat [collage_get_par_names] "/"] {
    eval_in_component [collage_get_hier_name -ip_name $par] {

      ##. Collection of interfaces which need optimization
      ##. Interface originating from same root IP are grouped
      array unset interface_grp
      array set interface_grp {}
      foreach_in_collection exp_ifc [find_interface_instances -exported -filter "Visible==1 && InterfaceDefinitionName==$interface_defn"] {
	set pifci [get_attribute $exp_ifc -attr TargetConnection -sub Provider]
	set provider_ip [lindex [split [get_attribute $pifci -attr InterfaceComponent] "%"] end]
	
	set cifci [index_collection [get_attribute $pifci -attr TargetConnection -sub Consumers] 0]
	set consumer_ip [lindex [split [get_attribute $cifci -attr InterfaceComponent] "%"] end]
	
	##. Needed for subsystem
	if {$consumer_ip == ""} {
	  set consumer_ip [get_attribute [current_design] -attr Name]
	}

	set match 0
	##. Add the interface into existing collection if its connnection match existing one
	foreach key [array names interface_grp] {
	  foreach element $interface_grp($key) {
	    set connection [lindex $element 0]
	    if { ([lsearch -exact $connection $provider_ip] > -1) || ([lsearch -exact $connection $consumer_ip] > -1) } { 
	      lappend interface_grp($key) [list [list $provider_ip $consumer_ip] [get_attribute $exp_ifc -attr UserName]]
	      set match 1
	      break
	    }
	  }
	}

	##. Else create a new connection
	if { !$match } {
	   lappend interface_grp($provider_ip) [list [list $provider_ip $consumer_ip] [get_attribute $exp_ifc -attr UserName]]
	}
      }


      ##. Optimize ports for Interface Group
      foreach key [array names interface_grp] {
	if { [llength $interface_grp($key)] == 1 } { continue }

	##. Get Interface ports on the partition driven from same RTL port
	foreach element $interface_grp($key) {
	  set exp_ifc_name [lindex $element end]

	  set exp_ifc [find_interface_instances -exported -filter "Visible==1 && InterfaceDefinitionName==$interface_defn && UserName==$exp_ifc_name"]
	  set pifci [get_attribute $exp_ifc -attr TargetConnection -sub Provider]
	  set cifci [index_collection [get_attribute $pifci -attr TargetConnection -sub Consumers] 0]
	  ##. One to many connections
	  if { [sizeof_collection [get_attribute $pifci -attr TargetConnection -sub Consumers]] > 1 } {
	    set cifci $pifci
	  }

	  foreach_in_collection ac [get_attribute $exp_ifc -attr AllChildren] {
	    if {[get_attribute $ac -attr TypeName] == "interfacePort"} {
	      set rtl_pin_name [get_attribute $ac -attr InterfaceLink]
	      if { ($rtl_pin_name=="<open>") || ($rtl_pin_name=="")} { continue }
	      
	      #set string trim $rtl_pin_name "${exp_ifc_name}_"
	      set interface_port [get_attribute $ac -attr UserName]

	      ##. Needed to find driver port (look towards provider or consumer)
	      set direction [get_attribute $ac -attr LinkDirection]

	      ##. Get the originating RTL pin name on provider IP
	      if { $direction == "fromConsumer" } {
		foreach_in_collection pac [get_attribute $cifci -attr AllChildren] {
		  if {[get_attribute $pac -attr TypeName] == "interfacePort"} {
		    
		    if { [get_attribute $pac -attr UserName] == $interface_port } {
		      set driver_rtl_port [get_attribute $pac -attr InterfaceLink]
		    }
		  }
		}
	      } else {
		foreach_in_collection pac [get_attribute $pifci -attr AllChildren] {
		  if {[get_attribute $pac -attr TypeName] == "interfacePort"} {
		    
		    if { [get_attribute $pac -attr UserName] == $interface_port } {
		      set driver_rtl_port [get_attribute $pac -attr InterfaceLink]
		    }
		  }
		}
	      }

	      if { [regexp {@SlotNumber} $driver_rtl_port] } { continue }

	      if { ![info exists rtl_driver_map($driver_rtl_port)] } {
		set rtl_driver_map($driver_rtl_port) [get_attribute $exp_ifc -attr UserName]
	      } else {
		lappend rtl_driver_map($driver_rtl_port) [get_attribute $exp_ifc -attr UserName]
	      }	      
	    }
	  }
	}

	#parray interface_grp
	#parray rtl_driver_map

	##. Optimize
	foreach element $interface_grp($key) {
	  set exp_ifc_name [lindex $element end]
	  set exp_ifc [find_interface_instances -exported -filter "Visible==1 && InterfaceDefinitionName==$interface_defn && UserName==$exp_ifc_name"]
	  set pifci [get_attribute $exp_ifc -attr TargetConnection -sub Provider]
	  set cifci [index_collection [get_attribute $pifci -attr TargetConnection -sub Consumers] 0]
	  ##. One to many connections
	  if { [sizeof_collection [get_attribute $pifci -attr TargetConnection -sub Consumers]] > 1 } {
	    set cifci $pifci
	  }

	  foreach_in_collection ac [get_attribute $exp_ifc -attr AllChildren] {
	    if {[get_attribute $ac -attr TypeName] == "interfacePort"} {
	      set rtl_pin_name [get_attribute $ac -attr InterfaceLink]
	      if { ($rtl_pin_name=="<open>") || ($rtl_pin_name=="")} { continue }
	      
	      #set string trim $rtl_pin_name "${exp_ifc_name}_"
	      set interface_port [get_attribute $ac -attr UserName]

	      ##. Needed to find driver port (look towards provider or consumer)
	      set direction [get_attribute $ac -attr LinkDirection]

	      ##. Get the originating RTL pin name on provider IP
	      set driver_ip ""
	      if { $direction == "fromConsumer" } {
		set driver_ip [lindex [split [get_attribute $cifci -attr InterfaceComponent] "%"] end]
		foreach_in_collection pac [get_attribute $cifci -attr AllChildren] {
		  if {[get_attribute $pac -attr TypeName] == "interfacePort"} {
		    
		    if { [get_attribute $pac -attr UserName] == $interface_port } {
		      set driver_rtl_port [get_attribute $pac -attr InterfaceLink]
		    }
		  }
		}
	      } else {
		set driver_ip [lindex [split [get_attribute $pifci -attr InterfaceComponent] "%"] end]
		##. Get the originating RTL pin name on provider IP
		foreach_in_collection pac [get_attribute $pifci -attr AllChildren] {
		  if {[get_attribute $pac -attr TypeName] == "interfacePort"} {
		    
		    if { [get_attribute $pac -attr UserName] == $interface_port } {
		      set driver_rtl_port [get_attribute $pac -attr InterfaceLink]
		    }
		  }
		}
	      }

	      if { [regexp {@SlotNumber} $driver_rtl_port] } { continue }
	      #if { ($driver_rtl_port=="<open>") || ($driver_rtl_port=="")} { continue }

	      ##. Interface port is driven by dedicated RTL port
	      if { [llength $rtl_driver_map($driver_rtl_port)] == 1 } { continue }
	      
	      set driver_rtl_port [regsub -all {\[|:|\]} $driver_rtl_port {_}]
	      set driver_rtl_port [regsub -all {_$} $driver_rtl_port {}]

	      if { $one_to_many } {
		set_attribute -attr InterfaceLink -value ${driver_ip}_${interface_port} $ac
		print_info "set_attribute -attr InterfaceLink -value ${driver_ip}_${interface_port} $ac"
	      } else {
                if {($driver_rtl_port=="<open>") || ($driver_rtl_port=="")} {
                  set_attribute -attr InterfaceLink -value ${driver_ip}_${interface_port} $ac
                  print_info "set_attribute -attr InterfaceLink -value ${driver_ip}_${interface_port} $interface_port" 
                } else {
		  set_attribute -attr InterfaceLink -value ${driver_ip}_${driver_rtl_port}_${interface_port} $ac
		  print_info "set_attribute -attr InterfaceLink -value ${driver_ip}_${driver_rtl_port}_${interface_port} $interface_port"
                }
	      }
	    }
	  }
	}
      }
    }
  }; #Each partition
}

define_proc_attributes collage_merge_common_ifc_ports \
    -info "Collage - Merge interface ports on partitions which originate from common RTL port..." \
    -define_args {
      {"-ifc_defn"   "Interface definition name like IOSF::DFX::SCAN" "" string required}
      {"-one_to_many"   "Multiple interfaces driven by same set of common driven pins" "" boolean optional}
    }

#proc collage_export_unconnected_ports {args} {
#  parse_proc_arguments -args $args opts
#
#  set context $opts(-context)
#  set_current_component -quiet [collage_get_hier_name -ip_name $context]
#
#  print_info "Exporting unconnected pins inside $context..."
#  foreach_in_collection c [find_item -quiet -type cell] {
#    set leaf [get_attribute $c -attr UserName]
#
#    set prefix $leaf
#    if { [info exists opts(-prefix)] } {
#      set prefix $opts(-prefix)
#    }
#
#    foreach_in_collection upin [find_item -quiet -type port $leaf/*] {
#      #set_attribute $upin -attr BlastBus 1
#      set chcnt [get_attribute $upin -attr HasCnctChildren]
#      set upin_bits [get_attribute $upin -attr Children]
#      #.Scalar
#      if {$upin_bits == {}} {
#        sv_lassign [get_attribute $upin -attrs "UserName UserName PortDirection"] upin_rd_user_name upin_user_name upin_dir
#        
#        #set upin_user_name [get_attribute $upin -attr UserName]
#        if {[get_attribute $upin -attr IsConnected] == 0} {
#          set msb [get_attribute $upin -attrs InterfacePortStart]
#          set lsb [get_attribute $upin -attrs InterfacePortEnd]
#          #. Export
#          set inst [lindex [split $upin_rd_user_name "/"] 0]
#          set top_pin [lindex [split $upin_rd_user_name "/"] end]
#          set top_pin [regsub -all {\[|\]|:|\.} $top_pin {_}] ; # remove "." and "[]"
#          set top_pin [regsub {_$} $top_pin {}] ; # remove trailing underscore
#          set top_pin [regsub -all {__} $top_pin {_}] ; # remove double underscore
#          if { [regexp $pattern $top_pin match] || [regexp $pattern1 $top_pin match] || [regexp $pattern2 $top_pin match]} {
#            set prefix $leaf
#            export_pin -auto_dim -port ${prefix}_${top_pin} $upin_rd_user_name -comment "Tool auto exported pin"
#          } else {
#            export_pin -auto_dim -port ${top_pin} $upin_rd_user_name -comment "Tool auto exported pin"
#          }
#        } 
#        continue
#      }
#    }
#  }
#
#  set_current_component -quiet
#}
#
#
#define_proc_attributes collage_export_unconnected_ports \
#    -info "Collage - Export unconnected ports..." \
#    -define_args {
#      {"-context"    "Context at which to run" "/" string required}
#      {"-prefix"     "Prefix to add on the exported pin" "" string optional}
#    }
#


proc create_power_pins {} {
  foreach par [concat [collage_get_par_names] "/"] {
    eval_in_component [collage_get_hier_name -ip_name $par] {
      foreach_in_collection ifc [find_item -type pin -quiet */vss] {
	set name [get_attribute -attr Name $ifc]
	export_pin -auto_dim -port vss $name
      }
      foreach_in_collection ifc [find_item -type pin -quiet */vcc*1p8*] {
	set name [get_attribute -attr Name $ifc]
	export_pin -auto_dim -port vcc1p8 $name
      }
      foreach_in_collection ifc [find_item -type pin -quiet */vcc*3p3*] {
	set name [get_attribute -attr Name $ifc]
	export_pin -auto_dim -port vcc3p3 $name
      }
      foreach_in_collection ifc [find_item -type pin -quiet */vcc*1p0*] {
	set name [get_attribute -attr Name $ifc]
	export_pin -auto_dim -port vcccore1p0 $name
      }
    }
  }
}

proc create_clk_pins {} {
    global design
    
  foreach par [concat [collage_get_par_names] "/"] {
    eval_in_component [collage_get_hier_name -ip_name $par] {
      foreach_in_collection ifc [find_item -type pin -quiet */${design}_clk] {
	set name [get_attribute -attr Name $ifc]
	export_pin -auto_dim -port ${design}_clk $name
      }
      foreach_in_collection ifc [find_item -type pin -quiet */rtc_clk] {
	set name [get_attribute -attr Name $ifc]
	export_pin -auto_dim -port rtc_clk $name
      }
    }
  }
}

proc create_rst_pins {} {
  foreach par [concat [collage_get_par_names] "/"] {
    eval_in_component [collage_get_hier_name -ip_name $par] {
      foreach_in_collection ifc [find_item -type pin -quiet */pwrgoodb] {
	set name [get_attribute -attr Name $ifc]
	export_pin -auto_dim -port pwr_good_rst_b $name
      }
      foreach_in_collection ifc [find_item -type pin -quiet */warm_rst_b] {
	set name [get_attribute -attr Name $ifc]
	export_pin -auto_dim -port warm_rst_b $name
      }
      foreach_in_collection ifc [find_item -type pin -quiet */host_rst_b] {
	set name [get_attribute -attr Name $ifc]
	export_pin -auto_dim -port host_rst_b $name
      }
      foreach_in_collection ifc [find_item -type pin -quiet */soc_rst_b] {
	set name [get_attribute -attr Name $ifc]
	export_pin -auto_dim -port soc_rst_b $name
      }
      foreach_in_collection ifc [find_item -type pin -quiet */sbi_rst_b] {
	set name [get_attribute -attr Name $ifc]
	export_pin -auto_dim -port side_rst_b $name
      }
    }
  }
}

proc create_fscan_pins {} {
  foreach par [concat [collage_get_par_names] "/"] {
    eval_in_component [collage_get_hier_name -ip_name $par] {
      foreach_in_collection ifc [find_item -type pin -quiet */fscan_byprst_b] {
	set name [get_attribute -attr Name $ifc]
	export_pin -auto_dim -port pwr_good_rst_b $name
      }
      foreach_in_collection ifc [find_item -type pin -quiet */fscan_clkungate] {
	set name [get_attribute -attr Name $ifc]
	export_pin -auto_dim -port warm_rst_b $name
      }
      foreach_in_collection ifc [find_item -type pin -quiet */fscan_latchclosed_b] {
	set name [get_attribute -attr Name $ifc]
	export_pin -auto_dim -port host_rst_b $name
      }
      foreach_in_collection ifc [find_item -type pin -quiet */fscan_latchopen] {
	set name [get_attribute -attr Name $ifc]
	export_pin -auto_dim -port soc_rst_b $name
      }
      foreach_in_collection ifc [find_item -type pin -quiet */fscan_mode] {
	set name [get_attribute -attr Name $ifc]
	export_pin -auto_dim -port side_rst_b $name
      }
      foreach_in_collection ifc [find_item -type pin -quiet */fscan_rstbypen] {
	set name [get_attribute -attr Name $ifc]
	export_pin -auto_dim -port warm_rst_b $name
      }
      foreach_in_collection ifc [find_item -type pin -quiet */fscan_shiften] {
	set name [get_attribute -attr Name $ifc]
	export_pin -auto_dim -port host_rst_b $name
      }
      foreach_in_collection ifc [find_item -type pin -quiet */fscan_clkgenctrl] {
	set name [get_attribute -attr Name $ifc]
	export_pin -auto_dim -port soc_rst_b $name
      }
      foreach_in_collection ifc [find_item -type pin -quiet */fscan_clkgenctrlen] {
	set name [get_attribute -attr Name $ifc]
	export_pin -auto_dim -port side_rst_b $name
      }

    }
  }
}


#########################################################################
# Hierachy Print 
#########################################################################
proc pch_hier_recurse_print {inst level hiervar fh udff} {
    if {$level == 0} {
      set hiervar `soc.
    }

    echo "Processing $inst .. " 
    set comp_name [get_cell_attribute $inst ReferenceDesignName]
    set cont 1  
    set user_define_filter [open "$udff" "r"]
    while {-1 != [gets $user_define_filter line]} {
         if { [ expr { $line eq $inst } ] } {
           set cont 0 
         }
    }
    close $user_define_filter
    if { $cont == 1 } {
          puts $fh "`define $inst \t\t $hiervar$inst"
    } else {
          echo "Interface $comp_name ($inst) is in fc hier skip list "
    } 

    if { ![regexp {^IF_*} $comp_name] } {
            set_current_component -quiet $inst
            set this_component [get_current_component]
            redirect /dev/null {set children [find_item -type cell -quiet ]}
            if {[sizeof_collection $children] > 0} {
             incr level
                foreach_in_collection child $children {
                 set hiervar "`$inst."
                    set inst_name [get_attribute -attrs Name $child]
                    pch_hier_recurse_print $inst_name $level $hiervar $fh $udff
                    set_current_component -quiet $this_component
                }
            }
    } else { 
          echo "Interface $comp_name ($inst) is not supported , skip for now "
    }  
}


proc gen_hier_defines {file_path user_define_file user_define_filter_file} {
    file mkdir $file_path 
    close [open "$file_path/fc_hier_defines_generated.sv" w]  
    set fh_hier [open "$file_path/fc_hier_defines_generated.sv" "w"]
    set user_define [open "$user_define_file" "r"]
    set top_level_inst {}
    set level 0
    set hierval {}
    set top_level_instances [find_item -type cell]
    foreach_in_collection tl_inst $top_level_instances {
        set_current_component -quiet
        set inst_name [get_attribute -attrs Name $tl_inst]
        set comp_name [get_attribute -attrs ReferenceDesignName $tl_inst]
        pch_hier_recurse_print $inst_name $level $hierval $fh_hier $user_define_filter_file
    }
     set_current_component
     puts $fh_hier [read $user_define]
     close $user_define
     close $fh_hier
}

# Proc to generate hierarchy report in collage
proc pch_generate_hier_report {} {
    set top_level_inst {}
    set level 0
    set top_level_instances [find_item -type cell]

    foreach_in_collection tl_inst $top_level_instances {
        set_current_component -quiet
        set inst_name [get_attribute -attrs Name $tl_inst]
        hier_recurse_print $inst_name $level
    }
    set_current_component
}


proc update_insert_modules_hdl {} {
  #need to copy all files included in the inserted code over to the gen/source/rtl directory
  #add the files to the .hdl
  set inserted_modules [::insert_mod::get_complete_file_list]
  if { $inserted_modules != "" } {
    #only create this directory and do this work if something has been inserted
    set path $::env(COLLAGE_WORK)/gen/source/rtl/inserted_modules
    file mkdir $path
    foreach mod $inserted_modules {
      set name [lindex [split $mod "/"] end]
      set cmd "file copy -force $mod $path/$name"
      puts $cmd
      eval $cmd
    }
    set modules {}
    set inserted_modules [::insert_mod::get_top_file_list]
    foreach mod $inserted_modules {
      set name [lindex [split $mod "/"] end]
      lappend modules "\t\t\"gen/source/rtl/inserted_modules/$name\",\n"
    }
    set my_mods [join $modules]          
    set my_mods [string trimright $my_mods "\n"]          


    set sochdl [file join $::env(COLLAGE_WORK) "gen/source/rtl/${::design}.hdl"]
    file copy -force $sochdl "$sochdl.orig"

    #read each line of file into list
    set line_con [lreplace [split [read [open $sochdl r]] "\n"] end end]
    set i 0
    foreach l $line_con {
      #look for end of vlog_files arrays and insert the inserted modules string
      if [regexp {],} $l] {
        set line_con [linsert $line_con $i $my_mods]
        break
      }
      incr i
    }

    set file  [open $sochdl w]
    #write modified vlog list into the soc.hdl file, do not append newline character at the end of file
    puts -nonewline $file [join $line_con "\n"]
    close $file
  }
}
