proc collage_fpga_stub_override_gen_clks {args} {

  parse_proc_arguments -args $args opts
  set mod_name $opts(-module)
  set out_file $opts(-file)
  set inp_clk $opts(-inp_clk)
  set exclude ""
  if {[info exists opts(-exclude)]} {
    set exclude $opts(-exclude)
  } 

  set out_fh [open $out_file "a"]
  set design_name [find_item -quiet -type design * -filter "UnelabName==$mod_name"]
  if {$design_name eq ""} {
    error "Module name $mod_name does not exist. Please fix and rerun."
  }
  if {[sizeof_collection $design_name] > 1} {
    set design_name [index_collection $design_name 0]
  }
  set inst_name [get_attribute [index_collection [get_attribute $design_name -attrs CellReferences] 0] -attr Name]
  #to make sure not writing the same clock multiple times as there can be many interfaces tagged with same clock
  array unset catch_duplicate
  array set catch_duplicate {}
  
  foreach i [collage_get_objects_with_descriptor_tag -object_type interface -descriptor_tag_name INTERFACE_CLKS] {
    sv_lassign $i ifc clk_name
    sv_lassign [split $ifc "/"] instance_name ifc_name
    if {$instance_name eq $inst_name} {
      if {![info exists ::col::emulation_stub_tieoff($mod_name,$clk_name)] && [lsearch -exact $exclude $clk_name] == -1} {
        if {![info exists catch_duplicate($clk_name)]} { 
          set catch_duplicate($clk_name) 1
          eval_in_component [collage_get_hier_name -ip_name $inst_name] {
            if {[get_attribute [find_item -type port $clk_name] -attr PortDirection] eq "out"} {
              puts $out_fh "set ::col::emulation_stub_tieoff($mod_name,$clk_name)  \"${inp_clk}\""
            }
          }
        }
      }
    } 
  }
  close $out_fh
}

define_proc_attributes collage_fpga_stub_override_gen_clks \
    -info "Collage - generate stub overrides for clocks" \
    -define_args {
      {"-module"   "Module Name "   "" string required}
      {"-inp_clk"   "Input clock"   "" string required}
      {"-exclude"   "Don't generate overrides for these clock"   "" string optional}
      {"-file"  "Output stub override file"   "" string required}
    }

proc collage_fpga_stub_override_gen_clkack {args} {

  parse_proc_arguments -args $args opts
  set mod_name $opts(-module)
  set out_file $opts(-file)
  set exclude ""
  if {[info exists opts(-exclude)]} {
    set exclude $opts(-exclude)
  } 
  set loopback 0
  if {[info exists opts(-loopback)]} {
    set loopback 1 
  } 

  set out_fh [open $out_file "a"]
  set design_name [find_item -quiet -type design * -filter "UnelabName==$mod_name"]
  if {$design_name eq ""} {
    error "Module name $mod_name does not exist. Please fix and rerun."
  }
  if {[sizeof_collection $design_name] > 1} {
    set design_name [index_collection $design_name 0]
  }
  set inst_name [get_attribute [index_collection [get_attribute $design_name -attrs CellReferences] 0] -attr Name]
  
  foreach i [collage_get_objects_with_descriptor_tag -object_type interface -descriptor_tag_name INTERFACE_CLKS] {
    sv_lassign $i ifc clk_name
    sv_lassign [split $ifc "/"] instance_name ifc_name
    if {$instance_name eq $inst_name} {
      eval_in_component [collage_get_hier_name -ip_name $inst_name] {
        set req [get_interface_link [find_item -type interfacePort [get_attribute [find_interface_instances -component $inst_name -name $ifc_name] -attrs AllChildren] -filter "Name==CLK_REQ"]]
        set ack [get_interface_link [find_item -type interfacePort [get_attribute [find_interface_instances -component $inst_name -name $ifc_name] -attrs AllChildren] -filter "Name==CLK_ACK"]]
        set ack_port_dir [get_attribute [find_item -type port $ack] -attr PortDirection]
        #check whether needed for output ack ports only??
        if { ($ack!="<open>") && ($ack!="") && $ack_port_dir eq "out" && ![info exists ::col::emulation_stub_tieoff($mod_name,$ack)] && [lsearch -exact $exclude $ack] == -1} { 
          if {!$loopback} {
             puts $out_fh "set ::col::emulation_stub_tieoff($mod_name,$ack)  \"'b1\""
          } else {
             if {$req == "<open>" || $req == ""} {error "Loopback not possible as there is no rtl port corresponding to CLK_REQ interface port of interface $ifc_name on instance $instance_name"}
             set_attribute [find_item -type port $req] -attr IfUnconnected -value one
             puts $out_fh "set ::col::emulation_stub_tieoff($mod_name,$ack)  \"${req}\""
          }  
        }
      }
    } 
  }
  close $out_fh
}

define_proc_attributes collage_fpga_stub_override_gen_clkack \
    -info "Collage - generate stub overrides for clkack" \
    -define_args {
      {"-module"   "Module Name "   "" string required}
      {"-loopback"   "Connect ack to req and req to 1 if unconnected"   "" boolean optional}
      {"-exclude"   "Don't generate overrides for these ports"   "" string optional}
      {"-file"  "Output stub override file"   "" string required}
    }

proc collage_check_stub_overrides {} {

  set err_found 0
  foreach mod_port_pair [array names ::col::emulation_stub_tieoff] {
    set mod_name [lindex [split $mod_port_pair ","] 0]
    set port_name [lindex [split $mod_port_pair ","] 1]

    set design_name [find_item -quiet -type design * -filter "UnelabName==$mod_name"]
    if {$design_name eq ""} {
      error "Module name $mod_name does not exist. Please fix module name in stub overrides and rerun."
    }
    if {[sizeof_collection $design_name] > 1} {
      set design_name [index_collection $design_name 0]
    }
    set inst_name [get_attribute [index_collection [get_attribute $design_name -attrs CellReferences] 0] -attr Name]

    eval_in_component [collage_get_hier_name -ip_name $inst_name] {
      set p_name [find_item -quiet -type port $port_name]
      if {$p_name eq ""} {
        set err_found 1
        print_error "Stub Overrides - Port name $port_name for module $mod_name does not exist."
      }
    }
  }
  if {$err_found} {
    error "Please fix above Stub Overrides error and rerun."
  }
}



