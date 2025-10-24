###############################################################################
# Remove when moving to 2.6.4 or newer
###############################################################################


###############################################################################
# 

proc chassis_optimize_orgate_ports { args } {
  parse_proc_arguments -args $args opts
  
  set orgate_template $opts(-orgate_template)
  
  set_current_component -quiet
  foreach_in_collection d [find_item -quiet -type design -filter "IsHierarchicalComponent==0 && Name==$orgate_template"] {

    foreach_in_collection c [get_attribute -attrs CellReferences $d] {

      ##. Get the shared ports on orgate
      array unset sharedPort
      array set sharedPort {}
      array unset sharedIfcPort
      array set sharedIfcPort {}

      foreach_in_collection ifc_i [find_interface_instances -component $c -filter {Visible==1 && InterfaceDefinitionName==IOSF::Primary && InterfaceType==consumer}] {
	set ifc_name [get_attribute $ifc_i -attr Name]
	foreach_in_collection ac [get_attribute $ifc_i -attr AllChildren] {
	  if {[get_attribute $ac -attr TypeName] == "interfacePort"} {
	    set rtl_pin_name [get_attribute $ac -attr InterfaceLink]

	    if { ($rtl_pin_name=="<open>") || ($rtl_pin_name=="")} { continue }

	    #Skip fromProvider pins
	    set direction [get_attribute $ac -attrs LinkDirection]
	    #if { $direction == "fromProvider" } {continue}


	    set interface_port [get_attribute $ac -attr UserName]
	    if { ![info exists sharedPort($rtl_pin_name)] } { 
	      set sharedPort($rtl_pin_name) $ifc_name
	      set sharedIfcPort($interface_port) $ifc_name
	    } else {
	      lappend sharedPort($rtl_pin_name) $ifc_name
	      lappend sharedIfcPort($interface_port) $ifc_name
	    }
	  }
	}
      }


      ##. Update interfaceLink on all crossing partition
      foreach_in_collection ifc_i [find_interface_instances -component $c -filter {Visible==1 && InterfaceDefinitionName==IOSF::Primary && InterfaceType==consumer}] {
	set consumer_ip [lindex [split [get_attribute $ifc_i -attr InterfaceComponent] "%"] end]
	
	set pifci [get_attribute $ifc_i -attr TargetConnection -sub Provider]
	if { [sizeof_collection $pifci] == 0 } { continue }	
	
	set pname [get_attribute $pifci -attr Name]
	set provider_ip [lindex [split [get_attribute $pifci -attr InterfaceComponent] "%"] end]

	foreach par [collage_get_par_names] {
	  #set_current_component -quiet 
	  set_current_component -quiet [collage_get_hier_name -ip_name $par]

	  foreach_in_collection exp_ifc [find_interface_instances -exported -filter {Visible==1 && InterfaceDefinitionName==IOSF::Primary}] {
	    set match 0

	    if { [get_attribute $exp_ifc -attr InterfaceType] == "consumer" } {
	      set npifci [get_attribute $exp_ifc -attr TargetConnection -sub Provider]
              set ncifci [get_attribute $npifci -attr TargetConnection -sub Consumers]
	      if { ([get_attribute $npifci -attr UserName] == [get_attribute $pifci -attr UserName]) && ([get_attribute $ncifci -attr UserName] == [get_attribute $ifc_i -attr UserName]) } {
		set match 1
	      }
	    }

	    if { [get_attribute $exp_ifc -attr InterfaceType] == "provider" } {
	      set ncifci [get_attribute $exp_ifc -attr TargetConnection -sub Consumers]
	      set npifci [get_attribute $ncifci -attr TargetConnection -sub Provider]
              if { ([get_attribute $npifci -attr UserName] == [get_attribute $pifci -attr UserName]) && ([get_attribute $ncifci -attr UserName] == [get_attribute $ifc_i -attr UserName]) } {
		set match 1
	      }
	    }


	    if { !$match } { 
		continue 
	    }

	    set exp_ifc_name [get_attribute $exp_ifc -attr Name]
	    foreach_in_collection ac [get_attribute $exp_ifc -attr AllChildren] {
	      if {[get_attribute $ac -attr TypeName] == "interfacePort"} {
		set rtl_pin_name [get_attribute $ac -attr InterfaceLink]
		if { ($rtl_pin_name=="<open>") || ($rtl_pin_name=="")} { continue }
		
		#set string trim $rtl_pin_name "${exp_ifc_name}_"
		set interface_port [get_attribute $ac -attr UserName]
		if { ![info exists sharedIfcPort($interface_port)] } { continue }

		if { [llength $sharedIfcPort($interface_port)] == 1 } { continue }
	       
		set_attribute -attr InterfaceLink -value ${provider_ip}_${consumer_ip}_${interface_port} $ac
		print_info "set_attribute -attr InterfaceLink -value ${provider_ip}_${consumer_ip}_${interface_port} $ac"
	      }
	      
	    }
	  }
	    
	    set_current_component -quiet
         }
      }
    }
    
  }
}

define_proc_attributes chassis_optimize_orgate_ports \
    -info "Collage - Merge interface ports on partitions which originate from common RTL port on orgate..." \
    -define_args {
      {"-orgate_template"    "Name of the orgate template" "" string required}
    }
