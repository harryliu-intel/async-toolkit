#############################################################
# Limited - not much error handling - equivalent of for_file from Tclx
#############################################################
proc sv_for_file {var filename cmd} {
  upvar 1 $var line
  set fp [open $filename r]
  while {[gets $fp line] >= 0} {
    #set code [catch {uplevel 1 $cmd} result]
    uplevel 1 $cmd
  }
  close $fp
}

#############################################################
# Equivalent of lassign from Tclx
#############################################################
proc sv_lassign {values args} {
  set vlen [llength $values]
  set alen [llength $args]
  for {set i $vlen} {$i < $alen} {incr i} { ;# Make lists equal length
    lappend values {}
  }
  uplevel 1 [list foreach $args $values break]

  return [lrange $values [llength $args] end]
}


#############################################################
# 
#############################################################
proc sv_dc_read_rtl_unit {} {
  set design "soc"

  set stubs_dir $::env(CONFIG_GEN)/stubs/fp_netlist/
  sv_dc_read_rtl $stubs_dir $design

  # Write soc clock net names
  sv_dc_write_clock_nets $::env(CONFIG_GEN)/../reports/${design}.clock_pins.par.txt outputs/soc.clock_nets.txt
  #sv_dc_write_clock_nets $::env(CONFIG_GEN)/../reports/${design}.clock_pins.unit.txt outputs/

  # Write soc net connectivity report
  sv_dc_write_net_conn $::env(CONFIG_GEN)/../reports/${design}.net_connectivity.txt 0
  sv_dc_write_net_conn $::env(CONFIG_GEN)/../reports/${design}.net_hier_connectivity.txt 1
  
  # Write mpins report
  sv_dc_write_mpins  $::env(CONFIG_GEN)/../reports/${design}.par_mpins.txt

  report_hierarchy -nosplit
  
  # Flatten the partitions to get top level only stub
  # - note: this has the downside of tran and assign statements added so it is not very useful 
  #         for taking into ICC-DP
  # sv_blackbox_pars
  # write_file -format verilog -hierarchy -output outputs/soc_par_stub.vg
  

}
proc soc_upf_dc_read_rtl_leaf {} {
  set design "soc"

  set stubs_dir $::env(CONFIG_GEN)/stubs/verification/
  soc_upf_dc_read_rtl $stubs_dir $design

}

proc soc_upf_dc_read_rtl {stubs_dir design} {

  set rtl_dir $::env(CONFIG_GEN)/source/rtl/

  lappend ::search_path $stubs_dir
  lappend ::search_path $rtl_dir


  read_file -autoread -top soc -format sverilog  [pwd]/inputs
  current_design $design

}

proc soc_upf_dc_read_rtl_stub {} {
  set design "soc"
  set stubs_dir $::env(CONFIG_GEN)/stubs/verification/
  set rtl_dir $::env(CONFIG_GEN)/source/rtl/
  set soc_rtl $::env(CONFIG_GEN)/fp_unit_netlist/outputs/soc.vg
  lappend ::search_path $stubs_dir
  lappend ::search_path $rtl_dir


  read_file -autoread -top soc -format sverilog  $soc_rtl
  current_design $design

}

proc sv_dc_read_rtl_leaf {} {
  set design "soc"

  set stubs_dir $::env(CONFIG_GEN)/stubs/verification/
  sv_dc_read_rtl $stubs_dir $design

  # Write soc net connectivity report
  sv_dc_write_net_conn $::env(CONFIG_GEN)/../reports/${design}.net_leaf_connectivity.txt 0
  sv_dc_write_net_conn $::env(CONFIG_GEN)/../reports/${design}.net_leaf_hier_connectivity.txt 1
}

proc sv_dc_read_rtl {stubs_dir design} {

  # /nfs/pdx/disks/hdk.bxt-base.5/satish/gsd/soc_head/soc-ertl-gsd-a0/target/soc/aceroot/collage_work/soc/gen/
  set rtl_dir $::env(CONFIG_GEN)/source/rtl/

  lappend ::search_path $stubs_dir
  lappend ::search_path $rtl_dir

  #foreach f [glob dir ${stubs_dir}/*] {
    #echo "Reading RTL: $f"
    #read_file -format sverilog $f
  #}

  read_file -autoread -top soc -format sverilog  [pwd]/inputs
  current_design $design

  # Write verilog
  write_file -format verilog -output outputs/soc_only.vg
  write_file -format verilog -hierarchy -output outputs/soc_hierarchical.vg

  # Write DDC
  cd outputs/ddc
  write_file -hierarchy
  cd ../..

}

proc sv_dc_write_clock_nets {in_clk_rep out_fn} {
  set out_fh [open $out_fn w]
  
  array set soc_net_names {}

  sv_for_file l $in_clk_rep  {
    set l [string trim $l]
    if {$l == ""} {continue} 
    if {[regexp {^\#} $l]} { continue }
    
    sv_lassign $l par_name clk_name pin_name
    set net_name [get_object_name [get_nets -of_objects ${par_name}/${pin_name}]]
    set soc_net_names($net_name) $clk_name
  }


  foreach n [lsort [array names soc_net_names]] {
    puts $out_fh [format "%-50s %s" $n $soc_net_names($n)]
  }
    
  close $out_fh
}

#############################################
# convert DC or string to CoreTools direction
#############################################
proc _get_dc2ct_pindir {dir} {
  # DC port directions : CoreTools Direction
  # 1 - in
  # 2 - out
  # 3 - inout
  switch $dir {
    1 {
      return "in"
    }
    2 {
      return "out"
    }
    3 {
      return "inout"
    }
    "output" {
      return "out"
    }
    "input" {
      return "in"
    }
    "inout" {
      return "inout"
    }
  }

  error "Invalid direction: $dir _get_dc2ct_pindir"
}

proc sv_dc_write_net_conn {out_fn {hierarchical 0}} {
  set fh [open $out_fn w]
  puts $fh "# ---------------------------------------------------------"
  puts $fh "# Net_name driver  receivers"
  puts $fh "# ---------------------------------------------------------"
  if {$hierarchical} {
    set nets [sort_collection [get_flat_nets] name]
  } else {
    set nets [sort_collection [get_nets] name]
  }
  foreach_in_collection n $nets {
    set n_name [get_object_name $n]
    if {[regexp {^\*Logic} $n_name]} {
      #skip global nets?
      #continue
    }

    set pins  [get_pins -leaf -of_objects $n]
    set drv [get_object_name [filter_collection $pins "direction==2"]]
    if {$drv == ""} {set drv "-"}
    set rcvrs [get_object_name [filter_collection $pins "direction==1"]]
    if {$rcvrs == ""} {set rcvrs "-"}
    puts $fh [format "%-50s %-100s %s" $n_name $drv $rcvrs]
  }
  close $fh
}

proc sv_dc_write_mpins {out_fn} {
  set fh [open $out_fn w]
  puts $fh "# ---------------------------------------------------------"
  puts $fh "# Net_name : Partition : Pins"
  puts $fh "# ---------------------------------------------------------"

  foreach_in_collection n [sort_collection [get_nets] name] {
    set n_name [get_object_name $n]
    if {[regexp {^\*Logic} $n_name]} {
      #skip global nets?
      #continue
    }

    set pins  [get_pins -of_objects $n]
    if {[sizeof_collection $pins] <= 2} {
      continue
    }
    
    array unset pin_pars
    array set pin_pars {}
    foreach_in_collection p $pins {
      set p_name [get_object_name $p]
      set p_par [lindex [split $p_name "/"] 0]
      if {[info exists pin_pars($p_par)]} {
	set pin_pars($p_par) "$pin_pars($p_par) $p_name"
      } else {
	set pin_pars($p_par) "$p_name"
      }
    }

    foreach par [array names pin_pars] {
      if {[llength $pin_pars($par)] > 1} {
	puts $fh "$n_name : $par : $pin_pars($par)"
      }
    }
    
  }

  close $fh
}

#############################################################
# 
#############################################################
proc sv_blackbox_pars {} {
  set orig_d [current_design]

  set cel_refs ""
  foreach_in_collection c [get_cells  -filter {ref_name!~**logic*}] {
    lappend cel_refs [get_attribute $c ref_name]
  }

  set cel_refs [lsort -uniq $cel_refs]

  foreach d $cel_refs {
    current_design $d
    remove_cell -all
  }

  current_design $orig_d
}
