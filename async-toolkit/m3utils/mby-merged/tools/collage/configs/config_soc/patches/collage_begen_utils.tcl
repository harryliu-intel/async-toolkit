# ------------------------------------------------------------------------
# Configuration file and its manipulation

namespace eval ::be_config {
  variable G_VARS 
  variable G_ICC_VARS 
  variable G_ICC2_VARS 
  variable PARTITION_BBOX
  variable PARTITION_POLY
  variable PARTITION_ORIGIN
  variable UNIT_POLY
  variable UNIT_ORIGIN
  variable PARTITION_CC
}

proc ::be_config::init { } {
  set ::be_config::G_VARS [list]
  set ::be_config::G_ICC_VARS [list]
  set ::be_config::G_ICC2_VARS [list]
  set ::be_config::PARTITION_BBOX [list]
  set ::be_config::PARTITION_POLY [list]
  set ::be_config::PARTITION_ORIGIN [list]
  set ::be_config::UNIT_POLY [list]
  set ::be_config::UNIT_ORIGIN [list]
  array unset ::be_config::PARTITION_CC
}
proc coll_be_config_gen_collateral {{macro_placement_file ""} {blackbox_file ""} {blocksetup_file ""} {is_unit 0} {for_icc2 0}} {
  if {$is_unit == 1} {
    set origin_list [join $::be_config::UNIT_ORIGIN]
    set poly_list [join $::be_config::UNIT_POLY]
    if {$for_icc2} {
      set origin_list "$origin_list [join $::be_config::PARTITION_ORIGIN]"
      set poly_list "$poly_list [join $::be_config::PARTITION_POLY]"
    }
  } else {
    set origin_list [join $::be_config::PARTITION_ORIGIN]
    set poly_list [join $::be_config::PARTITION_POLY]
  }
  if {[file exists $macro_placement_file]} {
    file delete -force $macro_placement_file
  }
  set fp_wr [open $macro_placement_file w]
  
  if {$for_icc2} {
    set design_orientation RO
    set design_option ""
    puts $fp_wr {set_app_options -list {shell.undo.enabled false}} 
    puts $fp_wr {set old_state [set_snap_setting -enabled false]} 
  } else {
    set design_orientation N 
    set design_option "-all"
    puts $fp_wr "undo_config -disable"
    puts $fp_wr "set oldSnapState \[set_object_snap_type -enabled false\]"
  }
  set all_cells [list]
  foreach {par_name or_x or_y} $origin_list {
    lappend all_cells ${par_name}     
    puts $fp_wr ""
    puts $fp_wr "#**************"
    puts $fp_wr "# Soft Macro "
    puts $fp_wr "# obj#: 26 "
    puts $fp_wr "# objects are in alphabetical ordering "
    puts $fp_wr "#**************"
    puts $fp_wr ""
    puts $fp_wr ""
    puts $fp_wr "set obj \[get_cells \{\"${par_name}\"\} $design_option\]"
    puts $fp_wr "set_attribute -quiet \$obj orientation $design_orientation"
    puts $fp_wr "set_attribute -quiet \$obj origin \{${or_x} ${or_y}\}"
    puts $fp_wr "set_attribute -quiet \$obj is_placed true"
    puts $fp_wr "set_attribute -quiet \$obj is_fixed true"
    if {!$for_icc2} {
      puts $fp_wr "set_attribute -quiet \$obj is_soft_fixed false"
      puts $fp_wr "set_attribute -quiet \$obj eco_status eco_reset"
    }
  } 
  if {$for_icc2} {
    set design_orientation RO
    puts $fp_wr {set_snap_setting -enabled true}
    puts $fp_wr {set_app_options -list {shell.undo.enabled true}}
  } else {
    puts $fp_wr "  set_object_snap_type -enabled \$oldSnapState"
    puts $fp_wr "  undo_config -enable"
  }
  close $fp_wr
  if {[file exists ${blackbox_file}]} {
    file delete -force ${blackbox_file}
  }
  set fp_wr [open $blackbox_file w]
  foreach {par_name poly} $poly_list {
    if {$for_icc2} {
      puts $fp_wr "  set_boundary -boundary \[list $poly\] \[get_cells ${par_name}\]"
    } else {
      puts $fp_wr "  estimate_fp_black_boxes -fixed_shape -polygon \[list $poly\] \[get_flat_cell ${par_name}\]"
    }
  }
  close $fp_wr
  
  if {[file exists ${blocksetup_file}]} {
    file delete -force ${blocksetup_file}
  }
  set fp_wr [open $blocksetup_file w]
  foreach {g_var g_var_val var_type} [join $::be_config::G_VARS] {
    if {[string equal $var_type "unit"] && ($is_unit == 0)} {
      continue
    }
    if {[string equal $var_type "par"] && ($is_unit == 1)} {
      continue
    }
    if {(!$for_icc2) && ([lsearch -exact $::be_config::G_ICC2_VARS $g_var] >= 0)} {
      continue
    }
    if {($for_icc2) && ([lsearch -exact $::be_config::G_ICC_VARS $g_var] >= 0)} {
      continue
    }
    if {[string equal $g_var_val auto_derive_cell_list]} {
      puts $fp_wr "set ${g_var} \"${all_cells}\""
      continue
    }
    puts $fp_wr "set ${g_var} \"${g_var_val}\""
  }
  close $fp_wr

}

proc coll_be_par_bbox {par_name poly} {
  set poly [join $poly]
  if {[llength $poly] < 4} {
    print_error "BBOX doesn't have 4 elements"
    return
  }
  set x 99999999999; set y 99999999999;
  foreach {poly_x poly_y} $poly {
    if {$poly_x < $x} {
      set x $poly_x
      set y $poly_y
    } elseif {$poly_x == $x} {
      if {$poly_y < $y} {
        set y $poly_y
      }
    }
  }
  foreach {llx lly urx ury} $poly {}
  set ::be_config::PARTITION_CC($par_name,llx) $llx
  set ::be_config::PARTITION_CC($par_name,lly) $lly
  set ::be_config::PARTITION_CC($par_name,L) [expr $urx - $llx]
  set ::be_config::PARTITION_CC($par_name,H) [expr $ury - $lly]
  set new_poly  "$llx $lly $urx $lly $urx $ury $llx $ury $llx $lly"
  set updated_poly [list]
  foreach {poly_x poly_y} $new_poly {
    lappend updated_poly [list  ${poly_x} ${poly_y}]
  }
  lappend ::be_config::PARTITION_ORIGIN [list ${par_name} ${x} ${y}]
  lappend ::be_config::PARTITION_POLY [list ${par_name} ${updated_poly}]
}

proc coll_be_unit_bbox {unit_hier_name poly} {
  set poly [join $poly]
  if {[llength $poly] < 4} {
    print_error "BBOX doesn't have 4 elements"
    return
  }
  set split_unit_name [file split $unit_hier_name] 
  if {[llength $split_unit_name] != 2} {
    print_error "Unit hierarchical name should be 2 elements $unit_hier_name"
    return
  }
  foreach {par_name unit_name} $split_unit_name {}

  set par_llx [set ::be_config::PARTITION_CC($par_name,llx)]
  set par_lly [set ::be_config::PARTITION_CC($par_name,lly)]
  set par_length [set ::be_config::PARTITION_CC($par_name,L)]
  set par_height [set ::be_config::PARTITION_CC($par_name,H)]
  
  foreach {llx lly urx ury} $poly {}
  foreach var "llx lly urx ury" {
    set value [set $var]
    set split_${var} [file split [set $var]]
    if {[llength [set split_${var}]] == 1} {
      if {[regexp "L"  $value] || [string equal $var "llx"]} {
	set orig  $par_llx
	set delta $par_length  
      }
      if {[regexp "H"  $value] || [string equal $var "lly"]} {
	set orig  $par_lly
	set delta $par_height  
      }
      set result [expr $orig + $delta]
      set idx_0 [lindex [set split_${var}] 0]
      if {[string equal $idx_0 0]} {
        set updated_$var $orig
      } elseif {[regexp "L"  $value] || [regexp "H"  $value]} {
        set updated_$var $result 
      }
    } elseif {[llength [set split_${var}]] == 2} {
      set idx_0 [lindex [set split_${var}] 0]
      set idx_1 [lindex [set split_${var}] 1]
      if {[regexp "L"  $value]} {
	set orig  $par_llx
	if {[string length $idx_0] == 2} {
          set delta [expr [regsub {L} $value \*\$par_length]]  
        } else {
          set delta [expr [regsub {L} $value \$par_length]]  
        }
      }
      if {[regexp "H"  $value]} {
	set orig  $par_lly
	if {[string length $idx_0] == 2} {
	  set delta [expr [regsub {H} $value \*\$par_height]]  
        } else {
	  set delta [expr [regsub {H} $value \$par_height]]  
        }
      }
      set result [expr $orig + $delta]
      if {[regexp "L"  $value] || [regexp "H"  $value]} {
        set updated_$var $result 
      }
    } elseif {[llength [set split_${var}]] > 2} {
    }
  }
  
  set new_poly  "$updated_llx $updated_lly $updated_urx $updated_lly $updated_urx $updated_ury $updated_llx $updated_ury $updated_llx $updated_lly"
  set updated_poly [list]
  foreach {poly_x poly_y} $new_poly {
    lappend updated_poly [list  ${poly_x} ${poly_y}]
  }
  lappend ::be_config::UNIT_ORIGIN [list ${unit_name} ${updated_llx} ${updated_lly}]
  lappend ::be_config::UNIT_POLY [list ${unit_name} ${updated_poly}]
}


proc coll_be_par_poly {par_name poly {is_unit 0}} {
  set poly [join $poly]
  if {[llength $poly] < 10} {
    print_error "Polygon doesn't have 4 elements"
    return
  }
  set x 99999999999; set y 99999999999;
  foreach {poly_x poly_y} $poly {
    if {$poly_y <= $y} {
      set y $poly_y
    } 
    if {$poly_x <= $x} {
      set x $poly_x
    }
  }
  set updated_poly [list]
  foreach {poly_x poly_y} $poly {
    lappend updated_poly [list ${poly_x} ${poly_y}]
    #lappend updated_poly [list [expr ${poly_x} - ${x}] [expr ${poly_y} - ${y}]]
  }
  if {$is_unit} {
    lappend ::be_config::UNIT_ORIGIN [list ${par_name} ${x} ${y}]
    lappend ::be_config::UNIT_POLY [list ${par_name} ${updated_poly}]
    
  } else {
    lappend ::be_config::PARTITION_ORIGIN [list ${par_name} ${x} ${y}]
    lappend ::be_config::PARTITION_POLY [list ${par_name} ${updated_poly}]
  }
}    

proc coll_be_gvars {var var_value {type both}} {
  lappend ::be_config::G_VARS [list ${var} ${var_value} $type]
}
proc coll_be_icc2_gvars {var var_value {type both}} {
  lappend ::be_config::G_VARS [list ${var} ${var_value} $type]
  lappend ::be_config::G_ICC2_VARS ${var}
}
proc coll_be_icc_gvars {var var_value {type both}} {
  lappend ::be_config::G_VARS [list ${var} ${var_value} $type]
  lappend ::be_config::G_ICC_VARS ${var}
}

proc collage_beconfig_var_info { args } {
  parse_proc_arguments -args $args options
  if {[info exists options(-vars)]} {
    set all_vars $options(-vars)
  }
  if {[info exists options(-info)]} {
    set info 1
  } else {
    set info 0
  }
  if {[info exists options(-usage)]} {
    set usage 1
  } else {
    set usage 0
  }
  if {[info exists options(-all_vars)]} {
    set all_vars "G_VARS PARTITION_BBOX PARTITION_POLY" 
  }
  puts "ALL of the following variables should be set in the config settings file"
  foreach var $all_vars {
    switch -regexp -- $var {
      {.*TRACK_PLAN_PRE_FILE} {
        puts "Var ::lattice::config::TRACK_PLAN_PRE_FILE"
        if {$info} {
          puts " INFO: This variable is used to provide a PRE file for the track planning stage in Lattice Route"
        }
        if {$usage} {
          puts "  USAGE: set ::lattice::config::TRACK_PLAN_PRE_FILE <complete path to the file>"
        }
      }      
    }
  }
}

define_proc_attributes "collage_beconfig_var_info" -info "Collage configuration options and variables reported to the screen." \
    -hide_body \
    -define_args {
      {"-all_vars"  "List all variables" "" "boolean" "optional" }
      {"-vars"  "List of variables names" "" "string" "optional"}
      {"-info"  "Flag to report information of the variable." "" "boolean" "optional"}
      {"-usage"  "Flag to report usage information of the variable" "" "boolean" "optional"}
    }

proc collage_clean_be_config {args } {
  parse_proc_arguments -args $args options
  namespace delete ::be_config 
}

define_proc_attributes "collage_clean_be_config" -info "Collage command to clean be_config" \
    -hide_body \
    -define_args {
      {"-verbose" "Verbose mode of operation" "" "boolean" "optional"}
    }

# write fp config lines for partitions (this procedure to be run in IC Compiler)
proc _coll_be_write_fp_config {fn cell_regexp} {
  
  set fh [open $fn w]

  foreach_in_collection par [get_cells -regexp $cell_regexp] {
    set bound [get_attribute $par boundary]
    if {[llength $bound] > 5} {
      # rectilinear
      puts $fh "coll_be_par_poly [get_object_name $par] \[ list [join $bound] \]"
    } else {
      # rectangle
      set bbox [get_attribute $par bbox]
      puts $fh "coll_be_par_bbox [get_object_name $par] \[ list [join $bbox] \]"
    }
  }

  close $fh
}

# --- Initialize the config 
::be_config::init
# --- END
