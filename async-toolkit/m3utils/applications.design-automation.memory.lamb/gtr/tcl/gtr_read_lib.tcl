
## GTR Generator Tools Release v1.0.0

### Notes on Synopsys arg handler package
# define_proc_attributes argHandler \
#    -info "Arguments processor" \
#    -define_args {
#        {-Oos "oos help"       AnOos   one_of_string {required value_help {values {a b}}}}
#        {-Int "int help"       AnInt   int     optional}
#        {-Float "float help"   AFloat  float   optional}
#        {-Bool "bool help"     ""      boolean optional}
#        {-String "string help" AString string  optional}
#        {-List "list help"     AList   list    optional}
#        {-IDup "int dup help"  AIDup   int     {optional merge_duplicates}}
#      } \
#    -define_arg_groups {
#       {exclusive {-Int -Float -Bool}}
# }

# db hash to keep lookup table definitions
if { [info exists ::__gtr_parse_lib_hash_luts ] } {
    unset ::__gtr_parse_lib_hash_luts
}
array unset ::__gtr_parse_lib_hash_luts_data

if { [info exists ::__gtr_parse_lib_hash_pluts ] } {
    unset ::__gtr_parse_lib_hash_pluts
}
array unset ::__gtr_parse_lib_hash_pluts_data

# db hash to keep header variables
if { [info exists ::__gtr_parse_lib_hash_header ] } {
    unset ::__gtr_parse_lib_hash_header
}
array unset ::__gtr_parse_lib_hash_header_data

proc gtr_parse_eat_comment { if } {
    set lines_eaten 0
    while { [gets $if line ] >= 0 } {
	incr lines_eaten
	if { [regexp "*/" $line ] } {
	    puts "INFO: gtr_parse_eat_comment, lines eaten $lines_eaten"
	    return $lines_eaten
	}
    }
    error "gtr_parse_eat_comment, encountered EOF before comment end."
}

proc gtr_parse_eat_section { if } {
    set brace_count 1
    set lines_eaten 0
    while { [gets $if line ] >= 0 } {
	incr lines_eaten
	if { [regexp "\{" $line ] } {
	    #puts "INFO: lines eaten: $lines_eaten, brace_count: $brace_count"
	    incr brace_count
	} elseif { [regexp "\}" $line ] } {
	    set brace_count [expr $brace_count - 1 ]
	} else {
	    #puts "BODY: $line"
	    ## process pair
	}
	if { $brace_count == 0 } {
	    puts "INFO: gtr_parse_eat_section, lines eaten $lines_eaten"
	    return $lines_eaten
	}
    }
    error "gtr_parse_eat_section, encountered EOF before section end."
}

proc gtr_parse_cell { if } {
    set brace_count 1
    set lines_eaten 0
    while { [gets $if line ] >= 0 } {
	incr lines_eaten
	if { [regexp "\{" $line ] } {
	    #puts "INFO: lines eaten: $lines_eaten, brace_count: $brace_count"
	    incr brace_count
	} elseif { [regexp "\}" $line ] } {
	    set brace_count [expr $brace_count - 1 ]
	} else {
	    #puts "BODY: $line"
	    ## process pair
	    while { [gets $if line ] >= 0 } {
		
	    }
	}
	if { $brace_count == 0 } {
	    puts "INFO: gtr_parse_eat_section, lines eaten $lines_eaten"
	    return $lines_eaten
	}
    }
    error "gtr_parse_eat_section, encountered EOF before section end."
}


proc gtr_read_lib { args } {
    parse_proc_arguments -args $args arg
    set proc_name [lindex [regsub "::" [info level 0]  "" ] 0 ]
    set date [date ]
    if { [info exists arg(-debug) ] } {
      echo "INFO: Setting Debug Mode"
    }
    set if [open $arg(-input_lib_file) r ]
    set of [open $arg(-output_file) w ]
    set basic_attrs [ list \
			  "date" "revision" "comment" "delay_model" "voltage_unit" "current_unit" "leakage_power_unit" \
			  "time_unit" "capacitive_load_unit" "pulling_resistance_unit" "default_threshold_voltage_group" \
			  "in_place_swap_mode" "default_max_transition" "default_cell_leakage_power" "default_leakage_power_density" \
			  "default_max_fanout" "default_fanout_load" "default_input_pin_cap" "default_inout_pin_cap" "default_output_pin_cap" "default_wire_load_area" \
			  "default_wire_load_capacitance" "default_wire_load_resistance" "slew_derate_from_library" "input_threshold_pct_rise" \
			  "input_threshold_pct_fall" "output_threshold_pct_rise" "output_threshold_pct_fall" "slew_lower_threshold_pct_rise" \
			  "slew_upper_threshold_pct_rise" "slew_upper_threshold_pct_fall" "slew_lower_threshold_pct_fall" \
			  "nom_process" "nom_temperature" "nom_voltage" "k_process_cell_leakage_power" "k_process_internal_power" \
			  "k_temp_cell_fall" "k_temp_cell_leakage_power" "k_temp_cell_rise" "k_temp_fall_propagation" \
			  "k_temp_fall_transition" "k_temp_hold_fall" "k_temp_hold_rise" "k_temp_internal_power" \
			  "k_temp_rise_propagation" "k_temp_setup_fall" "k_temp_setup_rise" "k_volt_cell_fall" \
			  "k_volt_cell_leakage_power" "k_volt_cell_rise" "k_volt_fall_propagation" "k_volt_rise_transition" \
			  "k_volt_fall_transition" "k_volt_hold_fall" "k_volt_hold_rise" "k_volt_internal_power" \
			  "k_volt_rise_propagation" "k_volt_setup_fall" "k_volt_setup_rise" "voltage_map" "library_features" \
			  "default_operating_conditions"
		     ]
    # operating_conditions, power_lut_template

    ## build a small hash table of base level attributes for quick match.
    array unset ::__gtr_read_lib_hash_base_attr
    foreach attr $basic_attrs {
	set ::__gtr_read_lib_hash_base_attr($attr) ""
    }
    while { [gets $if line ] >= 0 } {
	## implement preparser to quickly scan through the line processing before regexp
	
	# peak at the first token of the line, may need to massage "foo" ; to use lindex
	if { [string is list $line ] } {
	    set token [lindex $line 0 ]
	} elseif { [string is list [regsub ";" $line " YYY_SEMI"] ] } {
	    set token [lindex [regsub ";" $line " YYY_SEMI"] 0 ]
	} else {
	    set token ""
	}
	## need to clean token in case no space
	if { [regexp "\\(" $token ] } {
	    set token [regsub "\\(.*" $token "" ]
	}
	## first field easy tokens and continue, complex tables and cells later
	if { $token != "" && [info exists ::__gtr_read_lib_hash_base_attr($token) ] } {
	    ## need {} to protect ^$ chars but cannot ref $var inside {}
	    # first try : this hits most, then try with no colon lhs variable like capacitive_load_unit

	    set pre_exp "^\(\\s*\)$token\(\\s*\)\:\(\\s*\)"
	    if { [regexp $pre_exp $line ] } {
		set ::__gtr_read_lib_hash_base_attr($token) [regsub "\(\\s*)\;\(\\s*\)" [regsub $pre_exp $line "" ] "" ]
		continue
	    } else {
		set pre_exp "^\(\\s*\)$token\(\\s*\)"
		if { [regexp $pre_exp $line ] } {
		    set ::__gtr_read_lib_hash_base_attr($token) [regsub "\(\\s*);\(\\s*\)" [regsub $pre_exp $line "" ] "" ]
		    continue
		} else {
		    error "Unsupported, cannot remove prefix:\n-> ($token) $line"
		}
	    }
	} elseif { [regexp {^(\s*)library(\s*)\((\s*)(\S+)(\s*)\)} $line nopall nop1 nop2 nop3 libname ] } {
	    if { $libname == "" } {
		error "Unsupported, \"library( libname\" may not be split across lines.\n$line"
	    }
	    puts "Reading Library $libname"
	} elseif { [set is_lut [regexp {^(\s*)lu_table_template(\s*)\((\s*)(\S+)(\s*)\)} $line nopall nop1 nop2 nop3 tmplname ] ] ||
                   [set is_plut [regexp {^(\s*)power_lut_template(\s*)\((\s*)(\S+)(\s*)\)} $line nopall nop1 nop2 nop3 tmplname ]] } {
	    if { $tmplname == "" } {
		error "Unsupported, \"lu_table_template/power_lut_template may not be split across lines.\n$line"
	    }
	    ## read 
	    while { [gets $if line ] >= 0 && ! [regexp "\}" $line ] } {
		if { [regexp {^(\s*)variable_([0-9]+)(\s*):(\s*)(\S+)(\s*);(\s*)$} $line nop nop1 vnum nop3 nop4 vtype ] } {
		    lappend  ::__gtr_parse_lib_hash_luts $tmplname
		    if { [info exists is_lut ] && $is_lut } {
			set ::__gtr_parse_lib_hash_luts_data($tmplname,$vnum,variable) $vtype
		    } elseif { [info exists is_plut ] && $is_plut } {
			set ::__gtr_parse_lib_hash_pluts_data($tmplname,$vnum,variable) $vtype
		    } else {
			error "Unsupported, cannot decode lut."
		    }
		} elseif { [regexp {^(\s*)index_([0-9]+)(\s*)\((\s*)} $line nop nop1 inum nop3 nop4 ] } {
		    # strip header up to start of list
		    set rest [regsub {^(\s*)index_([0-9]+)(\s*)\((\s*)} $line "" ]
		    # strip trailer from end of list
		    set tmp [regsub {(s*)\)(s*);(s*)$} $rest ""]
		    # remove commas, and lindex 0 to make it a proper list?
		    set tmp2 [split [lindex [regsub {,} $tmp ""] 0 ] " " ]
		    # clean up list to remove extra null lists
		    set tmp3 [concat {*}$tmp2 ]
		    if { [info exists is_lut ] && $is_lut } {
			set ::__gtr_parse_lib_hash_luts_data($tmplname,$inum,index) $tmp3
		    } elseif { [info exists is_plut ] && $is_plut } {

			set ::__gtr_parse_lib_hash_pluts_data($tmplname,$inum,index) $tmp3
		    } else {
			error "Unsupported, cannot decode plut."
		    }

		}
	    }
	    if { [info exists is_lut ] && $is_lut } {
		puts "Reading Library Template $tmplname"
		lappend ::__gtr_parse_lib_hash_luts $tmplname
	    } elseif { [info exists is_plut ] && $is_plut } {
		puts "Reading Power Template $tmplname"
		lappend ::__gtr_parse_lib_hash_pluts $tmplname
	    } else {
		error "Unsupported, cannot decode plut."
	    }
	} elseif { [regexp {^(\s*)type(\s*)\((\s*)(\S+)(\s*)\)(\s*)\{} $line nopall nop1 nop2 nop3 cellname ] } {
	    if { $cellname == "" } {
		error "Unsupported, \"library( libname\" may not be split across lines.\n$line"
	    }
	    puts "Reading Type $cellname"
	    gtr_parse_eat_section $if
	} elseif { [regexp {^(\s*)cell(\s*)\((\s*)(\S+)(\s*)\)(\s*)\{} $line nopall nop1 nop2 nop3 cellname ] } {
	    if { $cellname == "" } {
		error "Unsupported, \"library( libname\" may not be split across lines.\n$line"
	    }
	    puts "Reading Cell $cellname"
	    gtr_parse_eat_section $if
	} elseif { [regexp "/\\*" $line ] } {
	    if { [regexp "\\*/" $line ] } {
		continue
	    }
	    gtr_parse_eat_comment $if
	} else {
	    if { $line != "" && ! [regexp {^(\s*)\}(\s*)$} $line ] } {
		puts "WARNING: Unsupported Token: $token Line: $line"
	    }
	}
    }
    close $if
    close $of
}

define_proc_attributes gtr_read_lib \
    -info "Parse .lib file to capture tables" \
    -define_args {
      {-input_lib_file "Input ASCII lib file" "<input_lib_file>" string required}
      {-output_file "Output TCL to create DB" "<output_file>" string required}
      {-debug "Report additional logging for debug purposes" "" boolean optional}
}


proc gtr_print_lib { args } {
    parse_proc_arguments -args $args arg
    set proc_name [lindex [regsub "::" [info level 0]  "" ] 0 ]
    set date [date ]
    if { [info exists arg(-debug) ] } {
      echo "INFO: Setting Debug Mode"
    }
    foreach tmplname $::__gtr_parse_lib_hash_luts {
	puts "lu_table_template ($tmplname) {"
	# two pass for now, variables first, sort to ensure correct indexing
	foreach tmpl_num_type [lsort [array names ::__gtr_parse_lib_hash_luts_data ] ] {
	    set sp_tmpl_num_type [split $tmpl_num_type "," ]
	    set tmpl [lindex $sp_tmpl_num_type 0 ]
	    set num [lindex $sp_tmpl_num_type 1 ]
	    set type [lindex $sp_tmpl_num_type 2 0 ]
	    if { "$tmpl" == "$tmplname" && $type == "variable" } {
		puts "    variable_$num : $::__gtr_parse_lib_hash_luts_data($tmpl_num_type) ;"
	    }
	}
	# index data next
	foreach tmpl_num_type [lsort [array names ::__gtr_parse_lib_hash_luts_data ] ] {
	    set sp_tmpl_num_type [split $tmpl_num_type "," ]
	    set tmpl [lindex $sp_tmpl_num_type 0 ]
	    set num [lindex $sp_tmpl_num_type 1 ]
	    set type [lindex $sp_tmpl_num_type 2 ]
	    if { $tmpl == $tmplname && $type == "index" } {
		set fdata [join $::__gtr_parse_lib_hash_luts_data($tmpl_num_type) ", " ]
		puts "    index_${num}(\"$fdata\");"
	    }
	}
	puts "}\n"
    }
    foreach tmplname $::__gtr_parse_lib_hash_pluts {
	puts "power_lut_template ($tmplname) {"
	# two pass for now, variables first, sort to ensure correct indexing
	foreach tmpl_num_type [lsort [array names ::__gtr_parse_lib_hash_pluts_data ] ] {
	    set sp_tmpl_num_type [split $tmpl_num_type "," ]
	    set tmpl [lindex $sp_tmpl_num_type 0 ]
	    set num [lindex $sp_tmpl_num_type 1 ]
	    set type [lindex $sp_tmpl_num_type 2 0 ]
	    if { "$tmpl" == "$tmplname" && $type == "variable" } {
		puts "    variable_$num : $::__gtr_parse_lib_hash_pluts_data($tmpl_num_type) ;"
	    }
	}
	# index data next
	foreach tmpl_num_type [lsort [array names ::__gtr_parse_lib_hash_pluts_data ] ] {
	    set sp_tmpl_num_type [split $tmpl_num_type "," ]
	    set tmpl [lindex $sp_tmpl_num_type 0 ]
	    set num [lindex $sp_tmpl_num_type 1 ]
	    set type [lindex $sp_tmpl_num_type 2 ]
	    if { $tmpl == $tmplname && $type == "index" } {
		set fdata [join $::__gtr_parse_lib_hash_pluts_data($tmpl_num_type) ", " ]
		puts "    index_${num}(\"$fdata\");"
	    }
	}
	puts "}\n"
    } 
}


define_proc_attributes gtr_print_lib \
    -info "Print parsed info from .lib file" \
    -define_args {
      {-debug "Report additional logging for debug purposes" "" boolean optional}
}

#	if { [regexp {^(\s*)date(\s*):} $line ] } {
#	    set ::__gtr_parse_lib_hash_header_data(date) [regsub "(s*);(s*)" [regsub {^(\s*)date(\s*):} $line "" ] "" ]
