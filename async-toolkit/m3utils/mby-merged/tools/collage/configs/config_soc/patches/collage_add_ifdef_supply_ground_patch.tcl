#########################################################################
# proc used to insert `ifdef NO_PWR_PINS around power/ground pins #
#########################################################################
proc collage_add_ifdef_supply_ground { args } {
  parse_proc_arguments -args $args opts

  set folder_path ""; set stage ""; set supply_pin_exception [list]
  set debug_mode 0

    set folder_path $opts(-folder_path)
    
    if {[info exists opts(-stage)]} {
	set stage $opts(-stage)
    }

    set supply_pin_include [list]
    if {[info exists opts(-supply_pin_include)]} {
	set supply_pin_include $opts(-supply_pin_include)
    }
    if {[info exists opts(-supply_pin_exception)]} {
	set supply_pin_exception $opts(-supply_pin_exception)
    }
    set supply_pin_include_merge ""
    foreach pin_inc $supply_pin_include {
      if {$supply_pin_include_merge == ""} {
        set supply_pin_include_merge "\\m${pin_inc}\\M"
      }         else {
        set supply_pin_include_merge "${supply_pin_include_merge}|\\m${pin_inc}\\M"
      }
    }
    set supply_pin_exception_merge ""
    foreach pin_exc $supply_pin_exception {
      if {$supply_pin_exception_merge == ""} {
        set supply_pin_exception_merge "\\m${pin_exc}\\M"
      } 	else {
        set supply_pin_exception_merge "${supply_pin_exception_merge}|\\m${pin_exc}\\M"
      }
    } 
  
  if {[file exists "${folder_path}/orig"]} {
    puts "-E-: \"${folder_path}/orig\" exists. Using this as the source. Please remove this dir if you want to use \"${folder_path}\" to be used as source"
  } else {
    file mkdir "${folder_path}/orig"
    foreach f [glob -nocomplain ${folder_path}/*.v ${folder_path}/*.sv] {
      file copy -force $f "${folder_path}/orig/."
    }
  }

  foreach f [glob -nocomplain ${folder_path}/orig/*.v ${folder_path}/orig/*.sv]  {
    
    set fields_array [split $f "\/"]
    set length_array [llength $fields_array]
    set index [expr $length_array - 1]
    
    set filename_pure [lindex $fields_array $index]
    set process_file_enable 1
    
    if {$stage == "soc_tb"} {
      if {$filename_pure == "soc_tb.sv"} {
      } else {
	set process_file_enable 0
      }
    }
    
    if {$process_file_enable} {
      
      puts "Processing $f ..."
      
      regsub -all "orig/$filename_pure" $f "$filename_pure" file_name_out

      ::col::ifdef_supply_ground_single_file $f $file_name_out $filename_pure $supply_pin_exception_merge $supply_pin_include_merge
    }
  }

  if {$debug_mode == 0} {
    puts "Deleting folder ${folder_path}/orig ..."
    file delete -force ${folder_path}/orig
  }
  
  return;
}

define_proc_attributes collage_add_ifdef_supply_ground \
    -info "Collage Insert ifdef INTEL_NO_PWR_PINS around power/ground pins" \
    -define_args {
      {"-folder_path"         "RTL dir path"   "" string required}
      {"-stage" "stage"   "" string optional}
      {"-supply_pin_exception" "List of pins to exclude in ifdef"   "" string optional}
      {"-supply_pin_include" "List of pins to include in ifdef"   "" string optional}
    }

########################################################################################
# proc used to insert `ifdef NO_PWR_PINS around power/ground pins in single file #
########################################################################################
proc ::col::ifdef_supply_ground_single_file { f file_name_out filename_pure supply_pin_exception supply_pin_include} {
  ######################################################################
  # filename_pure --> name file output (no hierarchical path included) #
  ######################################################################
  
  set fp [open $f]; set o_fp [open $file_name_out w+]

  set prev_lines ""; set if_def_lines ""
  while {-1 != [gets $fp line]} {
    set if_def 0; set open_needed 0; set close_needed 0; set close_needed_special_v0 0
    set sline [string trim $line]
    if {[regexp -nocase {^(vcc|vss|vref_).*,$} $sline] && ($supply_pin_exception == "" || [regexp $supply_pin_exception $sline] == 0)} { set if_def 1
      if {(![regexp -nocase {\,$} [lindex $sline end]])} {
	set close_needed_special_v0 1 
      }
    }
    if {[regexp -nocase {^(vcc|vss|vref_).*$} $sline] && ($supply_pin_exception == "" || [regexp $supply_pin_exception $sline] == 0)} { set if_def 1
      if {(![regexp -nocase {\,$} [lindex $sline end]])} {
	set close_needed_special_v0 1
      }
    }

    # vnn --> fuse
    if {[regexp {^(vnn).*,$} $sline] && ($supply_pin_exception == "" || [regexp $supply_pin_exception $sline] == 0)} { set if_def 1
      if {(![regexp -nocase {\,$} [lindex $sline end]])} {
	set close_needed_special_v0 1
      }
    }
    if {[regexp {^(vnn).*$} $sline] && ($supply_pin_exception == "" || [regexp $supply_pin_exception $sline] == 0)} { set if_def 1
      if {(![regexp -nocase {\,$} [lindex $sline end]])} {
	set close_needed_special_v0 1
      }
    }

    if {[regexp -nocase "^(${supply_pin_include}).*,$" $sline] && ${supply_pin_include} ne "" && ($supply_pin_exception == "" || [regexp $supply_pin_exception $sline] == 0)} { set if_def 1
      if {(![regexp -nocase {\,$} [lindex $sline end]])} {
        set close_needed_special_v0 1
      }
    }
    if {[regexp -nocase "^(${supply_pin_include}).*$" $sline] && ${supply_pin_include} ne "" && ($supply_pin_exception == "" || [regexp $supply_pin_exception $sline] == 0)} { set if_def 1
      if {(![regexp -nocase {\,$} [lindex $sline end]])} {
        set close_needed_special_v0 1
      }
    }

    set sline [split [regsub -all {\s+} $sline { }] { }]

    if {[regexp {^\/\/} $sline]} {
      if {[string length $if_def_lines]} {
        set if_def 1
      } else {
        set if_def 0
      }
    }
    
    # puts "if_def is : $if_def"

    if {[lsearch -exact {input output inout wire assign} [lindex "$sline" 0]] > -1} {
      if {[regexp -nocase {^(vcc|vss|vref_).*;$} [lindex "$sline" end]] && ($supply_pin_exception == "" || [regexp $supply_pin_exception $sline] == 0)}  { set if_def 1 } else {
      if {[regexp -nocase {^(vcc|vss|vref_).*,$} [lindex "$sline" end]] && ($supply_pin_exception == "" || [regexp $supply_pin_exception $sline] == 0)}  { set if_def 1 } else {
      if {[regexp -nocase {^(vcc|vss|vref_).*$}  [lindex "$sline" end]] && ($supply_pin_exception == "" || [regexp $supply_pin_exception $sline] == 0)}  { set if_def 1; set close_needed_special_v0 1 }}}
      # vnn --> fuse
      if {[regexp         {^(vnn).*;$} [lindex "$sline" end]] && ($supply_pin_exception == "" || [regexp $supply_pin_exception $sline] == 0)} { set if_def 1; } else {
      if {[regexp         {^(vnn).*,$} [lindex "$sline" end]] && ($supply_pin_exception == "" || [regexp $supply_pin_exception $sline] == 0)} { set if_def 1; } else {
      if {[regexp         {^(vnn).*$}  [lindex "$sline" end]] && ($supply_pin_exception == "" || [regexp $supply_pin_exception $sline] == 0)} { set if_def 1;  set close_needed_special_v0 1 }}}

    
      ########################################################################
      # Exceptions: This is bad ... Needed because Collage change wire names #
      #             and we cannot rely on ^vcc or ^vss.                      #
      ########################################################################
      if {[regexp         {^(chv73c6srampg_cpu0_vout).*;$} [lindex "$sline" end]]} { set if_def 1 }
      if {[regexp         {^(chv73c6srampg_cpu1_vout).*;$} [lindex "$sline" end]]} { set if_def 1 }
      if {[regexp         {^(ldo_cpll0_vccreg_1p0).*;$} [lindex "$sline" end]]} { set if_def 1 }
      if {[regexp         {^(ldo_cpll1_vccreg_1p0).*;$} [lindex "$sline" end]]} { set if_def 1 }
      if {[regexp         {^(ldo_dts_cpu0_vccreg_1p0).*;$} [lindex "$sline" end]]} { set if_def 1 }
      if {[regexp         {^(ldo_dts_cpu1_vccreg_1p0).*;$} [lindex "$sline" end]]} { set if_def 1 }
      if {[regexp         {^(ldo_dts_gen_vccreg_1p0).*;$} [lindex "$sline" end]]} { set if_def 1 }
      if {[regexp         {^(ldo_dts_soc_vccreg_1p0).*;$} [lindex "$sline" end]]} { set if_def 1 }
      if {[regexp         {^(ldo_gpll_vccreg_1p0).*;$} [lindex "$sline" end]]} { set if_def 1 }
      if {[regexp         {^(ldo_hpll_vccreg_1p0).*;$} [lindex "$sline" end]]} { set if_def 1 }
      if {[regexp         {^(ccu_vccssreg_1p0).*;$} [lindex "$sline" end]]} { set if_def 1 }
      if {[regexp         {^(ccu_vccxtalreg_1p0).*;$} [lindex "$sline" end]]} { set if_def 1 }
      if {[regexp         {^(ldo_dsidpll_vccreg_1p0).*;$} [lindex "$sline" end]]} { set if_def 1 }
      if {[regexp         {^(c73p4rtctop_vccrtc_uhv).*,$} [lindex "$sline" end]]} { set if_def 1 }
    }

    if {[lsearch -exact {input output inout wire assign} [lindex "$sline" 0]] > -1} {
      if {[regexp -nocase "^(${supply_pin_include}).*;$" [lindex "$sline" end]] && ${supply_pin_include} ne "" && ($supply_pin_exception == "" || [regexp $supply_pin_exception $sline] == 0)}  { set if_def 1 } else {
      if {[regexp -nocase "^(${supply_pin_include}).*,$" [lindex "$sline" end]] && ${supply_pin_include} ne "" && ($supply_pin_exception == "" || [regexp $supply_pin_exception $sline] == 0)}  { set if_def 1 } else {
      if {[regexp -nocase "^(${supply_pin_include}).*$"  [lindex "$sline" end]] && ${supply_pin_include} ne "" && ($supply_pin_exception == "" || [regexp $supply_pin_exception $sline] == 0)}  { set if_def 1; set close_needed_special_v0 1 }}}
    }

    # vnn --> fuse
    if {[regexp -nocase {\.(vcc|vss|vref_).*} [lindex "$sline" 0]] || [regexp {\.(vnn).*} [lindex "$sline" 0]] || [regexp {\.(vout).*} [lindex "$sline" 0]] || [regexp {\.(vin_1p15).*} [lindex "$sline" 0]] || [regexp {\.(c73p4rtctop_vccrtc_uhv).*} [lindex "$sline" 0]] || ([regexp $supply_pin_include  [lindex "$sline" 0]] && ${supply_pin_include} ne "")} {
       if {($supply_pin_exception == "" || [regexp $supply_pin_exception $sline] == 0)} {
          set if_def 1
       }

      if {[regexp -nocase {^\(} [lindex $sline 0]]} {
        set open_needed 1
      }

      if {[regexp -nocase {\);$} [lindex $sline end]]} {
        set close_needed 1
      }
    }
    
    if {$if_def} {
      if {$open_needed} {
        append prev_lines "\n[::col::get_leading_space $line]\("
        set s [string trimleft $line]
        set line "[::col::get_leading_space $line][string range $s 1 end]"
      }
      if {$close_needed} {
        set s [string trimright $line]
        set line [string range $s 0 end-2]
      }
      if {$close_needed_special_v0} {
        # Nothing to do ...
        # set s [string trimright $line]
        # set line [string range $s 0 end-2]
      }      
      if {[string length $if_def_lines]} {
        append if_def_lines "\n" 
      }
      append if_def_lines $line

      if {$close_needed || $close_needed_special_v0} {
        if {[string length $prev_lines]} {
          # --- Eat the "," from previous line and added to the first $if_def_lines
          set plines [split [string trimright $prev_lines] \n]
          set n_plines [list]
          set comma_stripped 0
          for {set ii [expr {[llength $plines]-1}]} {$ii > -1} {incr ii -1} {
            set tr_line [lindex $plines $ii]
            if {! $comma_stripped} {
              set tr_line [string trimright $tr_line]
              if {[string index $tr_line end] == ","} {
                set tr_line [string range $tr_line 0 end-1]
                set comma_stripped 1 ;  # Done with removing the comma
              }
            }
            lappend n_plines $tr_line
          }

          set prev_lines ""
          for {set ii [expr {[llength $n_plines]-1}]} {$ii > -1} {incr ii -1} {
            lappend prev_lines [lindex $n_plines $ii]
          }
          set prev_lines [join $prev_lines "\n"]

          if {$comma_stripped} {
            set if_def_lines "[::col::get_leading_space $if_def_lines], [string trimleft $if_def_lines]"
          }
        }

        puts $o_fp $prev_lines

        if {[string length $if_def_lines]} {
          puts $o_fp "`ifndef INTEL_NO_PWR_PINS"
          puts $o_fp $if_def_lines
          puts $o_fp "`endif"

          set if_def_lines ""
        }
        
	if {$close_needed} {
	  puts $o_fp "[::col::get_leading_space $line]\)\;"
        }
        
	set prev_lines ""; set if_def_lines ""
      }
    } else {
      # if {[string length $prev_lines]} {
      #  append prev_lines "\n"
      # } 
      # append prev_lines $line

      if {[string length $if_def_lines]} {
        puts $o_fp $prev_lines
        set prev_lines ""
        puts $o_fp "`ifndef INTEL_NO_PWR_PINS"
        puts $o_fp $if_def_lines
        puts $o_fp "`endif"

        set if_def_lines ""
	set prev_lines $line
      } else {
        if {[string length $prev_lines]} {
	  append prev_lines "\n"
        } 
        append prev_lines $line
      }
    }
  }

  if {[string length $prev_lines]} {
    puts $o_fp $prev_lines
  }

  if {[string length $if_def_lines]} {
    puts $o_fp "`ifndef INTEL_NO_PWR_PINS"
    puts $o_fp $if_def_lines
    puts $o_fp "`endif"
  }

  close $fp; close $o_fp

  return;
}

