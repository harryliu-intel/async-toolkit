#
# Based on utility from Mika
# Author : Mika Nystrom <mika.nystroem@intel.com>
# Author : Paul Donehue <paul.donehue@intel.com>
#

# BASIC GEOMETRIC DEFS
# N.B. depth of memory runs in X, width in Y
#
# SO to avoid confusion.  This file consistently uses the following
# nomenclature:
#
# depth             - # of entries in a RAM array
# width             - # of bits in each entry of a RAM array
# X [NOT "width"]   - a physical dimension in the horizontal direction when
#                     viewing a physical design representation on
#                     a graphics display
# Y [NOT "height" ] - a physical dimension in the vertical direction when
#                     viewing a physical design representation on
#                     a graphics display
#
# units are appended as suffixes
# if no units are mentioned for physical quantities, they are in SI base units
#

proc gtr_lamb_area { args } {
    set proc_name [lindex [regsub "::" [info level 0]  "" ] 0 ]
    set date [date ]
    parse_proc_arguments -args $args arg
    if { [info exists arg(-v) ] } {
        set verbose 1
    } else {
        set verbose 0
    }
    if { [info exists arg(-debug) ] } {
      echo "INFO: $proc_name, Starting"
    }
    set depth $arg(-data_depth)
    set width $arg(-data_width)
    set tech_node $arg(-tech_node)
    if { $tech_node == "n5" } {
	set pg_nm  51 ;# value of pg in nanometers
	set rw_nm 210 ;# value of row height in nanometers
    } elseif { $tech_node == "n3b" } {
	set pg_nm  45 ;# value of pg in nanometers
	set rw_nm 162 ;# value of row height in nanometers TBD fix this value.
    } elseif { $tech_node == "n3e" } {
	set pg_nm  48 ;# value of pg in nanometers
	set rw_nm 169 ;# value of row height in nanometers
    } else {
	error "Unsupported tech_node: $tech_node"
    }
    set bitx_pg 5 ;# width of a bit in PG
    set bity_rw 1 ;# height of a bit in rows
    
    set chunkdepth 16 ;# max chunk depth in words
    set maxchunks   8 ;# max chunks in a macro
    set maxwidth  144 ;# max width of a Lamb in bits
    set maxdepth  128 ;# max depth of a Lamb in words, n/c DFT words

    set dftextra_words 1 ;# how many extra words for DFT in a macro (gets snuck in)

    ##################################
    #
    # overheads in X direction
    #
    set rw_width_pg    50 ;# width of env region in / [PG]
    set snout_pg       20 ;# width of Lamb "snout" (central control) region / [PG]
    set extra_width_pg 10 ;# extra overhead /[PG]
    set perchunk_pg    20 ;# extra overhead per chunk /[PG]
    set dft_ox_pg [expr $dftextra_words * $bitx_pg ]  ;# overhead from DFT word(s)
    set fixed_ox_pg [expr $rw_width_pg + $snout_pg + $extra_width_pg + $dft_ox_pg ]


    ###################################
    #
    # overheads in Y direction
    #
    set decoder_h_rw 18    ;# height of decoder in rows
    set halo_h_rw    2    ;# halo at top and bottom of macro
    set fixed_oy_rw [expr $decoder_h_rw + $halo_h_rw ]
    set bit_area_nm2 [expr $pg_nm * $bitx_pg * $bity_rw * $rw_nm ]
    if { [info exists arg(-dual_clock) ] } {
        set mem_name "1w1r1c"
    } else {
        set mem_name "1w1r2c"
    }
    if { [info exists arg(-debug) ] } {
      echo "INFO: Processing Memory $mem_name"
    }
    # do the work for a full array (Hartvig!)
    set total_bits [expr  $depth * $width ]

    puts "OVERALL DESIGN PARAMETERS"
    puts "========================="
    puts [format "requested size %dD x %dW" $depth $width ]
           
    puts [format "raw bit cell area      %8d nm^2        %15le m^2" $bit_area_nm2 [expr $bit_area_nm2 / 1e18 ] ]

    set bits_area  [expr $total_bits * $bit_area_nm2 / 1e18 ]

    puts [format "total bits needed %5d x %5d = %d ; area %le m^2" $depth $width $total_bits $bits_area ]

    set ndeep [expr ($depth - 1) / $maxdepth + 1 ]
    set nwide [expr ($width - 1) / $maxwidth + 1 ]
    set nlambs [expr $ndeep * $nwide ]

    puts ""

    puts [format "total Lamb instances %5dD x %5dW = %6d" $ndeep $nwide $nlambs ]

    set deptheach [expr ($depth - 1) / $ndeep + 1 ]
    set widtheach [expr ($width - 1) / $nwide + 1 ]
    set bitseach  [expr $deptheach * $widtheach ]

    puts [format "each Lamb            %5dD x %5dW = %6d bits" $deptheach $widtheach $bitseach ]

    set physbits [expr $bitseach * $nlambs ]

    puts [format "physical Lamb bits %8d, abstract bits %8d (%5.1f%%)" $physbits $total_bits [expr ($physbits * 100.0) / ($total_bits * 1.0) ] ]

    puts ""
    # do the work for a single Lamb macro
    if { $depth > $maxdepth } {
            error "Lamb depth $depth > maxdepth $maxdepth"
    }
    if { $width > $maxwidth } {
            error "Lamb width $width > maxwidth $maxwidth"
    }

    puts "INDIVIDUAL LAMB CALCULATION"
    puts "==========================="

    set lamb_bits [expr $depth * $width ]
    set lamb_bits_area [expr $lamb_bits * $bit_area_nm2 / 1e18 ]

    puts [format "Indiv. Lamb bits      %5d x  %5d = %8d ; Lamb bits area %15le m^2" $depth $width $lamb_bits $lamb_bits_area ]

    set nchunks [expr (($depth - 1) / $chunkdepth) + 1 ]
    set lastdepth [expr $depth - ($nchunks - 1) * $chunkdepth ]
    
    puts [format "Indiv. Lamb: X chunks    %2d, last %3d deep" $nchunks $lastdepth ]

    set lamb_bits_x_pg [expr $bitx_pg * $depth ]
    set lamb_bits_y_rw [expr $bity_rw * $width ]

    set lamb_bits_x_nm [expr $lamb_bits_x_pg * $pg_nm ]
    set lamb_bits_y_nm [expr $lamb_bits_y_rw * $rw_nm ]

    puts [format "Indiv. Lamb bits:     %5d pg x %4d rows;   %5d nm x  %5d nm" \
	  $lamb_bits_x_pg $lamb_bits_y_rw $lamb_bits_x_nm $lamb_bits_y_nm ]

    set lamb_ox_pg [expr $fixed_ox_pg + $perchunk_pg * $nchunks ]
    set lamb_oy_rw $fixed_oy_rw 

    set lamb_ox_nm [expr $lamb_ox_pg * $pg_nm ]
    set lamb_oy_nm [expr $lamb_oy_rw * $rw_nm ]

    puts [format "Indiv. Lamb overheads: %4d pg %4d rows; X %5d nm Y %5d nm" \
	  $lamb_ox_pg $lamb_oy_rw $lamb_ox_nm $lamb_oy_nm ]

    set lamb_x_pg [expr $lamb_bits_x_pg + $lamb_ox_pg ]
    set lamb_y_rw [expr $lamb_bits_y_rw + $lamb_oy_rw ]
    set lamb_x_nm [expr $lamb_x_pg * $pg_nm ]
    set lamb_y_nm [expr $lamb_y_rw * $rw_nm ]

    set lamb_area [expr $lamb_x_nm * $lamb_y_nm / 1e18 ]

    puts [format "Indiv. Lamb size :     %4d pg x %4d rows:   %5d nm x  %5d nm %le m^2" \
	  $lamb_x_pg $lamb_y_rw $lamb_x_nm $lamb_y_nm $lamb_area ]
    puts [format "Indiv. Lamb area      %6.2lf sq. microns" [expr $lamb_area * 1e12 ] ]

    set lamb_eff [expr ($lamb_bits_area * 1.0) / ($lamb_area * 1.0) ]
    set oh_frac [expr ($lamb_area - $lamb_bits_area + 0.0) / ($lamb_bits_area) ]

    puts [format "Indiv. Lamb efficiency : %le / %le %5.1f%%, overhead %5.1f%%" \
	      $lamb_bits_area $lamb_area [expr $lamb_eff * 100.0 ] [expr $oh_frac * 100.0 ]]

    set total_lamb_area [expr $nlambs * $lamb_area ]

    set overall_eff [expr $bits_area / $total_lamb_area ]
    set overall_oh [expr ($total_lamb_area - $bits_area) / $bits_area ]

    puts ""
    puts "SUMMARY"
    puts "======="
    puts [format "tot area of Lambs = %e $overall_eff efficiency %5.1f%% overhead %5.1f%%" $total_lamb_area [expr $overall_eff * 100.0 ] [expr $overall_oh * 100.0 ] ]
    puts [format "tot area of Lambs = %8.2f sq. microns" [expr $total_lamb_area * 1e12 ] ]
    
    return [list $total_lamb_area $lamb_x_nm $lamb_y_nm ]
}

### Notes on proc handlest
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
define_proc_attributes gtr_lamb_area \
    -info "Utility to generate LAMB Memory collaterals" \
    -define_args {
	{-tech_node "Specify tech node (default n3b)" "AnOos" one_of_string {required {values {"n3b" "n3e" "n5"}}}}
	{-data_depth "Depth of the memory in words/entries(layout x direction)" "int" int required}
	{-data_width "Data bus width of the memory in bits(layout y direction)" "int" int required}
	{-dual_clocks "Specify if memory has dual async clocks" "" boolean optional}
	{-verbose "Verbose Reporting" "" boolean optional}
	{-debug "Report additional logging for debug purposes" "" boolean optional}
    }
