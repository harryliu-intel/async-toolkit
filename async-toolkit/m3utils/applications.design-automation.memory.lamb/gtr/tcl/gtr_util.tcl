## GTR Generator Tools Release v1.0.0

## Helper Function to work around primitive TCL date function
proc gtr_get_revision { } {
    set proc_name [lindex [regsub "::" [info level 0]  "" ] 0 ]
    set date [date ]
    set month [lindex $date 1 ]
    switch $month {
        "Jan" { set mm "01" }
        "Feb" { set mm "02" }
        "Mar" { set mm "03" }
        "Apr" { set mm "04" }
        "May" { set mm "05" }
        "Jun" { set mm "06" }
        "Jul" { set mm "07" }
        "Aug" { set mm "08" }
        "Sep" { set mm "09" }
        "Oct" { set mm "10" }
        "Nov" { set mm "11" }
        "Dec" { set mm "12" }
        default {
            error "$proc_name: Cannot decode month string: $month"
        }
    }
    set revision "[lindex $date 4].$mm.[lindex $date 2]"
    return $revision
}

proc xxgtr_copy_files { lt depth width } {
    set lamb "cdp_lamb_${::__variant}_${lt}_${depth}d_${width}b"
    set src_root_dir  "cdp_lamb_${lt}";
    set dst_root_dir "/nfs/sc/disks/tfc_ip/memlister/TFC_memories/lamb/$lamb/$lamb"
    file mkdir $dst_root_dir/rtl/verilog
    file copy cdp_lamb_$lt.sv $dst_root_dir/rtl/verilog/$lamb.sv
    system("sed -i 's/depth/$depth/' $dst_root_dir/rtl/verilog/$lamb.sv");
    system("sed -i 's/width/$width/' $dst_root_dir/rtl/verilog/$lamb.sv");
    file mkdir $dst_root_dir/timing_full
    system("cp lib_lef/$lamb.lib $dst_root_dir/timing_full/");
    file mkdir $dst_root_dir/layout_abstract/lef_5.8
    file copy cp $lamb.lef $dst_root_dir/layout_abstract/lef_5.8/
    file mkdir $dst_root_dir/ndm_2018
    file copy $lamb.ndm $dst_root_dir/ndm_2018/
}


proc gtr_lamb_gen_views { args } {
    set proc_name [lindex [regsub "::" [info level 0]  "" ] 0 ]
    set date [date ]
    parse_proc_arguments -args $args arg
    if { [info exists arg(-verbose) ] } {
        set verbose 1
    } else {
        set verbose 0
    }
    echo "INFO: $proc_name, date: $date"

    if { [info exists arg(-tech_node) ] } {
	set tech_node $arg(-tech_node)
    } else {
	set tech_node "n3b"
    }
    if { [info exists arg(-variant_type) ] } {
	set variant_type $arg(-variant_type)
    } else {
	set variant_type "n3bhd"
    }
    if { [info exists arg(-data_width) ] && \
	     ( [info exists arg(-min_data_width) ] || [info exists arg(-max_data_width) ] ) } {
	error "You cannot specify min/max data width options when using -data_width"
    }
    if { [info exists arg(-data_depth) ] && \
	     ( [info exists arg(-min_data_depth) ] || [info exists arg(-max_data_depth) ] ) } {
	error "You cannot specify min/max data depth options when using -data_depth"
    }
    if { [info exists arg(-min_data_width) ] } {
	set min_width $arg(-min_data_width)
    } else {
	set min_width 2
    }
    if { [info exists arg(-max_data_width) ] } {
	set max_width $arg(-max_data_width)
    } else {
	set max_width 2
    }

    ## fixed width is a base case of the looping
    if { [info exists arg(-data_width) ] } {
	set min_width $arg(-data_width)
	set max_width $arg(-data_width)
    }

    ## check data against reasonable bounds
    if { $min_width < 2 } {
	error "min_width must be greater than or equal to two"
    }
    if { $min_width > 256 } {
	error "min_width must be less than or equal to 256"
    }
    if { $max_width < $min_width } {
	error "max_width must be greater than or equal to -min_width"
    }

    #### now depth
    if { [info exists arg(-data_depth) ] && \
	     ( [info exists arg(-min_data_depth) ] || [info exists arg(-max_data_depth) ] ) } {
	error "You cannot specify min/max data depth options when using -data_depth"
    }
    if { [info exists arg(-data_depth) ] && \
	     ( [info exists arg(-min_data_depth) ] || [info exists arg(-max_data_depth) ] ) } {
	error "You cannot specify min/max data depth options when using -data_depth"
    }
    if { [info exists arg(-min_data_depth) ] } {
	set min_depth $arg(-min_data_depth)
    } else {
	set min_depth 4
    }
    if { [info exists arg(-max_data_depth) ] } {
	set max_depth $arg(-max_data_depth)
    } else {
	set max_depth 4
    }

    ## fixed depth is a base case of the looping
    if { [info exists arg(-data_depth) ] } {
	set min_depth $arg(-data_depth)
	set max_depth $arg(-data_depth)
    }

    ## check data against reasonable bounds
    if { $min_depth < 4 } {
	error "min_depth must be greater than or equal to four"
    }
    if { $min_depth > 256 } {
	error "min_depth must be less than or equal to 256"
    }
    if { $max_depth < $min_depth } {
	error "max_depth must be greater than or equal to -min_depth"
    }
    #set lambtypes [list "1r1w1c" "1r1w2c" ]                                                                                     
    set lambtypes [list "1r1w1c" ]
    foreach lt $lambtypes {
        for { set depth $min_depth } { $depth <= $max_depth } { set depth [expr $depth + 2 ] } {
            for { set width $min_width } { $width <= $max_width } { set width [expr $width + 2 ] } {
                set block_name "cdp_lamb_${variant_type}_${lt}_${depth}d_${width}b"
		gtr_lamb_gen_lib -block_name $block_name -data_depth $depth -data_width $width
		gtr_lamb_gen_lef -block_name $block_name -data_depth $depth -data_width $width
		gtr_lamb_gen_behav_sv -block_name $block_name -data_depth $depth -data_width $width
                gtr_gen_ndm -block_name $block_name -lef_file $block_name.lef -lib_file $block_name.lib -tech_node $tech_node
            }
        }
    }
}



define_proc_attributes gtr_lamb_gen_views \
    -info "Generate set of views for Lambs" \
    -define_args {
	{-all "Loop through all the default Lamb sizes" "" boolean optional}
	{-data_width "Specify a single data width for the Lamb(s)" "<data_width>" int optional}
	{-min_data_width "Specify min data width to start building loop(default 2)" "<min_data_width>" int optional}
	{-max_data_width "Specify max data width to stop building loop(default 144)" "<max_data_width>" int optional}
	{-data_width_increment "Specify increment to use when looping to generate lambs(default 2)" "<data_width_increment>" int optional}
	{-data_depth "Specify a single data depth for the Lamb(s)" "<data_depth>" int optional}
	{-min_data_depth "Specify min data depth to start building loop(default 4)" "<min_data_depth>" int optional}
	{-max_data_depth "Specify max data depth to stop building loop(default 128)" "<max_data_depth>" int optional}
	{-data_depth_increment "Specify increment to use when looping to generate lambs(default 2)" "<data_depth_increment>" int optional}
	{-tech_node "Specify tech node (default n3b)" "AnOos" one_of_string {optional {values {"n3b" "n3e" "n5"}}}}
	{-lamb_type "Specify lamb type (default )" "AnOos" one_of_string {optional {values {"n3bhd" "n3ehd" "n5hd"}}}}
	{-variant_type "Specify variant type (default n3bhd)" "AnOos" one_of_string {optional {values {"n3bhd" "n3ehd" "n5hd"}}}}
	{-dual_clocks "Specify if dual async clocks are to be used(Depreciated)" "" boolean optional}
	{-debug "Report additional logging for debug purposes" "" boolean optional}
    }
#        {-Oos "oos help"       AnOos   one_of_string {required value_help {values {a b}}}}                                      
