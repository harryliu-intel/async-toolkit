## GTR Generator Tools Release v1.0.0

set supported_min_depth        4
set supported_min_width        2
set supported_max_depth      256
set supported_max_width      144
set supported_width_divisor    2
set supported_depth_divisor    2

set cornerlibs [list]
set properties [dict create]

set proc_properties [dict create]
dict set proc_properties ssgnp sigma -3
dict set proc_properties tt    sigma  0
dict set proc_properties ffgnp sigma +3

dict set proc_properties ssgnp oc_pfx  S
dict set proc_properties tt    oc_pfx  T
dict set proc_properties ffgnp oc_pfx  F

set metal_rc_sigma_tbl [dict create]
dict set metal_rc_sigma_tbl typical           rsigma  0
dict set metal_rc_sigma_tbl typical           csigma  0

dict set metal_rc_sigma_tbl cworst_CCworst_T  rsigma  0
dict set metal_rc_sigma_tbl cworst_CCworst_T  csigma -1.5

dict set metal_rc_sigma_tbl rcworst_CCworst_T rsigma -1.5
dict set metal_rc_sigma_tbl rcworst_CCworst_T csigma -1.5

dict set metal_rc_sigma_tbl cworst_CCworst    rsigma  0
dict set metal_rc_sigma_tbl cworst_CCworst    csigma -3.0

dict set metal_rc_sigma_tbl rcworst_CCworst   rsigma -3.0
dict set metal_rc_sigma_tbl rcworst_CCworst   csigma -3.0

dict set metal_rc_sigma_tbl cworst_CCbest_T   rsigma  0
dict set metal_rc_sigma_tbl cworst_CCbest_T   csigma +1.5

dict set metal_rc_sigma_tbl cbest_CCbest_T    rsigma  0
dict set metal_rc_sigma_tbl cbest_CCbest_T    csigma +1.5

dict set metal_rc_sigma_tbl cbest_CCbest      rsigma  0
dict set metal_rc_sigma_tbl cbest_CCbest      csigma +3.0

dict set metal_rc_sigma_tbl rcbest_CCbest     rsigma +3.0
dict set metal_rc_sigma_tbl rcbest_CCbest     csigma +3.0

#                           proc   wiring             V        Tproc Tintcon
# these are lib corners from Karthik's presentation
set lib_corners0p75 [list \
                     [ list ssgnp cworst_CCworst_T  0.675        0       0  ]\
                     [ list ssgnp cworst_CCworst_T  0.675      125     125  ]\
                     [ list tt    typical           0.750       85     105  ]\
                     [ list ffgnp typical           0.770      105     105  ]\
                     [ list ffgnp cbest_CCbest_T    0.825        0       0  ]\
                     [ list ffgnp cbest_CCbest_T    0.825      125     125  ]\
                    ]

#                           proc   wiring             V        Tproc Tintcon
# these are design corners from Karthik's presentation
set design_corners0p75 [list \
                     [ list ssgnp cworst_CCworst_T  0.675        0       0  ]\
                     [ list ssgnp cworst_CCworst    0.675        0       0  ]\
                     [ list ssgnp rcworst_CCworst_T 0.675        0       0  ]\
                     [ list ssgnp rcworst_CCworst   0.675        0       0  ]\
                     [ list ssgnp cworst_CCworst_T  0.675      125     125  ]\
                     [ list ssgnp cworst_CCworst    0.675      125     125  ]\
                     [ list ssgnp rcworst_CCworst_T 0.675      125     125  ]\
                     [ list ssgnp rcworst_CCworst   0.675      125     125  ]\
                     [ list tt    typical           0.750       85     105  ]\
                     [ list ffgnp typical           0.770      105     105  ]\
                     [ list ffgnp cbest_CCbest      0.825        0       0  ]\
                     [ list ffgnp rcbest_CCbest     0.825        0       0  ]\
                     [ list ffgnp cbest_CCbest      0.825      125     125  ]\
                     [ list ffgnp rcbest_CCbest     0.825      125     125  ]\
                     [ list ffgnp cworst_CCworst    0.825        0       0  ]\
                     [ list ffgnp rcworst_CCworst   0.825        0       0  ]\
                     [ list ffgnp cworst_CCworst    0.825      125     125  ]\
                     [ list ffgnp rcworst_CCworst   0.825      125     125  ]\
                    ]

set design_corners0p70 [list \
                     [ list ssgnp cworst_CCworst_T  0.630        0       0  ]\
                     [ list ssgnp cworst_CCworst    0.630        0       0  ]\
                     [ list ssgnp rcworst_CCworst_T 0.630        0       0  ]\
                     [ list ssgnp rcworst_CCworst   0.630        0       0  ]\
                     [ list ssgnp cworst_CCworst_T  0.630      125     125  ]\
                     [ list ssgnp cworst_CCworst    0.630      125     125  ]\
                     [ list ssgnp rcworst_CCworst_T 0.630      125     125  ]\
                     [ list ssgnp rcworst_CCworst   0.630      125     125  ]\
                     [ list tt    typical           0.700       85     105  ]\
                     [ list ffgnp typical           0.770      105     105  ]\
                     [ list ffgnp cbest_CCbest      0.825        0       0  ]\
                     [ list ffgnp rcbest_CCbest     0.825        0       0  ]\
                     [ list ffgnp cbest_CCbest      0.825      125     125  ]\
                     [ list ffgnp rcbest_CCbest     0.825      125     125  ]\
                     [ list ffgnp cworst_CCworst    0.825        0       0  ]\
                     [ list ffgnp rcworst_CCworst   0.825        0       0  ]\
                     [ list ffgnp cworst_CCworst    0.825      125     125  ]\
                     [ list ffgnp rcworst_CCworst   0.825      125     125  ]\
                    ]

# for testing use the below:
#                           proc   wiring             V        Tproc Tintcon
set debug_corners1  [list \
                     [ list ssgnp cworst_CCworst_T  0.675        0       0  ]\
                    ]

#                           proc   wiring             V        Tproc Tintcon
set debug_corners2 [list \
                    [ list ssgnp cworst_CCworst_T  0.675        0       0  ]\
                    [ list ssgnp cworst_CCworst_T  0.675      125     125  ]\
                       ]

#set desired_corners $lib_corners0p75
set desired_corners $design_corners0p75
#set desired_corners $debug_corners2

proc gtr_produce_attributes { libname process_name filelistVar } {
   set fn "doc/${libname}.attribute.xml"
   file mkdir [file dirname $fn]
   if { [info exists filelistVar ] } {
        upvar $filelistVar fileList
        set thisEntry [dict create]
        dict set thisEntry path $fn
        dict set thisEntry nda_protection_level front_end        
        dict set thisEntry function documentation
        dict set thisEntry type attribute_xml
        lappend fileList $thisEntry
    }
   ## TODO: refine specification here -- probably should not be interface_type sram
   set af [open $fn w]
   puts $af "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
   puts $af "<attributes>"
   puts $af "<process_name>${process_name}</process_name>"
   puts $af "<block_name>${libname}</block_name>"
   puts $af "<heml>4</heml>"
   puts $af "<hpml>6</hpml>"
   puts $af "<hsml>4</hsml>"
   puts $af "<lay_iface_type>IPN3M18CU-IP</lay_iface_type>"
   puts $af "<lay_integration_type>APRB_H169P51</lay_integration_type>"
   puts $af "</attributes>"
   close $af
   exec xmllint $fn
}

## Produce manifest.xml, from files assembled in filelist by prior steps
proc gtr_generateManifest { libname filelist } {
   set mf [open manifest.xml w]
   set version [exec git describe]
   puts $mf "<root>"
   puts $mf "<lib name=\"$libname\" version=\"${version}\">"
   puts $mf "<Files>"
   foreach f $filelist {
      puts $mf "<file>"
      foreach { key } [dict keys $f] {
         set value [dict get $f $key]
         puts $mf "<${key}>${value}</${key}>"
      } 
      puts $mf "</file>"
   }
   puts $mf "</Files>"
   puts $mf "<Properties>"
   global properties
   foreach  { oc_type} [dict keys [dict get $properties oc_type]] {
      puts $mf "<property name=\"oc_type\" type=\"tag\" value=\"${oc_type}\">"
      foreach  { item } [dict keys [dict get $properties oc_type $oc_type]] {
         set value [dict get $properties oc_type $oc_type $item]
         puts $mf "<${item}>${value}</${item}>"
      }
      puts $mf "</property>"
   } 
   puts $mf "</Properties>"   
   puts $mf "</lib>"
   puts $mf "</root>"
   close $mf
   exec xmllint --format manifest.xml
}

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
    global desired_corners
    global cornerlistfns
    global supported_min_depth
    global supported_min_width
    global supported_max_depth
    global supported_max_width
    global supported_depth_divisor
    global supported_width_divisor

    set proc_name [lindex [regsub "::" [info level 0]  "" ] 0 ]
    set date [date ]
    parse_proc_arguments -args $args arg
    if { [info exists arg(-verbose) ] } {
        set verbose 1
    } else {
        set verbose 0
    }
    echo "INFO: $proc_name, date: $date"
    if { [info exists arg(-skipNDM) ] } {
       set ndmGenerate 0
    } else {
	   set ndmGenerate 1
    } 
    if { [info exists arg(-flow_through) ] } {
        set flowthrough 1
    } else {
	set flowthrough 0
    }
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
      set max_width 144
    }

    ## fixed width is a base case of the looping
    if { [info exists arg(-data_width) ] } {
      set min_width $arg(-data_width)
      set max_width $arg(-data_width)
    }

    ## check data against reasonable bounds
    if { $min_width < $supported_min_width } {
        error [ format "min_width must be greater than or equal to %d" $supported_min_width ]
    }
    if { $max_width > $supported_max_width } {
        error [format "max_width must be less than or equal to %d" $supported_max_width ]
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
      set max_depth 128
    }

    ## fixed depth is a base case of the looping
    if { [info exists arg(-data_depth) ] } {
	set min_depth $arg(-data_depth)
	set max_depth $arg(-data_depth)
    }

    ## check data against reasonable bounds
    if { $min_depth < $supported_min_depth } {
        error [ format "min_depth must be greater than or equal to %d" $supported_min_depth ]
    }
    if { $max_depth > $supported_max_depth } {
        error [ format "max_depth must be less than or equal to %d" $supported_max_depth ]
    }
    if { $max_depth < $min_depth } { error "max_depth must be greater than or equal to -min_depth" }
    #set lambtypes [list "1r1w1c" "1r1w2c" ]
    ## for now we will stripe the netbatch runs where each worker will build a full swath of ranges for a different depth.
    ## the runtime impact of depth is smaller since it only adds slightly to address bits vs more data.
    ## Current thinking is that for 5k lambs run it would have a good number of max machines 
    ## used to ~ 144/2=~72 and nb limit without feeder of ~500
    if { $flowthrough == 1 } {
        set lambtype "1ftr1w1c"
    } else {
        set lambtype "1r1w1c"
    }

    ############################################################
    #
    # check width and depth multiple of supported step size
    # we don't really need to do this but it probably represents the
    # principle of least surprise.
    #

    if { [ expr $min_depth % $supported_depth_divisor ] != 0 } {
        error [ format "min depth %d not divisible by %d" $min_depth $supported_depth_divisor ]
    }
    if { [ expr $max_depth % $supported_depth_divisor ] != 0 } {
        error [ format "max depth %d not divisible by %d" $max_depth $supported_depth_divisor ]
    }

    if { [ expr $min_width % $supported_width_divisor ] != 0 } {
        error [ format "min width %d not divisible by %d" $min_width $supported_width_divisor ]
    }
    if { [ expr $max_width % $supported_width_divisor ] != 0 } {
        error [ format "max width %d not divisible by %d" $max_width $supported_width_divisor ]
    }
    
    for { set depth $min_depth } { $depth <= $max_depth } { set depth [expr $depth + $supported_depth_divisor ] } {
   if { [info exists arg(-netbatch) ] } {
       set cmd_file "gtr_nbatch_cmd_cdp_lamb_${variant_type}_${lambtype}_${depth}d_${min_width}_${max_width}b.tcl"
       set of [open $cmd_file "w" ]
       puts $of "# Batch script to load TCL utilities and build a set of Lambs."
       puts $of "# Generated by GTR Generators Tool Release: $proc_name : $date\n"
     puts $of "# Load GTR:\nsource $::env(GTR_ROOT)/tcl/gtr_main.tcl"
   }
   set filelist [list]
   set lambsProduced 0
   for { set width $min_width } { $width <= $max_width } { set width [expr $width + $supported_width_divisor ] } {
      if { "$variant_type" == "n5hd" } {
        set block_name "cdp_lamb_${lambtype}_${depth}d_${width}b"
      } else {
        set block_name "cdp_lamb_${variant_type}_${lambtype}_${depth}d_${width}b"
      }
      if { [info exists arg(-netbatch) ] } {
        puts $of "gtr_lamb_gen_lib -block_name $block_name -data_depth $depth -data_width $width -tech_node $tech_node"
        puts $of "gtr_lamb_gen_lef -block_name $block_name -data_depth $depth -data_width $width -tech_node $tech_node"
        puts $of "gtr_lamb_gen_behav_sv -block_name $block_name -data_depth $depth -data_width $width -ftr_value $flowthrough"
        if { $ndmGenerate } { puts $of "gtr_gen_ndm -block_name $block_name -lef_file $block_name.lef -lib_file $block_name.lib -tech_node $tech_node\n" }
      } else {
        # count lambs actually produced (above it more of a recipe to produce in the future)
        incr lambsProduced +1          
        ## eventually, below should loop over corners
        set oc_type S_0
        set voltage 0.675

        # generate Liberty file for LAMB
        puts [list "desired corners " $desired_corners ]
        
#        set ndmlib [gtr_lamb_gen_lib -block_name $block_name \
#                        -data_depth $depth -data_width $width \
#                        -tech_node $tech_node -oc_type $oc_type \
#                        -voltage $voltage \
#                        -filelistVar filelist \
#                        -ftr_value $flowthrough]
#
#        puts [list "ndmlib is " $ndmlib ]

        foreach {c} $desired_corners {
            global proc_properties
            global metal_rc_sigma_tbl
            global properties

            set gtr_home $::env(GTR_HOME)
            set gtr_root ${gtr_home}/..
            
            set template_dir ${gtr_root}/libscaler/templates
            
            puts [list "doing corner " $c]
            set sicornernam [ lindex $c 0 ]
            set mtcornernam [ lindex $c 1 ]
            set v           [ lindex $c 2 ]
            set sitemp      [ lindex $c 3 ]
            set mttemp      [ lindex $c 4 ]

            set sicorner    [ dict get $proc_properties    $sicornernam  sigma ]
            set rcorner     [ dict get $metal_rc_sigma_tbl $mtcornernam rsigma ]
            set ccorner     [ dict get $metal_rc_sigma_tbl $mtcornernam csigma ]
            set vtxt [ regsub 0. [format %.3f $v] "0p" ]
            set sittxt [ regsub -- - $sitemp M ]
            set mtttxt [ regsub -- - $mttemp M ]

            puts [ list "voltage " $v " -> " $vtxt ]

            set pvtname [ format %s_%s_%s_%s_%s \
                              $sicornernam $vtxt $sittxt $mtttxt $mtcornernam ]

            set oc_pfx  [ dict get $proc_properties $sicornernam oc_pfx ]
            set oc_temp [ regsub -- - $sitemp M ]
            set oc_type [ format %s_%s $oc_pfx $oc_temp ]
            
            dict set properties oc_type $pvtname feol_process_corner $sicornernam
            dict set properties oc_type $pvtname temperature $sitemp

            puts [ list "pvtname" $pvtname ]

            # note that the templates use N7 names since they are N7 files
            if { $flowthrough == 0 } {
                # regular synchronous read template
                set template ${template_dir}/cdp_lamb_1w1sr_template.lib
            } else {
                # flow-through (asynchronous read) template
                set template ${template_dir}/cdp_lamb_1w1afr_template.lib
            }

            set libname [ format %s_%s $block_name $pvtname ]
            set dbfname [ format %s_%s.db $block_name $pvtname ]
            puts [ list "libname" $libname ]

            set workdir "timing"
            set path [ format %s/%s.lib $workdir $libname ]
            puts [ list "path" $path ]
            
            gtr_lamb_scale_lib \
                -w          $width \
                -d          $depth\
                -tech       $tech_node\
                -template   $template\
                -cellname   $block_name \
                -name       $libname \
                -path       $path \
                -sitemp     $sitemp \
                -mttemp     $mttemp \
                -v          $v\
                -sicorner   $sicorner \
                -rcorner    $rcorner \
                -ccorner    $ccorner\
                -pvtname    $pvtname

            lappend cornerlibs [ list $pvtname $path ]

            if { 1 } {
                set thisEntry [dict create]
                dict set thisEntry path                  $path
                dict set thisEntry nda_protection_level  front_end
                dict set thisEntry standard_name         liberty
                dict set thisEntry type                  lib_ccs_filelist
                dict set thisEntry voltage               $v
                dict set thisEntry rc_type               $mtcornernam
                dict set thisEntry liberty_ccs_type      ccs
                dict set thisEntry reliability_condition client
                dict set thisEntry timing_hold_margin    true
                dict set thisEntry variation_modeling    pocv
                dict set thisEntry oc_type               $oc_type
                dict set thisEntry feol_process_corner   $sicornernam
                dict set thisEntry temperature           $sitemp
                lappend filelist $thisEntry
            }
            
            # generate binary DB format using Liberty file as input

            puts "generating db..."
            
            gtr_lamb_gen_db \
                -block_name     $block_name \
                -lib_file       $path       \
                -fname          $dbfname    \
                -voltage        $v          \
                -oc_type        $oc_type    \
                -feol_corner    $sicorner   \
                -temperature    $sitemp     \
                -rc_type        $mtcornernam\
                -filelist_var   filelist
        }


        puts [list "cornerlibs " $cornerlibs ]

        # generate the LEF for the LAMB
        set ndmlef \
            [gtr_lamb_gen_lef -block_name $block_name \
                 -data_depth $depth \
                 -data_width $width \
                 -tech_node $tech_node \
                 -filelistVar filelist \
                 -ftr_value $flowthrough]

        if { $ndmGenerate } {
            # generate NDM file for LAMB
            # NDM contains: LEF, all the timing models for the LAMB
            # controversial whether this step should be handled here...
            # will need a list of tuples of process labels and lib files
            gtr_gen_ndm -block_name $block_name\
                -lef_file $ndmlef \
                -tech_node $tech_node \
                -filelistVar filelist
        }
        
        # generate the behavioral SV model for the LAMB
        gtr_lamb_gen_behav_sv -block_name $block_name -data_depth $depth -data_width $width -filelistVar filelist -ftr_value $flowthrough

    }
   }
   set process_name n3b
   if { $lambsProduced > 1} {
      gtr_produce_attributes lamb_lib $process_name filelist
      gtr_generateManifest lamb_lib $filelist
   } else {
      gtr_produce_attributes $block_name $process_name filelist
      gtr_generateManifest $block_name $filelist
   }

   if { [info exists arg(-netbatch) ] } {
       close $of
       ## do launch here
       set cmd "/usr/intel/bin/nbjob run --qslot /bfn/be --class \"SLES12&&2G\" $::env(GTR_FC_ROOT)/bin/icc2_lm_shell -x \"source $cmd_file\""
       puts "INFO: cmd:$cmd"
       exec {*}$cmd
   }
 }
}



define_proc_attributes gtr_lamb_gen_views \
    -info "Generate set of views for Lambs" \
    -define_args {
   {-flow_through "Make views for flow-through LAMB(s)" "" boolean optional }
   {-all "Loop through all the default Lamb sizes" "" boolean optional}
   {-netbatch "Run jobs striped using netbatch" "" boolean optional}
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
   {-skipNDM "Skip NDM abstracts" "" boolean optional}	
   {-debug "Report additional logging for debug purposes" "" boolean optional}
    }
#        {-Oos "oos help"       AnOos   one_of_string {required value_help {values {a b}}}}                                      
