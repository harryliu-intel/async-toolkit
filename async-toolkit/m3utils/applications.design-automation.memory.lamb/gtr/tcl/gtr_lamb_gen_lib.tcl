##
## GTR Generator Tools Release v1.0.0
##
##------------------------------------------------------------------------------
##
## INTEL CONFIDENTIAL
##
## Copyright 2021 - 2022 Intel Corporation All Rights Reserved.
##
## The source code contained or described herein and all documents related
## to the source code ("Material") are owned by Intel Corporation or its
## suppliers or licensors. Title to the Material remains with Intel
## Corporation or its suppliers and licensors. The Material contains trade
## secrets and proprietary and confidential information of Intel or its
## suppliers and licensors. The Material is protected by worldwide copyright
## and trade secret laws and treaty provisions. No part of the Material may
## be used, copied, reproduced, modified, published, uploaded, posted,
## transmitted, distributed, or disclosed in any way without Intel's prior
## express written permission.
##
## No license under any patent, copyright, trade secret or other intellectual
## property right is granted to or conferred upon you by disclosure or
## delivery of the Materials, either expressly, by implication, inducement,
## estoppel or otherwise. Any license under such intellectual property rights
## must be express and approved by Intel in writing.
##
##------------------------------------------------------------------------------


proc gtr_lamb_gen_lib { args } {
    parse_proc_arguments -args $args arg
    set proc_name [lindex [regsub "::" [info level 0]  "" ] 0 ]
    set date [date ]
    if { [info exists arg(-dual_clock) ] } {
        set mem_name "1w1r1c"
    } else {
        set mem_name "1w1r2c"
    }
    if { [info exists arg(-debug) ] } {
      echo "INFO: Processing Memory $mem_name"
    }
    set block_name $arg(-block_name)
    set data_depth $arg(-data_depth)
    set data_width $arg(-data_width)
    file mkdir timing
    set libfname timing/${block_name}_[gtr_cornerSuffix $arg(-oc_type) $arg(-voltage)].lib
    puts "INFO: $proc_name, Generating Liberty view: $libfname"
    global properties
    if { [info exists arg(-filelistVar) ] } {
        upvar $arg(-filelistVar) fileList
        set thisEntry [dict create]
        dict set thisEntry path $libfname
        dict set thisEntry nda_protection_level front_end
        dict set thisEntry standard_name liberty
        dict set thisEntry type lib_ccs_filelist
        dict set thisEntry voltage $arg(-voltage)
        dict set thisEntry rc_type all_rc_types
        dict set thisEntry liberty_ccs_type ccs
        dict set thisEntry reliability_condition client
        dict set thisEntry timing_hold_margin true
        dict set thisEntry variation_modeling pocv
        dict set thisEntry oc_type $arg(-oc_type)
        dict set thisEntry feol_process_corner [dict get $properties oc_type $arg(-oc_type) feol_process_corner]
        dict set thisEntry temperature [dict get $properties oc_type $arg(-oc_type) temperature]        
        lappend fileList $thisEntry
    } 
    set power "VDD"
    set ground "VSS"
    set addr_width [expr int(ceil(log($data_depth)/log(2))) ]

    set ipins_pg [list  "VDD" "VSS" ]
    # "VBP" "VBN" -- to be added back later
    set ipins_clk [list "clk" ]
    set ipins_simple [list "wen" "ren" "icg_force_on" "test__scan_en" "dft_read_bypass" "dft__mem_wr_disable" ]
    set ipins_bus [list \
                       [list "dft__core_si" 1 0] \
                       [list "radr" [expr $addr_width - 1 ] 0] \
                       [list "wadr" [expr $addr_width - 1 ] 0 ] \
                       [list "wdata" [expr $data_width - 1 ] 0] \
                       ]
    set opins_simple [list ]
    set opins_bus [list \
                       [list "dft__core_so" 1 0] \
                       [list "dout" [expr $data_width - 1 ] 0] \
                       ]

    set of_lib [open $libfname "w" ]
    gtr_lamb_gen_lib_hdr $of_lib $block_name

    if { [info exists bus_hash ] } {
        unset array bus_hash
    }
    foreach bus "$ipins_bus $opins_bus" {
        set bname [lindex $bus 0 ]
        set from [lindex $bus 1 ]
        set to [lindex $bus 2 ]
        if { [info exists bus_hash($from,$to) ] } {
            continue
        }
        set bus_hash($from,$to) 1
        set bname [lindex $bus 0 ]
        set from [lindex $bus 1 ]
        set to [lindex $bus 2 ]
        puts $of_lib "    type (bus_type_${from}_${to}) \{"
        puts $of_lib "        base_type : array "
        puts $of_lib "        data_type : bit "
        set w [expr $from - $to + 1]
        if { $w < 0 } { 
            set w [expr 0 - $w ]
        }
        puts $of_lib "        bit_width : $w;"
        puts $of_lib "        bit_from : $from;"
        puts $of_lib "        bit_to : $to;"
        puts $of_lib "        downto : true "
        puts $of_lib "    \}"
    }
    puts $of_lib "\n"

    puts $of_lib "    cell ($block_name) \{"
    set area_x_y [gtr_lamb_area -data_width $data_width -data_depth $data_depth -tech_node $arg(-tech_node)]
    set area [lindex $area_x_y 0 ]
    set width [lindex $area_x_y 1 ]
    set height [lindex $area_x_y 2 ]
    puts $of_lib "        area :  $area;"
    set pow [gtr_lamb_power -data_width $data_width -data_depth $data_depth -power_type "leak" ]
    puts $of_lib "        cell_leakage_power : $pow;"
    puts $of_lib "        dont_touch : true;"
    puts $of_lib "        dont_use : true;"
    puts $of_lib "        is_macro_cell : true;"

    #####################################################################
    #### Tech Params, TBD Move Above
    #####################################################################
    ## synopsys latch G ~.0005 per fanout, using 5x for similar to CB Lamb
    ## HDBSVT06_BUF_CA3QS_4 0.000519
    ## HDBSVT06_BUF_CA3QY2_16/A 0.00197
    #set pincap_nom 0.00197
    set pincap_nom 0.000
    ## synopsys flop .02-0.116
    set mpw_nom 0.100
    ## max tran from stdcell lib, most use 0.31837 
    set max_tran_nom 0.31837

    foreach pin $ipins_pg {
        puts $of_lib "        pg_pin ($pin) \{"
        puts $of_lib "            voltage_name : \"$pin\";"
        if { [regexp "BP" $pin ] } {
            puts $of_lib "            pg_type : \"pwell\";"
            set pwell_pin $pin
            set is_supply 0
        } elseif { [regexp "BN" $pin ] } {
            puts $of_lib "            pg_type : \"nwell\";"
            set nwell_pin $pin
            set is_supply 0
        } elseif { [regexp "VD" $pin ] } {
            puts $of_lib "            pg_type : primary_power;"
            set is_supply 1
        } elseif { [regexp "VS" $pin ] } {
            puts $of_lib "            pg_type : primary_ground;"
            set is_supply 1
        } else {
            error "$proc_name: Cannot determine bulk type for pin $pin"
        }
        puts $of_lib "            direction : input;"
        if { $is_supply != 0 } {
#           puts $of_lib "            related_bias_pin : \"$is_supply\";"
        } else {
            puts $of_lib "            physical_connection : device_layer;"
        }
        puts $of_lib "        \}"
    }
    puts $of_lib ""
    foreach pin $ipins_clk {
	## divide by two and split between rise and fall edges
	set pow_idle [expr [gtr_lamb_power -data_width $data_width -data_depth $data_depth -power_type "idle" ] / 2.0 ]
	set pow_rd [expr [gtr_lamb_power -data_width $data_width -data_depth $data_depth -power_type "rd" ] / 2.0 ]
	set pow_wr [expr [gtr_lamb_power -data_width $data_width -data_depth $data_depth -power_type "wr" ] / 2.0 ]
	set pow_rw [expr [gtr_lamb_power -data_width $data_width -data_depth $data_depth -power_type "rw" ] / 2.0 ]
        ## save last clock
        set clk_pin $pin
        puts $of_lib "        pin ($pin) \{"
        puts $of_lib "            direction : input;"
        puts $of_lib "            clock : true;"
        #puts $of_lib "            rise_capacitance : $pincap_nom;"
        #puts $of_lib "            fall_capacitance : $pincap_nom;"
        puts $of_lib "            capacitance : $pincap_nom;"
        puts $of_lib "            max_transition : $max_tran_nom;"
        puts $of_lib "            related_ground_pin : \"VSS\";"
        puts $of_lib "            related_power_pin : \"VDD\";"
        puts $of_lib "            fanout_load :  1.0000;"
        puts $of_lib "            internal_power () \{"
        puts $of_lib "                related_pg_pin : \"VDD\";"
        puts $of_lib "                when : \"!ren&!wen\";"
        puts $of_lib "                rise_power (scalar) \{"
        puts $of_lib "                    values (\"$pow_idle\");"
        puts $of_lib "                \}"
        puts $of_lib "                fall_power (scalar) \{"
        puts $of_lib "                    values (\"$pow_idle\");"
        puts $of_lib "                \}"
        puts $of_lib "            \}"
        puts $of_lib "            internal_power () \{"
        puts $of_lib "                related_pg_pin : \"VDD\";"
        puts $of_lib "                when : \"ren&!wen\";"
        puts $of_lib "                rise_power (scalar) \{"
        puts $of_lib "                    values (\"$pow_rd\");"
        puts $of_lib "                \}"
        puts $of_lib "                fall_power (scalar) \{"
        puts $of_lib "                    values (\"$pow_rd\");"
        puts $of_lib "                \}"
        puts $of_lib "            \}"
        puts $of_lib "            internal_power () \{"
        puts $of_lib "                related_pg_pin : \"VDD\";"
        puts $of_lib "                when : \"!ren&wen\";"
        puts $of_lib "                rise_power (scalar) \{"
        puts $of_lib "                    values (\"$pow_wr\");"
        puts $of_lib "                \}"
        puts $of_lib "                fall_power (scalar) \{"
        puts $of_lib "                    values (\"$pow_wr\");"
        puts $of_lib "                \}"
        puts $of_lib "            \}"
        puts $of_lib "            internal_power () \{"
        puts $of_lib "                related_pg_pin : \"VDD\";"
        puts $of_lib "                when : \"ren&wen\";"
        puts $of_lib "                rise_power (scalar) \{"
        puts $of_lib "                    values (\"$pow_rw\");"
        puts $of_lib "                \}"
        puts $of_lib "                fall_power (scalar) \{"
        puts $of_lib "                    values (\"$pow_rw\");"
        puts $of_lib "                \}"
        puts $of_lib "            \}"
        #puts $of_lib "            timing() \{"
        #puts $of_lib "                timing_type : min_clock_tree_path;"
        #puts $of_lib "                timing_sense : positive_unate;"
        #puts $of_lib "                cell_rise (tmg_ntin_oload_8x7 ) \{"
        #puts $of_lib "                    values(\\"
        #puts $of_lib "                        \" 0.0000, 0.0000\" \\;"
        #puts $of_lib "                    );"
        #puts $of_lib "                \}"
        #puts $of_lib "            \}"
        #puts $of_lib "            timing() \{"
        #puts $of_lib "                timing_type : max_clock_tree_path;"
        #puts $of_lib "                timing_sense : positive_unate;"
        #puts $of_lib "                cell_rise (tmg_ntin_oload_8x7 ) \{"
        #puts $of_lib "                    values(\\"
        #puts $of_lib "                       \" 0.0000, 0.0000\" \\;"
        #puts $of_lib "                    );"
        #puts $of_lib "                \}"
        #puts $of_lib "            \}"
        puts $of_lib "            min_pulse_width_low : $mpw_nom;"
        puts $of_lib "            min_pulse_width_high : $mpw_nom;"
        puts $of_lib "        \}"
    }
    foreach pin $ipins_simple {
        puts $of_lib "        pin ($pin) \{"
        puts $of_lib "            direction : input;"
        #puts $of_lib "            rise_capacitance : $pincap_nom;"
        #puts $of_lib "            fall_capacitance : $pincap_nom;"
        puts $of_lib "            capacitance : $pincap_nom;"
        puts $of_lib "            max_transition : $max_tran_nom;"
        puts $of_lib "            related_ground_pin : \"VSS\";"
        puts $of_lib "            related_power_pin : \"VDD\";"
	puts $of_lib "            internal_power () \{"
        puts $of_lib "                related_pg_pin : \"VDD\";"
        puts $of_lib "                rise_power (scalar) \{"
        puts $of_lib "                    values (\"0.0000001\");"
        puts $of_lib "                \}"
        puts $of_lib "                fall_power (scalar) \{"
        puts $of_lib "                    values (\"0.0000001\");"
        puts $of_lib "                \}"
        puts $of_lib "            \}"
        puts $of_lib "            timing() \{"
        puts $of_lib "                timing_type : setup_rising ;"
        puts $of_lib "                related_pin : \"$clk_pin\";"
        puts $of_lib "                rise_constraint (lut_timing_2) \{"
        puts $of_lib "                    values(\\"
        puts $of_lib "                        \" -0.0640, -0.0640, -0.0682, -0.0731, -0.0813, -0.0931, -0.1104, -0.1350\",  \\"
        puts $of_lib "                        \" -0.0639, -0.0639, -0.0681, -0.0730, -0.0812, -0.0930, -0.1102, -0.1349\",  \\"
        puts $of_lib "                        \" -0.0587, -0.0587, -0.0629, -0.0678, -0.0760, -0.0878, -0.1050, -0.1296\",  \\"
        puts $of_lib "                        \" -0.0526, -0.0526, -0.0568, -0.0617, -0.0699, -0.0817, -0.0989, -0.1236\",  \\"
        puts $of_lib "                        \" -0.0415, -0.0415, -0.0457, -0.0505, -0.0588, -0.0705, -0.0878, -0.1124\",  \\"
        puts $of_lib "                        \" -0.0246, -0.0246, -0.0288, -0.0336, -0.0418, -0.0536, -0.0709, -0.0955\",  \\"
        puts $of_lib "                        \" -0.0031, -0.0031, -0.0073, -0.0121, -0.0204, -0.0322, -0.0494, -0.0740\",  \\"
        puts $of_lib "                        \" 0.0264, 0.0264, 0.0221, 0.0173, 0.0091, -0.0027, -0.0200, -0.0446\"  \\"
        puts $of_lib "                    );"
        puts $of_lib "                \}"
        puts $of_lib "                fall_constraint (lut_timing_2) \{"
        puts $of_lib "                    values(\\"
        puts $of_lib "                        \" -0.0609, -0.0609, -0.0651, -0.0700, -0.0782, -0.0900, -0.1073, -0.1319\",  \\"
        puts $of_lib "                        \" -0.0608, -0.0608, -0.0650, -0.0699, -0.0781, -0.0899, -0.1071, -0.1318\",  \\"
        puts $of_lib "                        \" -0.0559, -0.0559, -0.0601, -0.0649, -0.0731, -0.0849, -0.1022, -0.1268\",  \\"
        puts $of_lib "                        \" -0.0506, -0.0506, -0.0548, -0.0596, -0.0678, -0.0796, -0.0969, -0.1215\",  \\"
        puts $of_lib "                        \" -0.0406, -0.0406, -0.0449, -0.0497, -0.0579, -0.0697, -0.0870, -0.1116\",  \\"
        puts $of_lib "                        \" -0.0261, -0.0261, -0.0303, -0.0352, -0.0434, -0.0552, -0.0725, -0.0971\",  \\"
        puts $of_lib "                        \" -0.0090, -0.0090, -0.0133, -0.0181, -0.0263, -0.0381, -0.0554, -0.0800\",  \\"
        puts $of_lib "                        \" 0.0114, 0.0114, 0.0072, 0.0024, -0.0058, -0.0176, -0.0349, -0.0595\"  \\"
        puts $of_lib "                    );"
        puts $of_lib "                \}"
        puts $of_lib "            \}"
        puts $of_lib "            timing() \{"
        puts $of_lib "                timing_type : hold_rising ;"
        puts $of_lib "                related_pin :\"$clk_pin\";"
        puts $of_lib "                rise_constraint (lut_timing_2) \{"
        puts $of_lib "                    values(\\"
        puts $of_lib "                        \" 0.1030, 0.1030, 0.1074, 0.1125, 0.1213, 0.1337, 0.1516, 0.1768\",  \\"
        puts $of_lib "                        \" 0.1031, 0.1031, 0.1075, 0.1126, 0.1214, 0.1338, 0.1517, 0.1769\",  \\"
        puts $of_lib "                        \" 0.0998, 0.0998, 0.1041, 0.1092, 0.1181, 0.1304, 0.1483, 0.1736\",  \\"
        puts $of_lib "                        \" 0.0959, 0.0959, 0.1003, 0.1054, 0.1142, 0.1266, 0.1445, 0.1698\",  \\"
        puts $of_lib "                        \" 0.0898, 0.0898, 0.0942, 0.0993, 0.1081, 0.1205, 0.1384, 0.1637\",  \\"
        puts $of_lib "                        \" 0.0813, 0.0813, 0.0857, 0.0908, 0.0996, 0.1120, 0.1299, 0.1551\",  \\"
        puts $of_lib "                        \" 0.0696, 0.0696, 0.0740, 0.0791, 0.0879, 0.1003, 0.1182, 0.1435\",  \\"
        puts $of_lib "                        \" 0.0533, 0.0533, 0.0576, 0.0627, 0.0715, 0.0839, 0.1018, 0.1271\"  \\"
        puts $of_lib "                    );"
        puts $of_lib "                \}"
        puts $of_lib "                fall_constraint (lut_timing_2) \{"
        puts $of_lib "                    values(\\"
        puts $of_lib "                        \" 0.1049, 0.1049, 0.1093, 0.1144, 0.1232, 0.1356, 0.1535, 0.1787\",  \\"
        puts $of_lib "                        \" 0.1051, 0.1051, 0.1094, 0.1145, 0.1233, 0.1357, 0.1536, 0.1788\",  \\"
        puts $of_lib "                        \" 0.1013, 0.1013, 0.1057, 0.1108, 0.1196, 0.1320, 0.1499, 0.1752\",  \\"
        puts $of_lib "                        \" 0.0979, 0.0979, 0.1022, 0.1073, 0.1162, 0.1286, 0.1464, 0.1716\",  \\"
        puts $of_lib "                        \" 0.0928, 0.0928, 0.0972, 0.1023, 0.1111, 0.1235, 0.1414, 0.1667\",  \\"
        puts $of_lib "                        \" 0.0866, 0.0866, 0.0910, 0.0961, 0.1049, 0.1173, 0.1352, 0.1604\",  \\"
        puts $of_lib "                        \" 0.0794, 0.0794, 0.0838, 0.0889, 0.0977, 0.1101, 0.1280, 0.1532\",  \\"
        puts $of_lib "                        \" 0.0725, 0.0725, 0.0769, 0.0819, 0.0908, 0.1032, 0.1211, 0.1462\"  \\"
        puts $of_lib "                     );"
        puts $of_lib "                \}"
        puts $of_lib "            \}"
        puts $of_lib "        \}"
    }
    foreach bus $ipins_bus {
        set bname [lindex $bus 0 ]
        set from [lindex $bus 1 ]
        set to [lindex $bus 2 ]
        set bname [lindex $bus 0 ]
        set from [lindex $bus 1 ]
        set to [lindex $bus 2 ]
        puts $of_lib "        bus ($bname) \{"
        puts $of_lib "            bus_type : bus_type_${from}_${to};"
	puts $of_lib "            direction : input;"
        set diff [expr $from - $to ]
        if { $diff < 0 } { 
            set w [expr 0 - $diff ]
        } else {
            set w diff
        }
        puts $of_lib "            related_ground_pin : \"VSS\";"
        puts $of_lib "            related_power_pin : \"VDD\";"
        if { $from < $to } {
            error "$proc_name: Unsupported decreasing bits range for bus $bname"
        }
        #puts $of_lib "            rise_capacitance : $pincap_nom;"
        #puts $of_lib "            fall_capacitance : $pincap_nom;"
        puts $of_lib "            capacitance : $pincap_nom;"
        puts $of_lib "            max_transition : $max_tran_nom;"
        puts $of_lib "            fanout_load : 1.000;"
        for { set i $from } { $i >= $to } { set i [expr $i - 1 ] } {
            puts $of_lib "            pin ($bname\[$i\]) \{"

	    puts $of_lib "                internal_power () \{"
	    puts $of_lib "                    related_pg_pin : \"VDD\";"
	    puts $of_lib "                    rise_power (scalar) \{"
	    puts $of_lib "                        values (\"0.0000001\");"
	    puts $of_lib "                    \}"
	    puts $of_lib "                    fall_power (scalar) \{"
	    puts $of_lib "                        values (\"0.0000001\");"
	    puts $of_lib "                    \}"
	    puts $of_lib "                \}"

            puts $of_lib "                timing() \{"
            puts $of_lib "                    timing_type : setup_rising ;"
            puts $of_lib "                    related_pin : \"$clk_pin\";"
            puts $of_lib "                    rise_constraint (lut_timing_2) \{"
            puts $of_lib "                        values(\\"
            puts $of_lib "                            \" -0.0640, -0.0640, -0.0682, -0.0731, -0.0813, -0.0931, -0.1104, -0.1350\",  \\"
            puts $of_lib "                            \" -0.0639, -0.0639, -0.0681, -0.0730, -0.0812, -0.0930, -0.1102, -0.1349\",  \\"
            puts $of_lib "                            \" -0.0587, -0.0587, -0.0629, -0.0678, -0.0760, -0.0878, -0.1050, -0.1296\",  \\"
            puts $of_lib "                            \" -0.0526, -0.0526, -0.0568, -0.0617, -0.0699, -0.0817, -0.0989, -0.1236\",  \\"
            puts $of_lib "                            \" -0.0415, -0.0415, -0.0457, -0.0505, -0.0588, -0.0705, -0.0878, -0.1124\",  \\"
            puts $of_lib "                            \" -0.0246, -0.0246, -0.0288, -0.0336, -0.0418, -0.0536, -0.0709, -0.0955\",  \\"
            puts $of_lib "                            \" -0.0031, -0.0031, -0.0073, -0.0121, -0.0204, -0.0322, -0.0494, -0.0740\",  \\"
            puts $of_lib "                            \" 0.0264, 0.0264, 0.0221, 0.0173, 0.0091, -0.0027, -0.0200, -0.0446\"  \\"
            puts $of_lib "                         );"
            puts $of_lib "                    \}"
            puts $of_lib "                    fall_constraint (lut_timing_2) \{"
            puts $of_lib "                        values(\\"
            puts $of_lib "                            \" -0.0609, -0.0609, -0.0651, -0.0700, -0.0782, -0.0900, -0.1073, -0.1319\",  \\"
            puts $of_lib "                            \" -0.0608, -0.0608, -0.0650, -0.0699, -0.0781, -0.0899, -0.1071, -0.1318\",  \\"
            puts $of_lib "                            \" -0.0559, -0.0559, -0.0601, -0.0649, -0.0731, -0.0849, -0.1022, -0.1268\",  \\"
            puts $of_lib "                            \" -0.0506, -0.0506, -0.0548, -0.0596, -0.0678, -0.0796, -0.0969, -0.1215\",  \\"
            puts $of_lib "                            \" -0.0406, -0.0406, -0.0449, -0.0497, -0.0579, -0.0697, -0.0870, -0.1116\",  \\"
            puts $of_lib "                            \" -0.0261, -0.0261, -0.0303, -0.0352, -0.0434, -0.0552, -0.0725, -0.0971\",  \\"
            puts $of_lib "                            \" -0.0090, -0.0090, -0.0133, -0.0181, -0.0263, -0.0381, -0.0554, -0.0800\",  \\"
            puts $of_lib "                            \" 0.0114, 0.0114, 0.0072, 0.0024, -0.0058, -0.0176, -0.0349, -0.0595\"  \\"
            puts $of_lib "                                );"
            puts $of_lib "                    \}"
            puts $of_lib "                \}"
            puts $of_lib "                timing() \{"
            puts $of_lib "                    timing_type : hold_rising ;"
            puts $of_lib "                    related_pin :\"$clk_pin\";"
            puts $of_lib "                    rise_constraint (lut_timing_2) \{"
            puts $of_lib "                        values(\\"
            puts $of_lib "                            \" 0.1030, 0.1030, 0.1074, 0.1125, 0.1213, 0.1337, 0.1516, 0.1768\",  \\"
            puts $of_lib "                            \" 0.1031, 0.1031, 0.1075, 0.1126, 0.1214, 0.1338, 0.1517, 0.1769\",  \\"
            puts $of_lib "                            \" 0.0998, 0.0998, 0.1041, 0.1092, 0.1181, 0.1304, 0.1483, 0.1736\",  \\"
            puts $of_lib "                            \" 0.0959, 0.0959, 0.1003, 0.1054, 0.1142, 0.1266, 0.1445, 0.1698\",  \\"
            puts $of_lib "                            \" 0.0898, 0.0898, 0.0942, 0.0993, 0.1081, 0.1205, 0.1384, 0.1637\",  \\"
            puts $of_lib "                            \" 0.0813, 0.0813, 0.0857, 0.0908, 0.0996, 0.1120, 0.1299, 0.1551\",  \\"
            puts $of_lib "                            \" 0.0696, 0.0696, 0.0740, 0.0791, 0.0879, 0.1003, 0.1182, 0.1435\",  \\"
            puts $of_lib "                            \" 0.0533, 0.0533, 0.0576, 0.0627, 0.0715, 0.0839, 0.1018, 0.1271\"  \\"
            puts $of_lib "                                );"
            puts $of_lib "                    \}"
            puts $of_lib "                    fall_constraint (lut_timing_2) \{"
            puts $of_lib "                        values(\\"
            puts $of_lib "                            \" 0.1049, 0.1049, 0.1093, 0.1144, 0.1232, 0.1356, 0.1535, 0.1787\",  \\"
            puts $of_lib "                            \" 0.1051, 0.1051, 0.1094, 0.1145, 0.1233, 0.1357, 0.1536, 0.1788\",  \\"
            puts $of_lib "                            \" 0.1013, 0.1013, 0.1057, 0.1108, 0.1196, 0.1320, 0.1499, 0.1752\",  \\"
            puts $of_lib "                            \" 0.0979, 0.0979, 0.1022, 0.1073, 0.1162, 0.1286, 0.1464, 0.1716\",  \\"
            puts $of_lib "                            \" 0.0928, 0.0928, 0.0972, 0.1023, 0.1111, 0.1235, 0.1414, 0.1667\",  \\"
            puts $of_lib "                            \" 0.0866, 0.0866, 0.0910, 0.0961, 0.1049, 0.1173, 0.1352, 0.1604\",  \\"
            puts $of_lib "                            \" 0.0794, 0.0794, 0.0838, 0.0889, 0.0977, 0.1101, 0.1280, 0.1532\",  \\"
            puts $of_lib "                            \" 0.0725, 0.0725, 0.0769, 0.0819, 0.0908, 0.1032, 0.1211, 0.1462\"  \\"
            puts $of_lib "                                );"
            puts $of_lib "                    \}"
            puts $of_lib "                \}"
            puts $of_lib "            \}"
        }
        puts $of_lib "        \}"
    }
    foreach pin $opins_simple {
        puts $of_lib "        pin($pin) \{"
	puts $of_lib "            direction : output;"
        puts $of_lib "            related_ground_pin : \"VSS\";"
        puts $of_lib "            related_power_pin : \"VDD\";"
        puts $of_lib "            max_transition : $max_tran_nom;"
        puts $of_lib "            internal_power () \{"
	puts $of_lib "                related_pg_pin : \"VDD\";"
	puts $of_lib "                rise_power (scalar) \{"
	puts $of_lib "                    values (\"0.0000001\");"
	puts $of_lib "                \}"
	puts $of_lib "                fall_power (scalar) \{"
	puts $of_lib "                    values (\"0.0000001\");"
	puts $of_lib "                \}"
	puts $of_lib "            \}"
        puts $of_lib "            timing() \{"
        puts $of_lib "                related_pin : \"$clk_pin\";"
        puts $of_lib "                timing_sense : non_unate "
        puts $of_lib "                timing_type : rising_edge"
        puts $of_lib "                rise_transition (lut_timing_6) \{"
        puts $of_lib "                    values(\\"
        puts $of_lib "                        \" 0.0059, 0.0088, 0.0154, 0.0265, 0.0509, 0.1021, 0.2060, 0.4143\",  \\"
        puts $of_lib "                        \" 0.0059, 0.0088, 0.0154, 0.0265, 0.0509, 0.1021, 0.2060, 0.4143\",  \\"
        puts $of_lib "                        \" 0.0059, 0.0088, 0.0154, 0.0265, 0.0509, 0.1021, 0.2060, 0.4143\",  \\"
        puts $of_lib "                        \" 0.0059, 0.0088, 0.0154, 0.0265, 0.0509, 0.1021, 0.2060, 0.4143\",  \\"
        puts $of_lib "                        \" 0.0059, 0.0088, 0.0154, 0.0265, 0.0509, 0.1021, 0.2060, 0.4143\",  \\"
        puts $of_lib "                        \" 0.0059, 0.0088, 0.0154, 0.0265, 0.0509, 0.1021, 0.2060, 0.4143\",  \\"
        puts $of_lib "                        \" 0.0059, 0.0088, 0.0154, 0.0265, 0.0509, 0.1021, 0.2060, 0.4143\",  \\"
        puts $of_lib "                        \" 0.0059, 0.0088, 0.0154, 0.0265, 0.0509, 0.1021, 0.2060, 0.4143\"  \\"
        puts $of_lib "                    );"
        puts $of_lib "                \}"
        puts $of_lib "                fall_transition (lut_timing_6) \{"
        puts $of_lib "                    values(\\"
        puts $of_lib "                        \" 0.0063, 0.0092, 0.0158, 0.0261, 0.0489, 0.0978, 0.1970, 0.3957\",  \\"
        puts $of_lib "                        \" 0.0063, 0.0092, 0.0158, 0.0261, 0.0489, 0.0978, 0.1970, 0.3957\",  \\"
        puts $of_lib "                        \" 0.0063, 0.0092, 0.0158, 0.0261, 0.0489, 0.0978, 0.1970, 0.3957\",  \\"
        puts $of_lib "                        \" 0.0063, 0.0092, 0.0158, 0.0261, 0.0489, 0.0978, 0.1970, 0.3957\",  \\"
        puts $of_lib "                        \" 0.0063, 0.0092, 0.0158, 0.0261, 0.0489, 0.0978, 0.1970, 0.3957\",  \\"
        puts $of_lib "                        \" 0.0063, 0.0092, 0.0158, 0.0261, 0.0489, 0.0978, 0.1970, 0.3957\",  \\"
        puts $of_lib "                        \" 0.0063, 0.0092, 0.0158, 0.0261, 0.0489, 0.0978, 0.1970, 0.3957\",  \\"
        puts $of_lib "                        \" 0.0063, 0.0092, 0.0158, 0.0261, 0.0489, 0.0978, 0.1970, 0.3957\"  \\"
        puts $of_lib "                    );"
        puts $of_lib "                \}"
        puts $of_lib "                cell_rise (lut_timing_6) \{"
        puts $of_lib "                    values(\\"
        puts $of_lib "                        \" 0.2277, 0.2305, 0.2358, 0.2435, 0.2567, 0.2819, 0.3313, 0.4296\", \\"
        puts $of_lib "                        \" 0.2277, 0.2305, 0.2358, 0.2435, 0.2567, 0.2819, 0.3313, 0.4296\",  \\"
        puts $of_lib "                        \" 0.2319, 0.2346, 0.2399, 0.2477, 0.2609, 0.2860, 0.3354, 0.4337\",  \\"
        puts $of_lib "                        \" 0.2368, 0.2395, 0.2448, 0.2526, 0.2658, 0.2909, 0.3404, 0.4386\",  \\"
        puts $of_lib "                        \" 0.2450, 0.2477, 0.2531, 0.2608, 0.2740, 0.2991, 0.3486, 0.4468\",  \\"
        puts $of_lib "                        \" 0.2569, 0.2596, 0.2649, 0.2727, 0.2859, 0.3110, 0.3605, 0.4587\",  \\"
        puts $of_lib "                        \" 0.2741, 0.2768, 0.2821, 0.2899, 0.3031, 0.3282, 0.3776, 0.4759\",  \\"
        puts $of_lib "                        \" 0.2985, 0.3012, 0.3066, 0.3143, 0.3275, 0.3526, 0.4021, 0.5003\"  \\"
        puts $of_lib "                    );"
        puts $of_lib "                \}"
        puts $of_lib "                cell_fall (lut_timing_6) \{"
        puts $of_lib "                    values(\\"
        puts $of_lib "                        \" 0.2364, 0.2393, 0.2451, 0.2533, 0.2669, 0.2926, 0.3428, 0.4433\",  \\"
        puts $of_lib "                        \" 0.2364, 0.2393, 0.2451, 0.2533, 0.2669, 0.2926, 0.3428, 0.4433\",  \\"
        puts $of_lib "                        \" 0.2405, 0.2434, 0.2492, 0.2574, 0.2710, 0.2967, 0.3470, 0.4474\",  \\"
        puts $of_lib "                        \" 0.2454, 0.2483, 0.2542, 0.2623, 0.2759, 0.3016, 0.3519, 0.4523\",  \\"
        puts $of_lib "                        \" 0.2536, 0.2566, 0.2624, 0.2705, 0.2841, 0.3098, 0.3601, 0.4605\",  \\"
        puts $of_lib "                        \" 0.2655, 0.2684, 0.2743, 0.2824, 0.2960, 0.3217, 0.3720, 0.4724\",  \\"
        puts $of_lib "                        \" 0.2827, 0.2856, 0.2915, 0.2996, 0.3132, 0.3389, 0.3892, 0.4896\",  \\"
        puts $of_lib "                        \" 0.3072, 0.3101, 0.3160, 0.3241, 0.3377, 0.3634, 0.4137, 0.5141\"  \\"
        puts $of_lib "                    )"
        puts $of_lib "                \}"
        puts $of_lib "            \}"
        puts $of_lib "        \}"
    }
    foreach bus $opins_bus {
        set bname [lindex $bus 0 ]
        set from [lindex $bus 1 ]
        set to [lindex $bus 2 ]
        puts $of_lib "        bus($bname) \{"
        puts $of_lib "            bus_type : bus_type_${from}_${to};"
	puts $of_lib "            direction : output;"
        set diff [expr $from - $to ]
        if { $diff < 0 } { 
            set w [expr 0 - $diff ]
        } else {
                set w diff
        }
        puts $of_lib "            related_ground_pin : \"VSS\";"
        puts $of_lib "            related_power_pin : \"VDD\";"
        puts $of_lib "            max_transition : $max_tran_nom;"
        if { $from < $to } {
            error "$proc_name: Unsupported decreasing bits range for bus $bname"
        }
        for { set i $from } { $i >= $to } { set i [expr $i - 1 ] } {
            puts $of_lib "            pin ($bname\[$i\]) \{"
	    puts $of_lib "                internal_power () \{"
	    puts $of_lib "                    related_pg_pin : \"VDD\";"
	    puts $of_lib "                    rise_power (scalar) \{"
	    puts $of_lib "                        values (\"0.0000001\");"
	    puts $of_lib "                    \}"
	    puts $of_lib "                    fall_power (scalar) \{"
	    puts $of_lib "                        values (\"0.0000001\");"
	    puts $of_lib "                    \}"
	    puts $of_lib "                \}"
            puts $of_lib "                timing() \{"
            puts $of_lib "                    related_pin : \"$clk_pin\";"
            puts $of_lib "                    timing_sense : non_unate "
            puts $of_lib "                    timing_type : rising_edge"
            puts $of_lib "                    rise_transition (lut_timing_6) \{"
            puts $of_lib "                        values(\\"
            puts $of_lib "                            \" 0.0059, 0.0088, 0.0154, 0.0265, 0.0509, 0.1021, 0.2060, 0.4143\",  \\"
            puts $of_lib "                            \" 0.0059, 0.0088, 0.0154, 0.0265, 0.0509, 0.1021, 0.2060, 0.4143\",  \\"
            puts $of_lib "                            \" 0.0059, 0.0088, 0.0154, 0.0265, 0.0509, 0.1021, 0.2060, 0.4143\",  \\"
            puts $of_lib "                            \" 0.0059, 0.0088, 0.0154, 0.0265, 0.0509, 0.1021, 0.2060, 0.4143\",  \\"
            puts $of_lib "                            \" 0.0059, 0.0088, 0.0154, 0.0265, 0.0509, 0.1021, 0.2060, 0.4143\",  \\"
            puts $of_lib "                            \" 0.0059, 0.0088, 0.0154, 0.0265, 0.0509, 0.1021, 0.2060, 0.4143\",  \\"
            puts $of_lib "                            \" 0.0059, 0.0088, 0.0154, 0.0265, 0.0509, 0.1021, 0.2060, 0.4143\",  \\"
            puts $of_lib "                            \" 0.0059, 0.0088, 0.0154, 0.0265, 0.0509, 0.1021, 0.2060, 0.4143\"  \\"
            puts $of_lib "                        );"
            puts $of_lib "                    \}"
            puts $of_lib "                    fall_transition (lut_timing_6) \{"
            puts $of_lib "                        values(\\"
            puts $of_lib "                            \" 0.0063, 0.0092, 0.0158, 0.0261, 0.0489, 0.0978, 0.1970, 0.3957\",  \\"
            puts $of_lib "                            \" 0.0063, 0.0092, 0.0158, 0.0261, 0.0489, 0.0978, 0.1970, 0.3957\",  \\"
            puts $of_lib "                            \" 0.0063, 0.0092, 0.0158, 0.0261, 0.0489, 0.0978, 0.1970, 0.3957\",  \\"
            puts $of_lib "                            \" 0.0063, 0.0092, 0.0158, 0.0261, 0.0489, 0.0978, 0.1970, 0.3957\",  \\"
            puts $of_lib "                            \" 0.0063, 0.0092, 0.0158, 0.0261, 0.0489, 0.0978, 0.1970, 0.3957\",  \\"
            puts $of_lib "                            \" 0.0063, 0.0092, 0.0158, 0.0261, 0.0489, 0.0978, 0.1970, 0.3957\",  \\"
            puts $of_lib "                            \" 0.0063, 0.0092, 0.0158, 0.0261, 0.0489, 0.0978, 0.1970, 0.3957\",  \\"
            puts $of_lib "                            \" 0.0063, 0.0092, 0.0158, 0.0261, 0.0489, 0.0978, 0.1970, 0.3957\"  \\"
            puts $of_lib "                        );"
            puts $of_lib "                    \}"
            puts $of_lib "                    cell_rise (lut_timing_6) \{"
            puts $of_lib "                        values(\\"
            puts $of_lib "                            \" 0.2277, 0.2305, 0.2358, 0.2435, 0.2567, 0.2819, 0.3313, 0.4296\", \\"
            puts $of_lib "                            \" 0.2277, 0.2305, 0.2358, 0.2435, 0.2567, 0.2819, 0.3313, 0.4296\",  \\"
            puts $of_lib "                            \" 0.2319, 0.2346, 0.2399, 0.2477, 0.2609, 0.2860, 0.3354, 0.4337\",  \\"
            puts $of_lib "                            \" 0.2368, 0.2395, 0.2448, 0.2526, 0.2658, 0.2909, 0.3404, 0.4386\",  \\"
            puts $of_lib "                            \" 0.2450, 0.2477, 0.2531, 0.2608, 0.2740, 0.2991, 0.3486, 0.4468\",  \\"
            puts $of_lib "                            \" 0.2569, 0.2596, 0.2649, 0.2727, 0.2859, 0.3110, 0.3605, 0.4587\",  \\"
            puts $of_lib "                            \" 0.2741, 0.2768, 0.2821, 0.2899, 0.3031, 0.3282, 0.3776, 0.4759\",  \\"
            puts $of_lib "                            \" 0.2985, 0.3012, 0.3066, 0.3143, 0.3275, 0.3526, 0.4021, 0.5003\"  \\"
            puts $of_lib "                        );"
            puts $of_lib "                    \}"
            puts $of_lib "                    cell_fall (lut_timing_6) \{"
            puts $of_lib "                        values(\\"
            puts $of_lib "                            \" 0.2364, 0.2393, 0.2451, 0.2533, 0.2669, 0.2926, 0.3428, 0.4433\",  \\"
            puts $of_lib "                            \" 0.2364, 0.2393, 0.2451, 0.2533, 0.2669, 0.2926, 0.3428, 0.4433\",  \\"
            puts $of_lib "                            \" 0.2405, 0.2434, 0.2492, 0.2574, 0.2710, 0.2967, 0.3470, 0.4474\",  \\"
            puts $of_lib "                            \" 0.2454, 0.2483, 0.2542, 0.2623, 0.2759, 0.3016, 0.3519, 0.4523\",  \\"
            puts $of_lib "                            \" 0.2536, 0.2566, 0.2624, 0.2705, 0.2841, 0.3098, 0.3601, 0.4605\",  \\"
            puts $of_lib "                            \" 0.2655, 0.2684, 0.2743, 0.2824, 0.2960, 0.3217, 0.3720, 0.4724\",  \\"
            puts $of_lib "                            \" 0.2827, 0.2856, 0.2915, 0.2996, 0.3132, 0.3389, 0.3892, 0.4896\",  \\"
            puts $of_lib "                            \" 0.3072, 0.3101, 0.3160, 0.3241, 0.3377, 0.3634, 0.4137, 0.5141\"  \\"
            puts $of_lib "                        )"
            puts $of_lib "                    \}"
            puts $of_lib "                \}"
            puts $of_lib "            \}"
        }
        puts $of_lib "        \}"
    }
    puts $of_lib "    \}"
    puts $of_lib "\}"
    close $of_lib
    return $libfname
}

define_proc_attributes gtr_lamb_gen_lib \
    -info "Utility to generate Lib Memory collaterals" \
    -define_args {
	{-block_name "Specify memory name" "<block_name>" string required}
	{-tech_node "Specify tech node (default n3b)" "AnOos" one_of_string {required {values {"n3b" "n3e" "n5"}}}}
	{-data_depth "Specify the depth" "int" int required}
	{-data_width "Specify the data bus width in bits" "int" int required}
	{-dual_clocks "Specify if dual async clocks are to be used" "" boolean optional}
	{-oc_type "Operating condition" "AnOos" one_of_string {required {values {"S_125" "S_M40" "S_0" "F_125" "F_M40" "F_0" "T_25" "T_85" }}}}
	{-voltage "Voltage condition" "voltage" string required}
   {-filelistVar "Update filelist for manifest.xml" "" string optional}
	{-debug "Report additional logging for debug purposes" "" boolean optional}
}


proc gtr_lamb_gen_lib_hdr { of_lib lib_name } {
    set proc_name [lindex [regsub "::" [info level 0]  "" ] 0 ]
    set date [date ]
    set copyright_note "Copyright 2021 Intel"
    puts $of_lib "\/* Generated by XFG GTR Flow: $proc_name *\/\n"
    puts $of_lib "library (${lib_name}) \{"
    puts $of_lib "    date : \"[date ]\";"
    puts $of_lib "    revision : \"[gtr_get_revision ]\";"
    puts $of_lib "    comment : \"$copyright_note\";" 
    puts $of_lib "    delay_model : table_lookup;"
    puts $of_lib "    voltage_unit : \"1V\";"
    puts $of_lib "    current_unit : \"1mA\";"
    puts $of_lib "    leakage_power_unit : \"1nW\";"
    puts $of_lib "    time_unit : \"1ns\";"
    puts $of_lib "    capacitive_load_unit ( 1.0000,pf);"
    puts $of_lib "    pulling_resistance_unit : \"1kohm\";"
    puts $of_lib "    default_threshold_voltage_group : SVT06;"
    puts $of_lib "\n    \/* Library parameters *\/"
    puts $of_lib "    library_features(report_delay_calculation, report_power_calculation, report_noise_calculation);"
    puts $of_lib "    in_place_swap_mode : match_footprint;"
    puts $of_lib "\n    \/* Attribute defaults *\/"
    puts $of_lib "    default_max_transition : 2.0000;"
    puts $of_lib "    default_cell_leakage_power : 0.0000;"
    puts $of_lib "    default_leakage_power_density : 0.0000;"
    puts $of_lib "    default_max_fanout : 50.0000;"
    puts $of_lib "    default_fanout_load : 1.0000;"
    puts $of_lib "    default_inout_pin_cap : 0.0000;"
    puts $of_lib "    default_input_pin_cap : 0.0000;"
    puts $of_lib "    default_output_pin_cap : 0.0000;"
    puts $of_lib "    default_wire_load_area :0.0000;"
    puts $of_lib "    default_wire_load_capacitance :0.0000;"
    puts $of_lib "    default_wire_load_resistance : 3.7000;"
    puts $of_lib "\n     \/* Threshold definitions *\/"
    puts $of_lib "    slew_derate_from_library : 0.5000;"
    puts $of_lib "    input_threshold_pct_rise : 50.0000;"
    puts $of_lib "    input_threshold_pct_fall : 50.0000;"
    puts $of_lib "    output_threshold_pct_rise : 50.0000;"
    puts $of_lib "    output_threshold_pct_fall : 50.0000;"
    puts $of_lib "    slew_lower_threshold_pct_rise : 30.0000;"
    puts $of_lib "    slew_upper_threshold_pct_rise : 70.0000;"
    puts $of_lib "    slew_upper_threshold_pct_fall : 70.0000;"
    puts $of_lib "    slew_lower_threshold_pct_fall : 30.0000;"
    puts $of_lib "\n    \/* Operating conditions *\/"
    puts $of_lib "    nom_process : 0.999998;"
    puts $of_lib "    nom_temperature : 0.0000;"
    puts $of_lib "    nom_voltage : 0.6750;"
    puts $of_lib "    operating_conditions (SSGNP0P675VN40C )\{"
    puts $of_lib "        process : 0.999998;"
    puts $of_lib "        temperature : 0.0000;"
    puts $of_lib "        voltage : 0.6750;"
    puts $of_lib "        tree_type : \"best_case_tree\";"
    puts $of_lib "    \}"
    puts $of_lib "    default_operating_conditions : \"SSGNP0P675VN40C\";"
    puts $of_lib "\n    \/* Derating parameters *\/"
    puts $of_lib "    k_process_cell_leakage_power : 0.0000;"
    puts $of_lib "    k_process_internal_power : 0.0000;"
    puts $of_lib "    k_temp_cell_fall : 0.0000;"
    puts $of_lib "    k_temp_cell_leakage_power : 0.0000;"
    puts $of_lib "    k_temp_cell_rise : 0.0000;"
    puts $of_lib "    k_temp_fall_propagation : 0.0000;"
    puts $of_lib "    k_temp_fall_transition : 0.0000;"
    puts $of_lib "    k_temp_hold_fall : 0.0000;"
    puts $of_lib "    k_temp_hold_rise : 0.0000;"
    puts $of_lib "    k_temp_internal_power : 0.0000;"
    puts $of_lib "    k_temp_rise_propagation : 0.0000;"
    puts $of_lib "    k_temp_setup_fall : 0.0000;"
    puts $of_lib "    k_temp_setup_rise : 0.0000;"
    puts $of_lib "    k_volt_cell_fall : 0.0000;"
    puts $of_lib "    k_volt_cell_leakage_power : 0.0000;"
    puts $of_lib "    k_volt_cell_rise : 0.0000;"
    puts $of_lib "    k_volt_fall_propagation : 0.0000;"
    puts $of_lib "    k_volt_rise_transition : 0.0000;"
    puts $of_lib "    k_volt_fall_transition : 0.0000;"
    puts $of_lib "    k_volt_hold_fall : 0.0000;"
    puts $of_lib "    k_volt_hold_rise : 0.0000;"
    puts $of_lib "    k_volt_internal_power : 0.0000;"
    puts $of_lib "    k_volt_rise_propagation : 0.0000;"
    puts $of_lib "    k_volt_setup_fall : 0.0000;"
    puts $of_lib "    k_volt_setup_rise : 0.0000;"
    puts $of_lib "\n    \/* Lookup tables: *\/"
    puts $of_lib "    lu_table_template (ccsn_dc_template) \{"
    puts $of_lib "        variable_1 : input_voltage;"
    puts $of_lib "        variable_2 : output_voltage;"
    puts $of_lib "    \}"
    puts $of_lib "    lu_table_template (ccsn_prop_template) \{"
    puts $of_lib "        variable_1 : input_noise_height;"
    puts $of_lib "        variable_2 : input_noise_width;"
    puts $of_lib "        variable_3 : total_output_net_capacitance;"
    puts $of_lib "        variable_4 : time;"
    puts $of_lib "    \}"
    puts $of_lib "    lu_table_template (ccsn_vout_template) \{"
    puts $of_lib "        variable_1 : input_net_transition;"
    puts $of_lib "        variable_2 : total_output_net_capacitance;"
    puts $of_lib "        variable_3 : time;"
    puts $of_lib "    \}"
    puts $of_lib "    lu_table_template (cnst_ctin_5) \{"
    puts $of_lib "        variable_1 : constrained_pin_transition;"
    puts $of_lib "        index_1 (\"1, 2, 3, 4, 5\");"
    puts $of_lib "    \}"
    puts $of_lib "    lu_table_template (cnst_ctin_rtin_5x5) \{"
    puts $of_lib "        variable_1 : constrained_pin_transition;"
    puts $of_lib "        variable_2 : related_pin_transition;"
    puts $of_lib "        index_1 (\"1, 2, 3, 4, 5\");"
    puts $of_lib "        index_2 (\"1, 2, 3, 4, 5\");"
    puts $of_lib "    \}"
    puts $of_lib "    lu_table_template (ndw_ntin_nvolt_8x27) \{"
    puts $of_lib "        variable_1 : input_net_transition;"
    puts $of_lib "        variable_2 : normalized_voltage;"
    puts $of_lib "        index_1 (\"1, 2, 3, 4, 5, 6, 7, 8\");"
    puts $of_lib "        index_2 (\"1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27\");"
    puts $of_lib "    \}"
    puts $of_lib "    lu_table_template (ndw_ntin_nvolt_11x27) \{"
    puts $of_lib "        variable_1 : input_net_transition;"
    puts $of_lib "        variable_2 : normalized_voltage;"
    puts $of_lib "        index_1 (\"1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11\");"
    puts $of_lib "        index_2 (\"1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27\");"
    puts $of_lib "    \}"
    puts $of_lib "    lu_table_template (tmg_ntin_8) \{"
    puts $of_lib "        variable_1 : input_net_transition;"
    puts $of_lib "        index_1 (\"1, 2, 3, 4, 5, 6, 7, 8\");"
    puts $of_lib "    \}"
    puts $of_lib "    lu_table_template (lut_timing_2) \{"
    puts $of_lib "        variable_1 : constrained_pin_transition;"
    puts $of_lib "        index_1 (\" 0.0000, 0.0040, 0.0154, 0.0309, 0.0618, 0.1236, 0.2471, 0.4943\");"
    puts $of_lib "        variable_2 : related_pin_transition;"
    puts $of_lib "        index_2 (\" 0.0000, 0.0040, 0.0154, 0.0309, 0.0618, 0.1236, 0.2471, 0.4943\");"
    puts $of_lib "    \}"
    puts $of_lib "    lu_table_template (lut_timing_6) \{"
    puts $of_lib "        variable_1 : input_net_transition;"
    puts $of_lib "        index_1 (\" 0.0000, 0.0040, 0.0154, 0.0309, 0.0618, 0.1236, 0.2471, 0.4943\");"
    puts $of_lib "        variable_2 : total_output_net_capacitance;"
    puts $of_lib "        index_2 (\" 0.0005, 0.0015, 0.0043, 0.0099, 0.0211, 0.0434, 0.0880, 0.1772\");"
    puts $of_lib "    \}"
    puts $of_lib "    lu_table_template (tmg_ntin_oload_8x7) \{"
    puts $of_lib "        variable_1 : input_net_transition;"
    puts $of_lib "        variable_2 : total_output_net_capacitance;"
    puts $of_lib "        index_1 (\"1, 2, 3, 4, 5, 6, 7, 8\");"
    puts $of_lib "        index_2 (\"1, 2, 3, 4, 5, 6, 7\");"
    puts $of_lib "    \}"
    puts $of_lib "    power_lut_template (pwr_tin_8) \{"
    puts $of_lib "        variable_1 : input_transition_time;"
    puts $of_lib "        index_1 (\"1, 2, 3, 4, 5, 6, 7, 8\");"
    puts $of_lib "    \}"
    puts $of_lib "    power_lut_template (pwr_tin_oload_8x7) \{"
    puts $of_lib "        variable_1 : input_transition_time;"
    puts $of_lib "        variable_2 : total_output_net_capacitance;"
    puts $of_lib "        index_1 (\"1, 2, 3, 4, 5, 6, 7, 8\");"
    puts $of_lib "        index_2 (\"1, 2, 3, 4, 5, 6, 7\");"
    puts $of_lib "    \}"
    #puts $of_lib "    output_current_template (ccs_template) \{"
    #puts $of_lib "        variable_1 : input_net_transition;"
    #puts $of_lib "        variable_2 : total_output_net_capacitance;"
    #puts $of_lib "        variable_3 : time;"
    #puts $of_lib "    \}\n"
    #puts $of_lib "    driver_model :\"emulated\";"
    #puts $of_lib "    emulated_driver_ratio : \"0.5000\";"
    puts $of_lib "    voltage_map (VDD , 0.6750);"
    puts $of_lib "    voltage_map (VDDC, 0.6750);"
    puts $of_lib "    voltage_map (VDDP, 0.6750);"
    # puts $of_lib "    voltage_map (VBP, 0.6750);"
    puts $of_lib "    voltage_map (VSS, 0.0000);"
    # puts $of_lib "    voltage_map (VBN, 0.0000);"
    puts $of_lib "    voltage_map (VDDCF, 0.6750);"
    #puts $of_lib "    receiver_capacitance_rise_threshold_pct (\"0.0 10.0 30.0 50.0 60.0 70.0 80.0 90.0 100.0\");"
    #puts $of_lib "    receiver_capacitance_fall_threshold_pct (\"100.0 90.0 80.0 70.0 60.0 50.0 30.0 10.0 0.0\");"
    puts $of_lib "\n    /* Cell definitions:: */"
    #puts $of_lib "    normalized_driver_waveform (ndw_ntin_nvolt_8x27) \{"
    #puts $of_lib "        driver_waveform_name : \"preDrv\";"
    #puts $of_lib "        index_1 (\"0.0021084, 0.00433068, 0.00889529, 0.0182711, 0.037529, 0.0770851, 0.158334, 0.32522\");"
    #puts $of_lib "        index_2 (\"0, 0.05, 0.167424, 0.3, 0.416813, 0.520871, 0.61461, 0.7, 0.778634, 0.851802, 0.920547, 0.935714, 0.957915, 0.972449, 0.981964, 0.988192, 0.99227, 0.99494, 0.996687, 0.997831, 0.99858, 0.999071, 0.999392, 0.999602, 0.999739, 0.999829, 1\");"
    #puts $of_lib "        values (\"0, 0.00026355, 0.0005271, 0.00079065, 0.0010542, 0.00131775, 0.0015813, 0.00184485, 0.0021084, 0.00237195, 0.0026355, 0.00289905, 0.00342615, 0.00395325, 0.00448035, 0.00500745, 0.00553455, 0.00606165, 0.00658875, 0.00711585, 0.00764295, 0.00817005, 0.00869715, 0.00922425, 0.00975135, 0.0102784, 0.0108056\", \\"
    #puts $of_lib "              \"0, 0.000541335, 0.00108267, 0.00162401, 0.00216534, 0.00270668, 0.00324801, 0.00378935, 0.00433068, 0.00487202, 0.00541335, 0.00595469, 0.00703736, 0.00812003, 0.0092027, 0.0102854, 0.011368, 0.0124507, 0.0135334, 0.0146161, 0.0156987, 0.0167814, 0.0178641, 0.0189467, 0.0200294, 0.0211121, 0.0221948\", \\"
    #puts $of_lib "              \"0, 0.00111191, 0.00222382, 0.00333573, 0.00444764, 0.00555955, 0.00667146, 0.00778338, 0.00889529, 0.0100072, 0.0111191, 0.012231, 0.0144548, 0.0166787, 0.0189025, 0.0211263, 0.0233501, 0.025574, 0.0277978, 0.0300216, 0.0322454, 0.0344692, 0.036693, 0.0389169, 0.0411407, 0.0433645, 0.0455883\", \\"
    #puts $of_lib "              \"0, 0.00228388, 0.00456776, 0.00685164, 0.00913552, 0.0114194, 0.0137033, 0.0159872, 0.0182711, 0.0205549, 0.0228388, 0.0251227, 0.0296905, 0.0342582, 0.038826, 0.0433937, 0.0479615, 0.0525293, 0.057097, 0.0616648, 0.0662325, 0.0708003, 0.0753681, 0.0799358, 0.0845036, 0.0890713, 0.0936391\", \\"
    #puts $of_lib "              \"0, 0.00469112, 0.00938225, 0.0140734, 0.0187645, 0.0234556, 0.0281467, 0.0328379, 0.037529, 0.0422201, 0.0469112, 0.0516023, 0.0609846, 0.0703668, 0.0797491, 0.0891313, 0.0985136, 0.107896, 0.117278, 0.12666, 0.136043, 0.145425, 0.154807, 0.164189, 0.173571, 0.182954, 0.192336\", \\"
    #puts $of_lib "              \"0, 0.00963564, 0.0192713, 0.0289069, 0.0385425, 0.0481782, 0.0578138, 0.0674494, 0.0770851, 0.0867207, 0.0963563, 0.105992, 0.125263, 0.144535, 0.163806, 0.183077, 0.202348, 0.22162, 0.240891, 0.260162, 0.279433, 0.298705, 0.317976, 0.337247, 0.356519, 0.37579, 0.395061\", \\"
    #puts $of_lib "              \"0, 0.0197917, 0.0395835, 0.0593752, 0.0791669, 0.0989587, 0.11875, 0.138542, 0.158334, 0.178126, 0.197917, 0.217709, 0.257292, 0.296876, 0.33646, 0.376043, 0.415626, 0.45521, 0.494793, 0.534377, 0.57396, 0.613544, 0.653127, 0.692711, 0.732294, 0.771878, 0.811461\", \\"
    #puts $of_lib "              \"0, 0.0406525, 0.081305, 0.121957, 0.16261, 0.203263, 0.243915, 0.284567, 0.32522, 0.365872, 0.406525, 0.447178, 0.528482, 0.609788, 0.691092, 0.772397, 0.853703, 0.935007, 1.01631, 1.09762, 1.17892, 1.26023, 1.34153, 1.42284, 1.50414, 1.58545, 1.66675\");"
    #puts $of_lib "      \}"

}

