# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0

# ensure min_pulse_width is noncratering
proc noncrater_mpw {lib} {
    foreach cell [pub::get_obj_list $lib type cell] {
        foreach pin [pub::get_obj_match $cell {type pin}] {
            foreach mpw [pub::get_obj_match $pin {type timing valCmpFunc equal keyValList {timing_type min_pulse_width}}] {
                foreach rf {rise fall} {
                    set match [list type "${rf}_constraint"]
                    set const [pub::get_obj_match $mpw $match]
                    if {$const ne {}} {
                        set indices [pub::get_obj_attr $const index_1]
                        set values [pub::get_obj_attr $const values]
                        set newvals {}
                        foreach index $indices value $values {
                            set min [expr {$index/0.6*1.1}]
                            if {$min-$value>1.0} {
                                set value [format "%.3f" $min]
                            }
                            lappend newvals $value
                        }
                        pub::set_obj_attr $const [list values $newvals]
                        set values [pub::get_obj_attr $const values]
                    }
                }
            }
        }
    }
}

proc add_variants {lib} {
    foreach cell [pub::get_obj_list $lib type "cell"] {
        set name [pub::get_obj_name $cell]
        foreach v {2 3} {
            set new [pub::copy_obj $cell $lib]
            set i end-5
            pub::set_obj_name $new [string replace $name $i $i $v]
            puts "added $name [pub::get_obj_name $new]"
            foreach attr {cell_footprint intc_vt_swap_pattern} i {end-5 end-7} {
                set val [pub::get_obj_attr $new $attr MISSING]
                if {$val ne "MISSING"} {
                    pub::set_obj_attr $new [list $attr [string replace $val $i $i $v]]
                }
            }
        }
    }
}

proc update_operating_condition {lib} {
    set typ "typical_1.00"
    set opcs [pub::get_obj_list $lib type "operating_conditions"]
    set found 0
    set remove {}
    foreach opc $opcs {
        set name [pub::get_obj_name $opc]
        if {($found == 0) && ($name eq $typ)} {
            set found 1
        } else {
            lappend remove $opc
        }
    }
    if {$found == 1} {
        foreach opc $remove {
            pub::del_obj $opc
        }
    } else {
        set opc [lindex $remove 0]
        pub::set_obj_name $opc $typ
        pub::set_obj_attr $opc [list tree_type "balanced_tree"]
    }
    pub::set_obj_attr $lib [list default_operating_conditions $typ]
}

proc update_comment {lib} {
    set cpdk [file tail [file normalize $::env(INTEL_PDK)]]
    set upf [exec grep -m 1 process_upf_version $::env(PROJECT_HSP_FILE)]
    set hsp [file tail [file normalize $::env(PROJECT_HSP_FILE)]]
    set comment "Characterized by [lindex [get_version_info] 0] ; CPDK: $cpdk ; HSP_Version: [lindex $upf 3] ; HSP: $hsp ; CMI: libCMImodel.so"
    pub::set_obj_attr $lib [list comment $comment]
}

enable_api pub
namespace import pub::*
set lib [pub::read_model -liberty [lindex $argv 0]]
noncrater_mpw $lib
#add_variants $lib  # no variants in 1278
update_operating_condition $lib
update_comment $lib
set name [lindex $argv 1]
pub::set_obj_name $lib $name
write_model $lib $name.lib
