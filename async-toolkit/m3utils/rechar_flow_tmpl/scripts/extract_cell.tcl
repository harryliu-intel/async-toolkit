enable_api pub
namespace import pub::*

proc filter_group {handle path} {
    set this [lindex $path 0]
    set rest [lrange $path 1 end]
    foreach child [pub::get_obj_list $handle type $this] {
        if {$rest=={}} {
            pub::del_obj $child
        } else {
            filter_group $child $rest
        }
    }
}

set filters {{receiver_capacitance}
             {timing receiver_capacitance_fall}
             {timing receiver_capacitance_rise}
             {timing ocv_sigma_cell_fall}
             {timing ocv_sigma_cell_rise}
             {timing ocv_sigma_fall_constraint}
             {timing ocv_sigma_fall_transition}
             {timing ocv_sigma_rise_constraint}
             {timing ocv_sigma_rise_transition}}

if {[catch {open [lindex $argv 2]} fh]} {
    error "Can't open filelist"
} else {
    set keeplist {}
    while {[gets $fh line] >= 0} {
        lappend keeplist $line
    }
    close $fh
    set lib [pub::read_model -liberty [lindex $argv 0]]
    foreach cell [pub::get_obj_list $lib type "cell"] {
        set name [pub::get_obj_name $cell]
        set keep 0
        foreach pat $keeplist {
            if {[string match $pat $name]} {
                set keep 1
                break
            }
        }
        if {!$keep} {
            pub::del_obj $cell
        }
    }
    pub::write_model $lib "[lindex $argv 1].lvf"
    foreach cell [pub::get_obj_list $lib type "cell"] {
        foreach filter $filters {
            filter_group $cell [concat pin $filter]
            filter_group $cell [concat bundle pin $filter]
        }
    }
    pub::write_model $lib "[lindex $argv 1]"
}
