set ndm_name [file normalize $env(NDM)]
set ldb_names $env(LDB)

set ldb [open_lib $ndm_name]
set char_points [get_attribute $ldb characterization_points]
close_lib $ldb

set bundle_dir [file dirname [file dirname $ndm_name]]
create_workspace -flow normal [file tail $ndm_name] -scale_factor 4000
set_app_options -as_user_default -name lib.workspace.save_design_views -value true
set_app_options -as_user_default -name lib.workspace.save_layout_views -value true
set_app_options -name lib.workspace.keep_all_physical_cells -value true
set_app_options -name lib.logic_model.keep_pin_direction_same_as_db -value true

read_ndm $ndm_name
foreach pt $char_points {
    set map [dict create]
    foreach kv $pt {
        dict set map {*}$kv
    }
    read_db "$bundle_dir/lib/[dict get $map source_lib].ldb" -process_label [dict get $map process_label]
}
foreach ldb_name $ldb_names {
    set ldb_name [file normalize $ldb_name]
    set tags [split [file tail $ldb_name] _]
    read_db $ldb_name -process_label [join [lmap i {0 6 9 10} {lindex $tags $i}] _]
}
check_workspace -allow_missing
commit_workspace
exit
