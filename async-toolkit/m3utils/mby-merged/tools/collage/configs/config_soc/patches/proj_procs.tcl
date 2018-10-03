set curr_dir [file dirname [info script]] 
proc soc_obv_hook_do_create_clock_latency { hier_name clk } {
  return 1
}
proc soc_obv_hook_do_generated_clock_latency { hier_name clk immediate_parent } {
  return 1
}
proc soc_obv_hook_do_add_proj_header {hier_name} {
    
    if {[info proc rdt_source_if_exists] != ""} {
        rdt_source_if_exists [concat $hier_name\_clock_params.tcl]
    } else {
        #sourcing clock_procs is needed if not in RDT flow because RDT automatically sources this file
        set file_path_clk_proc [file join $::curr_dir [concat clock_procs.tcl]]
        puts "Sourcing $file_path_clk_proc"
        uplevel 1 "source $file_path_clk_proc" 
        puts "Finished Sourcing $file_path_clk_proc"

        set file_path_param [file join $::curr_dir [concat $hier_name\_clock_params.tcl]]
        puts "Sourcing $file_path_param"
        uplevel 1 "source $file_path_param" 
        puts "Finished Sourcing $file_path_param"
    } 
    return 1
}
