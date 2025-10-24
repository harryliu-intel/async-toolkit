set ::sh_output_log_file sv_dc_load.log

set this_dir [file dir [info script]]
source ${this_dir}/gen_fp_netlist_procs.tcl

catch {
  sv_dc_read_rtl_leaf

  current_design soc
  report_hierarchy -nosplit
  puts "# -------------------------------------------------"
  puts "# Number of nets: [sizeof_collection [get_nets]]"
  puts "# Number of pins: [sizeof_collection [get_pins]]"
  puts "# -------------------------------------------------"
}

exit
