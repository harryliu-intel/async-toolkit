
# Loading of local clock stamping package should be removed when using Collage version later than 3.14
set clk_CVAL_code_file "[file dir [info script]]/clk_msg_list.tcl"
if {[file exists ${clk_CVAL_code_file}]} {
  source ${clk_CVAL_code_file}
}
set clk_stamping_pkg_dir "[file dir [info script]]/obviate"
if {[file isdirectory ${clk_stamping_pkg_dir}]} {
  set ::auto_path "${clk_stamping_pkg_dir} $::auto_path"
  package forget obviate
  package require obviate
}

