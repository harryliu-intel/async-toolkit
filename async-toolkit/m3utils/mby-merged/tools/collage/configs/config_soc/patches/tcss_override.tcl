proc ipi_generate_scan_clock_specs {} {
  #collage_obv_wrapper init 1

  print_info "Processing (ulti)scan clock controller file ${::ipi_integ_specs_dir}/integ_scan_clk_rst_scc.txt - START"
  collage_generate_scan_clock_specs -scan_spec_file ${::ipi_integ_specs_dir}/integ_scan_clk_rst_scc.txt -fscan_clk_pin pre_clocks -ascan_clk_pin post_clocks -sink_side -scan_ip_pattern  clst_ultiscan.* -out_fn tcss_scan_spec_file_debug.txt -out_debug_fn tcss_scc_errors.txt 
  print_info "Processing (ulti)scan clock controller file ${::ipi_integ_specs_dir}/integ_scan_clk_rst_scc.txt - END"

  #scc_pre_steps
  collage_process_conn_file -merge_conn -file tcss_scan_spec_file_debug.txt
  #scc_post_steps
}

