# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0

set ::sh_output_log_file sv_dc_load.log

set this_dir [file dir [info script]]
source ${this_dir}/gen_fp_netlist_procs.tcl

if { [catch {

  sv_dc_read_rtl_unit

  current_design soc

  puts "# -------------------------------------------------"
  puts "# Number of nets: [sizeof_collection [get_nets]]"
  puts "# Number of pins: [sizeof_collection [get_pins]]"
  puts "# -------------------------------------------------"

} ] } {
  puts stderr "\n\nERROR (Code = ${::errorCode}):\n${::errorInfo}\n"
}
exit
