set this_dir [file dir [info script]]

#======================#
# START OF CATCH BLOCK #
#======================#
if { [catch {
#======================#

source ${this_dir}/open_chassis_ws.tcl
source ${this_dir}/assembler.soc_tb.tcl

################################
# exit unless debugging
################################
if {[array names ::env "CHASSIS_DEBUG_MODE"] eq ""} {
    exit 0
}

#####################################################
#####################################################
#====================#
# END OF CATCH BLOCK #
#====================#
} ] } {
  puts stderr "\n\nERROR (Code = ${::errorCode}):\n${::errorInfo}\n"
  if {![info exists ::env(CHASSIS_DEBUG_MODE)] } {
    exit 1
  }
}
