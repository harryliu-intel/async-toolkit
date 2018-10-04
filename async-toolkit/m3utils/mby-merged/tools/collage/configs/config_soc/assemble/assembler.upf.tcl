if { [catch {
global env

set this_dir [file dir [info script]]
source ${this_dir}/open_chassis_ws.tcl

# --- Load the UPF configuration file
::upf::load_config ${soc_integ_specs_dir}/soc_upf_config.txt ${soc_integ_specs_dir}/soc_upf_clamp.txt 1

# --- Generate the UPF files
::upf::generate $::design
collage_batch_exit

} ] } {
   puts stderr "\n\nERROR (Code = ${::errorCode}):\n${::errorInfo}\n"
   collage_batch_exit
   exit 1
}
