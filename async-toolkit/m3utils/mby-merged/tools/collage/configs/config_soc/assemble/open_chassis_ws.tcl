set this_dir [file dir [info script]]
source ${this_dir}/configuration.tcl
source ${this_dir}/assembler.soc_init.tcl

# Read kb and open workspace
read_kb $::env(COLLAGE_ROOT)/comps/rtl_integ/templates/gplugins/GClocks.kb
open_workspace soc

# Process instance alias
collage_process_instance_define_file -file ${::soc_integ_specs_dir}/${design}_inst_alias.txt

# Generate and populate hierarchy spec
collage_write_par_file -par_fn ${design}_full_par.txt
collage_set_hier_spec -dont_create -file ${design}_full_par.txt
