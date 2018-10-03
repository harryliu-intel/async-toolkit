proc collage_emu_add_include_files {design} {
  foreach par [collage_get_par_names] {
    collage_eval_in_component -edit_mi -use_hier_name $par {
    echo "par : $par : [sizeof_collection [find_item -quiet -type cell]]"

      if {[sizeof_collection [find_item -quiet -type cell]] > 0} {
	set_design_attribute {CustomizedTestbenchCode[Verilog_Additional_Wire]} \
	    "[get_design_attribute {CustomizedTestbenchCode[Verilog_Additional_Wire]}]\n `ifdef USE_SLA_RTL_TLM \n `include \"sla_macros.svh\"\n `include \"emu_[collage_get_folded_name -name ${par}].sv\" \n\`endif \n"
      } else {
	print_warning "Emulation auto include: skip partition with no cells : $par"
      }
    }
  }
  set_design_attribute {CustomizedTestbenchCode[Verilog_Additional_Wire]} \
        "[get_design_attribute {CustomizedTestbenchCode[Verilog_Additional_Wire]}]\n `ifdef USE_SLA_RTL_TLM \n `include \"sla_macros.svh\"\n \`endif \n"
}

#Nathan fixme removed from top design attribute
#fixme removed from 2nd design attribute
#`include \"emu_${design}.sv\" \n
