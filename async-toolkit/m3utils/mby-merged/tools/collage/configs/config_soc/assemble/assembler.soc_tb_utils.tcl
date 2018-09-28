# ============================================
# Additional `includes to be added here
# ============================================

proc soc_tb_pre_hook_generate_subsystem_rtl {} {
  set_design_attribute {CustomizedTestbenchCode[Verilog_Additional_Header]} \
    {
// *************************************************************************
// - DO NOT hand edit
// *************************************************************************
    }

  #import SV packages
  set_design_attribute {CustomizedTestbenchCode[Verilog_Additional_Wire]} \
      "[get_design_attribute {CustomizedTestbenchCode[Verilog_Additional_Wire]}]\n   `define [string toupper ${::design}]_TOP ${::design}\n"
  set_design_attribute {CustomizedTestbenchCode[Verilog_Additional_Wire]} \
      "[get_design_attribute {CustomizedTestbenchCode[Verilog_Additional_Wire]}]\n   `define [string toupper ${::design}]_TB  ${::design}_tb\n"
  #set_design_attribute {CustomizedTestbenchCode[Verilog_Additional_Wire]} \
  #    "[get_design_attribute {CustomizedTestbenchCode[Verilog_Additional_Wire]}]\n   `include \"${::design}_ip_hier_defines.sv\"\n"
  set_design_attribute {CustomizedTestbenchCode[Verilog_Additional_Wire]} \
      "[get_design_attribute {CustomizedTestbenchCode[Verilog_Additional_Wire]}]\n   `include \"${::design}_tb_initialize.svh\"\n"

  #adhoc interfaces    
  set_design_attribute {CustomizedTestbenchCode[Verilog_Additional_Wire]} \
      "[get_design_attribute {CustomizedTestbenchCode[Verilog_Additional_Wire]}]\n   `include \"${::design}_gen_intf_include.svh\"\n"

  #after test island
  set_design_attribute {CustomizedTestbenchCode[Verilog_Additional_Post_Instance] } \
      "[get_design_attribute {CustomizedTestbenchCode[Verilog_Additional_Post_Instance]}]\n   `include \"${::design}_dfx_def.svh\"\n"
  set_design_attribute {CustomizedTestbenchCode[Verilog_Additional_Post_Instance] } \
      "[get_design_attribute {CustomizedTestbenchCode[Verilog_Additional_Post_Instance]}]\n   `include \"soc_ace_pli_include.sv\"\n"
  set_design_attribute {CustomizedTestbenchCode[Verilog_Additional_Footer]} \
    {
    }
}

proc soc_tb_hook_init {} {
}


proc collage_post_process_tb {} {
  #collage_remove_ifdef $::env(COLLAGE_WORK)/${design}_tb/src/${design}_tb.sv SPI
  #collage_add_ifdef_supply_ground -folder_path $::env(COLLAGE_WORK)/${design}_tb/src/
  #file copy -force "$::env(COLLAGE_WORK)/gen/tb_spec/${design}_ip_hier_defines.svh" "$::env(IP_ROOT)/verif/tb/includes"
}

#automatically tie off design ports
proc auto_wdu_tb_conn {} {
  set design_name "${design}"
  set f_name "$::env(COLLAGE_WORK)/gen/tb_specs/auto_wdu_tb_conn.txt"
  set fh [open $f_name w]

  #default hip naming convention
  foreach_in_collection p [find_item -quiet -type port *_ircomp*] {
    set pin [get_attribute $p -attr Name]
    puts $fh "Wdu '0 $design_name $pin"
  }

  #SOC pad naming convention
  foreach_in_collection p [find_item -quiet -type port *_IRCOMP] {
    set pin [get_attribute $p -attr Name]
    puts $fh "Wdu '0 $design_name $pin"
  }
  
  foreach_in_collection p [find_item -quiet -type port *_idv*i] {
    set pin [get_attribute $p -attr Name]
    puts $fh "Wdu '0 $design_name $pin"
  }
  
  foreach_in_collection p [find_item -quiet -type port vss*] {
    set pin [get_attribute $p -attr Name]
    puts $fh "Wdu '1 $design_name $pin"
  }

  foreach_in_collection p [find_item -type port vcc*] {
    set pin [get_attribute $p -attr Name]
    puts $fh "Wdu '1 $design_name $pin"
  }

  foreach_in_collection p [find_item -type port *_jta_*] {
    set pin [get_attribute $p -attr Name]
    puts $fh "Wdu '0 $design_name $pin"
  }

  foreach_in_collection p [find_item -type port *fscan_*] {
    set pin [get_attribute $p -attr Name]
    if { [regexp {_(b|B)$} $pin] } {
      puts $fh "Wdu '1 $design_name $pin"
    } else {
      puts $fh "Wdu '0 $design_name $pin"
    }
  }

  close $fh
}


proc collage_remove_ifdef { file prefix} {
  file copy -force $file "$file.org"
  set fp [open "$file.org"]; set o_fp [open $file w+]
  set prev_lines ""

  set found_ti 0;
  set interface_begin 0;
  set interface_end 0;
  while {-1 != [gets $fp line]} {
    set if_def 0; set endif 0; 
    set sline [string trim $line]

    if {[regexp "^`ifdef ${prefix}_TI.+ENABLE$" $sline] } {
        set if_def 1
        set interface_begin 1
    }
    if {[regexp {^`else$} $sline] } {
        set interface_end 1
    }
    if {[regexp "^`ifdef ${prefix}_ENABLE$" $sline] } {
        set if_def 1
        set found_ti 1
    }
    if {[regexp {^`endif$} $sline]} {
        set endif 1
    }
    if { !$if_def && !$endif && !$interface_begin} {
      #puts $line
      append prev_lines "$line\n"
    }
    if {$interface_begin && $interface_end} {
      set interface_begin 0
      set interface_end 0
    }
  }

  if {[string length $prev_lines]} {
    puts $o_fp $prev_lines
  }
  
  close $fp; close $o_fp
  return;
}

