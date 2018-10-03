
package require Itcl
source /p/com/eda/intel/collage/2.7.5/core/common/tcl/utils.tcl

catch { ::itcl::delete class sv_mcp_inst }

# collage_set_hier_spec -dont_create -file gen/conneci

proc collage_process_mcp_delay_spec {mcp_spec_fn gen_specs_dir} {

  sv_mcp_inst::init $gen_specs_dir

  array set pins_by_par {}
  array set pins_delay_mult {}

  sv_for_file l $mcp_spec_fn {
    set l [string trim $l]
    if {$l == ""} {continue}
    if {[regexp {^\#} $l]} { continue }

    set clk_name [set delay_mult ""]

    sv_lassign $l inst_name pin_name delay_mult clk_name
    
    set par_full [::col::get_ip_hier_par $inst_name]
    set par [lindex [split $par_full "/"] 0]

    if {![info exists pins_by_par($par)]} {
      set pins_by_par($par) ""
    }

    lappend pins_by_par($par) ${inst_name}/${pin_name}

    set pins_delay_mult(${inst_name}/${pin_name}) "$delay_mult $clk_name"
  }

  parray pins_by_par
  parray pins_delay_mult
  
  set i_fn "${gen_specs_dir}/mcp_inst_stage.tcl"
  set i_fh [open $i_fn w]

  set p_fn "${gen_specs_dir}/mcp_inst_par.txt"
  set p_fh [open $p_fn w]

  set c_fn  "${gen_specs_dir}/mcp_conn_stage.tcl"
  set c_fh [open $c_fn w]

  set fixp_fn  "${gen_specs_dir}/mcp_fix_params.txt"
  set fixp_fh [open $fixp_fn w]

  set s_fn  "${gen_specs_dir}/mcp_static_connections.txt"
  set s_fh  [open $s_fn w]

  

  foreach p [array names pins_by_par] {
    echo "Processing partition: $p"
    #instantiate_
    set inst_name mcp_$p
    
    puts $p_fh "$inst_name $p"
    puts $s_fh "C cci_top/cci_xxx_vnn_CRO100_VNN_1_clk $inst_name/clk "
    puts $s_fh "C pmc/pmc_vnnaon_pwrgood $inst_name/clk_resetb "

    puts $i_fh "eval_in_component $p \{"
    puts $i_fh "  collage_instantiate_component  par_mcp_block  -name $inst_name -noauto "
    sv_mcp_inst $inst_name $inst_name sig_in sig_out
      
    set da_params_val {}
    set idx 0
    foreach pin $pins_by_par($p) {
      sv_lassign [split $pins_delay_mult($pin)] pin_delay_mult pin_clk
      puts $c_fh "$inst_name add_conn $pin $pin_delay_mult $pin_clk"
      puts $i_fh "  set_configuration_parameter -component $inst_name DELAY_ARRAY($idx) $pin_delay_mult"
      if {$da_params_val == ""} {
	set da_params_val "$pin_delay_mult"
      } else {
	set da_params_val "$pin_delay_mult, $da_params_val"
      }
      incr idx
    }

    set num_pins [llength $pins_by_par($p)]
    puts $i_fh "  set_configuration_parameter -component $inst_name NUM_MCP_INPUTS $num_pins"
    
    puts $i_fh "\}"

    puts $c_fh ""

    puts $fixp_fh "$inst_name \{ $da_params_val \}"
  }

  puts $c_fh ""
  puts $c_fh "sv_mcp_inst::complete"

  close $i_fh
  close $c_fh
  close $p_fh

  close $fixp_fh
  close $s_fh

  #
  # sv_mcp_inst::complete
}

itcl::class sv_mcp_inst {
  public variable num_used;
  public variable rtl_inst_name;
  public variable rtl_pin_in;
  public variable rtl_pin_out;

  private common _fh;
  private common _gen_specs_dir  {}


  constructor {inst_name pin_in pin_out} {
    set num_used 0
    set rtl_inst_name $inst_name
    set rtl_pin_in $pin_in
    set rtl_pin_out $pin_out
  }

  proc init {d} {
    set _gen_specs_dir $d
    set _fn "${_gen_specs_dir}/mcp_adhoc_conn.txt"
    set _fh [open $_fn w]
  }

  proc complete {} {
    close $_fh
  }

  method add_conn {rcv_pin delay_mult clk} {
    set rcv_pin_par [::col::get_ip_hier_par [lindex [split $rcv_pin "/"] 0]]
    
    # get the driver
    eval_in_component $rcv_pin_par {
      set drv [find_item -quiet [get_connections -sort -hierarchy $rcv_pin] -filter "PortDirection==out && TypeName==pin"]
      if {[sizeof_collection $drv] != 1} {
	error "Number of drivers for pin $rcv_pin not equal to 1 : is [sizeof_collection $drv]"
      }
      set drv_name [get_attribute -attrs UserName $drv]
      set drv_inst [lindex [split $drv_name "/"] end-1]
      set drv_pin [lindex [split $drv_name "/"] end]
      set drv_name_leaf ${drv_inst}/${drv_pin}

      echo "-D- Driver: $drv_name"
      
      #set cmd1 "D ${rcv_pin_par}/$rcv_pin"; echo $cmd1
      set cmd1 "D $rcv_pin"; echo $cmd1
      puts $_fh $cmd1
      set cmd2 "C ${drv_name} ${rtl_inst_name}/${rtl_pin_in}\[${num_used}\] "; echo $cmd2
      puts $_fh $cmd2
      set cmd2 "C ${rtl_inst_name}/${rtl_pin_out}\[${num_used}\] ${rcv_pin} "; echo $cmd2
      puts $_fh $cmd2

      incr num_used
    }


  }

  method process_conn {} {
    collage_process_conn_file -hier_lookup -merge_conn -file $_fn
  }

  method print {} {
  }

  proc tempfile {{filenameVar {}}} {
    set chars abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789
    if {$filenameVar ne {}} {
      upvar 1 $filenameVar filename
    }
    for {set i 0} {$i < 10} {incr i} {
      set filename /tmp/tcl_
      for {set j 0} {$j < 10} {incr j} {
	append filename [string index $chars [expr {int(rand() * 62)}]]
      }
      if {![catch {open $filename {RDWR CREAT EXCL} 0600} channel]} {
	return $channel
      }
    }
    error "failed to find an unused temporary file name"
  }
}

# instantiate the module in each partition 
# Reconnec 
