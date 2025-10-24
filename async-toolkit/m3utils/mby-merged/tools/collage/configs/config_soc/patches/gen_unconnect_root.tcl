set generated_clocks [list]
set root_clocks [list]
foreach clk [join [::obv_soc_clock::get_soc_clock_lists]] {
  if {[llength [$clk cget -post_reset_root_clocks]]} {
    lappend generated_clocks $clk
  } else {
    lappend root_clocks $clk
  }
}
set problem_clock_lists [list]
foreach gen_clk [lreverse $generated_clocks] {
  set clk_src [join [$gen_clk cget -source_pin]]
  set clk_parents [$gen_clk get_parents]
  set clk_parents_reverse [lreverse $clk_parents]
  
  set child_clk $gen_clk
  
  foreach parent $clk_parents_reverse {
    if {![llength $clk_src]} {
      continue
    }
    set parent_src [$parent cget -source_pin]
    if {![llength $parent_src]} {
      puts "Child clock: $child_clk Parent: $parent (No source)"
      lappend problem_clock_lists [list $child_clk $parent]
      
      break;
    }
    set child_clk $parent
  }
}
set problem_clock_lists [lsort -unique $problem_clock_lists]

set conn_issues [list]
foreach gen_clk [lreverse $generated_clocks] {
  
  set child_clk $gen_clk
  
  set clk_src [join [$child_clk cget -source_pin]]
  set sp_clk_src [split $clk_src /]
  set clk_src_ip [lindex $sp_clk_src 0]
    
  set clk_parents [$gen_clk get_parents]
  set clk_parents_reverse [lreverse $clk_parents]
  
  
  
  foreach parent $clk_parents_reverse {
    if {![llength $clk_src]} {
      continue
    }
    set pass 0
    set parent_src [$parent get_all_ip_clock_pins]
    if {[llength $parent_src]} {
      foreach {ip_clk hier_clk dir} [join $parent_src] {
         set sp_ip_clk [split $ip_clk /]
	 set ip_clk_ip [lindex $sp_ip_clk 0]
	 if {[string equal ${ip_clk_ip} ${clk_src_ip}]} {
	   set pass 1; break
	 }
      }
      if {!$pass} {
        lappend conn_issues [list $child_clk $parent]
        #puts "Child clock: $child_clk Parent clock: $parent (Un connected)"
      }
    }
    set child_clk $parent
    set clk_src [join [$child_clk cget -source_pin]]
  
    set sp_clk_src [split $clk_src /]
    set clk_src_ip [lindex $sp_clk_src 0]
  }
}
set conn_issues [lsort -index 1 [lsort -unique ${conn_issues}]]

foreach {child_clk parent} [join $conn_issues] {
  puts "Child clock: $child_clk Parent clock: $parent (Un connected)"
}
1;
