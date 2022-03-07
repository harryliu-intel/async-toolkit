#!/usr/intel/pkgs/tcl-tk/8.6.8/bin/tclsh

# Generate Netbatch Feeder File to produce a collection of LAMBs
proc produceLambs { lambList {taskname lambgen } } {
   global cheetahProject
   global cheetahConfig
   global nbqueue
   global qslot
   set tf [open ${taskname}.conf w]
   set toolset librarycompiler,fusioncompiler,cth_LR
   
   puts $tf "Task ${taskname} {"
   puts $tf " WorkArea [pwd]/BUILD"
   puts $tf " Queue ${nbqueue} { Qslot $qslot }"
   puts $tf " Jobs {"
   foreach l $lambList {
      set width [dict get $l width]
      set depth [dict get $l depth]
      set type  [dict get $l type]
      ## would be better to refactor naming convention to common place for
      ## batching and production cases
      set ward cdp_lamb_n3bhd_${type}_${depth}d_${width}b
      set lm_shell_cmd "icc2_lm_shell -batch -x 'source $::env(GTR_HOME)/tcl/gtr_main.tcl ; gtr_lamb_gen_views -data_width $width -data_depth $depth'" 
      set cmd "    /p/cth/bin/cth_psetup -p ${cheetahProject} -cfg ${cheetahConfig}.cth -tool $toolset -ward $ward -cmd \"$lm_shell_cmd\""
      puts $tf $cmd
   }
   puts $tf " }"
   puts $tf "}"
   close $tf
   puts "Taskfile prepared. To execute it, eg: "
   puts "  nbfeeder start --join --task ${taskname}.conf "
   puts "Once running, to see progress: https://nbflow.intel.com"
}

proc parseLambfile { lf } {
   set if [open $lf r]
   set lamblist [list]
   while {[gets $if line]>=0} {   
      set vals [split $line]
      set l [dict create]
      dict set l type [lindex $vals 0]
      dict set l width [lindex $vals 1]
      dict set l depth [lindex $vals 2]
      lappend lamblist $l
   }
   return $lamblist
}



proc lambRange { typelist minWidth minDepth maxWidth maxDepth } {
  set lamblist [list]
  foreach t { 1r1w1c } {
    for { set depth $minDepth } { $depth <= $maxDepth} {incr depth +1} {
      for { set width $minWidth } { $width <= $maxWidth} {incr width +1} {
         set l [dict create]
         dict set l width $width
         dict set l depth $depth
         dict set l type $t
         lappend lamblist $l
      }
    }
  }
  return $lamblist
}

set cheetahProject tfc
set cheetahConfig tfc_n5
set nbqueue $::env(EC_SITE)_normal
set qslot /bfn/fe

package require cmdline
set options {
   {lf.arg  "" "build lambs from given file list"}
   {mindepth.arg  4  "use specified minimum depth"}
   {maxdepth.arg  4  "use specified minimum depth"}
   {minwidth.arg  4  "use specified minimum depth"}
   {maxwidth.arg  4  "use specified minimum depth"}

}
set usage ": batchLambs.tcl \[options]\noptions:"

try {
   array set params [::cmdline::getoptions argv $options $usage]
   # Note: argv is modified now. The recognized options are
	# removed from it, leaving the non-option arguments behind.
   } trap {CMDLINE USAGE} {msg o} {
   # Trap the usage signal, print the message, and exit the application.
   # Note: Other errors are not caught and passed through to higher levels!
	puts $msg
	exit 1
}

if {  [string length $params(lf)] > 0 } {
   set lf $params(lf)
   puts "Producing lambs specified by $lf"
   set lamblist [parseLambfile $lf]
} else {
  puts "Producing lambs specified by range"
  set lamblist [lambRange { 1r1w1c } $params(mindepth) $params(minwidth) $params(maxdepth) $params(maxwidth)]
}

set wwdate [exec workweek -f %IYWW%IW.%w_%H.%M ]
set taskname lambgen_[exec git describe]_$::env(USER)_${wwdate}
produceLambs $lamblist $taskname
