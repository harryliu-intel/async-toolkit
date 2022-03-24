#!/usr/intel/pkgs/tcl-tk/8.6.8/bin/tclsh

# Generate Netbatch Feeder File to produce a collection of LAMBs
# Validate these lambs with SHIP

proc produceLambs { lambList {taskname lambgen } {tag testtag} { archive 0 } } {
   global cheetahProject
   global cheetahConfig
   global nbqueue
   global qslot
   set tf [open ${taskname}.conf w]
   # https://hsdes.intel.com/appstore/article/#/14016296198
   # Seeks to make this insensitive to FC tool version
   set toolset librarycompiler,fusioncompiler,ship

   puts "Producing [llength $lambList] LAMBs primitives"

   puts $tf "CompositeTask ${taskname} {"
   puts $tf " WorkArea [pwd]/BUILD/${taskname}"
   puts $tf " SubmissionArgs --class SLES12"
   puts $tf " Queue ${nbqueue} { Qslot $qslot }"
   foreach l $lambList {
      set width   [dict get $l width]
      set depth   [dict get $l depth]
      set type    [dict get $l type]
      set variant [dict get $l variant]

      set cthSetup "/p/cth/bin/cth_psetup -p ${cheetahProject} -cfg ${cheetahConfig}.cth"
      set ward cdp_lamb_${variant}_${type}_${depth}d_${width}b
      set lm_shell_cmd "icc2_lm_shell -batch -x 'source $::env(GTR_HOME)/tcl/gtr_main.tcl ; gtr_lamb_gen_views -variant_type $variant -data_width $width -data_depth $depth'" 
      set cmd "     $cthSetup -tool $toolset -ward $ward -cmd \"$lm_shell_cmd\""

      puts $tf " JobsTask  build_$ward {"
      puts $tf "   jobs { "
      puts $tf $cmd
      puts $tf "   }"
      puts $tf " }"
      puts $tf " JobsTask  ship_$ward {"
      puts $tf "   DependsOn build_${ward}\[OnSuccess\]"
      puts $tf "   jobs { "
      set shipcmd "ship.pl -skip_prompt -block $ward -tag $tag -ip_type hip -source ../$ward"
      if { $archive == 0 } {
         append shipcmd " -skip_stages archive"
      }
      puts $tf "     $cthSetup -tool $toolset -ward ship_$ward -cmd \"$shipcmd"
      puts $tf "   }"
      puts $tf " }"

   }
   puts $tf "}"
   close $tf
   puts "Taskfile prepared. Please review it! To execute it, eg: "
   puts "  nbfeeder start --join --task ${taskname}.conf "
   puts "Once running, to see progress: https://nbflow.intel.com"
}

# Parse a list of lamb files formatted as:
# type1 width1 depth1
# type2 width2 depth2
# ...
# typeN widthN depthN
#
# Or allow a line entry to be the same of supported LAMB

proc parseLambfile { lf } {
   set if [open "|sort -u $lf" r+]
   set lamblist [list]
   while {[gets $if line]>=0} {   
      set l [dict create]
      if { [regexp cdp_lamb_(.*)_(.*)_(\[0-9\]+)d_(\[0-9\]+)b $line junk variant type depth width]} {
      } else {
         set vals [split $line]
         set type [lindex $vals 0]
         set width [lindex $vals 1]
         set depth [lindex $vals 2]
         set variant n3bhd
      }
      dict set l variant $variant
      dict set l type $type
      dict set l width $width
      dict set l depth $depth
      lappend lamblist $l
   }
   return $lamblist
}



proc lambRange { typelist minWidth minDepth maxWidth maxDepth } {
  set lamblist [list]
  foreach t { 1r1w1c } {
    for { set depth [expr max($minDepth,1)] } { $depth <= $maxDepth} {incr depth +1} {
      for { set width [expr max($minWidth,1)] } { $width <= $maxWidth} {incr width +1} {
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
set cheetahConfig tfc_ipde_n3

package require cmdline
set options [list \
   {lf.arg  "" "build lambs from given file list"} \
   {mindepth.arg  0  "use specified minimum depth"} \
   {maxdepth.arg  0  "use specified minimum depth"} \
   {minwidth.arg  0  "use specified minimum depth"} \
   {maxwidth.arg  0  "use specified minimum depth"} \
   {qslot.arg  /bfn/fe  "use specified qslot"} \
   [list nbqueue.arg   $::env(EC_SITE)_normal  "use specified NB queue"] \
   {archive  0 "archive SHIP results to tag based on git configuration"} \
]
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

set nbqueue $params(nbqueue)
set qslot  $params(qslot)


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
produceLambs $lamblist $taskname [exec git describe] $params(archive)
