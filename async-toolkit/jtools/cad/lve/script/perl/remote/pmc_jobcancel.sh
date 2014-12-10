#!/bin/sh
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# COPYRIGHT (C) 2004 BY PMC-SIERRA, INC.
# ALL RIGHTS RESERVED
#
# Name: pmc_rr_status_update
#
#
# Author: Al Keddy
# Date:   2004-04-08
#
# $Author: deshmane $
# $Revision: 1.2 $
# $Date: 2004/05/04 23:37:50 $
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

## REMOVED NO LOG MESSAGES SECTION

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# The next line restarts using stcl \
exec /usr/local/syncinc/bin/stclc -- $0 "$@"
#===================================================================

#-------------------------------------------------------------------
# exit if SYNC_DIR not set
#-------------------------------------------------------------------
if [catch {set SYNC_DIR $env(SYNC_DIR)} msg] {
  puts stderr "ERROR: SYNC_DIR not set\n"
  exit 1
}

#-------------------------------------------------------------------
# global variables
#-------------------------------------------------------------------
set script RemoteRun_Srv.tcl

#-------------------------------------------------------------------
# option variables
#-------------------------------------------------------------------
set serverport "sync://fulcrum-gs:7000"
set rr_note_id ""
set message ""

#-------------------------------------------------------------------
# Script help message
#-------------------------------------------------------------------
proc help {} {
puts stderr {

Name:  pmc_rr_cancel

Synopsis:
  Cancel the specified note.
  Update the Status comment with the message.

}
  usage
}

#-------------------------------------------------------------------
# Script usage message
#-------------------------------------------------------------------
proc usage {} {
  global argv0

puts stderr {
 Usage:  pmc_rr_cancel -id rr_note_id
  -id rr_note_id - rr_note_id directory path
  -m   message   - status update message
  -h             - help

}
  exit 1
}

#-------------------------------------------------------------------
# Process the arguments.
#-------------------------------------------------------------------
set length [llength $argv]
set i 0
while {$i < $length} {

  set val [lindex $argv $i]

  switch -exact -- $val {
   -id {
        incr i
        set val [lindex $argv $i]
        set rr_note_id $val
      }
   -m {
        incr i
        set message [lindex $argv $i]
      }
   -h { help }
    default {
      puts stderr "Unknown option: $val"
      usage
    }
  }
  incr i
}

#===================================================================
# local procs
#===================================================================
#-------------------------------------------------------------------
# decodeUrl - decode URL string
#-------------------------------------------------------------------
proc decodeUrl {str} {
  regsub -all {\+} $str { } str
  regsub -all {%([0-9a-hA-H][0-9a-hA-H])} $str {[format %c 0x\1]} str

  return [subst $str]
}

#-------------------------------------------------------------------
# remote_request - serverside execution request
#-------------------------------------------------------------------
proc remote_request {cmd {parms ""}} {

  set urlparams "cmd=$cmd"
  if { $parms != "" } {
    append urlparams "&$parms"
  }

  set ret [rstcl -server $::serverport -script $::script -urlparams $urlparams]

  set dret [decodeUrl $ret]

  array set ret_array $dret

  if {$ret_array(status) == 0} {
    return $ret_array(msg)
  } else {
    puts stderr "Error: server=side execution failed"
    puts stderr "     : server $::serverport"
    puts stderr "     : server script $::script\n"
    puts stderr " err : $ret_array(msg)"
    exit $ret_array(status)
  }
}

#===================================================================
# check required variables are defined
#===================================================================
#-------------------------------------------------------------------
# check serverport
#-------------------------------------------------------------------
if {$serverport == ""} {
  puts stderr "server not set\n"
  usage
}

set urlparms "Id=$rr_note_id"
set rr_info_alist [remote_request NoteInfo $urlparms ]
array set rr_info $rr_info_alist

puts "Id         => $rr_info(Id)"
puts "Mode       => $rr_info(Mode)"
puts "SrcData:"
array set src_data $rr_info(SrcData)
foreach data [lsort [array names src_data]]  {
  puts [format "% 10.10s => %s" $data $src_data($data)]
}
puts "VaultURL   => $rr_info(VaultURL)"

#===================================================================
set urlparms "Id=$rr_note_id&State=cancelled"
if {$message != ""} {
  append urlparms "&Status=$message"
}

remote_request RunStatus $urlparms

exit 0
#===================================================================
