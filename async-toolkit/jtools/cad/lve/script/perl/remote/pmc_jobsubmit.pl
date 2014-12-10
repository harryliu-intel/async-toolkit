#!/usr/intel/bin/perl
# $Id$
# $Log: pmc_jobsubmit.pl,v $
# Revision 1.12  2004/11/02 19:28:03  deshmane
# added rcc support
#
# Revision 1.11  2004/10/27 00:48:10  deshmane
# added antenna support
#
# Revision 1.10  2004/10/21 21:28:03  deshmane
# *** empty log message ***
#
# Revision 1.9  2004/08/16 21:22:57  deshmane
# fixed a bug that the hier optionwas nt available to the drc mode
#
# Revision 1.8  2004/08/05 00:01:13  deshmane
# commented out 2 print statements
#
# Revision 1.7  2004/08/04 23:59:05  deshmane
# silenced a print statement
#
# Revision 1.6  2004/07/21 01:10:07  deshmane
# added option -portmapfile for specifying different nmaes between layout and netlist
# and mapping them
#
# Revision 1.5  2004/06/24 23:18:09  deshmane
# added silicondrc
#
# Revision 1.3  2004/05/11 16:26:33  deshmane
# commented out registry key file settings, since it was clobbering
# hte user/passwd session info
#
# 04/07/2004: Harsh Deshmane
# Fulcrum client-side script to submit calibre jobs to LSF queue
# version 1.3

use strict;
use Getopt::Long;
use FileHandle;
use File::Basename;

my($serverport) = "sync://fulcrum-gs:7000";
#my($serverport) = "sync://remote-run-test:8012";
my($optCtl) = {};
my($optRes);

 Getopt::Long::config("ignore_case");
 Getopt::Long::config("auto_abbrev");
$optRes = GetOptions($optCtl,
		     "help!",
		     "mode:s", # currently support drc/lvs/rce/ant
		     "gds:s", # gds file to run calibre on
		     "netlist:s",# netlist file for lvs/rce
		     "workdir:s", # cannot be "/tmp"..because of sync. errors
		     "top:s", # top cell name for gds file
		     "nettop:s",# top cell name for netlist file
		     "hier!", # run hierarchical calibre option
		     "query!", # optional boolean argument to check jobstatus
		               # -not supported yet
		     "jobid:i", # takes an optional integer id. for query flag
		     "populate:i", # after job completion, get results back
		                   # requires -workdir option to be set
		     "rcemode:s", # one of rc/cc/conly
                     "portmapfile:s", # port names mapping file frm layout/src             
		     "cancel!",
		     );

# help option
my($help) = $optCtl->{"help"};
if ($help) {
  &echoUsage();
  exit(0);
}


#must specify workdir - it is required for all operations
my($workdir) = $optCtl->{"workdir"};
unless ($workdir) {
  &echoUsage();  
  die "Error: Must specify workdir with -workdir flag.\n";
 }

# for -populate flag, populate the id specified
my($popid) = $optCtl->{"populate"};
if ( $popid) {
  my($popdir) =  $workdir."/".$popid."/submit";
  chdir $popdir;
  my($popcmd) = "dssc pop -rec";
  my($popfd) = new FileHandle("|$popcmd 1> pop.log.$$ 2>&1");
  unless ($popfd->close()){
    die "Cannot populate workspace $popdir";
  }
  # if populated, then job done.
  exit(0);
}
  
# for job submissions, -mode is required
my($mode) = $optCtl->{"mode"};
unless ($mode) {
  printf "Error: Must specify at least 1 mode for submission.\n";
  &echoUsage();
  die;
}

if ($mode !~ /drc/ && $mode !~ /lvs/ && $mode !~ /rce/ && $mode !~ /siliconlvs/ && $mode !~ /silicondrc/ && $mode !~ /ant/){
  printf "Error: Must specify -mode as one of: drc, lvs, rce, ant";
  &echoUsage();
  die;
}

my($rcemode) = $optCtl->{"rcemode"};
if ( $rcemode) {
  unless ($rcemode =~ /cc/ || $rcemode =~ /conly/ || $rcemode =~ /^rc$/ ||$rcemode =~ /rcc/) {
    &echoUsage();
    die "Error: When specifying -rcemode use one of rc/cc/rcc/conly\n";
  }
}

# gds always needed for job submission
my($gds) = $optCtl->{"gds"};
unless ($gds) {
  &echoUsage(); 
  die "Error: Must specify gds file.\n";
}
# check if gds is present

unless (-f $gds) {
  &echoUsage();
  die "Error: Cannot find specified GDS file.\n";
}
# need to specify top cell in the gds2 file
my($gdstop) = $optCtl->{"top"};
unless ($gdstop) {
  &echoUsage();
  die "Error: Must specify top cell. \n";
}
# derived from gdstop, since it might specify an entire path.
my($top) = basename($gdstop);


# for lvs and rce, need netlist file as well
my($hierorflat);
my($portmapflag) = 0;
my($portmapfile);
my($netlist) = $optCtl->{"netlist"};
my($nettop) = $optCtl->{"nettop"};
if ( $mode =~ /lvs/ || $mode =~ /rce/ || $mode =~ /siliconlvs/) {
    unless ($netlist) {
      &echoUsage();
    die "Error: Must specify netlist file for lvs or rce mode, 
         using -netlist option.\n";
    }
    # once netlist is specified, need to make sure it's top cell is
    # specified too.
    unless ($nettop) {
      &echoUsage();
      die "Error: Must specify top cell for netlist using -nettop option\n";
    }
    # find out if ( portmapfile is specified, if it is - then change
    # the mode to be _p
    
    $portmapfile = $optCtl->{"portmapfile"};
    if (-f $portmapfile && ! -z $portmapfile) {
      $mode = $mode."_p";
      $portmapflag = 1;
    }
}
    
# specify if hierarchical run or not.
    
    my($hier) = $optCtl->{"hier"};
    
    if ( $hier) {
      $hierorflat = "hier";
    }
    else {
      $hierorflat = "flat";
    }
    



########################################################
# following is the command set to send to stclc, written
# by Al Keddy. Modified to feed options directly
##########################################################

my($submitfile) = '
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

';
  
$submitfile = "set serverport ".$serverport."\n";

$submitfile = $submitfile."set work_dir ".$workdir."\n";
$submitfile = $submitfile."set mode ".$mode."\n";
$submitfile = $submitfile."set top ".$gdstop."\n";
$submitfile = $submitfile."set gds ".$gds."\n";
$submitfile = $submitfile."set hier ".$hierorflat."\n";
if ($rcemode) {
  $submitfile = $submitfile."set rcemode ".$rcemode."\n";
}
$submitfile = $submitfile."lappend src_data gdsii=".$gds;
if ($netlist) {
  $submitfile =  $submitfile." netlist=".$netlist;
  if ($portmapflag) {
    $submitfile = $submitfile." portmap=".$portmapfile."\n";
    $submitfile = $submitfile."set portmapfile ".$portmapfile."\n";
    }
  $submitfile = $submitfile."\n";
  $submitfile = $submitfile."set nettop ".$nettop."\n";
}
else {
  $submitfile = $submitfile."\n";
}
  
$submitfile = $submitfile.'
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

#  set ret [rstcl -server $::serverport -script $::script -urlparams $urlparams]
  set ret [rstcl -server $::serverport -script RemoteRun_Srv.tcl -urlparams $urlparams]
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
  #usage
}

#-------------------------------------------------------------------
# check work_dir
#-------------------------------------------------------------------
set fail 0
if {$work_dir == ""} {
  puts stderr "work_dir not set\n"
  usage
} elseif { ! [file isdirectory $work_dir]} {
  puts stderr "work_dir not found: $work_dir\n"
  #usage
}

#-------------------------------------------------------------------
# check mode
#-------------------------------------------------------------------
set fail 0
set mode_data_list [remote_request Modes]
array set mode_src_data_fields $mode_data_list
if {$mode == ""} {
  puts stderr "mode not set\n"
  set fail 1
} else {
  if { ! [info exists mode_src_data_fields($mode)]} {
    puts stderr "invalid mode: $mode"
    set fail 1
  }
}

if {$fail == 1} {
  puts stderr "valid modes on $serverport:"
  foreach mode [array names mode_src_data_fields] {
    puts stderr "[format "   Mode: %-10.10s InputFile: %s" $mode $mode_src_data_fields($mode)]"
  }

  usage
}

#-------------------------------------------------------------------
# check mode src data
#-------------------------------------------------------------------
set fail 0
if {$src_data == ""} {
  puts stderr "src_data not set\n"
  usage
} else {
  foreach src $src_data {
    set s [split $src =]
    if {[llength $s] != 2} {
      puts stderr "src_data is not a field=value pair."
      set fail 1
    } else {
      set src_data_array([lindex $s 0]) [lindex $s 1]
    }
  }

  # check required fields are set
  set mode_src $mode_src_data_fields($mode)
  foreach field $mode_src {
    if { ! [info exists src_data_array($field)]} {
      puts stderr "src_data missing data: $field"
      set fail 1
    }
  }
}

if {$fail == 1} {
  puts stderr "\nMode \"$mode\" requires the following data be defined:\n"
  puts stderr "    [join [split $mode_src] "\n    "]\n"
  usage
}

#-------------------------------------------------------------------
# check src_data files exist
#-------------------------------------------------------------------
set fail 0
foreach field $mode_src {
  if { ! [file isfile $src_data_array($field)]} {
    puts stderr "src_data file not found: $src_data_array($field)"
    set fail 1
  }
}

if {$fail == 1} {usage}

#===================================================================
# create RemoteRun note
#===================================================================
set urlparms "Mode=$mode&SrcData=[array get src_data_array]"
set rr_info_alist [remote_request NoteCreate $urlparms ]
array set rr_info $rr_info_alist

#===================================================================
# create RemoteRun workspace 
# Modified by Harsh: to add Topname and hierarchy info
#===================================================================
if {[catch {
  set ws $work_dir/$rr_info(Id)
  file mkdir $ws/submit
} err]} {
  puts stderr "Error creating workspace directory: $ws/submit"
  exit 1
}

if {[catch {
  set fd [open $ws/submit/RunInfo.txt w]
  puts $fd "Mode:$rr_info(Mode)"
  puts $fd "Hier:$hier"
  puts $fd "workdir:$work_dir"
  puts $fd "Top:$top"
  puts $fd "GDS:$gds"
';
  if ($rcemode) {
    $submitfile = $submitfile.'puts $fd "rcemode:$rcemode"
';
  }
  if ($netlist) {
    $submitfile = $submitfile.'puts $fd "nettop:$nettop"
';
  }
  if ($portmapflag eq 1) {
    
    $submitfile = $submitfile.'puts $fd "portmap:$portmapfile"';
    
  }
  $submitfile =$submitfile.'
  foreach field $mode_src {
    puts $fd "$field: $src_data_array($field)"
    file copy $src_data_array($field) $ws/submit
  }

  close $fd
  
} err]} {
  puts stderr "Error setting up submit data in: $ws/submit"
  exit 1
}

record {
  set sv_msg [setvault $rr_info(VaultURL)/$rr_info(Id)/submit@Trunk $ws/submit]
} sv_log

if {[string match "*Fail*" $sv_log]} {
  puts stderr "setvault error:"
  puts stderr "\n$sv_log\n"
  exit 1
}

record {
  set ci_msg [ci -new -comm "Submission of src_data for $rr_info(Id)" -rec $ws/submit]
} ci_log

#{Objects succeeded (2)} {}
if {[string match "*failed*" $ci_msg]} {
  puts stderr "checkin error:"
  puts stderr "\n$ci_log\n"
  exit 1
}

# additions on 10/03/04 -harsh
set urlparams "Id=$rr_info(Id)&State=submitted&Status=Job Submission complete"
remote_request RunStatus $urlparams

#puts "Job Submitted"
#puts "workspace  => $ws"
#puts "id         => $rr_info(Id)"
puts "id=$rr_info(Id)";
#puts "Mode       => $rr_info(Mode)"
#puts "SrcData    => $rr_info(SrcData)"
#puts "VaultURL   => $rr_info(VaultURL)"

exit 0
#===================================================================
';










# call pmc_sync. programs.



my($user);
unless ($user = $ENV{'USER'}){
  printf "Error: Pl. specify environment variable \$USER.\n";
  exit(1);
}
 
#$ENV{'LOGNAME'} = $user;
#$ENV{'SYNC_USER_CFGDIR'} = "/tmp/sync_silent-$user";
#my($SYNC_USER_CFGDIR) = $ENV{'SYNC_USER_CFGDIR'};
#if ( !-d $SYNC_USER_CFGDIR) {
#  system("mkdir -p $ENV{'SYNC_USER_CFGDIR'}");
#}

#my($regfile) = "$SYNC_USER_CFGDIR/UserRegistry.reg";
#my($keybase) =
#    'HKEY_CURRENT_USER\Software\Synchronicity\General\Logging';
#my($key1) = $keybase.'\LogAtStartup=dword:0' ;
#my($key2) = $keybase.'\LogDetailedOutput=dword:0';

#my($regfd) = new FileHandle($regfile, "w");
#unless ($regfd) {
#  printf "Error: Cannot open file handle for %s. Aborting ..",$regfile;
#  exit(0);
#}
#$regfd->printf("## SYNC_VERSION 1.0\n");
#$regfd->printf("%s\n", $key1);
#$regfd->printf("%s\n", $key2);

#unless($regfd->close()){
#  printf "Error: cannot create reg file %s. Aborting..\n", $regfile;
#  exit(0);
#}

my($stclc_cmd) = "1> pmcjobsubmit$$.log 2>&1 stclc --";
#print $stclc_cmd;
my($stclcfd) = new FileHandle("| $stclc_cmd");
#print $submitfile;
$stclcfd->print($submitfile);

unless ($stclcfd->close()){
  die "Error: Cannot submit job.Pl. report to PMC-MPD\n";
}
# submitted successfully: 
printf "Job Submitted successfully.\n";

my($jobidfd) = new FileHandle("pmcjobsubmit$$.log", "r");
unless ($jobidfd) {
  printf "Error: Cannot get job id file. \n";
}
my($line);
my(@words) = [];
if ($jobidfd) {
  while ($line =$jobidfd->getline()){
    if ($line =~ /stcl\> id=/) {
      chop($line);
      @words = split('=',$line);
      my($jobid) = $words[1];
      printf "\n\n\n";
      printf "Job-Id:".$jobid."\n";
      printf "Use above id for querying jobstatus \n";
      printf "\n\n\n";
    }
  }
  $jobidfd->close();
}
system("/bin/rm -f pmcjobsubmit$$.log");




#####------------------------------------------------
# querying procedure 
####-------------------------------------------------

sub checkJobStatus {
  my($jobid) = @_;
  my($stclc_cmd_checkstatus) = "";

}



sub echoUsage() {
  printf "Usage: \n\npmc_jobsubmit.pl -workdir <workdir> -mode drc -gds <filename> -top <gdstopname>\n";
  printf "  [optional] -hier\n";
  printf "\n\npmc_job_submit.pl -workdir <workdir> -mode lvs -gds <filename> -top <gdstopname> -netlist <filename> -nettop <netlist_topcell>\n";
  printf "[optional] -portmap <portmapfile>\n";
  printf "\n\npmc_job_submit.pl -workdir <workdir> -mode siliconlvs -gds <filename> -top <gdstopname> -netlist <filename> -nettop <netlist_topcell>\n";
  printf "[optional] -portmap <portmapfile>\n";
  printf "\n\npmc_job_submit.pl -workdir <workdir> -mode silicondrc -gds <filename> -top <gdstopname>>\n";
  printf "  [optional] -hier\n";
  printf "\n\npmc_jobsubmit.pl -workdir <workdir> -mode rce -gds <filename> -top <gdstopname> -netlist <filename> -nettop <netlist_topcell> -rcemode <cc/rc/rcc/conly>\n";
  printf "[optional] -portmap <portmapfile>\n";
  printf "\n\npmc_jobsubmit.pl -workdir <workdir> -mode ant -gds <filename> -top <gdstopname>\n";
  printf "\n\npmc_jobsubmit.pl -workdir <workdir> -populate <id>\n";
  # printf "\n\npmc_jobsubmit -query -jobid <>\n";
  printf "\n\n\n";

}

# ^L
# Local Variables:
# mode: perl
# perl-indent-level: 2
# fill-column: 300
# End:
