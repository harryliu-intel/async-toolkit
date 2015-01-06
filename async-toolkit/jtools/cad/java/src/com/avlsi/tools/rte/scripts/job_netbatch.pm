# vim:et:sw=4:ts=4:tw=79:
###############################################################################
# File:             job_netbatch.pm
# Author:           Chris Edmiston
# Creation Date:    May 1, 2012
# Description:      Defines how to run a job using netbatch 
#
# INTEL TOP SECRET
# Copyright 2004 - 2012 Intel Corporation
# All Rights Reserved.
###############################################################################
package job_netbatch;
use strict;
use warnings;

use base qw(Exporter);

use Cwd;
use File::Temp;
use IO::File;

use verif_util;
use util;
use rteLib;

our @EXPORT =
qw(
    job_cleanup
    job_hostname
    job_start
    job_starttime
    job_status
    job_isRunning
    job_isComplete
    nb_queue_cmd
);
our @EXPORT_FAIL =
qw(
    report_error
);

my $nb_class_default="";
my $nb_class_mem_core=0;
my %qslot_map=();

###############################################################################
# Local Functions
###############################################################################

sub report_error
{
    my ($output, $format, @arguments) = @_;

    my $fh = IO::File->new($output, "a");
    if (defined($fh))
    {
        printf($fh $format, @arguments);

        # Close the file pointed to by $fh.
        undef($fh);
    }

}   # end report_error


###############################################################################
# Public Functions
###############################################################################

###############################################################################
## @method public int job_start(char *cmd,
#                               char *path,
#                               char *outputDir,
#                               char *start_file,
#                               char *output,
#                               char *end_file,
#                               char *spec,
#                                char* nbextra,
#                               char *test)
#
# @desc             Submits the specified command as a batch job to the Grid
#                   Engine.
#
# @param[in]        cmd is the command to execute.
#
# @param[in]        path is the path to to change into before @a cmd is
#                   executed.

# @param[in]        outputDir is the path where you want the job script to be
#                   placed
#
# @param[in]        start_file is the file to create (in @a path) before @a cmd
#                   is executed.
#
# @param[in]        output is the file to send the output produced by @a cmd
#                   to.
#
# @param[in]        end_file is the file to create upon completion of @a cmd.
#
# @param[in]        nb_cfg points to the set of options to pass to netbatch(1).
#

#
# @param[in]        test is the fully qualified test name.
#
# @return           the Grid Engine job identifier.
# @return           0 if the job cannot be started.
#
###############################################################################
sub job_start
{
    my ($cmd, $path,$outputDir,$start_file, $output, $end_file, $nb_cfg,$test) = @_;

    my $cwd         = getcwd();
    my $subroutine  = get_subroutine();
    my $test_name   = extract_test_name($test);

    my $job_id      = 0;
    my $nbjob_bin   = "nbjob";
    my $umask       = UMASK;

    debug_print(9, "** $subroutine\n");
    debug_print(9, #"cmd: $cmd\n".  # too much output
                   "path: $path\n".
                   "start_file: $start_file\n".
                   "output: $output\n".
                   "end_file: $end_file\n".
                   "nb_cfg: $nb_cfg\n".
                   "test: $test\n");

    # Generate the command script.
    # [Note: It is assumed that all file names given are absolute paths.]
    my $script = 
"#!/usr/intel/bin/bash --noprofile
#
# When the main bash process is finished,0cleanup any other processes
# that may still be running.  Do not kill nbjobleader.out (the session leader)
# or netbatch will re-queue the job again and it'll run multiple times.  Also,
# do not kill this main bash process or the return code will get not be
# preserved, resulting in netbatch job failure.  
#
# For non-interactive mode netbatch jobs, we must filter by process group id.
# If we try to filter by session id, we'll end up killing processes for a
# different netbatch job!  Only the pgid can be safely used to filter to your
# job.
mainPID=`bash -c 'echo \$PPID'`
pgid=`ps x -o pid,pgid | grep \"\$mainPID \" | awk '{print \$2}' | head -1`

# parameters:
#   getPid          Name of the process to get the pid for
#
# returns:
#   processId       The process id for the specified process
function get_pid_for_process
{
    processId=`pgrep -u \$USER -g \$pgid -f -n \$getPid`
}

function cleanup_procs
{
    #echo
    #echo \"MAIN PID: \$mainPID\"
    #echo

    #echo \"BEFORE\"
    #ps xf -o user,sid,pid,pgid,cmd
    #echo

    # TODO: we may want to cleanup more than just mmonitor if we later have 
    # problems.

    getPid=mmonitor
    get_pid_for_process

    echo \"Killing mmonitor pid \$processId\"
    kill -9 \$processId 2>&1 >/dev/null
}
trap 'cleanup_procs' EXIT

umask $umask
touch $start_file
echo \"START:\$(date)\"
# Start the Fulcrum memory monitor in the background.
fulcrum --latest mmonitor &
echo \"netbatch spec  : \" $nb_cfg
cd $path
(
$cmd
)
echo \"The command :\" $cmd;
echo -n \"memory statistics: \"
getPid=mmonitor
get_pid_for_process
kill -HUP \$processId

# Sleep for 1 second to allow the memory monitor to print its statistics.
sleep 1
echo \"END:\$(date)\"
touch $end_file
# Sleep for 1 second to allow the end_file to show up before exiting, causing
# processes to be killed
sleep 1
";

    # Create the job script.
    my $fh = File::Temp->new(TEMPLATE =>  ".$test_name.XXXXXX",
                             DIR      => $outputDir,
                             SUFFIX   => '.sh',
                             UNLINK   => 0);
    my $filename = $fh->filename();
    
    $script .= "rm $filename\n";
    debug_print(10, "**\t netbatch command script: $script\n");

    # Write the job script to disk.
    debug_print(10, "**\t creating job script $filename\n");
    if (defined($fh))
    {
        print($fh $script);
        # Flush the file to disk.
        if (!defined($fh->flush()) || !defined($fh->sync()))
        {
            # The job script could not be flushed to disk. Since it is likely
            # that this failure is caused by disk quotas being exceeded or the
            # disk being full, it is futile to try to report the error via the
            # output file. Instead, a warning is generated and control is
            # returned to the caller immediately.
            my $disk = get_disk($filename);
            print("WARNING: disk quota exceeded or disk full on $disk\n");
            goto ABORT;
        }
        # Close the file pointed to by $fh.
        $fh->close();

        # Test whether nbjob(1) is a recognized command.
        if (system("which $nbjob_bin >/dev/null") != 0)
        {
            report_error($output, "FAIL: nbjob(1) not found in \$PATH\n");
            goto ABORT;
        }

        # Run netbatch.
        # [Note: STDOUT and STDERR are merged into a single stream by netbatch.]
        #my $nbjob_cmd =   "$nbjob_bin $nb_cfg"
        #               .          " -N 'r.$test_name'"         # NAME
        #               .          " -o $output -j yes"         # LOG
        #               .          " -S /bin/bash $filename";   # SCRIPT

        # first, test if the job will be accepted
        my $cmdPrefix = "$nbjob_bin run $nb_cfg ".
                        "--log-file $output ";
        my $cmdSuffix = "/usr/intel/bin/bash $filename";
        my $nbjob_cmd = "$cmdPrefix $cmdSuffix";
        debug_print(7, "nbjob command line: $nbjob_cmd\n");

        # First validate the command to avoid possible queuing errors
        # This is important to ensure that all netbatch options are valid
      
        #print "\nThe validationstring is : $cmdPrefix --validate $cmdSuffix";
        my $validate = `$cmdPrefix --validate $cmdSuffix`;
        if ($validate =~ /Job's parameters were validated/)
        {
            debug_print(7, "netbatch job command validated\n");
        }
        else
        {
            printf("\nNetbatch command validation failed: $validate\n");
            report_error($output, 
                         "Netbatch command validation failed: $validate\n");
            goto ABORT;
        }

        # Run the job and handle IO
        my $pipe = IO::Handle->new();
        open($pipe, "$nbjob_cmd 2>&1 |") or die("Can't start nbjob: $!");
        sleep 10; # allow job to start
        my $line = <$pipe>;
        # Close the pipe pointed to by $pipe.
        undef($pipe);

        if ($line =~ m/JobID (\d+)/)
        {
            $job_id = $1;
            goto EXIT;
        }
        else
        {
            report_error($output,
                         "FAIL: nbjob exit status: %s (nbjob spec: %s)\n",
                         $line,
                         $nb_cfg);
        }
    }
    else
    {
        # Try to report the error.
        report_error($output, "FAIL: cannot open $filename\n");
    }

ABORT:
    # cleanup when a failure occurs before running the job
    system("touch $end_file");
    
    # Remove the (empty) job script.
    unlink($filename);

EXIT:
    return $job_id;
}   # end job_start


# job_status($file)
#
# $file    : the expected file upon termination
#
# Checks for the existence of the file

sub job_status {
    my ($file) = @_;

    if(-e $file) {
        return 1;
    } else {
        return 0;
    }
}
# job_isRunning($jobId)

# $jobId : To find out if the job with id "$jobId" is running
# Uses the nbstatus jobs command to do so 

sub job_isRunning {
    my ($job_id) = @_;
    
    my $handle = IO::Handle->new();
    open($handle, "-|",
        "nbstatus jobs --fields Jobid,Status --format script Jobid=$job_id")
        or die ("Can't start nbstatus: $!");
    my $output = <$handle>;
    my ($id, $status) = split(/,/, $output);
    $handle->close();
    chomp($status);

    my $isRunning = (lc($status) eq "run") ? 1 : 0;
    if($isRunning){
       return 1;
    }
    else{
       return 0;
    }

}

# job_isComplete($jobId)

# $jobId : To find out if the job with id "$jobId" is complete
# Uses the nbstatus jobs command to do so 
# If nbstatus returns no status, then the job must also have finished.

sub job_isComplete {
    my ($job_id) = @_;
    
    my $handle = IO::Handle->new();
    open($handle, "-|",
        "nbstatus jobs --fields Jobid,Status --format script Jobid=$job_id")
        or die ("Can't start nbstatus: $!");
    my $output = <$handle>;
    my $isComplete;
    if ($output) {
        my ($id, $status) = split(/,/, $output);
        $handle->close();
        chomp($status);

        $isComplete = (lc($status) eq "comp") ? 1 : 0;
    } else {
        $isComplete = 1;
    }
    return $isComplete;
}


###############################################################################
## @method public void job_cleanup(int job_id, char *path)
#
# @desc             Cleans up after the Grid Engine batch job has finished.
#
# @param[in]        job_id is the job ID.
#
# @param[in]        path points to the session's working directory.
#
###############################################################################
sub job_cleanup
{
    my ($job_id, $path) = @_;

    my $subroutine = get_subroutine();
    debug_print(7, "$subroutine: job_id = $job_id, path = $path\n"); 

    # First, determine if the job has completed.  If so, don't do anything.
    my $handle = IO::Handle->new();
    open($handle, "-|",
        "nbstatus jobs --fields Jobid,Status --format script Jobid=$job_id")
        or die ("Can't start nbstatus: $!");
    my $output = <$handle>;
    my ($id, $status) = split(/,/, $output);
    $handle->close();
    chomp($status);

    my $doRemove = (lc($status) eq "comp") ? 0 : 1;

    # see nbstatus jobs -h for filter info
    if ($doRemove)
    {
        debug_print(7, "$subroutine: Removing job $job_id from Netbatch\n");
        my $filter="Jobid=='$job_id'";
        print(`nbjob remove --reason "Regression System" $filter >/dev/null`);
    }
}   # end job_cleanup


###############################################################################
## @method public char* job_hostname(int job_id)
#
# @desc             Retrieves the hostname on which the job corresponding to
#                   the supplied job ID is running.
#
# @param[in]        job_id is the job ID.
#
# @return           the hostname on which the job corresponding to @a job_id is
#                   running.
# @return           QUEUED if the job is queued and waiting.
# @return           @c undef otherwise.
#
###############################################################################
sub job_hostname
{
    my ($job_id) = @_;

    my $subroutine = get_subroutine();

    debug_print(7, "$subroutine: job_id = $job_id\n");
   
    my $handle = IO::Handle->new();
    open($handle, "-|", 
        "nbstatus jobs --fields Jobid,Status,Workstation ".
                      "--format script Jobid=$job_id") 
        or die ("Can't start nbstatus: $!");
    my $output = <$handle>;

    my ($id, $status, $host) = split(/,/, $output);
    $handle->close();
    chomp($host);

    if (defined $id)    # if id is defined, all fields will be
    {
        # Sanity check the id
        if ($id != $job_id)
        {
            printf("id doesn't match: $id != $job_id\n");
            return undef;
        }

        # status can be: Run, Wait, Comp, Send, Fail, DEL, Disc-D, Disc-R,
        #                Move, Inactive
        # If the job isn't running or waiting, we'll consider it an error
        if (lc($status) eq "wait")
        {
            return "QUEUED";
        }
        elsif (lc($status) eq "run")
        {
            return $host;
        }
        else
        {
            # some sort of error likely occured
            return undef;
        }
    }
    else # !defined $id
    {
        # XXX: It's possible this means the job is in the qslots:
        #      nbstatus qslots.  Not yet clear if this is the case.

        # if not found, return undef
        return undef;
    }
}   # end job_hostname


# job_starttime($tag)
#
# $tag    : the job id (returned by job_start)
#
# Checks for the time when the job actually started running
sub job_starttime {
    my ($tag) = @_;

    my $subroutine = get_subroutine();

    my $time = "UNKNOWN";

    my $handle = IO::Handle->new();
    open($handle, "-|", 
        "nbstatus jobs --fields Jobid,StartTime ".
                      "--format script Jobid=$tag") 
        or die ("Can't start nbstatus: $!");
    my $output = <$handle>;

    debug_print(7, "$subroutine: output: $output");
    
    my ($id, $starttime) = split(/,/, $output);
    $handle->close();
    chomp($starttime);

    if (defined $id)    # if id is defined, all fields will be
    {
        # Sanity check the id
        if ($id != $tag)
        {
            printf("id doesn't match: $id != $tag\n");
            return undef;
        }

        # XXX if the job hasn't started, starttime might be null

        # TODO: not sure what the start time format needs to look like, 
        # so for now we're going to leave it alone
        # we know it needs to have yyyy/mm/dd

        # starttime = 04/26/2012 10:14:26
        $starttime =~ s/(\d\d\/\d\d)\/(\d{4})/$2\/$1/;

        # starttime = 2012/04/26 10:14:26
        return $starttime;
    }
    else # !defined $id
    {
        # XXX: It's possible this means the job is in the qslots:
        #      nbstatus qslots.  Not yet clear if this is the case.

        # if not found, return undef
        return undef;
    }

    #my $time = `qstat | grep $tag| cut -c 45-63`;
    #chomp($time);

    #convert from mm/dd/yyyy to yyyy/mm/dd
    #$time =~ s/(\d\d\/\d\d)\/(\d{4})/$2\/$1/;

    return $time
}


sub parse_nbconfig{
  return if (not defined $ENV{FULCRUM_NB_CONFIG});
  return if %qslot_map;

  if (open (P, "<$ENV{FULCRUM_NB_CONFIG}")) {
    while(<P>){
      chomp;
      next if (/^#/);
      if(/TOOLNAME/i){
        my $ptool=0; my $pslot=0;
        my @tools; my $qslot="";
        my @tokens=split(/\s|=|,/, $_);
        foreach my $t(@tokens){
          if($t =~ /TOOLNAME/i){
              $ptool=1; $pslot=0; next;
          }elsif($t =~ /QSLOT/i){
              $ptool=0; $pslot=1; next;
          }elsif($ptool==1){
              push @tools, $t;
          }elsif($pslot==1){
              $qslot=$t;
          }
        }
        foreach my $t (@tools){
            $qslot_map{$t}=$qslot;
          if($t eq 'default'){
            $qslot_map{''}=$qslot;
          }
        }
      }if(/NBPOOL\s*=\s*(\S+)/){
        $ENV{NBPOOL}=$1;
      }if(/CLASS_DEFAULT\s*=\s*(\S+)/){
        $nb_class_default=$1;
      }if(/CLASS_MEM_CORE\s*=\s*(\d+)/){
        $nb_class_mem_core=$1;
      }
    }
  }
}


sub nb_queue_cmd {
    my ($exe, $cores, $mem, $parallel) = @_;

    parse_nbconfig();
    
    my @smartresource=();
    if($nb_class_mem_core){
      if($nb_class_default ne ""){
        my $nb_class_prefix=$nb_class_default;
        $nb_class_prefix=~s/_(\d+)G(&&\d+C)?$//;
        push @smartresource, "${nb_class_prefix}_${mem}G";
        push @smartresource, "${cores}C" if ($cores > 1);
      }
    }else{
      push @smartresource, $nb_class_default if($nb_class_default ne "");
    }
    
    my @res = ();
    push @res, "cores=$cores" if $cores and $cores =~ /^\d+$/;
    push @res, "memory=$mem" if $mem and $mem =~ /^\d+$/;
    my $qslot=$qslot_map{''};
    if ( defined $qslot_map{"rteReport"}){
      $qslot=$qslot_map{"rteReport"};
    }elsif($qslot_map{"rte"}){
      $qslot=$qslot_map{"rte"};
    }
    
    my @cmd = ('nbjob', 'run', '--mode', 'interactive', '--qslot', $qslot);
    push @cmd, '--class-reservation', join(',', @res) if @res;
    push @cmd, '--parallel', $parallel if $parallel;
    push @cmd, "--class","\'".join("\&\&", @smartresource)."\'" if @smartresource;
    return join(' ', @cmd, $exe);
}

1;
