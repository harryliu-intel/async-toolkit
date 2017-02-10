#
# Util
#
# Supersize module containing utility functions needed by other modules
#
#

package Supersize::Util;

BEGIN {
    use Exporter;
    our @ISA = qw(Exporter);
    our @EXPORT = qw( 
        &usage
        &command_die
        &debug_print
        &basetype_of
        &subtype_number_of
        &base_array_instance_name
        &combine_hashes
        &backup_directory
        &restore_directory
        &delete_backup_directory
        &path_to_tool
        &path_to_libs
        &check_free_system_memory
        &supersize_system
        $DISTRIBUTED_JOB
        $MAJOR_JOB
        $MINOR_JOB
        $LOCAL_JOB
        &launch_job
        &kill_job
        &check_jobs
        &job_is_running
        &save_job_state
        &restore_job_state
        JOB_KILLED
        JOB_EXIT
        &get_active_command
        &fqcn_to_file
        &fqcn_to_directory
        &get_work_dir
        &set_dirty
        &underline
        &bold
        &get_pdk_variable
    );
    our @EXPORT_OK = qw(
        &get_module_data
    );
}

use Carp;
use IPC::Open2;
use Term::ANSIColor;
use Supersize::TypeUtil;
use strict;

#
# Main help message (centralized here so other packages can reference it)
#
sub usage {
    my $SS_r = shift;
    my $msg  = shift;
    # print ${$SS_r->{IGV}->{cmd_stack}}[$#{$SS_r->{IGV}->{cmd_stack}}] . ": " 
    #     if (defined ${$SS_r->{IGV}->{cmd_stack}}[0] && defined $msg);
    print $msg . "\n" if (defined $msg);
    if (!$SS_r->{IGV}->{INTERACTIVE}) {
        print "\n" if (defined $msg);
        print <<EOF;
Usage: supersize [options] [<command> [command arguments]]

  General options:
   [--work-dir=<directory>]             (Default: *_\\d+ps)
   [--source=init_file]                 (Default: WORK_DIR/supersize.vars)
   [--verbose=0|1]                      (Default: 1)
   [--debug=0|1]                        (Default: 0)
   [--backup=0|1]                       (Default: 0)

    If --work-dir isn't provided, an attempt will be made to find an 
    ubersize working directory within the current directory.  If you have
    more than one (or none), you'll need to specify --work-dir or else set
    it in an initialization --source file.  The default command is 
    'interactive', which is all you really need.
EOF
    }
    if (!$SS_r->{IGV}->{INTERACTIVE} || !defined $msg) {
        my ($cmd, $mod, $display_mod);
        format CommandListing_Top =

  Supported Commands:                (module)

.
        format CommandListing =
    @<<<<<<<<<<<<<<<<<<<<<<<<<<<<<   @<<<<<<<<<<<<<<<<<<<<<<<
$cmd,$display_mod
.
        STDOUT->format_top_name("CommandListing_Top");
        STDOUT->format_name("CommandListing");
        STDOUT->format_lines_per_page(1000);
        STDOUT->format_lines_left(0);
        foreach $mod (sort keys %{$SS_r->{IGV}{MODULES}}) {
            $display_mod = $mod eq "" ? "-" : $mod;
            foreach $cmd (sort keys %{$SS_r->{IGV}{MODULES}{$mod}{COMMANDS}}) {
                write;
            }
        }
        print "\n";
    }
    if ($SS_r->{IGV}->{INTERACTIVE} && !defined $msg) {
        print "  Type 'help " . color('underline') . "command" .
              color('reset') . "' (or 'help " . color('underline') .
              "module" . color('reset') . "/" . color('underline') .
              "command" . color('reset') . "') ".
              "for more information.\n\n";
    }
}

# Call this to exit a command due to an error condition
sub command_die {
    my $SS_r = shift;
    my $msg  = shift;
    if (!$SS_r->{IGV}{INTERACTIVE}) {
        print STDERR "$msg\n" if (defined $msg);
        usage(@_);
        exit -1;
    }
    else {
        print STDERR "$msg\n" if (defined $msg);
        if ($SS_r->{GS}{DEBUG}) {
            Carp::longmess;
        }
        die;
    }
}

# Prints message only if DEBUG==1
sub debug_print {
    my $SS_r = shift;
    my $msg  = shift;
    print $msg if ($SS_r->{GS}{DEBUG});
}

# Given a.b.c.NUM, returns a.b.c
sub basetype_of {
    my $fqcn = shift;
    my $last_dot = rindex $fqcn, ".";
    return substr($fqcn, 0, $last_dot);
}

# Given a.b.c.NUM, returns NUM
sub subtype_number_of {
    my $fqcn = shift;
    my $last_dot = rindex $fqcn, ".";
    return substr($fqcn, $last_dot+1);
}

# Returns an instance name stripped of array indices.  For example,
# one[2].two[1].three -> one.two.three
sub base_array_instance_name {
    my $instname = shift;
    my $inst_str = $instname;
    $inst_str =~ s/\[[^\]]*\]//g;
    return $inst_str;
}

# Given (\%src_hash, \%dest_hash_1, \%dest_hash_2, ...), adds all key/value
# pairs from dest_hash_i (i=1..N) to src_hash.  You'd think Perl would
# already define this.
sub combine_hashes {
    my $dest_ref = shift;
    while (my $src_ref = shift) {
        foreach my $k (keys %{$src_ref}) {
            $dest_ref->{$k} = $src_ref->{$k};
        }
    }
    return $dest_ref;
}

# Returns the active working directory.  This is usually WORK_DIR as set
# by the user at runtime, but can be overridden (e.g. during trial merging)
# by setting the internally-set WORK_SUBDIR variable.
sub get_work_dir {
    my $SS_r = shift;
    if (exists $SS_r->{GS}{WORK_SUBDIR} && $SS_r->{GS}{WORK_SUBDIR} ne "") {
        return $SS_r->{GS}{WORK_SUBDIR};
    }
    else {
        return $SS_r->{GS}{WORK_DIR};
    }
}

# Saves a backup of WORK_DIR/$dir by copying (-a) all contents to
# WORK_DIR/$dir.save.
sub backup_directory {
    my $SS_r = shift;
    my $dir  = shift;

    my $fqdir = get_work_dir($SS_r) . "/$dir";
    if (-e "${fqdir}.save") {
        print STDERR "Warning: Deleting stale ${fqdir}.save.\n"
            if ($SS_r->{GS}{VERBOSE});
        `rm -rf "${fqdir}.save"`;
    }
    `cp -a "$fqdir" "${fqdir}.save"`;
}

# Restores WORK_DIR/$dir, assuming a backup was saved.  Moves the old
# WORK_DIR/$dir to WORK_DIR/$dir.${cmd}_error if DEBUG==1.
sub restore_directory {
    my $SS_r = shift;
    my $dir  = shift;
    my $wdir = get_work_dir($SS_r);

    if (!-e "$wdir/$dir.save") {
        print STDERR "Warning: Can't restore $wdir/$dir.\n";
        print STDERR "         This error may be unrecoverable.\n";
        return;
    }
    if ($SS_r->{GS}{DEBUG}) {
        my ($mod, $cmd) = get_active_command($SS_r);
        rename "$wdir/$dir", "$wdir/${dir}.${cmd}_error" 
    }
    else {
        `rm -rf "$wdir/$dir"`;
    }
    rename "$wdir/$dir.save", "$wdir/$dir";
}

# Get rid of the backup directory
sub delete_backup_directory {
    my $SS_r = shift;
    my $dir  = shift;
    my $wdir = get_work_dir($SS_r);
    `rm -rf "$wdir/$dir.save"`;
}


# Returns an absolute executable path to the specified tool.
# Relies on PACKAGE_ROOT.  If PACKAGE_ROOT isn't set (really, an error 
# condition), will return "/usr/local/fulcrum/bin/fulcrum <tool>" just 
# in case fulcrum can find the program.
#
# In the case that this java command will be dispatched to SGE, the
# following additional variables come into play:
#
#   SGE_LEVEL           - Current SGE run level
#   SGE_ARCH::level     - Architecture to which qsub jobs will be dispatched
#   SGE_PACKAGE_ROOT::arch - Package root for each architecture.
#
# The argument $run_level determines whether this command will be dispatched
# to SGE; if run_level <= SGE_LEVEL, it's assumed the job will be dispatched
# to a machine of SGE_ARCH::run_level
#
# The $use_server argument indicates whether the specified java command is
# to be sent to the cast file server.  If it is, the command line will be
# formatted appropriately (Java class will be referenced etc.)  If the 
# Java class isn't known (or if the job is to be dispatched to SGE), then
# the $use_server variable will be overridden to 0 and passed back as the
# routine's second return value.
#
sub path_to_tool {
    my $SS_r       = shift;
    my $tool       = shift;
    my $runlevel   = shift;
    my $use_server = shift;

    # Java class map
    my %java_class_map = (
        "jauto" => "com.avlsi.tools.jauto.Jauto"
    );

    # Where to find tools in other packages, and whether those tools require
    # a --pdk.
    my %fulcrum_tools = (
        cdsp4add     => { needs_pdk => 0 },
        cdsp4addlibs => { needs_pdk => 0 },
        cdsp4edit    => { needs_pdk => 0 },
        lve          => { needs_pdk => 1 },
        mkcdswd      => { needs_pdk => 1 },
        layout       => { needs_pdk => 1 },
        layoutPlus   => { needs_pdk => 1 },
    );
    my $use_fulcrum = grep { $tool eq $_ } keys %fulcrum_tools;

    # Determine path based on runlevel vs SGE_LEVEL
    if (!$use_fulcrum && defined $SS_r->{GS}{PACKAGE_ROOT} &&
        (!defined $runlevel || !defined $SS_r->{GS}{SGE_LEVEL} ||
         $runlevel > $SS_r->{GS}{SGE_LEVEL} ||
         !is_scalar($SS_r, "SGE_ARCH::$runlevel"))) {
        # Run locally, or assume SGE arch is the same as local one
        if (defined $use_server && $use_server && 
            exists $java_class_map{$tool}) {
            # Run with CastFileServer
            return ($java_class_map{$tool}, $use_server);
        }
        else {
            # Run via wrapper script
            $use_server = 0;
            return ("$SS_r->{GS}{PACKAGE_ROOT}/bin/$tool", $use_server);
        }
    }
    elsif (!$use_fulcrum && defined $runlevel && 
           defined $SS_r->{GS}{SGE_LEVEL} &&
           $runlevel <= $SS_r->{GS}{SGE_LEVEL} &&
           is_scalar($SS_r, "SGE_ARCH::$runlevel")) {
        # Run remotely on SGE_ARCH::runlevel
        my $sge_arch = dereference_scalar($SS_r, "SGE_ARCH::$runlevel");
        my $sge_package_root = 
            dereference_scalar($SS_r, "SGE_PACKAGE_ROOT::$sge_arch");
        command_die($SS_r, "SGE_PACKAGE_ROOT::$sge_arch isn't defined.") 
            if (!defined $sge_package_root);
        return ("$sge_package_root/bin/$tool", 0);
    }
    else {
        # invoke tool directly rather than through fulcrum, as path has been
        # setup by fulcrum already, and all tools are available in the all
        # package
        my $cmd = $tool;
        if ($fulcrum_tools{$tool}->{needs_pdk}==1) {
            $cmd .= " --fulcrum-pdk-root=$SS_r->{GS}{PDK_ROOT}";
        }
        return ($cmd, 0);
    }
}


# Get LD_LIBRARY_PATH (only valid for local jobs).  Nasty system-specific crap
sub path_to_libs {
    my $SS_r = shift;

    my $osType=`uname -s`; chomp $osType;
    my $archType=`uname -m`; chomp $archType;

    my $lib_path = "$SS_r->{GS}{PACKAGE_ROOT}/$osType-$archType/lib:" .
                   "/usr/local/lib:";

    if ($archType eq "x86_64") {
        $lib_path .= "/usr/lib64:/lib64";
    }
    else {
        $lib_path .= "/usr/lib:/lib";
    }
}


#
# Checks if the amount of free memory (in megabytes) that's available for 
# supersize use is less than the MEM/LOCAL_MEM/SGE_MEM amount specified.
# Note: this only works on a linux system.  TODO: Support solaris?
#
sub check_free_system_memory {
    my $required = shift;
    # Convert to kB
    $required =~ s/([MG]?)$//;
    if ($1 eq "M") {
        $required *= 1000;
    }
    elsif ($1 eq "G") {
        $required *= 1000000;
    }
    else {
        # strange format, let the caller deal with it
        return 1;
    }
    return 1 if (!-e "/proc/meminfo" || !open (MEMINFO, "/proc/meminfo"));
    my $free = 0;
    while (<MEMINFO>) {
        if (/^MemFree:\s*(\d+)\s+kB$/) {
            $free += $1;
        }
        elsif (/^Buffers:\s*(\d+)\s+kB$/) {
            $free += $1;
        }
        elsif (/^Cached:\s*(\d+)\s+kB$/) {
            $free += $1;
        }
    }
    close MEMINFO;
    # Undoubtedly indicates an unexpected /proc/meminfo syntax...
    return 1 if ($free == 0);
    return ($required < $free, sprintf("%dM", $free/1000));
}

# Launches a concurrent job (runs $cmd in a forked process).
#
# When this function is invoked, $job_file should have been created under
# WORK_DIR/concurrent.  $id should be a very short identification string
# (e.g. "CS") which is used to identify the job to the user.  Internally,
# $job_file serves as a unique job handle.  
#
# When the job completes for any reason (command exits or the process is
# killed), the completion &{$callback} subroutine will be called with the
# following arguments:
#
#   ($SS_r, $job_file, $status, $exit_status)
#
# Where $status is either of the following:
# 
use constant JOB_KILLED => -1;
use constant JOB_EXIT   => 0;
#
# and $exit_status is the exit status of $cmd, unless $cmd couldn't be started.
#
# When the callback is invoked, $job_file will have been renamed to 
# $job_file.done.  If there was an error running the command, the message
# that would have been printed to STDERR will be in $job_file.error.
#
sub launch_job {
    my $SS_r     = shift;
    my $cmd      = shift;
    my $job_file = shift;
    my $id       = shift;
    my $callback = shift;
    my $runlevel = shift;   # Run level of forked $cmd
    my $setenv_r = shift;   # environment variables to set for $cmd.
    my $out_file = shift;   # Job's standard output is sent to this file
    my $err_file = shift;   # JOb's standard error is sent to this file

    my $wdir     = get_work_dir($SS_r);
    my $jf       = "$wdir/concurrent/$job_file";

    # Assertion checks
    if (!-e $jf) {
        die "Internal assertion error in launch_job: " .
            "Job file doesn't exist.\n";
    }
    if (job_is_running($SS_r, $job_file)) {
        die "Internal assertion error in launch_job: " .
            "Prior job $job_file is running.\n";
    }

    # Fork and run
    my $pid = fork;
    if (!defined $pid) {
        rename $jf, "$jf.done";
        command_die($SS_r, "Couldn't fork child process.");
    }
    if (!$pid) {
        # child
        close STDIN;
        close STDERR;
        open STDIN, "/dev/null";
        open STDERR, ">$err_file";
        my $status = -1;
        eval {
            # Run in eval to ensure we clean up $job_file if the command exits 
            # abnormally.
            supersize_system($SS_r, $cmd, $runlevel, $setenv_r, $out_file);
            $status = ($? == -1 || ($? & 127)) ? $? : ($? >> 8);
        };
        if ($@) {
            my $f = "$wdir/concurrent/$job_file.error";
            open(RESULT, ">$f") || die "Couldn't write to $f.\nCommand: $cmd\n".
                                       "Failed with: $@";
            print RESULT $@;
            close RESULT;
        }
        rename $jf, "$jf.done" || die "Error cleaning up launched job:\n" .
                                      "Couldn't rename $job_file.";
        exit $status;
    }

    # Set up run state
    $SS_r->{IGV}{concurrent} = {} if (!exists $SS_r->{IGV}{concurrent});
    $SS_r->{IGV}{concurrent}{$job_file} = {};
    $SS_r->{IGV}{concurrent}{$job_file}{ID} = $id;
    $SS_r->{IGV}{concurrent}{$job_file}{CALLBACK} = $callback;
    $SS_r->{IGV}{concurrent}{$job_file}{PID} = $pid;
}

# Forcibly kill a running concurrent job.  Invokes the completion callback.
sub kill_job {
    my $SS_r     = shift;
    my $job_file = shift;

    my $wdir = get_work_dir($SS_r);

    # Kill the process
    kill 9, $SS_r->{IGV}{concurrent}{$job_file}{PID};
    waitpid $SS_r->{IGV}{concurrent}{$job_file}{PID}, 0;

    # Move job file
    my $jf = "$wdir/concurrent/$job_file";
    rename $jf, "$jf.done" if (-e $jf);

    # Invoke callback
    my $callback = $SS_r->{IGV}{concurrent}{$job_file}{CALLBACK};
    eval "$callback(\$SS_r, \$job_file, Supersize::Util::JOB_KILLED);";
    if ($@) {
        print STDERR "Error: $@";
    }

    # Purge job state
    delete $SS_r->{IGV}{concurrent}{$job_file};
}
    
# Check if a concurrent job is still running
sub job_is_running {
    my $SS_r        = shift;
    my $job_file = shift;
    
    if (!exists $SS_r->{IGV}{concurrent}{$job_file} ||
        !-e "/proc/$SS_r->{IGV}{concurrent}{$job_file}{PID}") {
        return 0;
    }
    else {
        return 1;
    }
}

# Called to check if any concurrent jobs have completed.  Closes them down
# if so.
sub check_jobs {
    my $SS_r = shift;

    $SS_r->{IGV}{job_string} = "";
    return if (!exists $SS_r->{IGV}{concurrent});

    my $jdir = get_work_dir($SS_r) . "/concurrent";
    foreach my $job_file (sort keys %{$SS_r->{IGV}{concurrent}}) {
        if (!-e "$jdir/$job_file") {
            # Job completed
            my $pid = $SS_r->{IGV}{concurrent}{$job_file}{PID};
            my $id  = $SS_r->{IGV}{concurrent}{$job_file}{ID};
            my $status = -1;
            print color('yellow') . color('bold') .
                  ">>> Job $id completed <<<" .  color('reset');

            # Reap process
            if (-e "/proc/$pid") {
                print "\n";
                waitpid $pid, 0;
                $status = $?;
            } else {
                print '  ' . color('red') . color('bold') .
                      '[Unable to reap process to get exit status]' .
                      color('reset') . "\n";
            }

            # Invoke callback
            my $callback = $SS_r->{IGV}{concurrent}{$job_file}{CALLBACK};
            (my $package = $callback) =~ s/::[^:]+$//;
            eval "$callback(\$SS_r, \$job_file, Supersize::Util::JOB_EXIT, \$status);";
            if ($@) {
                print STDERR "Error in callback $callback: $@";
            }

            # Purge job state
            delete $SS_r->{IGV}{concurrent}{$job_file};
        }
        else {
            if (!-e "/proc/$SS_r->{IGV}{concurrent}{$job_file}{PID}") {
                print STDERR "Concurrent job " .
                    $SS_r->{IGV}{concurrent}{$job_file}{ID} . " died badly.\n";
                print STDERR "Cleaning up the mess.\n";
                unlink "$jdir/$job_file";
                delete $SS_r->{IGV}{concurrent}{$job_file};
                next;
            }
            if ($SS_r->{IGV}{job_string} eq "") {
                $SS_r->{IGV}{job_string} = 
                    color('reset') . "(" .  color('bold') . color('green');
            }
            else {
                $SS_r->{IGV}{job_string} .= "|";
            }
            $SS_r->{IGV}{job_string} .= $SS_r->{IGV}{concurrent}{$job_file}{ID};
        }
    }
    if ($SS_r->{IGV}{job_string} ne "") {
        $SS_r->{IGV}{job_string} .= color('reset') . ")" . color('underline');
    }
}

# Store state of all running concurrent jobs so they can be passed on
# to a future supersize session
sub save_job_state {
    my $SS_r = shift;
    return if (!exists $SS_r->{IGV}{concurrent} ||
               !keys %{$SS_r->{IGV}{concurrent}});
    
    my $wdir = get_work_dir($SS_r);
    my $jdir = "$wdir/concurrent";
    if (open(JS, ">$wdir/supersize.jobs")) {
        print JS "# Auto-generated supersize file containing active,\n";
        print JS "# concurrent job state.  Delete if you don't care to\n";
        print JS "# recover these jobs in your next supersize session.\n";
        foreach my $job_file (sort keys %{$SS_r->{IGV}{concurrent}}) {
            print JS "$job_file ";
            print JS $SS_r->{IGV}{concurrent}{$job_file}{PID} . " ";
            print JS $SS_r->{IGV}{concurrent}{$job_file}{ID} . " ";
            print JS $SS_r->{IGV}{concurrent}{$job_file}{CALLBACK} . "\n";
        }
        close JS;
    }
}

# Restore concurrent job state from a prior supersize session
sub restore_job_state {
    my $SS_r = shift;
    my $wdir = get_work_dir($SS_r);
    if (!-e "$wdir/supersize.jobs") {
        my $cdir = "$wdir/concurrent";
        return if (!-e $cdir);
        opendir CDIR, $cdir;
        my @jfiles = grep { $_ !~ /^\./ && $_ !~ /\.done$/ } readdir CDIR;
        foreach my $jf (@jfiles) {
            print STDERR "Deleting stale concurrent job file $jf.\n";
            unlink "$cdir/$jf";
        }
    }
    elsif (open JS, "$wdir/supersize.jobs") {
        $SS_r->{IGV}{concurrent} = {} if (!exists $SS_r->{IGV}{concurrent});
        while (<JS>) {
            s/\s*\#.*$//; s/^\s*//; chomp;
            next if (/^\s*$/);
            my ($job_file, $pid, $id, $callback) = split /\s+/, $_;
            if (!defined $job_file || !defined $pid || !defined $id ||
                !defined $callback) {
                print STDERR "Skipping malformed line from supersize.jobs:\n";
                print STDERR "  $_\n";
                next;
            }
            $SS_r->{IGV}{concurrent}{$job_file} = {};
            $SS_r->{IGV}{concurrent}{$job_file}{PID} = $pid;
            $SS_r->{IGV}{concurrent}{$job_file}{ID} = $id;
            $SS_r->{IGV}{concurrent}{$job_file}{CALLBACK} = $callback;
        }
        close JS;
        unlink "$wdir/supersize.jobs";
    }
}

#
# SGE_LEVEL (runlevel) constants.  One of these values needs to be passed
# to the supersize_system routine.
#
our $DISTRIBUTED_JOB = 1;
our $MAJOR_JOB = 2;
our $MINOR_JOB = 3;
our $LOCAL_JOB = 4;

# External system call, i.e. to run a tool which may require significant
# system resources.  For now, all this does is save the call's standard
# output to WORK_DIR/$cmd.out, and optionally prints the command only if
# JUST_PRINT==1.  If SGE_LEVEL>=runlevel, qrsh will be used to dispatch 
# the job to an SGE compute server.  In this case, the following variables 
# come into play:
#
my $supersize_system_globals_r = {
    SGE_LEVEL => { DESC =>
        "Determines which jobs are dispatched to SGE, based on whether " .
        "the job's runlevel <= SGE_LEVEL.  Generally, the higher the " .
        "value, the more jobs that will be sent to SGE.  Run levels " .
        "are defined as follows:\n" .
        "  1 - Distributed jobs such as lve charge sharing.\n" .
        "  2 - Major compute jobs requiring maximal system\n" .
        "      resources.\n" .
        "  3 - Jobs which must parse the entire Cast of TOP,\n" .
        "      but don't do much more.\n" .
        "  4 - Trivial local tasks which you wouldn't\n" .
        "      normally consider sending to SGE.\n" .
        "The default value of SGE_LEVEL is 0.  If you set it higher, be " .
        "sure to also set the corresponding SGE_* variables that are " .
        "parameterized on run level." },
    SGE_ARCH  => { 
        TYPE => $TYPE_MAP, 
        DESC => "Determines the SGE arch " .
        "resource of each job run level.  If it isn't defined, no arch " .
        "resource will be specified and it's assumed that the remote " .
        "architecture is the same as the local one.  Parameterized on " .
        "job run level." },
    SGE_MEM   => { 
        TYPE => $TYPE_MAP,
        DESC =>
        "Sets max-heap-size for Java commands dispatched to SGE.  Also " .
        "sets mem= resource to SGE.  Specify as <num>M or <num>G. (For " .
        "SGE, will round up to next gigabyte.)  Parameterized on job " .
        "run level." },
    SGE_NOW   => { 
        TYPE => $TYPE_MAP,
        DESC =>
        "If set, will abort command if a compute server isn't available. " .
        "Parameterized on job run level." },
    SGE_PRIORITY => { 
        TYPE => $TYPE_MAP,
        DESC =>
        "Specifies the priority to pass to qrsh (optional).  Parameterized " .
        "on job run level." },
    SGE_PACKAGE_ROOT => {
        TYPE => $TYPE_MAP,
        DESC =>
        "Package root for jobs dispatched to SGE.  Parameterized by " .
        "SGE arch (e.g. lx24-amd64).  If SGE_ARCH is set, then this " .
        "variable -must- also be set." }
};
#
# Determining failure: If we can't run the command, a message to that effect
# will be included in a call to command_die.  However, the return value of
# the command itself is not checked; you'll need to check $?.
#
# If $return_stdin==1, then the command will be run with open2 and its *STDIN
# glob will be returned, along with the process ids from the open2 call as well
# as the forked child process so that waitpid can be called when the caller is
# done.
# 
sub supersize_system {
    my ($SS_r, $cmd, $runlevel, $setenv_r, $outfile, $return_stdin,
        $echo_stdout) = @_;
    my $wdir = get_work_dir($SS_r);

    # SGE setup
    my $do_sge = 0;
    my @sge_resources = ();
    if (defined $SS_r->{GS}{SGE_LEVEL} && 
        $SS_r->{GS}{SGE_LEVEL} >= $runlevel) {
        $do_sge = 1;
        if (is_scalar($SS_r, "SGE_ARCH::$runlevel")) {
            push @sge_resources, "arch=" . 
                dereference_scalar($SS_r, "SGE_ARCH::$runlevel");
        }
        my $sge_mem = dereference_scalar($SS_r, "SGE_MEM::$runlevel");
        $sge_mem = $SS_r->{GS}{MEM} if (!defined $sge_mem);
        if ($sge_mem =~ /(\d+)(\w)/) {
            my $sge_mem = $1;
            if ($2 eq "M") {
                push @sge_resources, "mem=" . 1000*int(($sge_mem+999)/1000);
            }
            elsif ($2 eq "G") {
                push @sge_resources, "mem=" . $sge_mem*1000;
            }
        }
    }
    my $sge_now = dereference_scalar($SS_r, "SGE_NOW::$runlevel");
    my $sge_priority = dereference_scalar($SS_r, "SGE_PRIORITY::$runlevel");

    # Set up qrsh command line
    if ($do_sge) {
        my $sge_cmd = "qrsh -cwd ";
        foreach my $sge_resource (@sge_resources) {
            $sge_cmd .= "-l $sge_resource ";
        }
        if ($sge_now) {
            $sge_cmd .= "-now y ";
        }
        else {
            $sge_cmd .= "-now n ";
        }
        if (defined $sge_priority) {
            $sge_cmd .= "-p $sge_priority ";
        }
        if (defined $setenv_r && %{$setenv_r}) {
            $sge_cmd .= "-v ";
            foreach my $var (keys %{$setenv_r}) {
                $sge_cmd .= "$var=" . $setenv_r->{$var} . ",";
            }
            # Don't forward X11 display
            $sge_cmd .= "DISPLAY= ";
        }
        $sge_cmd .= "-noshell "; #/bin/bash --norc -c ";
        $cmd = "$sge_cmd \\\n" . $cmd;
    }
    else {
        # Set environment variables if running locally
        my $pre_cmd = "";
        if (defined $setenv_r && %{$setenv_r}) {
            foreach my $var (keys %{$setenv_r}) {
                $pre_cmd .= "$var=" . $setenv_r->{$var} . " ";
            }
        }
        $cmd = $pre_cmd . $cmd;
    }

    # Set outfile if one wasn't specified
    if (!defined $outfile) {
        my $cmd_short = (get_active_command($SS_r))[1];
        $outfile = "$wdir/$cmd_short.out";
    }

    # Run the command
    if ($SS_r->{GS}{JUST_PRINT} || $SS_r->{GS}{DEBUG}) {
        print "---- Running ----\n";
        print $cmd . "\n";
        print "-----------------\n";
        return if ($SS_r->{GS}{JUST_PRINT});
    }
    print STDERR "Dispatching job to SGE.\n" 
        if ($do_sge && $SS_r->{GS}{VERBOSE});
    my $cmd_pid;
    if (!defined $return_stdin || !$return_stdin) {
        if (!open(CMD_OUT, "$cmd |")) {
            command_die($SS_r, "Error: Couldn't run $cmd.");
        }
    }
    else {
        $cmd_pid = open2(*CMD_OUT, *CMD_IN, $cmd);
        if (!$cmd_pid) {
            command_die($SS_r, "Error: Couldn't run $cmd.");
        }
        # Fork if caller needs to write to CMD_IN
        my $pid = fork;
        if ($pid) {
            close CMD_OUT;
            return (\*CMD_IN, $cmd_pid, $pid);
        }
    }
    close CMD_IN;
    # dup command's output to standard output if requested
    if ($outfile eq "STDOUT") {
        open OUT, ">&STDOUT";
    }
    else {
        if (!open OUT, ">$outfile") {
            command_die($SS_r, "Error: Couldn't write to $outfile.");
        }
    }
    # Handle command's output
    while (<CMD_OUT>) {
        print OUT;
        print if ($SS_r->{GS}{DEBUG} || defined($echo_stdout) && $echo_stdout);
    }
    close OUT;
    close CMD_OUT;
    exit if (defined $return_stdin && $return_stdin);
}

# Return the command that's currently active (at the top of the command
# stack).  Returns ($mod, $cmd).
sub get_active_command {
    my $SS_r = shift;
    return split /\//, 
        ${$SS_r->{IGV}{cmd_stack}}[$#{$SS_r->{IGV}->{cmd_stack}}];
}

# Convert a dotted fqcn to an absolute file path
sub fqcn_to_file {
    my ($root, $file) = @_;
    $file =~ s/\./\//g;
    $file = "$root/${file}.cast";
    return $file;
}

# Convert a dotted fqcn to an absolute path to the subtype's parent directory
sub fqcn_to_directory {
    my ($root, $dir) = @_;
    $dir =~ s/\.[^\.]+$//;
    $dir =~ s/\./\//g;
    return "$root/$dir";
}

# Sets the dirty bits of all specified cells.  If an inconsistency with
# the cached cell map is detected (i.e. a specified cell does not exist)
# the global variable DIRTY is set to 1.  Note this provides a simple
# way to set the global DIRTY state (pass a value such as "global" or "all").
sub set_dirty {
    my $SS_r = shift;
    while (defined (my $fqcn = shift)) {
        if (exists $SS_r->{GM}{DIRTY_CELL_MAP}{$fqcn}) {
            $SS_r->{GM}{DIRTY_CELL_MAP}{$fqcn} = [ $TYPE_SCALAR, 1 ];
        }
        else {
            $SS_r->{GS}{DIRTY} = 1;
        }
    }
    # TEMPORARY (until incremental reparsing support is available)
    $SS_r->{GS}{DIRTY} = 1;
}


# Underlines $str by surrounding it with the appropriate ANSI control 
# characters.  Unfortunately Text::Wrap doesn't realize that these
# characters aren't displayed, so the formatting can get a bit messed up.
sub underline {
    my $str = shift;
    return color('underline') . $str . color('reset');
}

# Makes $str bold by surrounding it with the appropriate ANSI control 
# characters.
sub bold {
    my $str = shift;
    return color('bold') . $str . color('reset');
}


# This module only adds global variables to the main Supersize module
sub get_module_data {
    return { 
        NAME => "",
        DESC => "Supersize internal utilities.",
        GLOBALS => $supersize_system_globals_r
    };
}

# Looks up variable pdk_var in pdk file pdk_file.  For example,
# pdk_file = jauto/process.config pdk_var = technologyName.
#
# Note the file is actually referenced to PDK_ROOT/share/Fulcrum.
# Returns the exact string the variable is set to (so comma-separated
# list variables will need to be split by the invoking code).  Returns
# undef if the variable wasn't found.  Descends into any nested files
# of pdk_file.
sub get_pdk_variable {
    my $SS_r     = shift;
    my $pdk_file = shift;
    my $pdk_var  = shift;
    my $pdk_val;

    if (!exists $SS_r->{GS}{PDK_ROOT}) {
        command_die($SS_r, "Variable PDK_ROOT isn't set.");
    }
    my $file = ($pdk_file =~ /^\//) ? $pdk_file :
        "$SS_r->{GS}{PDK_ROOT}/share/Fulcrum/$pdk_file";
    local *PDK;
    open (PDK, $file) || command_die($SS_r, "Couldn't read $file.");
    while (<PDK>) {
        chomp; s/\s*$//;
        if (/^$pdk_var=(.*)$/) {
            $pdk_val = $1;
        }
        if (/^config=(.*)$/) {
            my $nested_val = get_pdk_variable($SS_r, $1, $pdk_var);
            $pdk_val = $nested_val if (defined $nested_val);
        }
    }
    close PDK;
    return $pdk_val;
}

1;
