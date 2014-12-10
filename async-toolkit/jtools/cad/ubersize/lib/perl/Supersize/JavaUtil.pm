#
# JavaUtil
#
# Supersize module containing utility functions for Java interfacing
#
#

package Supersize::JavaUtil;

BEGIN {
    use Exporter;
    our @ISA = qw(Exporter);
    our @EXPORT = qw( 
        &set_java_cmd
        &initialize_cast_server
        &kill_cast_server
        &query_cast_server
        &run_cast_server_command
        &cast_server_system
        &refresh_cast_server
        &set_server_cast_path
        &set_server_merge_dir
        &unset_server_cast_path
        &get_cast_defines
        &get_cast_path
        &get_routed_option
    );
    our @EXPORT_OK = qw(
        &get_module_data
    );
}

use strict;
use Supersize::Util;
use Supersize::TypeUtil;
use FileHandle;
use IPC::Open3;
use IO::Select;

our $java_globals_r = {
    CUSTOM_CONFIG => { 
        DESC => "Custom --config file to pass to any Cast java program." },
    JRE => { 
        DESC => "If set, will override the default JRE." }
};

#
# Generates the base shell command for running the specified java tool.
# Requires various global variables:
#
#   PACKAGE_ROOT    - Package root directory
#   PDK_ROOT        - PDK root directory
#   CUSTOM_CONFIG   - Custom java --config file [optional]
#   MEM             - Heap size [optional]
#   JRE             - JRE Override [optional]
#
# In the case that this java command will be dispatched to SGE, the
# following additional variables come into play:
#
#   SGE_LEVEL           - Current SGE run level
#   SGE_ARCH::level     - Architecture to which qsub jobs will be dispatched
#   SGE_PACKAGE_ROOT::arch - Package root for each architecture.
#   SGE_MEM::level      - Heap size to use
#
# The argument $run_level determines whether this command will be dispatched
# to SGE; if run_level <= SGE_LEVEL, it's assumed the job will be dispatched
# to a machine of SGE_ARCH::run_level
#
# A $use_server value is returned (0/1).  When set to 1, the command should
# be given to the java cast server (run_cast_server_command); otherwise it
# should be given to supersize_system.
#
sub set_java_cmd {
    my ($SS_r, $tool, $runlevel, $use_server) = @_;
    $use_server = 1 if (!defined $use_server);

    # tools that don't support the CastFileServer
    my %unsupported_server_tools = (
        cast2skill => 1,
        jlvs => 1,
        subtype_merge => 1,
        cast2cdl => 1
    );

    # SGE-dependent values
    my $sge = (defined $SS_r->{GS}{SGE_LEVEL} && 
               $runlevel <= $SS_r->{GS}{SGE_LEVEL});
    my $mem = $SS_r->{GS}{MEM};

    # Set mem if using SGE
    if ($sge && is_scalar($SS_r, "SGE_MEM::$runlevel")) {
        $mem = dereference_scalar($SS_r, "SGE_MEM::$runlevel");
    }

    # Make sure everything was defined correctly
    if (!defined $mem) {
        command_die($SS_r, "MEM or SGE_MEM::$runlevel needs to be set.");
    }

    # Don't use the cast server if the command is to be sent to SGE or if
    # the tool isn't supported
    if ($sge || exists $unsupported_server_tools{$tool}) {
        $use_server = 0;
    }

    # Common pdk files
    my $process_config = 
        "$SS_r->{GS}{PDK_ROOT}/share/Fulcrum/jauto/process.config";
    if (!defined $process_config) {
        command_die($SS_r, "Required variable PDK_ROOT isn't set.");
    }

    # Basic command setup
    my $cmd;
    ($cmd, $use_server) = path_to_tool($SS_r, $tool, $runlevel, 
                                       $use_server);

    # assume all tools supported --routed, set if specified
    $cmd .= " " . get_routed_option($SS_r) . " \\\n";

    # Common Java arguments
    if (!$use_server) {
        $cmd .= " --script-verbose \\\n" if ($SS_r->{GS}{DEBUG});
        $cmd .= " --max-heap-size=$mem \\\n";
        $cmd .= " --jre=$SS_r->{GS}{JRE} \\\n" 
                unless (!$SS_r->{GS}{JRE});
        $cmd .= " --jre-args=$SS_r->{GS}{JRE_ARGS} \\\n" 
                unless (!$SS_r->{GS}{JRE_ARGS});
    }
    else {
        # set in wrapper script
        $cmd .= " --package-root=$SS_r->{GS}{PACKAGE_ROOT} \\\n";
    }
    $cmd .= get_cast_defines($SS_r) . "\\\n";
    
    # Tool-specific arguments
    if ($tool eq "jauto") {
        my $jauto_config = 
            "$SS_r->{GS}{PDK_ROOT}/share/Fulcrum/jauto/jauto.config";
        $cmd .= " --config=$jauto_config \\\n";
        $cmd .= " --config=$process_config \\\n";
        $cmd .= " --config=$SS_r->{GS}{CUSTOM_CONFIG} \\\n"
                unless (!$SS_r->{GS}{CUSTOM_CONFIG});
        $cmd .= " --solver=useJCGSolver \\\n";
    }
    elsif ($tool eq "jlvs") {
        my $gates_stacks_config =
            "$SS_r->{GS}{PDK_ROOT}/share/Fulcrum/jlvs/gates_stacks.config";
    }

    return ($cmd, $use_server);
}

our $SERVER_PID = -1;
our $SEL;
our $SERVER_CAST_PATH;
our $SERVER_CAST_PATH_OVERRIDE;
our $SERVER_ERROR = 0;

#
# Initialize CastFileServer interface
#
sub initialize_cast_server {
    my $SS_r = shift;

    if ($SERVER_PID != -1) {
        terminate_cast_server("Restarting Java cast server.\n");
    }
    else {
        print STDERR "Initializing Java cast server.\n";
    }
    $SERVER_CAST_PATH = defined $SERVER_CAST_PATH_OVERRIDE ? 
        $SERVER_CAST_PATH_OVERRIDE : get_cast_path($SS_r);
    my $cmd = "$SS_r->{GS}{PACKAGE_ROOT}/bin/cast_file_server ";

    my $mem = defined $SS_r->{GS}{SERVER_MEM} ? $SS_r->{GS}{SERVER_MEM} :
              defined $SS_r->{GS}{LOCAL_MEM} ? $SS_r->{GS}{LOCAL_MEM} :
              $SS_r->{GS}{MEM};
    command_die($SS_r, "SERVER_MEM or MEM must be set in order to run the " .
                       "Java cast server.") if (!defined $mem);
    my ($enough_free, $free_mem) = check_free_system_memory($mem);
    if (!$enough_free) {
        command_die($SS_r, "Aborting Java due to insufficient free system ".
            "memory.\n" .
            "    SERVER_MEM = $mem\n" .
            "    Available  = $free_mem");
    }
    $cmd .= " --max-heap-size=$mem \\\n";
    $cmd .= " --jre=$SS_r->{GS}{JRE} \\\n" 
            unless (!$SS_r->{GS}{JRE});
    $cmd .= " --jre-args=$SS_r->{GS}{JRE_ARGS} \\\n" 
                unless (!$SS_r->{GS}{JRE_ARGS});
    $cmd .= "'--cast-path=$SERVER_CAST_PATH' ";
    $cmd .= get_cast_defines($SS_r);

    print STDERR "  Running $cmd\n" if ($SS_r->{GS}{DEBUG});
    $SERVER_PID = open3(*In, *Out, *Err, $cmd);
    $SERVER_ERROR = 0;
    #In->autoflush(1);
    $SEL = new IO::Select();
    $SEL->add(*Out);
    $SEL->add(*Err);
}

#
# Kill the cast server (to free system memory)
#
sub kill_cast_server {
    my $SS_r = shift;
    terminate_cast_server("Terminating Java cast server.\n");
}

#
# Terminate the cast server
#
sub terminate_cast_server {
    if ($SERVER_PID != -1) {
        my $msg = shift;
        print STDERR $msg if defined($msg);
        close In;
        close Out;
        close Err;
        kill "KILL", $SERVER_PID;
        sleep 1;
        waitpid $SERVER_PID, 0;
        $SERVER_PID = -1;
    }
}

#
# Issue a CastQuery command to the CastFileServer
#
sub query_cast_server {
    my ($SS_r, $query_cmd, $output_ref, $be_quiet, $output_handler,
        $dont_die, $err_mode) = @_;
    my $routed = get_routed_option($SS_r);
    $query_cmd = $routed . " " . $query_cmd;
    my $status = run_cast_server_command($SS_r, 
        "com.avlsi.tools.jauto.CastQuery $query_cmd", 
        ($be_quiet ? undef : \*STDOUT), undef,
        (defined($err_mode) ? $err_mode : -1), $output_ref,
        $output_handler);
    if ($status == -2 && !$dont_die) {
        command_die($SS_r, "cast_query $query_cmd failed");
    }
    return $status;
}

#
# Run an arbitrary Java class command line using the CastFileServer 
# If be_quiet is set, then no output is printed whatsoever except 
# fatal errors.  Otherwise, the Java class' stdout and stderr is 
# printed (stderr is collected and presented on completion).  
# Returns one of three values:
#
#    0 - No problems
#   -1 - Unrecognized output to stderr, but not an exception
#   -2 - Exception, IPC problems, class termination, etc.
#
# If $out_fh or $err_fh are defined, then the command's standard
# output and error will be written to the respective file handle.
#
# $err_mode: If -1, the command's standard error output will be printed
# to our standard error (in addition to writing it to $err_fh, if 
# specified).  If >=0, this number of lines will be saved and reported on 
# command completion in an error condition (Note: 0 is an interesting case).
#
# Pushes all "trimmed" stdout lines to @{$output_ref}, if it's non-undef.  
# ("Trimmed" in the sense that preceding & trailing whitespace is removed
# and empty lines are filtered.)  If output_handler is defined, it will be
# invoked on each complete stdout/stderr line as
#
#   &{$output_handler}(0, stdout_line)
#   &{$output_handler}(1, stderr_line)
#
# In this case, there is no filtering or trimming of the command output.
#
sub run_cast_server_command {
    my $SS_r        = shift;
    my $cmd         = shift;
    my $out_fh      = shift;        # standard output filehandle (can be undef)
    my $err_fh      = shift;        # standard error filehandle (can be undef)
    my $err_mode    = shift;        # See above
    my $output_ref  = shift;        # optional
    my $output_handler = shift;     # optional

    # remove shell line extensions from command
    $cmd =~ s/\\\n//sg;
    $cmd =~ s/['"]//g;
    $cmd =~ s/\s+/ /g;

    refresh_cast_server($SS_r, 0);
    if ($SS_r->{GS}{DEBUG}) {
        print STDERR "Issuing command to Java cast server:\n";
        print STDERR "  $cmd\n";
    }
    print In "- - $cmd\n";
    my $class = (split /\s+/, $cmd)[0];
    my $done = 0;
    my @ready;
    my $out_data;
    my $err_data;
    my @saved_err_lines;
    my $out_line = "";
    my $err_line = "";
    my $return_code = 0;
    my $select_timeout;
    $select_timeout = $SS_r->{GS}{JAVA_SERVER_TIMEOUT} 
        if (defined $SS_r->{GS}{JAVA_SERVER_TIMEOUT});
    while (!$done && (@ready = $SEL->can_read($select_timeout))) {
        foreach my $fh (@ready) {
            if ($fh eq *Out) {
                if (!sysread Out, $out_data, 4096) {
                    print STDERR "Warning: Java cast server closed stdout.\n";
                    $return_code = -2;
                    #$done = 1;
                }
                my @lines = split /\n/, $out_data, -1;
                my $leftover = pop @lines;
                while (@lines) {
                    $out_line .= shift @lines;
                    if (defined $output_handler) {
                        &{$output_handler}(0,$out_line);
                    }
                    print $out_fh "$out_line\n" if (defined $out_fh);
                    $out_line =~ s/\s*$//; $out_line =~ s/^\s*//;
                    if ($out_line !~ /^$/ && defined $output_ref) {
                        push @{$output_ref}, $out_line;
                    }
                    $out_line = ""; $out_data = "";
                }
                $out_line .= $leftover if ($leftover);
            }
            elsif ($fh eq *Err) {
                if (!sysread Err, $err_data, 80) {
                    print STDERR "Warning: Java cast server closed stderr.\n";
                    $return_code = -2;
                    $done = 1;
                }
                my @lines = split /\n/, $err_data, -1;
                my $leftover = pop @lines;
                while (@lines) {
                    $err_line .= shift @lines;
                    if ($err_line !~ /^CastFileServer:/) {
                        if (defined $output_handler) {
                            &{$output_handler}(1,$err_line);
                        }
                        if ($err_mode==-1) {
                            print STDERR "$err_line\n";
                        }
                        elsif (@saved_err_lines < $err_mode) {
                            push @saved_err_lines, $err_line;
                        }
                        print $err_fh "$err_line\n" if (defined $err_fh);
                    }
                    elsif ($err_line eq "CastFileServer: EXCEPTION") {
                        $return_code = -2;
                        if ($SS_r->{GS}{DEBUG}) {
                            print STDERR "---------------------------\n";
                            print STDERR "Error running\n$cmd\n";
                            print STDERR "---------------------------\n";
                        }
                        else {
                            print STDERR "Error running $class:\n" 
                        }
                    }
                    elsif ($err_line =~ /^CastFileServer: EXIT (.*)$/) {
                        print STDERR "Warning: $class exited with return ".
                            "code $1 \n" if ($SS_r->{GS}{VERBOSE});
                        $return_code = -2 if ($1 != 0);
                    }
                    elsif ($err_line eq "CastFileServer: SUCCESS" && 
                           $SS_r->{GS}{DEBUG}) {
                        print STDERR "Succefully ran $cmd.\n";
                    }
                    elsif ($err_line eq "CastFileServer: READY") {
                        $done = 1;
                    }
                    print STDERR "ERR: $err_line\n" if ($SS_r->{GS}{DEBUG});
                    $err_line = ""; $err_data = "";
                }
                $err_line .= $leftover if ($leftover);
            }
        }
    }
    $return_code = -2 if (!$done);
    # Drain stdout (not usually necessary)
    while ($SEL->can_read(0)) {
        #print "Draining stdout...\n";
        last if (!sysread Out, $out_data, 4096);
        my @lines = split /\n/, $out_data, -1;
        my $leftover = pop @lines;
        while (@lines) {
            $out_line .= shift @lines;
            if (defined $output_handler) {
                &{$output_handler}(0,$out_line);
            }
            print $out_fh "$out_line\n" if (defined $out_fh);
            $out_line =~ s/\s*$//; $out_line =~ s/^\s*//;
            if ($out_line !~ /^$/ && defined $output_ref) {
                push @{$output_ref}, $out_line;
            }
            $out_line = "";
        }
        $out_line .= $leftover;
    }
    foreach my $l (@saved_err_lines) {
        if ($l !~ /^SYNTAX/ && $l !~ /^$/ && $return_code > -2) {
            $return_code = -1;
        }
        print STDERR $l . "\n";
    }
    $SERVER_ERROR = 1 if ($return_code == -2);
    return $return_code;
}

# Interface similar to supersize_system.  Writes standard output and
# standard error to "WORK_DIR/cmd.out" and "WORK_DIR/cmd.err".
# $err_mode is defined as for run_cast_server_command.  Working directory
# (where .out/.err files will be placed) can be overridden from WORK_DIR
# by specifying $work_dir.
# Returns non-zero on error (see run_cast_server_command error codes).
sub cast_server_system {
    my $SS_r     = shift;
    my $cmd      = shift;
    my $err_mode = shift;
    my $work_dir = shift;

    # determing working directory
    #$work_dir = $SS_r->{GS}{WORK_DIR} if (!defined $work_dir);
    $work_dir = get_work_dir($SS_r) if (!defined $work_dir);

    # set up .out .err files
    my $cmd_name = (get_active_command($SS_r))[1];
    my $out_file = "$work_dir/$cmd_name.out";
    my $err_file = "$work_dir/$cmd_name.err";
    open (OUT, ">$out_file") ||
        command_die($SS_r, "Couldn't write to $out_file.");
    open (ERR, ">$err_file") ||
        command_die($SS_r, "Couldn't write to $err_file.");

    my $ret = run_cast_server_command($SS_r, $cmd, \*OUT, \*ERR, $err_mode);

    close OUT;
    close ERR;

    return $ret;
}

#
# Maintain cached cell data structures.  If restart==1, will force a
# restart of the Java cast server in order to ensure the cell data is 
# up-to-date.  Otherwise, restarts only when it feels it needs to
# (based on {GS}->{DIRTY} and cast-path changes).
#
sub refresh_cast_server {
    my $SS_r    = shift;
    my $restart = shift;

    # Conditions for restarting
    if ($SERVER_PID == -1 || $SERVER_ERROR ||
        !defined $SS_r->{GS}{DIRTY} || $SS_r->{GS}{DIRTY} ||
        !defined $SERVER_CAST_PATH ||
        defined $SERVER_CAST_PATH_OVERRIDE &&
            $SERVER_CAST_PATH ne $SERVER_CAST_PATH_OVERRIDE ||
        !defined $SERVER_CAST_PATH_OVERRIDE &&
            $SERVER_CAST_PATH ne get_cast_path($SS_r)) {
        $restart = 1;
    }

    # Start or restart the server
    if ($restart) {
        initialize_cast_server($SS_r);
        $SS_r->{GS}{DIRTY} = 0;
        
        # Construct per-cell dirty map
        $SS_r->{GM}{DIRTY_CELL_MAP} = {};

        return if (!$SS_r->{GS}{TOP});  # Nothing to look up
        #my @cells = ();
        #my $cmd = "--task=subcells --cell=$SS_r->{GS}{TOP}";
        #my $ret = query_cast_server($SS_r, $cmd, \@cells, 1);
        #if ($ret == -2 || !@cells) {
        #    my $msg = "Warning: Could not retrieve cell data for\n" .
        #              "         $SS_r->{GS}{TOP}.";
        #    $msg .=   "\n         You may wish to set AUTO_REFRESH to 0." 
        #        if ($SS_r->{GS}{AUTO_REFRESH});
        #    command_die($SS_r, $msg);
        #}
        #foreach my $cell (@cells) {
        #    $SS_r->{GM}{DIRTY_CELL_MAP}{$cell} = [ $TYPE_SCALAR, 0 ];
        #}
    }
}

# default cast path
sub get_cast_path {
    my $SS_r = shift;
    my $cp = "$SS_r->{GS}{CAST_DIR}:";
    if (exists $SS_r->{GS}{WORK_SUBDIR} && $SS_r->{GS}{WORK_SUBDIR} ne "") {
        # General mechanism to descend into a temporary subtype subdirectory;
        # used only by the merging mechanism currently.
        $cp .= $SS_r->{GS}{WORK_SUBDIR} . "/cast:";
    }
    $cp .= "$SS_r->{GS}{WORK_DIR}/cast:" if ($SS_r->{GS}{WORK_DIR});
    $cp .= "$SS_r->{GS}{SPEC_DIR}";
    return $cp;
}

# other modules may call this if they want to set the cast path to
# something non-standard.  However, the module needs to unset the
# cast path when it's done!
sub set_server_cast_path {
    $SERVER_CAST_PATH_OVERRIDE = shift;
}

# Less general version of set_server_cast_path; sets to a path appropriate in a
# merging context, and includes SPEC_DIR and CAST_DIR as usual.
sub set_server_merge_dir {
    my $SS_r = shift;
    my $wdir = shift;
    $SERVER_CAST_PATH_OVERRIDE = 
        "$SS_r->{GS}{CAST_DIR}:$wdir/cast:".
        ($wdir ne $SS_r->{GS}{WORK_DIR} ? "$SS_r->{GS}{WORK_DIR}/cast:" : '').
        "$SS_r->{GS}{SPEC_DIR}";
}

# Call this when done with using the cast server with non-standard
# directories.
sub unset_server_cast_path {
    $SERVER_CAST_PATH_OVERRIDE = undef;
}

# Sets --max-heap-size, --cast-path arguments in preparation for running
# a java tool.
sub set_java_args {
    my $SS_r = shift;
    my $args = "";
    if (defined $SS_r->{GS}{MEM}) {
        $args = "--max-heap-size=$SS_r->{GS}{MEM} ";
    }
    $args .= "'--cast-path=" . get_cast_path($SS_r) . "'";
    return $args;
}

# Return --define variable:value arguments
sub get_cast_defines {
    my ($SS_r, $no_quote) = @_;
    my $str = "";
    if (exists $SS_r->{GL}{CAST_DEFINES}) {
        foreach my $def (@{$SS_r->{GL}{CAST_DEFINES}}) {
            if ($def !~ /^[\w\d\.,\{\}\(\)-]+:.*$/) {
                print STDERR "Warning: Skipping bad cast define $def.\n";
                next;
            }
            $str .= "'" unless $no_quote;
            $str .= "--define=$def";
            $str .= "'" unless $no_quote;
            $str .= ' ';
        }
    }
    return $str;
}

# Returns --routed if the ROUTED global scalar is set to 1
sub get_routed_option {
    my $SS_r = shift;
    return exists $SS_r->{GS}{ROUTED} && $SS_r->{GS}{ROUTED}==1 ?
        "--routed" : "";
}

# This module only adds global variables to the main Supersize module
sub get_module_data {
    return {
        NAME => "",
        DESC => "Supersize java interfacing utilities.",
        GLOBALS => $java_globals_r
    };
}


1;
