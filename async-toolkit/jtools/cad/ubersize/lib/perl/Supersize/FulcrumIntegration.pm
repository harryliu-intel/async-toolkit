#
# FulcrumIntegration
#
# Interfaces to other Fulcrum packaged tools, such as lve, slacker, proteus.
#

package Supersize::FulcrumIntegration;

BEGIN {
    use Exporter;
    our @ISA = qw(Exporter);
    our @EXPORT_OK = qw( 
        &get_module_data
        &test_job_callback
        &alint_callback
        &lve_callback
    );
}

use strict;
use Supersize::ModifySubtypes;
use Supersize::Util;
use Supersize::TypeUtil;
use Supersize::JavaUtil;
use Supersize::LayoutIntegration;
use Term::ANSIColor;
use Text::Wrap;

sub get_module_data {
  return { 
    NAME => "FulcrumIntegration",
    DESC => "LVE interface commands.",
    COMMANDS => {
      alint => {
        SUBREF => \&alint,
        USAGE  => "alint [lve_args] [".underline("cell-list")."]",
        DESC   =>
          "Tests cells for alint problems.  By default, all ".
          "sizable cells under TOP will be tested.  A specific list of ".
          "cells can by tested by providing ".underline("cell-list").".\n\n".
          "This command is launched as a concurrent supersize job.  As ".
          "long as the lve is running, you will see a \"alint\" marker in your ".
          "prompt.  Once it completes, a brief summary will be displayed.  ".
          "The job state is maintained between supersize sessions, so feel ".
          "free to quit (^D) before the lve is done.",
        IV => {
          num_servers => {
            DESC => "Obsolete -- better to set global LVE_JOBS." } },
        GLOBALS => {
          TOP      => { REQUIRED => 1 },
          WORK_DIR => { REQUIRED => 1 },
          CAST_DIR => { REQUIRED => 1 },
          SPEC_DIR => { REQUIRED => 1 },
          DFII_DIR => { REQUIRED => 0 },
          PDK_ROOT => { REQUIRED => 1 },
          LVE_MEM  => { REQUIRED => 0 },
          LVE_JOBS => { REQUIRED => 0 },
          MEM      => { REQUIRED => 0 } },
        RV      => {
          "failed_cells" => {
            TYPE   => $TYPE_LIST,
            DESC   => "List of cells that failed charge sharing" } },
      },
      generate_lib => {
        SUBREF => \&generate_lib,
        USAGE  => "generate_lib [lve_args] [".underline("cell-list")."]",
        DESC   =>
          "Characterizes timing of cells in lib format By default, the first" .
          "level of routed sizable subcells under TOP are processed.  A specific ".
          "list of cells can be tested by providing ".underline("cell-list").".\n\n",
        GLOBALS => {
          TOP      => { REQUIRED => 1 },
          WORK_DIR => { REQUIRED => 1 },
          CAST_DIR => { REQUIRED => 1 },
          SPEC_DIR => { REQUIRED => 1 },
          DFII_DIR => { REQUIRED => 1 },
          PDK_ROOT => { REQUIRED => 1 },
          LVE_MEM  => { REQUIRED => 0 },
          LVE_JOBS => { REQUIRED => 0 },
          MEM      => { REQUIRED => 0 } },
      },
      lve_todo => {
        SUBREF => \&lve_todo,
        USAGE  => "lve_todo [lve_args] [".underline("cell-list")."]",
        DESC   =>
          "Writes a lve.todo file with SuperSize variables like CAST_DIR, ".
          "DFII_DIR, SPEC_DIR.  Also writes lve_args if any.  Also ".
          "appends ".underline("cell-list")." if specified.\n\n",
        GLOBALS => {
          TOP      => { REQUIRED => 1 },
          WORK_DIR => { REQUIRED => 1 },
          CAST_DIR => { REQUIRED => 1 },
          SPEC_DIR => { REQUIRED => 1 },
          DFII_DIR => { REQUIRED => 1 },
          PDK_ROOT => { REQUIRED => 1 } },
      },
      lve => {
        SUBREF => \&lve,
        USAGE  => "lve [lve_args] [".underline("cell-list")."]",
        DESC   =>
          "Writes a lve.todo file with SuperSize variables like CAST_DIR, ".
          "DFII_DIR, SPEC_DIR.  Also writes lve_args if any.  Also ".
          "appends ".underline("cell-list")." if specified.  Then launches ".
          "lve script with this todo file.\n\n" .
          "This command is launched as a concurrent supersize job.  As ".
          "long as the lve is running, you will see a \"LVE\" marker in your ".
          "prompt.  Once it completes, a brief summary will be displayed.  ".
          "The job state is maintained between supersize sessions, so feel ".
          "free to quit (^D) before the lve is done.",
        GLOBALS => {
          TOP      => { REQUIRED => 1 },
          WORK_DIR => { REQUIRED => 1 },
          CAST_DIR => { REQUIRED => 1 },
          SPEC_DIR => { REQUIRED => 1 },
          DFII_DIR => { REQUIRED => 1 },
          PDK_ROOT => { REQUIRED => 1 } },
      },
      proteus_config => {
        SUBREF => \&proteus_config,
        USAGE  => "proteus_config [proteus_args]",
        DESC   =>
          "Writes a proteus.config file with SuperSize variables like TOP, CAST_DIR, ".
          "DFII_DIR, SPEC_DIR.  Also writes proteus_args.\n\n",
        GLOBALS => {
          TOP      => { REQUIRED => 1 },
          PROTEUS_CELL => { REQUIRED => 0},
          PROTEUS_ABSTRACT => { REQUIRED => 0},
          WORK_DIR => { REQUIRED => 1 },
          CAST_DIR => { REQUIRED => 1 },
          SPEC_DIR => { REQUIRED => 1 },
          DFII_DIR => { REQUIRED => 0 },
          PDK_ROOT => { REQUIRED => 1 },
          LEFDEF_DIR => { REQUIRED => 0 },
          SPAR_DIR => { REQUIRED => 0 } },
      },
      proteus => {
        SUBREF => \&proteus,
        USAGE  => "proteus [proteus_args]",
        DESC   =>
          "Writes a proteus.config file with SuperSize variables like TOP, CAST_DIR, ".
          "DFII_DIR, SPEC_DIR.  Also writes proteus_args.  Then launches proteus script ".
          "with this config file.\n\n",
        GLOBALS => {
          TOP      => { REQUIRED => 1 },
          PROTEUS_CELL => { REQUIRED => 0},
          PROTEUS_ABSTRACT => { REQUIRED => 0},
          WORK_DIR => { REQUIRED => 1 },
          CAST_DIR => { REQUIRED => 1 },
          SPEC_DIR => { REQUIRED => 1 },
          DFII_DIR => { REQUIRED => 0 },
          PDK_ROOT => { REQUIRED => 1 },
          LEFDEF_DIR => { REQUIRED => 0 },
          SPAR_DIR => { REQUIRED => 0 } },
      },
      add_slacker_times => {
        SUBREF => \&add_slacker_times,
        USAGE  => "add_slacker_times [slacker args] [".underline("cell-list")."]",
        DESC   =>
          "Run slacker on the refinement parent of each subtype specified in " .
          underline("cell-list")." then mark each subtype as a slacker_leaf " .
          "and back annotate slacker_time directives generated by slacker.  " .
          "Additional slacker arguments may also be specified on the " .
          "command line.  Note that because inlining is processed " .
          "differently in slacker, the cast file server cannot be used.",
        GLOBALS => {
          WORK_DIR => { REQUIRED => 1 },
          CAST_DIR => { REQUIRED => 1 },
          SPEC_DIR => { REQUIRED => 1 } },
      },
      #test_job => {
      #  SUBREF => \&test_job,
      #  USAGE  => "test_job",
      #  DESC   => "Crap" }
    },
    GLOBALS => {
        LVE_JOBS => {
          DESC => "Number of parallel lve jobs to launch using Grid." },
        LVE_MEM => { 
          DESC => "Amount to pass to lve's --mem argument.  If not set, MEM ".
                  "will be used.  Note that by keeping this value less than ".
                  "1G, you may be able to utilize twice as many compute ".
                  "servers.  If you take care to exclude very large mid-level ".
                  "cells in your cell lists, you should be able fit all lve ".
                  "tasks within 1G." }
    }
  };
}

# alint
sub alint {
    my ($extra_args, $cells_lr) = parse_args(@_);
    my $SS_r = shift;

    # Check for a pending run
    my $job_file = "lve_alint";
    if (job_is_running($SS_r, $job_file)) {
        command_die($SS_r, "A charge sharing run is already in progress.\n" .
            "Support has not yet been implemented for multiple concurrent ".
            "runs.");
    }

    # Determine cell list if one wasn't specified
    if (!@{$cells_lr}) {
        my $cmd = "--cell=$SS_r->{GS}{TOP} --task=subcells --filter=!fixed";
        print "Determining cell list.\n";
        query_cast_server($SS_r, $cmd, $cells_lr, 1);
        if (!@{$cells_lr}) {
            command_die($SS_r, "All cells are fixed, so nothing to test; " .
                "try using an explicit cell list.");
        }
    }
    
    # Create charge-sharing specific todo file
    my $wdir = get_work_dir($SS_r);
    my $todo = "$wdir/alint.todo";
    open(my $fh, ">$todo") || 
        command_die($SS_r, "Couldn't write to $todo: $!");
    print $fh common_lve_todo($SS_r);
    print $fh "--mode=nogeometry\n";
    print $fh "--output-dir=$wdir/alint\n";
    print $fh "--task=alint\n";
    print $fh "--minC=1\n";     # ignore wire cap
    print $fh "--bumpCC=0\n";   # ignore cap-coupling
    print $fh "--delayFast=1\n";# only fast delay scenarios 
    print $fh "--delayTau=8\n"; # only one fast tau for delay
    print $fh "--alint-dynamic-only=1\n";
    print $fh "--summarize=1\n";
    print $fh "$extra_args\n";
    print $fh cell_list_string($cells_lr);
    close $fh;

    # Create the job file (no information needed)
    my $cdir = "$wdir/concurrent";
    mkdir $cdir if (!-e $cdir);
    open(JF, ">$cdir/$job_file") || 
        command_die($SS_r, "Couldn't create $cdir/$job_file.");
    close JF;
    
    # Launch the LVE job
    print "Launching alint.  For job status, refer to:\n";
    print "    $wdir/alint.{out,err}\n";
    my $cmd = (path_to_tool($SS_r, "lve", $MINOR_JOB))[0] .
        " '--include=$todo'";
    launch_job($SS_r, $cmd, $job_file, "alint", 
        "Supersize::FulcrumIntegration::alint_callback",
        $MINOR_JOB, undef, "$wdir/alint.out",
        "$wdir/alint.err");
}

# Charge sharing concurrent job callback.
sub alint_callback {
    my $SS_r = shift;
    my $job_file = shift;
    my $status   = shift;

    # initialize return value
    $SS_r->{RV}{alint} = {};
    my $RV_r = $SS_r->{RV}{alint};

    # Give ultra-brief summary
    my $wdir        = get_work_dir($SS_r);
    my $pass_cnt    = 0;
    my $unknown_cnt = 0;
    my @failures;
    open(TABLE, "$wdir/alint/index.table") ||
        die "Couldn't read LVE's index.table file.\n";
    while (<TABLE>) {
        my @results = split /\s+/, $_;
        my $cell;
        foreach my $r (@results) {
            if ($r =~ /^CELL:=(.*)$/) {
                $cell = $1;
            }
            elsif ($r =~ /^ALINT:=(.*)$/) {
                if ($1 eq "FAIL") {
                    push @failures, $cell;
                }
                elsif ($1 eq "PASS" || $1 eq "NA" || $1 eq "NOT_TESTED") {
                    $pass_cnt++;
                }
                else {
                    $unknown_cnt++;
                }
            }
        }
    }
    close TABLE;
    if (!@failures && $unknown_cnt==0) {
        print "All $pass_cnt cells passed.\n";
    }
    else {
        print "$pass_cnt cells passed, " . scalar(@failures) . " failed.  ".
              "Failing cells:\n";
        foreach my $c (@failures) {
            print "  $c\n";
        }
    }
    print "For a complete report, see\n";
    print "$wdir/alint/index.html\n";

    set_cmd_return_variable($SS_r, "failed_cells", [ $TYPE_LIST, \@failures ],
                            "FulcrumIntegration/alint");
}

# For debugging only!
sub test_job {
    my $SS_r = shift;
    my $wdir = get_work_dir($SS_r);
    print "Launching test job.\n";
    my $cdir = "$wdir/concurrent";
    mkdir $cdir if (!-e $cdir);
    my $cmd = "/home/user/mid/bin/crap";
    open JF, ">$cdir/crap";
    print JF "Info about crap.\n";
    close JF;
    launch_job($SS_r, $cmd, "crap", "alint",
               "Supersize::FulcrumIntegration::test_job_callback", 
               $LOCAL_JOB, undef, 
               "$wdir/test_job.out", "$wdir/test_job.err");
    print "Launched.\n";
}

# debugging callback
sub test_job_callback {
    my $SS_r     = shift;
    my $job_file = shift;
    my $status   = shift;
    my $wdir     = get_work_dir($SS_r);
    print "In $SS_r->{IGV}{concurrent}{$job_file}{ID} callback.\n";
    print "Job file was moved to $job_file.done as expected.\n"
        if (-e "$wdir/concurrent/$job_file.done");
    unlink "$wdir/concurrent/$job_file.done";
}

# generate_lib
sub generate_lib {
    my ($extra_args, $cells_lr) = parse_args(@_);
    my $SS_r = shift;
    my $GS_r = $SS_r->{GS};

    # Default to sizable routed subcells of TOP if cell list wasn't specified
    if (!@{$cells_lr}) {
        command_die($SS_r, "TOP must be defined unless an explicit cell list " .
                    "is specified.") unless defined($GS_r->{'TOP'});
        $cells_lr = get_sizable_routed($SS_r);
        if (!@{$cells_lr}) {
            print "All direct routed subcells of TOP are fixed sized, " .
                "nothing to do.\n";
            return;
        }
    }

    # Create lib.todo file
    my $wdir = get_work_dir($SS_r);
    my $todo = "$wdir/lib.todo";
    my $isfake = $extra_args =~ /--task=fakelib/;
    open(my $fh, ">$todo") || 
        command_die($SS_r, "Couldn't write to $todo: $!");
    print $fh common_lve_todo($SS_r);
    print $fh "--mode=" . ($isfake ? "nogeometry" : "estimated") . "\n";
    print $fh "--output-dir=$wdir/lve\n";
    print $fh "--task=lib\n" unless $isfake;
    print $fh "$extra_args\n";
    print $fh cell_list_string($cells_lr);
    close $fh;

    # Launch the LVE job (not concurrently)
    print "Launching generate_lib.  For job status, refer to:\n";
    print "    $wdir/generate_lib.{out,err}\n";
    my $cmd = (path_to_tool($SS_r, "lve", $MINOR_JOB))[0] .
        " '--include=$todo'";
    supersize_system($SS_r, $cmd, $LOCAL_JOB);
}

# lve_todo
sub lve_todo {
    my ($extra_args, $cells_lr) = parse_args(@_);
    my $SS_r = shift;

    # write lve.todo
    my $wdir = get_work_dir($SS_r);
    my $todo = "$wdir/lve.todo";
    open(my $fh, ">$todo") || 
        command_die($SS_r, "Cannot create $todo: $!");
    print $fh common_lve_todo($SS_r);
    print $fh "--mode=estimated\n";
    print $fh "--output-dir=$wdir/lve\n";
    print $fh "--task=\n"; # clear default tasks
    print $fh "$extra_args\n";
    print $fh cell_list_string($cells_lr);
    close $fh;
    print "Wrote $todo.\n";
}

# lve
sub lve {
    lve_todo(@_);
    my $SS_r = shift;
    my $wdir = get_work_dir($SS_r);

    # check job file
    my $job_file = "lve_job";
    if (job_is_running($SS_r, $job_file)) {
        command_die($SS_r, "An lve job is already in progress.\n" .
            "Support has not yet been implemented for multiple concurrent ".
            "runs.");
    }

    # Create the job file (no information needed)
    my $cdir = "$wdir/concurrent";
    mkdir $cdir if (!-e $cdir);
    open(JF, ">$cdir/$job_file") || 
        command_die($SS_r, "Couldn't create $cdir/$job_file.");
    close JF;

    # Launch the LVE job
    my $todo = "$wdir/lve.todo";
    print "Launching lve.  For job status, refer to:\n";
    print "    $wdir/lve.{out,err}\n";
    my $cmd = (path_to_tool($SS_r, "lve", $MINOR_JOB))[0] .
        " '--include=$todo'";
    launch_job($SS_r, $cmd, $job_file, "LVE", 
               "Supersize::FulcrumIntegration::lve_callback",
               $MINOR_JOB, undef, "$wdir/lve.out","$wdir/lve.err");
}

# called when lve parallel job finishes
sub lve_callback {
    my $SS_r     = shift;
    my $job_file = shift;
    my $status   = shift;
    my $exit_status = shift;
    my $wdir     = get_work_dir($SS_r);
    if ($exit_status) {
        print "LVE failed with non-zero exit status,\n".
              "    inspect $wdir/lve.out\n" .
              "            $wdir/lve.err\n";
        my $error = "$wdir/concurrent/$job_file.error";
        print "            $error\n" if -e $error;
    } else {
        print "LVE completed successfully,\n".
              "    inspect $wdir/lve/index.html.\n";
    }

    my $done = "$wdir/concurrent/$job_file.done";
    unlink $done if -e $done;
}

# proteus_config
sub proteus_config {
    my ($extra_args, $cells_lr) = parse_args(@_);
    my $SS_r = shift;
    my $wdir = get_work_dir($SS_r);
    open(my $fh, "> $wdir/proteus.config") ||
        command_die($SS_r, "Couldn't write proteus.config: $!");
    print $fh "--cast-path=$SS_r->{GS}{CAST_DIR}:$SS_r->{GS}{SPEC_DIR}\n";
    print $fh "--cast-dir=$SS_r->{GS}{CAST_DIR}\n";
    print $fh "--spec-dir=$wdir/cast\n";
    print $fh "--dfII-dir=$SS_r->{GS}{DFII_DIR}\n"
        if exists $SS_r->{GS}{DFII_DIR};
    print $fh "--spar-dir=$SS_r->{GS}{SPAR_DIR}\n"
        if exists $SS_r->{GS}{SPAR_DIR};
    print $fh "--lefdef-dir=$SS_r->{GS}{LEFDEF_DIR}/temp/route/" . to_cadence($SS_r->{GS}{TOP}) . "\n"
        if exists $SS_r->{GS}{LEFDEF_DIR};
    print $fh "--lve-path=$wdir/lve\n";
    print $fh "--output-dir=$wdir/proteus\n";
    print $fh "--scratch-dir=\n";
    print $fh "--base=$SS_r->{GS}{TOP}\n";
    print $fh "--abstract=$SS_r->{GS}{PROTEUS_ABSTRACT}\n"
        if exists $SS_r->{GS}{PROTEUS_ABSTRACT};
    print $fh "--cell=$SS_r->{GS}{PROTEUS_CELL}\n"
        if exists $SS_r->{GS}{PROTEUS_CELL};
    print $fh "--cast-define=" . join('::', @{$SS_r->{GL}{CAST_DEFINES}}) . "\n"
        if exists $SS_r->{GL}{CAST_DEFINES};
    print $fh "--task=spr\n";
    print $fh "$extra_args\n";
    close $fh;
    print "Wrote $wdir/proteus.config.\n";
}

# proteus
sub proteus {
    proteus_config(@_);
    my $SS_r = shift;
    my $wdir = get_work_dir($SS_r);
    my $config = "$wdir/proteus.config";
    print "Launching proteus.  For job status, refer to:\n";
    print "    $wdir/proteus.{out,err}\n";
    my $cmd = (path_to_tool($SS_r, "proteus", $MINOR_JOB))[0] .
        " '--include=$config'";
    supersize_system($SS_r, $cmd, $LOCAL_JOB, undef, undef, undef,
                     $SS_r->{GS}{VERBOSE});
}

sub parse_slacker_result {
    my ($file, $topcell) = @_;
    my $slacker_dirs;
    my $numbuf = 0;
    if (open(my $fh, $file)) {
        local $_;
        my $cell = '';
        $slacker_dirs = {};
        while (<$fh>) {
            if (/^Cell:\s*(\S+)/) {
                $cell = $1;
            } elsif (/\s*num_buffer\((.*)\)\s*=\s*(\S+);/) {
                $numbuf = 1;
            } elsif (/\s*(slacker_\w+)\((.*)\)\s*=\s*(\S+);/) {
                $slacker_dirs->{$1}->{$2} = $3 if ($cell eq $topcell);
            }
        }
        close($fh);
    }
    return ($numbuf, $slacker_dirs);
}

sub add_slacker_times {
    my ($extra_args, $cells_lr) = parse_args(@_);
    my $SS_r = shift;
    my $GS_r = $SS_r->{GS};

    # Default to sizable routed subcells of TOP if cell list wasn't specified
    if (!@{$cells_lr}) {
        command_die($SS_r, "TOP must be defined unless an explicit cell list " .
                           "is specified.") unless defined($GS_r->{'TOP'});
        $cells_lr = get_sizable_routed($SS_r);
        if (!@{$cells_lr}) {
            print "All direct routed subcells of TOP are fixed sized, " .
                  "nothing to do.\n";
            return;
        }
    }

    # find all refinement parents, removing duplicates
    my %parents = ();
    foreach my $cell (@{$cells_lr}) {
        push @{$parents{basetype_of($cell)}}, $cell;
    }

    foreach my $cell (keys %parents) {
        my @result = ();
        my $cmd = "--cell=$cell --task=subcells --no-recurse " .
                  "--filter=\"directive=slacker_leaf:true\"";
        query_cast_server($SS_r, $cmd, \@result, 1);
        delete $parents{$cell} if (grep(/^\Q$cell\E/, @result));
    }

    if (!%parents) {
        print "All candidate cells already annotated with slacker_leaf=true, ".
              "nothing to do.\n";
        return;
    }

    # create slacker run directory
    my $wdir = get_work_dir($SS_r);
    my $rdir = "$wdir/slacker";
    mkdir $rdir if (!-e $rdir);

    my $cast_path = get_cast_path($SS_r);
    my $templ = (path_to_tool($SS_r, "slacker", $MINOR_JOB))[0] . " \\\n";
    $templ .= "--cast-path='$cast_path' \\\n";
    $templ .= "--run-dir='$rdir' \\\n";
    $templ .= "--max-heap-size=$GS_r->{MEM} \\\n" if (defined $GS_r->{MEM});
    $templ .= "$extra_args \\\n" if ($extra_args);

    my $has_errors = 0;
    foreach my $parent (keys %parents) {
        my $log = "$rdir/$parent.log";
        my $err = "$rdir/$parent.err";
        my $out = "$rdir/$parent.out";
        unlink $log, $err, $out;

        my $cmd = $templ;
        $cmd .= "--log-file='$log' \\\n";
        $cmd .= "--cell='$parent' 2>'$err' \\\n";

        my ($cmdin, $cmdpid, $pid) =
            supersize_system($SS_r, $cmd, $MINOR_JOB, {}, $out, 1);
        close ($cmdin);
        waitpid $cmdpid, 0;
        waitpid $pid, 0;

        my $status;
        my ($error, $warning) = (0, 0);

        my @parse_errors = ();
        if (open(my $fh, "$wdir/slacker.out")) {
            push @parse_errors, grep { !/^WARNING:|^SYNTAX/ } <$fh>;
            close($fh);
        }
        if (open(my $fh, "$wdir/slacker.err")) {
            push @parse_errors, grep { !/^WARNING:|^SYNTAX/ } <$fh>;
            close($fh);
        }

        my @slacker_errors = ();
        if (open(my $fh, $log)) {
            push @slacker_errors, grep { /^WARNING|^ERROR/i } <$fh>;
            close($fh);
        }

        my %dirs = ();
        if ($? || @slacker_errors) {
            $error = 1;
        } else {
            my ($numbuf, $slacker_dirs) = parse_slacker_result($log, $parent);
            if (defined($slacker_dirs)) {
                my $slacker_time = $slacker_dirs->{'slacker_time'};
                foreach my $channel (sort keys %{$slacker_time}) {
                    my $time = $slacker_time->{$channel};
                    $dirs{"slacker_time($channel)"} = $time;
                    if ($time != int($time)) {
                        $warning = 1;
                        push @slacker_errors,
                             "$channel has non-integer slacker_time $time\n";
                    }
                }

                foreach my $dir ('slacker_ignore',
                                 'slacker_dont_touch',
                                 'slacker_initial_tokens') {
                    my $vals = $slacker_dirs->{$dir};
                    foreach my $channel (sort keys %{$vals}) {
                        $dirs{"$dir($channel)"} = $vals->{$channel};
                    }
                }

                if ($numbuf) {
                    $warning = 1;
                    push @slacker_errors,
                         "$parent not fully slack matched\n";
                }
            } else {
                $error = 1;
                push @slacker_errors, "unexpected slacker output\n";
            }
        }

        if ($warning || $error) {
            $has_errors = 1;
            print "$parent has " . ($error ? "errors" : "warnings") . "\n";
            print map { "    $_" } (@parse_errors, @slacker_errors)
                if $GS_r->{VERBOSE};
        }
        if (!$error) {
            $dirs{'slacker_leaf'} = 'true';
            foreach my $subtype (@{$parents{$parent}}) {
                get_or_set_directive_in_subtype($SS_r, undef, $subtype, 1,
                                                \%dirs, 'top', 0);
            }
        }
    }

    if ($has_errors) {
        my $help = "For more information on errors and warnings, see files ".
                   "in $rdir. ".underline("cell").".log contains slacker ".
                   "related output; ".underline("cell").".err and ".
                   underline("cell").".out contains exceptions and messages ".
                   "from the parser.";
        print "\n" . wrap('', '', $help) . "\n";
    }
}

########################## Shared Subroutines ###################################

# Create string for header of lve.todo
sub common_lve_todo {
    my $SS_r = shift;

    # determine num_servers
    my $num_servers;
    $num_servers = $SS_r->{GS}{LVE_JOBS} if exists $SS_r->{GS}{LVE_JOBS};
    my $n = get_cmd_input_scalar($SS_r, "num_servers");
    $num_servers = $n if defined $n;
   
    # determine mem value
    my $mem;
    if (exists $SS_r->{GS}{LVE_MEM}) {
        $mem = $SS_r->{GS}{LVE_MEM};
    }
    elsif (exists $SS_r->{GS}{MEM}) {
        $mem = $SS_r->{GS}{MEM};
    }

    # construct a string for an lve.todo file
    my $wdir = get_work_dir($SS_r);
    my $str = "";
    $str .= "--cast-dir=$SS_r->{GS}{CAST_DIR}\n";
    $str .= "--spec-dir=$wdir/cast:$SS_r->{GS}{SPEC_DIR}\n";
    $str .= "--dfII-dir=$SS_r->{GS}{DFII_DIR}\n" if exists $SS_r->{GS}{DFII_DIR};
    if (defined $num_servers && $num_servers > 0) {
        $str .= "--qsub=1\n";
        $str .= "--jobs=$num_servers\n";
    }
    $str .= "--mem=$mem\n" if (defined $mem);
    return $str;
}

# Parse args, to separate extra_args and cell-list
sub parse_args {
    my $SS_r = shift;
    my $cells_lr = [];
    my $extra_args = "";
    while (num_args(\@_)) {
        my ($type, $arg) = shift_next_arg(\@_);
        if ($type == $TYPE_SCALAR) {
            $extra_args .=" " if ($arg =~ /^--/ );
            $extra_args .="$arg";
        }
        elsif ($type == $TYPE_LIST && @{$cells_lr}) {
            command_die($SS_r, "Too many cell lists specified.");
        }
        elsif ($type == $TYPE_LIST) {
            $cells_lr = $arg;
        }
        else {
            command_die($SS_r, "Unrecognized argument specified.");
        }
    }
    return ($extra_args, $cells_lr);
}

# Convert cell list to string
sub cell_list_string {
    my ($cells_lr) = @_;
    my $str = "";
    if (defined @{$cells_lr}) {
        foreach my $c (@{$cells_lr}) {
            $str .= "$c\n";
        }
    }
    return $str;
}


# Get sizable routed subcells under TOP
sub get_sizable_routed {
    my $SS_r = shift;
    my @cells_lr = ();
    my $cmd = "--cell=$SS_r->{GS}{TOP} --task=subcells ".
        "--routed --filter=\"one-level&!fixed\"";
    print "Determining cell list.\n";
    query_cast_server($SS_r, $cmd, \@cells_lr, 1);
    return \@cells_lr;
}

1;
