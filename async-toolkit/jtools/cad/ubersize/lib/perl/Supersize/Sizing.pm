#
# Sizing
#
# This it, this is what it's all about.
#
#

package Supersize::Sizing;

BEGIN {
    use Exporter;
    our @ISA = qw(Exporter);
    our @EXPORT = qw(
        &size_common
    );
    our @EXPORT_OK = qw( 
        &get_module_data
    );
}

use strict;
use Supersize::DensityFactor;
use Supersize::JavaUtil;
use Supersize::LayoutIntegration;
use Supersize::ModifySubtypes;
use Supersize::SizingDebug;
use Supersize::TypeUtil;
use Supersize::Util;

my $SIZING_GLOBALS_r = { 
    TAU      => { 
      REQUIRED => 1,
      DESC     => "Tau (in picoseconds) at which to size the circuit." },
    TOP      => { REQUIRED => 1 },
    WORK_DIR => { REQUIRED => 1 },
    CAST_DIR => { REQUIRED => 1 },
    SPEC_DIR => { REQUIRED => 1 },
    PDK_ROOT => { REQUIRED => 1 },
    ROUTED   => { REQUIRED => 0 },
};

sub get_module_data {
  return { 
    NAME => "Sizing",
    DESC => "Commands for sizing transistors.",
    COMMANDS => {
    "size"    => {
        SUBREF => \&size,
        USAGE  => "size [--no-floorplan] [--quit-after-setup] ".
                       "[--skip-density-factor-update] ".
                       "[--generate-delay-signoff] " .
                       "[--all-fixed-size] " .
                       "[--extra-jauto-args]",
        DESC   =>
          "Sizes TOP at TAU ps using the floorplanning from DFII_DIR.  If " .
          "--no-geometry is specified, then the command is equivalent to " .
          "ubersize's presize action (geometry will be ignored and a " .
          "default wire length will be applied to each net.)  If the " .
          "--quit-after-setup option is specified, Jauto will quit after " .
          "generating the wires.debug and the delaybias.debug files (see " .
          "extended help for details on the latter).\n\n".
          "One very important difference between Supersize's \"size\" and ".
          "ubersize's \"size\" commands is in their handling of the ".
          "auto_layout directive.  This command will update the ".
          "density_factor directives of all cells with auto_layout==1 to ".
          "predicted GLC values.  The --skip-density-factor-update ".
          "argument disables this behavior.\n\n".
          "If --generate-delay-signoff is specified, then Jauto will emit ".
          "a file that contains the delays of fixed size paths; later, this ".
          "file can be read in again (by setting the use_delay_signoff ".
          "variable) to override the calculated delay budget.  This is ".
          "useful to suppress irrelevant delay violations on fully fixed ".
          "size paths.\n\n".
          "The --all-fixed-size option will cause Jauto to treat all subtypes ".
          "as if their fixed_size directives were set to true.  This causes ".
          "all SBUF/RBUF channels (or, in general, any wires constrained by ".
          "the min_wirelength directive) to be sized with their actual ".
          "floorplanned wire lengths.\n\n".
          "The --extra-jauto-args option is used to pass arbitrary additional ".
          "arguments to Jauto.  All arguments specified after this option " .
          "will be passed directly to Jauto without further processing.",
        GLOBALS => combine_hashes({
          DFII_DIR => { REQUIRED => 1 },
          IC       => { REQUIRED => 1 },
          FLOORPLAN_VIEW => { 
            REQUIRED => 0,
            DESC => "DFII view name of floorplan hierarchy.  If not set, ".
                    "defaults to 'floorplan'." } },
          $SIZING_GLOBALS_r),
        IV => {
          fixed_size_delaybias => {
            TYPE => $TYPE_SCALAR,
            DESC => "Delaybias to apply to all fixed-size cells in the ".
                    "unit (use with care).  Recommended value: 1.0.",
            REQUIRED => 0 },
          sizable_delaybias => {
            TYPE => $TYPE_SCALAR,
            DESC => "Delaybias to apply to all sizable cells in the ".
                    "unit (use with care).  Recommended value: 0.9.",
            REQUIRED => 0 },
          preserve_density_factors => {
            TYPE => $TYPE_SCALAR,
            DESC => "If set to 1, the density_factor directives of any " .
                    "cells that have auto_layout==1 will be restored to ".
                    "their pre-sizing values.",
            REQUIRED => 0 },
          debug_delaybias => {
            TYPE => $TYPE_SCALAR,
            DESC => "When set to 1, will cause Jauto to generate " .
                    "delaybias.debug, which records the instance-based " .
                    "delay bias value applied to every instance under " .
                    "TOP.  For large designs, this file can become very " .
                    "large so the default is 0." },
          solve_hours => {
            TYPE => $TYPE_SCALAR,
            DESC => "Sets the minimum time (in hours) to spend in the CG ".
                    "solver even if convergence conditions are met. ".
                    "The default is disabled, intended for quick sizing ".
                    "iterations.  Andrew recommends you set this to 0.25 on ".
                    "your final sizing run." },
          max_solve_hours => {
            TYPE => $TYPE_SCALAR,
            DESC => "Sets the maximum time (in hours) to spend in the CG ".
                    "solver.  Default is disabled.  Set for quicker trials." },
          use_delay_signoff => {
            TYPE => $TYPE_SCALAR,
            DESC => "When set to 1, will cause Jauto to read in a file " .
                    "containing fixed size path delays, and override " .
                    "the calculated delay budget.  The file to read is " .
                    "specified by the \"delay_signoff_file\" variable.  " .
                    "The default is 0." },
          delay_signoff_file => {
            TYPE => $TYPE_SCALAR,
            DESC => "Specifies the name of the file that contains the " .
                    "delay budget of fixed size paths.  The default value " .
                    "is \"delay_signoff.debug\".  Set this variable to use " .
                    "an alternate file." } } },
    "presize" => {
        SUBREF => \&size,
        USAGE  => "presize",
        DESC   => 
          "Sizes TOP at TAU ps without using any floorplanning placement ".
          "information.  This command is a degenerate application of the " .
          "\"size\" command (namely \"size --no-floorplan\"); all of that " .
          "command's arguments and input/output variables apply to this " .
          "one. (The latter are accessed under the \"size\" command " .
          "space).",
        GLOBALS => $SIZING_GLOBALS_r }
    }
  };
}

#
# size
#
sub size {
    my $SS_r = shift;
    my $GS_r = $SS_r->{GS};

    # Parse arguments
    my $quit_after_setup = 0;
    my $update_df = 1;
    my $use_floorplan = (get_active_command($SS_r))[1] eq "size" ? 1 : 0;
    my $generate_delay_signoff = 0;
    my $all_fixed_size = 0;
    my @extra_jauto_args;
    while (num_args(\@_)) {
        my $arg = shift_next_scalar(\@_);
        if ($arg eq "--quit-after-setup") {
            $quit_after_setup = 1;
        }
        elsif ($arg eq "--no-floorplan") {
            $use_floorplan = 0;
        }
        elsif ($arg eq "--skip-density-factor-update") {
            $update_df = 0;
        }
        elsif ($arg eq "--generate-delay-signoff") {
            $generate_delay_signoff = 1;
        }
        elsif ($arg eq "--all-fixed-size") {
            $all_fixed_size = 1;
        }
        elsif ($arg eq "--extra-jauto-args") {
            push @extra_jauto_args, shift_next_scalar(\@_)
                while (num_args(\@_));
        }
        else {
            command_die($SS_r, "Unknown argument '$arg'.");
        }
    }

    # Set up command and run Jauto
    my $cmd_scope = "Sizing/size";
    my $err = size_common($SS_r, get_work_dir($SS_r), $SS_r->{GS}{TOP}, 
        { use_floorplan          => $use_floorplan,
          update_density_factor  => $update_df,
          quit_after_setup       => $quit_after_setup,
          generate_delay_signoff => $generate_delay_signoff,
          all_fixed_size         => $all_fixed_size,
          extra_jauto_args       => \@extra_jauto_args,
          delaybias_debug        => 
            get_cmd_input_scalar($SS_r, "debug_delaybias", $cmd_scope),
          use_delay_signoff      => 
            get_cmd_input_scalar($SS_r, "use_delay_signoff", $cmd_scope),
          delay_signoff_file     => 
            get_cmd_input_scalar($SS_r, "delay_signoff_file", $cmd_scope),
          fixed_size_delaybias   => 
            get_cmd_input_scalar($SS_r, "fixed_size_delaybias", $cmd_scope),
          sizable_delaybias      => 
            get_cmd_input_scalar($SS_r, "sizable_delaybias", $cmd_scope),
          solve_hours            => 
            get_cmd_input_scalar($SS_r, "solve_hours", $cmd_scope),
          max_solve_hours        => 
            get_cmd_input_scalar($SS_r, "max_solve_hours", $cmd_scope),
          violations_debug       => 1
        });

    command_die($SS_r, "Error: Jauto failed.") if ($err);
}


#
# Common code for sizing
# Returns 1 if Jauto exited abnormally, 0 otherwise.
#
sub size_common {
    my $SS_r                   = shift;
    my $wdir                   = shift;
    my $fqcn                   = shift;
    my $opts                   = shift;

    my $use_floorplan          = $opts->{use_floorplan};
    my $fixed_size_delaybias   = $opts->{fixed_size_delaybias};
    my $sizable_delaybias      = $opts->{sizable_delaybias};
    # update df's of auto_layout==1 cells
    my $update_df              = $opts->{update_density_factor};
    my $debug_delaybias        = $opts->{debug_delaybias};
    # set undef or 0 for default behavior
    my $solve_hours            = $opts->{solve_hours};
    my $max_solve_hours        = $opts->{max_solve_hours};
    my $quit_after_setup       = $opts->{quit_after_setup};
    # treats all cells as fixed_size=true
    my $all_fixed_size         = $opts->{all_fixed_size};
    # list of additional arguments
    my $extra_jauto_args_r     = $opts->{extra_jauto_args};
    my $generate_delay_signoff = $opts->{generate_delay_signoff};
    my $use_delay_signoff      = $opts->{use_delay_signoff};
    my $delay_signoff_file     = $opts->{delay_signoff_file};
    my $violations_debug       = $opts->{violations_debug};
    # options not exposed to user, and only used by MergeLibrary
    my $use_existing_instances = $opts->{use_existing_instances};
    my $dont_clear_df          = $opts->{dont_clear_density_factor};

    my $GS_r = $SS_r->{GS};
    my $view = exists $GS_r->{FLOORPLAN_VIEW} ? 
                $GS_r->{FLOORPLAN_VIEW} : "floorplan";

    # Clear density factor directives in auto_layout==0 cells so they
    # are filled in with the correct values.
    clear_manual_flow_density_factors($SS_r, $wdir, $fqcn)
        unless defined($dont_clear_df);

    # Override dirty state since removing density factors can't have
    # any effect on sizing.
    $SS_r->{GS}{DIRTY} = 0;

    # Construct cast_path.
    my $cast_path = "$GS_r->{CAST_DIR}:$wdir/cast:$GS_r->{SPEC_DIR}";

    # Directory setup
    if (! -e "$wdir/cast") {
        command_die($SS_r, "Cast subtype hierarchy doesn't yet exist. Can't ".
                           "size (run create_subtypes first.)");
    }
    if ($GS_r->{SAVE_BACKUPS} && !$GS_r->{JUST_PRINT}) {
        if (-e "$wdir/cast.save") {
            print "Warning: Deleting stale $wdir/cast.save.\n" 
                if ($GS_r->{VERBOSE});
            `rm -rf "$wdir/cast.save"`;
        }
        `cp -a "$wdir/cast" "$wdir/cast.save"`;
    }

    # Get floorplan "instances"
    my $instances_dir = "$wdir/instances";
    if ($use_floorplan && !defined($use_existing_instances)) {
        print "Processing floorplan.\n";
        extract_dfII_instances($SS_r, $fqcn, $view, $wdir);
    }
        
    # Determine base cell and subtype
    my $idx = rindex $fqcn, ".";
    if ($idx == -1 || $idx == length($fqcn)-1) {
        command_die($SS_r, "Invalid TOP cell name $fqcn.");
        return;
    }
    my $cell    = substr $fqcn, 0, $idx;
    my $subtype = substr $fqcn, $idx+1;

    # Command setup
    my ($cmd, $use_server) = set_java_cmd($SS_r, "jauto", $MAJOR_JOB);
    return if ($cmd eq "");
    $cmd .= "--cdlRoot='$wdir' \\\n";
    $cmd .= "--spice-output=\"${fqcn}.spice\" \\\n";
    $cmd .= "--outRoot='$wdir' \\\n";
    $cmd .= "--subtypePath='$wdir/cast' \\\n";
    $cmd .= "--castInRoot='$cast_path' \\\n";
    $cmd .= "--cellName='$cell' \\\n";
    $cmd .= "--subtype=$subtype \\\n";
    $cmd .= "--tau=$GS_r->{TAU}e-12 \\\n";
    $cmd .= "--mode=size \\\n";
    $cmd .= "--trial \\\n";
    $cmd .= "--layoutRoot=\"$instances_dir\" \\\n" if ($use_floorplan);
    $cmd .= "--noFloorplan \\\n" if (!$use_floorplan);
    $cmd .= "--min-solve-hours=$solve_hours \\\n" if (defined $solve_hours);
    $cmd .= "--max-solve-hours=$max_solve_hours \\\n" if (defined $max_solve_hours);
    if (defined $fixed_size_delaybias) {
        $cmd .= "--fixedSizeDelayBias=$fixed_size_delaybias \\\n";
    }
    if (defined $sizable_delaybias) {
        $cmd .= "--sizableDelayBias=$sizable_delaybias \\\n";
    }
    if (defined $violations_debug && $violations_debug == 1) {
        $fixed_size_delaybias = 1.0 if (!defined $fixed_size_delaybias);
        $cmd .= "--violationReportLimit=$fixed_size_delaybias \\\n";
    }
    $cmd .= "--debugDelayBias \\\n" if ($debug_delaybias);
    $cmd .= "--exitAfterSetup \\\n" if ($quit_after_setup);
    $cmd .= "--allFixedSize \\\n" if ($all_fixed_size);
    $cmd .= "--generateDelaySignoff \\\n" if ($generate_delay_signoff);
    if ($use_delay_signoff) {
        $delay_signoff_file = "$wdir/delay_signoff.debug"
            unless $delay_signoff_file;
        $cmd .= "--delaySignoffFile='$delay_signoff_file' \\\n";
    }

    if (defined $extra_jauto_args_r) {
        foreach my $arg (@{$extra_jauto_args_r}) {
            $cmd .= $arg . " \\\n";
        }
    }

    # Run Jauto
    my $err = 0;
    if ($use_server) {
        $err = cast_server_system($SS_r, $cmd, -1, $wdir);
    }
    else {
        my $outfile = "$wdir/" . (get_active_command($SS_r))[1] . ".out";
        supersize_system($SS_r, $cmd, $MAJOR_JOB, { LS_COLORS => "" }, 
                         $outfile);
        $err = !$?;
    }

    # Generate sizing report -- do this before dirtying the cast so
    # it's printed immediately (fixed/fragment state doesn't change during
    # sizing, so the old cast is good enough)
    print generate_sizing_report($SS_r, $wdir, "Sizing Report");

    # Create some useful additional files based on jauto's output files
    generate_derived_files($wdir, (get_active_command($SS_r))[1]);

    # Need to dirty all subtypes so the density_factor estimation uses
    # the latest sizes.
    set_dirty($SS_r, "all");

    # Set GLC density factors of auto_layout==1 cells
    if ($update_df) {
        update_glc_density_factors($SS_r, $wdir);
    }

    # Return pass/fail return value
    return $err!=0;
}


1;
