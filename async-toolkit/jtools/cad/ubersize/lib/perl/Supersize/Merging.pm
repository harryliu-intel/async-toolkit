#
# Merging
#
# Supersize module for finding the optimal merging of a split subtype space
#
#

package Supersize::Merging;

BEGIN {
    use Exporter;
    our @ISA = qw(Exporter);
    our @EXPORT = qw(
        &find_latest_merge_trial
        &get_selected_merge_trial
        &create_next_trial_dir
        &trial_to_merge_dir
        &parse_trials
    );
    our @EXPORT_OK = qw( 
        &get_module_data
    );
}

use strict;
use POSIX;
use Supersize::MergeLibrary;
use Supersize::ModifySubtypes;
use Supersize::Netlist;
use Supersize::Sizing;
use Supersize::LayoutIntegration;
use Supersize::Util;
use Supersize::TypeUtil;
use Supersize::JavaUtil;
use Term::ANSIColor;
use Text::Wrap;

my $MERGE_GLOBALS_r = { 
    TOP      => { REQUIRED => 1 },
    WORK_DIR => { REQUIRED => 1 },
    CAST_DIR => { REQUIRED => 1 },
    SPEC_DIR => { REQUIRED => 1 }
};

sub get_module_data {
  return { 
    NAME => "Merging",
    DESC => "Commands for finding an optimal merging of a split subtype " .
            "space.",
    COMMANDS => {
    "trial_merge" => {
        SUBREF => \&trial_merge,
        USAGE  => "trial_merge [--threshold=" . underline("MT1") .
                                    "[,".underline("MT2").",..]] " .
                              "[--max-width-ratio=" . underline("MWR1") .
                                    "[,".underline("MWR2").",..]] " .
                              "[--max-fixed-width-ratio=" . underline("MFWR1") .
                                    "[,".underline("MFWR2").",..]] " .
                              "[--check-directives=0|1] " .
                              "[--merge-cells=" . underline("MCL1") .
                                    "[,".underline("MCL2").",..]] " .
                              "[--exclude-cells=" . underline("XCL1") .
                                    "[,".underline("XCL2").",..]] " . 
                              "[--complex-filter=" . underline("CF1") .
                                    "[,".underline("CF2").",...]] " .
                              "[--library-cells=" . underline("LCL1") .
                                    "[,".underline("LCL2").",..]]",
        DESC   =>
          "Evaluates one or more merging scenarios, based on the specified ".
          "thresholds and cell lists.  If all threshold and cell list " .
          "arguments are assigned single values, and MERGE_DIR is not ".
          "set, then the following directory and files will be created:\n\n".
          "  WORK_DIR/merging/0/merge.hint\n" .
          "                     README\n\n" .
          "The merge.hint file contains a specification of which cells " .
          "will be merged in this scenario.  The README file contains a " .
          "record of all relevant merging parameters as specified to " .
          "trial_merge.\n\n" .
          "If a subsequent trial_merge command is run, a new \"merging/1\" ".
          "directory will be created containing its merge.hint and README ".
          "files.  To accelerate evaluating multiple merging scenarios, ".
          "all merging threshold and cell list arguments may be given a ".
          "comma-separated list of values.  All combinations of these " .
          "values will be enumerated and evaluated.  Their results will be ".
          "placed under directories N+1, N+2, etc., where N is the largest ".
          "intially existing numbered directory.\n\n" .
          "The --complex-filter option is an alternative way of specifying ".
          "what cells to merge, thus it is exclusive with --merge-cells ".
          "and --exclude-cells.  Each complex filter consists of an ".
          "arbitrary number of --merge-cells and --exclude-cells arguments ".
          "which are processed in order, and the first match determines if ".
          "a cell will be included or excluded.  The default behavior, if ".
          "nothing matched, is specified via --merge-cells (without a value) ".
          "or --exclude-cells (without a value).  For example:\n\n".
          "   --complex-filter=<--exclude-cells FQCN1 ...\n".
          "                     --merge-cells FQCN2 ...\n".
          "                     --exclude-cells FQCN3 ...\n".
          "                     --merge-cells>\n\n".
          "If the MERGE_DIR variable is set (see ".bold("select_merge")."), ".
          "then the source cast directory in all trials will be taken from " .
          "WORK_DIR/merging/MERGE_DIR/cast, and results will be written " .
          "into directories WORK_DIR/merging/MERGE_DIR/{N+1,N+2,...}.",
        EXTRA_DESC =>
          "Default values:\n\n" .
          "  threshold              -  0.001\n" .
          "  max-width-ratio        -  4.0\n" .
          "  max-fixed-width-ratio  -  1.1\n" .
          "  check-directives       -  1\n" .
          "  merge-cells            -  <> (indicating all)\n" .
          "  exclude-cells          -  <>\n" .
          "  library-cells          -  <>\n\n" .
          "The cells provided to merge-cells and exclude-cells " .
          "are evaluated over each candidate merging subtype's entire " .
          "refinement and inheritance lineage.  Thus, for example, you may ".
          "exclude all operator cells from merging by specifying\n\n" .
          "  --exclude-cells=<standard.base.OPERATOR>\n\n" .
          "Metaparameters can be left out to match all parameterizations.\n\n".
          "The library-cells argument (mergeTargetCells in ubersize) " .
          "identifies a list of (presumably) layed out, fixed-size cells ".
          "that you wish to attempt to merge against (good luck).",
        GLOBALS => combine_hashes({}, $MERGE_GLOBALS_r, {
          MERGE_DIR => { REQUIRED => 0 } }) },
    "trial_pmerge" => {
        SUBREF => \&trial_pmerge,
        USAGE  => "trial_pmerge [--tau-limit=" . underline("TL1") .
                                    "[,".underline("TL2").",..]] " .
                              "[--input-limit=" . underline("IL1") .
                                    "[,".underline("IL2").",..]] " .
                              "[--output-limit=" . underline("OL1") .
                                    "[,".underline("OL2").",..]] " .
                              "[--slack-limit=" . underline("SL1") .
                                    "[,".underline("SL2").",..]] " .
                              "[--area-limit=" . underline("AL1") .
                                    "[,".underline("AL2").",..]] " .
                              "[--merge-cells=" . underline("MCL1") .
                                    "[,".underline("MCL2").",..]] " .
                              "[--exclude-cells=" . underline("XCL1") .
                                    "[,".underline("XCL2").",..]] ",
        DESC   =>
          "Evaluates a number of merging scenarios using port-based merging. ".
          "Make sure both the fixed-size library and all leaf cells of TOP ".
          "have been characterized before running this (see " .
          bold("characterize") . ").  You'll probably need to set the " .
          underline("lib_file") . " input variable.",
        EXTRA_DESC =>
          "Default values:\n\n" .
          "  tau-limit              -  1.0\n" .
          "  input-limit            -  1.0\n" .
          "  output-limit           -  1.0\n" .
          "  slack-limit            -  0.0\n" .
          "  area-limit             -  0.0 (no limit)\n" .
          "  merge-cells            -  <> (indicating all)\n" .
          "  exclude-cells          -  <>\n\n" .
          "Like the standard MergeHint-based trial_merge, the " .
          "merge-cells and exclude-cells are evaluated over " .
          "each subtype's refinement and inheritance lineage, and ".
          "metaparameters may be left out to match all parameterization.",
        IV => {
          lib_file => {
            DESC => "Fixed-size library characterization file.  Output of ".
                    bold("characterize").".  Defaults to TOP with \".LIB\" ".
                    "inserted after the base cell name." } },
        GLOBALS => combine_hashes({}, $MERGE_GLOBALS_r, {
          MERGE_DIR => { REQUIRED => 0 } }) },
    "trial_size" => {
        SUBREF => \&trial_size,
        USAGE  => "trial_size [" . underline("trial_number") . "] | [" .
                                   underline("trial_list") . "]",
        DESC   =>
          "Sizes one or more merging trials.  If no trials are specified, " .
          "the most recently merged trial under MERGE_DIR (or ".
          "WORK_DIR/merging if MERGE_DIR isn't set) will be sized.  If ".
          "multiple merging trials are listed, they will be sized in ".
          "parallel using qsub [not yet implemented].  All of the " .
          "Sizing/size input variables will take effect for the sizing ".
          "runs launched by this command (e.g. fixed_size_delaybias).\n\n".
          "A ".underline("trial_list")." can be specified as a standard ".
          "supersize list of trial numbers (e.g. < 0 1 2 3 >) or, as a ".
          "comma-separated list of ranges (e.g. 0-4,6,8).",
        GLOBALS => combine_hashes({}, $MERGE_GLOBALS_r, {
          MERGE_DIR => { REQUIRED => 0 } }) },
    "select_merge" => {
        SUBREF => \&select_merge,
        USAGE  => "select_merge [" . underline("trial_number") . "]",
        DESC   =>
          "Selects a particular merging scenario for further merge ".
          "iterations. Causes the source cast subtype hierarchy to be ".
          "merged into WORK_DIR/merging/". underline("trial_number")."/cast.  ".
          "Sets MERGE_DIR to \"/".underline("trial_number")."\".  The " .
          underline("trial_number") . " must have been created with a prior ".
          bold("trial_merge")." command.  The select_merge command also sets ".
          "the cast path used by the Java Cast Server, so commands such as ".
          "'query' will refer to the merging trial directory subtype tree ".
          "after a particular merging trial is selected.\n\n" .
          "If no ".underline("trial_number")." is specified, MERGE_DIR is ".
          "cleared and all further trial merging will be done referenced to ".
          "the root WORK_DIR/cast subtype tree.",
        GLOBALS => combine_hashes({}, $MERGE_GLOBALS_r, {
          MERGE_DIR => { REQUIRED => 0 } }) },
    "accept_merge" => {
        SUBREF => \&accept_merge,
        USAGE  => "accept_merge [--force] [" . underline("trial_number") . "]",
        DESC   =>
          "The final step in the merging flow.  Invoked after a trial_merge ".
          "or trial_pmerge command, once you are happy with (and optionally ".
          "have sized w/ trial_size) the tentative merging trial.  ".
          "This command causes the cast subtype hierarchy ".
          "under WORK_DIR/merging/MERGE_DIR/cast to be copied into the " .
          "WORK_DIR/cast subtype space, and instances information under ".
          "WORK_DIR/merging/MERGE_DIR/instances to WORK_DIR/instances.  " .
          "After this command, the contents of WORK_DIR/merging may be ".
          "deleted unless you plan to do further merging evaluation work.\n\n" .
          "Nothing will be done if merge.hint indicates no merging; override ".
          "with --force to continue anyway.\n\n" .
          "If no ".underline("trial_number")." is specified, the most ".
          "recently generated/sized one will be used.",
        GLOBALS => combine_hashes({}, $MERGE_GLOBALS_r, {
          MERGE_DIR => { REQUIRED => 0 } }) }
    },
    GLOBALS => {
        MERGE_DIR => { DESC => 
          "The current merging evaluation directory.  For more information on ".
          "its semantics, see the \"trial_merge\", \"select_merge\", and ".
          "\"accept_merge\" commands." },
        MERGE_MID => { DESC => 
          "Enables or disabled mid-level cell merging.  Default is 1." }
    }
  };
}


#
# trial_merge
#
sub trial_merge {
    my $SS_r = shift;
    my %params = (
        threshold => [], max_width_ratio => [], max_fixed_width_ratio => [],
        merge_cells => [], exclude_cells => [], library_cells => [],
        complex_filter => []
    );
    my $check_directives = 1;

    # parse arguments
    while (num_args(\@_)) {
        my $arg = shift_next_scalar(\@_);
        my $eq  = shift_next_scalar(\@_);
        if (!defined $arg || $arg !~ /^--(.*)$/ || $eq ne "=") {
            command_die($SS_r, "Bad argument to trial_merge.");
        }
        $arg = $1;
        if ($arg eq "check-directives") {
            $check_directives = shift_next_scalar(\@_);
            command_die($SS_r, "Bad argument to --check-directives.")
                if (!defined $check_directives || 
                    ($check_directives != 0 && $check_directives != 1));
        }
        elsif ($arg eq "threshold" || $arg eq "max-width-ratio" ||
               $arg eq "max-fixed-width-ratio") {
            my ($type, $val) = shift_next_arg(\@_);
            my @list;
            if ($type == $TYPE_SCALAR) {
                @list = split /,/, $val;
            }
            elsif ($type == $TYPE_LIST) {
                @list = @{$val};
            }
            else {
                command_die($SS_r, "Bad argument to $arg.");
            }
            $arg =~ s/-/_/g;
            push @{$params{$arg}}, @list;
        }
        elsif ($arg eq "merge-cells" || $arg eq "exclude-cells" ||
               $arg eq "library-cells" || $arg eq "complex-filter") {
            $arg =~ s/-/_/g;
            my $comma = ",";
            while ($comma eq ",") {
                my ($type, $val) = shift_next_arg(\@_);
                if ($type eq $TYPE_LIST) {
                    push @{$params{$arg}}, $val;
                    if ($arg eq "complex_filter" && $val->[0] !~ '^--') {
                        command_die($SS_r,
                            "--complex-filter must start with --merge-cells " .
                            "or --exclude-cells");
                    }
                }
                else {
                    command_die($SS_r, "Bad argument to --$arg.");
                }
                $comma = next_scalar(\@_);
                shift_next_scalar(\@_) if (defined $comma && $comma eq ",");
            }
        }
    }

    if (@{$params{complex_filter}} && (@{$params{merge_cells}} ||
                                      @{$params{exclude_cells}})) {
        command_die($SS_r, "Cannot specify --merge-cells and/or " .
                           "--exclude-cells with --complex-filter");
    }

    # set defaults
    push @{$params{threshold}}, 0.001 if (!@{$params{threshold}});
    push @{$params{max_width_ratio}}, 4.0 if (!@{$params{max_width_ratio}});
    push @{$params{max_fixed_width_ratio}}, 1.1 
        if (!@{$params{max_fixed_width_ratio}});
    push @{$params{merge_cells}}, [] if (!@{$params{merge_cells}});
    push @{$params{exclude_cells}}, [] if (!@{$params{exclude_cells}});
    push @{$params{library_cells}}, [] if (!@{$params{library_cells}});
    push @{$params{complex_filter}}, [] if (!@{$params{complex_filter}});

    # run trials
    foreach my $threshold (@{$params{threshold}}) {
        foreach my $mwr (@{$params{max_width_ratio}}) {
            foreach my $mfwr (@{$params{max_fixed_width_ratio}}) {
                foreach my $cfl (@{$params{complex_filter}}) {
                    foreach my $mcl (@{$params{merge_cells}}) {
                        foreach my $xcl (@{$params{exclude_cells}}) {
                            foreach my $lcl (@{$params{library_cells}}) {
                                run_trial_merge($SS_r, 
                                    $check_directives, $threshold, $mwr, $mfwr, 
                                    $mcl, $xcl, $lcl, $cfl);
                            }
                        }
                    }
                }
            }
        }
    }
}

#
# trial_pmerge
#
sub trial_pmerge {
    my $SS_r = shift;
    my %params = (
        tau_limit => [], input_limit => [], output_limit => [], 
        slack_limit => [], area_limit => [], merge_cells => [], 
        exclude_cells => []
    );

    # parse arguments
    while (num_args(\@_)) {
        my $arg = shift_next_scalar(\@_);
        my $eq  = shift_next_scalar(\@_);
        if (!defined $arg || $arg !~ /^--(.*)$/ || $eq ne "=") {
            command_die($SS_r, "Bad argument to trial_pmerge.");
        }
        $arg = $1;
        if ($arg eq "tau-limit" || $arg eq "input-limit" ||
               $arg eq "output-limit" || $arg eq "slack-limit" ||
               $arg eq "area-limit") {
            my ($type, $val) = shift_next_arg(\@_);
            my @list;
            if ($type == $TYPE_SCALAR) {
                @list = split /,/, $val;
            }
            elsif ($type == $TYPE_LIST) {
                @list = @{$val};
            }
            else {
                command_die($SS_r, "Bad argument to $arg.");
            }
            $arg =~ s/-/_/g;
            push @{$params{$arg}}, @list;
        }
        elsif ($arg eq "merge-cells" || $arg eq "exclude-cells") {
            $arg =~ s/-/_/g;
            my $comma = ",";
            while ($comma eq ",") {
                my ($type, $val) = shift_next_arg(\@_);
                if (!defined $type || $type != $TYPE_LIST) {
                    command_die($SS_r, "Bad argument to --$arg.");
                }
                push @{$params{$arg}}, $val;
                $comma = next_scalar(\@_);
                shift_next_scalar(\@_) if (defined $comma && $comma eq ",");
            }
        }
        else {
            command_die($SS_r, "Unrecognized option --$arg.");
        }
    }

    # set defaults
    push @{$params{tau_limit}},    1.0 if (!@{$params{tau_limit}});
    push @{$params{input_limit}},  1.0 if (!@{$params{input_limit}});
    push @{$params{output_limit}}, 1.0 if (!@{$params{output_limit}});
    push @{$params{slack_limit}},  0.0 if (!@{$params{slack_limit}});
    push @{$params{area_limit}},   0.0 if (!@{$params{area_limit}});
    push @{$params{merge_cells}},   [] if (!@{$params{merge_cells}});
    push @{$params{exclude_cells}}, [] if (!@{$params{exclude_cells}});

    # determine characterization files
    my $leaf_name = basetype_of($SS_r->{GS}{TOP}) . ".LEAF." .
                    subtype_number_of($SS_r->{GS}{TOP});
    my $lib_name  = basetype_of($SS_r->{GS}{TOP}) . ".LIB." .
                    subtype_number_of($SS_r->{GS}{TOP});
    my $leaf_file = $SS_r->{GS}{WORK_DIR} . "/$leaf_name.sslib";
    my $lib_file  = $SS_r->{GS}{WORK_DIR} . "/$lib_name.sslib";

    if (get_cmd_input_scalar($SS_r, "lib_file")) {
        $lib_file = get_cmd_input_scalar($SS_r, "lib_file");
    }
    if (get_cmd_input_scalar($SS_r, "leaf_file")) {
        $leaf_file = get_cmd_input_scalar($SS_r, "leaf_file");
    }

    if (!-e $leaf_file) {
        command_die($SS_r, "Characterized leaf data file $leaf_file doesn't ".
                           "exist.");
    }
    if (!-e $lib_file) {
        command_die($SS_r, "Characterized library $lib_file doesn't exist.");
    }

    my $sizable_r = read_characterization_data($SS_r, $leaf_file);
    my $lib_r     = read_characterization_data($SS_r, $lib_file);

    # append to readme file
    my $merge_dir = trial_to_merge_dir($SS_r, get_selected_merge_trial($SS_r)). 
                    "/merging";
    mkdir $merge_dir if (!-e $merge_dir);
    open (my $readme_fh, ">>$merge_dir/README") ||
        command_die($SS_r, "Couldn't write to $merge_dir/README.");

    # run trials
    foreach my $t_limit (@{$params{tau_limit}}) {
      foreach my $i_limit (@{$params{input_limit}}) {
        foreach my $o_limit (@{$params{output_limit}}) {
          foreach my $s_limit (@{$params{slack_limit}}) {
            foreach my $a_limit (@{$params{area_limit}}) {
              foreach my $mcl (@{$params{merge_cells}}) {
                foreach my $xcl (@{$params{exclude_cells}}) {
                  run_trial_pmerge($SS_r, $readme_fh, $sizable_r, 
                              $lib_r,
                              $t_limit, $i_limit, $o_limit, $s_limit, 
                              $a_limit, $mcl, $xcl, $lib_file);
                }
              }
            }
          }
        }
      }
    }

    close $readme_fh;
}

#
# trial_size
#
sub trial_size {
    my $SS_r = shift;
    my @trials;

    # parse args...
    while (num_args(\@_)) {
        my ($type,$arg) = shift_next_arg(\@_);
        if ($type == $TYPE_SCALAR) {
            if ($arg =~ /^--/) {
                command_die($SS_r, "Unknown option specified: $arg.");
            }
            else {
                push @trials, parse_trials($SS_r, $arg);
            }
        }
        elsif ($type == $TYPE_LIST) {
            if (@trials) {
                command_die($SS_r, "Too many trials specified.");
            }
            @trials = @{$arg};
        }
        else {
            command_die($SS_r, "Bad argument type specified.");
        }
    }

    @trials = (find_latest_merge_trial($SS_r)) if (!@trials);

    # Make sure there's something to size
    if (!@trials || $trials[0] eq "") {
        command_die($SS_r, "No merge trial to size.  Run trial_merge or " .
                           "trial_pmerge first.");
    }

    # Make sure all merge trial cast hierarchies exist
    my @do_trials = ();
    my @skip_trials = ();
    foreach my $trial (@trials) {
        my $trial_dir = trial_to_merge_dir($SS_r, $trial);
        if (has_hints($trial_dir)) {
            push @do_trials, $trial;
            merge_trial_space($SS_r, $trial);
        } else {
            push @skip_trials, $trial;
        }
    }

    if (@skip_trials) {
        my $trials = @skip_trials == 1 ? "trial" : "trials";
        print "trial_size skipped for $trials @skip_trials because no cells " .
              "are actually merged in these trials\n";
    }

    return unless @do_trials;

    print "Sizing trials @do_trials.\n" if ($SS_r->{GS}{DEBUG});

    # default fixed_size_delaybias set from size IV, or 1.0 if not defined
    my $fixed_size_delaybias = get_cmd_input_scalar($SS_r,
                                "fixed_size_delaybias", "Sizing/size");
    my $sizable_delaybias = get_cmd_input_scalar($SS_r,
                                "sizable_delaybias", "Sizing/size");
    my $use_delay_signoff = get_cmd_input_scalar($SS_r,
                                "use_delay_signoff", "Sizing/size");
    my $delay_signoff_file = get_cmd_input_scalar($SS_r,
                                "delay_signoff_file", "Sizing/size");
    
    # Size each specified merge directory
    foreach my $trial (@do_trials) {
        # Size
        my $dir = trial_to_merge_dir($SS_r, $trial);
        if ($SS_r->{GS}{VERBOSE}) {
            print "Sizing merge trial $trial.\n";
        }
        # WORK_DIR/cast included as last component to make fixed-size cells
        # in WORK_DIR/cast visible (comes up in ECOs and other unique
        # sizing situations).
        set_server_merge_dir($SS_r, $dir);
        my $err = size_common($SS_r, $dir, $SS_r->{GS}{TOP},
                              { use_floorplan         => 1, 
                                fixed_size_delaybias  => $fixed_size_delaybias,
                                sizable_delaybias     => $sizable_delaybias,
                                update_density_factor => 1,
                                use_delay_signoff     => $use_delay_signoff,
                                delay_signoff_file    => $delay_signoff_file,
                                violations_debug      => 1
                              });
        if ($err) {
            command_die($SS_r, "Error: Jauto exitted abnormally.");
        }
    }
}

# Parses a trial range list, returns a list of individual trials.
# Example: 3/2-3/4 returns (3/2,3/3,3/4).  3/2-4 also returns the same list.
sub parse_trials {
    my $SS_r = shift;
    my $str  = shift;
    my @trials;

    my @ranges = split /,/, $str;
    foreach my $r (@ranges) {
        next if ($r=~ /^$/);
        if  ($r =~ /^[\/\d]+$/) {
            push @trials, $r;
        }
        elsif ($r =~ /^([\/\d]+\/)?(\d+)-\1?([\/\d]+)$/) {
            my $base = (defined $1 ? $1 : "");
            foreach my $t ($2..$3) {
                push @trials, $base . $t;
            }
        }
        else {
            command_die($SS_r, "Bad trial range specified: $r.");
        }
    }
    return @trials;
}


#
# trial_query
#
sub trial_query {
    my $SS_r = shift;

    # parse args...
    my $trial;
    while (num_args(\@_)) {
    }
}

#
# select_merge
#
sub select_merge {
    my $SS_r = shift;
    # parse args
    my $select = shift_next_scalar(\@_);
    if (!defined $select) {
        # Unset MERGE_DIR so future trial merge runs will use the original
        # WORK_DIR/cast subtype tree
        delete $SS_r->{GS}{MERGE_DIR};
        # Unset WORK_SUBDIR so commands now return to the standard WORK_DIR
        # working directory.
        delete $SS_r->{GS}{WORK_SUBDIR};
        return;
    }
    if ($select !~ /^(\d+\/)*\d+$/ || num_args(\@_)) {
        command_die($SS_r, "Bad arguments to select_merge.");
    }
    merge_trial_space($SS_r, $select);
    $SS_r->{GS}{MERGE_DIR} = $select;
    $SS_r->{GS}{WORK_SUBDIR} = trial_to_merge_dir($SS_r, $select);
}

sub merge_trial_space {
    my $SS_r       = shift;
    my $dest_trial = shift;
    my $dest_dir = trial_to_merge_dir($SS_r, $dest_trial);

    (my $src_trial = $dest_trial) =~ s/\/?\d+//g;
    my $src_dir  = trial_to_merge_dir($SS_r, $src_trial);

    if ($dest_trial !~ /^(\d+\/)*\d+$/ || $src_trial !~ /^(\d+\/)*\d*$/) {
        command_die($SS_r, "Bad merge trial format.");
    }

    # Create subtypes if they don't already exist
    if (!-e "$dest_dir/cast" && -e "$src_dir/cast") {
        if (!-e "$dest_dir/merge.hint") {
            command_die($SS_r, "$dest_dir/merge.hint doesn't exist.  Run ".
                        "trial_merge first.");
        }
        if ($SS_r->{GS}{VERBOSE}) {
            print "Merging subtype space for trial $dest_trial.";
            if (defined $SS_r->{GS}{MERGE_MID} && $SS_r->{GS}{MERGE_MID}==0) {
                print "  Excluding mid-level cells.\n";
            }
            else {
                print "\n";
            }
        }
        merge_subtypes($SS_r, $src_dir, $dest_dir, 
                       defined $SS_r->{GS}{MERGE_MID} ? $SS_r->{GS}{MERGE_MID} :
                       1);

        if ($src_trial ne "" && -e "$src_dir/instances") {
            print "Copying geometry information from trial $src_trial.\n";
            `cp -a $src_dir/instances $dest_dir/instances`;
        }
        elsif (-e get_work_dir($SS_r) . "/instances") {
            my $dir = get_work_dir($SS_r) . "/instances";
            print "Copying geometry information from $dir.\n";
            `cp -a $dir $dest_dir/instances`;
        }
        elsif (-e $SS_r->{GS}{WORK_DIR} . "/instances") {
            print "Copying geometry information from WORK_DIR.\n";
            my $dir = "$SS_r->{GS}{WORK_DIR}/instances";
            `cp -a $dir $dest_dir/instances`;
        }
    }
    elsif (!-e "$src_dir/cast") {
        command_die($SS_r, "Source subtype hierarchy $src_dir/cast\n" .
                           "doesn't exist.");
    }
    return ($src_dir, $dest_dir);
}


#
# accept_merge
#
sub accept_merge {
    my $SS_r = shift;

    # parse args
    my $trial;
    my $force = 0;
    while (num_args(\@_)) {
        my $arg = shift_next_scalar(\@_);
        if ($arg eq "--force") {
            $force = 1;
        }
        else {
            $trial = $arg;
            if (!defined $trial || $trial !~ /^(\d+\/)*\d+$/ || num_args(\@_)) {
                command_die($SS_r, "Bad arguments to accept_merge.");
            }
        }
    }
    if (!defined($trial)) {
        $trial = find_latest_merge_trial($SS_r);
        if (!defined $trial || $trial eq "") {
            command_die($SS_r, "No merge trial found.  Run trial_merge or ".
                               "trial_pmerge first.");
        }
    }

    my $trial_dir = trial_to_merge_dir($SS_r, $trial);
    if (!$force && !has_hints($trial_dir)) {
        print "accept_merge skipped because trial $trial does not " .
              "actually merge any cells.\n";
        return;
    }

    merge_trial_space($SS_r, $trial);

    # Copy cast/instances into WORK_DIR
    my @subdirs = ("cast","instances");
    foreach my $subdir (@subdirs) {
        my $src_dir = "$trial_dir/$subdir";
        my $dest_dir = "$SS_r->{GS}{WORK_DIR}/$subdir";
        if (-e $src_dir) {
            print "Copying merge trial $trial into WORK_DIR/$subdir.\n";
            $src_dir =~ s/\/$//;
            $dest_dir =~ s/\/$//;
            #`rm -rf '$dest_dir'`; remove per Brian's request, see bug 14227
            #`cp -af '$src_dir' '$dest_dir'`;
            `/usr/bin/rsync -aq '$src_dir/' '$dest_dir'`;
        }
    }

    # Clear MERGE_DIR variable so that merging continues from WORK_DIR/cast
    delete $SS_r->{GS}{MERGE_DIR};

    # Unset WORK_SUBDIR so commands now return to the standard WORK_DIR
    # working directory.
    delete $SS_r->{GS}{WORK_SUBDIR};

    # Dirties WORK_DIR/cast obviously
    set_dirty($SS_r, "all");
}

################################# Utilities #################################

sub has_hints {
    my $trial_dir = shift;
    local $_;
    open (my $fh, "$trial_dir/merge.hint") || return 0;

    my $found = 0;
    while (<$fh>) {
        # find any real, non-commented, merge hint, which indicates some cells
        # would be merged
        if (!/^#/) {
            $found = 1;
            last;
        }
    }
    close $fh;

    return $found;
}

sub merge_summary {
    my ($merge_cnt, $unmerged_cnt, $base_cnt) = @_;
    my $result = "Merged $merge_cnt leaf cells.  ";
    if ($base_cnt == 0) {
        $result .= "No merge candidates after applying --merge-cells and " .
                   "--exclude-cells filters.";
    } else {
        my $pct = POSIX::ceil(100.0 * ($unmerged_cnt / $base_cnt - 1.0));
        $result .= "Number of leaf cells after merging: ".  $unmerged_cnt .
                   " (" . ($unmerged_cnt >= $base_cnt ? "+":"") . "$pct%)";
    }
    return $result;
}

sub run_trial_merge {
    my $SS_r                = shift;
    my $check_directives    = shift;
    my $threshold           = shift;
    my $max_w_ratio         = shift;
    my $max_fixed_w_ratio   = shift;
    my $merge_cell_list_r   = shift;
    my $exclude_cell_list_r = shift;
    my $library_cell_list_r = shift;
    my $complex_filter_r    = shift;

    my ($merge_dir, $trial_num, $cast_dir) = create_next_trial_dir($SS_r);
    print bold("TRIAL $trial_num:") . " ($threshold $max_w_ratio " . 
        "$max_fixed_w_ratio)\n";

    # Fist create TRIAL file
    open (README, ">$merge_dir/TRIAL") || 
        command_die($SS_r, "Couldn't write to $merge_dir/TRIAL.");
    print README "threshold             = $threshold\n";
    print README "max-width-ratio       = $max_w_ratio\n";
    print README "max-fixed-width-ratio = $max_fixed_w_ratio\n";
    print README "check-directives      = $check_directives\n";
    print README "-------------------------------------------------------\n";
    print README "merge-cells =\n";
    my $mcl_str = "";
    foreach my $mc (@{$merge_cell_list_r}) {
        print README "  $mc\n";
        $mcl_str .= ":" if ($mcl_str ne "");
        $mcl_str .= "$mc";
    }
    print README "-------------------------------------------------------\n";
    print README "exclude-cells =\n";
    my $xcl_str = "";
    foreach my $xc (@{$exclude_cell_list_r}) {
        print README "  $xc\n";
        $xcl_str .= ":" if ($xcl_str ne "");
        $xcl_str .= "$xc";
    }
    print README "-------------------------------------------------------\n";
    print README "complex-filter =\n";
    my $filter_str = "";
    my @accum = ();
    foreach my $cf (@{$complex_filter_r}) {
        print README "  $cf\n";
        if ($cf =~ '^--') {
            $filter_str .= "=" . join(':', @accum) if (@accum);
            $filter_str .= " $cf";
            @accum = ();
        } else {
            push @accum, $cf;
        }
    }
    $filter_str .= "=" . join(':', @accum) if (@accum);
    print README "-------------------------------------------------------\n";
    print README "library-cells =\n";
    my $lcl_str = "";
    foreach my $lc (@{$library_cell_list_r}) {
        print README "  $lc\n";
        $lcl_str .= ":" if ($lcl_str ne "");
        $lcl_str .= "$lc";
    }
    close README;

    # Construct MergeHint command
    my $cmd = "com.avlsi.tools.jauto.MergeHint ";
    $cmd .= "--cell=$SS_r->{GS}{TOP} ";
    $cmd .= "--ignore-staticizer ";
    $cmd .= "--threshold=$threshold ";
    $cmd .= "--max-width-ratio=$max_w_ratio ";
    $cmd .= "--max-fixed-width-ratio=$max_fixed_w_ratio ";
    $cmd .= "--directive " if ($check_directives);
    $cmd .= "--merge-cells=$mcl_str " if ($mcl_str ne "");
    $cmd .= "--exclude-cells=$xcl_str " if ($xcl_str ne "");
    $cmd .= " $filter_str " if ($filter_str ne "");
    $cmd .= "--merge_target=$lcl_str " if ($lcl_str ne "");
    $cmd .= "--instance-dir=$SS_r->{GS}{WORK_DIR}/instances "
        if (-e "$SS_r->{GS}{WORK_DIR}/instances");
    
    # Warn if floorplan information isn't available
    if (!-e "$SS_r->{GS}{WORK_DIR}/instances") {
        print "Warning: Floorplan information isn't available, so merging\n".
              "         will not consider placement.\n";
    }

    # Run the MergeHint command
    my @hints;
    set_server_merge_dir($SS_r, $cast_dir);
    my $ret = run_cast_server_command($SS_r, $cmd, undef, undef, -1, \@hints);

    # Write merge.hint file if success
    my $num_leaf_base = 1;
    my $num_leaf_subtypes = 0;
    my $num_leaf_subtypes_before = 0;
    if ($ret == 0) {
        open (MH, ">$merge_dir/merge.hint") || 
            command_die($SS_r, "Couldn't write to $merge_dir/merge.hint");
        foreach my $l (@hints) {
            if ($l =~ /number of leaf types:\s+(\d+)/) {
                $num_leaf_base = $1;
            }
            elsif ($l =~ /number of leaf subtypes:\s+(\d+)/) {
                $num_leaf_subtypes_before = $1;
            }
            elsif ($l =~ /Number of leaf cells after merging:\s+(\d+)/) {
                $num_leaf_subtypes = $1;
            }
            print MH $l . "\n";
        }
        close MH;
    }
    else {
        command_die($SS_r, "Error running MergeHint.  Java Cast server " .
            "returned $ret.");
    }

    # short summary
    print merge_summary($num_leaf_subtypes_before - $num_leaf_subtypes,
                        $num_leaf_subtypes, $num_leaf_base) . "\n";

    # return merge directory
    return $merge_dir;
}

sub filter_predicate {
    my ($SS_r, $cell, $filter) = @_;
    my $cmd = "--cell=$cell --no-recurse --task=subcells --filter=$filter";
    my @lines;
    query_cast_server($SS_r, $cmd, \@lines, 1);
    return scalar(@lines);
}

sub run_trial_pmerge {
    my $SS_r                = shift;
    my $readme_fh           = shift;
    my $sizable_r           = shift;
    my $lib_r               = shift;
    my $t_limit             = shift;
    my $i_limit             = shift;
    my $o_limit             = shift;
    my $s_limit             = shift;
    my $a_limit             = shift;
    my $merge_cell_list_r   = shift;
    my $exclude_cell_list_r = shift;
    my $lib_file_name       = shift;

    my ($merge_dir, $trial_num, $cast_dir) = create_next_trial_dir($SS_r);
    my $str = bold("TRIAL $trial_num:") . " ($t_limit $i_limit $o_limit " . 
              "$s_limit $a_limit)";
    print "$str\n" if ($SS_r->{GS}{VERBOSE});
    print $readme_fh "$str [port-based]\n";

    # Fist create TRIAL file
    open (README, ">$merge_dir/TRIAL") || 
        command_die($SS_r, "Couldn't write to $merge_dir/TRIAL.");
    print README "tau-limit     = $t_limit\n";
    print README "input-limit   = $i_limit\n";
    print README "output-limit  = $o_limit\n";
    print README "slack-limit   = $s_limit\n";
    print README "area-limit    = $a_limit\n";
    print README "library file  = $lib_file_name\n";
    print README "-------------------------------------------------------\n";
    print README "merge-cells =\n";
    foreach my $mc (@{$merge_cell_list_r}) {
        print README "  $mc\n";
    }
    print README "-------------------------------------------------------\n";
    print README "exclude-cells =\n";
    foreach my $xc (@{$exclude_cell_list_r}) {
        print README "  $xc\n";
    }
    print README "-------------------------------------------------------\n";
    close README;

    # Determine list of cells to merge
    my $cell_list_r;
    my $base_cnt = 0;

    my $mcl_filter;
    if (@{$merge_cell_list_r}) {
        $mcl_filter = join('|', map { "ancestor=$_" } @{$merge_cell_list_r});
    }

    my $xcl_filter;
    if (@{$exclude_cell_list_r}) {
        $xcl_filter = join('|', map { "ancestor=$_" } @{$exclude_cell_list_r});
    }

    my %base_list = ();
    foreach my $b (keys %{$sizable_r}) {
        foreach my $s (keys %{$sizable_r->{$b}}) {
            if (defined($mcl_filter)) {
                next if filter_predicate($SS_r, $s, $mcl_filter) == 0;
            }

            if (defined($xcl_filter)) {
                next unless filter_predicate($SS_r, $s, $xcl_filter) == 0;
            }

            $base_list{$b} = 1;
            push @{$cell_list_r}, $s;
        }
    }
    $base_cnt = scalar(keys(%base_list));
                    
    # Attempt to merge all cells
    my $merge_cnt = 0;
    my $unmerged_cnt = 0;
    my %merge_map;
    my @merge_targets;
    foreach my $fqcn (@{$cell_list_r}) {
        my $base = basetype_of($fqcn);
        if (!exists $sizable_r->{$base} ||
            !exists $sizable_r->{$base}{$fqcn}) {
            print STDERR "Couldn't find characterization data for $fqcn.\n";
            next;
        }
        my $cell_r = $sizable_r->{$base}{$fqcn};

        # Look for a merge target for this subtype
        my ($target, $t_ratio, $i_ratio, $o_ratio, $a_ratio) = 
            find_pmerge_target($SS_r, $fqcn, $SS_r->{GS}{TAU} * 1e-12,
                               $cell_r, $lib_r, $t_limit, $i_limit, 
                               $o_limit, $s_limit, $a_limit);

        if (defined $target) {
            $merge_map{$target} = [] if (!exists $merge_map{$target});
            push @{$merge_map{$target}}, $fqcn;
            push @merge_targets, $target;
            $merge_cnt++;
        }
        else {
            $merge_map{$fqcn} = [$fqcn];
            $unmerged_cnt++;
        }
    }

    # Write merge.hint file
    open (MH, ">$merge_dir/merge.hint") || 
        command_die($SS_r, "Couldn't write to $merge_dir/merge.hint");
    print MH "# Merge targets:\n";
    foreach my $t (@merge_targets) {
        print MH "#   $t\n";
    }
    foreach my $t (keys %merge_map) {
        next if (@{$merge_map{$t}}==1 && $merge_map{$t}->[0] eq $t);
        print MH basetype_of($t) . " " . subtype_number_of($t);
        foreach my $s (@{$merge_map{$t}}) {
            print MH " " . subtype_number_of($s);
        }
        print MH "\n";
    }
    close MH;

    # short summary
    $str = merge_summary($merge_cnt, $unmerged_cnt, $base_cnt) . "\n";
    print $str if ($SS_r->{GS}{VERBOSE});
    print $readme_fh $str;

    # return merge directory
    return $merge_dir;
}

# Creates the next merge trial directory and returns it.  Also returns
# the trial number and the cast subtype directory to be used with this 
# merge trial (specifically, WORK_DIR/cast if MERGE_DIR isn't set; 
# otherwise WORK_DIR/merging/MERGE_DIR/cast).  If $src_trial is specified,
# this trial directory will be used instead of MERGE_DIR.
sub create_next_trial_dir {
    my $SS_r      = shift;
    my $src_trial = shift;

    my $merge_dir = $SS_r->{GS}{WORK_DIR};

    if (defined $src_trial) {
        $merge_dir = trial_to_merge_dir($SS_r, $src_trial);
    }
    elsif (exists $SS_r->{GS}{MERGE_DIR}) {
        $merge_dir = trial_to_merge_dir($SS_r, $SS_r->{GS}{MERGE_DIR});
    }

    my $cast_dir = $merge_dir;
    $merge_dir .= "/merging";

    mkdir $merge_dir if (!-e $merge_dir);

    opendir(MDIR, $merge_dir) || 
        command_die($SS_r, "Couldn't read merge directory $merge_dir.");
    my @nums = sort { $b <=> $a } grep { $_ =~ /^\d+$/ } readdir MDIR;
    my $num = (!@nums) ? 0 : $nums[0]+1;
    $merge_dir .= "/$num";
    mkdir($merge_dir) || command_die($SS_r, "Couldn't create $merge_dir.");
    return ($merge_dir, $num, $cast_dir);
}

sub get_selected_merge_trial {
    my $SS_r = shift;
    return (exists $SS_r->{GS}{MERGE_DIR}) ? $SS_r->{GS}{MERGE_DIR} : "";
}

sub find_latest_merge_trial {
    my $SS_r = shift;
    my $merge_dir = $SS_r->{GS}{WORK_DIR} . "/merging";
    my $trial_base = "";
    if (exists $SS_r->{GS}{MERGE_DIR}) {
        $merge_dir = trial_to_merge_dir($SS_r, $SS_r->{GS}{MERGE_DIR}) .
                        "/merging";
        $trial_base = $SS_r->{GS}{MERGE_DIR} . "/";
    }
    opendir(MDIR, $merge_dir) || 
        command_die($SS_r, "Couldn't read merge directory $merge_dir.");
    my @nums = sort { $b <=> $a } grep { $_ =~ /^\d+$/ } readdir MDIR;
    return $trial_base . (@nums ? $nums[0] : "");
}

# Converts a merging trial string (e.g. "0/5/2") into a merge directory
# (e.g. WORK_DIR/merging/0/merging/5/merging/2).  Converts an empty trial
# string into WORK_DIR.
sub trial_to_merge_dir {
    my $SS_r  = shift;
    my $trial = shift;
    my $merge_dir = $SS_r->{GS}{WORK_DIR};
    if (defined $trial && $trial ne "") {
        $trial =~ s/\//\/merging\//g;
        $trial = "/merging/" . $trial;
    }
    else {
        $trial = "";
    }
    return $merge_dir . $trial;
}

# Merges cast subtypes given a merge.hint file
sub merge_subtypes {
    my $SS_r     = shift;
    my $src_dir  = shift;
    my $dest_dir = shift;
    my $do_mid   = shift;

    # determine cells in TOP
    set_server_merge_dir($SS_r, $src_dir);
    my $query_cmd = "--cell=$SS_r->{GS}{TOP} --task=subcells";
    my @lines;
    query_cast_server($SS_r, $query_cmd, \@lines, 1);
    my %top_cells;
    foreach my $l (@lines) {
        $l =~ s/\s*$//; chomp $l;
        $top_cells{$l} = 1;
    }

    # read merge hints file to get merge targets
    my $merge_target_str = "";
    open (MH, "$dest_dir/merge.hint") || 
        command_die($SS_r, "Couldn't read $dest_dir/merge.hint");
    while (<MH>) {
        next if (/^#/ || /^\s*$/);
        my @parts = split /\s+/, $_;
        if (!exists $top_cells{"$parts[0].$parts[1]"}) {
            $merge_target_str .= "$parts[0].$parts[1]:";
        }
    }
    close MH;
    $merge_target_str =~ s/:$//;
    
    my $cmd = (set_java_cmd($SS_r, "subtype_merge", $MINOR_JOB, 0))[0];
    $cmd .= "--cast-path=\"$SS_r->{GS}{CAST_DIR}:$src_dir/cast:" .
                        "$SS_r->{GS}{WORK_DIR}/cast:" .
                        "$SS_r->{GS}{SPEC_DIR}\" \\\n";
    $cmd .= "--cell=\"$SS_r->{GS}{TOP}\" \\\n";
    $cmd .= "--subtype-path=\"$dest_dir/cast\" \\\n";
    $cmd .= "--all \\\n" if (defined $do_mid && $do_mid==1);
    $cmd .= "--instance-dir=\"$SS_r->{GS}{WORK_DIR}/instances\" \\\n"
        if (-e "$SS_r->{GS}{WORK_DIR}/instances");
    $cmd .= "--merge_target=\"$merge_target_str\" \\\n"
        if ($merge_target_str ne "");
    $cmd .= "--output-leaves \\\n";
    $cmd .= "< \"$dest_dir/merge.hint\"";
    supersize_system($SS_r, $cmd, $MINOR_JOB);
}


#
# merge_me
#
sub merge_me {
    my $SS_r = shift;
    my $leaf_name = basetype_of($SS_r->{GS}{TOP}) . ".LEAF." .
                    subtype_number_of($SS_r->{GS}{TOP});
    my $lib_name  = basetype_of($SS_r->{GS}{TOP}) . ".LIB." .
                    subtype_number_of($SS_r->{GS}{TOP});
    my $leaf_file = $SS_r->{GS}{WORK_DIR} . "/$leaf_name.sslib";
    my $lib_file  = $SS_r->{GS}{WORK_DIR} . "/$lib_name.sslib";
    
    # parse args
    my $cell_list_r = [];
    while (num_args(\@_)) {
        my ($type, $val) = shift_next_arg(\@_);
        if ($type == $TYPE_SCALAR) {
            if ($val eq "--library") {
                my $eq = shift_next_scalar(\@_);
                my $lib_file = shift_next_scalar(\@_);
                if (!defined $eq || $eq ne "=" || !defined $lib_file) {
                    command_die($SS_r, "Bad arguments to $val.");
                }
                $lib_file .= ".sslib";
            }
            elsif ($val eq "--leaf-sslib") {
                my $eq = shift_next_scalar(\@_);
                my $leaf_file = shift_next_scalar(\@_);
                if (!defined $eq || $eq ne "=" || !defined $leaf_file) {
                    command_die($SS_r, "Bad arguments to $val.");
                }
            }
            if (@{$cell_list_r}) {
                command_die($SS_r, "Too many arguments to merge_me.");
            }
            push @{$cell_list_r}, $val;
        }
        elsif ($type == $TYPE_LIST) {
            if (@{$cell_list_r}) {
                command_die($SS_r, "Too many arguments to merge_me.");
            }
            $cell_list_r = $val;
        }
        else {
            command_die($SS_r, "Bad argument to merge_me.");
        }
    }
    if (!@{$cell_list_r}) {
        command_die($SS_r, "Insufficient arguments to merge_me.") 
    }

    # get library config
    if (get_cmd_input_scalar($SS_r, "lib_file")) {
        $lib_file = get_cmd_input_scalar($SS_r, "lib_file");
    }
    if (get_cmd_input_scalar($SS_r, "leaf_file")) {
        $leaf_file = get_cmd_input_scalar($SS_r, "leaf_file");
    }

    if (!-e $leaf_file) {
        command_die($SS_r, "Characterized leaf data file $leaf_file doesn't ".
                           "exist.");
    }
    if (!-e $lib_file) {
        command_die($SS_r, "Characterized library $lib_file doesn't exist.");
    }

    my $sizable_r = read_characterization_data($SS_r, $leaf_file);
    my $lib_r     = read_characterization_data($SS_r, $lib_file);

    my $merge_cnt = 0;
    foreach my $cell (@{$cell_list_r}) {
        my $base = basetype_of($cell);
        if (!exists $sizable_r->{$base} ||
            !exists $sizable_r->{$base}{$cell}) {
            print STDERR "Couldn't find characterization data for $cell.\n";
            next;
        }
        my $cell_r = $sizable_r->{$base}{$cell};

        # Now attempt to merge
        my ($target, $t_ratio, $i_ratio, $o_ratio, $a_ratio) = 
            find_pmerge_target($SS_r, $cell, $SS_r->{GS}{TAU} * 1e-12,
                              $cell_r, $lib_r,
                              2.0, 1.0, 0.5, 0.0, 1.0);

        if (defined $target) {
            print "Identified merge target $target.\n";
            $merge_cnt++;
        }
        else {
            print "Could not identify a merge target for $cell.\n";
        }
    }
    print "Successfully merged $merge_cnt of ".scalar(@{$cell_list_r})." ".
          "cells (" . sprintf("%.3g", $merge_cnt/scalar(@{$cell_list_r})*100) .
          "%)\n";
}


sub mergeable_directive {
    my ($SS_r, $fqcn, $candidates_r) = @_;
    my $cmd = "com.avlsi.tools.jauto.MergeHint --cell=$fqcn " .
              "--check-directives --merge_target=" .
              join(':', @{$candidates_r});

    # Run the MergeHint command
    my @hints;
    my $ret = run_cast_server_command($SS_r, $cmd, undef, undef, -1, \@hints);
    if ($ret) {
        command_die($SS_r, "cannot determine if cells have compatible " .
                           "directives: $ret");
    }

    my @results = ();
    foreach my $hint (@hints) {
        my @cells = split(/ /,$hint);
        if ($cells[0] eq $fqcn) {
            @results = @cells;
            shift(@results);
            last;
        }
    }

    return \@results;
}

# Looks for candidate merge targets for a specified cell.  Arguments:
#   fqcn          - source subtype we wish to merge
#   tau           - this cell's effective tau requirement
#   c_r           - leaf cell characterization data
#   lib_r         - library cell characterizatoin data
#   t_limit       - maximum (lib cell tau) / (source cell tau)
#   i_limit       - maximum (lib cell i_load) / (source cell i_load) across
#                   all input ports. (1.0+epsilon typical)
#   o_limit       - minimum (lib cell o_strength) / (source cell o_strength)
#                   across all output ports.  (1.0-epsilon typical)
#   s_limit       - minimum (lib o_slack - cell o_slack) across all output
#                   ports, ps.  (0.0-epsilon typical)
#   a_limit       - maximum (lib cell area) / (source cell area).  A value 
#                   of 0.0 (or negative) indicates no limit.
#   
sub find_pmerge_target {
    my $SS_r      = shift;
    my $fqcn      = shift;
    my $tau       = shift;
    my $c_r       = shift;
    my $lib_r     = shift;
    my $t_limit   = shift;
    my $i_limit   = shift;
    my $o_limit   = shift;
    my $s_limit   = shift;
    my $a_limit   = shift;


    # return values
    my ($target, $t_ratio, $o_ratio, $i_ratio, $a_ratio);

    my $base_cell = basetype_of($fqcn);
    return if (!exists $lib_r->{$base_cell});

    # Get candidate merge targets that have compatible directives with fqcn
    my @all_candidates = keys %{$lib_r->{$base_cell}};
    my $compatible_dir = mergeable_directive($SS_r, $fqcn, \@all_candidates);

    # Iterate over all candidate lib subtypes
    foreach my $subtype (@{$compatible_dir}) {
        # lib target cell data reference
        my $lc_r = $lib_r->{$base_cell}{$subtype};
        my $disqualify = 0;

        # Basic tau check
        my $t_pass = 1;
        my $lt_ratio = 0.0;
        if (exists $lc_r->{TAU}) {
            $lt_ratio = ($c_r->{TAU} != 0.0) ? $lc_r->{TAU}/$c_r->{TAU} : 0.0;
            $t_pass = ($lt_ratio <= $t_limit);
        }
        else {
            if ($SS_r->{GS}{DEBUG}) {
                print STDERR "Warning: Library $subtype doesn't define TAU.\n";
            }
            $disqualify = 1;
        }

        # Compare input loads
        my $li_ratio_max;
        foreach my $i (keys %{$c_r->{I}}) {
            if (!exists $lc_r->{I}{$i}) {
                if ($SS_r->{GS}{DEBUG}) {
                    print STDERR "Warning: Library $subtype doesn't define ".
                                 "input $i.\n";
                }
                $disqualify = 1;
            }
            elsif (!exists $lc_r->{I}{$i}{GATE_LOAD}) {
                if ($SS_r->{GS}{DEBUG}) {
                    print STDERR "Warning: Input $i of library subtype ";
                    print STDERR "$subtype doesn't define GATE_LOAD.\n";
                }
                $disqualify = 1;
            }
            else {
                my $i_r = $c_r->{I}{$i}{GATE_LOAD} != 0.0 ?
                    $lc_r->{I}{$i}{GATE_LOAD}/$c_r->{I}{$i}{GATE_LOAD} :
                    $lc_r->{I}{$i}{GATE_LOAD}/1e-16;
                if (!defined $li_ratio_max || $i_r > $li_ratio_max) {
                    $li_ratio_max = $i_r;
                }
            }
        }
        $li_ratio_max = 1.0 if (!defined $li_ratio_max);
        my $i_pass = ($li_ratio_max <= $i_limit);
        my $i_best = $i_pass && (!defined $target || $li_ratio_max < $i_ratio);

        # Compare output slacks and drive strengths
        my $ls_diff_min;
        my $lo_ratio_min;
        foreach my $o (keys %{$c_r->{O}}) {
            if (!exists $lc_r->{O}{$o}) {
                if ($SS_r->{GS}{DEBUG}) {
                    print STDERR "Warning: Library $subtype doesn't define " .
                                 "output $o.\n";
                }
                $disqualify = 1;
            }
            else {
                if (!exists $lc_r->{O}{$o}{SLACK} || 
                    !defined $lc_r->{O}{$o}{SLACK}) {
                    if ($SS_r->{GS}{DEBUG}) {
                        print STDERR "Warning: Output $o of library subtype ";
                        print STDERR "$subtype doesn't define SLACK.\n";
                    }
                    $disqualify = 1;
                }
                else {
                    if (!exists $c_r->{O}{$o}{SLACK}) {
                        command_die($SS_r, "SLACK not defined for output $o ".
                                           "of cell $fqcn.");
                    }
                    my $s_d = $lc_r->{O}{$o}{SLACK} - $c_r->{O}{$o}{SLACK};
                    if (!defined $ls_diff_min || $s_d < $ls_diff_min) {
                        $ls_diff_min = $s_d;
                    }
                }
                if (!exists $lc_r->{O}{$o}{OHMS_UP}) {
                    if ($SS_r->{GS}{DEBUG}) {
                        print STDERR "Warning: Output $o of library subtype ";
                        print STDERR "$subtype doesn't define OHMS_UP.\n";
                    }
                    $disqualify = 1;
                }
                else {
                    if (!exists $c_r->{O}{$o}{OHMS_UP}) {
                        command_die($SS_r, "OHMS_UP not defined for output $o ".
                                           "of cell $fqcn.");
                    }
                    my $o_r = $c_r->{O}{$o}{OHMS_UP}/$lc_r->{O}{$o}{OHMS_UP};
                    if (!defined $lo_ratio_min || $o_r < $lo_ratio_min) {
                        $lo_ratio_min = $o_r;
                    }
                }
                if (!exists $lc_r->{O}{$o}{OHMS_DN}) {
                    if ($SS_r->{GS}{DEBUG}) {
                        print STDERR "Warning: Output $o of library subtype ";
                        print STDERR "$subtype doesn't define OHMS_DN.\n";
                    }
                    $disqualify = 1;
                }
                else {
                    if (!exists $c_r->{O}{$o}{OHMS_DN}) {
                        command_die($SS_r, "OHMS_DN not defined for output $o ".
                                           "of cell $fqcn.");
                    }
                    my $o_r = $c_r->{O}{$o}{OHMS_DN}/$lc_r->{O}{$o}{OHMS_DN};
                    if (!defined $lo_ratio_min || $o_r < $lo_ratio_min) {
                        $lo_ratio_min = $o_r;
                    }
                }
            }
        }
        $ls_diff_min  = 0.0 if (!defined $ls_diff_min);
        $lo_ratio_min = 1.0 if (!defined $lo_ratio_min);
        my $o_pass = ($lo_ratio_min >= $o_limit);
        my $s_pass = ($ls_diff_min >= $s_limit);
        #my $o_best = $o_pass && (!defined $target || $o_ratio_min >= $o_ratio);
            
        # Area comparison
        my $a_pass = 1;
        my $a_ratio = 0.0;
        if ($a_limit > 0 && exists $c_r->{AREA} && defined $c_r->{AREA}) {
            if (!exists $lc_r->{AREA} || !defined $lc_r->{AREA} ||
                $lc_r->{AREA}==0) {
                print STDERR "Warning: library subtype $subtype doesn't have ".
                             "an area.\n";
            }
            $a_ratio = $lc_r->{AREA} / $c_r->{AREA} if ($c_r->{AREA}>0);
            $a_pass  = 0 if ($a_ratio > $a_limit);
        }

        if ($SS_r->{GS}{DEBUG}) {
            print color('bold') if ($t_pass && $o_pass && $i_pass && $s_pass);
            print "Candidate $subtype:\n";
            print color('bold') if ($t_pass);
            print "  t_ratio = " . sprintf("%.2g", $lt_ratio);
            print color('reset') if ($t_pass);
            print color('bold') if ($i_pass);
            print "  i_ratio = " . sprintf("%.2g", $li_ratio_max);
            print color('reset') if ($i_pass);
            print color('bold') if ($o_pass);
            print "  o_ratio = " . sprintf("%.2g", $lo_ratio_min);
            print color('reset') if ($o_pass);
            print color('bold') if ($s_pass);
            print "  s_diff = " . sprintf("%.2g", $ls_diff_min);
            print color('reset') if ($s_pass);
            print color('bold') if ($a_pass);
            print "  a_ratio = " . sprintf("%.2g", $a_ratio);
            print color('reset') if ($a_pass);
            print " DISQUALIFIED" if ($disqualify);
            print "\n";
        }

        # prefer lesser input load since this will tend to bring transistor
        # sizes down.  TODO: Refine selection metric, using a combination 
        # of area cost & convergence goodness.  Perhaps define a configurable
        # parameter for this.
        if ($i_best && $o_pass && $t_pass && $s_pass && $a_pass && 
            !$disqualify) {
            $target = $subtype;
            $t_ratio = $lt_ratio;
            $i_ratio = $li_ratio_max;
            $o_ratio = $lo_ratio_min;
        }
    }

    # return what we found (or undef if nothing satisfied the limits)
    return ($target, $t_ratio, $i_ratio, $o_ratio, $a_ratio);
}


1;
