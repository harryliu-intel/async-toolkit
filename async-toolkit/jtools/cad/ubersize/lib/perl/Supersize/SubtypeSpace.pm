#
# SubtypeSpace
#
# Supersize module for handling major changes to the layout subtype
# space, i.e. creating and updating the entire hierarchy.
#
#

package Supersize::SubtypeSpace;

BEGIN {
    use Exporter;
    our @ISA = qw(Exporter);
    our @EXPORT = qw(
        &look_for_subtype_of
        &look_for_subtype
        &find_next_available_subtype
    );
    our @EXPORT_OK = qw( 
        &get_module_data
    );
}

use strict;
use Supersize::Util;
use Supersize::TypeUtil;
use Supersize::JavaUtil;
use Supersize::ModifySubtypes;

my $GEN_SUBTYPES_GLOBALS_r = { 
    TOP              => { REQUIRED => 1 },
    TAU              => { REQUIRED => 1 },
    WORK_DIR         => { REQUIRED => 1 },
    CAST_DIR         => { REQUIRED => 1 },
    SPEC_DIR         => { REQUIRED => 1 },
    PDK_ROOT         => { REQUIRED => 1 },
    LAYOUT_ATTRIBUTE => { REQUIRED => 0 }
};

sub get_module_data {
  return { 
    NAME => "SubtypeSpace",
    DESC => "Commands for creating and updating the entire layout ".
            "subtype space.",
    COMMANDS => {
    "create_subtypes"    => {
        SUBREF => \&create_subtypes,
        USAGE  => "create_subtypes [--dir=" . underline("cast_dir") . "]",
        DESC   =>
          "Formerly known as 'ubersize spec'.  Creates a new hierarchy of ".
          "layout subtypes under WORK_DIR/cast for the cell referenced by ".
          "the TOP global variable.  Working directory is created if " .
          "necessary.\n\n" .
          "In contrast to ubersize's spec action, create_subtypes does " .
          "not erase all contents of WORK_DIR/cast prior to generating the ".
          "new subtypes.  Thus it is entirely safe to run create_subtypes ".
          "in-place on a subcell of the existing subtype hierarchy.\n\n" .
          "The optional --dir argument sets the directory name within the ".
          "working directory into which the cast subtype hierarchy will be ".
          "written. It defaults to 'cast'.",
        GLOBALS => $GEN_SUBTYPES_GLOBALS_r },
    "update_subtypes"   => {
        SUBREF => \&update_subtypes,
        USAGE  => "update_subtypes [--src=" . underline("src_dir") . "] " .
                                  "[--match-top-level-cells] " . 
                                  "[--no-split | --split-from=".
                                  underline("num")."] " .
                                  "[--restore-size[=".
                                  underline("strength_file")."]]" ,
        DESC   =>
          "Regenerates the TOP cell's layout subtype hierarchy, preserving as ".
          "much of the original (source) hierarchy as possible.  As long as ".
          "instances are merely added, deleted, or inlined in the base Cast, ".
          "the subtypes of all original (remaining) instances will be ".
          "preserved. Only a single level of inlining is supported (i.e. ".
          "don't inline a cell and then inline one of its subcells.)  Note " .
          "you can always work around this limitation by running " .
          "update_subtypes in stages\n\n".
          "The default behavior of update_subtypes is to split each added ".
          "instance to an unused subtype number (beginning from the subtype ".
          "number of TOP, or from ".underline("num")." if specified).  ".
          "To disable this behavior, supply the --no-split ".
          "option.  This will cause all added cells to take the same subtype ".
          "number as that of TOP.\n\n" .
          "Note: This command will not preserve the original leaf cell " .
          "netlists.  All leaf cell subtype netlist blocks are " .
          "regenerated from scratch.  However, all subtype directives " .
          "are preserved from the source hierarchy.\n\n" .
          "By default " . underline("src_dir") . "=\$WORK_DIR/cast, meaning " .
          "the subtype hierarchy is regenerated in place.\n\n" .
          "If --restore-size is specified, then half operator sizes will " .
          "be read from " . underline("strength_file") . ", and instead of " .
          "initial sizes, any half operator, identified by the name of the " .
          "output node and type name, that still exists will be assigned " .
          "the size read from the file.  If " . underline("strength_file") .
          " is not specified, then it defaults to hsizes.debug, which is " .
          "automatically generated after sizing.  This can be used to " .
          "quickly iterate when adding charge sharing directives.  Use with " .
          "caution if the netlist is substantially different from the " .
          "netlist that the " . underline("strength_file") . " was generated " .
          "from." ,
        EXTRA_DESC  =>
          "When --match-top-level-cells is specified, the TOP cell and its ".
          "children do not necessarily need to exist in the source hierarchy. ".
          "The destination subtype hierarchy will be left at default subtype ".
          "numbers until some cell BASE.NUM from the source tree matches a ".
          "base cell name BASE in the destination tree.  The destination " .
          "subtype will be changed to NUM and from that point down in the ".
          "hierarchy, the source hierarchy will be regenerated as usual. " .
          "Note: In this mode, matching begins with the subcells of TOP; " .
          "the top-level cell itself will not be changed to a matching " .
          "source subtype.\n\n" .
          "For sizing drop aggregation purposes, a directory path " .
          "may be supplied for " .  underline("src_dir") . ".  This path " .
          "may either be specified as a colon-separated scalar argument " .
          "or as a list type.  In this mode, matching hierarchy from any " .
          "of the source subtype directories will be regenerated.  Whenever ".
          "the same subtypes exist in multiple source directories, " .
          "left-to-right precedence as supplied in the " . 
          underline("src_dir") . " " .  "argument applies.\n\n" .
          "One final subtlety: if a particular source cell references a " .
          "subtype that does not exist in the source WORK_DIR, then it is ".
          "assumed that the subtype is fixed-size and located under " .
          "SPEC_DIR (or else intended to be.)  Thus such subtypes are " .
          "deleted from the destination WORK_DIR hierarchy if they " .
          "initially exist.  This stays consistent with the intent of " .
          "making the destination hierarchy match the source hierarchy.",
        GLOBALS => $GEN_SUBTYPES_GLOBALS_r,
        RV     => {
          new_instances => { 
            TYPE => $TYPE_MAP,
            DESC => "Map of subtype -> ( new_inst1 new_inst2 ... ), where " .
                    "new_inst_i are instances within the subtype which did ".
                    "not exist in the source subtype hierarchy." },
          deleted_instances => { 
            TYPE => $TYPE_MAP,
            DESC => "Map of subtype -> ( del_inst1 del_inst2 ... ), where " .
                    "del_inst_i are instances within the source subtype " .
                    "which no longer exist in the subtype hierarchy." },
          matched_subtypes => {
            TYPE => $TYPE_LIST,
            DESC => "List of all subtypes which were successfully matched ".
                    "against the source subtype hierarchy." },
          new_subtypes => {
            TYPE => $TYPE_MAP,
            DESC => "Map of src_subtype -> ( new_subtype1 new_subtype2 ... ), ".
                    "for all new subtypes that were created (for newly ".
                    "added instances) with src_subtype as the original " .
                    "(unsplit) source subtype." } } },
    "clean_subtypes" => {
        SUBREF => \&clean_subtypes,
        USAGE  => "clean_subtypes --used [--in-spec] | --unused | --all",
        DESC   =>
          "Cleans the WORK_DIR/cast subtype hierarchy by deleting ".
          "the subtypes that are instantiated under TOP (--used), ".
          "the subtypes that are NOT instantiated under TOP (--unused), ".
          "or all subtypes (--all).  If --in-spec is given with the --used ".
          "option, then only the subtypes that are instantiated under TOP ".
          "AND also exist under SPEC_DIR will be deleted.\n\n" .
          "Obviously, much care should be exercised when running this " .
          "command.  However, if you make a mistake, the old hierarchy " .
          "will be saved in WORK_DIR/cast.save, whether or not SAVE_BACKUPS ".
          "is set.",
        GLOBALS => {
          CAST_DIR => { REQUIRED => 1 },
          SPEC_DIR => { REQUIRED => 1 },
          WORK_DIR => { REQUIRED => 1 },
          TOP      => { REQUIRED => 1 } } },
    "split_subtypes" => {
        SUBREF => \&split_subtypes,
        USAGE  => "split_subtypes [--range=".underline("from")."[:".
                                             underline("to")."]] ".
                                 "[".underline("subtypes")."] ..." ,
        DESC   =>
          "Splits the subtype space of TOP.  Behaves just like " .
          "ubersize's split_spec action, except that only instances with ".
          "initially conflicting subtype numbers will be resubtyped.  That ".
          "is, if the input subtype space is already fully split, no change ".
          "will be made. (Ubersize's split_spec translates all subtypes ".
          "upwards in the subtype range).\n\n".
          "Split can be optionally restricted to a range of source subtypes ".
          "[".underline("from").":".underline("to")."] using the --range ".
          "option.\n\n" .
          "If no ".underline("subtypes")." are specified, then only ".
          "splittable subtypes (i.e., those with the splittable directives ".
          "set to true) will be split. Otherwise, the specified subtypes ".
          "will be split without regard to the setting of the splittable ".
          "directives.  Lists are usable in this context.  For ".
          "example, the default behavior with no subtypes specified is " .
          "equivalent to:\n\n".
          "  query subcells --filter=\"directive=splittable:true\"\n".
          "  split_subtypes \$query.result\n\n".
          "To propagate the splitting to your floorplan, pass the split_map ".
          "return variable to ".bold("copy_floorplan").":\n\n".
          "  split_subtypes\n" .
          "  copy_floorplan \$split_subtypes.split_map",
        RV => {
          split_map => {
            TYPE => $TYPE_MAP,
            DESC => "Map of original_subtype -> ( new_subtype1 new_subtype2 ".
                    "... ) specifying what subtypes where created.  This ".
                    "map can be passed to copy_floorplan in order to ".
                    "achieve the same subtype translations in dfII." } },
        GLOBALS => {
          SPEC_DIR => { REQUIRED => 1 },
          WORK_DIR => { REQUIRED => 1 },
          TOP      => { REQUIRED => 1 } } },
    "compact_subtypes" => {
        SUBREF => \&compact_subtypes,
        USAGE  => "compact_subtypes [--to=".underline("compact-num")."] ".
                                   "[--range=".underline("from").":".
                                               underline("to")."]",
        DESC   =>
          "Compacts the working directory subtype space to " .
          underline("compact-num").", if specified, or 0 otherwise.  " .
          "Compaction can be optionally restricted to a range of source ".
          "subtypes [".underline("from").":".underline("to")."].",
        RV => {
          result => {
            TYPE => $TYPE_MAP,
            DESC => "Map of original_subtype -> compacted_subtype specifying ".
                    "where each subtype was compacted to.  Can be passed to ".
                    "copy_floorplan to compact the floorplan views." } },
        GLOBALS => {
          CAST_DIR     => { REQUIRED => 1 },
          SPEC_DIR     => { REQUIRED => 1 },
          WORK_DIR     => { REQUIRED => 1 },
          PACKAGE_ROOT => { REQUIRED => 1 },
          TOP          => { REQUIRED => 1 } } },
    "copy_subtypes" => {
        SUBREF => \&copy_subtypes,
        USAGE  => "copy_subtypes [--src-dir=".underline("src")."] ".
                                "[--dest-dir=".underline("dest")."] ".
                                "[--make-writeable] ".
                                "[--skip-if-exists|abort-if-exists] ".
                                "[".underline("cell-list")."] ".
                                "[".underline("fqcn")."[:".
                                    underline("dest_fqcn")."]] ...",
        DESC   =>
          "Copies subtypes between two cast subtype directories.  If only ".
          "the source or the destination subtype root directory is specified, ".
          "the other will be set to either WORK_DIR/cast or SPEC_DIR, ".
          "depending on the value of the specified directory.  If one is ".
          "WORK_DIR/cast, the other will be set to SPEC_DIR and vice versa.  ".
          "If neither a ".underline("src")." nor a ".underline("dest")." is ".
          "specified, subtypes will be copied from SPEC_DIR to ".
          "WORK_DIR/cast, unless a subtype renaming is in effect (with ".
          underline("fqcn").":".underline("dest_fqcn")."), in which case ".
          "both source and destination directories will be WORK_DIR/cast.\n\n".
          "If no ".underline("cell-list")." or individual ".underline("fqcn").
          " cells are specified, all cells under TOP will be copied.  The ".
          underline("fqcn").":".underline("dest_fqcn")." form allows ".
          "subtypes to be renamed.  If ".underline("dest_fqcn")." is a ".
          "number, the source subtype's base cell name will be used.\n\n".
          "By default, existing subtypes in the ".
          "destination hierarchy will be overwritten (even if they are ".
          "unwriteable but owned by you); use --skip-if-exists or ".
          "--abort-if-exists to change this behavior.  To make all copied ".
          "subtypes writeable, specify --make-writeable.",
        GLOBALS => {
          CAST_DIR     => { REQUIRED => 1 },
          SPEC_DIR     => { REQUIRED => 1 },
          WORK_DIR     => { REQUIRED => 1 },
          TOP          => { REQUIRED => 0 } } },
    "regenerate_leaf" => {
        SUBREF => \&regenerate_leaf,
        USAGE  => "regenerate_leaf [--no-refresh] ".
                                  "[".underline("cell") . " | "
                                     .underline("cell-list") . "] ...",
        DESC   =>
          "Regenerates netlist of the listed leaf cell subtypes taking into ".
          "account the settings of their current directives.  By default, ".
          "the CAST server will be refreshed at the start, because ".
          "presumably the CAST has been modified to necessitate the ".
          "regeneration; to disable this behavior use --no-refresh.",
        GLOBALS => {
          CAST_DIR => { REQUIRED => 1 },
          SPEC_DIR => { REQUIRED => 1 },
          WORK_DIR => { REQUIRED => 1 },
          TOP      => { REQUIRED => 1 } } },
    "optimize_threshold" => {
        SUBREF => \&optimize_threshold,
        USAGE  => "optimize_threshold [".underline("cell") . " | "
                                        .underline("cell-list") . "] ...",
        DESC   =>
          "Optimize the threshold of the the given leaf cells (if not " .
          "specified, defaults to all non-fixed size leaf cells under TOP " .
          "without a netlist block in CAST). " .
          "All input variables can also be overridden on the commandline.",
        GLOBALS => {
          CAST_DIR => { REQUIRED => 1 },
          SPEC_DIR => { REQUIRED => 1 },
          WORK_DIR => { REQUIRED => 1 },
          TOP      => { REQUIRED => 1 } },
        IV => {
          fast_non_leaky => {
            TYPE => $TYPE_SCALAR,
            DESC => "FAST in non-leaky direction.  ".
                    "Defaults to false.",
            REQUIRED => 0 },
          fast_2off_nmos => {
            TYPE => $TYPE_SCALAR,
            DESC => "FAST in leaky direction with 2-off NMOS in series.  " .
                    "Defaults to false.",
            REQUIRED => 0 },
          fast_2off_pmos => {
            TYPE => $TYPE_SCALAR,
            DESC => "FAST in leaky direction with 2-off PMOS in series.  " .
                    "Defaults to false.",
            REQUIRED => 0 },
          slow_1off_nmos => {
            TYPE => $TYPE_SCALAR,
            DESC => "SLOW in leaky direction with 1-off NMOS in series.  " .
                    "Defaults to false.",
            REQUIRED => 0 },
          slow_1off_pmos => {
            TYPE => $TYPE_SCALAR,
            DESC => "SLOW in leaky direction with 1-off PMOS in series.  " .
                    "Defaults to false.",
            REQUIRED => 0 },
          fast_vs_weak_allowed => {
            TYPE => $TYPE_SCALAR,
            DESC => "Allow FAST opposing weak staticizer.  " .
                    "Defaults to false.",
            REQUIRED => 0 },
          slow_vs_weak_allowed => {
            TYPE => $TYPE_SCALAR,
            DESC => "Allow SLOW opposing weak staticizer.  " .
                    "Defaults to false.",
            REQUIRED => 0 } } }
    },
    GLOBALS => {
        TAU => { DESC => "The tau value has no effect on any " .
          "SubtypeSpace commands, but Jauto requires that it be " .
          "specified.  (The tau directive will be set to this in all new ".
          "subtypes.)" }
    }
  };
}

#
# create_subtypes
#
sub create_subtypes {
    # Get reference to global variables
    my $SS_r   = shift;
    my $GS_r   = $SS_r->{GS};

    # Parse arguments
    my $cdir = "cast";
    while (num_args(\@_)) {
        my $arg = shift_next_scalar(\@_);
        if ($arg eq "--dir") {
            my $eq = shift_next_scalar(\@_);
            $cdir = shift_next_scalar(\@_);
            if ($eq ne "=" || $cdir eq "") {
                command_die($SS_r, "Invalid arguments to --dir.");
            }
        }
        else {
            command_die($SS_r, "Unknown create_subtypes argument '$arg'.");
        }
    }

    # Set up command and run Jauto
    if (gen_subtypes_common($SS_r, $cdir, 1, undef, undef) != 0) {
        command_die($SS_r, "Error: Failed to generate subtypes.\nSee " .
                           "create_subtypes.out for more information.");
    }

    # Remove saved cast directory
    if (-e "$GS_r->{WORK_DIR}/$cdir.save" && !$GS_r->{JUST_PRINT} &&
        !$GS_r->{SAVE_BACKUPS}) {
        `rm -rf "$GS_r->{WORK_DIR}/$cdir.save"`;
    }

    # Definitely dirties everything
    set_dirty($SS_r, "all");
}


sub regenerate_leaf {
    # Get reference to global variables
    my $SS_r   = shift;
    my $GS_r   = $SS_r->{GS};
    my $cmd    = "SubtypeSpace/regenerate_leaf";
    my $wdir   = get_work_dir($SS_r);
    my $fresh  = 1;

    # Parse arguments
    my @leafs  = ();
    while (num_args(\@_)) {
        my ($type, $value) = shift_next_arg(\@_);
        if (defined $type && $type == $TYPE_SCALAR) {
            if ($value =~ '^--') {
                if ($value eq '--no-refresh') {
                    $fresh = 0;
                } else {
                    command_die($SS_r, "Invalid option '$value'.");
                }
            } else {
                push @leafs, $value;
            }
        }
        elsif (defined $type && $type == $TYPE_LIST) {
            push @leafs, @{$value};
        }
        else {
            command_die($SS_r, "Invalid regenerate_leaf type for argument '$value'.");
        }
    }

    if (@leafs) {
        set_dirty($SS_r, "all") if $fresh;

        backup_directory($SS_r, "cast") if ($SS_r->{GS}{SAVE_BACKUPS});

        # Generate the new preliminary subtype hierarchy
        foreach my $leaf (@leafs) {
            if (gen_subtypes_common($SS_r, "cast", 1, undef, \@leafs,
                                    $leaf, 1) != 0) {
                command_die($SS_r, "Failed to regenerate $leaf.\n" .
                            "See regenerate_leaf.out for more information.\n");
            }
        }

        # Dirties everything
        set_dirty($SS_r, "all");
    }
    else {
        command_die($SS_r, "No leaf cell specified.\n");
    }
}

sub optimize_threshold {
    # Get reference to global variables
    my $SS_r   = shift;
    my $GS_r   = $SS_r->{GS};
    my $cmd    = "SubtypeSpace/optimize_threshold";
    my $wdir   = get_work_dir($SS_r);
    my $fresh  = 1;

    my %opts    = map { $_ => 'false' } get_cmd_input_variables($SS_r);

    # get global settings
    foreach my $iv (keys %opts) {
        my $val = get_cmd_input_scalar($SS_r, $iv);
        $opts{$iv} = $val if defined $val;
    }

    # Parse arguments
    my @leafs  = ();
    while (num_args(\@_)) {
        my ($type, $value) = shift_next_arg(\@_);
        if (defined $type && $type == $TYPE_SCALAR) {
            if ($value =~ '^--') {
                (my $opt = $value) =~ s/^--//;
                if (exists $opts{$opt}) {
                    my $eq = shift_next_scalar(\@_);
                    $opts{$opt} = shift_next_scalar(\@_);
                    if ($eq ne "=" || $opts{$opt} eq "") {
                        command_die($SS_r, "Invalid argument to --$opt.");
                    }
                }
                else {
                    command_die($SS_r, "Invalid option '$value'.");
                }
            }
            else {
                push @leafs, $value;
            }
        }
        elsif (defined $type && $type == $TYPE_LIST) {
            push @leafs, @{$value};
        }
        else {
            command_die($SS_r, "Invalid optimize_threshold type for argument" .
                               " '$value'.");
        }
    }

    my $extra_args = [ "--optimizeThreshold=" .
                       join('|', map { "$_:" . ($opts{$_} eq 'true' ? 'true'
                                                                    : 'false') }
                                     sort keys %opts)
                     ];

    # if no subtypes specified, query for all non-fixed leaf subtypes without a
    # manually written netlist block (i.e., if refinement parent has a netlist
    # block)
    if (@leafs == 0) {
        my $query_cmd = "--filter=leaf&!fixed&!parent:block=netlist " .
                        "--cell=$GS_r->{TOP} --task=subcells";
        my $result_r = [];
        query_cast_server($SS_r, $query_cmd, $result_r, 1);
        if (scalar(@{$result_r}) == 0) {
            print "No non-fixed size leaf subtypes without netlist block in " .
                  "CAST found.\n";
            return;
        }
        @leafs = @{$result_r};
    }

    set_dirty($SS_r, "all") if $fresh;

    backup_directory($SS_r, "cast") if ($SS_r->{GS}{SAVE_BACKUPS});

    # Generate the new preliminary subtype hierarchy
    foreach my $leaf (@leafs) {
        if (gen_subtypes_common($SS_r, "cast", 1, undef, \@leafs,
                                $leaf, 1, $extra_args) != 0) {
            command_die($SS_r, "Failed to regenerate $leaf.\n" .
                        "See optimize_threshold.out for more information.\n");
        }
    }

    # Dirties everything
    set_dirty($SS_r, "all");
}

#
# update_subtypes
#
sub update_subtypes {
    # Get reference to global variables
    my $SS_r   = shift;
    my $GS_r   = $SS_r->{GS};
    my $cmd    = "SubtypeSpace/update_subtypes";
    my $wdir   = get_work_dir($SS_r);

    # Parse arguments
    my @src_path1    = ("$wdir/cast");
    my $matched      = 1;
    my $split_from   = subtype_number_of($GS_r->{TOP});
    my $restore_size;
    while (num_args(\@_)) {
        my $arg = shift_next_scalar(\@_);
        if ($arg eq "--src") {
            my $eq = shift_next_scalar(\@_);
            my ($type, $value) = shift_next_arg(\@_);
            if (defined $type && $type == $TYPE_SCALAR) {
                @src_path1 = split /:/, $value;
            }
            elsif (defined $type && $type == $TYPE_LIST) {
                @src_path1 = @{$value};
            }
            else {
                command_die($SS_r, "Bad arguments to --src.");
            }
            command_die($SS_r, "Bad argument to --src.") 
                if ($eq ne "=" || !@src_path1);
        }
        elsif ($arg eq "--match-top-level-cells") {
            $matched = 0;
        }
        elsif ($arg eq "--no-split") {
            $split_from = -1;
        }
        elsif ($arg eq "--split-from") {
            my $eq = shift_next_scalar(\@_);
            $split_from = shift_next_scalar(\@_);
            if (!defined $eq || $eq ne "=" || !defined $split_from) {
                command_die($SS_r, "Bad argument to --split-from.");
            }
        }
        elsif ($arg eq "--restore-size") {
            if (next_scalar(\@_) eq '=') {
                shift_next_scalar(\@_);
                $restore_size = shift_next_scalar(\@_);
                if (!defined $restore_size) {
                    command_die($SS_r, "Bad argument to --restore-size.");
                }
            }
            else {
                $restore_size = $SS_r->{GS}{WORK_DIR} . '/hsizes.debug';
            }
        }
        else {
            command_die($SS_r, "Unknown update_subtypes argument '$arg'.");
        }
    }

    if (defined $restore_size && !-r $restore_size) {
        command_die($SS_r, "Strength file '$restore_size' is unreadable.");
    }

    # Check source path
    my $dest_dir = "$wdir/cast";
    push @src_path1, $SS_r->{GS}{SPEC_DIR} if (!$matched);
    my @src_path;
    foreach my $src_dir (@src_path1) {
        if ($src_dir eq $dest_dir) {
            push @src_path, "$dest_dir.save";
        }
        elsif (! -d $src_dir) {
            print STDERR "Warning: Directory $src_dir doesn't exist.\n";
        }
        else {
            push @src_path, $src_dir;
        }
    }
    if (!@src_path) {
        command_die($SS_r, "No valid source directories to work on.")
    }

    # Restart cast server since the cast has presumably changed...
    set_dirty($SS_r, "all");

    # Generate the new preliminary subtype hierarchy
    if (gen_subtypes_common($SS_r, "cast", 1, $restore_size, undef) != 0) {
        command_die($SS_r, "Failed to generate initial subtypes.\n" .
                    "See update_subtypes.out for more information.\n");
    }

    # Recursively regenerate subtypes
    my %visited_subtypes = ();
    $SS_r->{RV}{$cmd}{new_instances}     = [ $TYPE_MAP, {} ];
    $SS_r->{RV}{$cmd}{deleted_instances} = [ $TYPE_MAP, {} ];
    $SS_r->{RV}{$cmd}{matched_subtypes}  = {};
    $SS_r->{RV}{$cmd}{new_subtypes}      = [ $TYPE_MAP, {} ];
    eval {
        update_subtypes_recursively($SS_r, \@src_path, $dest_dir, 
            $SS_r->{GS}{TOP}, "", \%visited_subtypes, $matched, 
            $split_from, undef, undef, $GS_r->{VERBOSE});
    };
    if ($@) {
        # Restore backup directory
        print STDERR "Restoring original subtypes.\n";
        restore_directory($SS_r, "cast");
        die;
    }
    elsif (!$SS_r->{GS}{SAVE_BACKUPS}) {
        delete_backup_directory($SS_r, "cast");
    }

    print "Done.\n";

    # Put matched_subtype list into proper form
    my @matched_subtypes = keys %{$SS_r->{RV}{$cmd}{matched_subtypes}};
    $SS_r->{RV}{$cmd}{matched_subtypes} = 
        [ $TYPE_LIST, \@matched_subtypes ];

    # Remove source cast directory (when src_path==dest_dir)
    if (-e "$dest_dir.save" && !$GS_r->{JUST_PRINT} && !$GS_r->{SAVE_BACKUPS}) {
        `rm -rf "$dest_dir.save"`;
    }

    # Dirties everything
    set_dirty($SS_r, "all");
}


#
# split_subtypes
#
sub split_subtypes {
    my $SS_r = shift;
    my $GS_r = $SS_r->{GS};
    my ($mod, $cmd) = get_active_command($SS_r);
    my $wdir        = get_work_dir($SS_r);

    # Parse arguments
    my $split_from = subtype_number_of($SS_r->{GS}{TOP});
    my $split_to;
    my %split_list = ();
    while (num_args(\@_)) {
        my @arg = shift_next_arg(\@_);
        if ($arg[0] == $TYPE_SCALAR && $arg[1] eq "--range") {
            my $eq = shift_next_scalar(\@_);
            my $range = shift_next_scalar(\@_);
            if ($eq eq "=" && $range =~ /^(\d+)(:\d*)?$/) {
                $split_from = $1;
                if ($2 ne ":") {
                    $split_to = substr $2, 1;
                }
            }
            else {
                command_die($SS_r, "Bad $arg[1] argument.");
            }
        }
        elsif ($arg[0] == $TYPE_SCALAR && $arg[1] =~ /^--/) {
            command_die($SS_r, "Unknown split_subtypes option '$arg[1]'.");
        }
        elsif ($arg[0] == $TYPE_SCALAR) {
            $split_list{$arg[1]} = 1;
        }
        elsif ($arg[0] == $TYPE_LIST) {
            foreach my $elem (@{$arg[1]}) {
                $split_list{$elem} = 1;
            }
        }
        else {
            command_die($SS_r, "Unknown split_subtypes argument '$arg[1]'.");
        }
    }


    # if no subtypes specified, query for all splittable subtypes
    if (scalar(keys %split_list) == 0) {
        my $query_cmd = "--filter=directive=splittable:true " .
                        "--cell=$GS_r->{TOP} --task=subcells";
        my $result_r = [];
        query_cast_server($SS_r, $query_cmd, $result_r, 1);
        if (scalar(@{$result_r}) == 0) {
            print "No splittable subtypes found.\n";
            return;
        }
        foreach my $cell (@{$result_r}) {
            $split_list{$cell} = 1;
        }
    }

    # Directory setup
    backup_directory($SS_r, "cast") if ($SS_r->{GS}{SAVE_BACKUPS});

    # Initialize variables
    my @src_path   = ();
    my %visited_subtypes = ();
    $SS_r->{RV}{"$mod/$cmd"}{split_map} = [ $TYPE_MAP, {} ];

    my $is_splittable;
    my %unsplittable;
    if (scalar(keys %split_list) == 0) {
        $is_splittable = undef;
    } else {
        $is_splittable = {
            'query' => sub {
                my $cell = $_[0]->[1];
                if (exists($unsplittable{$cell})) {
                    return 0;
                } else {
                    if (!exists($split_list{$cell})) {
                        $unsplittable{$cell} = 1;
                    }
                    return 1;
                }
            },
            'add'   => sub {
                $split_list{$_[0]} = 1;
            }
        };
    }

    # Recursively split all subtypes within specified range
    eval {
        update_subtypes_recursively($SS_r, \@src_path, 
            "$wdir/cast", $SS_r->{GS}{TOP}, "", 
            \%visited_subtypes, 1, $split_from, $split_to, $is_splittable,
            2*$SS_r->{GS}{VERBOSE});
    };
    if ($@) {
        # Restore backup directory
        print STDERR "Restoring original subtypes.\n";
        restore_directory($SS_r, "cast");
        die;
    }
    elsif (!$SS_r->{GS}{SAVE_BACKUPS}) {
        delete_backup_directory($SS_r, "cast");
    }

    # Check if anything was split
    my $num_split = scalar(values %{$SS_r->{RV}{"$mod/$cmd"}{split_map}[1]});
    if ($num_split > 0) {
        print "Split $num_split subtypes.\n";
    }
    else {
        print "No subtypes to split.\n";
    }

    # Dirties everything (well, not really -- refine later)
    set_dirty($SS_r, "all") if ($num_split > 0);
}


# Recursive workhorse function
# Splits any new instances if split_from>0.
#   verbose_mode - 0: no user feedback output
#                  1: update_subtypes verbose output
#                  2: split_subtypes verbose output
#
sub update_subtypes_recursively {
    my $SS_r         = shift;
    my $src_path_r   = shift;
    my $dest_dir     = shift;
    my $fqcn         = shift;
    my $inst_path    = shift;
    my $vs_r         = shift;
    my $matched      = shift;
    my $split_from   = shift;
    my $split_to     = shift;
    my $is_splittable= shift;
    my $verbose_mode = shift;

    my ($mod, $short_cmd) = get_active_command($SS_r);
    my $cmd = "$mod/$short_cmd";

    # Locate and read source subtype file, get instance map if mid-level cell
    my $src_dir = look_for_subtype($fqcn, $src_path_r);
    my ($src_cell_type, $src_instmap_r, $src_head_r, $src_tail_r, $src_fixed);
    if (defined $src_dir) {
        ($src_cell_type, $src_instmap_r, $src_head_r, $src_tail_r, $src_fixed) =
            read_subtype($SS_r, $src_dir, $fqcn);
    }
    else {
        $src_cell_type = $MISSING_CELL_TYPE;
    }

    if ($src_cell_type != $MISSING_CELL_TYPE && 
        $src_cell_type != $INVALID_CELL_TYPE && $matched) {
        # Record this cell as a matched subtype
        $SS_r->{RV}{$cmd}{matched_subtypes}{$fqcn} = 1;
    }

    # Read preliminary destination subtype
    my ($dest_cell_type, $dest_instmap_r, $dest_head_r, $dest_tail_r) =
        read_subtype($SS_r, $dest_dir, $fqcn);

    if ($dest_cell_type == $MID_CELL_TYPE && 
        $src_cell_type != $MID_CELL_TYPE) {
        # Clear the source instance->subtype map (if it's a leaf cell,
        # this data structure is used to hold the netlist block).
        $src_instmap_r = {};
    }

    # Debug
    if ($SS_r->{GS}{DEBUG}) {
        print ">> In $fqcn ($inst_path), type $dest_cell_type\n";
        if ($src_cell_type == $MID_CELL_TYPE && 
            defined $src_instmap_r && defined $dest_instmap_r) {
            print "-- keys src_map = " . scalar(keys(%{$src_instmap_r})) . 
                    " keys dest_map = ".scalar(keys(%{$dest_instmap_r}))."\n";
            print "--";
            foreach my $inst (keys %{$src_instmap_r}) {
                print " $inst";
            }
            print "\n";
        }
    }

    if ($dest_cell_type == $LEAF_CELL_TYPE) {
        # it's now a leaf cell, nothing can be subtyped
        $vs_r->{$fqcn} = 1;

        # check if it used to be a mid-level cell
        if ($src_cell_type == $MID_CELL_TYPE) {
            # Add all source instances to the deleted instance map
            if ($verbose_mode==1) {
                print "Cell $fqcn (" .  $inst_path . ") is now a leaf cell.\n";
            }
            if ($cmd =~ /update_subtypes/) {
                foreach my $inst (keys %{$src_instmap_r}) {
                    if (!defined $SS_r->{RV}{$cmd}{deleted_instances}[1]{$fqcn}) {
                        $SS_r->{RV}{$cmd}{deleted_instances}[1]{$fqcn} = 
                            [ $TYPE_LIST, [] ];
                    }
                    push @{${$SS_r->{RV}{$cmd}{deleted_instances}[1]{$fqcn}}[1]}, 
                            $inst;
                }
            }
        }
        elsif ($src_cell_type == $LEAF_CELL_TYPE) {
            if ($verbose_mode==1) {
                print "Leaving leaf cell $fqcn ($inst_path) as-is.\n";
            }
        }
    }
    elsif ($dest_cell_type == $MID_CELL_TYPE) {
        # Mid-level cell.  Determine proper instance->subtype map

        # Table of instance subtype maps of all inlined cells
        my %inlined_instmaps;

        # Map of subtypes assigned to arrayed instances (for splitting)
        my %array_subnums;

        # Iterate through each new instance, compare to source instance
        foreach my $inst (keys %{$dest_instmap_r}) {
            my $ddata = $dest_instmap_r->{$inst};
            my $sdata = $src_instmap_r->{$inst};

            if ($SS_r->{GS}{DEBUG}) {
                print "-- instance $inst:\n";
                print "--   src: " . (defined $sdata ? $sdata->[1] : "(none)") .
                    " dst: $ddata->[1]\n";
            }

            # If we haven't matched yet, look for a subtype of this instance's 
            # base cell in the source hierarchy
            if (!$matched) {
                ($src_dir, my $subtype) = 
                    look_for_subtype_of(basetype_of($ddata->[1]), $src_path_r);
                if (defined $subtype) {
                    $sdata = [ $ddata->[0], $subtype ] if (defined $subtype);
                    if ($verbose_mode==1) {
                        print "Matched $ddata->[1] to $subtype.\n";
                    }
                }
            }

            # Check if instance belongs to a subcell inlined from src subtype
            if (!defined $sdata && $inst =~ /\./) {
                my @comps = split (/\./, $inst);
                my $inlined_inst = "";
                my $idx = 0;
                while (($idx = index $inst, ".", $idx+1) != -1) {
                    if (grep { $_ eq substr($inst, 0, $idx) } 
                            keys %{$src_instmap_r}) {
                        $inlined_inst = substr($inst, 0, $idx);
                        last;
                    }
                }
                if ($inlined_inst ne "") {
                    my $subcell_inst = substr $inst, $idx+1;
                    my $inlined_subtype = $src_instmap_r->{$inlined_inst}[1];
                    if (!defined $inlined_instmaps{$inlined_subtype}) {
                        if ($verbose_mode==1) {
                            print "Identified inlined cell: $inlined_subtype ". 
                                "($inst_path/$inlined_inst)\n";
                        }
                        my $inlined_src_dir = 
                            look_for_subtype($inlined_subtype, $src_path_r);
                        if (defined $inlined_src_dir) {
                            (my $inlined_src_type, 
                             $inlined_instmaps{$inlined_subtype}) = 
                                (read_subtype($SS_r, $inlined_src_dir, 
                                    $inlined_subtype))[0..1];
                             if ($inlined_src_type != $MID_CELL_TYPE ||
                                 !$inlined_instmaps{$inlined_subtype}) {
                                 $inlined_instmaps{$inlined_subtype} = {};
                             }
                        }
                    }
                    # Note this may returned undef if the inlined source
                    # subtype is incompatible with the destination subcells.
                    # That's okay, though.
                    $sdata = $inlined_instmaps{$inlined_subtype}->
                                    {$subcell_inst};
                    
                    if (defined $sdata && $verbose_mode==1) {
                        print "Restoring inlined instance: $sdata->[1] " .
                              "($inst_path/$inst)\n";
                    }

                    # Delete src instance so later we know which ones didn't
                    # appear in destination
                    #delete $src_instmap_r->{$inlined_inst};
                }
            }

            # Adopt source instance subtype if it still exists and has the
            # same base type in the destination.  Apply this procedure
            # recursively to the reused subtype.
            if (defined $sdata && $sdata->[0] eq $ddata->[0]) {
                # Instance exists in both and have the same base type
                my $subtype = $sdata->[1];
                my $sfile = fqcn_to_file($src_dir, $subtype);
                my $dfile = fqcn_to_file($dest_dir, $subtype);

                if ($verbose_mode==1) {
                    print "Restoring subtype $subtype ($inst_path/$inst).\n";
                }

                if (-e $sfile && $src_dir ne $SS_r->{GS}{SPEC_DIR}) {
                    # Examine source subtype only if it exists in the source
                    # directory.   (If it doesn't exist [once we've matched],
                    # it must live in SPEC_DIR, which is off limits.)
                    # Copy the subtype 
                    if ($subtype ne $ddata->[1] && !$vs_r->{$subtype} &&
                        -e fqcn_to_file($dest_dir, $ddata->[1])) {
                        
                        my $new_dest_file = fqcn_to_file($dest_dir, $subtype);
                        unlink $new_dest_file if (-e $new_dest_file);
                        copy_subtype($SS_r, $dest_dir, $ddata->[1], $dest_dir, 
                            $subtype);
                    }
                    if (!$vs_r->{$subtype}) {
                        update_subtypes_recursively($SS_r, $src_path_r, 
                            $dest_dir, $subtype, "$inst_path/$inst", $vs_r, 
                            1, $split_from, $split_to, $is_splittable,
                            $verbose_mode);
                    }
                }
                elsif (-e $dfile) {
                    # If this subtype happens to exist in dest dir, and
                    # it does not exist in src dir (but is referenced in
                    # the src subtype), delete it from dest dir
                    unlink $dfile;

                    if ($verbose_mode==1) {
                        print "Deleting subytpe $subtype to match source.\n"
                    }
                }
                # Set dest instance's subtype to mach src
                $dest_instmap_r->{$inst} = $sdata;

                # Delete src instance from map so later we know which ones are
                # left over.
                delete $src_instmap_r->{$inst};
            }
            else {
                # New instance.
                my $new_subtype = $ddata->[1];
                my $dfile = fqcn_to_file($dest_dir, $new_subtype);

                # Find the next available subtype if splitting is turned on
                my $subnum = subtype_number_of($new_subtype);
                if (($subnum =~ /^\d+$/) &&
                    $split_from >= 0 && $subnum >= $split_from &&
                    (!defined $split_to || $subnum <= $split_to) &&
                    (!defined($is_splittable) ||
                     &{$is_splittable->{'query'}}($ddata))) {

                    # Check for array subtype numbering constraint.  Use both
                    # instance name and base type as the key, because the
                    # types of elements in an array may be different.  For
                    # example, this is valid:
                    #
                    # lib.buffer.half.BUFS_1of2(1) bufW[0];
                    # lib.buffer.half.BUFS_1of2(2) bufW[1];
                    my $inst_mod = base_array_instance_name($inst) . '|' .
                                   $ddata->[0];
                    if (exists $array_subnums{$inst_mod}) {
                        $subnum = $array_subnums{$inst_mod};
                        $new_subtype =~ s/\.[^\.]+$/.$subnum/;
                    }
                    else {
                        # New assignment needs to be made; find an available 
                        # number. (note: finds next available number across 
                        # -all- source hierarchies)
                        push my @dir_list, @{$src_path_r}, $dest_dir;

                        # Find subtype number.  Temporarily move the 
                        # preliminary subtype file so it isn't considered in 
                        # the search (unless it's already in use elsewhere)
                        my $pref_num = defined($src_dir) &&
                            -e fqcn_to_file($src_dir, $ddata->[1]) ? 
                            $subnum : $split_from;
                        rename $dfile, "$dfile.update_subtypes_tmp"
                            unless ($vs_r->{$ddata->[1]});
                        $subnum = find_next_available_subtype($SS_r, \@dir_list,
                            basetype_of($ddata->[1]), $pref_num, $split_from,
                            -1);
                            # was: $ddata->[0], $subnum, $split_from, -1);
                        rename "$dfile.update_subtypes_tmp", $dfile
                            unless ($vs_r->{$ddata->[1]});

                        # Remember subtype number if it's an arrayed instance
                        $array_subnums{$inst_mod} = $subnum;

                        # Relocate the preliminary subtype file appropriately
                        $new_subtype =~ s/\.[^\.]+$/.$subnum/;
                        if ($new_subtype ne $ddata->[1]) {
                            # copy to new number
                            copy_subtype($SS_r, $dest_dir, $ddata->[1], 
                                $dest_dir, $new_subtype);
                            if ($cmd =~ /split_subtypes/) {
                                $SS_r->{RV}{$cmd}{split_map}[1]{$ddata->[1]} =
                                    [ $TYPE_LIST, [] ] if (!defined 
                                $SS_r->{RV}{$cmd}{split_map}[1]{$ddata->[1]});
                                push @{$SS_r->{RV}{$cmd}{split_map}[1]
                                    {$ddata->[1]}[1]}, $new_subtype;
                            }
                            &{$is_splittable->{'add'}}($new_subtype)
                                if defined($is_splittable);
                            if ($verbose_mode==2) {
                                print "Splitting $ddata->[1] -> $subnum.\n";
                            }
                            if (!exists $SS_r->{RV}{$cmd}{new_subtypes}
                                                         [1]{$ddata->[1]}) {
                                $SS_r->{RV}{$cmd}{new_subtypes}
                                        [1]{$ddata->[1]} = [ $TYPE_LIST, [] ];
                            }
                            push @{$SS_r->{RV}{$cmd}{new_subtypes}[1]
                                   {$ddata->[1]}[1]}, $new_subtype;
                        }
                    }
                    if ($verbose_mode==1) {
                        print "Assigning new subtype $new_subtype " .
                              "($inst_path/$inst).\n"
                    }
                }
                else {
                    # No splitting, just leave new subtype as is
                    if ($verbose_mode==1) {
                        print "No match for $new_subtype ($inst_path/$inst) " .
                              "in source.\n";
                    }
                }
                # Make the subtype assignment in the instance-to-subtype map
                $dest_instmap_r->{$inst} = [ $ddata->[0], $new_subtype ];

                # Update new_instances return variable map
                if ($cmd =~ /update_subtypes/) {
                    if (!defined $SS_r->{RV}{$cmd}{new_instances}[1]{$fqcn}) {
                        $SS_r->{RV}{$cmd}{new_instances}[1]{$fqcn} = 
                            [ $TYPE_LIST, [] ];
                    }
                    push @{${$SS_r->{RV}{$cmd}{new_instances}[1]{$fqcn}}[1]}, 
                        $inst;
                }

                if (!$matched || !$vs_r->{$new_subtype}) {
                    update_subtypes_recursively($SS_r, $src_path_r, $dest_dir,
                        $new_subtype, "$inst_path/$inst", $vs_r, $matched, 
                        $split_from, $split_to, $is_splittable, $verbose_mode);
                }
            }
        }
    }

    # Write new dest subtype
    if ($dest_cell_type != $MISSING_CELL_TYPE &&
        !write_subtype($SS_r, $dest_dir, $fqcn, $dest_cell_type,
            (defined $src_fixed && $src_fixed && 
             $dest_cell_type == $LEAF_CELL_TYPE ? $src_instmap_r : 
             $dest_instmap_r), 
            $dest_head_r, 
            (defined $src_tail_r && @{$src_tail_r} && 
             $src_cell_type == $dest_cell_type ? $src_tail_r : 
                                                 $dest_tail_r))) {
        print "Error writing subtype $fqcn.\n";
    }

    # Determine leftover instances from source; these were deleted
    if ($src_cell_type == $MID_CELL_TYPE) {
        foreach my $inst (keys %{$src_instmap_r}) {
            if (!defined $SS_r->{RV}{$cmd}{deleted_instances}[1]{$fqcn}) {
                $SS_r->{RV}{$cmd}{deleted_instances}[1]{$fqcn} = 
                    [ $TYPE_LIST, [] ];
            }
            push @{${$SS_r->{RV}{$cmd}{deleted_instances}[1]{$fqcn}}[1]}, $inst;
            if ($verbose_mode==1) {
                print "No match for $src_instmap_r->{$inst}[1] " .
                      "($inst_path/$inst) in destination.\n";
            }
        }
    }

    # Mark as visited, pop instance path and return
    $vs_r->{$fqcn} = 1;
    $inst_path = substr $inst_path, 0, rindex($inst_path, "/");
}

# Looks under src_dir for the existence of any subtype of the specified
# base cell type.  Returns the lowest numerical subtype if one or more exists.
# Otherwise returns undef.
sub look_for_subtype_of {
    my $base_type  = shift;
    my $src_path_r = shift;
    (my $dir = $base_type) =~ s/\./\//g;
    foreach my $src_dir (@{$src_path_r}) {
        if (-e "$src_dir/$dir") {
            opendir DIR, "$src_dir/$dir";
            my @files = grep { /^[^\.]/ } readdir DIR;
            closedir DIR;
            @files = grep { s/\.cast$// } @files;
            return ($src_dir, "$base_type." . (sort { $a <=> $b } @files)[0]);
        }
    }
    return (undef, undef);
}

# Identifies the source directory containing the specified subtype file
sub look_for_subtype {
    my $subtype    = shift;
    my $src_path_r = shift;
    foreach my $src_dir (@{$src_path_r}) {
        my $file = fqcn_to_file($src_dir, $subtype);
        return $src_dir if (-e $file);
    }
    return undef;
}


#
# Common code for generating a new subtype hierarchy,
#
# Shared by both gen_subtypes and update_subtypes.  Saves old directory
# hierarchy into $cdir.save, puts new hierarchy in $cdir.  If $in_place
# is set, then the original directory $cdir will be copied rather than
# moved to $cdir.save.
#
sub gen_subtypes_common {
    my ($SS_r, $cdir, $in_place, $restore_size, $regenerate_netlist,
        $fqcn, $skip_backup, $extra_args) = @_;
    my $GS_r = $SS_r->{GS};
    my $wdir = $GS_r->{WORK_DIR};
    $fqcn = $GS_r->{TOP} unless defined($fqcn);
    $skip_backup = 0 unless defined($skip_backup);

    # Construct cast_path.  Note WORK_DIR is -not- included.  If it is,
    # and $in_place is set, then the subtype numbers won't end up being
    # what the user expects (Jauto finds the next available number when
    # subtypes already exist).
    my $cast_path = "$GS_r->{CAST_DIR}:$GS_r->{SPEC_DIR}";

    # Temporary directory setup
    if (! -e $wdir) {
        print "Creating $wdir working directory.\n" if ($GS_r->{VERBOSE});
        mkdir $wdir;
    }
    my $tmp_cdir = "$wdir/$cdir.tmp_gen_subtypes";
    `rm -rf "$tmp_cdir"` if (-e "$tmp_cdir");
    mkdir $tmp_cdir;
        
    # Determine fqcn and subtype
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
    $cmd .= "--defaultLayoutAttributes=$GS_r->{LAYOUT_ATTRIBUTE} \\\n"
        if exists $GS_r->{LAYOUT_ATTRIBUTE};
    $cmd .= "--cdlRoot='$wdir' \\\n";
    $cmd .= "--outRoot='$wdir' \\\n";
    $cmd .= "--subtypePath='$tmp_cdir' \\\n";
    $cmd .= "--castInRoot='$cast_path' \\\n";
    $cmd .= "--cellName='$cell' \\\n";
    $cmd .= "--subtype=$subtype \\\n";
    $cmd .= "--tau=$GS_r->{TAU}e-12 \\\n";
    if (defined $restore_size || defined $regenerate_netlist) {
        $cmd .= "--mode=resize \\\n";
    }
    else {
        $cmd .= "--mode=spec \\\n";
    }
    if (defined $restore_size) {
        $cmd .= "--hsizes='$restore_size' \\\n";
    }
    if (defined $regenerate_netlist) {
        $cmd .= "--netlist-from-prs='" . join(':', @{$regenerate_netlist}) .
                "' \\\n";
    }
    if (defined $extra_args) {
        $cmd .= join(" \\\n", @{$extra_args}) . " \\\n";
    }
    $cmd .= "--noFloorplan";

    # Run Jauto
    my $err = 0;
    if ($use_server) {
        $err = cast_server_system($SS_r, $cmd, -1);
    }
    else {
        supersize_system($SS_r, $cmd, $MAJOR_JOB, { LS_COLORS => "" });
        $err = $?;
    }

    # Move temp directory into main cast directory
    if ($err == 0) {
        if (-e "$wdir/$cdir" && !$skip_backup) {
            if (-e "$wdir/$cdir.save" && $GS_r->{SAVE_BACKUPS}) {
                print "Warning: Deleting stale $wdir/$cdir.save.\n" 
                    if ($GS_r->{VERBOSE});
                `rm -rf "$wdir/$cdir.save"`;
            }
            if (!$in_place && $GS_r->{SAVE_BACKUPS}) {
                rename "$wdir/$cdir", "$wdir/$cdir.save" 
                    unless ($GS_r->{JUST_PRINT});
            }
            elsif (!$in_place && !$GS_r->{SAVE_BACKUPS}) {
                `rm -rf "$wdir/$cdir"`;
            }
            else {
                `cp -a "$wdir/$cdir" "$wdir/$cdir.save"`;
            }
        }
        mkdir "$wdir/$cdir" if (!-e "$wdir/$cdir");
        `cp -a "$tmp_cdir"/* "$wdir/$cdir"`;
        `rm -rf "$tmp_cdir"`;
    }

    # Return pass/fail return value
    return $err;
}


#
# clean_subtypes
#
sub clean_subtypes {
    my $SS_r = shift;
    my $clean_used       = 0;
    my $clean_unused     = 0;
    my $clean_if_in_spec = 0;
    my $clean_from       = 0;
    my $clean_to;
    my $wdir = get_work_dir($SS_r);

    # Look at command line args
    while (num_args(\@_)) {
        my $arg = shift_next_scalar(\@_);
        command_die($SS_r, "Invalid scalar argument.") if (!defined $arg);
        if ($arg eq "--unused") {
            $clean_unused = 1;
        }
        elsif ($arg eq "--used") {
            $clean_used = 1;
        }
        elsif ($arg eq "--all") {
            $clean_used = 1;
            $clean_unused = 1;
        }
        elsif ($arg eq "--in-spec") {
            $clean_if_in_spec = 1;
        }
        elsif ($arg eq "--subtype-range") {
            my $eq = shift_next_scalar(\@_);
            my $range = shift_next_scalar(\@_);
            command_die($SS_r, "Bad --subtype-range argument.") if
                (!defined $eq || !defined $range);
            if ($range =~ /^(\d+):(\d+)$/) {
                $clean_from = $1;
                $clean_to   = $2;
                command_die($SS_r, "Sorry, --subtype-range isn't supported ".
                    "yet.");
            }
            else {
                command_die($SS_r, "Bad subtype range $1 specified.");
            }
        }
        else {
            command_die($SS_r, "Unknown clean argument: $arg.");
        }
    }
    if (!$clean_used && !$clean_unused) {
        command_die($SS_r, "Nothing to be done.  Must specify either " .
                           "--used, --unused, or --all.");
    }
    if ($clean_unused && $clean_if_in_spec) {
        command_die($SS_r, "Sorry, --unused --in-spec isn't supported.");
    }

    # Simplest case
    if ($clean_used && $clean_unused && !defined $clean_to) {
        print "Deleting cast working directory.\n";
        `rm -rf "$wdir/cast"/*`;
        return;
    }

    # For all other cases, [well except clean_from/to w/ both used & unused
    # specified] we need to know what subtypes are in use.
    my @cells_in_use;
    query_cast_server($SS_r, "--task=subcells --cell=$SS_r->{GS}{TOP}",
                      \@cells_in_use, 1);

    # Directory setup
    my $wcdir    = "$wdir/cast";
    my $tmp_cdir = "$wdir/cast.tmp_clean_subtypes";
    if (-e $tmp_cdir) {
        print "Warning: Deleting stale $tmp_cdir.\n";
        `rm -rf '$tmp_cdir'`;
    }
    if ($clean_unused) {
        # If we're keeping the cells in use, create a new scratch directory
        mkdir $tmp_cdir;
    }
    else {
        # Otherwise copy the original directory
        `cp -a '$wdir/cast' '$tmp_cdir'`;
    }

    # Iterate through list of used cells; either copy them to the new
    # directory or delete them from the new directory.
    foreach my $cell (@cells_in_use) {
        my $file      = fqcn_to_file($wcdir, $cell);
        my $spec_file = fqcn_to_file($SS_r->{GS}{SPEC_DIR}, $cell);
        my $dest_file = fqcn_to_file($tmp_cdir, $cell);

        # Nothing to do if the cell's file doesn't exist already
        if (!-e $file) {
            if (!-e $spec_file) {
                print "Warning: $cell doesn't exist in either SPEC_DIR or " .
                      "WORK_DIR/cast.\n";
            }
            next;
        }

        if ($clean_unused) {
            # Copy only the ones to keep
            print "Retaining $cell.\n" if ($SS_r->{GS}{VERBOSE});
            if ($dest_file =~ /^(.*)\/[^\/]+$/) {
                `mkdir -p '$1'`;
            }
            `cp '$file' '$dest_file'`;
        }
        elsif ($clean_used) {
            # Delete those that are used
            if ($clean_if_in_spec && -e $spec_file || !$clean_if_in_spec) {
                print "Deleting $cell.\n" if ($SS_r->{GS}{VERBOSE});
                unlink $dest_file;
            }
        }
    }

    # Directory cleanup
    my $save_cdir = "$wdir/cast.save";
    if (-e $save_cdir) {
        print "Warning: Deleting stale $save_cdir.\n";
        `rm -rf '$save_cdir'`;
    }
    rename $wcdir, $save_cdir;
    rename $tmp_cdir, $wcdir;

    # dirty everything
    set_dirty($SS_r, "all") if ($clean_used);
}


#
# compact_subtypes
#
sub compact_subtypes {
    my $SS_r = shift;
    my $range_from;
    my $range_to;
    my $to = 0;
    my $wdir = get_work_dir($SS_r);

    # parse args
    while (num_args(\@_)) {
        my $arg = shift_next_scalar(\@_);
        if (!defined $arg) {
            command_die($SS_r, "Unexpected non-scalar argument specified.");
        }
        if ($arg eq "--range") {
            my $eq = shift_next_scalar(\@_);
            my $r  = shift_next_scalar(\@_);
            if (!defined $eq || $eq ne "=" || 
                !defined $r || $r !~ /^(\d+):(\d+)$/) {
                command_die($SS_r, "Bad argument to --range.");
            }
            $range_from = $1;
            $range_to = $2;
        }
        elsif ($arg eq "--to") {
            my $eq = shift_next_scalar(\@_);
            $to = shift_next_scalar(\@_);
            if (!defined $eq || $eq ne "=" || 
                !defined $to || $to !~ /^\d+$/) {
                command_die($SS_r, "Bad argument to --range.");
            }
        }
        else {
            command_die($SS_r, "Unknown argument $arg specified.");
        }
    }

    # Create temporary compact directory
    my $tmp_cdir = "$wdir/cast_compact";
    if (-e $tmp_cdir) {
        print "Warning: Deleting existing cast_compact directory.\n";
        `rm -rf \"$tmp_cdir\"`;
    }
    mkdir $tmp_cdir;

    # Runs SubtypeSplit to do the compact
    print "Compacting subtypes...\n";
    my $compact_path = "$wdir/cast:$SS_r->{GS}{SPEC_DIR}";
    my $cast_path = "$SS_r->{GS}{CAST_DIR}:$compact_path";

    my ($cmd, $use_server) = set_java_cmd($SS_r, "subtype_split", $MINOR_JOB);
    if (!defined $cmd || $cmd eq "") {
        command_die($SS_r, "Internal error: Can't run subtype_split.")
    }
    $cmd .= "--cell=\"$SS_r->{GS}{TOP}\" \\\n";
    $cmd .= "--min-subtype=$to \\\n";
    $cmd .= "--cast-path=\"$cast_path\" \\\n";
    $cmd .= "--subtype-path=\"$tmp_cdir\" \\\n";
    $cmd .= "--compact \\\n";
    $cmd .= "--compact-range=$range_from:$range_to \\\n" 
        if (defined $range_from && defined $range_to);
    $cmd .= "--compact-path=\"$compact_path\" \\\n";
    $cmd .= "--output=\"$wdir/compact.spec\"";

    my $err = 0;
    if ($use_server) {
        $err = cast_server_system($SS_r, $cmd, -1);
    }
    else {
        supersize_system($SS_r, $cmd, $MINOR_JOB);
        $err = $?;
    }
    command_die($SS_r, "Error running subtype_split.") if ($err != 0);
    
    my $compact_spec_mr = 
        read_split_spec($SS_r, "$wdir/compact.spec");

    # copy temporary compact hierarchy into main cast directory
    `cp -a \"$tmp_cdir\"/* \"$wdir/cast\"`;
    `rm -rf \"$tmp_cdir\"` unless ($SS_r->{GS}{SAVE_BACKUPS});
    
    print "Compacted " . scalar(keys %{$compact_spec_mr}) . " subtypes.\n";

    set_cmd_return_variable($SS_r, "result", [ $TYPE_MAP, $compact_spec_mr ]);
}


#
# copy_subtypes
#
sub copy_subtypes {
    my $SS_r = shift;

    # Parse command args to determine what we're doing
    my $if_exists     = 0;      # ignore/overwrite
    my $preserve_perm = 1;
    my $src_dir;
    my $dest_dir;
    my @subtypes;
    my @cmd_split_maps;
    while (num_args(\@_)) {
        my @arg = shift_next_arg(\@_);
        if ($arg[0] == $TYPE_SCALAR && $arg[1] eq "--src-dir") {
            my $eq   = shift_next_scalar(\@_);
            $src_dir = shift_next_scalar(\@_);
            if (!defined $eq || $eq ne "=" || !defined $src_dir) {
                command_die($SS_r, "Bad --src-dir arguments.\n");
                return;
            }
        }
        elsif ($arg[0] == $TYPE_SCALAR && $arg[1] eq "--dest-dir") {
            my $eq    = shift_next_scalar(\@_);
            $dest_dir = shift_next_scalar(\@_);
            if (!defined $eq || $eq ne "=" || !defined $dest_dir) {
                command_die($SS_r, "Bad --dest-dir arguments.\n");
                return;
            }
        }
        elsif ($arg[0] == $TYPE_SCALAR && $arg[1] eq "--make-writeable") {
            $preserve_perm = 0;
        }
        elsif ($arg[0] == $TYPE_SCALAR && $arg[1] eq "--skip-if-exists") {
            $if_exists = 1;
        }
        elsif ($arg[0] == $TYPE_SCALAR && $arg[1] eq "--abort-if-exists") {
            $if_exists = 2;
        }
        elsif ($arg[0] == $TYPE_SCALAR && $arg[1] =~ /^--/) {
            command_die($SS_r, "Bad argument: $arg[1].");
            return;
        }
        elsif ($arg[0] == $TYPE_SCALAR) {
            push @subtypes, $arg[1];
        }
        elsif ($arg[0] == $TYPE_LIST) {
            push @subtypes, @{$arg[1]};
        }
        elsif ($arg[0] == $TYPE_MAP) {
            push @cmd_split_maps, $arg[1];
        }
        else {
            command_die($SS_r, "Unexpected argument.");
            return;
        }
    }

    if (!@subtypes && !@cmd_split_maps) {
        # Attempt to copy ALL subtypes instantiated under TOP
        if (query_cast_server($SS_r, "--task=subcells --cell=$SS_r->{GS}{TOP}",
                              \@subtypes, 1) < 0) {
            command_die($SS_r, "Error determining TOP subtypes.  " .
                "Run 'query subcells' to debug.");
        }
    }

    # Construct split_map from individual subtypes specified on cmd line
    my %split_map;
    my $split = 0;
    foreach my $s (@subtypes) {
        if ($s =~ /^([^:]+)(:[^:]*)?$/) {
            my $src = $1;
            $split_map{$src} = [] if (!defined $split_map{$src});
            if (defined $2) {
                (my $dest = $2) =~ s/^://;
                if ($dest =~ /^\d.*$/) {
                    $dest = substr($src,0,rindex($src,".")) . ".$dest";
                }
                push @{$split_map{$src}}, $dest;
                $split = 1 if ($dest ne $src);
            }
            else {
                push @{$split_map{$src}}, $src;
            }
        }
        else {
            print "Warning: skipping unrecognized subtype $s.\n";
        }
    }
    # Add any specified split_map variables to the split map
    foreach my $sm (@cmd_split_maps) {
        foreach my $src (keys %{$sm}) {
            $split_map{$src} = [] if (!defined $split_map{$src});
            if ($sm->{$src}[0] == $TYPE_SCALAR) {
                push @{$split_map{$src}}, $sm->{$src}[1];
            }
            elsif ($sm->{$src}[0] == $TYPE_LIST) {
                push @{$split_map{$src}}, @{$sm->{$src}[1]};
            }
            else {
                command_die($SS_r, "Unrecognized map type specified.");
            }
            $split = 1;
        }
    }

    my $wdir = get_work_dir($SS_r);

    my $dest_specified = defined $dest_dir;
    if (!$dest_specified) {
        if (defined $src_dir && $src_dir eq "$wdir/cast") {
            $dest_dir = $SS_r->{GS}{SPEC_DIR};
        }
        else {
            $dest_dir = "$wdir/cast";
        }
    }
    if (!defined $src_dir) {
        if ($dest_specified && $dest_dir eq "$wdir/cast" ||
            !$dest_specified && $split==0) {
            $src_dir = $SS_r->{GS}{SPEC_DIR};
        }
        else {
            $src_dir = "$wdir/cast";
        }
    }

    print "Attempting to copy " . scalar(keys %split_map) . " source subtypes ".
          "to " .  scalar(values %split_map) . " destination subtypes.\n";

    # Copy subtypes
    foreach my $s (keys %split_map) {
        foreach my $d (@{$split_map{$s}}) {
            copy_subtype($SS_r, $src_dir, $s, $dest_dir, $d, $if_exists, 
                         $preserve_perm, 1);
        }
    }
}



################################# Utilities #################################

sub read_split_spec {
    my $SS_r      = shift;
    my $spec_file = shift;
    my $split_map = {};

    open (SPEC, $spec_file) || command_die($SS_r, "Can't read $spec_file.");
    while (<SPEC>) {
        chomp; s/^\s*//; s/\s*$//; s/\#.*$//;
        next if (/^$/);
        if (/^SPLIT:\s+([^\s]+)\s+([^\s]+)\s+(.*)$/) {
            my $base   = $1;
            my $subnum = $2;
            my $to_str = $3;
            my @to = split /\s+/, $to_str;
            if (!exists $split_map->{"base.$subnum"}) {
                $split_map->{"$base.$subnum"} = [ $TYPE_LIST, [] ];
            }
            foreach my $to_num (@to) {
                push @{$split_map->{"$base.$subnum"}[1]}, "$base.$to_num";
            }
        }
        elsif (/^COPY\s+([^\s+]+)\s+([^\s+]+)$/) {
            if (!exists $split_map->{$1}) {
                $split_map->{$1} = [ $TYPE_LIST, [] ];
            }
            push @{$split_map->{$1}[1]}, $2;
        }
        else {
            command_die($SS_r, "Unrecognized line in $spec_file:\n" . $_);
        }
    }
    close SPEC;
    return $split_map;
}
                

#
# Find the next available subtype in the specified range [sub_from,sub_to),
# giving preference to pref_num if one is specifid.  Basic routine for 
# splitting.  If sub_to is <=0 or undefined, range becomes [sub_from,infinity).
# Note: pref_num will be returned if it is unused, regardless of whether it
# falls within the [sub_from,sub_to) range.
#
# If the subtypes_r map (basetype->[subnum1 subnum2 ...]) is defined,
# then this will be used as an additional source of subtypes to avoid.
#
# If $dir_list is undefined or empty then only SPEC_DIR (and subtypes_r) will
# be used.
#
# If no subtype numbers are available in the specified range, -1 is
# returned.
#
sub find_next_available_subtype {
    my $SS_r       = shift;
    my $dir_list_r = shift;         # ( "WORK_DIR1/cast" "WORK_DIR2/cast" ..)
    my $basetype   = shift;         # Base type to find a subtype for
    my $pref_num   = shift;         # preferred number
    my $sub_from   = shift;         # Subtype range; see above
    my $sub_to     = shift;
    my $subtypes_r = shift;         # Additional defined subtypes map

    my %subtypes_in_use;
    (my $cell_dir_path = $basetype) =~ s/\./\//g;

    # Get subtypes from spec dir
    my $cell_spec_dir = "$SS_r->{GS}{SPEC_DIR}/$cell_dir_path";
    find_subtypes($cell_spec_dir, \%subtypes_in_use);

    # Get subtypes from each working dir
    if (defined $dir_list_r) {
        foreach my $dir (@{$dir_list_r}) {
            my $cell_work_dir = "$dir/$cell_dir_path";
            find_subtypes($cell_work_dir, \%subtypes_in_use);
        }
    }

    # Get subtypes from subtypes map
    if (defined $subtypes_r && defined $subtypes_r->{$basetype}) {
        foreach my $subnum (@{$subtypes_r->{$basetype}}) {
            if ($subnum =~ /^\d+$/) {
                $subtypes_in_use{$subnum} = 1;
            }
        }
    }

    # Give the preferred number a chance (look for conflicts)
    if (defined $pref_num && !exists $subtypes_in_use{$pref_num}) {
        return $pref_num;
    }

    # Now look for the lowest available subtype number in the specified
    # range
    my @subnums = sort { $a <=> $b } keys %subtypes_in_use;
    my $next_available = $sub_from;
    for my $subnum (@subnums) {
        if ($next_available > $subnum) {
            next;
        }
        if ($next_available == $subnum) {
            $next_available++;
            return -1 if ($sub_to > 0 && $next_available >= $sub_to);
        }
        else {
            # next subtype number in use skipped ahead, leaving a hole
            # we can use
            last;
        }
    }
    return $next_available;
}

sub find_subtypes {
    my $dir      = shift;
    my $in_use_r = shift;

    opendir (DIR, $dir) || return;
    my @cast_files = grep { $_ =~ /\.cast$/ } readdir DIR;
    closedir DIR;
    foreach my $file (@cast_files) {
        if ($file =~ /^(.*)\.cast$/) {
            my $subnum = $1;
            if ($subnum =~ /^\d+$/) {
                $in_use_r->{$subnum} = 1;
            }
        }
    }
}

1;
