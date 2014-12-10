#
# Unmerging
#
# Support for reverting merging problems
#

package Supersize::Unmerging;

BEGIN {
    use Exporter;
    our @ISA = qw(Exporter);
    our @EXPORT = qw(
    );
    our @EXPORT_OK = qw( 
        &get_module_data
    );
}

use strict;
use Supersize::DensityFactor;
use Supersize::LayoutModeling;
use Supersize::Merging;
use Supersize::ModifySubtypes;
use Supersize::Netlist;
use Supersize::SubtypeSpace;
use Supersize::JavaUtil;
use Supersize::TypeUtil;
use Supersize::Util;
use Term::ANSIColor;

sub get_module_data {
  return { 
    NAME => "Unmerging",
    DESC => "Commands to revert slack violations caused by merging.",
    COMMANDS => {
    "trial_unmerge" => {
        SUBREF => \&trial_unmerge,
        USAGE  => "trial_unmerge [--subtype-limit=". underline("SL1") .
                                    "[,".underline("SL2")."...]] " .
                                "[--split-threshold=".underline("ST1") .
                                    "[,".underline("ST2")."...]] " .
                                "[--split-cost-factor=".underline("SCF1") .
                                    "[,".underline("SCF2")."...]] " .
                                "[--unit-cost=0|1[,0|1]] " .
                                "[--fixed-cost-scaling=" . underline("FCS1") .
                                    "[,".underline("FCS2")."...]] " .
                                "[--random-cost-scaling=" .  underline("RCS1") .
                                    "[,".underline("RCS2")."...]] ",
        DESC =>
          "Determines the optimal set of cells to subtype in the selected ".
          "trial merge space in order to fix slack violations.  The basic ".
          "assumption of this algorithm is that any cell instance which ".
          "either drives or loads a violating path could be the culprit, ".
          "and by subtyping it (to a new sizable subtype) the slack violation ".
          "can be eliminated.  Given the set of such cell instances for each ".
          "negative-slack path, the problem reduces to choosing the minimal ".
          "set of cells to subtype while ensuring that at least one cell from ".
          "each path is subtyped.\n\n" .
          "After performing this optimization, trial_unmerge applies the ".
          "subtyping decisions to the merge space.  The number of new ".
          "subtypes introduced per original subtype is limited by the value ".
          "specified by subtype-limit.  If set to -1 or 0, no limit ".
          "will be imposed (beware: array elements may be split).  When set ".
          ">=0, instances with identical sizing environments will be subtyped ".
          "together (highly recommended).  The default subtype limit is 1.  ".
          "For more information about unmerging parameters, see --extra ".
          "help.\n\n".
          "This command should be used in the same general way as the ".
           bold("trial_merge")." and ".bold("trial_pmerge")." commands.  ".
          "Specifically, the source subtype space analyzed by this command ".
          "is WORK_DIR/cast unless a specific merge trial has been selected ".
          "with the ".bold("select_merge")." command.\n\n".
          "Note: After trial_unmerge, you will need to propagate the ".
          "subtyping decisions to the floorplanning by running\n\n".
          "  copy_floorplan \$trial_unmerge.split_map\n".
          "  update_floorplan",
        EXTRA_DESC => 
          "Description of unmerging parameters:\n\n".
          "  subtype-limit       - After unmerging, no source subtype will\n".
          "                        have more than this many new subtypes\n" .
          "                        created from it (unless split-threshold\n".
          "                        applies).  If set to -1 or 0, no limit\n".
          "                        will be imposed.  When set >= 0, instances\n".
          "                        of the same cell with identical sizing\n".
          "                        environments will be subtyped together.\n".
          "                        Default: 1\n".
          "  split-threshold     - If a source subtype has more than this\n".
          "                        many unique instantiation contexts (i.e.\n".
          "                        instantiations with different environment\n".
          "                        cells), then all instances will be\n".
          "                        considered together for subtyping.  If\n".
          "                        selected, each instance will be assigned\n".
          "                        a unique new subtype.  If set to -1, this\n".
          "                        splitting behavior will never apply.\n".
          "                        Default: -1\n".
          "  split-cost-factor   - When splitting has taken effect for a\n".
          "                        given subtype due to split-threshold, the\n".
          "                        subtyping cost of that subtype will be\n".
          "                        modelled as\n".
          "                            Cost(leaf)*ceil(#inst/scf)\n".
          "                        Where #inst is the number of unique inst-\n".
          "                        ances.  If set to 0, the cost will be set\n".
          "                        to Cost(leaf) regardless of #inst.  This\n".
          "                        factor should be interpreted as the ex-\n".
          "                        pected number of split subtypes per post-\n".
          "                        merging subtypes.  If you expect perfect\n".
          "                        merging, set it 0.  If you expect to \n".
          "                        end up with 1 eventual subtype for every\n".
          "                        10 that are split, set it to 10.\n".
          "                        Default: 0\n".
          "  unit-cost           - If set to 1, all cells are modelled to \n".
          "                        have the same layout cost.\n".
          "                        Default: 0\n".
          "  fixed-cost-scaling  - The cost of all fixed size cells are mul-\n".
          "                        tiplied by this value.  Typically, a value\n".
          "                        less than one will be supplied in order to\n".
          "                        make fixed sized cells more likely to be\n".
          "                        unmerged.\n".
          "  random-cost-scaling - This parameter is provided as a way to\n".
          "                        randomly obtain slightly different\n".
          "                        solutions by scrambling the subtype layout\n".
          "                        costs.  The cost equation is\n".
          "                          Cost = (1-rcs)*Cost(leaf) + rcs*random\n".
          "                        thus setting it to 1 makes the cost en-\n".
          "                        tirely random while setting it to 0 (the\n".
          "                        default) keeps it entirely deterministic.\n".
          "  random-cost-seed    - This parameter is exposed so that it can\n".
          "                        be set to that of a prior unmerging run\n".
          "                        (see the TRIAL file for the seed used on\n".
          "                        any given unmerging trial.)\n",
        GLOBALS => {
          WORK_DIR => { REQUIRED => 1 },
          TOP      => { REQUIRED => 1 },
          CAST_DIR => { REQUIRED => 1 },
          SPEC_DIR => { REQUIRED => 1 } },
        RV => {
          split_map => {
            TYPE => $TYPE_MAP,
            DESC => "Map of s -> <new_subtype_1 new_subtype_2 ...> ".
                    "for each original subtype s that was subtyped in one ".
                    "or more instances.  This map can be passed to copy_views ".
                    "in order to create the new subtypes." },
          old_inst_map => {
            TYPE => $TYPE_MAP,
            DESC => "Map of i -> original_subtype for each instance i that ".
                    "was subtyped." },
          new_inst_map => {
            TYPE => $TYPE_MAP,
            DESC => "Map of i -> new_subtype for each instance i that was ".
                    "subtyped." } } }
    }
  };
}


#
# trial_unmerge
#
# 1. Read violations.debug file, constructing two lists per violation:
#      (driver1 driver2 ...), (load1 load2 ...)
#    Maintain a map of subtype -> unique_number for each subtype.
#    Also maintain the reverse map number -> subtype.
#    Also maintain a map of subtype -> (inst1 inst2 ...) of all 
#    violation instances of that subtype.
#
# 2. Get layout costs of all subtypes.
#
# 3. Write glpsol file.  Constraints:
#
#     num(v[i].d[0]) + num(v[i].d[1]) + ... + num(v[i].l[0]) >= 1
#     num(v[i].d[0]) + num(v[i].d[1]) + ... + num(v[i].l[1]) >= 1
#     ...
#
#     -unless- the only fixed-size cells in the violation are within
#     the load set, in which case only expand the constraints over the
#     fixed-size load cells.
#
#    Objective function:
#
#     Sum(cost(subtype) * num(subtype))  [over all subtypes]
#
# 4. Invoke glpsol, read the solution, build a list of subtypes
#    chosen for subtyping.
#
# 5. Iterate through list of chosen subtypes, iterate through instances
#    of that subtype, then for each one determine the new subtype number
#    to assign to (based on uniquely_subtype).  For each instance, 
#    call resubtype.  Maintain split_map, old_inst_map, and new_inst_map.
#
#    [Later: modify resubtype or create a new routine to propagate the
#    subtyping upwards, so fully_subtype==1 can be supported.]
#
#
# Improvements:
#    - For chosen fixed-size subtypes, instead of assigning a new 
#      subtype, optionally reassign to the old (pre-merged) value.
#      Will need instance -> subtype info for the pre-merged space
#      (adapt merge_space_info to return this?)
#    - Optionally decrease costs of fixed-size cells, so that these 
#      cells will be favored for unmerging.  In fact, it might be 
#      reasonable to drop the cost all the way to zero.  Introduce
#      multiplicative scale factor IV.
#
sub trial_unmerge {
    my $SS_r = shift;
    my %params = (
        subtype_limit => [],
        split_threshold => [],
        unit_cost => [],
        fixed_cost_scaling => [],
        random_cost_scaling => [],
        split_cost_factor => []
    );
    my $random_seed = time ^ $$;

    # parse args
    while (num_args(\@_)) {
        my $arg = shift_next_scalar(\@_);
        if (!defined $arg) {
            command_die($SS_r, "Bad argument specified to trial_unmerge.");
        }
        elsif ($arg =~ /^--(.*)$/) {
            $arg = $1;
            if ($arg eq "fixed-cost-scaling" || $arg eq "subtype-limit" ||
                $arg eq "split-threshold" || $arg eq "random-cost-scaling" ||
                $arg eq "unit-cost" || $arg eq "split-cost-factor") {
                my $eq = shift_next_scalar(\@_);
                my ($type, $val) = shift_next_arg(\@_);
                if (!defined $eq || $eq ne "=" || !defined $type ||
                    $type != $TYPE_SCALAR && $type != $TYPE_LIST) {
                    command_die($SS_r, "Bad argument to $arg.");
                }
                my @list;
                if ($type == $TYPE_SCALAR) {
                    @list = split /,/, $val;
                }
                else {
                    @list = @{$val};
                }
                $arg =~ s/-/_/g;
                push @{$params{$arg}}, @list;
            }
            elsif ($arg =~ /^--random-seed=(.*)$/) {
                $random_seed = $1;
            }
            else {
                command_die($SS_r, "Unrecognized option --$arg.");
            }
        }
        else {
            command_die($SS_r, "Unrecognized argument.");
        }
    }
    my $trial = get_selected_merge_trial($SS_r);
    my $trial_dir = trial_to_merge_dir($SS_r, $trial);
    if ($SS_r->{GS}{VERBOSE}) {
        if ($trial ne "") {
            print "Performing unmerge analysis of merging trial $trial.\n";
        }
        else {
            print "WARNING: Performing unmerge analysis of WORK_DIR/cast.\n";
            print "         You may have forgotten to run select_merge!\n";
        }
    }
    if (!-e "$trial_dir/cast") {
        command_die($SS_r, "Subtype hierarchy for trial $trial doesn't ".
                           "exist.\nRun trial_size first.");
    }

    # Set non-standard cast path
    set_server_merge_dir($SS_r, $trial_dir);

    # Construct TOP instantiation hierarchy data structures.  Later will
    # need to add instantiation maps
    if ($SS_r->{GS}{VERBOSE}) {
        print "Determining instantiation hierarchy of TOP.\n";
    }
    my ($leaf2num_r, $num2leaf_lr, $mid2num_r, $num2mid_lr, $fixed_lr,
        $leaf2parents_lr, $mid2parents_lr) = 
        query_instantiation_hierarchy($SS_r);

    # Read violations.debug file
    print "Processing violations.debug file.\n" if ($SS_r->{GS}{VERBOSE});
    my $violations_lr = [];
    if (!-e "$trial_dir/violations.debug") {
        command_die($SS_r, "violations.debug file does not exist.  Run " .
                           "trial_size first.");
    }
    my ($v_lr, $isis_lr, $leaf2isi_lr) = 
        read_violations_file($SS_r, "$trial_dir/violations.debug",
                             $leaf2num_r, $mid2num_r, $fixed_lr);

    # debug
    if ($SS_r->{GS}{DEBUG}) {
        print "  Leaf subtype numbering:\n";
        foreach my $n (0..$#{$num2leaf_lr}) {
            print "    $n: $num2leaf_lr->[$n]";
            print " FIXED" if ($fixed_lr->[$n]);
            print "\n";
        }
        print "  Mid subtype numbering:\n";
        foreach my $n (0..$#{$num2mid_lr}) {
            print "    $n: $num2mid_lr->[$n]\n";
        }
        print "  ISI numbering:\n";
        foreach my $isi (0..$#{$isis_lr}) {
            print "    $isi:";
            foreach my $instspec_lr (@{$isis_lr->[$isi]}) {
                print " " . instspec_to_string($instspec_lr);
            }
            print "\n";
        }
        if (0) {
            print "  Violations:\n";
            foreach my $v_r (@{$v_lr}) {
                print "    Top: $v_r->{top}\n";
                print "    Amount: $v_r->{vworst}\n";
                print "    Drivers:\n";
                foreach my $isi_num (@{$v_r->{drivers}}) {
                    print "      " .  instspec_to_string(
                        $isis_lr->[$isi_num][0]) . "\n";
                }
                print "    Loads:\n";
                foreach my $isi_num (@{$v_r->{loads}}) {
                    print "      " .  instspec_to_string(
                        $isis_lr->[$isi_num][0]) . "\n";
                }
            }
        }
        if (0) {
            print "  Parents of leaf cells:\n";
            foreach my $l (0..$#{$leaf2parents_lr}) {
                print "    $l:";
                foreach my $pn (@{$leaf2parents_lr->[$l]}) {
                    print " ($pn->[0] $pn->[1])";
                }
                print "\n";
            }
            print "  Parents of mid-level cells:\n";
            foreach my $m (0..$#{$mid2parents_lr}) {
                print "    $m:";
                foreach my $pn (@{$mid2parents_lr->[$m]}) {
                    print " ($pn->[0] $pn->[1])";
                }
                print "\n";
            }
        }
    }

    # determine layout cost of all leaf cells (pre-scaling)
    if ($SS_r->{GS}{VERBOSE}) {
        print "Determining layout costs of all leaf cells.\n";
    }
    my $props_r = {};
    my $num2cost_lr = [];
    query_netlist_props($SS_r, $trial_dir, $num2leaf_lr, $props_r);
    query_manual_density_factors($SS_r, $num2leaf_lr, $props_r);
    foreach my $leaf (@{$num2leaf_lr}) {
        push @{$num2cost_lr}, 
            calculate_manual_cost($leaf, $props_r->{$leaf});
    }

    # set defaults of sweep parameters
    if (!@{$params{subtype_limit}}) {
        push @{$params{subtype_limit}}, 1;
    }
    if (!@{$params{split_threshold}}) {
        push @{$params{split_threshold}}, -1;
    }
    if (!@{$params{unit_cost}}) {
        push @{$params{unit_cost}}, 0;
    }
    if (!@{$params{fixed_cost_scaling}}) {
        push @{$params{fixed_cost_scaling}}, 1.0;
    }
    if (!@{$params{random_cost_scaling}}) {
        push @{$params{random_cost_scaling}}, 0.0;
    }
    if (!@{$params{split_cost_factor}}) {
        push @{$params{split_cost_factor}}, 0.0;
    }

    # run trials
    foreach my $sl (@{$params{subtype_limit}}) {
        foreach my $st (@{$params{split_threshold}}) {
            # Merge instance set identifier space
            my ($misi2isi_lr, $isi2misi_lr, $leaf2misi_lr, $degenerate_misi_lr,
                $split_misi_lr, $fully_covered_leafs_lr) =
                merge_isis($SS_r, $sl, $st, $isis_lr, $leaf2isi_lr, $fixed_lr,
                           $mid2parents_lr, $leaf2parents_lr);
            foreach my $uc (@{$params{unit_cost}}) {
                foreach my $fcs (@{$params{fixed_cost_scaling}}) {
                    foreach my $rcs (@{$params{random_cost_scaling}}) {
                        foreach my $scf (@{$params{split_cost_factor}}) {
                            run_trial_unmerge($SS_r, $trial, $isis_lr, 
                                $misi2isi_lr, $isi2misi_lr, $leaf2misi_lr, 
                                $degenerate_misi_lr, $split_misi_lr, 
                                $fully_covered_leafs_lr,
                                $leaf2num_r, $num2leaf_lr, $num2cost_lr, 
                                $mid2num_r, $num2mid_lr, $fixed_lr, 
                                $v_lr, $mid2parents_lr, $leaf2parents_lr,
                                { subtype_limit => $sl,
                                  split_threshold => $st,
                                  unit_cost => $uc,
                                  fixed_cost_scaling => $fcs,
                                  random_cost_scaling => $rcs,
                                  split_cost_factor => $scf,
                                  random_seed => $random_seed,
                                });
                        }
                    }
                }
            }
        }
    }
    print "Done.\n";
}


# Extracts the instantiation tree under TOP, returning a number of data
# structures that will be used in the downstream unmerging algorithms.
#   leaf2num_r      - Map of leaf fqcn to unique numerical ID.
#   num2leaf_lr     - Reverse map off leaf2num_r.
#   mid2num_r       - Map of mid-level fqcn to unique numerical ID.  NOTE:
#                     eventually this map will need to satisfy the property
#                     that i<j => mid2num[j] does not instantiate mid2num[i].
#   num2mid_lr      - Reverse map of mid2num_r.
#   fixed_lr        - Map of leaf number to fixed-size state (1/0)
#   leaf2parents_lr - Map of leaf_num -> [ [parent1 n1] [parent2 n2] ..]
#   mid2parents_lr  - Map of mid_num -> [ [parent1 n1] [parent2 n2] ..]
#
sub query_instantiation_hierarchy {
    my $SS_r = shift;

    # Initialize structures
    my $leaf2num_r      = {};
    my $num2leaf_lr     = [];
    my $mid2num_r       = {};
    my $num2mid_lr      = [];
    my $fixed_lr        = [];
    my $leaf2parents_lr = [];
    my $mid2parents_lr  = [];

    # Query for the leaf and mid cell lists
    my $cmd = "--cell=$SS_r->{GS}{TOP} --task=subcells --filter=leaf";
    query_cast_server($SS_r, $cmd, $num2leaf_lr, 1);

    # Note: Relies on proper ordering of cells (parent-to-child)
    $cmd = "--cell=$SS_r->{GS}{TOP} --task=subcells --filter=!leaf " .
           "--instantiation-order";
    query_cast_server($SS_r, $cmd, $num2mid_lr, 1);

    # Reverse maps
    foreach my $i (0..$#{$num2leaf_lr}) {
        $leaf2num_r->{$num2leaf_lr->[$i]} = $i;
        $fixed_lr->[$i] = 0;
    }
    foreach my $i (0..$#{$num2mid_lr}) {
        $mid2num_r->{$num2mid_lr->[$i]} = $i;
    }

    # Fixed-size list (ordered correctly)
    my @fixed_leaf;
    $cmd = "--cell=$SS_r->{GS}{TOP} --task=subcells --filter=leaf&fixed";
    query_cast_server($SS_r, $cmd, \@fixed_leaf, 1);
    foreach my $f (@fixed_leaf) {
        $fixed_lr->[$leaf2num_r->{$f}] = 1;
    }

    # TODO: Construct mid2inst_r and leaf2parents_r.
    foreach my $midcell (keys %{$mid2num_r}) {
        $cmd = "--cell=$midcell --task=instance_list --no-recurse";
        my @sub_insts;
        query_cast_server($SS_r, $cmd, \@sub_insts, 1);
        my $p = $mid2num_r->{$midcell};
        foreach my $s (@sub_insts) {
            my ($child, $n) = split /\s+/, $s;
            if (exists $leaf2num_r->{$child}) {
                my $c = $leaf2num_r->{$child};
                if (!defined $leaf2parents_lr->[$c]) {
                    $leaf2parents_lr->[$c] = [ [$p, $n] ];
                }
                else {
                    push @{$leaf2parents_lr->[$c]}, [$p, $n];
                }
            }
            else {
                if (!exists $mid2num_r->{$child}) {
                    print "Warning: Ignoring unknown subcell $child of ".
                          "$midcell.\n";
                    next;
                }
                my $c = $mid2num_r->{$child};
                if (!defined $mid2parents_lr->[$c]) {
                    $mid2parents_lr->[$c] = [ [$p, $n] ];
                }
                else {
                    push @{$mid2parents_lr->[$c]}, [$p, $n];
                }
            }
        }
    }

    return ($leaf2num_r, $num2leaf_lr, $mid2num_r, $num2mid_lr, $fixed_lr,
            $leaf2parents_lr, $mid2parents_lr);
}


# Reads Jauto's violations.debug file.
# (See sw/cad/doc/specs/jauto/sizing/JautoUnmergingSupport.pdf)
#
# Returns (in a list) numerous maps & lists:
#     v_lr        - List of all violation map references (see below)
#     isis_lr     - List of unique instance set identifiers.  Each element
#                   is a list of instance specifications 
#                       isi_num -> [ [hT hN v_r] ..]
#                   (see below for instance specification format)
#     leaf2isi_lr - List mapping leaf number to isi number
#
# For each violation, a map is constructed with the following keys:
#     top          -> top-level-cell
#     vworst       -> worst relative violation in this group
#     drivers      -> list of driver+load unique ISI numbers
#     leafs        -> list of load-only unique ISI numbers
#     leaf_drivers -> list of unique driver leaf cell types (numerical)
#     leaf_loads   -> list of unique load-only leaf cell types (numerical)
#
# Each instance specification is represented as a list of a /-separated 
# subtype & instance hierarchy (numerical), plus the reference of the 
# defining violation set:
#     [ p0/p1/.../l, i0/i1/.../il, $v_r ]
#
sub read_violations_file {
    my $SS_r        = shift;
    my $vfile       = shift;    # violations.debug file
    my $leaf2num_r  = shift;
    my $mid2num_r   = shift;
    my $fixed_lr    = shift;

    my $v1_lr        = [];
    my $isis1_lr     = [];
    my $leaf2isi1_lr = [];
    my %merge_isi_map;

    open (VFILE, $vfile) || command_die($SS_r, "Couldn't read $vfile.");
    my $pstate = 0;
    my $line_num = 0;
    my $v_r;
    my $load_hThN_lr;
    my $driver_hThN_lr;
    my %leaf_drivers;
    my %leaf_loads;
    while (<VFILE>) {
        $line_num++;
        s/\#.*$//; s/^\s*//; s/\s*$//; chomp;
        next if (/^$/);
        if ($pstate == 0) {
            my ($vstr, $top, $vworst) = (split(/\s+/, $_))[0..2];
            if (!defined $vstr || !defined $top || !defined $vworst ||
                $vstr ne "VIOLATION" || $top !~ /^[\w\.,\[\]\(\)\{\}]+$/ ||
                $vworst !~ /^[\d\.eE-]+$/ || $_ !~ /{$/) {
                command_die($SS_r, "Unexpected syntax on line $line_num of ".
                                   $vfile . ".");
            }
            $v_r = {
                top => $top,
                vworst => $vworst,
                line_num => $line_num,
                drivers => [], loads => [],
                leaf_drivers => [], leaf_loads => []
            };
            $load_hThN_lr = [];
            $driver_hThN_lr = [];
            %leaf_drivers = ();
            %leaf_loads = ();
            $pstate = 1;
            if (!exists $mid2num_r->{$top} && !exists $leaf2num_r->{$top}) {
                command_die($SS_r, "Unknown top-level violation cell $top ".
                                   "encountered.");
            }
        }
        elsif ($_ =~ /^(\w+)(\*?)(\s+([^\s]+)\s+([^\s]+))?$/) {
            my ($type, $suffix) = split /_/, $1;
            my $load = $2 eq "*" ? 1 : 0;
            my @type_hier = split /\//, $4 if (defined $4);
            unshift @type_hier, $v_r->{top};
            my $hN = $5;
            if (!defined $3) {
                # Path violation within a leaf cell
                if ($SS_r->{GS}{VERBOSE}) {
                    print "  Internal path violation encountered in ".
                          $v_r->{top}.", line $line_num.\n";
                }
                @type_hier = ($v_r->{top});
                $hN = "";
            }
            elsif (!@type_hier) {
                command_die($SS_r, "Unexpected syntax on line $line_num of ".
                                   $vfile . ".");
            }
            #if ($type =~ /_UNIQUE$/) {
            #    if ($SS_r->{GS}{DEBUG}) {
            #        print "Ignoring unique subtype $type_hier[$#type_hier].\n";
            #    }
            #    next;
            #}
            my $leaf = $type_hier[$#type_hier];
            my $hT = "";
            foreach my $i (0..$#type_hier-1) {
                if (!exists $mid2num_r->{$type_hier[$i]}) {
                    command_die($SS_r, "Unknown mid-level cell $type_hier[$i] ".
                                       "encountered.");
                }
                $hT .= $mid2num_r->{$type_hier[$i]} . "/";
            }
            if (!exists $leaf2num_r->{$leaf}) {
                command_die($SS_r, "Unknown leaf cell $leaf encountered.");
            }
            my $lnum = $leaf2num_r->{$leaf};
            if ($type eq "FIXED" && !$fixed_lr->[$lnum]) {
                command_die($SS_r, "Inconsistent fixed-size state reported ".
                                   "for leaf cell $leaf.");
            }
            $hT .= $lnum;
            if ($load) {
                # load-only cell
                push @{$load_hThN_lr}, [ $hT, $hN ];
                $leaf_loads{leaf_of($hT)} = 1;
            }
            else {
                # driver+load cell
                push @{$driver_hThN_lr}, [ $hT, $hN ];
                $leaf_drivers{leaf_of($hT)} = 1;
            }
        }
        elsif ($_ =~ /^}$/) {
            if (!@{$load_hThN_lr} && !@{$driver_hThN_lr}) {
                if ($SS_r->{GS}{VERBOSE}) {
                    print "Fully unique violation set encountered within ".
                          "$v_r->{top}.\n";
                    print "  See line $line_num of violations.debug.\n";
                }
            }
            else {
                # Construct/determine ISI
                foreach my $i (0..1) {
                    my $hThN_lr = $i==0 ? $driver_hThN_lr : $load_hThN_lr;
                    foreach my $hThN_r (@{$hThN_lr}) {
                        my $l = leaf_of($hThN_r->[0]);
                        my $inst_spec = [ @{$hThN_r}, $v_r ];
                        my $isi_num = -1;
                        if (!defined $leaf2isi1_lr->[$l]) {
                            $isi_num = push(@{$isis1_lr}, [$inst_spec]) - 1;
                            $leaf2isi1_lr->[$l] = [$isi_num];
                            debug_print($SS_r, "Adding new inst_spec #" .
                                  $isi_num." ". instspec_to_string($inst_spec) .
                                  "\n");
                        }
                        else {
                            foreach my $isi_k (@{$leaf2isi1_lr->[$l]}) {
                                my $isi_r = $isis1_lr->[$isi_k][0];
                                if (instspec_covers($isi_r, $inst_spec)) {
                                    # If this is less general than existing most
                                    # general instance spec, then add this one
                                    # to the end of the ISI list
                                    push @{$isis1_lr->[$isi_k]}, $inst_spec;
                                    $isi_num = $isi_k;
                                    debug_print($SS_r, 
                                        "Adding less general inst_spec ".
                                        instspec_to_string($inst_spec) . "\n");
                                    last;
                                }
                                elsif (instspec_covers($inst_spec, $isi_r)) {
                                    # If this is more general than existing most
                                    # general instance spec, add this one at the
                                    # beginning of the ISI list.  Note: This
                                    # may match to multiple ISIs (will be
                                    # pruned down below).
                                    if ($isi_num == -1) {
                                        unshift @{$isis1_lr->[$isi_k]}, 
                                                $inst_spec;
                                        debug_print($SS_r, 
                                            "Adding more general inst_spec ".
                                            instspec_to_string($inst_spec) .
                                            "\n");
                                        $isi_num = $isi_k;
                                    }
                                    else {
                                        # This is the second matching less
                                        # general isi
                                        $merge_isi_map{$isi_k} = $isi_num;
                                        debug_print($SS_r, 
                                            "Replacing redundant less ".
                                            "general isi #" . $isi_k . " " .
                                            instspec_to_string($isi_r) . 
                                            " with #" . $isi_num . " " .
                                            instspec_to_string($inst_spec) .
                                            "\n");
                                    }
                                }
                            }
                            if ($isi_num == -1) {
                                # This instance spec is not redundant with any
                                # already defined ISIs.  Thus it's unique and 
                                # needs to be added.
                                $isi_num = push(@{$isis1_lr}, [$inst_spec]) - 1;
                                push @{$leaf2isi1_lr->[$l]}, $isi_num;
                                debug_print($SS_r, "Adding unique inst_spec ".
                                      instspec_to_string($inst_spec) . "\n");
                            }
                        }
                        if ($i==0) {
                            push @{$v_r->{drivers}}, $isi_num;
                        }
                        else {
                            push @{$v_r->{loads}}, $isi_num;
                        }
                    }
                }
                push @{$v_r->{leaf_drivers}}, keys(%leaf_drivers);
                push @{$v_r->{leaf_loads}}, keys(%leaf_loads);
                push @{$v1_lr}, $v_r;

            }
            $pstate = 0;
        }
        else {
            print STDERR "Warning: Ignoring unrecognized line $line_num of " .
                         $vfile . ".\n";
        }
    }
    if ($pstate != 0) {
        command_die($SS_r, "Bad parse state at end of $vfile.");
    }
    close VFILE;

    # Reconstruct isis & leaf2isi maps, pruning redundant entries which
    # have arisen from more general inst specs appearing after less general
    # ones.
    if ($SS_r->{GS}{VERBOSE}) {
        print "Reducing instance set identifier space.\n";
    }
    my ($isis_lr, $leaf2isi_lr, $v_lr) = reduce_isis($SS_r, \%merge_isi_map, 
                                             $isis1_lr, $leaf2isi1_lr, $v1_lr);

    return ($v_lr, $isis_lr, $leaf2isi_lr);
}

# Merges the instance-set-identifier space (propagating the merging to
# leaf2isi map and v_lr), given a src_isi -> dest_isi map.
# NOTE: merge_isi_map_r must only map isi numbers -downward-.
sub reduce_isis {
    my $SS_r            = shift;
    my $merge_isi_map_r = shift;
    my $src_isis_lr     = shift;
    my $src_leaf2isi_lr = shift;
    my $src_v_lr        = shift;

    my $isis_lr     = [];
    my $leaf2isi_lr = [];
    my $v_lr        = [];
    my @old2new;            # old-to-new isi numbering

    # Merge isis_lr, maintaining map from old-to-new isi numbers
    foreach my $isi (0..$#{$src_isis_lr}) {
        if (!exists $merge_isi_map_r->{$isi}) {
            $old2new[$isi] = push(@{$isis_lr}, $src_isis_lr->[$isi]) - 1;
        }
        else {
            my $dest_isi = $merge_isi_map_r->{$isi};
            $old2new[$isi] = $old2new[$dest_isi];
            debug_print($SS_r, "Remapping $isi -> $dest_isi ".
                               "(now $old2new[$dest_isi]).\n");
            if (!defined $isis_lr->[$old2new[$dest_isi]]) {
                die "Assertion failure! merge_isi_map doesn't map downwards.\n";
            }
            push @{$isis_lr->[$old2new[$dest_isi]]}, @{$src_isis_lr->[$isi]};
        }
    }
    # Construct new v_lr
    my %old2new_v;
    foreach my $src_v_r (@{$src_v_lr}) {
        my $v_r = { 
            top => $src_v_r->{top},
            vworst => $src_v_r->{vworst},
            line_num => $src_v_r->{line_num},
            leaf_drivers => $src_v_r->{leaf_drivers},
            leaf_loads => $src_v_r->{leaf_loads},
            drivers => [],
            loads => []
        };
        foreach my $i (0..1) {
            my $src_isi_lr = $i==0 ? $src_v_r->{drivers} : $src_v_r->{loads};
            my $isi_lr     = $i==0 ?     $v_r->{drivers} :     $v_r->{loads};
            foreach my $isi (@{$src_isi_lr}) {
                push @{$isi_lr}, $old2new[$isi];
            }
        }
        push @{$v_lr}, $v_r;
        $old2new_v{$src_v_r} = $v_r;
    }
    # Re-assign v_r references in isis_lr
    foreach my $isi (0..$#{$isis_lr}) {
        foreach my $i (0..$#{$isis_lr->[$isi]}) {
            $isis_lr->[$isi][$i][2] = $old2new_v{$isis_lr->[$isi][$i][2]};
        }
    }
    # Merge leaf2isi
    foreach my $l (0..$#{$src_leaf2isi_lr}) {
        debug_print($SS_r, "Reassigning leaf2isi map for leaf cell $l\n");
        $leaf2isi_lr->[$l] = [];
        foreach my $isi (@{$src_leaf2isi_lr->[$l]}) {
            if (!exists $merge_isi_map_r->{$isi}) {
                debug_print($SS_r, "  Adding old isi $isi, now ".
                                   $old2new[$isi].".\n");
                push @{$leaf2isi_lr->[$l]}, $old2new[$isi];
            }
        }
    }

    return ($isis_lr, $leaf2isi_lr, $v_lr);
}
    

# Merges an instance set identifier space down so that no leaf cell can
# be subtyped more than subtype_limit times.  These become the independent
# variables glpsol optimally sets to be 0 (don't subtype) or 1 (subtype).
#
# The merging happens in two steps.  First, unless subtype_limit==-1, the
# space is unconditionally collapsed over common violation set constraints.
# That is, if the violation sets of two ISI's are equivalent in type space
# then they will be combined.  This serves to collapse most homogeneous
# arrays, e.g. in "<i:16: SLACK_1of4(5) sl[i](x[i],y[i]); >" the middle
# buffers will be combined (the l and r buffers might not be, depending
# on what surrounds the slack cells.)
#
# Next, the subtype-limit is adjusted on a per-cell basis.  That is, the
# limit is incremented by 1 if all instances of the leaf cell are covered
# by the ISIS (i.e. if all instances in the ISIS were changed to be the
# same new subtype, there would be no change whatsoever to the sizing 
# problem).  Also in this step a check is made if |leaf2isi(leaf)| is at 
# least two for such cells; if it isn't, then the cell cannot be subtyped
# and it will not appear in the merged ISIS (MISIS) map.  (Note: the
# subtype limit increase is only for purposes of ISIS merging; the sum
# of all MISI subtyping variables for each of these cells will still be 
# constrained to be less-than-or-equal-to the global subtype limit.)
#
# Finally, as long as subtype_limit>0, the ISIS will be merged down to
#   misis_num -> [ isi_1, isi_2, ... ]
#
# Such that |misis^-1(leaf2isi(leaf))| <= subtype_limit(leaf). 
#
sub merge_isis {
    my $SS_r            = shift;
    my $subtype_limit   = shift;
    my $split_threshold = shift;
    my $isis_lr         = shift;
    my $leaf2isi_lr     = shift;
    my $fixed_lr        = shift;
    my $mid2parents_lr  = shift;
    my $leaf2parents_lr = shift;

    # Initial (step 1) ISIS merging
    my $misi2isi1_lr  = [];     # many-valued map
    my $isi2misi1_lr  = [];     # single-valued map (reverse of misi2isi)
    my $leaf2misi1_lr = [];

    if ($SS_r->{GS}{VERBOSE}) {
        print "Performing initial instance space reduction.\n";
    }
    if ($subtype_limit != -1) {
        foreach my $l (0..$#{$leaf2isi_lr}) {
            my @equivalent_isi;
            my @unique_isi_sets;
            debug_print($SS_r, "Analyzing merge possibilities for leaf ".
                               "cell $l.\n");
            foreach my $isi (@{$leaf2isi_lr->[$l]}) {
                my @env_set = determine_isi_env_set($isi, $isis_lr);
                debug_print($SS_r, "Env set for isi $isi: @env_set\n");
                my $unique = 1;
                foreach my $i (0..$#unique_isi_sets) {
                    my $unique_set_lr = $unique_isi_sets[$i];
                    if (isi_env_equivalent(\@env_set, $unique_set_lr)) {
                        debug_print($SS_r, "Merged isi $isi with ".
                              "(@{$equivalent_isi[$i]}).\n");
                        push @{$equivalent_isi[$i]}, $isi;
                        $unique = 0;
                    }
                }
                if ($unique) {
                    debug_print($SS_r,"Determined unique isi $isi.\n");
                    push @equivalent_isi, [$isi];
                    push @unique_isi_sets, \@env_set;
                }
            }
            $leaf2misi1_lr->[$l] = [];
            foreach my $isi_set_lr (@equivalent_isi) {
                my $misi = push(@{$misi2isi1_lr}, $isi_set_lr) - 1;
                push @{$leaf2misi1_lr->[$l]}, $misi;
                foreach my $isi (@{$isi_set_lr}) {
                    if (defined $isi2misi1_lr->[$isi]) {
                        command_die($SS_r, "Assertion: isi2misi should be ".
                                           "single-valued.");
                    }
                    $isi2misi1_lr->[$isi] = $misi;
                }
            }
        }
    }
    else {
        # 1-to-1 map, no merging
        foreach my $i (0..$#{$isis_lr}) {
            push @{$misi2isi1_lr}, [$i];
            push @{$isi2misi1_lr}, $i;
        }
        foreach my $l (0..$#{$leaf2isi_lr}) {
            $leaf2misi1_lr->[$l] = [];
            push @{$leaf2misi1_lr->[$l]}, @{$leaf2isi_lr->[$l]};
        }
    }

    # Debug
    if ($SS_r->{GS}{DEBUG}) {
        print "Initial MISI space:\n";
        foreach my $misi (0..$#{$misi2isi1_lr}) {
            print "  $misi:";
            foreach my $isi (@{$misi2isi1_lr->[$misi]}) {
                print " " . instspec_to_string($isis_lr->[$isi][0]);
            }
            print "\n";
        }
    }

    # Check for total leaf cell coverage within ISIS and degenerate subtyping.
    if ($SS_r->{GS}{VERBOSE}) {
        print "Further merging MISI space to satisfy subtype limit (".
              $subtype_limit.").\n";
    }
    my $degenerate_misi1_lr    = [];
    my $split_misi1_lr         = [];
    my $fully_covered_leafs_lr = [];
    my %merged_misi_map;
    foreach my $l (0..$#{$leaf2misi1_lr}) {
        my $contained = check_leaf_containment($l, $leaf2isi_lr->[$l],
                            $isis_lr, $mid2parents_lr, $leaf2parents_lr);
        #print "Leaf cell $l containment: $contained.\n";
        my $leaf_subtype_limit = $subtype_limit;
        if ($contained) {
            if (@{$leaf2misi1_lr->[$l]}==1) {
                if ($fixed_lr->[$l]) {
                    debug_print($SS_r,"Single MISI fully covers leaf cell ".
                        $l . " (fixed).\n");
                }
                else {
                    debug_print($SS_r,"Single MISI " . $leaf2misi1_lr->[$l][0] .
                        " fully covers sizable leaf cell $l.\n" .
                        "  Will be removed.\n");
                    push @{$degenerate_misi1_lr}, $leaf2misi1_lr->[$l][0];
                }
            }
            else {
                debug_print($SS_r, "All instances of leaf cell $l are fully ".
                    "covered by ".scalar(@{$leaf2misi1_lr->[$l]}) . 
                    " MISI variables.\n");
                push @{$fully_covered_leafs_lr}, $l;
                $leaf_subtype_limit = $subtype_limit + 1;
            }
        }
        debug_print($SS_r, "MISI variables mapped to leaf $l: ".
                    scalar(@{$leaf2misi1_lr->[$l]}) . "\n");

        if ($split_threshold >= 0 && 
            scalar(@{$leaf2misi1_lr->[$l]}) > $split_threshold) {
            debug_print($SS_r, "  Exceeds split threshold; MISIs will be ".
                        "merged and instances will be split if chosen.\n");
            foreach my $i (0..$#{$leaf2misi1_lr->[$l]}) {
                $merged_misi_map{$leaf2misi1_lr->[$l][$i]} =
                    $leaf2misi1_lr->[$l][0];
                # Save number of leaf instances covered by the new (merged)
                # MISI.  This will be number of split subtypes.
                $split_misi1_lr->[$leaf2misi1_lr->[$l][0]] = 
                    scalar(@{$leaf2misi1_lr->[$l]});
            }
        }
        elsif (scalar(@{$leaf2misi1_lr->[$l]}) > $leaf_subtype_limit &&
            $subtype_limit > 0) {
            debug_print($SS_r, "That's too many.  Merging MISIs.\n");

            # FIXME: Do something smarter than this for godsake!
            foreach my $i (0..$#{$leaf2misi1_lr->[$l]}) {
                debug_print($SS_r, "Mapping MISI $leaf2misi1_lr->[$l][$i] to ".
                            $leaf2misi1_lr->[$l][$i%$leaf_subtype_limit] .
                            ".\n");
                $merged_misi_map{$leaf2misi1_lr->[$l][$i]} =
                    $leaf2misi1_lr->[$l][$i%$leaf_subtype_limit];
            }
        }
    }

    # Now, finally, construct final misi structures
    print "Reconstructing MISI space.\n" if ($SS_r->{GS}{VERBOSE});
    my $misi2isi_lr  = [];     # many-valued map
    my $isi2misi_lr  = [];     # single-valued map (reverse of misi2isi)
    my $leaf2misi_lr = [];
    my $degenerate_misi_lr = [];
    my $split_misi_lr      = [];
    my %old2new;
    foreach my $misi (0..$#{$misi2isi1_lr}) {
        if (!exists $merged_misi_map{$misi} || $misi==$merged_misi_map{$misi}) {
            $old2new{$misi} = push(@{$misi2isi_lr}, $misi2isi1_lr->[$misi]) - 1;
        }
        else {
            my $new_misi = $old2new{$merged_misi_map{$misi}};
            push @{$misi2isi_lr->[$new_misi]}, @{$misi2isi1_lr->[$misi]};
            $old2new{$misi} = $new_misi;
        }
    }
    foreach my $isi (0..$#{$isi2misi1_lr}) {
        $isi2misi_lr->[$isi] = $old2new{$isi2misi1_lr->[$isi]};
    }
    foreach my $l (0..$#{$leaf2misi1_lr}) {
        my %new_misi_set;
        foreach my $misi (@{$leaf2misi1_lr->[$l]}) {
            $new_misi_set{$old2new{$misi}} = 1;
        }
        $leaf2misi_lr->[$l] = [ keys %new_misi_set ];
    }
    foreach my $misi (@{$degenerate_misi1_lr}) {
        push @{$degenerate_misi_lr}, $old2new{$misi};
    }
    foreach my $misi (0..$#{$misi2isi1_lr}) {
        if (defined $split_misi1_lr->[$misi] && $split_misi1_lr->[$misi] > 0) {
            $split_misi_lr->[$old2new{$misi}] = $split_misi1_lr->[$misi];
        }
        elsif (!defined $split_misi_lr->[$old2new{$misi}]) {
            $split_misi_lr->[$old2new{$misi}] = 0;
        }
    }

    # Debug
    if ($SS_r->{GS}{DEBUG}) {
        print "Final MISI space:\n";
        foreach my $misi (0..$#{$misi2isi_lr}) {
            print "  $misi:";
            foreach my $isi (@{$misi2isi_lr->[$misi]}) {
                print " " . instspec_to_string($isis_lr->[$isi][0]);
            }
            print "\n";
        }
    }

    return ($misi2isi_lr, $isi2misi_lr, $leaf2misi_lr, $degenerate_misi_lr,
            $split_misi_lr, $fully_covered_leafs_lr);
}

# Determines if the specified leaf cell has any instantiations outside of
# those defined by the ISI space.  Returns 1 if leaf cell is fully contained,
# 0 otherwise.
sub check_leaf_containment {
    my $leaf            = shift;
    my $isi_set_lr      = shift;
    my $isis_lr         = shift;
    my $mid2parents_lr  = shift;
    my $leaf2parents_lr = shift;

    my $cell2parents_lr     = $leaf2parents_lr;
    my @uncovered_instspecs = ( [$leaf, ""] );
    my $active              = 1;

    while ($active && @uncovered_instspecs) {
        my @next_instspecs;
        foreach my $instspec (@uncovered_instspecs) {
            my $top = parent_of($instspec->[0]);
            my $parents_lr = $cell2parents_lr->[$top];
            $active = 0;
            foreach my $pinst (@{$parents_lr}) {
                my ($p, $n) = @{$pinst};
                my $new_instspec = narrow_instspec($instspec, $p, $n);
                my $covered = 0;
                foreach my $isi (@{$isi_set_lr}) {
                    my $isi_instspec = $isis_lr->[$isi][0];
                    if (instspec_equals($isi_instspec, $new_instspec)) {
                        #print "Instance ".instspec_to_string($new_instspec).
                        #    " covered by isi $isi ".
                        #    instspec_to_string($isi_instspec).".\n";
                        $covered = 1;
                        last;
                    }
                }
                if (!$covered) {
                    push @next_instspecs, $new_instspec;
                    #print "Uncovered instance: ".
                    #    instspec_to_string($new_instspec) . "\n";
                }
                $active = 1;
            }
        }
        @uncovered_instspecs = @next_instspecs if ($active);
        $cell2parents_lr = $mid2parents_lr;
    }
    if (@uncovered_instspecs) {
        return 0;
    }
    else {
        return 1;
    }
}
    
# Narrows the scope of the instance specification by adding another level
# of instantiation hierarchy ($parent $name).
sub narrow_instspec {
    my $instspec = shift;
    my $parent   = shift;
    my $name     = shift;

    my $new_instspec = [ @{$instspec} ];
    $new_instspec->[0] = "$parent/" . $instspec->[0];
    if ($instspec->[1] ne "") {
        $new_instspec->[1] = "$name/" . $instspec->[1];
    }
    else {
        $new_instspec->[1] = $name;
    }

    return $new_instspec;
}

sub determine_isi_env_set {
    my $isi     = shift;
    my $isis_lr = shift;

    my %env_set = ();
    foreach my $inst_spec_lr (@{$isis_lr->[$isi]}) {
        my $v_r = $inst_spec_lr->[2];
        foreach my $i (0..1) {
            my $env_isi_lr = $i==0 ? $v_r->{drivers} : $v_r->{loads};
            foreach my $env_isi (@{$env_isi_lr}) {
                my $hT = $isis_lr->[$env_isi][0][0];
                if (!defined $hT) {
                    print "Undefined hT at isi $env_isi, i=$i.\n";
                }
                $env_set{$hT} = 1;
            }
        }
    }
    return ($isis_lr->[$isi][0][0], sort keys %env_set);
}

sub isi_env_equivalent {
    my $env_set1_lr = shift;
    my $env_set2_lr = shift;
    return 0 if (@{$env_set1_lr} != @{$env_set2_lr});
    foreach my $i (0..$#{$env_set1_lr}) {
        return 0 if ($env_set1_lr->[$i] ne $env_set2_lr->[$i]);
    }
    return 1;
}


# Determines if a set of instance specifications are equivalent, i.e.
# (hT_i hN_i) == (hT_j hN_j)
sub instspec_equals {
    my $is1_r = shift;
    foreach my $is_r (@_) {
        return 0 if ($is1_r->[0] ne $is_r->[0] || $is1_r->[1] ne $is_r->[1]);
    }
    return 1;
}

# Determines if the first instance spec is equal to or more general than 
# (i.e. covers) all others provided
sub instspec_covers {
    my $is1_r = shift;
    my $lenT = length($is1_r->[0]);     # type hierarchy
    my $lenN = length($is1_r->[1]);     # name hierarchy
    foreach my $is_r (@_) {
        next if (instspec_equals($is1_r, $is_r));
        my $cmp_lenT = length($is_r->[0]);
        my $cmp_lenN = length($is_r->[1]);
        if ($cmp_lenT <= $lenT || $cmp_lenN <= $lenN ||
            substr($is_r->[0],-$lenT) ne $is1_r->[0] ||
            substr($is_r->[1],-$lenN) ne $is1_r->[1]) {
            return 0 
        }
    }
    return 1;
}

# TODO: Map to real type names maybe
sub instspec_to_string {
    my $inst_spec = shift;
    return "($inst_spec->[0] $inst_spec->[1])";
}

sub run_trial_unmerge {
    my $SS_r                    = shift;
    my $src_trial               = shift;
    my $isis_lr                 = shift;
    my $misi2isi_lr             = shift;
    my $isi2misi_lr             = shift;
    my $leaf2misi_lr            = shift;
    my $degenerate_misi_lr      = shift;
    my $split_misi_lr           = shift;
    my $fully_covered_leafs_lr  = shift;
    my $leaf2num_r              = shift;
    my $num2leaf_lr             = shift;
    my $num2cost_lr             = shift;
    my $mid2num_r               = shift;
    my $num2mid_lr              = shift;
    my $fixed_lr                = shift;
    my $v_lr                    = shift;
    my $mid2parents_lr          = shift;
    my $leaf2parents_lr         = shift;
    my $params_r                = shift;

    # Create new unmerged trial directory
    my ($unmerge_dir, $trial_num, $cast_dir) = 
        create_next_trial_dir($SS_r, $src_trial);

    # Append to README file
    open (README, ">>$unmerge_dir/../README") ||
        command_die($SS_r, "Couldn't write to $unmerge_dir/../README.");

    my $desc_str = bold("TRIAL $trial_num:") . " " .
            "(SL=$params_r->{subtype_limit}, ST=$params_r->{split_threshold}, ".
             "SCE=$params_r->{split_cost_factor} UC=$params_r->{unit_cost}, ".
             "FCS=$params_r->{fixed_cost_scaling}, ".
             "RCS=$params_r->{random_cost_scaling})";
    print $desc_str . "\n";
    print README $desc_str . " [unmerge]\n";
    close README;

    # Write input file for GLPSOL
    my $glpsol_in_file = $unmerge_dir . "/glpsol.in";
    open (GLPSOL_IN, ">$glpsol_in_file") ||
        command_die($SS_r, "Couldn't write to $glpsol_in_file.");

    # Layout cost objective function
    print GLPSOL_IN "minimize\n";
    print GLPSOL_IN "  Cost:\n";
    foreach my $misi (0..$#{$misi2isi_lr}) {
        my $leaf = leaf_of($isis_lr->[$misi2isi_lr->[$misi][0]][0][0]);
        my $cost = ($params_r->{unit_cost}==0 ? $num2cost_lr->[$leaf] : 1.0);
        $cost *= $params_r->{fixed_cost_scaling} if ($fixed_lr->[$leaf]);
        my $r = $params_r->{random_cost_scaling} * rand;
        $cost = (1-$params_r->{random_cost_scaling}) * $cost + $r;
        if ($split_misi_lr->[$misi] > 0) {
            # split_misi_lr gives number of unique leaf instances this MISI 
            # covers (after instances w/ identical cell environments have
            # been merged).
            $cost *= $params_r->{split_cost_factor}==0.0 ? 1.0 :
                     POSIX::ceil($split_misi_lr->[$misi]/
                                 $params_r->{split_cost_factor});
        }
        print GLPSOL_IN "    +" . sprintf("%.4g", $cost) . " x$misi \\\n";
    }
    print GLPSOL_IN "\n";

    # Per-path set constraints
    print GLPSOL_IN "subject to\n";
    foreach my $v_r (@{$v_lr}) {
        my $pnum = $v_r->{line_num};
        my %misi_set;
        my $all_degenerate = 1;
        foreach my $d_isi (@{$v_r->{drivers}}) {
            my $misi = $isi2misi_lr->[$d_isi];
            if (!grep { $_ == $misi } @{$degenerate_misi_lr}) {
                $misi_set{$misi} = 1;
                $all_degenerate = 0;
            }
        }
        my $d_str = "";
        foreach my $m (keys %misi_set) {
            $d_str .= " +x$m";
        }
        if (!@{$v_r->{loads}}) {
            if (!$all_degenerate) {
                # no load-only cells: just output driver cells
                print GLPSOL_IN "  V${pnum}:${d_str} >= 1\n";
            }
            else {
                print "Warning: Skipping unsatisfiable constraint from ".
                      "violations.debug line " . $v_r->{line_num} . ".\n";
            }
        }
        else {
            # violation set includes both driver & load-only cells
            # check if load-only cells account for all fixed-size cells;
            # if so, only require those load cells to be subtyped.
            my $only_load_fixed = check_if_only_load_is_fixed($v_r, $fixed_lr);
            my $lnum = 0;
            my $satisfiable = 0;
            foreach my $l_isi (@{$v_r->{loads}}) {
                my $m = $isi2misi_lr->[$l_isi];
                if ($all_degenerate && grep {$_==$m} @{$degenerate_misi_lr}) {
                    next;
                }
                my $l = leaf_of($isis_lr->[$l_isi][0][0]);
                if (!$only_load_fixed || $only_load_fixed && $fixed_lr->[$l]) {
                    my $v_str = "  V${pnum}_${lnum}:${d_str} ";
                    if (exists $misi_set{$m}) {
                        print GLPSOL_IN $v_str . ">= 1\n";
                    }
                    else {
                        print GLPSOL_IN $v_str . "+x$m >= 1\n";
                    }
                }
                $satisfiable = 1;
                $lnum++;
            }
            if (!$satisfiable) {
                print "Warning: Skipping unsatisfiable constraint from ".
                      "violations.debug line " . $v_r->{line_num} . ".\n";
            }
        }
    }

    # Degenerate instance constraints (these must not be subtyped)
    my $dnum = 0;
    foreach my $d_misi (@{$degenerate_misi_lr}) {
        print GLPSOL_IN "  D${dnum}: +x${d_misi} = 0\n";
        $dnum++;
    }

    # Fully-covered leaf cell subtyping constraints (these must not all
    # be subtyped -- one should be left as the original subtype number)
    my $fcnum = 0;
    foreach my $l (@{$fully_covered_leafs_lr}) {
        print GLPSOL_IN "  FC${fcnum}: ";
        my %misi_set;
        foreach my $misi (@{$leaf2misi_lr->[$l]}) {
            $misi_set{$misi} = 1;
        }
        foreach my $m (keys %misi_set) {
            print GLPSOL_IN "+x${m} ";
        }
        print GLPSOL_IN "< " . scalar(keys %misi_set) . "\n";
        $fcnum++;
    }

    # force all variables to be binary 0/1 integers
    print GLPSOL_IN "binary\n";
    for my $i (0..$#{$misi2isi_lr}) {
        print GLPSOL_IN "  x$i\n";
    }
    print GLPSOL_IN "end\n";
    close GLPSOL_IN;

    # Run GLPSOL
    my $glpsol_out_file = $unmerge_dir . "/glpsol.out";
    my $cmd = "glpsol --lpt $glpsol_in_file --output $glpsol_out_file";
    supersize_system($SS_r, $cmd, $MINOR_JOB);

    # Read solution from GLPSOL
    my @misi2action = ();
    my $success = 0;
    open (GLPSOL_OUT, $glpsol_out_file) ||
        command_die($SS_r, "Didn't receive a solution from glpsol.");
    while (<GLPSOL_OUT>) {
        if (/^(\w+):\s+(.*)$/) {
            if ($1 eq "Status") {
                if ($2 ne "INTEGER OPTIMAL") {
                    command_die($SS_r, "glpsol returned unexpected status $2.");
                }
            }
        }
        elsif (/^\s+\d+\s+/) {
            s/^\s*//;
            my @parts = split /\s+/, $_;
            if ($parts[1] =~ /^x(\d+)$/) {
                my $l = $1;
                if ($parts[3] !~ /^[01]$/) {
                    command_die($SS_r, "Received non-binary solution $parts[3]".
                                       " from glpsol.");
                }
                $misi2action[$l] = $parts[3];
            }
        }
    }
    close GLPSOL_OUT;

    # Verify we got a complete solution, tally layout cost
    my $layout_cost = 0.0;
    my $num_fixed = 0;
    my $num_sizable = 0;
    my @chosen_ones;
    foreach my $misi (0..$#{$misi2isi_lr}) {
        my $l = leaf_of($isis_lr->[$misi2isi_lr->[$misi][0]][0][0]);
        if (!defined $misi2action[$misi]) {
            command_die($SS_r, "Did not receive a solution for MISI $misi.");
        }
        elsif ($misi2action[$misi]) {
            push @chosen_ones, $misi;
            $layout_cost += $num2cost_lr->[$l];
            if ($fixed_lr->[$l]) {
                $num_fixed++;
            }
            else {
                $num_sizable++;
            }
        }
    }

    # Summary
    my $sstr = "";
    for (0..59) { $sstr .= "-"; } $sstr .= "\n";
    $sstr .= "Total layout cost:                   $layout_cost\n";
    $sstr .= "Fixed-size cells to be subtyped:     $num_fixed\n";
    $sstr .= "Sizable cells to be subtyped:        $num_sizable\n";
    for (0..59) { $sstr .= "-"; } $sstr .= "\n";
    $sstr .= "Leaf cells to subtype:\n";
    foreach my $misi (@chosen_ones) {
        $sstr .= "  Instance set $misi: " .
            $num2leaf_lr->[leaf_of($isis_lr->[$misi2isi_lr->[$misi][0]][0][0])];
        $sstr .= " (" . scalar(@{$misi2isi_lr->[$misi]}) . " instances)\n    ";
        foreach my $isi (@{$misi2isi_lr->[$misi]}) {
            $sstr .= instspec_to_string($isis_lr->[$isi][0]) . " ";
        }
        $sstr .= "\n";
    }
    for (0..59) { $sstr .= "-"; } $sstr .= "\n";
    print $sstr;
    write_summary($SS_r, $unmerge_dir, $params_r, $sstr);

    # Copy source merge directory, apply subtype actions
    my $src_dir = trial_to_merge_dir($SS_r, $src_trial);
    `cp -a '$src_dir/cast' '$unmerge_dir'`;
    #subtype_chosen_ones($SS_r, $src_dir, $unmerge_dir, \@chosen_ones, 
    #    $leaf2num_r, $num2leaf_lr, $mid2num_r, $num2mid_lr, $fixed_lr, $v_lr)
    subtype_chosen_ones($SS_r, $src_dir, $unmerge_dir, \@chosen_ones, $isis_lr,
                        $misi2isi_lr, $split_misi_lr, $mid2parents_lr, 
                        $leaf2parents_lr, $num2leaf_lr, $num2mid_lr, $fixed_lr);
}

sub check_if_only_load_is_fixed {
    my $v_r      = shift;
    my $fixed_lr = shift;
    my $only_load_fixed = 0;
    foreach my $l (@{$v_r->{leaf_loads}}) {
        $only_load_fixed = 1 if ($fixed_lr->[$l]);
    }
    foreach my $d (@{$v_r->{leaf_drivers}}) {
        $only_load_fixed = 0 if ($fixed_lr->[$d]);
    }
    return $only_load_fixed;
}

# Returns leaf subtype number of a hierarchical instantiation spec, e.g.
# 4/5/2/7 returns 7.
sub leaf_of {
    my $type_hier = shift;
    my @cells = split /\//, $type_hier;
    return $cells[$#cells];
}

# Returnts top parent subtype number of a hierarchical instantiations spec,
# e.g. 4/5/2/7 returns 4.
sub parent_of {
    my $type_hier = shift;
    my @cells = split /\//, $type_hier;
    return $cells[0];
}

# TODO: Handle isolated leaf cell subtyping (i.e. fixed->non-fixed)
sub subtype_chosen_ones {
    my $SS_r            = shift;
    my $merge_dir       = shift;
    my $unmerge_dir     = shift;
    my $chosen_lr       = shift;
    my $isis_lr         = shift;
    my $misi2isi_lr     = shift;
    my $split_misi_lr   = shift;
    my $mid2parents_lr  = shift;
    my $leaf2parents_lr = shift;
    my $num2leaf_lr     = shift;
    my $num2mid_lr      = shift;
    my $fixed_lr        = shift;

    # Directory path (in addition to SPEC_DIR) over which to exclude new
    # subtypes.  Source merge directory is included in case it is a prior
    # unmerge directory (therefore may contain additional subtypes not
    # in WORK_DIR/cast.
    my $dir_list_lr = [ "$merge_dir/cast", "$SS_r->{GS}{WORK_DIR}/cast" ];
    my $src_path_lr = [ $SS_r->{GS}{SPEC_DIR}, @{$dir_list_lr} ];
    push @{$dir_list_lr}, "$unmerge_dir/cast";

    # New new subtypes will have numbers lower than this
    my $subnum_from = subtype_number_of($SS_r->{GS}{TOP});

    my $mid2changes_lr = [];

    # Create new leaf cell subtypes, construct initial parent instance
    # change lists.
    foreach my $misi (@{$chosen_lr}) {
        my $isi_lr = $misi2isi_lr->[$misi];
        my $first_isi = $isi_lr->[0];
        my $instspec = $isis_lr->[$first_isi][0];
        my $src_lnum = leaf_of($instspec->[0]);
        my $src_leaf = $num2leaf_lr->[$src_lnum];
        my $src_base = basetype_of($src_leaf);

        debug_print($SS_r, "Split count of misi $misi: ".
                           "$split_misi_lr->[$misi]\n");

        my %isi2new;
        my $new_leaf;
        foreach my $isi (@{$isi_lr}) {
            if (!%isi2new || $split_misi_lr->[$misi]) {
                $new_leaf = $src_base . "." .
                               find_next_available_subtype($SS_r, $dir_list_lr, 
                                    $src_base, undef, $subnum_from, -1);
                print "Creating new leaf cell $new_leaf.\n";
                my $root_dir = look_for_subtype($src_leaf, $src_path_lr);
                copy_subtype($SS_r, $root_dir, $src_leaf, "$unmerge_dir/cast", 
                             $new_leaf);

                # Check if source had fixed_size=true, if so make it sizable
                if ($fixed_lr->[$src_lnum]) {
                    debug_print($SS_r, " Setting fixed_size=false.\n");
                    set_subtype_directives($SS_r, "top", "fixed_size",
                        {$new_leaf => "false"}, 0, "$unmerge_dir/cast");
                }
            }
            $isi2new{$isi} = $new_leaf;
        }

        # Register subtyping as instance change in parent cell of each isi
        foreach my $isi (@{$isi_lr}) {
            register_subtype_change($isis_lr->[$isi][0], $isi2new{$isi}, 
                                    $mid2changes_lr);
        }
    }

    # Now iteratively apply instance changes to mid-level cells, traversing
    # in reverse-instantiation order.
    print "Propagating subtyping up the hierarchy.\n";
    foreach my $i (0..$#{$num2mid_lr}) {
        my $mnum = $#{$num2mid_lr} - $i;
        my $fqcn = $num2mid_lr->[$mnum];
        if (!defined $mid2changes_lr->[$mnum]) {
            print "No changes to be made to mid cell $fqcn ($mnum).\n";
            next;
        }
        print "Processing changes to cell $fqcn ($mnum).\n";

        # Read mid-level subtype info
        my $root_dir = look_for_subtype($fqcn, $src_path_lr);
        my ($cell_type, $src_instmap_r, $src_head_r, $src_tail_r, $src_fixed) =
            read_subtype($SS_r, $root_dir, $fqcn);

        my $last_change;
        my %new_instmap = %{$src_instmap_r};
        my $new_subtype = $fqcn;
        foreach my $change (@{$mid2changes_lr->[$mnum]}) {
            my $instspec = $change->[0];
            my $subtype  = $change->[1];
            my $subname  = $change->[2];
            if (!exists $src_instmap_r->{$subname}) {
                print "Warning: Instance $subname not found in $fqcn!\n";
                print "         Ignoring subtype change.\n";
                next;
            }
            my $basetype = $src_instmap_r->{$subname}[0];

            if ($change->[0][1] eq "") {
                # completely localized change (i.e. applies to all instances)
                $src_instmap_r->{$subname} = [$basetype, $subtype];
                if (defined $last_change) {
                    print "Assertion error: new_instmap should be null.\n";
                }
            }
            else {
                #print ">> instspec = " . instspec_to_string($instspec) . "\n";
                if (!defined $last_change) {
                    # First new subtype. Apply change.
                    $new_subtype = basetype_of($fqcn) . "." .
                        find_next_available_subtype($SS_r, $dir_list_lr, 
                            basetype_of($fqcn), undef, $subnum_from, -1);
                    print " Assigning new subtype $new_subtype.\n";
                    register_subtype_change($instspec, $new_subtype, 
                                            $mid2changes_lr);
                }
                elsif (!instspec_covers($last_change->[0], $instspec)) {
                    # This one isn't a sub-subtype of the previous one. 
                    # (A sub-subtype is one which shared some of the changes 
                    # of a more localized subtype.)  Thus it requires a new
                    # subtype.
                    debug_print($SS_r, " Writing subtype.\n");
                    write_subtype($SS_r, "$unmerge_dir/cast", $new_subtype,
                                  $cell_type, \%new_instmap, $src_head_r,
                                  $src_tail_r);
                    $new_subtype = basetype_of($fqcn) . "." .
                        find_next_available_subtype($SS_r, $dir_list_lr, 
                            basetype_of($fqcn), undef, $subnum_from, -1);
                    print " Assigning new subtype $new_subtype.\n";
                    register_subtype_change($instspec, $new_subtype, 
                                            $mid2changes_lr);

                    %new_instmap = %{$src_instmap_r};
                }
                elsif (defined $last_change &&
                       instspec_equals($last_change->[0], $instspec)) {
                    # New subtype not yet needed.  Apply change to this one.
                }
                else {
                    if (!instspec_covers($last_change->[0], $instspec)) {
                        print "Assertion error: last change should cover ".
                              "this one! ($fqcn)\n";
                    }
                    # A more specific (less localized/general) change to a
                    # new subtype whose other changes match this change's 
                    # instance(s).  (This is a sub-subtype of the previous 
                    # subtype.)
                    debug_print($SS_r, " Writing subtype.\n");
                    write_subtype($SS_r, "$unmerge_dir/cast", $new_subtype,
                                  $cell_type, \%new_instmap, $src_head_r,
                                  $src_tail_r);
                    $new_subtype = basetype_of($fqcn) . "." .
                        find_next_available_subtype($SS_r, $dir_list_lr, 
                            basetype_of($fqcn), undef, $subnum_from, -1);
                    print " Assigning new subtype $new_subtype.\n";
                    register_subtype_change($instspec, $new_subtype, 
                                            $mid2changes_lr);
                }
                $new_instmap{$subname} = [$basetype, $subtype];
                $last_change = $change;
            }
        }

        debug_print($SS_r, " Writing subtype.\n");
        write_subtype($SS_r, "$unmerge_dir/cast", $new_subtype,
                      $cell_type, (defined $last_change ? \%new_instmap
                                                        : $src_instmap_r), 
                      $src_head_r, $src_tail_r);

    }
}

# Registers a subtype change in the subtype's parent change list
sub register_subtype_change {
    my $instspec       = shift;     # Old instspec of changed subcell
    my $new_subcell    = shift;     # New cell number of subcell
    my $mid2changes_lr = shift;

    # Remove leaf of instspec
    my $parent_instspec = [];
    my $name;
    if ($instspec->[0] =~ /^(.*)\/([^\/]+)$/) {
        $parent_instspec->[0] = $1;
    }
    else {
        warn "Assertion error: instspec is degenerate in ".
              "register_subtype_change. (skipping)\n";
        return;
    }
    if ($instspec->[1] =~ /^(.*)\/([^\/]+)$/) {
        $parent_instspec->[1] = $1;
        $name = $2;
    }
    else {
        $parent_instspec->[1] = "";
        $name = $instspec->[1];
    }
        
    my $pnum = leaf_of($parent_instspec->[0]);
    if (!defined $mid2changes_lr->[$pnum]) {
        $mid2changes_lr->[$pnum] = [];
    }
    my $changes_lr = $mid2changes_lr->[$pnum];

    #print " Registering change in parent cell $pnum.\n";

    # Insert change in an ordered manner (more-to-less general/localized)
    my @new_changes;
    my $inserted = 0;
    my $last_covered = 0;
    my $covers;
    my $change;
    foreach $change (@{$changes_lr}) {
        if (!$inserted) {
            if (instspec_covers($parent_instspec, $change->[0])) {
                # This instspec matches existing and is more general 
                push @new_changes, [$parent_instspec, $new_subcell, $name];
                $inserted = 1;
            }
            else {
                $covers = instspec_covers($change->[0], $parent_instspec);
                if (!$covers && $last_covered) {
                    # This instspec matched the last element but was less 
                    # general.  It no longer matches, so this is where it 
                    # belongs.
                    push @new_changes, [$parent_instspec, $new_subcell, $name];
                    $inserted = 1;
                }
                $last_covered = $covers;
            }
        }
        push @new_changes, $change;
    }
    if (!$inserted) {
        # No match (or the least general of the last matching set)
        push @new_changes, [$parent_instspec, $new_subcell, $name];
    }

    # Not a very efficient way to insert elements but unfortunately 
    # perl doesn't seem to offer a better way
    @{$changes_lr} = @new_changes;
}

# Writes summary of this unmerging trial to $trial_dir/TRIAL.
# TODO: Make this more useful, i.e. give new subtype of each unmerged
# instance.
sub write_summary {
    my $SS_r      = shift;
    my $trial_dir = shift;
    my $params_r  = shift;
    my $summary   = shift;

    open (TRIAL, ">$trial_dir/TRIAL") ||
        command_die($SS_r, "Couldn't write to $trial_dir/TRIAL.");
    print TRIAL "subtype-limit       = $params_r->{subtype_limit}\n";
    print TRIAL "split-threshold     = $params_r->{split_threshold}\n";
    print TRIAL "split-cost-factor   = $params_r->{split_cost_factor}\n";
    print TRIAL "unit-cost           = $params_r->{unit_cost}\n";
    print TRIAL "fixed-cost-scaling  = $params_r->{fixed_cost_scaling}\n";
    print TRIAL "random-cost-scaling = $params_r->{random_cost_scaling}\n";
    print TRIAL "random-seed         = $params_r->{random_seed}\n";
    print TRIAL $summary;
    close TRIAL;
}

1;
