#
# DensityFactor
#
# Density factor prediction & optimization commands.
#

package Supersize::DensityFactor;

BEGIN {
    use Exporter;
    our @ISA = qw(Exporter);
    our @EXPORT = qw(
        &update_glc_density_factors
        &clear_manual_flow_density_factors
    );
    our @EXPORT_OK = qw( 
        &get_module_data
    );
}

use strict;
use Supersize::LayoutIntegration;
use Supersize::LayoutModeling;
use Supersize::ModifySubtypes;
use Supersize::Netlist;
use Supersize::JavaUtil;
use Supersize::TypeUtil;
use Supersize::Util;
use Term::ANSIColor;

sub get_module_data {
  return { 
    NAME => "DensityFactor",
    DESC => "Density factor prediction and optimization commands.",
    COMMANDS => {
    "update_density_factors" => {
        SUBREF => \&update_density_factors,
        USAGE  => "update_density_factors",
        DESC =>
          "Calculates the GLC density factors of all sizable subtypes which ".
          "have auto_layout==1, and then sets their density_factor directives ".
          "appropriately.",
        GLOBALS => {
          WORK_DIR => { REQUIRED => 1 },
          PDK_ROOT => { REQUIRED => 1 } }
    },
    "optimize_density_factors" => {
        SUBREF => \&optimize_density_factors,
        USAGE  => "optimize_density_factors " . 
                              "--area-limit=".
                                    underline("area_increase_limit")." " .
                              "[--glc-instances=< instmap1 instmap2 ... > " .
                              "--glc-leaf-data=< leafdata1 leafdata2 ... > " .
                              "--glc-areas=< glcmap1 glcmap2 ... >] " .
                              "[--instances-dir=".underline("inst_dir") . "] ".
                              "[--exclude-instances=".underline("excl_list").
                              "] " . "--constant-manual-cost " .
                              "[--critical-width-paths=<path1 path2 ...>] ".
                              "[--critical-width-limits=<w1 w2 ...>]",
        DESC   =>
          "Determine the optimal set of density factors to annotate into " .
          "all sizable cells of TOP, given the following information:\n\n".
          "  1. Acceptable overall area increase limit (e.g. 1.15 to allow\n" .
          "     a 15% increase).\n\n" .
          "  2. GLC trial routing data for the cells from prior merging\n" .
          "     trials.  (Results may be incomplete.)  These results are\n" .
          "     specified by three lists of map variables, with each list\n" .
          "     element corresponding to a GLC trial.  The three required\n" .
          "     maps are the following:\n" .
          "       glc-instances   -  Subtype -> instance list map\n" .
          "       glc-leaf-data   -  Subtype -> leaf_data_map\n" .
          "       glc-areas       -  Subtype -> area of GLC layout view\n\n" .
          "  3. A mk_instances output directory (defaults to\n" .
          "     WORK_DIR/instances) in which initial floorplan areas for\n" .
          "     each subtype in the final merged subtype space (of\n" .
          "     WORK_DIR/cast) may be found.  This is specified with\n".
          "     " . underline("inst_dir") . ".\n\n" .
          "Outputs a summary of results, and sets two map return variables " .
          "specifying the auto_layout and density_factor directives for all ".
          "sizable leaf cells, suitable for passing to " . 
          bold("set_directive") . ".",
        IV => {
          layout_cost_of_gate => {
            TYPE => $TYPE_SCALAR,
            DESC => "Layout cost of a gate instance (1.0 default)." },
          layout_cost_of_stack => {
            TYPE => $TYPE_SCALAR,
            DESC => "Layout cost of a stack instance (1.0 default)." },
          enable_manual_layout_cost_scaling => {
            TYPE => $TYPE_SCALAR,
            DESC => "When enabled, a layout cost scaling term is applied ".
                    "to all terms, calculated as glc_density_factor / " .
                    "manual_density_factor.  If disabled (set to 0), ".
                    "this scaling will not be applied.  The cost model ".
                    "will simply be layout_cost_of_gate * num_gates + " .
                    "layout_cost_of_stack * num_stacks." } },
        RV => {
          auto_layout_directives => {
            TYPE => $TYPE_MAP,
            DESC => "Map of leaf_fqcn -> auto_flow{0,1} directive, " .
                    "identifying which flow the cell is to be routed with. ".
                    "To be passed to ".bold("set_directive")."." },
          density_factor_directives => {
            TYPE => $TYPE_MAP,
            DESC => "Map of leaf_fqcn -> density factor which satisfies the ".
                    "area increase constraint.  To be passed to " .
                    bold("set_directive")."." } },
        GLOBALS => {
          WORK_DIR => { REQUIRED => 1 },
          CAST_DIR => { REQUIRED => 1 },
          SPEC_DIR => { REQUIRED => 1 },
          PDK_ROOT => { REQUIRED => 1 } } },
    "get_merge_space_info" => {
        SUBREF => \&get_merge_space_info,
        USAGE  => "get_merge_space_info " . 
                    "[--exclude-instances=".underline("excl_list")."]",
        DESC   =>
          "For a prior merge space with GLC trial data, retrieves all " .
          "information needed for a subsequent call to " .  
          bold("optimize_density_factors").".  Use this command to retrieve ".
          "the Cast data from the old working directory, then save the ".
          "returned maps to files using " . bold("write_file") . ", then ".
          "load them into the session in which you will run the optimization.",
        RV => {
          instance_map => {
            TYPE => $TYPE_MAP,
            DESC => "Map of subtype -> de-arrayed instances under TOP." },
          leaf_data_map => {
            TYPE => $TYPE_MAP,
            DESC => "Map of subtype -> ( inst_count, transistor_area, " .
                    "height, pcell_count, net_count )." } },
        GLOBALS => {
          WORK_DIR => { REQUIRED => 1 },
          CAST_DIR => { REQUIRED => 1 },
          SPEC_DIR => { REQUIRED => 1 },
          PDK_ROOT => { REQUIRED => 1 } } },
    "estimate_glc_area" => {
        SUBREF => \&estimate_glc_area,
        USAGE  => "estimate_glc_area [" . underline("cell") . " | " .
                                         underline("cell_list") . "]",
        DESC   =>
          "Estimates the Gen Leaf Cell areas of the specified cell(s) ".
          "using the linear predictor.  Both cell areas and density " .
          "factors are reported.  If no cells are specified, values " .
          "are reported for all sizable leaf cells under TOP.",
        RV => {
          result => {
            TYPE => $TYPE_MAP,
            DESC => "Map of subtype -> cell area." } },
        GLOBALS => {
          WORK_DIR => { REQUIRED => 1 },
          CAST_DIR => { REQUIRED => 1 },
          SPEC_DIR => { REQUIRED => 1 },
          PDK_ROOT => { REQUIRED => 1 } } } 
    }
  };
}

#
# update_density_factors
#
sub update_density_factors {
    my $SS_r = shift;

    # parse args
    while (num_args(\@_)) {
        my $arg = shift_next_scalar(\@_);
        if (defined $arg) {
            command_die($SS_r, "Unrecognized argument $arg.");
        }
        else {
            command_die($SS_r, "Bad argument specified.");
        }
    }
    update_glc_density_factors($SS_r);
}

sub update_glc_density_factors {
    my $SS_r = shift;
    my $wdir = shift;
    $wdir = get_work_dir($SS_r) if (!defined $wdir);

    # Determine necessary cell data
    print "Updating density_factor directives.\n";
    my $d_r = get_subtype_info($SS_r, $wdir, undef, [], 0, undef, 0, 1);

    my $cnt = 0;
    foreach my $c (keys %{$d_r}) {
        my $glc_df = predict_glc_area($SS_r, $d_r->{$c}{ta},
                                $d_r->{$c}{height}, $d_r->{$c}{pcell_cnt},
                                $d_r->{$c}{net_cnt}, $d_r->{$c}{bitpitch}) /
                                $d_r->{$c}{ta};
        get_or_set_directive_in_subtype($SS_r, "$wdir/cast", $c, 1,
                                        { 'density_factor' =>
                                            sprintf("%.3g", $glc_df) },
                                        "top", 0);
        $cnt++;
    }
    print "Updated $cnt cells.\n";
}

# Run this prior to sizing in order to remove all density_factor directives
# in cells with auto_layout==0.  Jauto will emit the correct density_factor
# directives for these cells.
sub clear_manual_flow_density_factors {
    my $SS_r = shift;
    my $wdir = shift;
    my $fqcn = shift;
    $wdir = get_work_dir($SS_r) if (!defined $wdir);

    my @manual_cells = ();
    my $cmd = "--cell=$fqcn --task=subcells " .
              "--filter=!fixed&leaf&directive=auto_layout:0";
    query_cast_server($SS_r, $cmd, \@manual_cells, 1);
    my %subtype_to_value;
    foreach my $c (@manual_cells) {
        $subtype_to_value{$c} = "-";
    }
    set_subtype_directives($SS_r, "top", "density_factor", \%subtype_to_value,
                           0, "$wdir/cast");
}


#
# optimize_density_factors
#
sub optimize_density_factors {
    my $SS_r = shift;

    my $skip_extract = 0;
    my $const_manual_cost = 0;  # Don't try to model manual layout costs
    my $area_limit;
    my @glc_instances;
    my @glc_leaf_data;
    my @glc_trial_areas;
    my @crit_width_paths;
    my $crit_width_limits_lr = [];
    my $inst_exclusions_lr;
    my $wdir = get_work_dir($SS_r);
    my $fp_instances_dir = "$wdir/instances";

    # parse args
    while (num_args(\@_)) {
        my ($type, $val) = shift_next_arg(\@_);
        if ($type == $TYPE_SCALAR) {
            if ($val eq "--area-limit") {
                my $eq = shift_next_scalar(\@_);
                $area_limit = shift_next_scalar(\@_);
                if (!defined $eq || $eq ne "=" || !defined $area_limit) {
                    command_die($SS_r, "Bad argument to $val.");
                }
            }
            elsif ($val eq "--glc-instances") {
                my $eq = shift_next_scalar(\@_);
                @glc_instances = shift_next_list_of_maps($SS_r, \@_);
                if (!defined $eq || $eq ne "=" || !@glc_instances) {
                    command_die($SS_r, "Bad argument to $val.");
                }
            }
            elsif ($val eq "--glc-leaf-data") {
                my $eq = shift_next_scalar(\@_);
                @glc_leaf_data = shift_next_list_of_maps($SS_r, \@_);
                if (!defined $eq || $eq ne "=" || !@glc_leaf_data) {
                    command_die($SS_r, "Bad argument to $val.");
                }
            }
            elsif ($val eq "--glc-areas") {
                my $eq = shift_next_scalar(\@_);
                @glc_trial_areas = shift_next_list_of_maps($SS_r, \@_);
                if (!defined $eq || $eq ne "=" || !@glc_trial_areas) {
                    command_die($SS_r, "Bad argument to $val.");
                }
            }
            elsif ($val eq "--instances-dir") {
                my $eq = shift_next_scalar(\@_);
                $fp_instances_dir = shift_next_scalar(\@_);
                if (!defined $eq || $eq ne "=" || !defined $fp_instances_dir) {
                    command_die($SS_r, "Bad argument to $val.");
                }
            }
            elsif ($val eq "--exclude-instances") {
                my $eq = shift_next_scalar(\@_);
                $inst_exclusions_lr = shift_next_list(\@_);
                if (!defined $eq || $eq ne "=" || !defined $inst_exclusions_lr){
                    command_die($SS_r, "Bad arguments to $val.");
                }
            }
            elsif ($val eq "--constant-manual-cost") {
                $const_manual_cost = 1;
            }
            elsif ($val eq "--skip-extract") {
                $skip_extract = 1;
            }
            elsif ($val eq "--critical-width-paths") {
                my $eq = shift_next_scalar(\@_);
                @crit_width_paths = shift_next_list_of_maps($SS_r, \@_);
                if (!defined $eq || $eq ne "=" || !@crit_width_paths) {
                    command_die($SS_r, "Bad argument to $val.");
                }
            }
            elsif ($val eq "--critical-width-limits") {
                my $eq = shift_next_scalar(\@_);
                $crit_width_limits_lr = shift_next_list(\@_);
                if (!defined $eq || $eq ne "=" || 
                    !defined $crit_width_limits_lr){
                    command_die($SS_r, "Bad arguments to $val.");
                }
            }
            else {
                command_die($SS_r, "Unrecognized argument $val.");
            }
        }
        else {
            command_die($SS_r, "Bad arguments to derive_density_factors.");
        }
    }
    if (!defined $area_limit || !defined $fp_instances_dir) {
        command_die($SS_r, "Insufficient arguments: must supply area-limit ".
                           "and either fp-areas or\ninstances-dir.");
    }
    if ((@glc_instances || @glc_leaf_data || @glc_trial_areas) && 
        (!@glc_instances || !@glc_leaf_data || !@glc_trial_areas)) {
        command_die($SS_r, "Incomplete glc data set arguments.");
    }
    if (@crit_width_paths != @{$crit_width_limits_lr}) {
        command_die($SS_r, "Inconsistent sizes of critical width arguments.");
    }

    # Make sure floorplan instances files are up-to-date
    unless ($skip_extract) {
        print "Extracting floorplan placement...\n";
        extract_dfII_instances($SS_r, $SS_r->{GS}{TOP}, "floorplan", $wdir);
    }

    # Determine necessary drop data
    print "Obtaining leaf cell data from cast...\n";
    my $d_r = get_subtype_info($SS_r, $wdir, undef, 
                               $inst_exclusions_lr, 1, $fp_instances_dir, 1, 0);

    # invert glc subtype->(instlist) maps
    my @inv_glc_inst_maps;
    foreach my $glc_inst_map_r (@glc_instances) {
        my $inv_glc_inst_mr = {};
        foreach my $c (keys %{$glc_inst_map_r}) {
            if ($glc_inst_map_r->{$c}[0] != $TYPE_LIST) {
                command_die($SS_r, "Invalid glc map type (map to scalar; " .
                                   "expecting map to list).");
            }
            foreach my $i (@{$glc_inst_map_r->{$c}[1]}) {
                $inv_glc_inst_mr->{$i} = $c;
            }
        }
        push @inv_glc_inst_maps, $inv_glc_inst_mr;
    }

    # Determine best glc area guesses for all sizable leaf cells
    my $TIA = 0.0;                  # total initial area
    my $TMA = 0.0;                  # total manual flow area
    my $TAA = 0.0;                  # total auto flow (glc) area
    my $total_area_diff_mr = {};
    my @stats;
    for my $i (0..1+scalar(@glc_trial_areas)) { $stats[$i] = 0; }
    print "Calculating GLC areas...\n";
    foreach my $c (keys %{$d_r}) {
        my $glc_prediction = predict_glc_area($SS_r, $d_r->{$c}{ta},
                                $d_r->{$c}{height}, $d_r->{$c}{pcell_cnt},
                                $d_r->{$c}{net_cnt}, $d_r->{$c}{bitpitch});
        ($d_r->{$c}{glc_area}, my $action) = 
                             best_match_area($SS_r, $c, $d_r->{$c}{inst_list},
                                $glc_prediction, $d_r->{$c}{ta},
                                \@inv_glc_inst_maps, \@glc_leaf_data,
                                \@glc_trial_areas);
        $stats[2+$action]++;
        $d_r->{$c}{match_src} = $action;
        $TIA += $d_r->{$c}{fp_area} * $d_r->{$c}{inst_cnt};
        $TAA += $d_r->{$c}{glc_area} * $d_r->{$c}{inst_cnt};
        my $manual_area = predict_manual_area($d_r->{$c}{manual_df},
                                              $d_r->{$c}{ta},
                                              $d_r->{$c}{height},
                                              $d_r->{$c}{bitpitch},
                                              $d_r->{$c}{width_minimum});
        $TMA += $manual_area * $d_r->{$c}{inst_cnt};
        $total_area_diff_mr->{$c} = 
            ($d_r->{$c}{glc_area} - $manual_area) * $d_r->{$c}{inst_cnt};
        $d_r->{$c}{manual_area} = $manual_area;
        $d_r->{$c}{manual_cost} = $const_manual_cost ? 1.0 :
            calculate_manual_cost($c, $d_r->{$c});
    }
    write_area_contribution_debug_file($SS_r, $d_r, 
                                       "manual_area_contributions.csv",
                                       "manual_area", "manual_cost");
    write_area_contribution_debug_file($SS_r, $d_r,
                                       "floorplan_area_contributions.csv",
                                       "fp_area");
    write_area_contribution_debug_file($SS_r, $d_r,
                                       "predicted_glc_area_contributions.csv",
                                       "glc_area", "match_src");

    # Sort total area contribution differences from largest to smallest
    # (largest being hightest cost for accepting the glc 
    print "Solving...\n";
    my $diff_budget = ($area_limit-1) * $TIA;
    my ($M, $TDA, $total_cost, $auto_layout_mr, $crit_width_results) = 
        solve_for_density_factors($SS_r, $d_r,
                                  $diff_budget, $total_area_diff_mr,
                                  \@crit_width_paths,
                                  $crit_width_limits_lr);
    print "Done.\n";

    # Set return values
    my $auto_layout_rv = [ $TYPE_MAP, {} ];
    my $df_rv = [ $TYPE_MAP, {} ];
    foreach my $c (keys %{$auto_layout_mr}) {
        if ($auto_layout_mr->{$c}) {
            $auto_layout_rv->[1]{$c} = [ $TYPE_SCALAR, 1 ];
            $df_rv->[1]{$c} = [ $TYPE_SCALAR, 
                sprintf("%.3g", $d_r->{$c}{glc_area} / $d_r->{$c}{ta}) ];
        }
        else {
            $auto_layout_rv->[1]{$c} = [ $TYPE_SCALAR, 0 ];
            $df_rv->[1]{$c} = [ $TYPE_SCALAR, $d_r->{$c}{manual_df} ];
        }
    }
    set_cmd_return_variable($SS_r, "auto_layout_directives", $auto_layout_rv);
    set_cmd_return_variable($SS_r, "density_factor_directives", $df_rv);

    # Print summary
    for (0..77) { print "-"; } print "\n";
    my $t = 0;
    for my $i (2..2+$#glc_trial_areas) {
        $t += $stats[$i];
    }
    print "Number of GLC trial cell areas used:                 $t\n";
    for my $i (2..2+$#glc_trial_areas) {
        print "  Number of areas used from trial " . ($i-2) . 
            ":                 $stats[$i]\n";
    }
    print "Number unused due to large predicted discrepancy:    $stats[1]\n";
    print "Number of cells with no match in any GLC trial run:  $stats[0]\n";

    for (0..77) { print "-"; } print "\n";
    print "Total floorplanned area:                             " . 
                sprintf("%.3g", $TIA/1e6) . " mm^2\n";
    print "Total manual flow area:                              " . 
                sprintf("%.3g", $TMA/1e6) . " mm^2\n";
    print "Total GLC flow area:                                 " . 
                sprintf("%.3g", $TAA/1e6) . " mm^2\n";

    if (@{$crit_width_results}) {
        for (0..77) { print "-"; } print "\n";
        for my $i (0..$#{$crit_width_limits_lr}) {
            print "Critical width " . sprintf("%2s", $i) . 
                  ":                                   " . 
                  sprintf("%.3g", $crit_width_results->[$i]) . " um\n";
        }
    }

    for (0..77) { print "="; } print "\n";
    print "Optimal number of manual flow cells:                 " . $M . "\n";
    print "Total layout work (hours):                           " .
        sprintf("%.5g", $total_cost/60) . "\n";
    print "Final area increase:                                 " .
        sprintf("%.3g", $TDA/1e6) . " mm^2  (" .
        sprintf("%.3g",100.0*$TDA/$TIA) . "%)\n";
    print "Allowed area increase:                               " . 
                sprintf("%.3g",$diff_budget/1e6) . " mm^2\n";
    for (0..77) { print "="; } print "\n";

    print "Manual leaf cells:\n\n";
    STDOUT->format_top_name("Cell_List_Top");
    STDOUT->format_name("Cell_List");
    STDOUT->format_lines_per_page(10000);
    STDOUT->format_lines_left(0);
    my $c;
    foreach $c (sort { $total_area_diff_mr->{$b} <=> 
                       $total_area_diff_mr->{$a} }
                     keys %{$auto_layout_mr}) {
        if ($auto_layout_mr->{$c}==0) {
            #print "  $c  " . sprintf("%.3g", $total_area_diff_mr->{$c});
            #print "  " . sprintf("%.3g", $d_r->{$c}{manual_cost}) . "\n";
            write;
        }
    }

    format Cell_List_Top =
 Cell                                                  FP Area  Saved  DF Cost
 ----------------------------------------------------- ------- ------- -- ----
.                                
    format Cell_List =           
 @<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< @>>>>>> @>>>>>> @> @>>>
$c,sprintf("%.4g",$d_r->{$c}{fp_area}*$d_r->{$c}{inst_cnt}/1e6),sprintf("%.4g",$total_area_diff_mr->{$c}/1e6),int($d_r->{$c}{manual_df}),sprintf("%.3g",$d_r->{$c}{manual_cost})
.
}

sub solve_for_density_factors {
    my $SS_r               = shift;
    my $d_r                = shift;
    my $diff_budget        = shift; # total area increase budget
    my $total_area_diff_mr = shift; # total per-cell GLC-vs-manual area cost
    my $crit_width_paths_lr= shift;
    my $crit_width_limits_lr=shift;

    my $wdir = get_work_dir($SS_r);

    # Enumerate cells
    my @cells;
    my %cell_num;
    foreach my $c (keys %{$total_area_diff_mr}) {
        push @cells, $c;
        $cell_num{$c} = $#cells;
    }

    # Write input file for GLPSOL
    my $glpsol_in_file = "$wdir/glpsol.in";
    open (GLPSOL_IN, ">$glpsol_in_file") ||
        command_die($SS_r, "Couldn't write to $glpsol_in_file.");

    # Manual layout cost objective function
    print GLPSOL_IN "minimize\n";
    print GLPSOL_IN "  Cost:\n";
    for my $i (0..$#cells) {
        print GLPSOL_IN "    +" . $d_r->{$cells[$i]}{manual_cost} . " m$i \\\n";
    }

    # Primary area growth constraint
    print GLPSOL_IN "subject to\n";
    print GLPSOL_IN "  Area:\n";
    for my $i (0..$#cells) {
        print GLPSOL_IN "    ";
        my $d = $total_area_diff_mr->{$cells[$i]};
        print GLPSOL_IN "+" if ($d>=0.0);
        print GLPSOL_IN "$d a$i \\\n";
    }
    print GLPSOL_IN "    <= $diff_budget\n";

    # Critical width constraints
    for my $i (0..$#{$crit_width_paths_lr}) {
        print GLPSOL_IN "  Width$i:\n";
        foreach my $c (keys %{$crit_width_paths_lr->[$i]}) {
            if (!exists $d_r->{$c}{glc_area}) {
                print STDERR "Warning: Undefined GLC area for cell $c.\n";
                next;
            }
            if (!exists $d_r->{$c}{manual_area}) {
                print STDERR "Warning: Undefined Manual area for cell $c.\n";
                next;
            }
            if (!exists $d_r->{$c}{height}) {
                print STDERR "Warning: Undefined height for cell $c.\n";
                next;
            }
            my $dw = ($d_r->{$c}{glc_area} - $d_r->{$c}{manual_area}) / 
                     ($d_r->{$c}{height} * $d_r->{$c}{bitpitch}) * 
                     $crit_width_paths_lr->[$i]{$c}[1];
            print GLPSOL_IN "    ";
            print GLPSOL_IN "+" if ($dw>=0.0);
            print GLPSOL_IN "$dw a" . $cell_num{$c} . " \\\n";
        }
        print GLPSOL_IN "    <= $crit_width_limits_lr->[$i]\n";
    }

    # force all variables to be binary 0/1 integers
    for my $i (0..$#cells) {
        print GLPSOL_IN "  x$i: m$i + a$i = 1\n";
    }

    print GLPSOL_IN "binary\n";
    foreach my $i (0..$#cells) {
        print GLPSOL_IN "  m$i\n  a$i\n";
    }
    print GLPSOL_IN "end\n";
    close GLPSOL_IN;

    # Run GLPSOL
    my $glpsol_out_file = "$wdir/glpsol.out";
    my $cmd = "glpsol --lpt $glpsol_in_file --output $glpsol_out_file";
    supersize_system($SS_r, $cmd, $MINOR_JOB);

    # Read solution from GLPSOL
    my $M;
    my $TDA;
    my $auto_layout_mr = {};
    my $crit_width_results = [];
    my $cost = "Unknown";
    open (GLPSOL_OUT, $glpsol_out_file) ||
        command_die($SS_r, "Didn't receive a solution from glpsol.");
    my $success = 0;
    while (<GLPSOL_OUT>) {
        if (/^(\w+):\s+(.*)$/) {
            if ($1 eq "Status") {
                if ($2 ne "INTEGER OPTIMAL") {
                    command_die($SS_r, "glpsol returned unexpected status $2.");
                }
            }
            elsif ($1 eq "Objective") {
                if ($2 =~ /Cost\s+=\s+([^\s]+)/) {
                    $cost = $1;
                }
            }
        }
        elsif (/^\s+\d+\s+/) {
            s/^\s*//;
            my @parts = split /\s+/, $_;
            if ($parts[1] eq "Area") {
                $TDA = $parts[2];
            }
            elsif ($parts[1] =~ /^Width(\d+)$/) {
                $crit_width_results->[$1] = $parts[2];
            }
            elsif ($parts[1] =~ /^m(\d+)$/) {
                if ($parts[3] != 0 && $parts[3] != 1) {
                    print STDERR "Assertion failure: Received non-binary " .
                                 "$parts[3] from glpsol.\n";
                }
                $auto_layout_mr->{$cells[$1]} = 1-$parts[3];
                $M += $parts[3];
            }
        }
    }
    close GLPSOL_OUT;

    # Make sure we got a complete solution
    for my $i (0..$#cells) {
        if (!exists $auto_layout_mr->{$cells[$i]}) {
            command_die($SS_r, "Received incomplete solution from glpsol.");
        }
    }
    
    return ($M, $TDA, $cost, $auto_layout_mr, $crit_width_results);
}


sub write_area_contribution_debug_file {
    my $SS_r       = shift;
    my $d_r        = shift;
    my $filename   = shift;
    my $area_var   = shift;

    my $wdir = get_work_dir($SS_r);
    
    open (DEBUG, ">$wdir/$filename") || 
        command_die($SS_r, "Couldn't write to $wdir/$filename.");
    my @order = sort { ($d_r->{$b}{$area_var}*$d_r->{$b}{inst_cnt}) <=> 
                       ($d_r->{$a}{$area_var}*$d_r->{$a}{inst_cnt}) } 
                       keys %{$d_r};

    foreach my $c (@order) {
        print DEBUG "$c ";
        print DEBUG sprintf("%.5g", $d_r->{$c}{$area_var}) . " ";
        print DEBUG sprintf("%.7g", $d_r->{$c}{$area_var}*$d_r->{$c}{inst_cnt});
        foreach my $var (@_) {
            if (exists $d_r->{$c}{$var}) {
                print DEBUG " " . $d_r->{$c}{$var};
                if ($var eq "manual_cost") {
                    print DEBUG " " . $d_r->{$c}{manual_df} . "\n";
                }
            }
        }
        print DEBUG "\n";
    }
    close DEBUG;
}

sub best_match_area {
    my $SS_r                 = shift;
    my $fqcn                 = shift;
    my $inst_lr              = shift;
    my $predicted_area       = shift;
    my $ta                   = shift;
    my $inv_glc_inst_maps_lr = shift;
    my $glc_leaf_data_lr     = shift;
    my $glc_areas_lr         = shift;
    my $action;

    my $min_ta_diff = 1e9;
    my ($match_run, $match_subtype);
    foreach my $inst (@{$inst_lr}) {
        foreach my $run (0..@{$inv_glc_inst_maps_lr}-1) {
            # instance might not exist if the design changed in some way
            # since one of the earlier trials.
            if (exists $inv_glc_inst_maps_lr->[$run]{$inst}) {
                my $s = $inv_glc_inst_maps_lr->[$run]{$inst};
                if (exists $glc_areas_lr->[$run]{$s}) {
                    # there is a routed view of this subtype
                    if (abs($glc_leaf_data_lr->[$run]{$s}[1][2] - $ta) <
                                                               $min_ta_diff) {
                        $match_run = $run;
                        $match_subtype = $s;
                    }
                }
            }
        }
    }

    if (defined $match_run && defined $match_subtype) {
        # Determine if predicted DFs are close enough to call this a
        # genuine match
        my $matched_predicted_area = predict_glc_area($SS_r,
                                        @{$glc_leaf_data_lr->
                                        [$match_run]{$match_subtype}[1]}
                                        [2,3,4,5,6]);
        my $matched_predicted_df   = $matched_predicted_area / 
                                        $glc_leaf_data_lr->
                                        [$match_run]{$match_subtype}[1][2];
        my $predicted_df           = $predicted_area / $ta;

        my $MAX_ERROR              = 0.20;
        if ((abs($matched_predicted_df - $predicted_df) / 
                    $matched_predicted_df) < $MAX_ERROR) {
            my $df = $glc_areas_lr->[$match_run]{$match_subtype}[1] / 
                      $glc_leaf_data_lr->[$match_run]{$match_subtype}[1][2];
            #print "Using GLC measured DF (".sprintf("%.3g",$df).") for $fqcn.\n";
            return ($df * $ta, $match_run);
        }
        else {
            # Predicted GLC DFs were not close enough to justify using
            # the trial GLC result
            $action = -1;
        }
    }
    else {
        # Did not find a matching subtype from any of the runs. Huh.
        $action = -2;
    }
    my $df = $predicted_area / $ta;
    #print "Resorting to GLC prediction (".sprintf("%.3g",$df).") for $fqcn.\n";
    return ($predicted_area, $action);
}


#
# get_merge_space_info
#
sub get_merge_space_info {
    my $SS_r = shift;
    my $inst_excl_lr;

    # parse args
    while (num_args(\@_)) {
        my $arg = shift_next_scalar(\@_);
        #if ($arg eq "--no-floorplan") {
        #    $get_fp_area = 0;
        #}
        if ($arg eq "--exclude-instances") {
            my $eq = shift_next_scalar(\@_);
            my $inst_excl_lr = shift_next_list(\@_);
            if (!defined $eq || $eq ne "=" || !defined $inst_excl_lr) {
                command_die($SS_r, "Bad argument to $arg.");
            }
        }
        else {
            command_die($SS_r, "Unrecognized argument $arg.");
        }
    }

    print "Retrieving leaf cell info...\n";
    my $d_r = get_subtype_info($SS_r, get_work_dir($SS_r), undef, 
                               $inst_excl_lr, 0, 1, 0);

    # package all results into two return maps
    my $ss_inst_map_rv  = [ $TYPE_MAP, {} ];
    my $ss_leaf_data_rv = [ $TYPE_MAP, {} ];
    foreach my $c (keys %{$d_r}) {
        # instances map
        $ss_inst_map_rv->[1]{$c} = [ $TYPE_LIST, [] ];
        my @inst_list = @{$d_r->{$c}{inst_list}};
        $ss_inst_map_rv->[1]{$c}[1] = \@inst_list;
        # data map
        $ss_leaf_data_rv->[1]{$c} = [ $TYPE_LIST, [] ];
        my @data_list = ();
        push @data_list, $d_r->{$c}{inst_cnt};
        push @data_list, $d_r->{$c}{manual_df};
        push @data_list, $d_r->{$c}{ta};
        push @data_list, $d_r->{$c}{height};
        push @data_list, $d_r->{$c}{pcell_cnt};
        push @data_list, $d_r->{$c}{net_cnt};
        push @data_list, $d_r->{$c}{bitpitch};
        $ss_leaf_data_rv->[1]{$c}[1] = \@data_list;
    }

    set_cmd_return_variable($SS_r, "instance_map", $ss_inst_map_rv);
    set_cmd_return_variable($SS_r, "leaf_data_map", $ss_leaf_data_rv);

    print "Done.\n";
}
            
    
#
# estimate_glc_area
#
sub estimate_glc_area {
    my $SS_r = shift;

    # parse args
    my @cells;
    while (num_args(\@_)) {
        my ($type,$arg) = shift_next_arg(\@_);
        if ($type == $TYPE_SCALAR) {
            push @cells, $arg;
        }
        elsif ($type == $TYPE_LIST) {
            if (@cells) {
                command_die($SS_r, "Too many arguments to estimate_glc_area.");
            }
            @cells = @{$arg};
        }
        else {
            command_die($SS_r, "Bad argument specified.");
        }
    }

    # Retrieve necessary subtype info
    my $d_r = get_subtype_info($SS_r, get_work_dir($SS_r), \@cells, [], 
                               0, undef, 0, 0);
    
    STDOUT->format_top_name("GlcArea_Top");
    STDOUT->format_name("GlcArea_List");
    STDOUT->format_lines_per_page(10000);
    STDOUT->format_lines_left(0);
    my $glc_area;
    my $result_r = {};
    my $c;
    foreach $c (@cells) {
        $glc_area = predict_glc_area($SS_r, $d_r->{$c}{ta},
                                $d_r->{$c}{height}, $d_r->{$c}{pcell_cnt},
                                $d_r->{$c}{net_cnt}, $d_r->{$c}{bitpitch});
        write;
        $result_r->{$c} = [ $TYPE_SCALAR, $glc_area ];
    }

    set_cmd_return_variable($SS_r, "result", [$TYPE_MAP, $result_r]);

    format GlcArea_Top =
 Cell                                                   GLC Area    DF 
 ----------------------------------------------------- --------- -----
.
    format GlcArea_List =
 @<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< @>>>>>>>> @>>>>
$c,sprintf("%.4g",$glc_area),sprintf("%.2g",$glc_area/$d_r->{$c}{ta})
.
}

# Retrieves all information needed for DF optimization from a particular 
# merge space.  Returns numerous subtype -> variable map references:
#
# subtype -> 
#     inst_list -> dearrayed_inst_list (\@)
#     inst_cnt  -> inst_count,
#     manual_df -> manual_density_factor,
#     ta        -> transistor_area,       [um^2]
#     pcell_cnt -> pcell_count,
#     net_cnt   -> net_count,
#     height    -> height,                [bits]
#     stack_cnt -> stack_count,
#     fp_area   -> floorplan_area )       [um^2]   <- optional
#
# the last map is returned only if $get_fp_area is 1.  (In this case,
# $fp_instances_dir must be specified.)  
#
# If $cells_r is undefined, then all sizable subtypes under TOP will be
# queried.  In this case, instance lists and counts are adjusted to exclude 
# the instance scopes specified in the @{$inst_excl_lr} list.  (If $cells_r
# is defined, then $inst_excl_r is ignored.)
#
sub get_subtype_info {
    my $SS_r             = shift;
    my $wdir             = shift;
    my $cells_lr         = shift;   # Look up these specific cells if specified
    my $inst_excl_lr     = shift;   # exclude these instance scopes
    my $get_fp_area      = shift;
    my $fp_instances_dir = shift;
    my $verbose          = shift;
    my $auto_layout_only = shift;   # only include cells w/ auto_layout==1
    my $d_r = {};

    if (!defined $cells_lr || !@{$cells_lr}) {
        # determine sizable leaf cells
        $cells_lr = [] if (!defined $cells_lr);
        my $cmd = "--cell=$SS_r->{GS}{TOP} --task=subcells ";
        $cmd .= "--filter=leaf&!fixed";
        $cmd .= "&directive=auto_layout:1" if ($auto_layout_only);
        print "  Determining cell list\n" if ($verbose);
        query_cast_server($SS_r, $cmd, $cells_lr, 1);
    }

    # initialize data map
    foreach my $c (@{$cells_lr}) {
        $d_r->{$c} = {};
    }

    # instance map & counts of sizable leaf cells, with exclusions
    print "  Constructing instance lists\n" if ($verbose);
    query_instances($SS_r, $cells_lr, $inst_excl_lr, $d_r);

    # manual flow density factors
    print "  Looking up manual flow density factors\n" if ($verbose);
    query_manual_density_factors($SS_r, $cells_lr, $d_r);

    # transistor areas
    print "  Querying total transistor areas\n" if ($verbose);
    query_transistor_areas($SS_r, $cells_lr, $d_r);

    # pcell and net count
    print "  Querying pcell and net counts\n" if ($verbose);
    query_routing_data($SS_r, $cells_lr, $d_r);

    # stack counts
    print "  Determining stack counts\n" if ($verbose);
    query_netlist_props($SS_r, $wdir, $cells_lr, $d_r);

    # height, bitpitch, and width_minimum directives
    print "  Querying height and other directives\n" if ($verbose);
    query_directive_data($SS_r, $cells_lr, $d_r);

    # floorplan areas
    if ($get_fp_area) {
        print "  Calculating floorplan areas\n" if ($verbose);
        foreach my $c (@{$cells_lr}) {
            $d_r->{$c}{fp_area} = get_floorplan_area($SS_r, $c, 
                                                     $fp_instances_dir);
        }
    }

    return $d_r;
}

# Look up transistor areas (in um^2) of a specified list of subtypes
sub query_transistor_areas {
    my $SS_r     = shift;
    my $cells_lr = shift;
    my $d_r      = shift;

    # set up map
    foreach my $c (@{$cells_lr}) {
        $d_r->{$c}{ta} = 0.0;
    }

    # issue cast query command
    my $lines_lr = [];
    my $cmd = "--cell=$SS_r->{GS}{TOP} --task=transistors --filter=leaf";
    query_cast_server($SS_r, $cmd, $lines_lr, 1);

    # process output
    foreach my $l (@{$lines_lr}) {
        my ($c, $ta) = (split /\s+/, $l)[0,3];
        next if (!exists $d_r->{$c});
        $d_r->{$c}{ta} = $ta * 1e12;
    }
}

# Look up netlist block pcell and net counts
sub query_routing_data {
    my $SS_r     = shift;
    my $cells_lr = shift;
    my $d_r      = shift;

    # set up maps
    foreach my $c (@{$cells_lr}) {
        $d_r->{$c}{pcell_cnt} = 0;
        $d_r->{$c}{net_cnt} = 0;
    }

    # issue cast query command
    my $lines_lr = [];
    my $cmd = "--cell=$SS_r->{GS}{TOP} --task=routing --filter=leaf";
    query_cast_server($SS_r, $cmd, $lines_lr, 1);

    # process output
    foreach my $l (@{$lines_lr}) {
        my ($c, $pcs, $nets) = (split /\s+/, $l)[0..2];
        next if (!exists $d_r->{$c});
        $d_r->{$c}{pcell_cnt} = $pcs;
        $d_r->{$c}{net_cnt} = $nets;
    }
}

# Look up height, bitpitch, and width_minimum
sub query_directive_data {
    my $SS_r     = shift;
    my $cells_lr = shift;
    my $d_r      = shift;

    # issue cast query command
    my $lines_lr = [];
    my $cmd = "--cell=$SS_r->{GS}{TOP} --task=directive=height,directive=bitpitch,directive=width_minimum --filter=leaf";
    query_cast_server($SS_r, $cmd, $lines_lr, 1);

    # process output
    foreach my $l (@{$lines_lr}) {
        my ($c, $height, $pitch, $cutoff) = (split /\s+/, $l)[0..3];
        next if (!exists $d_r->{$c});
        $d_r->{$c}{height} = $height;

        # bitpitch and width_minimum are in m, convert to um
        $d_r->{$c}{bitpitch} = $pitch * 1e6;
        $d_r->{$c}{width_minimum} = $cutoff * 1e6;
    }
}

#
# Determine subtype -> instance map of a list of subcells under TOP, 
# subtracting some set of instance scopes.  Any instance name which matches 
# to some element of @{$inst_excl_lr} will be excluded.  Also returns a
# map of subtype -> instance_count, since it's easy and could be handy.
#
# Because all elements of the same array must share the same subtype (i.e.
# due to a relatively arbitrary restriction of our synthesis flow),
# the array indices in instance names are eliminated in order to save
# memory.  For example, all instances a[i].b[j].c[k] (potentially a huge
# number) are recorded as a.b.c.
#
our $inst_exclusions_lr;
our $inst_exclusions_covered_lr;
our $inst_map_mr;
our $inst_counts_mr;

sub query_instances {
    my $SS_r         = shift;
    my $cells_lr     = shift;
    my $inst_excl_lr = shift;
    my $d_r          = shift;
    
    # set up shared variables
    $inst_exclusions_lr = $inst_excl_lr;
    $inst_exclusions_covered_lr = {};
    $inst_map_mr = {};
    $inst_counts_mr = {};
    foreach my $cell (@{$cells_lr}) {
        $inst_map_mr->{$cell} = {};
        $inst_counts_mr->{$cell} = 0;
    }
    foreach my $inst (@{$inst_exclusions_lr}) {
        $inst_exclusions_covered_lr->{$inst} = 0;
    }

    # issue query command
    my $cmd = "--cell=$SS_r->{GS}{TOP} --task=instance_list";
    query_cast_server($SS_r, $cmd, undef, 1, \&query_instances_handler);

    # make sure all instance exclusions were exercised
    foreach my $inst (@{$inst_exclusions_lr}) {
        if (!$inst_exclusions_covered_lr->{$inst}) {
            print STDERR "Warning: Instance exclusion $inst wasn't ".
                         "exercised.\n";
        }
    }

    # reorganize the inst_map structure
    foreach my $c (keys %{$inst_map_mr}) {
        my @inst_list = keys %{$inst_map_mr->{$c}};
        $d_r->{$c}{inst_list} = \@inst_list;
        $d_r->{$c}{inst_cnt} = $inst_counts_mr->{$c};
    }
}

sub query_instances_handler {
    my $action = shift;
    if ($action == 0) {
        # standard output
        my $line = shift; chomp $line;
        return if ($line =~ /^\s*$/);
        my ($fqcn, $inst) = split /\s+/, $line;
        if (defined $inst_exclusions_lr) {
            foreach my $excl (@{$inst_exclusions_lr}) {
                if (substr($inst,0,length($excl)) eq $excl) {
                    $inst_exclusions_covered_lr->{$excl} = 1;
                    return;
                }
            }
        }
        return if (!exists $inst_counts_mr->{$fqcn});
        my $fl_inst = base_array_instance_name($inst);
        $inst_map_mr->{$fqcn}{$fl_inst} = 1;
        $inst_counts_mr->{$fqcn}++;
    }
    elsif ($action == 1) {
        # standard error
        my $line = shift; chomp $line;
        #print "$line\n" if ($line !~ /^SYNTAX/);
    }
}

# GLC area predictor.  TODO: Should get the model coefficients from the PDK.
sub predict_glc_area {
    my $SS_r = shift;
    my $TA = shift;     # transistor area in um^2
    my $H  = shift;     # height in bits
    my $NS = shift;     # number of subcircuits/pcells
    my $NN = shift;     # number of nets
    my $pitch = shift;  # bitpitch in um
    my $A;

    eval {
        push @INC, "$SS_r->{GS}{PDK_ROOT}/share/Fulcrum/supersize";
        require GLCPredictor;
        $A = GLCPredictor::predict_area({ 'transistor_area'  => $TA,
                                          'height'           => $H,
                                          'subcircuit_count' => $NS,
                                          'localnode_count'  => $NN,
                                          'bitpitch'         => $pitch });
        pop @INC;
    };

    my $hint = '';
    if ($@ =~ /Can't locate/) {
        $hint = ' (make sure to use a PDK after change 124008, see bug 5600)';
    }
    command_die($SS_r, "GLC area predictor failed$hint: $@") if $@;

    # Multiply by 1.1 for margin
    $A *= 1.1;

    return $A;
}

# manual flow area pedictor (density factor w/ minimum width cut-off)
sub predict_manual_area {
    my $DF = shift;     # specified density factor
    my $TA = shift;     # transistor area in um^2
    my $H  = shift;     # height
    my $pitch = shift;  # bitpitch in um
    my $cutoff = shift; # boundary width cutoff in um
    my $A;

    my $wishful_area = $TA * $DF;
    my $wishful_width = $wishful_area / ($H * $pitch);
    if ($wishful_width < $cutoff) {
        $A = $cutoff * $H * $pitch;
    }
    else {
        $A = $wishful_area;
    }
    return $A;
}

1;
