#
# SizingDrop
#
# Commands to ease the pain
#

package Supersize::SizingDrop;

BEGIN {
    use Exporter;
    our @ISA = qw(Exporter);
    our @EXPORT_OK = qw( 
        &get_module_data
    );
}

use strict;
use Supersize::LayoutIntegration;
use Supersize::Util;
use Supersize::TypeUtil;
use Supersize::JavaUtil;
use Term::ANSIColor;

sub get_module_data {
  return { 
    NAME => "SizingDrop",
    DESC => "Commands to ease the pain.",
    COMMANDS => {
      set_routed_directives => {
        SUBREF => \&set_routed_directives,
        USAGE  => "set_routed_directives [".underline("cell-list")."]",
        DESC   =>
          "Determines the appropriate values of the routed directives in ".
          "all cells under TOP, or in all cells instantiated by each cell ".
          "in ".underline("cell-list").".  This command doesn't actually ".
          "change any Cast subtypes; it only determines what the appropriate ".
          "directive values are, according to the following rules:\n\n".
          "  Set FALSE if current directive is TRUE, a prelayout view\n".
          "      exists but a layout view does not exist.\n".
          "  Set TRUE if current directive is FALSE and a layout view\n".
          "      exists.\n\n".
          "The directive values are returned in the 'result' return ".
          "variable.  To apply these to the Cast subtypes, do the ".
          "following:\n\n".
          "  set changed_subtypes = [[keys \$set_routed_directives.result]]\n".
          "  p4_edit --cast-only \$changed_subtypes\n".
          "  copy_subtypes --src-dir=\$SPEC_DIR \$changed_subtypes\n".
          "  set_directive routed=\$set_routed_directives.result\n".
          "  copy_subtypes --dest-dir=\$SPEC_DIR \$changed_subtypes\n\n".
          "(and then do a p4 submit).",
        RV => {
          result => {
            TYPE => $TYPE_MAP,
            DESC => "Map of subtype -> routed_directive_value." } },
        GLOBALS => {
          WORK_DIR => { REQUIRED => 1 },
          CAST_DIR => { REQUIRED => 1 },
          DFII_DIR => { REQUIRED => 1 },
          SPEC_DIR => { REQUIRED => 1 } } 
      },
      report => {
        SUBREF => \&report,
        USAGE  => "report [--format=html|text] [".underline("topic")."]",
        DESC   =>
          "Looks up information about the sizing drop and presents it in ".
          "a specified format (text or html).  With no arguments, various ".
          "simple (scalar) properties of the drop are identified and set ".
          "as return variables.  Supported topics:\n\n".
          "  jauto_warnings - Generates a table summarizing the Jauto\n".
          "     warnings in the final sizing run.",
        RV => {
          average_transistor_width => {
            DESC => "Average transistor width." },
          total_transistor_count => {
            DESC => "Total transistor count in the entire drop." } },
        GLOBALS => {
          WORK_DIR => { REQUIRED => 1 },
          CAST_DIR => { REQUIRED => 1 },
          SPEC_DIR => { REQUIRED => 1 } } 
      }
    }
  };
}


#
# set_routed_directives
#
sub set_routed_directives_old {
    my $SS_r = shift;
    my $top_cells_lr;

    # parse args
    while (num_args(\@_)) {
        my ($type, $arg) = shift_next_arg(\@_);
        if ($type == $TYPE_SCALAR) {
            command_die($SS_r, "Unrecognized argument $arg.");
        }
        elsif ($type == $TYPE_LIST) {
            $top_cells_lr = $arg;
        }
        else {
            command_die($SS_r, "Bad argument to set_routed_directives.");
        }
    }

    # Determine complete cell list
    my %cells;
    my %routed;
    if (!defined $top_cells_lr) {
        if (!exists $SS_r->{GS}{TOP}) {
            command_die($SS_r, "TOP must be set if no cells are specified.");
        }
        $top_cells_lr = [$SS_r->{GS}{TOP}];
    }
    foreach my $t (@{$top_cells_lr}) {
        my $query_cmd = "subcells";
        my @result;
        my $cmd = "--cell=$t --task=subcells";
        query_cast_server($SS_r, $cmd, \@result, 1);
        foreach my $c (@result) {
            $cells{$c} = 1;
        }
        @result = ();
        $cmd = "--cell=$t --task=subcells --filter=routed";
        query_cast_server($SS_r, $cmd, \@result, 1);
        foreach my $c (@result) {
            $routed{$c} = 1;
        }
    }

    # Inspect each cell
    my $routed_directives = {};
    my $num_to_true = 0;
    my $num_to_false = 0;
    foreach my $c (keys %cells) {
        my $cadence_fqcn = to_cadence($c);
        my $prelayout_exists = 
            -e get_cadence_cell_view_dir($SS_r->{GS}{DFII_DIR}, 
                                         $cadence_fqcn, "prelayout");
        my $layout_exists    = 
            -e get_cadence_cell_view_dir($SS_r->{GS}{DFII_DIR}, 
                                         $cadence_fqcn, "layout");

        if (exists $routed{$c}) {
            if ($prelayout_exists && !$layout_exists) {
                $routed_directives->{$c} = [$TYPE_SCALAR, "false"];
                $num_to_false++;
            }
        }
        else {
            if ($layout_exists) {
                $routed_directives->{$c} = [$TYPE_SCALAR, "true"];
                $num_to_true++;
            }
        }
    }

    print "Identified $num_to_false subtypes that should have routed=false,\n";
    print "           $num_to_true that should have routed=true.\n";

    # Result
    set_cmd_return_variable($SS_r, "result", [$TYPE_MAP, $routed_directives]);
}

#
# set_routed_directives
#
sub set_routed_directives {
    my $SS_r = shift;
    my %cell_state;

    # parse args
    while (num_args(\@_)) {
        my ($type, $arg) = shift_next_arg(\@_);
        if ($type == $TYPE_SCALAR) {
            command_die($SS_r, "Unrecognized argument $arg.");
        }
        elsif ($type == $TYPE_LIST) {
            foreach my $c (@{$arg}) {
                $cell_state{$c} = 0;
            }
        }
        else {
            command_die($SS_r, "Bad argument to set_routed_directives.");
        }
    }

    # Determine complete cell list
    if (!%cell_state) {
        if (!exists $SS_r->{GS}{TOP}) {
            command_die($SS_r, "TOP must be set if no cells are specified.");
        }
        $cell_state{$SS_r->{GS}{TOP}} = 0;
    }

    # Analyze all cells instantiated under the list of top-level cells.
    my %error_info;
    my $next;
    while (defined ($next = find_next_unvisited(\%cell_state))) {
        compare_layout_vs_cast($SS_r, $next, \%cell_state, \%error_info);
    }

    # Summarize results...
    my $routed_directives = {};

    # Result
    set_cmd_return_variable($SS_r, "result", [$TYPE_MAP, $routed_directives]);
}

sub find_next_unvisited {
    my $cs_r = shift;
    foreach my $c (keys %{$cs_r}) {
        return $c if ($cs_r->{$c} == 0);
    }
    return undef;
}

# cell states
my $OK_UNVISITED    = 0;
my $OK_VISITED      = 1;
my $ROUTED_TO_TRUE  = 2;
my $ROUTED_TO_FALSE = 3;
my $ROUTED_ERROR    = 4;    # inconsistent routed, non-routed in layout
my $NOT_YET_ROUTED  = 5;

# error codes
my $ERR_INST_TYPE        = 0;  # Cast-vs-layout subcell type discrepancy
my $ERR_EXTRA_IN_CAST    = 1;  # Extra instance in cast hierarchy
my $ERR_UNROUTED_SUBCELL = 2;  # corresponds to ROUTED_TO_TRUE
my $ERR_ROUTED_SUBCELL   = 3;  # corresponds to ROUTED_TO_FALSE
my $ERR_EXTRA_IN_DFII    = 4;  # Extra instance in layout hierarchy

sub compare_layout_vs_cast {
    my $SS_r = shift;
    my $fqcn = shift;
    my $cs_r = shift;
    my $ei_r = shift;

    print "Examining $fqcn.\n" if ($SS_r->{GS}{VERBOSE});
    my $cast_insts_r = get_cast_subcells($SS_r, $fqcn);
    my $dfII_insts_r = get_dfII_subcells($SS_r, $fqcn, "layout");
    my $cast_marks_r = {};

    # Assume cell isn't routed [yet] if layout view didn't exist
    if (!%{$dfII_insts_r}) {
        $cs_r->{$fqcn} = $NOT_YET_ROUTED;
    }
    
    # Compare layout instances to cast
    foreach my $li (keys %{$dfII_insts_r}) {
        if (exists $cast_insts_r->{$li}) {
            # Instance exists in both layout and cast -- as it should
            $cast_marks_r->{$li} = 1;
            if ($cast_insts_r->{$li} ne $dfII_insts_r->{$li}) {
                # Cell type discrepancy encountered
                $ei_r->{$fqcn} = [] if (!exists $ei_r->{$fqcn});
                push @{$ei_r->{$fqcn}}, [ $ERR_INST_TYPE, $li,
                    $cast_insts_r->{$li}, $dfII_insts_r->{$li} ];
            }
            my $subtype = $cast_insts_r->{$li};
            if (!exists $cs_r->{$subtype}) {
                $cs_r->{$subtype} = $OK_UNVISITED;
            }
            elsif ($cs_r->{$subtype} == $ROUTED_TO_TRUE) {
                    # Previously, found that it was unrouted in layout; here
                    # it's routed though.
                    $cs_r->{$subtype} = $ROUTED_ERROR;
            }
        }
        else {
            #  Instance does not exist in cast
            my @inlined_cast_instances = 
                look_for_inlined_subcells($li, $cast_insts_r);
            if (@inlined_cast_instances) {
                # The layout instance is an inlined subcell in the cast,
                # i.e. routed incorrectly set to false
                my $subtype = $dfII_insts_r->{$li};
                if (!exists $cs_r->{$subtype}) {
                    $cs_r->{$subtype} = $ROUTED_TO_TRUE;
                }
                elsif ($cs_r->{$subtype} == $OK_UNVISITED ||
                       $cs_r->{$subtype} == $OK_VISITED) {
                    # Previously found to be unrouted in the layout, i.e.
                    # consistent with the cast.
                    $cs_r->{$subtype} = $ROUTED_ERROR;
                }
                $ei_r->{$fqcn} = [] if (!exists $ei_r->{$fqcn});
                push @{$ei_r->{$fqcn}}, [ $ERR_UNROUTED_SUBCELL,
                    $li, $subtype ];
                # Mark all of the cast's inlined subcells so we don't 
                # report them to be missing from the layout
                foreach my $ci (@inlined_cast_instances) {
                    $cast_marks_r->{$ci} = 1;
                }
            }
            else {
                my $inlined_ci = look_for_inlined_instance($li, $cast_insts_r);
                if (defined $inlined_ci) {
                    # The layout instance is not inlined (routed) in the cast
                    my $subtype = $cast_insts_r->{$inlined_ci};
                    if (!exists $cs_r->{$subtype}) {
                        $cs_r->{$subtype} = $ROUTED_TO_FALSE;
                    }
                    $ei_r->{$fqcn} = [] if (!exists $ei_r->{$fqcn});
                    push @{$ei_r->{$fqcn}}, [ $ERR_ROUTED_SUBCELL,
                        $inlined_ci, $subtype ];
                    $cast_marks_r->{$inlined_ci} = 1;
                }
                else {
                    # Mysteriously missing from the cast
                    $ei_r->{$fqcn} = [] if (!exists $ei_r->{$fqcn});
                    push @{$ei_r->{$fqcn}}, [ $ERR_EXTRA_IN_DFII, $li,
                        $dfII_insts_r->{$li} ];
                }
            }
        }
    }

    # Examine leftover cast instances
    foreach my $ci (keys %{$cast_insts_r}) {
        if (!exists $cast_marks_r->{$ci}) {
            if (%{$dfII_insts_r}) {
                # Instance missing in layout
                $ei_r->{$fqcn} = [] if (!exists $ei_r->{$fqcn});
                push @{$ei_r->{$fqcn}}, 
                    [ $ERR_EXTRA_IN_CAST, $ci, $cast_insts_r->{$ci} ];
            }
            else {
                # there was no layout view, so add all subcells as unvisited.
                my $subtype = $cast_insts_r->{$ci};
                if (!exists $cs_r->{$subtype}) {
                    $cs_r->{$subtype} = $OK_UNVISITED;
                }
            }
        }
    }
}


#
# report
#
sub report {
    my $SS_r = shift;

    my $format = "text";

    # parse args
    while (num_args(\@_)) {
        my $arg = shift_next_scalar(\@_);
        if (!defined $arg) {
            command_die($SS_r, "Unexpected non-scalar argument specified.");
        }
        elsif ($arg eq "--format") {
            my $eq = shift_next_scalar(\@_);
            $format = shift_next_scalar(\@_);
            if (!defined $eq || $eq ne "=" || !defined $format) {
                command_die($SS_r, "Malformed argument $arg.");
            }
            if ($format ne "text" && $format ne "html") {
                command_die($SS_r, "Unsupported format $format specified.");
            }
        }
        else {
            command_die($SS_r, "Unrecignozed argument $arg.");
        }
    }

    command_die($SS_r, "Sorry, report isn't implemented.");
}

1;
