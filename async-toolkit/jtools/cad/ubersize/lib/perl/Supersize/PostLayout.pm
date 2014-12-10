#
# PostLayout
#
# Commands for post-layout maintenance / analysis
#

package Supersize::PostLayout;

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
    NAME => "PostLayout",
    DESC => "Commands for post-layout maintenance & analysis.",
    COMMANDS => {
      check_routed_directives => {
        SUBREF => \&check_routed_directives,
        USAGE  => "check_routed_directives [".underline("cell-list")."]",
        DESC   =>
          "Determines the appropriate values of the routed directives in ".
          "all cells under TOP, or in all cells instantiated by each cell ".
          "in ".underline("cell-list").".  This command doesn't actually ".
          "change any Cast subtypes; it only determines what the appropriate ".
          "directive values are.\n\n" .
          "The directive values are returned in the 'result' return ".
          "variable.  To apply these to the Cast subtypes, do the ".
          "following:\n\n".
          "  set changed_subtypes = [[keys \$check_routed_directives.result]]\n".
          "  p4_edit --cast-only \$changed_subtypes\n".
          "  copy_subtypes --src-dir=\$SPEC_DIR \$changed_subtypes\n".
          "  set_directive routed=\$check_routed_directives.result\n".
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
      }
    }
  };
}


#
# Constants for check_routed_directives
#
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
my $ERR_EXTRA_VENDOR_CELLS_IN_CAST = 5;

#
# check_routed_directives
#
sub check_routed_directives {
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
            command_die($SS_r, "Bad argument to check_routed_directives.");
        }
    }

    # Determine complete cell list
    if (!%cell_state) {
        if (!exists $SS_r->{GS}{TOP}) {
            command_die($SS_r, "TOP must be set if no cells are specified.");
        }
        $cell_state{$SS_r->{GS}{TOP}} = $OK_UNVISITED;
    }

    # Analyze all cells instantiated under the list of top-level cells.
    my %error_info;
    my %uv = ( $SS_r->{GS}{TOP} => 1 );
    while (%uv) {
        my $next = (keys %uv)[0];
        if ($cell_state{$next} == $OK_UNVISITED) {
            compare_dfII_vs_cast($SS_r, $next, \%cell_state, 
                                  \%error_info, \%uv);
        }
        else {
            delete $uv{$next};
        }
    }

    # Summarize results...
    my $routed_directives = {};
    my $num_to_set_true  = 0;
    my $num_to_set_false = 0;
    my $num_error        = 0;
    my $num_unrouted     = 0;
    print "Summary:\n";
    foreach my $fqcn (keys %cell_state) {
        if ($cell_state{$fqcn} == $ROUTED_TO_TRUE) {
            $num_to_set_true++;
            print "  $fqcn: Unexpectedly routed.\n";
            $routed_directives->{$fqcn} = [$TYPE_SCALAR, "true"];
        }
        elsif ($cell_state{$fqcn} == $ROUTED_TO_FALSE) {
            $num_to_set_false++;
            print "  $fqcn: Unexpectedly inlined.\n";
            $routed_directives->{$fqcn} = [$TYPE_SCALAR, "false"];
        }
        elsif ($cell_state{$fqcn} == $ROUTED_ERROR) {
            $num_error++;
            print "  $fqcn: Inconsistently routed!\n";
        }
        elsif ($cell_state{$fqcn} == $NOT_YET_ROUTED) {
            $num_unrouted++;
            print "  $fqcn: Not yet routed.\n";
        }
    }
    my $err_file = get_work_dir($SS_r) . "/check_routed_directives.err";
    open (ERR, ">$err_file") ||
        command_die($SS_r, "Couldn't write to $err_file.");
    foreach my $fqcn (keys %error_info) {
        print ERR "In $fqcn:\n";
        foreach my $err_lr (@{$error_info{$fqcn}}) {
            if ($err_lr->[0] == $ERR_INST_TYPE) {
                print ERR "  Mismatch between DFII & Cast types for instance ".
                      $err_lr->[1] . "\n";
                print ERR "    Cast: $err_lr->[2], DFII: $err_lr->[3]\n";
            }
            elsif ($err_lr->[0] == $ERR_UNROUTED_SUBCELL) {
                print ERR "  Subcell unexpectedly routed in DFII:\n" .
                          "    $err_lr->[2] ($err_lr->[1])\n";
            }
            elsif ($err_lr->[0] == $ERR_ROUTED_SUBCELL) {
                print ERR "  Subcell unexpectedly inlined in DFII:\n" .
                          "    $err_lr->[2] ($err_lr->[1])\n";
            }
            elsif ($err_lr->[0] == $ERR_EXTRA_IN_CAST) {
                print ERR "  Subcell missing in DFII:\n" .
                          "    $err_lr->[2] ($err_lr->[1])\n";
            }
            elsif ($err_lr->[0] == $ERR_EXTRA_IN_DFII) {
                print ERR "  Subcell missing in Cast:\n" .
                          "    $err_lr->[2] ($err_lr->[1])\n";
            }
            elsif ($err_lr->[0] == $ERR_EXTRA_VENDOR_CELLS_IN_CAST) {
                print ERR "  Missing vendor cells in DFII: " .
                          $err_lr->[1] . " total\n";
            }
            else {
                print ERR "    Unknown error code $err_lr->[0].\n";
            }
        }
    }
    close ERR;
    print "\n";
    print "Number of cells requiring routed=true:       $num_to_set_true\n";
    print "Number of cells requiring routed=false:      $num_to_set_false\n";
    print "Number of cells with inconsistent routing:   $num_error\n";
    print "Number of cells not yet routed:              $num_unrouted\n\n";

    # Result
    set_cmd_return_variable($SS_r, "result", [$TYPE_MAP, $routed_directives]);
}

# Finds the next cell with state OK_UNVISITED (or undef if none).
sub find_next_unvisited {
    my $cs_r = shift;
    foreach my $c (keys %{$cs_r}) {
        return $c if ($cs_r->{$c} == 0);
    }
    return undef;
}

# For the cell fqcn, compares dfII subcell instances to cast subcell 
# instances.  Updates cell state map cs_r, error info map ei_r, and
# unvisited map uv_r.
sub compare_dfII_vs_cast {
    my $SS_r = shift;
    my $fqcn = shift;
    my $cs_r = shift;
    my $ei_r = shift;
    my $uv_r = shift;

    print "Examining $fqcn.\n" if ($SS_r->{GS}{VERBOSE});
    my $cast_insts_r = get_cast_subcells($SS_r, $fqcn);
    my $dfII_insts_r = get_dfII_subcells($SS_r, $fqcn, "layout");
    my $cast_marks_r = {};

    # Assume cell isn't routed [yet] if layout view didn't exist
    my $missing_dfII = 0;
    if (!defined $dfII_insts_r) {
        $cs_r->{$fqcn} = $NOT_YET_ROUTED;
        $missing_dfII  = 1;
        $dfII_insts_r = {};
    }
    else {
        $cs_r->{$fqcn} = $OK_VISITED;
    }
    delete $uv_r->{$fqcn};
    
    # Compare layout instances to cast
    my $num_missing_from_cast = 0;
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
                $uv_r->{$subtype} = 1;
            }
            elsif ($cs_r->{$subtype} == $ROUTED_TO_FALSE) {
                # Previously, found that it was improperly routed 
                # in layout; here it's properly routed though.
                $cs_r->{$subtype} = $ROUTED_ERROR;
            }
        }
        else {
            # Attempt to ignore wiring cells
            next if ($dfII_insts_r->{$li} =~ /\.wires\./);

            # Instance does not exist in cast
            my @inlined_cast_instances = 
                look_for_inlined_cast_subcells($li, $cast_insts_r);
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
                    # consistent with the cast.  NOTE: This case should
                    # never happen, unless the cell has routed=true, was
                    # inlined in the cast, but not inlined in layout.
                    # (But maybe it could come up due to ECOs...)
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
                    # The layout instance is not inlined (unrouted) in the cast
                    my $subtype = $cast_insts_r->{$inlined_ci};
                    if (!exists $cs_r->{$subtype}) {
                        $cs_r->{$subtype} = $ROUTED_TO_FALSE;
                    }
                    elsif ($cs_r->{$subtype} == $OK_UNVISITED ||
                           $cs_r->{$subtype} == $OK_VISITED) {
                        # Was previously encountered as a non-inlined layout
                        # instance.
                        $cs_r->{$subtype} = $ROUTED_ERROR;
                    }
                    if (!exists $cast_marks_r->{$inlined_ci}) {
                        $ei_r->{$fqcn} = [] if (!exists $ei_r->{$fqcn});
                        push @{$ei_r->{$fqcn}}, [ $ERR_ROUTED_SUBCELL,
                            $inlined_ci, $subtype ];
                    }
                    $cast_marks_r->{$inlined_ci} = 1;
                }
                else {
                    # Mysteriously missing from the cast
                    $ei_r->{$fqcn} = [] if (!exists $ei_r->{$fqcn});
                    push @{$ei_r->{$fqcn}}, [ $ERR_EXTRA_IN_DFII, $li,
                        $dfII_insts_r->{$li} ];
                    $num_missing_from_cast++;
                    if ($num_missing_from_cast > 100) {
                        print "Warning: Large number (>100) of dfII instances ".
                              "missing from cast in\n" .
                              "         $fqcn\n";
                        print "         Skipping further analysis.\n";
                        last;
                    }
                }
            }
        }
    }

    # Examine leftover cast instances
    my $num_missing_vendor_cells = 0;
    foreach my $ci (keys %{$cast_insts_r}) {
        if (!exists $cast_marks_r->{$ci}) {
            if (!$missing_dfII) {
                if ($cast_insts_r->{$ci} =~ /^vendor\./) {
                    # vendor cells typically have problems...
                    $num_missing_vendor_cells++;
                }
                elsif ($cast_insts_r->{$ci} =~ /\.\d[^\.]*$/) {
                    # Appears to be a well-formed layout subtype; instance
                    # is missing in layout.
                    $ei_r->{$fqcn} = [] if (!exists $ei_r->{$fqcn});
                    push @{$ei_r->{$fqcn}}, 
                        [ $ERR_EXTRA_IN_CAST, $ci, $cast_insts_r->{$ci} ];
                }
                # (otherwise presumably a zero-prs cell)
            }
            else {
                # there was no layout view, so add all subcells as unvisited.
                my $subtype = $cast_insts_r->{$ci};
                if (!exists $cs_r->{$subtype}) {
                    $cs_r->{$subtype} = $OK_UNVISITED;
                    $uv_r->{$subtype} = 1;
                }
            }
        }
    }
    if ($num_missing_vendor_cells > 0) {
        push @{$ei_r->{$fqcn}}, [ $ERR_EXTRA_VENDOR_CELLS_IN_CAST, 
                                  $num_missing_vendor_cells ];
    }
}

# Queries (routed hierarchy) subcells of fqcn
sub get_cast_subcells {
    my $SS_r = shift;
    my $fqcn = shift;

    # Run query
    #my $cmd = "com.avlsi.tools.jflat.JFlat ";
    #$cmd .= "--cast-path=$SS_r->{GS}{CAST_DIR}:$SS_r->{GS}{SPEC_DIR} ";
    #$cmd .= "--routed ";
    #$cmd .= "--tool=query ";
    #$cmd .= "--query-tasks=instance_list ";
    #$cmd .= "--query-no-recurse ";
    #$cmd .= "--cell=$fqcn";
    #run_cast_server_command($SS_r, $cmd, undef, undef, 0, $lines);
    my $cmd = "--task=instance_list --routed --no-recurse --cell=$fqcn";
    my $lines = [];
    query_cast_server($SS_r, $cmd, $lines, 1);

    my $inst_r = {};
    foreach my $l (@{$lines}) {
        chomp $l;
        my ($type, $inst) = split /\s+/, $l;
        if (!defined $type || !defined $inst) {
            command_die($SS_r, "Unexpected syntax in instance_list query " .
                               "of $fqcn.");
        }
        $inst_r->{$inst} = $type;
    }
    return $inst_r;
}

# Given layout instance name "li", looks for cast instance names of the
# form "li.foo", "li.bar", etc.  Returns all such instance names in a list.
# Note: Iterates through the entire instance set, so the performance of the
# comparison algorithm will become extremely bad for cells with large numbers
# of mismatched instance names. (scales as N^2, N=#instances)
sub look_for_inlined_cast_subcells {
    my $li   = shift;
    my $ci_r = shift;

    my @inlined_subcells;
    foreach my $ci (keys %{$ci_r}) {
        push @inlined_subcells, $ci if (length($ci) > length($li) &&
                                        substr($ci,0,length($li)+1) eq "$li.");
    }
    return @inlined_subcells;
}

# Given layout instance name "a.b.c", looks for a cast instance name which
# belongs to a parent cell which was unexpectedly inlined in the layout (e.g.
# "a" or "a.b".)
sub look_for_inlined_instance {
    my $li   = shift;
    my $ci_r = shift;

    my $ci;
    foreach my $c (split /\./, $li) {
        if (!defined $ci) { $ci = $c; }
        else { $ci .= ".$c"; }
        return $ci if (exists $ci_r->{$ci});
    }
    return undef;
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
