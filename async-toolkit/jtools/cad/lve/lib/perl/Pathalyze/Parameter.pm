# $Id: $
# $DateTime: $

package Pathalyze::Parameter;

#
# NOTE: Naming convention change in progress!  Old name of "parameter"
#       has been deprecated in favor of "property".  Code still calls
#       these parameters.
#

use strict;
use Util::Numeric;

BEGIN {
    use Exporter;
    our @ISA = qw(Exporter);
    our @EXPORT = qw(
        &set_prop
        &evaluate_cell_properties
        &constraint_prop_expr
        &delay_prop_base_name
    );
}

############################ Property Caculation ############################

#
# Set parameter corresponding to the path data (min/max delay & slew)
# d_r is a structure of the following form:
# 
#   INDEPENDENT_VARS -> [ <ind_var_list> ]
#   RESULT -> ivar1 -> ivar2 -> .. -> min_delay -> <max_delay>
#                                     max_delay -> <min_delay>
#                                     min_slew  -> <min_slew>
#                                     max_slew  -> <max_slew>
#                                     delay     -> (<delaylist>)
#                                     slew      -> (<slewlist>)
#
# (e.g. as returned by measure_path_delay)  <ind_var_list> should be a 
# list of strings of the form "slew(<transition>)" or "cap(<node>)".
# Not all delay/slew keys need to be present.
#
sub set_prop {
    my $params_r   = shift;
    my $param      = shift;
    my $d_r        = shift;
    my $accumulate = shift;

    $accumulate = 0 if (!defined $accumulate);

    if (!exists $params_r->{$param}) {
        $params_r->{$param} = {
            TYPE => -1
        };
    }
    my $param_r = $params_r->{$param};
    $param_r->{RESULT} = {} if (!exists $param_r->{RESULT});

    die "Assertion: set_prop (1).\n" if (!exists $d_r->{INDEPENDENT_VARS});
    die "Assertion: set_prop (2).\n" if (!exists $d_r->{RESULT});

    if (!exists $param_r->{INDEPENDENT_VARS} || !$accumulate) {
        $param_r->{INDEPENDENT_VARS} = [];
    }
    else {
        my $match = 
            @{$d_r->{INDEPENDENT_VARS}}==@{$param_r->{INDEPENDENT_VARS}};
        if ($match) {
            foreach my $i (0..$#{$d_r->{INDEPENDENT_VARS}}) {
                (my $dv = $d_r->{INDEPENDENT_VARS}[$i]) =~ s/\(.*$//;
                (my $pv = $param_r->{INDEPENDENT_VARS}[$i]) =~ s/\(.*$//;
                $match = 0 if ($dv ne $pv);
            }
        }
        if (!$match) {
            print "@{$d_r->{INDEPENDENT_VARS}}==@{$param_r->{INDEPENDENT_VARS}}\n";
        }
        die "Assertion: set_prop (3).\n" if (!$match);
    }
    @{$param_r->{INDEPENDENT_VARS}} = @{$d_r->{INDEPENDENT_VARS}};
    recursive_set_param_result($d_r->{INDEPENDENT_VARS}, 0, $d_r->{RESULT},
                               $param_r->{RESULT}, $accumulate);
}

sub recursive_set_param_result {
    my $ivar_lr       = shift;
    my $ivar_idx      = shift;
    my $from_result_r = shift;
    my $to_result_r   = shift;
    my $accumulate    = shift;

    if ($ivar_idx < @{$ivar_lr}) {
        $ivar_idx++;
        foreach my $iv (keys %{$from_result_r}) {
            if (@{$ivar_lr}) {
                $to_result_r->{$iv} = {} if (!exists $to_result_r->{$iv});
                recursive_set_param_result($ivar_lr, $ivar_idx,
                                           $from_result_r->{$iv},
                                           $to_result_r->{$iv},
                                           $accumulate);
            }
        }
    }
    else {
        # end of the line
        if (!%{$to_result_r} || !$accumulate) {
            $to_result_r->{delay} = [];
            $to_result_r->{slew} = [];
        }
        foreach my $k ("delay", "slew") {
            foreach my $dk ($k, "min_${k}", "max_${k}") {
                if (exists $from_result_r->{$dk}) {
                    if ($dk eq "delay" || $dk eq "slew") {
                        push @{$to_result_r->{$k}}, @{$from_result_r->{$dk}};
                    }
                    else {
                        push @{$to_result_r->{$k}}, $from_result_r->{$dk};
                    }
                }
            }
        }
    }
}


#
# Evaluates all parameters in a given cell.  Records values in
# OUTPUT_DIR/<cell>/param_results (if OUTPUT_DIR is defined).  Results are 
# also set within the PARAM structure for later use:
#
#   PARAM -> <param_name> -> 
#       INDEPENDENT_VARS -> [ ivar1, ivar2, ... ]
#       RESULT -> <ival1> -> <ival2> -> ... ->
#                                   delay -> [ delay1, delay2, ... ]
#                                   slew  -> [ slew1, slew2, ... ]
#
# Note: 'slew' result values are not meaningful for paths of TYPE==1,2,
#       and possibly -1.
#
sub evaluate_cell_properties {
    my $V_r     = shift;
    my $cell_r  = shift;
    my $cell    = shift;

    print "Cell $cell:\n" if ($V_r->{VERBOSE});

    my $result_file;
    my $results_fh;
    if (defined $V_r->{OUTPUT_DIR}) {
        $result_file = "$V_r->{OUTPUT_DIR}/$cell/properties";
        `mkdir -p '$V_r->{OUTPUT_DIR}/$cell'`;
        open ($results_fh, ">$result_file") ||
            die "Couldn't write to $result_file.\n";
    }

    foreach my $param (@{$cell_r->{PARAM_ORDER}}) {
        eval {
            my $d_r = evaluate_param($V_r, $cell_r->{PARAM}, $param);
            set_prop($cell_r->{PARAM}, $param, $d_r);
        };
        if ($@) {
            (my $err_str = $@) =~ s/^/    /m;
            print "  Couldn't evaluate " . param_type($cell_r->{PARAM}{$param}).
                  " property $param:\n$err_str" if ($V_r->{VERBOSE});
        }
        else {
            print "  Evaluated " . param_type($cell_r->{PARAM}{$param}) . 
                  " property $param.\n" if ($V_r->{VERBOSE});
            if (defined $result_file) {
                print_param($results_fh, $cell_r->{PARAM}{$param}, $param);
            }
        }
    }

    $results_fh->close() if (defined $result_file);
}


sub print_param {
    my $fh      = shift;
    my $param_r = shift;
    my $param   = shift;

    my ($data_slew, $ref_slew, $cap, $delay, $slew);
    $fh->print(ucfirst(param_type($param_r)) . " property " . $param . ":\n");
    if ($param_r->{TYPE}==1 || $param_r->{TYPE}==2) {
        $fh->format_top_name("ConstraintFormat_Top");
        $fh->format_name("ConstraintFormat");
        $fh->format_lines_per_page(10000);
        $fh->format_lines_left(0);
        $fh->print("  Data input:      " . $param_r->{IO} . "\n");
        $fh->print("  Reference input: " . $param_r->{REF} . "\n\n");
        if (@{$param_r->{INDEPENDENT_VARS}} != 2) {
            die "Assertion: print_param (1).\n";
        }
        my $result_r = $param_r->{RESULT};
        foreach my $slew1 (sort {$a<=>$b} keys %{$result_r}) {
            foreach my $slew2 (sort {$a<=>$b} keys %{$result_r->{$slew1}}) {
                $data_slew = $param_r->{TYPE}==1 ? $slew1 : $slew2;
                $ref_slew  = $param_r->{TYPE}==1 ? $slew2 : $slew1;
                $delay = $result_r->{$slew1}{$slew2}{delay}[0];
                write $fh;
            }
        }
    }
    elsif ($param_r->{TYPE}==0) {
        $fh->format_top_name("DelayFormat_Top");
        $fh->format_name("DelayFormat");
        $fh->format_lines_per_page(10000);
        $fh->format_lines_left(0);
        $fh->print("  Input:  " . $param_r->{REF} . "\n");
        $fh->print("  Output: " . $param_r->{IO} . "\n\n");
        my $result_r = $param_r->{RESULT};
        foreach $ref_slew (sort {$a<=>$b} keys %{$result_r}) {
            if (@{$param_r->{INDEPENDENT_VARS}}==1) {
                $cap = "-";
                $delay = $result_r->{$ref_slew}{delay}[0];
                $slew  = $result_r->{$ref_slew}{slew}[0];
                write $fh;
            }
            elsif (@{$param_r->{INDEPENDENT_VARS}}==2) {
                foreach $cap (sort {$a<=>$b} keys %{$result_r->{$ref_slew}}) {
                    $delay = $result_r->{$ref_slew}{$cap}{delay}[0];
                    $slew  = $result_r->{$ref_slew}{$cap}{slew}[0];
                    write $fh;
                }
            }
            else {
                die "Assertion: print_param (2).\n";
            }
        }
    }
    else {
        my $result_r = $param_r->{RESULT};
        my $pt = "";
        foreach my $i (0..$#{$param_r->{INDEPENDENT_VARS}}) {
            my @keys = keys %{$result_r};
            die "Assertion: print_param (2).\n" if (!@keys);
            $result_r = $result_r->{$keys[0]};
            $pt .= $keys[0];
            $pt .= ", " if ($i < $#{$param_r->{INDEPENDENT_VARS}});
        }
        $fh->print("  @($pt) delay = $result_r->{delay}[0], " .
                   "slew=$result_r->{slew}[0]\n");
    }
    my $str = ""; for (0..59) { $str .= "-"; }
    $fh->print($str . "\n");

    format ConstraintFormat_Top =
   Data Slew       Ref Slew      Constraint
 -------------   -------------  -------------
.
    format ConstraintFormat =
    @<<<<<<<<       @<<<<<<<<    @<<<<<<<<<<
$data_slew,$ref_slew,round_float($delay,-5)
.

    format DelayFormat_Top =
   Input Slew       Output Cap       Delay      Output Slew
 -------------   ---------------  -----------  -------------
.
    format DelayFormat =
    @<<<<<<<<      @<<<<<<<<<<<    @<<<<<<<<<   @<<<<<<<<<
$ref_slew,$cap,round_float($delay,-5),round_float($slew,-5)
.
}


# Returns the specified parameter's type description
sub param_type {
    my $param_r = shift;
    return $param_r->{TYPE}==-1 ?  "generic" : 
          ($param_r->{TYPE}==0  ?  "delay" :
          ($param_r->{TYPE}==1  ?  "setup" :
          ($param_r->{TYPE}==2  ?  "hold" :
                                   "unknown" )));
}


#
# Evaluates the min/max delay/slews of the specified parameter in the
# PARAM structure at all input slew rates (IN_SLEWS) and output
# capacitances (OUT_CAPS).  Other parameter values referenced in the
# expression are interpolated if necessary.  If a referenced parameter is
# not swept over output capacitance, the <internal> values are used.
#
sub evaluate_param {
    my $V_r      = shift;
    my $params_r = shift;
    my $param    = shift;

    my $type = $params_r->{$param}{TYPE};
    if ($type==-1 || $type==0) {
        return evaluate_simple_param($V_r, $params_r, $param);
    }
    elsif ($type==1 || $type==2) {
        return evaluate_constraint_param($V_r, $params_r, $param);
    }
}


#
# Uses textual substitution of param-matching references in the parameter
# expression to evaluate the parameter's delay/slew.  A single sweep over
# IN_SLEWS is done (inaccurate if the expression involves paths with
# different input transitions) and a single sweep over OUT_CAPS is attempted
# (again inaccurate if paths w/ different output nodes are referenced).
#
# Return structure is of the form
#   INDEPENDENT_PARAMS -> [ <ivarlist> ]
#   RESULT -> <slew> -> <cap> -> delay -> [ <delay> ]
#                                slew  -> [ <output_slew> ]
#
# (where the <cap> dimension may not exist if all referenced parameters
# in this parameter's expression are not cap-swept.)
#
sub evaluate_simple_param {
    my $V_r      = shift;
    my $params_r = shift;
    my $param    = shift;

    my $type = $params_r->{$param}{TYPE};
    my $d_r = { 
        INDEPENDENT_VARS => [ 
            ($type==0 ? $params_r->{$param}{REF} : "slew")
        ],
        RESULT => {} 
    };

    #
    # Tricky part: Escape all path/param-type names in the expression
    # with a function call to evaluate the particular (slew, cap) point
    # in the subsequent eval.
    #
    my $eval_expr = $params_r->{$param}{EXPR};
    # first protect reserved function names with # prefix
    foreach my $fname ("min", "max", "val") {
        $eval_expr =~ s/([^\w.;:\[\]]?)$fname(\s*\()/$1#$fname$2/g;
    }
    print "Evaluation expression 1: $eval_expr\n" if ($V_r->{DEBUG});

    # replace argument of min_slew/max_slew
    $eval_expr =~ s/([^\w.;:\[\]]?)(max|min)_slew\s*\(\s*([a-zA-Z_;][\w.;:\[\]]*)\s*\)/$1#$2(#lookup_param(\$params_r,\"#$3\",\"slew\",\@point))/g;
    print "Evaluation expression 2: $eval_expr\n" if ($V_r->{DEBUG});

    # replace vars with call to lookup_param()
    $eval_expr =~ s/([,\(\s])([a-zA-Z_;][\w.;:\[\]]*)/$1#lookup_param(\$params_r,\"#$2\",\$key,\@point)/g;
    print "Evaluation expression 3: $eval_expr\n" if ($V_r->{DEBUG});

    # restore protected names
    $eval_expr =~ s/#//g;
    print "Evaluation expression 4: $eval_expr\n" if ($V_r->{DEBUG});

    local $SIG{'__WARN__'}= 'die_on_warning';

    my $external = 1;
    foreach my $slew (@{$V_r->{IN_SLEWS}}) {
        $d_r->{RESULT}{$slew} = {};
        # first try w/out cap sweep
        my @point = ($slew);
        foreach my $key ("delay", "slew") {
            my $value = eval $eval_expr;
            if ($@ && !@{$V_r->{OUT_CAPS}}) {
                # this should not have failed
                die $@;
                #die "Error evaluating expression $params_r->{$param}{EXPR}:\n".
                #    "  $@\n";
            }
            elsif (defined $value) {
                # success
                $d_r->{RESULT}{$slew} = {} if (!exists $d_r->{RESULT}{$slew});
                $d_r->{RESULT}{$slew}{$key} = [ $value ];
                $external = 0;
            }
        }
        # if no success, try as a cap sweep
        if ($external && !%{$d_r->{RESULT}{$slew}}) {
            foreach my $cap (@{$V_r->{OUT_CAPS}}) {
                $d_r->{$slew}{$cap} = {};
                my @point = ($slew, $cap);
                foreach my $key ("delay", "slew") {
                    my $value = eval $eval_expr;
                    if ($@) {
                        die $@;
                        #die "Error evaluating expression " .
                        #    $params_r->{$param}{EXPR} . ":\n  $@\n";
                    }
                    if (!defined $value) {
                        print "Warning: $key unknown for property " .
                              "$param for (slew=$slew, cap=$cap).\n";
                        delete $d_r->{RESULT}{$slew}{$cap};
                        last;
                    }
                    else {
                        $d_r->{RESULT}{$slew}{$cap}{$key} = [ $value ];
                    }
                }
                if ($V_r->{DEBUG}) {
                    print "Evaluated $param ($slew, $cap) " .
                          "delay=$d_r->{RESULT}{$slew}{$cap}{delay}[0], " .
                          "slew=$d_r->{RESULT}{$slew}{$cap}{slew}[0]\n";
                }
            }
        }
        elsif (!$external && !%{$d_r->{RESULT}{$slew}}) {
            # Inconsistent results
            die "Assertion: evaluate_simple_param (1).\n";
        }
        else {
            if ($V_r->{DEBUG}) {
                print "Evaluated $param ($slew) " .
                      "delay=$d_r->{RESULT}{$slew}{delay}[0], " .
                      "slew=$d_r->{RESULT}{$slew}{slew}[0]\n";
            }
        }
    }
    if ($external) {
        my $out_node = "cap";
        ($out_node = $params_r->{$param}{IO}) =~ s/[-+]$// if ($type==0);
        push @{$d_r->{INDEPENDENT_VARS}}, $out_node;
    }
    return $d_r;
}

sub evaluate_constraint_param {
    my $V_r      = shift;
    my $params_r = shift;
    my $param    = shift;

    my $type = $params_r->{$param}{TYPE};

    my $d_r = {
        INDEPENDENT_VARS => [
            "slew(" . ($type==1 ? $params_r->{$param}{IO}
                                : $params_r->{$param}{REF}) . ")",
            "slew(" . ($type==1 ? $params_r->{$param}{REF}
                                : $params_r->{$param}{IO}) . ")"
        ],
        RESULT => {}
    };

    local $SIG{'__WARN__'}= 'die_on_warning';

    foreach my $max_path_input_slew (@{$V_r->{IN_SLEWS}}) {
        foreach my $min_path_input_slew (@{$V_r->{IN_SLEWS}}) {
            foreach my $key ("delay", "slew") {
                my $val = eval $params_r->{$param}{EXPR};
                if ($@) {
                    if ($V_r->{DEBUG}) {
                        die "Error evaluating expression " .
                            $params_r->{$param}{EXPR} . ":\n  $@";
                    }
                    else {
                        die $@;
                    }
                }
                $d_r->{RESULT}{$max_path_input_slew}
                              {$min_path_input_slew}{$key} = [ $val ];
            }
            if ($V_r->{DEBUG}) {
                print "Evaluated constraint property $param " .
                      "(max_path_input_slew=$max_path_input_slew,".
                      " min_path_input_slew=$min_path_input_slew):\n" .
                      "  delay=$d_r->{RESULT}{$max_path_input_slew}{$min_path_input_slew}{delay}[0], " .
                      "slew=$d_r->{RESULT}{$max_path_input_slew}{$min_path_input_slew}{slew}[0]\n";
            }
        }
    }
    return $d_r;
}

#
# Returns the expression to evaluate a constraint-type param:
#   max(maxpath1, maxpath2, ...) - min(minpath1, minpath2, ...)
# at a specific (max_path_input_slew, min_path_input_slew) point.
#
# Note: In order for this to be accurate, each max_paths and min_paths
#       set must each share a common input transition.  No path can
#       drive external to the cell (i.e. there is no support for
#       cap-sweeping these values.)
#
sub constraint_prop_expr {
    my $max_paths_lr = shift;
    my $min_paths_lr = shift;

    my $expr = "max(";
    foreach my $i (0..$#{$max_paths_lr}) {
        $expr .= "lookup_param(\$params_r, \"$max_paths_lr->[$i]\", ".
                 "\$key, \$max_path_input_slew)";
        $expr .= ", " if ($i < $#{$max_paths_lr});
    }
    $expr .= ") - min(";
    foreach my $i (0..$#{$min_paths_lr}) {
        $expr .= "lookup_param(\$params_r, \"$min_paths_lr->[$i]\", ".
                 "\$key, \$min_path_input_slew)";
        $expr .= ", " if ($i < $#{$min_paths_lr});
    }
    $expr .= ")";
}


sub die_on_warning {
    die $_[0];
}

sub lookup_param {
    my $params_r = shift;
    my $param    = shift;
    my $key      = shift;
    my @point    = @_;

    if (!exists $params_r->{$param}) {
        die "Reference to undefined parameter $param.\n" 
    }
    if (@point != @{$params_r->{$param}{INDEPENDENT_VARS}}) {
        die "Assertion: lookup_param (1).\n";
    }
    if (!exists $params_r->{$param} || !exists $params_r->{$param}{RESULT}) {
        die "Reference to undefined parameter $param.\n"
    }

    my $result_r = $params_r->{$param}{RESULT};
    foreach my $iv (@point) {
        die "Assertion: lookup_param (2).\n" if (!exists $result_r->{$iv});
        $result_r = $result_r->{$iv};
    }
    if (!exists $result_r->{$key}) {
        die "Assertion: lookup_param (3), $param \@ @point ($key).\n";
    }
    return @{$result_r->{$key}};
}

# For use in expression evaluation
sub min {
    my $val = shift;
    while (@_) {
        my $newval = shift;
        $val = defined $val && defined $newval ? 
                ($newval < $val ? $newval : $val) : undef;
    }
    return $val;
}

# For use in expression evaluation
sub max {
    my $val = shift;
    while (@_) {
        my $newval = shift;
        $val = defined $val && defined $newval ? 
                ($newval > $val ? $newval : $val) : undef;
    }
    return $val;
}

# For use in expression evaluation
sub val {
    return shift;
}

# Given (a, b, c), returns "a, b, c".
sub list_to_string {
    my $str = "";
    while (@_) {
        $str .= shift;
        $str .= ", " if (@_);
    }
    return $str;
}

############################### API routines ###############################

#
# Delay properties are named "min_delay|...", "max_delay|..." etc.
# but we'll expose them as "delay|...".
#
sub delay_prop_base_name {
    my $delay_prop = shift;
    $delay_prop =~ s/^(min|max)_//;
    return $delay_prop;
}


1;
