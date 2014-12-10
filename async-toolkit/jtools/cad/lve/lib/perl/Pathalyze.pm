# $Id: //depot/sw/cad/lve/main/lib/perl/Pathalyze.pm#5 $
# $DateTime: 2005/09/02 14:10:08 $

package Pathalyze;

use strict;
use Cast::NodeAliasing;
use Pathalyze::Parser;
use Pathalyze::Measurement;
use Pathalyze::Parameter;

my  $package_root;

BEGIN {
    use Exporter;
    our @ISA = qw(Exporter);
    our @EXPORT = qw(
        &default_vars
        &pathalyze
        &query_pin_properties
        &query_constraint_pins
        &query_delay_pins
        &query_delay_value
        &query_constraint_value
    );
}

#
# Returns a pathalyze runtime variable structure, initialized to
# defaults.  Needs a java package root passed to it.
#
sub default_vars {
    my $java_package_root = shift;
    return (
        ALINT_MODE       => "extracted",
        CAST_DIR         => "",
        CONSERVATIVE     => 1,
        CORNER           => "ss",
        DEBUG            => 0,
        HEAP_SIZE        => "1G",
        IN_SLEWS         => [30],
        LVE_CAP_DIR      => "",
        LVE_DIRS         => [],
        MODE             => "extracted",
        NO_SWEEPS        => 0,
        OUTPUT_DIR       => undef,       # no file output if undef
        OUT_CAPS         => [],
        PACKAGE_ROOT     => $java_package_root,
        FULCRUM_PDK_ROOT => $ENV{FULCRUM_PDK_PACKAGE_ROOT},
        PATH_REPORT_RATE => 0,
        PACKAGE_NAME     => "all",
        PIN_TO_PIN       => 1,
        ROOT_SUBTYPE     => "",
        SLEW_WARN_HI     => 0.0,
        SLEW_WARN_LO     => 0.0,
        SPEC_DIR         => "",
        TEMP             => 125,
        VERBOSE          => 0,
        VIEW             => undef,
        VOLTAGE          => 1.08,
    );
}


#
# Master pathalyze routine
#
sub pathalyze {
    my $VARS_r        = shift;
    my $pathspec_file = shift;

    my $v = $VARS_r->{VERBOSE};
    #
    # Global path and property definitions
    #
    my $P_r = {
        VARS => $VARS_r,
        CELLS => {},
        INFO => {
            num_files => 0,
            num_paths => 0,
            num_exp_paths => 0,
            num_params => 0
        }
    };

    #
    # Check for consistency/correctness of input parameters
    #
    if (@{$VARS_r->{OUT_CAPS}}) {
        if (!-d $VARS_r->{LVE_CAP_DIR}) {
            die "Error: Directory $VARS_r->{LVE_CAPS_DIR} doesn't exist.\n";
        }
        my @caps = ();
        foreach my $cap (@{$VARS_r->{OUT_CAPS}}) {
            my $dir = $VARS_r->{LVE_CAP_DIR} . "/$cap";
            if (!-e $dir) {
                print STDERR "Warning: Cap-sweep directory\n";
                print STDERR "         $cap\n";
                print STDERR "         doesn't exist.  Skipping this " .
                             "capacitance value.\n";
            }
            else {
                push @caps, $cap;
            }
        }
        $VARS_r->{OUT_CAPS} = \@caps;
    }

    $VARS_r->{CAST_PATH} = $VARS_r->{CAST_DIR} . ":" . $VARS_r->{SPEC_DIR};

    #
    # Read pathalyze specification file(s)
    #
    print "==== Parsing $pathspec_file ====\n" if $v;
    parse_path_spec($pathspec_file, $P_r, $VARS_r->{ROOT_SUBTYPE});

    if ($v) {
        print "Number of files parsed:     $P_r->{INFO}{num_files}\n";
        print "Number of cells:            " . scalar(keys %{$P_r->{CELLS}})
                                             . "\n";
        print "Number of path definitions: $P_r->{INFO}{num_paths}\n";
        print "Number of expanded paths:   $P_r->{INFO}{num_exp_paths}\n";
        print "Number of properties:       $P_r->{INFO}{num_params}\n";
    }

    #
    # Measure all path delays
    #
    print "\n==== Measuring all path delays ====\n" if $v;
    my %DELAY_MAP = ();
    my %SLEW_MAP = ();
    my $path_count = 0;
    foreach my $top (keys %{$P_r->{CELLS}}) {
        foreach my $pname (keys %{$P_r->{CELLS}{$top}{PATHS}}) {
            my $path_r = $P_r->{CELLS}{$top}{PATHS}{$pname};
            if ($VARS_r->{DEBUG}) {
                print "Measuring path $pname:\n";
                print_path($P_r->{CELLS}{$top}{PATHS}{$pname});
            }
            my $pd_r;
            eval {
                $pd_r = measure_path_delay($VARS_r, $top, $path_r->{INPUT}, 
                                           $path_r->{SEQUENCE}, $pname);
            };
            if ($@) {
                print "In cell $top:\n";
                print "     Couldn't measure path $pname.\n";
                (my $msg = $@) =~ s/\n/\n     /sg;
                $msg =~ s/\s*$//;
                print "     $msg\n";
            }
            else {
                # Add measured delay/slew values under both the path base
                # name (w/out array indices or directionality) and under
                # the specific expanded path name
                (my $pbasename = $pname) =~ s/\[.*$//;
                $pbasename =~ s/\:.*$//;
                #
                # Make sure there is no name conflict with this path name
                # and a param definition
                #
                if (exists $P_r->{CELLS}{$top}{PARAM}{$pname}) {
                    die "Property $pname in cell $top conflicts with path " .
                        "of the same name.\n";
                }
                set_prop($P_r->{CELLS}{$top}{PARAM}, $pname, $pd_r);
                set_prop($P_r->{CELLS}{$top}{PARAM}, $pbasename, $pd_r, 1);
                print "Set properties $pname & $pbasename\n" if ($VARS_r->{DEBUG});
            }
            $path_count++;
            if ($VARS_r->{PATH_REPORT_RATE} != 0 &&
                $path_count % $VARS_r->{PATH_REPORT_RATE} == 0) {
                print "Measured $path_count / $P_r->{INFO}{num_exp_paths} " .
                      "paths.\n";
            }
        }
    }

    #
    # Calculate all cell properties
    #
    print "\n==== Property Evaluation ====\n" if ($v);
    foreach my $cell (keys %{$P_r->{CELLS}}) {
        evaluate_cell_properties($VARS_r, $P_r->{CELLS}{$cell}, $cell);
    }

    return $P_r;
}

#
# Query a (cell, pin) for a list of associated properties.
#
# Call after first running pathalyze().  Returns the following structure:
#
#   TYPE -> -1: not an external endpoint of any property's path(s).
#            0: output specified by one or more delay path properties.
#            1: data input constrained by setup and/or hold properties.
#            2: reference input constrained by setup and/or hold properties.
#            3: hybrid case (output of a delay path -and-
#               referenced by a setup/hold constraint input, or else
#               referenced by both data and reference inputs of setup/hold
#               constraints).  This case should not happen!
#            4: data input to one or more delay paths only.
#
#   DELAY_OUTPUT_PROPS -> [ <delay_output_proplist> ]
#   DELAY_INPUT_PROPS  -> [ <delay_input_proplist> ]
#   SETUP_DATA_PROPS   -> [ <setup_data_proplist> ]
#   SETUP_REF_PROPS    -> [ <setup_ref_proplist> ]
#   HOLD_DATA_PROPS    -> [ <hold_data_proplist> ]
#   HOLD_REF_PROPS     -> [ <hold_ref_proplist> ]
#
# The 'TYPE' value is fully specified by which <*_proplist>'s are non-empty.
# Specifically:
#
#   TYPE==0:
#       delay_output_proplist > 0 && all others empty
#   TYPE==1:
#       (setup_data_props > 0 || hold_data_props > 0)
#       && all others except DATA_INPUT_PROPS empty
#   TYPE==2:
#       (setup_ref_props > 0 || setup_ref_props > 0)
#       && all others except DATA_INPUT_PROPS empty
#   TYPE==3:
#       delay_output_proplist > 0 &&
#           (setup_data_props > 0 || hold_data_props > 0 ||
#            setup_ref_props > 0  || hold_ref_props > 0)
#       || (setup_data_props > 0 || hold_data_props > 0 &&
#           (setup_ref_props > 0  || hold_ref_props > 0)
#
sub query_pin_properties {
    my $P_r  = shift;
    my $cell = shift;
    my $pin  = shift;

    my $ret_r = {
        TYPE => -1,
        DELAY_OUTPUT_PROPS => [],
        DELAY_INPUT_PROPS  => [],
        SETUP_DATA_PROPS   => [],
        SETUP_REF_PROPS    => [],
        HOLD_DATA_PROPS    => [],
        HOLD_REF_PROPS     => []
    };
    my %delay_in_props = ();
    my %delay_out_props = ();

    die "Undefined cell $cell queried.\n" if (!exists $P_r->{CELLS}{$cell});

    foreach my $prop (sort keys %{$P_r->{CELLS}{$cell}{PARAM}}) {
        my $prop_r = $P_r->{CELLS}{$cell}{PARAM}{$prop};
        if ($prop_r->{TYPE}==0) {
            (my $n = $prop_r->{IO}) =~ s/[-+]$//;
            my $delay_prop = delay_prop_base_name($prop);
            if (is_aliased($P_r->{VARS}, $cell, $n, $pin, 1)) {
                $delay_out_props{$delay_prop} = 1;
            }
            ($n = $prop_r->{REF}) =~ s/[-+]$//;
            if (is_aliased($P_r->{VARS}, $cell, $n, $pin, 1)) {
                $delay_in_props{$delay_prop} = 1;
            }
        }
        elsif ($prop_r->{TYPE}==1) {
            (my $n = $prop_r->{IO}) =~ s/[-+]$//;
            if (is_aliased($P_r->{VARS}, $cell, $n, $pin, 1)) {
                push @{$ret_r->{SETUP_DATA_PROPS}}, $prop;
            }
            ($n = $prop_r->{REF}) =~ s/[-+]$//;
            if (is_aliased($P_r->{VARS}, $cell, $n, $pin, 1)) {
                push @{$ret_r->{SETUP_REF_PROPS}}, $prop;
            }
        }
        elsif ($prop_r->{TYPE}==2) {
            (my $n = $prop_r->{IO}) =~ s/[-+]$//;
            if (is_aliased($P_r->{VARS}, $cell, $n, $pin, 1)) {
                push @{$ret_r->{HOLD_DATA_PROPS}}, $prop;
            }
            ($n = $prop_r->{REF}) =~ s/[-+]$//;
            if (is_aliased($P_r->{VARS}, $cell, $n, $pin, 1)) {
                push @{$ret_r->{HOLD_REF_PROPS}}, $prop;
            }
        }
    }
    push @{$ret_r->{DELAY_OUTPUT_PROPS}}, sort keys %delay_out_props;
    push @{$ret_r->{DELAY_INPUT_PROPS}}, sort keys %delay_in_props;

    if (@{$ret_r->{DELAY_OUTPUT_PROPS}} == 0 &&
        @{$ret_r->{DELAY_INPUT_PROPS}} == 0 &&
        @{$ret_r->{SETUP_DATA_PROPS}} == 0 &&
        @{$ret_r->{SETUP_REF_PROPS}} == 0 &&
        @{$ret_r->{HOLD_DATA_PROPS}} == 0 &&
        @{$ret_r->{HOLD_REF_PROPS}} == 0) {
        $ret_r->{TYPE} = -1;
    }
    elsif (@{$ret_r->{DELAY_OUTPUT_PROPS}} > 0 &&
        @{$ret_r->{DELAY_INPUT_PROPS}} == 0 &&
        @{$ret_r->{SETUP_DATA_PROPS}} == 0 &&
        @{$ret_r->{SETUP_REF_PROPS}} == 0 &&
        @{$ret_r->{HOLD_DATA_PROPS}} == 0 &&
        @{$ret_r->{HOLD_REF_PROPS}} == 0) {
        $ret_r->{TYPE} = 0;
    }
    elsif (@{$ret_r->{DELAY_OUTPUT_PROPS}} == 0 &&
        @{$ret_r->{DELAY_INPUT_PROPS}} > 0 &&
        @{$ret_r->{SETUP_DATA_PROPS}} == 0 &&
        @{$ret_r->{SETUP_REF_PROPS}} == 0 &&
        @{$ret_r->{HOLD_DATA_PROPS}} == 0 &&
        @{$ret_r->{HOLD_REF_PROPS}} == 0) {
        $ret_r->{TYPE} = 4;
    }
    elsif ((@{$ret_r->{SETUP_DATA_PROPS}} > 0 || 
            @{$ret_r->{HOLD_DATA_PROPS}} > 0)   &&
           @{$ret_r->{DELAY_OUTPUT_PROPS}} == 0 &&
           @{$ret_r->{SETUP_REF_PROPS}} == 0    &&
           @{$ret_r->{HOLD_REF_PROPS}} == 0) {
        $ret_r->{TYPE} = 1;
    }
    elsif ((@{$ret_r->{SETUP_REF_PROPS}} > 0 || 
            @{$ret_r->{HOLD_REF_PROPS}} > 0)    &&
           @{$ret_r->{DELAY_OUTPUT_PROPS}} == 0 &&
           @{$ret_r->{SETUP_DATA_PROPS}} == 0   &&
           @{$ret_r->{HOLD_DATA_PROPS}} == 0) {
        $ret_r->{TYPE} = 2;
    }
    else {
        $ret_r->{TYPE} = 3;
    }
    return $ret_r;
}

#
# Returns the data and reference input pins of the specified constraint 
# property.  Returns a pair (ref_pin, data_pin), where each pin string
# will either be of the form '<node>', indicating a posedge+negedge 
# constraint, or '<node>+' or '<node>-', indicating a singled-ended 
# posedge or negedge constraint respectively.  In most cases, ref_pin
# will be directional while data_pin will be bidirectional.
#
sub query_constraint_pins {
    my $P_r             = shift;
    my $cell            = shift;
    my $constraint_prop = shift;
    check_prop($P_r, $cell, $constraint_prop, 1);
    return ($P_r->{CELLS}{$cell}{PARAM}{$constraint_prop}{REF},
            $P_r->{CELLS}{$cell}{PARAM}{$constraint_prop}{IO});
}

#
# Returns the input and output pins associated with the specified delay 
# path property.  Returns a pair (in_pin, out_pin), where each pin
# is always directional ('<node>+' or '<node->').
# 
sub query_delay_pins {
    my $P_r        = shift;
    my $cell       = shift;
    my $delay_prop = shift;
    check_prop($P_r, $cell, $delay_prop, 0);
    return ($P_r->{CELLS}{$cell}{PARAM}{"min_$delay_prop"}{REF},
            $P_r->{CELLS}{$cell}{PARAM}{"min_$delay_prop"}{IO});
}

#
# Returns the output delay & slew rate of the specified cell delay property,
# for the specified input slew rate and output capacitance.  If minmax==0,
# min values are returned, otherwise max values are returned.  Returns
# a pair (delay_value, slew_value).
#
# Note: input_slew and output_cap *must* be values from the IN_SLEWS and
#       OUT_CAPS lists.
#
sub query_delay_value {
    my $P_r        = shift;
    my $cell       = shift;
    my $delay_prop = shift;
    my $input_slew = shift;
    my $output_cap = shift;
    my $minmax     = shift;     # 0:min, 1:max

    check_prop($P_r, $cell, $delay_prop, 0);
    my $prop_r = $minmax ? $P_r->{CELLS}{$cell}{PARAM}{"max_$delay_prop"}
                         : $P_r->{CELLS}{$cell}{PARAM}{"min_$delay_prop"};
    if (!exists $prop_r->{RESULT}{$input_slew}) {
        
        my @s=(keys %{$prop_r->{RESULT}});
        print STDERR "Existing Slews : @s\n";
        die "Undefined slew point $input_slew requested for cell $cell,".
           "property $delay_prop.\n";
    }
    die "Undefined capacitance point $output_cap requested for cell $cell, " .
        "property $delay_prop.\n" 
        if (!exists $prop_r->{RESULT}{$input_slew}{$output_cap});
    return ($prop_r->{RESULT}{$input_slew}{$output_cap}{delay}[0],
            $prop_r->{RESULT}{$input_slew}{$output_cap}{slew}[0]);
}

#
# Returns the specified constraint's value (a delay in ps) for the specified
# data input slew rate and reference input slew rate.
#
# Note: data_slew and ref_slew *must* be values from the IN_SLEWS list.
# 
sub query_constraint_value {
    my $P_r             = shift;
    my $cell            = shift;
    my $constraint_prop = shift;
    my $ref_slew        = shift;
    my $data_slew       = shift;

    check_prop($P_r, $cell, $constraint_prop, 1);
    my $prop_r = $P_r->{CELLS}{$cell}{PARAM}{$constraint_prop};
    my $slew1 = $prop_r->{TYPE}==1 ? $data_slew : $ref_slew;
    my $slew2 = $prop_r->{TYPE}==1 ? $ref_slew  : $data_slew;
    die "Undefined slew point $slew1 requested for cell $cell, property ".
        "$constraint_prop.\n" if (!exists $prop_r->{RESULT}{$slew1});
    die "Undefined slew point $slew2 requested for cell $cell, property ".
        "$constraint_prop.\n" if (!exists $prop_r->{RESULT}{$slew1}{$slew2});
    return $prop_r->{RESULT}{$slew1}{$slew2}{delay}[0];
}


# Correctness assertions
sub check_prop {
    my $P_r      = shift;
    my $cell     = shift;
    my $prop     = shift;
    my $ptype    = shift;

    die "Undefined cell '$cell' quried.\n" if (!exists $P_r->{CELLS}{$cell});
    if ($ptype==1 && !exists $P_r->{CELLS}{$cell}{PARAM}{$prop} ||
        $ptype==0 && (!exists $P_r->{CELLS}{$cell}{PARAM}{"min_$prop"} ||
                      !exists $P_r->{CELLS}{$cell}{PARAM}{"max_$prop"})) {
        die "Undefined property '$prop' queried for cell $cell.\n";
    }
    if ($ptype==0 && !$P_r->{CELLS}{$cell}{PARAM}{"min_$prop"}{TYPE}==0 ||
        $ptype==1 && !$P_r->{CELLS}{$cell}{PARAM}{$prop}{TYPE}==1 &&
                     !$P_r->{CELLS}{$cell}{PARAM}{$prop}{TYPE}==2) {
        die "Property '$prop' of cell $cell not of the expected type.\n";
    }
}
    

# Just for debugging...
sub print_path {
    my $path_r = shift;

    print "  input       = " . $path_r->{INPUT} 
                        .  "\n";
    print "  output      = " . $path_r->{OUTPUT} 
                        .  "\n";
    print "  transitions =";
    foreach my $t (@{$path_r->{SEQUENCE}}) {
        print " $t";
    }
    print "\n";
}



1;
