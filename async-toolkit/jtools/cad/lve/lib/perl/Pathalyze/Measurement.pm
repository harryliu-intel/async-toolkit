# $Id: $
# $DateTime: $

package Pathalyze::Measurement;

use strict;
use FileHandle;
use Cast::JavaServer;
use Util::Numeric;

BEGIN {
    use Exporter;
    our @ISA = qw(Exporter);
    our @EXPORT = qw(&measure_path_delay);
}


#
# Measures a path delay
#
#   input   - Input fan-in transition (optional, but highly recommended).
#             Delay of this transition is not included in the path delay.
#   path_lr - Array of nodes/transitions.  Either 'input' or the first
#             node of path_lr should include a trailing "-" or "+".  
#             If none is provided, then "+" is assumed.  All other nodes 
#             in the path may optionally include a trailing "-/+",
#             and a warning will be printed if they're non-alternating.
#
#   V_r     - Reference to a configuration variable map.  Should define the
#             following:
#
#       IN_SLEWS        - list of input slew rates to measure (ps)
#       DEBUG           - debug mode (0|1)
#       SPEC_DIR        - Cast layout subtypes root directory
#       LVE_DIRS        - List of lve root directories to search for delays
#       VIEW            - layout, floorplan, etc.
#       MODE            - estimated, extracted, etc.
#       CORNER          - lve results process corner (tt/ss/etc.)
#       VOLTAGE         - lve results voltage (e.g. 0.9)
#       TEMP            - lve results temperature (e.g. 90)
#       PATH_FILE       - File in which delay breakdown info should go.
#
#       OUT_CAPS        - [Optional] List of output capacitances to sweep
#                         over.  If specified, each cap value is expected
#                         to have a corresponding directory under 
#                         LVE_CAP_DIRS and the cap-specific directory 
#                         should contain alint results for the last
#                         transition in the path.  (One or more of these
#                         cap-specific directories may also need to be
#                         included in LVE_DIRS if they contain results
#                         for other intermediate transitions in the path.)
#       LVE_CAP_DIRS    - [Optional] Root directory containing cap-specific
#                         alint delay characterization results for the
#                         output transition of the path.  Each subdirectory
#                         under LVE_CAP_DIRS should have a floating point
#                         name (e.g. 0.08532e-12) corresponding to the cap
#                         values in the OUT_CAPS list.
#
# Note: Will -die- if there is a problem determining the path delay.  Run
# this routine in an eval{} block.
#
# Returns a structure of the following form:
#
#   INDEPENDENT_VARS -> [ "slew(input_transition)", "cap(output_node)"
#   RESULT -> <input_slew> -> <output_cap> -> min_slew
#                                             max_slew
#                                             min_delay
#                                             max_delay
#
# However, if either (a) OUT_CAPS is empty or (b) the output transition 
# is not found under a cap-sweep directories but *is* found under LVE_DIRS, 
# then the <output_cap> dimension will be omitted, and the INDEPENDENT_VARS
# list will only include the slew reference.
#
# Subtlety: If a cell output appears as a non-terminal transition of some
# path, and cell output transitions have all been cap-swept, then one
# capacitance directory should be chosen for calculating the delay of such
# a path.  That cap directory (and no others) should be included in LVE_DIRS.
#
sub measure_path_delay {
    my $V_r       = shift;
    my $top       = shift;      # Top-level cell in which path is defined
    my $input     = shift;      # Optional input transition
    my $path_lr   = shift;      # Path transition sequence 
    my $path_name = shift;      # Path name (for user feedback only)

    # set up return value structure
    my $pd_r = {
        INDEPENDENT_VARS => [ "slew($input)" ],
        RESULT => {}
    };
    my $s;
    foreach $s (@{$V_r->{IN_SLEWS}}) {
        $pd_r->{RESULT}{$s} = { 
            internal => {
                min_slew => $s,
                max_slew => $s,
                min_delay => 0.0,
                max_delay => 0.0,
            },
            FH => undef
        };
    }

    # Open detailed path info file(s)
    my $file_output = defined $V_r->{OUTPUT_DIR};
    if ($file_output) {
        foreach $s (@{$V_r->{IN_SLEWS}}) {
            my $slew_dir = "$V_r->{OUTPUT_DIR}/$top/slew_$s";
            `mkdir -p '$slew_dir'`;
            open ($pd_r->{RESULT}{$s}{FH}, ">$slew_dir/$path_name") || 
                die "Couldn't write to $slew_dir/$path_name.\n";
            $pd_r->{RESULT}{$s}{FH}->format_top_name("Report_Top");
            $pd_r->{RESULT}{$s}{FH}->format_name("Report");
        }
    }

    my $transition;
    my $dir = 1;
    my @dir_label   = ( "-", "+" );
    my $last_pathnode;
    my $last_instance;
    my $last_subtype;
    if ($input =~ /^(.*)([-+])$/) {
        # Strip leading "." from node name (some script-generated pathalyze 
        # spec files will specify top-level nodes as ".name").
        ($last_pathnode = $1) =~ s/^\.//;
        ($last_subtype, $last_instance, $last_pathnode) =
            local_node_lookup($V_r, $top, $last_pathnode, "", 1);

        if (!defined $last_subtype) {
            die "Initial node $1 doesn't exist.\n";
        }
        $dir = 0 if ($2 eq "+");
        #$start = 1;
    }
    else {
        $dir = 0 if ($path_lr->[0] =~ /([-+]?)$/ && $1 eq "-");
    }

    my ($d_r, $cap);
    my $internal_match = 0;
    my $external_match = 0;
    for my $i (0..$#{$path_lr}) {
        my $tran = $path_lr->[$i];
        (my $pathnode = $tran) =~ s/([-+]?)$//;
        # Strip leading "." from node name (see above)
        $pathnode =~ s/^\.//;
        if ($dir_label[$dir] ne $1) {
            warn "Warning: Non-inverse-monotonic path transition $tran\n";
            $dir = 1 if ($1 eq "+");
        }
        $transition = $pathnode . $dir_label[$dir];

        print "Determining delay of $transition.\n" if ($V_r->{DEBUG});

        my ($subtype, $instance, $node) = 
            local_node_lookup($V_r, $top, $pathnode, "", 1);

        print "  Local node: $subtype($instance)/$node\n" if ($V_r->{DEBUG});

        if (!defined $subtype) {
            die "Node $pathnode doesn't exist.\n";
        }

        my $fanin_node = ();
        if (defined $last_pathnode) {
            $fanin_node = identify_fanin_alias($V_r, $last_pathnode, 
                                        $last_instance, $last_subtype,
                                        $instance, $subtype);
            print "  Fanin node: $fanin_node\n" if ($V_r->{DEBUG});
            if (!defined $fanin_node) {
                $last_instance .= "/" if ($last_instance);
                die "Couldn't identify appropriate fan-in alias of\n" .
                    "$last_subtype/${last_instance}$last_pathnode\n" .
                    "within instance $instance ($subtype)\n";
            }
        }

        foreach $s (@{$V_r->{IN_SLEWS}}) {
            my @caps = ($i == $#{$path_lr} && @{$V_r->{OUT_CAPS}}) ?
                       @{$V_r->{OUT_CAPS}} : ("internal");
            foreach $cap (@caps) {
                $d_r = interpolate_delay($V_r, $subtype, $node, $dir, 
                                $pd_r->{RESULT}{$s}{internal}{min_slew}, 
                                $pd_r->{RESULT}{$s}{internal}{max_slew},
                                $fanin_node, $path_name, 
                                "$V_r->{LVE_CAP_DIR}/$cap");

                if (($d_r->{min_delay} == -1 || $d_r->{min_slew} == -1) ||
                    ($d_r->{max_delay} == -1 || $d_r->{max_slew} == -1)) {
                    die "Couldn't determine delay for transition $tran.\n";
                }
                # Determine whether transition is external or internal
                if ($d_r->{cap_dir}) {
                    die "Assertion: measure_delay (1).\n" 
                        if ($i != $#{$path_lr});
                    die "Assertion: Duplicate output cap measurements in ".
                        "measure_delay (2).\n" if (exists $d_r->{$s}{$cap});
                    $external_match = 1;
                    last if ($internal_match);
                    $transition = $pathnode . $dir_label[$dir] . " (".
                                  round_float($cap*1e15,-4) . " fF)";
                }
                else {
                    $internal_match = 1 if ($i==$#{$path_lr});
                    if ($file_output) {
                        $pd_r->{RESULT}{$s}{FH}->format_name("Report") 
                    }
                    $transition = $pathnode . $dir_label[$dir];
                    $cap = "internal";
                }
                if (!exists $pd_r->{RESULT}{$s}{$cap}) {
                    $pd_r->{RESULT}{$s}{$cap} = {};
                }
                $pd_r->{RESULT}{$s}{$cap}{min_slew}  = $d_r->{min_slew};
                $pd_r->{RESULT}{$s}{$cap}{max_slew}  = $d_r->{max_slew};
                $pd_r->{RESULT}{$s}{$cap}{min_delay} = $d_r->{min_delay} +
                    $pd_r->{RESULT}{$s}{internal}{min_delay};
                $pd_r->{RESULT}{$s}{$cap}{max_delay} = $d_r->{max_delay} +
                    $pd_r->{RESULT}{$s}{internal}{max_delay};
                write $pd_r->{RESULT}{$s}{FH} if ($file_output);
                last if ($internal_match);
            }
        }
        $dir           = 1-$dir;
        $last_pathnode = $node;
        $last_instance = $instance;
        $last_subtype  = $subtype;
    }
    if ($internal_match && $external_match) {
        print STDERR "Warning: Inconsistent delay data for " .
                     "$path_lr->[$#{$path_lr}].\n".
                     "         Appears as both internal and external ".
                     "transition.\n".
                     "         Attempting to treat as internal, but some ".
                     "slew points could be wrong.\n";
        foreach my $slew (keys %{$pd_r->{RESULT}}) {
            foreach my $cap (keys %{$pd_r->{RESULT}{$s}}) {
                delete $pd_r->{RESULT}{$slew}{$cap} 
                    unless ($cap eq "internal" || $cap eq "FH");
            }
        }
        $external_match = 0;
    }
    if ($external_match) {
        push @{$pd_r->{INDEPENDENT_VARS}}, "cap($last_pathnode)";
        # Eliminate incomplete "internal" delay/slew data point
        foreach my $slew (keys %{$pd_r->{RESULT}}) {
            delete $pd_r->{RESULT}{$slew}{internal};
        }
    }
    else {
        # Eliminate degenerate "internal" dimension in return structure
        foreach my $slew (keys %{$pd_r->{RESULT}}) {
            $pd_r->{RESULT}{$slew}{min_slew} = 
                $pd_r->{RESULT}{$slew}{internal}{min_slew};
            $pd_r->{RESULT}{$slew}{max_slew} = 
                $pd_r->{RESULT}{$slew}{internal}{max_slew};
            $pd_r->{RESULT}{$slew}{min_delay} = 
                $pd_r->{RESULT}{$slew}{internal}{min_delay};
            $pd_r->{RESULT}{$slew}{max_delay} = 
                $pd_r->{RESULT}{$slew}{internal}{max_delay};
            delete $pd_r->{RESULT}{$slew}{internal};
        }
    }

    if ($file_output) {
        foreach my $slew (@{$V_r->{IN_SLEWS}}) {
            $pd_r->{RESULT}{$slew}{FH}->print("\n");
            $pd_r->{RESULT}{$slew}{FH}->close();
            delete $pd_r->{RESULT}{$slew}{FH};
        }
    }

    format Report_Top =

                                       Delay        Slew         Total Delay
Transition                           min , max    min , max       min , max
----------------------------------  -----------  ------------  ---------------  
.

    format Report =
@<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  @>>>>,@>>>>  @>>>>,@>>>>>  @>>>>>>,@>>>>>>
$transition,round_float($d_r->{min_delay},-1),round_float($d_r->{max_delay},-1),round_float($d_r->{min_slew},-1),round_float($d_r->{max_slew},-1),round_float($pd_r->{RESULT}{$s}{$cap}{min_delay},-1),round_float($pd_r->{RESULT}{$s}{$cap}{max_delay},-1)
.

    return $pd_r;
}

##############################################################################
#                                 SUBROUTINES                                #
##############################################################################

#
# Looks up subtype in which the specified node is local.  Returns fully-
# qualified subtype name and the local node name.  This routine is 
# totally unoptimized for now -- many files are opened for each node 
# lookup (eventually should implement a cell cache.)
#
# Arguments:
#     subtype: Subtype beneath which to look for the node
#     node: Node name
#     instname: Instance name of 'subtype' (for recursive use)
#
# Uses:
#     DEBUG, ROUTED, ALINT_MODE, 
#     others [see JavaServer].
#
# Returns:
#     Subtype in which 'node' is local.
#     Instance name of the subtype, referenced from the root subtype. Note:
#       All hierarchy separator characters are switched to ".".
#     Local node name.
#
#
my $LOCALNODE_CACHE_r = {};

sub get_localnode_cache {
    my $V_r = shift;
    my $subtype = shift;
    my $include_ports = shift;

    if (!exists $LOCALNODE_CACHE_r->{$subtype}) {
        print "Querying aliases for subtype $subtype.\n" if ($V_r->{DEBUG});
        $LOCALNODE_CACHE_r->{$subtype} = {};

        # first look for local nodes; this is originally the only nodes
        # considered.
        my $cmd = "com.avlsi.tools.jflat.JFlat ";
        $cmd .= " --cast-path=$V_r->{CAST_PATH} ";
        $cmd .= " --routed" if ($V_r->{ALINT_MODE} eq "routed");
        $cmd .= " --tool=query";
        $cmd .= " --query-tasks=local_nodes=all_aliases,external_nodes=all_aliases:implied";
        $cmd .= " --query-no-recurse";
        $cmd .= " --query-no-header";
        $cmd .= " --cell=$subtype";
        my $alias_lines = [];
        run_cast_server_command($V_r, $cmd, undef, undef, -1, $alias_lines);
        foreach my $l (@{$alias_lines}) {
            my @aliases = split /=/, (split /\s+/, $l)[0];
            foreach my $a (@aliases) {
                $LOCALNODE_CACHE_r->{$subtype}{$a} = $aliases[0];
            }
        }
    }
}

sub local_node_lookup {
    my $V_r           = shift;
    my $subtype       = shift;
    my $node          = shift;
    my $instname      = shift;
    my $include_ports = shift;      # Include ports only at topmost level

    if ($V_r->{DEBUG}) {
        print "Looking up node $node in $subtype/$instname\n";
    }
    get_localnode_cache($V_r, $subtype, $include_ports);
    if (exists $LOCALNODE_CACHE_r->{$subtype}{$node}) {
        print "Found $subtype/$instname/".
            $LOCALNODE_CACHE_r->{$subtype}{$node}."\n" if ($V_r->{DEBUG});
        return ($subtype,$instname,$LOCALNODE_CACHE_r->{$subtype}{$node});
    }

    print " Determining instance of $subtype containing $node.\n" 
        if ($V_r->{DEBUG});
    my ($child,$local_inst,$local_node) = instance_lookup($V_r,$subtype,$node)
        unless ($V_r->{ALINT_MODE} eq "flat");

    if (!defined $child) {
        print "  -> Didn't find one.\n" if ($V_r->{DEBUG});
        # No more hierarchy in the node name or else this is flat mode.
        # Assume Local node is here. (The name has a dot in it, e.g. _x.0)
        if ($V_r->{ALINT_MODE} eq "flat") {
            return ($subtype,$instname,$node);
        }
        else {
            # Couldn't locate the node
            return undef;
        }
    }
    else {
        print "  -> Child: $child, Instance: $instname.\n" if ($V_r->{DEBUG});
        # The node becomes local further down; recurse down to child
        if ($instname eq "") { $instname = $local_inst; }
        else { $instname = "$instname.$local_inst"; }
        return local_node_lookup($V_r, $child, $local_node, $instname, 0);
    }
}

sub identify_fanin_alias {
    my $V_r             = shift;
    my $fanin_nodename  = shift;
    my $fanin_instance  = shift;
    my $fanin_subtype   = shift;
    my $target_instance = shift;
    my $target_subtype  = shift;

    if ($V_r->{DEBUG}) {
        print "Looking for aliases of fanin $fanin_nodename from\n" .
              "  $fanin_subtype ($fanin_instance) within $target_instance.\n";
    }

    # Resolve input fan-in node alias (three cases)
    my $fanin_alias;
    if ($fanin_instance eq $target_instance) {
        # Case 1: Both are local in the same parent cell
        $fanin_alias = $fanin_nodename;
    }
    elsif (length($fanin_instance) > length($target_instance) &&
           substr($fanin_instance, 0, length($target_instance)) eq 
                $target_instance) {
        # Case 2: Input fan-in node is local in a subcell of the target
        # node's parent cell.
        my $l = length($target_instance);
        $fanin_alias = substr($fanin_instance, ($l==0 ? 0 : $l+1)) .
                           ".$fanin_nodename";
    }
    elsif (length($fanin_instance) < length($target_instance) &&
           substr($target_instance, 0, length($fanin_instance)) eq 
                $fanin_instance) {
        # Case 3: Input fan-in node is local in some parent cell of 
        # the target node's local parent cell.  In this case, we need
        # to find all aliases of the fan-in node which have the same
        # instance prefix of the target node's parent cell (i.e. the
        # target parent cell's port node aliases).
        my $l = length($fanin_instance);
        my $sub_instname = substr($target_instance, ($l==0 ? 0 : $l+1)) . ".";
        $l = length($sub_instname);
        if (!exists $LOCALNODE_CACHE_r->{$fanin_subtype}) {
            warn "Assertion: Local node aliases of $fanin_subtype not defined.";
        }
        foreach my $a (keys %{$LOCALNODE_CACHE_r->{$fanin_subtype}}) {
            if ($LOCALNODE_CACHE_r->{$fanin_subtype}{$a} eq $fanin_nodename &&
                substr($a,0,$l) eq $sub_instname) {
                get_localnode_cache($V_r, $target_subtype, 1);
                $fanin_alias = substr $a, $l;
                $fanin_alias = $LOCALNODE_CACHE_r->{$target_subtype}{$fanin_alias};
                last;
            }
        }
    }
    else {
        print STDERR "Can't find fanin node $fanin_nodename in " .
                     "$fanin_subtype/$fanin_instance\n";
        print STDERR "  of target node in $target_instance.\n";
    }
    return $fanin_alias;
}

# Given instance name w/x/y/z in top-level cell TOP, determines subcell
# X which has instance name w/x and (if V_r->{ALINT_MODE} is "routed") has 
# routed=true.  Note that if V_r->{ALINT_MODE} is not "routed", then this 
# routine will always resolve only a single level of hierarchy in the given 
# instance name (i.e. in this example it will determine subcell W with instance
# name w under TOP.)  Returns (X,w/x,y/z).
#
# Uses:
#   ROUTED      - If 1, operates on routed design hierarchy
#   others      - See Cast::JavaServer::initialize_cast_server
#
my $SUBCELL_CACHE_r = {};
sub instance_lookup {
    my $V_r      = shift;
    my $subtype  = shift;
    my $instance = shift;

    if (!exists $SUBCELL_CACHE_r->{$subtype}) {
        # Look up subcells within this subtype
        $SUBCELL_CACHE_r->{$subtype} = {};
        my $cmd = "com.avlsi.tools.jflat.JFlat ";
        $cmd .= " --cast-path=$V_r->{CAST_PATH} ";
        $cmd .= " --routed" if ($V_r->{ALINT_MODE} eq "routed");
        $cmd .= " --tool=query";
        $cmd .= " --query-tasks=instance_list";
        $cmd .= " --query-no-recurse";
        $cmd .= " --cell=$subtype";
        my $instance_lines = [];
        run_cast_server_command($V_r, $cmd, undef, undef, -1, $instance_lines);
        foreach my $l (@{$instance_lines}) {
            my ($subcell, $instname) = split /\s+/, $l;
            $SUBCELL_CACHE_r->{$subtype}{$instname} = $subcell;
        }
    }
    
    foreach my $n (local_instance_names($V_r, $instance)) {
        if (exists $SUBCELL_CACHE_r->{$subtype}{$n}) {
            return ($SUBCELL_CACHE_r->{$subtype}{$n},
                    $n, substr $instance, length($n)+1);
        }
    }
    return ();
}

# Given instance name w.x.y.z, returns a list of (w w.x w.x.y w.x.y.z)
sub local_instance_names {
    my $V_r      = shift;
    my $instname = shift;
    my @names;
    my $idx = index $instname, ".";
    while ($idx != -1) {
        push @names, substr $instname, 0, $idx;
        $idx = index $instname, ".", $idx+1;
    }
    push @names, $instname;
    return @names;
}


#
# For the specified node of the specified cell, makes a best guess of
# the min/max transition delay and slew rate given an input slew rate.
# Reads the delay data from the cell's alint raw file in order to
# perform a linear interpolation.
#
#   V_r         - uses LVE_DIRS, VIEW, MODE, CORNER, VOLTAGE, TEMP, NO_SWEEPS, 
#                 SLEW_WARN_LO, SLEW_WARN_HI.
#                 CONSERVATIVE:  If set to 1, then the extrapolation will be
#                 very conservative when slew rates fall outside of
#                 the characterized range.  See Pathalyze wiki page for more 
#                 details.
#   dir         - 0:dn, 1:up
#   lve_cap_dir - Specify to prefer this results directory (for cap sweeps)
#
# If successful, returns a structure containing
#   min_delay -> <min_delay>
#   max_delay -> <max_delay>
#   min_slew  -> <min_slew>
#   max_slew  -> <max_slew>
#   cap_dir   -> 0,1            (was delay data found in lve_cap_dir?)
# Otherwise, either dies or returns all -1's.
#
sub interpolate_delay {
    my $V_r           = shift;
    my $cell          = shift;
    my $node          = shift;
    my $dir           = shift;
    my $min_in_slew   = shift;
    my $max_in_slew   = shift;
    my $fanin_node    = shift;
    my $path_name     = shift;
    my $lve_cap_dir   = shift;  # Specify to prefer this directory for cap

    my $result_r = { 
        min_delay => -1,
        max_delay => -1,
        min_slew  => $min_in_slew,
        max_slew  => $max_in_slew,
        cap_dir   => 0
    };

    my @dir_str = ( "-", "+" );

    #
    # Determine potential sources of alint delay characterization data,
    # look up delay data in prioritized order.
    # 
    print "Looking for delay results for $cell/$node$dir_str[$dir]:\n"
        if ($V_r->{DEBUG});
    my @alint_results = find_alint_results($V_r, $cell, $lve_cap_dir);
    my $dataref    = {};
    my $is_cap_dir = 0;
    my $is_d0      = 0;
    while (@alint_results) {
        my $results_file = shift @alint_results;
           $is_cap_dir   = shift @alint_results;
           $is_d0        = shift @alint_results;
       
        print "    in $results_file\n" if ($V_r->{DEBUG});
        if ($is_d0) {
            # Attempt to read directives.0 file
            $dataref = read_data_from_directives($V_r, $results_file, $node,
                                                 $dir, $fanin_node, $cell);
            $is_d0 = 1;
        }
        else {
            # Otherwise raw file
            $dataref = read_data_from_raw($results_file, $node, $dir);
            $is_d0 = 0;
        }
        if (%{$dataref}) {
            print "Found delay data for $cell/$node$dir_str[$dir]\n".
                  "  in $results_file.\n" if ($V_r->{DEBUG});
            last;
        }
    }
    if (!%{$dataref}) {
        die "FAIL: Couldn't find any delay characterization results for\n" .
            "      $cell/$node$dir_str[$dir]\n";
    }
    if (!$is_d0) {
        print STDERR "Warning: Resorting to alint.raw for " .
                     "$cell/$node$dir_str[$dir]\n";
    }
    $result_r->{cap_dir} = $is_cap_dir;

    # 
    # Set slew scaling to use to convert alint PrsTau values to ps values.
    # (alint.raw files are reported in PrsTau units)
    #
    my $SLEW_SCALING = $is_d0 ? 1.0 : log(2);

    #
    # Perform interpolation
    #
    my @slews = sort { $a <=> $b } keys %{$dataref};
    my @in_slew = ( $min_in_slew, $max_in_slew );
    foreach my $minmax (0..1) {
        my @sd = (-1,-1);
        my @ss = (-1,-1);
        my @d  = (-1,-1);
        my @os = (-1,-1);

        if (@slews >= 2) {
            #
            # Find input slew results below and above the specified slew rate
            #
            my $i;
            my @s = (-1,-1);
            for $i (0..$#slews) {
                if ($slews[$i] * $SLEW_SCALING < $in_slew[$minmax]) {
                    $s[0] = $slews[$i];
                }
                if ($slews[$i] * $SLEW_SCALING >= $in_slew[$minmax] && 
                    $s[1] == -1) {
                    $s[1] = $slews[$i];
                }
            }
            if ($s[0] == -1) {
                # input slew rate is faster than all points
                if ($slews[0] * $SLEW_SCALING - $in_slew[$minmax] 
                    > $V_r->{SLEW_WARN_LO}) {
                    print STDERR "Input slew rate " . 
                        sprintf("%g",$in_slew[$minmax]) . 
                        " is faster than all measured points (min=".
                        sprintf("%g",$slews[0] * $SLEW_SCALING) . ")\n";
                    print STDERR "  Path: $path_name\n";
                    print STDERR "  Cell: $cell\n";
                    print STDERR "  Transition: $node$dir_str[$dir]\n";
                }
                if (!$V_r->{CONSERVATIVE}) {
                    # standard linear extrapolation
                    @sd = @slews[0..1]; @ss = @slews[0..1];
                }
                elsif ($minmax == 1) {
                    # max case: need to distinguish between delay and slew
                    # due to different 2nd derivatives.
                    @sd = @slews[0..1];
                    @ss = ($slews[0],$slews[0]);
                }
                else {
                    # min case: slew is easy (linearly extrapolate).  delay
                    # is much tricker -- increase extrapolation slope by a
                    # factor of 2.  Can't be sure this is conservative in all
                    # cases, but it should be good enough.
                    @sd = ($slews[0],2*$slews[1]);
                    @ss = @slews[0..1];
                }
            }
            elsif ($s[1] == -1) {
                # input slew rate is slower than all points
                if ($in_slew[$minmax] - $slews[$#slews] * $SLEW_SCALING
                    > $V_r->{SLEW_WARN_HI}) {
                    print STDERR "Input slew rate " . 
                        sprintf("%g",$in_slew[$minmax]) . 
                        " is slower than all simulated points (max=".
                        sprintf("%g",$slews[$#slews]*$SLEW_SCALING).")\n";
                    print STDERR "  Path: $path_name\n";
                    print STDERR "  Cell: $cell\n";
                    print STDERR "  Transition: $node$dir_str[$dir]\n";
                }
                if ($minmax == 1 || !$V_r->{CONSERVATIVE}) {
                    # max case: extrapolate with final slope. Due to convexity
                    # of delay & slew curves, this is conservative.
                    @sd = @slews[$#slews-1..$#slews];
                    @ss = @slews[$#slews-1..$#slews];
                }
                else {
                    # min case: saturate the value at the final data point.
                    # Due to the monotinicity of the delay & slew curves, this
                    # is conservative.
                    @sd = ($slews[$#slews], $slews[$#slews]);
                    @ss = ($slews[$#slews], $slews[$#slews]);
                }
            }
            else {
                # No extrapolation; use the same input slew values for delay
                # and output slew interpolation.
                @sd = @s; @ss = @s;
            }
        }
        else {
            if ($V_r->{NO_SWEEPS} == 0) {
                warn "Warning: Only one slew point for cell $cell.\n";
                $sd[0] = $slews[0]; $ss[0] = $slews[0];
                $sd[1] = $slews[0]; $ss[1] = $slews[0];
            }
            else {
                # Arbitrary LVE default input slew rate
                $sd[0] = 14; $ss[0] = 14;
                $sd[1] = 14; $ss[1] = 14;
            }
        }
        if ($ss[0] == -1 || $ss[1] == -1 || $sd[0] == -1 || $sd[1] == -1) {
            die "Assertion error (1) in interpolate_delay.\n";
        }

        #
        # Look up delay and output slew rates from adjacent input slew points,
        # interpolate between them.
        #
        for my $i (0..1) {
            $d[$i]  = $dataref->{$sd[$i]}{$minmax ? "MAX" : "MIN"}[0];
            $os[$i] = $dataref->{$ss[$i]}{$minmax ? "MAX" : "MIN"}[1];
            die "Assertion error (2) in interpolate_delay.\n" 
                if ($os[$i] < 0.0);
        }
        if ($V_r->{DEBUG}) {
            print "Input slew range (delay): s0 = $sd[0], s1 = $sd[1].\n";
            print "                 (slew):  s0 = $ss[0], s1 = $ss[1].\n";
            print "Delay range: d[0] = $d[0], d[1] = $d[1]\n";
        }
        # Convert slew rates from aspice exponential time constant units
        # to real-time picoseconds
        for my $i (0..1) { 
            $sd[$i] *= $SLEW_SCALING; 
            $ss[$i] *= $SLEW_SCALING; 
        }
        my ($delay,$out_slew);
        if ($sd[0] != $sd[1]) {
            # linearly inperpolate/extrapolate
            $delay = $d[0] + ($d[1]-$d[0]) / ($sd[1]-$sd[0])
                                           * ($in_slew[$minmax]-$sd[0]);
        }
        else {
            # saturate
            $delay = $d[0];
        }
        if ($ss[0] != $ss[1]) {
            # linearly inperpolate/extrapolate
            $out_slew = $os[0] + ($os[1]-$os[0]) / ($ss[1]-$ss[0]) 
                                                 * ($in_slew[$minmax]-$ss[0]);
        }
        else {
            # saturate
            $out_slew = $os[0];
        }
        $result_r->{$minmax ? "max_delay" : "min_delay"} = $delay;
        $result_r->{$minmax ? "max_slew"  : "min_slew"}  = $out_slew;
    }
    return $result_r;
}


#
# Reads a node's delay and slew values, as a function of input tau, 
# from the specified raw file.  Returns a reference to a hash of
#
#   tau -> MIN -> [ min_delay, min_slew ]
#          MAX -> [ max_delay, max_slew ]
#
# dir:    0=>dn,  1=>up
#
sub read_data_from_raw {
    my $file = shift;
    my $node = shift;
    my $dir  = shift;

    my @raw_delay_str = ( "fast_delay_dn", "fast_delay_up",
                          "slow_delay_dn", "slow_delay_up" );
    my @raw_slew_str  = ( "fast_slew_dn", "fast_slew_up",
                          "slow_slew_dn", "slow_slew_up" );
    my @raw_delay_idx = ( 11, 12,  9, 10 );
    my @raw_slew_idx  = ( 17, 18, 15, 16 );
    my $dataref = {};

    die "Alint raw file $file doesn't exist.\n" if (! -e $file);
    open (RAW, $file) || die "Couldn't read raw file $file.\n";
    $node =~ s/\[/\\\[/g; $node =~ s/\]/\\\]/g;
    while (<RAW>) {
        my @node_data = split /\s+/, $_;
        if ($node_data[4] =~ /^node=$node$/ && $node_data[5] !~ /^leak/ &&
            $node_data[7] !~ /^bump/) {
            foreach my $minmax (0..1) {
                # Delay line for the node of interest
                my $tau = -1;
                my $delay = -1;
                my $slew = -1;
                if ($node_data[6] =~ /^tau=(\d+)$/) {
                    $tau = $1;
                }
                if ($node_data[$raw_delay_idx[2*$minmax+$dir]] =~
                    /^$raw_delay_str[2*$minmax+$dir]=([^@]+)@/) {
                    $delay = $1;
                    if ($delay eq "FAIL") {
                        warn "WARINING: FAIL delay incountered in $file.\n";
                        warn "          Using a delay of 0 ps.\n";
                        $delay = 0;
                    }
                }
                if ($node_data[$raw_slew_idx[2*$minmax+$dir]] =~
                    /^$raw_slew_str[2*$minmax+$dir]=([^@]+)@/) {
                    $slew = $1;
                    if ($slew eq "FAIL") {
                        warn "WARINING: FAIL slew rate incountered in $file.\n";
                        warn "          Using a slew rate of 0 ps.\n";
                        $slew = 0;
                    }
                }
                die "Unrecognized syntax in $file.\n" 
                    if ($delay == -1 || $slew == -1 || $tau == -1);
                
                $dataref->{$tau} = {} if (!exists $dataref->{$tau});
                if ($minmax==0) {
                    $dataref->{$tau}{MIN} = [ $delay, $slew ];
                }
                else {
                    $dataref->{$tau}{MAX} = [ $delay, $slew ];
                }
            }
        }
    }
    close RAW;
    #die "Couldn't find delay data for $node in $file.\n" if (!%$dataref);
    return $dataref;
}

my $C_r = {};   # Characterization cache
sub lookup_characterization_data {
    my $V_r        = shift;
    my $rundir     = shift;
    my $node       = shift;
    my $dir        = shift;     # 0=>dn,  1=>up
    my $fanin_node = shift;
    my @dir_str    = ( "-", "+" );

    my $dataref;
    my $transition = $node . $dir_str[$dir];
    if (!exists $C_r->{$rundir} || !exists $C_r->{$rundir}{$transition} ||
        !exists $C_r->{$rundir}{$transition}{$fanin_node}) {
        $dataref = read_data_from_directives($V_r, $rundir, $node, $dir,
                                             $fanin_node);
    }
    else {
        $dataref = $C_r->{$rundir}{$transition}{$fanin_node};
    }
    return $dataref;
}
    
#
# Reads a node's delay and slew values, as a function of input slew
# rate and input aggressor node from the specified directives.0 file.
# Returns a reference to a hash of
#
#   in_slew -> MIN -> [ min_delay, min_slew ]
#              MAX -> [ max_delay, max_slew ]
#
# Note: if fanin_node isn't defined or if PIN_TO_PIN is set to 0, then
# min & max will be calcualted over all fan-in nodes.
#
sub read_data_from_directives {
    my $V_r        = shift;     # Uses VERBOSE  
    my $file       = shift;     # full path to directives.0 file
    my $node       = shift;
    my $dir        = shift;     # 0=>dn,  1=>up
    my $fanin_node = shift;
    my $cell       = shift;     # just for user output

    my @dir_str    = ( "-", "+" );
    my $transition = $node . $dir_str[$dir];

    #
    # Read directives.0 file only if we haven't read it already
    #
    (my $rundir = $file) =~ s/\/directives\.0$//;
    if (!exists $C_r->{$rundir} || !exists $C_r->{$rundir}{$transition}) {
        # Process directives.0 file
        $C_r->{$rundir} = {} if (!exists $C_r->{$rundir});
        open (DIR, $file) || die "Couldn't read directives file $file.\n";
        # cache all nodes that have the same prefix as the specified node up
        # to the first [.
        my $node_prefix = $node;
        $node_prefix =~ s/\[.*$//;
        $node_prefix =~ s/\./\\\./g;
        my $directive_state = 0;
        my @in_slew = ();
        my @delay1= (); my @delay2 = ();
        my @slew1 = (); my @slew2  = ();
        my ($c_transition, $c_fanin_node);
        while (<DIR>) {
            s/^\s*//;
            if ($directive_state==0 &&
                /^measured_delay\(\{($node_prefix[^-+]*[-+]),([^-+]+)[-+]\}\)/ ) {
                $c_transition = $1;
                $c_fanin_node = $2;
                $directive_state = 1;
            }
            elsif ($directive_state==1 && /^\{ 0 \}/) {
                $directive_state = 2;
            }
            elsif ($directive_state==1 && /^\{ 1 \}/) {
                die "Directive type of file $file is unsupported.\n";
            }
            elsif ($directive_state==2 && /^\{\s*(.*)\s*\}/) {
                @in_slew = split /, /, $1;
                $directive_state = 3;
            }
            elsif ($directive_state==3 && /^\{\s*(.*)\s*\}/) {
                @delay1 = split /, /, $1;
                $directive_state = 4;
            }
            elsif ($directive_state==4 && /^\{\s*(.*)\s*\}/) {
                @slew1 = split /, /, $1;
                $directive_state = 5;
            }
            elsif ($directive_state==5 && /^\{\s*(.*)\s*\}/) {
                @delay2 = split /, /, $1;
                $directive_state = 6;
            }
            elsif ($directive_state==6 && /^\{\s*([^\}]*)\s*\}/) {
                @slew2 = split /, /, $1;
                $directive_state = 0;

                # Workaround for occasional bug in extract flow
                $c_fanin_node =~ s/:$//;

                $C_r->{$rundir}{$c_transition} = {} 
                    if (!exists $C_r->{$rundir}{$c_transition});
                $C_r->{$rundir}{$c_transition}{$c_fanin_node} = {}
                    if (!exists $C_r->{$rundir}{$c_transition}{$c_fanin_node});

                foreach my $i (0..$#in_slew) {
                    my ($d, $s);
                    $d = ($delay2[$i] < $delay1[$i]) ? $delay2[$i] 
                                                     : $delay1[$i];
                    $s = ($slew2[$i] < $slew1[$i]) ? $slew2[$i] : $slew1[$i];
                    $C_r->{$rundir}{$c_transition}{$c_fanin_node}{$in_slew[$i]}
                        = { MIN => [ $d, $s ] };
                    $d = ($delay2[$i] < $delay1[$i]) ? $delay1[$i] 
                                                     : $delay2[$i];
                    $s = ($slew2[$i] < $slew1[$i]) ? $slew1[$i] : $slew2[$i];
                    $C_r->{$rundir}{$c_transition}{$c_fanin_node}{$in_slew[$i]}
                        {MAX} = [ $d, $s ];
                    if (check_for_bad_directive_values($C_r->{$rundir}
                        {$c_transition}{$c_fanin_node}{$in_slew[$i]},
                        $rundir, $c_transition, $c_fanin_node, 
                        $in_slew[$i])) {
                         delete $C_r->{$rundir}{$c_transition}{$c_fanin_node}
                                      {$in_slew[$i]};
                    }
                }
            }
            elsif ($directive_state != 0) {
                die "Couldn't parse directives file $file.\n";
            }
        }
        close DIR;
    }

    # Performing pin-to-pin?
    if ($V_r->{PIN_TO_PIN} && defined $fanin_node) {
        # Determine the correct alias for the fan-in node
        my $asp_fanin_node;
        my $nodedir = "$rundir/$node";
        for (my $bin = 0; -d "$rundir/alint.bin.$bin"; $bin++) {
            if ( -d "$rundir/alint.bin.$bin/$node") {
                $nodedir = "$rundir/alint.bin.$bin/$node";
                last;
            }
        }
        if (-e $nodedir && opendir (NODE_DIR, $nodedir)) {
            my @names_files = grep { $_ =~ /\.names$/ } readdir NODE_DIR;
            closedir NODE_DIR;
            if (@names_files) {
                my $names_file = "$nodedir/$names_files[0]";
                $asp_fanin_node = resolve_aspice_fanin_alias($V_r, $names_file, 
                                                             $fanin_node);
            }
        }
        if (!defined $asp_fanin_node || $asp_fanin_node eq "") {
            print STDERR "Couldn't find $nodedir names file, " .
                "relying on CAST fanin alias.\n";
            $asp_fanin_node = $fanin_node;
        }
        if (exists $C_r->{$rundir}{$transition}{$asp_fanin_node}) {
            my $ret_r = $C_r->{$rundir}{$transition}{$asp_fanin_node};
            print STDERR "Success $asp_fanin_node$dir_str[1-$dir] -> " .
                "$transition from $file.\n" if ($V_r->{DEBUG});
            return $ret_r;
        }
        elsif ($V_r->{DEBUG}) {
           print STDERR "Couldn't find $asp_fanin_node$dir_str[1-$dir] -> " .
              "$transition in $file.\n";
        }
    }
    elsif (exists $C_r->{$rundir}{$transition}) {
        #
        # Aggregate over fanin for worst-case min/max delay & slew
        # NOTE: This code probably not yet tested
        #
        print STDERR "Warning: No fan-in node for $cell/$transition.\n" .
                     "         Aggregating min/max over entire fan-in set.\n";
        my $ret_r = {};
        foreach my $fanin (keys %{$C_r->{$rundir}{$transition}}) {
            foreach my $slew (keys %{$C_r->{$rundir}{$transition}{$fanin}}) {
                if (!exists $ret_r->{$slew}) {
                    $ret_r->{$slew} = { 
                        MIN => [  1e6,  1e6 ],
                        MAX => [ -1e6, -1e6 ]
                    };
                }
                my $fds = $C_r->{$rundir}{$transition}{$fanin}{$slew};
                foreach my $ds (0, 1) {
                    if ($fds->{MIN}[$ds] < $ret_r->{$slew}{MIN}[$ds]) {
                        $ret_r->{$slew}{MIN}[$ds] = $fds->{MIN}[$ds];
                    }
                    if ($fds->{MAX}[$ds] > $ret_r->{$slew}{MAX}[$ds]) {
                        $ret_r->{$slew}{MAX}[$ds] = $fds->{MAX}[$ds];
                    }
                }
            }
        }
        print STDERR "Success: All fan-in delays/slews aggregated for ".
                     "$transition from $file.\n" if ($V_r->{DEBUG});
        return $ret_r;
    }
    else {
        print STDERR "Couldn't find delay/slew data for $transition in ".
                     "$file.\n";
    }
    return {};
}

sub check_for_bad_directive_values {
    my $delay_slew_r = shift;
    my $rundir       = shift;
    my $transition   = shift;
    my $fanin_node   = shift;
    my $in_slew      = shift;

    my $bad = 0;
    # No longer worry about negative delays; they are measured correctly now
    foreach my $sd (1) {
        if ($delay_slew_r->{MIN}[$sd] < 0.0 || 
            $delay_slew_r->{MAX}[$sd] < 0.0) {
            $bad = 1;
            print STDERR "Warning: Discarding negative " . 
                ($sd ? "slew rates" : "delays") .
                " observed in directives.0.\n" .
                "  directory:  $rundir\n" .
                "  transition: $fanin_node;$transition\n" .
                "  input slew: $in_slew\n" .
                "  min:        " .
                sprintf("%g", $delay_slew_r->{MIN}[$sd]) . "\n" .
                "  max:        " .
                sprintf("%g", $delay_slew_r->{MAX}[$sd]) . "\n";
        }
    }
    return $bad;
}


# 
# Resolves a set of lve directories that may have alint results for this
# cell.  Returns a list of 
#
#   ( alint_file1,        - first candidate alint directives.0 or alint.raw file
#     is_cap_dir,         - is this lve_cap_dir?
#     is_directives0,     - is alint_file a directives.0 file?
#     alint_file2,        - second candidate alint results file
#     ... )
#
# order is prioritized; index 0 is most preferred match (either it is
# lve_cap_dir, it has a directives.0 file whereas others only have alint.raw,
# or it appears earlier in lve_dirs.)
#
sub find_alint_results {
    my $V_r         = shift;
    my $cell        = shift;
    my $lve_cap_dir = shift;

    my @alt_layout_view_names = ( "lvsclean", "layout_tag", "layout_pg", "layout" );
    (my $cell_path = $cell) =~ s/\./\//g;

    my @d0_results = ();
    my @raw_results = ();
    foreach my $lve_dir ($lve_cap_dir, @{$V_r->{LVE_DIRS}}) {
        my $cell_dir = "$lve_dir/$cell_path";
        my @views=();
        @views = ( $V_r->{VIEW} ) if defined $V_r->{VIEW};
        push @views, @alt_layout_view_names
            if (! defined ($V_r->{VIEW}) or $V_r->{VIEW} eq "layout");
        foreach my $view (@views) {
            my $alint_dir = "$cell_dir/$view/$V_r->{MODE}/alint/" .
                            "$V_r->{CORNER}/$V_r->{VOLTAGE}V/$V_r->{TEMP}C";
            if (-f "$alint_dir/directives.0") {
                push @d0_results, ( "$alint_dir/directives.0",
                                    ($lve_dir eq $lve_cap_dir ? 1 : 0), 1 );
            }
            if (-f "$alint_dir/alint.raw") {
                push @raw_results, ( "$alint_dir/alint.raw",
                                    ($lve_dir eq $lve_cap_dir ? 1 : 0), 0 );
            }
        }
    }
    return ( @d0_results, @raw_results );
}


#
# Given an alint names file for a particular target node and a Cast name
# of the input fan-in node, determines what name was considered canonical 
# in the aspice run.  This step is required due to annoying gate terminal 
# aliases that don't appear in the Cast.
#
my $F_r = {};
sub resolve_aspice_fanin_alias {
    my $V_r        = shift;
    my $names_file = shift;
    my $fanin_node = shift;

    if (exists $F_r->{$names_file}) {
        if (exists $F_r->{$names_file}{$fanin_node}) {
            return $F_r->{$names_file}{$fanin_node};
        }
    }
    else {
        $F_r->{$names_file} = {};
    }

    (my $fanin_match = $fanin_node) =~ s/\[.*$//;
    $fanin_match =~ s/\./\\\./g;
    open (NAMES, $names_file) || die "Couldn't read names file $names_file.\n";
    my $cname = <NAMES>;    # First line is 'time'
    $cname = "";
    while (<NAMES>) {
        chomp;
        if (/^\s*$/) {
            $cname = "";
        }
        elsif ($cname eq "" && /^[^=]+$/) {
            # Workaround for occasional bug in extract flow (leaves ':' 
            # appended to net names
            ($cname = $_) =~ s/:$//;
            if ($cname =~ /^$fanin_match/) {
                $F_r->{$names_file}{$cname} = $cname;
            }
            last if ($cname eq $fanin_node);
        }
        elsif ($cname ne "" && /^=(.*)$/) {
            my $alias = $1;
            if ($alias =~ /^$fanin_match/) {
                $F_r->{$names_file}{$alias} = $cname;
            }
            last if ($alias eq $fanin_node);
        }
        else {
            print STDERR "Warning: Strange line in $names_file:\n";
            print STDERR "  $_\n";
        }
    }
    close NAMES;
    return $F_r->{$names_file}{$fanin_node};
}


1;
