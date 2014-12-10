#
# Netlist
#
# Commands for reading & manipulating leaf cell transistor/gate netlists
#
#

package Supersize::Netlist;

BEGIN {
    use Exporter;
    our @ISA = qw(Exporter);
    our @EXPORT = qw(
        &captally
        &query_netlist_props
    );
    our @EXPORT_OK = qw( 
        &get_module_data
    );
}

use strict;
use Supersize::SubtypeSpace;
use Supersize::ModifySubtypes;
use Supersize::Util;
use Supersize::TypeUtil;
use Supersize::JavaUtil;
use Term::ANSIColor;

sub get_module_data {
  return { 
    NAME => "Netlist",
    DESC => "Commands for querying and manipulating leaf cell transistor " .
            "netlists.",
    COMMANDS => {
    "captally" => {
        SUBREF => \&captally_cmd,
        USAGE  => "captally " . underline("node") . " | " .
                                underline("node-list"),
        DESC   =>
          "Returns a report of the total capacitance on " .underline("node").
          " or on each node of " .underline("node-list"). " within TOP.",
        GLOBALS => {
          WORK_DIR => { REQUIRED => 1 },
          CAST_DIR => { REQUIRED => 1 },
          SPEC_DIR => { REQUIRED => 1 },
          PDK_ROOT => { REQUIRED => 1 } } },
    "graph_netlist" => {
        SUBREF => \&graph_netlist,
        USAGE  => "graph_netlist [\"args\"] [".underline("cell-list")."]",
        DESC   =>
            "Generate *.ps2 files for netlists of ".underline("cell-list").".",
        GLOBALS => {
          WORK_DIR => { REQUIRED => 1 },
          CAST_DIR => { REQUIRED => 1 },
          SPEC_DIR => { REQUIRED => 1 },
          PDK_ROOT => { REQUIRED => 1 } } }
    }
  };
}

#
# captally
#
sub captally_cmd {
    my $SS_r = shift;

    # parse args
    my $cap_mode = 2;       # both wire+gate cap
    my $node;
    my @node_list;
    while (num_args(\@_)) {
        my ($type, $val) = shift_next_arg(\@_);
        if ($type == $TYPE_SCALAR) {
            if ($val eq "--gate") {
                $cap_mode = 0;
                command_die($SS_r, "Sorry, --gate isn't supported.");
            }
            elsif ($val eq "--wire") {
                $cap_mode = 1;
                command_die($SS_r, "Sorry, --wire isn't supported.");
            }
            elsif ($val =~ /^--/) {
                command_die($SS_r, "Unrecognized option '$val'.");
            }
            elsif (defined $node || @node_list) {
                command_die($SS_r, "Too many arguments to captally.");
            }
            else {
                $node = $val;
            }
        }
        elsif ($type == $TYPE_LIST) {
            if (@node_list || defined $node) {
                command_die($SS_r, "Too many arguments to captally.");
            }
            push @node_list, @{$val};
        }
        else {
            command_die($SS_r, "Bad arguments to captally.");
        }
    }
    if (defined $node) {
        push @node_list, $node;
    }

    # Check to be sure TOP is a leaf cell
    my $wdir = get_work_dir($SS_r);
    my $dir = look_for_subtype($SS_r->{GS}{TOP}, 
                    [$SS_r->{GS}{SPEC_DIR}, "$wdir/cast"]);
    command_die($SS_r, "Couldn't locate subtype file for $SS_r->{GS}{TOP}.")
        if (!defined $dir);

    # Generate a CDL file
    my $cdl_file = "$wdir/$SS_r->{GS}{TOP}.cdl";
    my ($cmd, $use_server) = set_java_cmd($SS_r, "cast2cdl", $LOCAL_JOB);
    #$cmd .= " --flatten \\\n";
    $cmd .= "'--cast-path=" . get_cast_path($SS_r) . "' \\\n";
    $cmd .= " --cell=$SS_r->{GS}{TOP} \\\n";
    $cmd .= " --output=$cdl_file \\\n";
    if ($use_server) {
        # Note: not yet tested
        run_cast_server_command($SS_r, $cmd, undef, undef, -1);
    }
    else {
        supersize_system($SS_r, $cmd, $LOCAL_JOB);
    }

    # Tally capacitances
    my $capmap_r = captally($SS_r, $SS_r->{GS}{TOP}, 
                            $cdl_file, undef, @node_list);

    # Generate report
    foreach my $n (@node_list) {
        print "$n " unless (defined $node);
        print sprintf("%.5g", $capmap_r->{$n}) . "\n";
    }

    # cleanup
    unlink $cdl_file unless ($SS_r->{GS}{DEBUG});
}

# Graph netlist of a list of cells.  If none specified, graph all leaf
# cells.  Uses working graph_netlist, not broken show_netlist.
sub graph_netlist {
    my $SS_r = shift;
    my $cells_lr = [];
    my $wdir = get_work_dir($SS_r);
    my $args = "--format=ps2";

    # parse args
    while (num_args(\@_)) {
        my ($type, $arg) = shift_next_arg(\@_);
        if ($type == $TYPE_SCALAR) {
            $args = $arg;
        }
        elsif ($type == $TYPE_LIST && @{$cells_lr}) {
            command_die($SS_r, "Too many cell lists specified.");
        }
        elsif ($type == $TYPE_LIST) {
            $cells_lr = $arg;
        }
        else {
            command_die($SS_r, "Unrecognized argument specified.");
        }
    }

    # Determine cell list if one wasn't specified
    if (!@{$cells_lr}) {
        my $cmd = "--cell=$SS_r->{GS}{TOP} --task=subcells --filter=leaf";
        query_cast_server($SS_r, $cmd, $cells_lr, 1);
    }

    # graph them with graph_netlist
    foreach my $cell (@{$cells_lr}) {
        my $cmd = "graph_netlist " .
            "'--cast-path=" . get_cast_path($SS_r) . "' " .
            "$args '$cell'";
        supersize_system($SS_r, $cmd, $LOCAL_JOB);
    }
}


############################## Utility Functions ##############################

#
# For each node 'n' in node_list, returns a map reference
#   n -> total_cap
# All capacitances are in Farads. Requires a flattened CDL file.
#
sub captally {
    my $SS_r      = shift;
    my $cell      = shift;
    my $cdl_file  = shift;
    my $lib_file  = shift;
    my @node_list = @_;

    # initialization
    my $cap_map_r = {};
    foreach my $n (@node_list) {
        $cap_map_r->{$n} = 0.0;
    }
    my $capnum = 0;
    my $mosnum = 0;

    # Call Aubrey's captally
    my $cmd = "fulcrum captally \\\n";
    $cmd .= "--fulcrum-pdk-root=" . $SS_r->{GS}{PDK_ROOT} . " \\\n";
    $cmd .= "'--cell=$cell' \\\n";
    $cmd .= "'--cdl=$cdl_file' \\\n";
    $cmd .= "'--library=$lib_file' \\\n" if (defined $lib_file);
    my $cap_file = "${cdl_file}.captally";
    supersize_system($SS_r, $cmd, $LOCAL_JOB, undef, $cap_file);
    if ($!) {
        command_die($SS_r, "Error running captally: $!");
    }

    # Read node output
    open (CAPS, $cap_file) || command_die($SS_r, "Error running captally.");
    while (<CAPS>) {
        my ($node, $cap) = split /\s+/, $_;
        if (exists $cap_map_r->{$node}) {
            $cap_map_r->{$node} = $cap;
        }
    }
    close CAPS;
    unlink $cap_file unless ($SS_r->{GS}{DEBUG});

    return $cap_map_r;
}

# Unfortunately, there's no cast query task for obtaining counts of specific
# gate/stack types within leaf cells, so we need to look at the subtype files.
#
# For each cell listed in @{$cells_lr}, sets various properties in the 
# map reference $d_r->{cell}{property}.  Sets the following properties:
#
#   stack_fet_cnt  - Number of transistors in stacks
#   stack_cnt      - Number of stacks
#   precharge_cnt  - Number of precharge stacks (included in stack_cnt)
#   gate_cnt       - Number of gates
#
sub query_netlist_props {
    my $SS_r     = shift;
    my $wdir     = shift;
    my $cells_lr = shift;
    my $d_r      = shift;   # Map of cell->{property}->value (return values)

    foreach my $c (@{$cells_lr}) {
        $d_r->{$c}{gate_cnt} = 0;
        $d_r->{$c}{stack_cnt} = 0;
        $d_r->{$c}{stack_fet_cnt} = 0;
        $d_r->{$c}{precharge_cnt} = 0;
        my $file = fqcn_to_file("$wdir/cast", $c);
        if (!-e $file && exists $SS_r->{GS}{SPEC_DIR}) {
            $file = fqcn_to_file($SS_r->{GS}{SPEC_DIR}, $c)
        }
        open (SUB, $file) || command_die($SS_r, "Couldn't read $file.");
        my $in_netlist = 0;
        while (<SUB>) {
            if (/^\s*netlist\s+{/) {
                $in_netlist = 1;
            }
            elsif ($in_netlist) {
                if (/\/\s+stack\.[NP]MOS_CHAIN_(\d+)/) {
                    $d_r->{$c}{stack_cnt}++;
                    $d_r->{$c}{stack_fet_cnt} += $1;
                }
                elsif (/\/\s+stack\.PRECHARGE/) {
                    $d_r->{$c}{precharge_cnt}++;
                    $d_r->{$c}{stack_cnt}++;
                    $d_r->{$c}{stack_fet_cnt}++;
                }
                elsif (/\/\s+gate\./) {
                    $d_r->{$c}{gate_cnt} += 1;
                }
                #$d_r->{$c}{stack_cnt}++ if (/\/\s+stack\./);
                $in_netlist = 0 if (/^\s*}/);
            }
        }
        close SUB;
    }
}

1;
