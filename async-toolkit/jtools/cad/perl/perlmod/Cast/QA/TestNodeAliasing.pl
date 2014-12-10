#!/usr/bin/perl -w

BEGIN {
    # Must be run from current directory
    push @INC, "../..";
}

use strict;
use Cast::NodeAliasing;

my $java_package_root = "/home/local/common/fulcrum/tools/lve/104981";

sub usage_exit {
    my $str = shift;
    print $str if (defined $str);
    print "Usage: TestNodeAliasing.pl [--brief] [--cast-path=<base_cast_dir>]\n";
    print "                           [--node1=<n1> --node2=<n2>]\n";
    print "                           [--omit-port-nodes] [--omit-local-nodes]\n";
    exit;
}

#
# Runtime variable structure, passed to all NodeAliasing and 
# JavaServer functions
#
my %VARS = (
    PACKAGE_ROOT => $java_package_root,
    CAST_PATH    => "$ENV{HOME}/hw/cast",
    HEAP_SIZE    => "256M",
    DEBUG        => 0
);

my $brief = 0;
my $omit_port_nodes = 0;
my $omit_local_nodes = 0;
my ($node1, $node2);

while (@ARGV) {
    if ($ARGV[0] =~ /^--cast-path=(.*)$/) {
        $VARS{CAST_PATH} = $1;
    }
    elsif ($ARGV[0] eq "--brief") {
        $brief = 1;
    }
    elsif ($ARGV[0] =~ /^--node1=(.*)$/) {
        $node1 = $1;
    }
    elsif ($ARGV[0] =~ /^--node2=(.*)$/) {
        $node2 = $1;
    }
    elsif ($ARGV[0] eq "--omit-port-nodes") {
        $omit_port_nodes = 1;
    }
    elsif ($ARGV[0] eq "--omit-local-nodes") {
        $omit_local_nodes = 1;
    }
    elsif ($ARGV[0] eq "--debug") {
        $VARS{DEBUG} = 1;
    }
    else {
        usage_exit("Unrecognized argument $ARGV[0].\n");
    }
    shift;
}

if (defined $node1 && !defined $node2 ||
    !defined $node1 && defined $node2) {
    usage_exit("node1 and node2 must both be specified.\n");
}

my $cell = "lib.math.add.ADD_32";


#
# Check if node1 and node2 are aliases, if they are specified
#
if (defined $node1) {
    my $a = is_aliased(\%VARS, $cell, $node1, $node2, $omit_local_nodes,
                       $omit_port_nodes);
    print "Nodes $node1 and $node2 are" . ($a ? "" : " NOT") . " aliases.\n\n";
}

#
# Test query_alias_set
# (Note: this should return immediately if a node alias comparison
# is done, due to caching.)
#
my $alias_set_r = query_alias_set(\%VARS, $cell, $omit_local_nodes, 
                                  $omit_port_nodes);

if (!$brief) {
    print "Alias set of $cell:\n";
    foreach my $cnode (sort keys %{$alias_set_r}) {
        print "$cnode:";
        foreach my $alias (sort @{$alias_set_r->{$cnode}}) {
            print " $alias";
        }
        print "\n";
    }
}
else {
    print "Completed querying alias set for $cell.\n";
}
