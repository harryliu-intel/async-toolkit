# $Id$
# $DateTime$

package Cast::NodeAliasing;

BEGIN {
    use Exporter;
    our @ISA = qw(Exporter);
    our @EXPORT = qw(
        &query_alias_set
        &is_aliased
    );
}

use strict;
use warnings;
use IPC::Open2;
use Cast::JavaServer;

my $LN_CACHE_r = {};


#
# Queries all aliases of the cell's local and port nodes.  
# Returns a structure of the form
#
#   <canonical_node> -> [ <alias1>, <alias2>, ... ]
#
# Needs the following variables set in V_r:
#   PACKAGE_ROOT, CAST_PATH, MAX_HEAP_SIZE, DEBUG
#
# If omit_local_nodes or omit_port_nodes is set to 1, the corresponding
# set of nodes will not be included in the alias set.
#
sub query_alias_set {
    my $V_r              = shift;
    my $cell             = shift;
    my $omit_local_nodes = shift;   # optional
    my $omit_port_nodes  = shift;   # optional

    $omit_local_nodes = 0 if (!defined $omit_local_nodes);
    $omit_port_nodes  = 0 if (!defined $omit_port_nodes);

    #
    # Check if the alias set is in the cache, return it if so
    #
    if (exists $LN_CACHE_r->{$cell} &&
        $LN_CACHE_r->{$cell}{omit_local_nodes}==$omit_local_nodes &&
        $LN_CACHE_r->{$cell}{omit_port_nodes}==$omit_port_nodes) {
        return $LN_CACHE_r->{$cell}{set};
    }

    #
    # Not in cache, so query it
    #
    $LN_CACHE_r->{$cell} = {
        omit_local_nodes => $omit_local_nodes,
        omit_port_nodes  => $omit_port_nodes,
        set => {}
    };
    my $out_lr = [];
    my $cmd = "--cast-path=$V_r->{CAST_PATH} --no-header --cell=$cell ";

    #
    # Query local node aliases
    #
    unless (defined $omit_local_nodes && $omit_local_nodes==1) {
        query_cast_server($V_r, "$cmd --task=local_nodes=all_aliases", 
                          $out_lr, 1);
    }

    #
    # Query port node aliases
    #
    unless (defined $omit_port_nodes && $omit_port_nodes==1) {
        query_cast_server($V_r, "$cmd --task=external_nodes=all_aliases:im",
                          $out_lr, 1);
    }

    # Now process combined output 
    #
    foreach my $line (@{$out_lr}) {
        if ($line =~ /^([^\s]+)\s?/) {
            my @aliases = split /=/, $1;
            my $cnode = shift @aliases;
            $LN_CACHE_r->{$cell}{set}{$cnode} = \@aliases;
        }
    }

    return $LN_CACHE_r->{$cell}{set};
}


#
# Determines whether one node of the parent cell is aliased to another.
# Will call query_alias_set behind the scenes, so many applications can
# get away with only using this routine.  See query_alias_set for required
# V_r variables.
#
# By default, both local and port nodes are considered.  Specify
# omit_local_nodes or omit_port_nodes to pare down the node set.
#
# Note: Performance is improved if one of the specified nodes is a
#       canonical name.
#
# Returns 0 if either node is not a valid alias within the cell.
#
sub is_aliased {
    my $V_r   = shift;
    my $cell  = shift;
    my $node1 = shift;
    my $node2 = shift;
    my $omit_local_nodes = shift;   # optional
    my $omit_port_nodes  = shift;   # optional

    my $aliases_r = query_alias_set($V_r, $cell, $omit_local_nodes,
                                    $omit_port_nodes);

    my $c1 = exists $aliases_r->{$node1};
    my $c2 = exists $aliases_r->{$node2};
    if ($c1 || $c2) {
        return 1 if ($node1 eq $node2);
        return 0 if ($c1 && $c2);
        my $cnode = $c1 ? $node1 : $node2;
        my $anode = $c1 ? $node2 : $node1;
        return scalar(grep {$_ eq $anode} @{$aliases_r->{$cnode}});
    }
    else {
        foreach my $cnode (keys %{$aliases_r}) {
            if (grep {$_ eq $node1} @{$aliases_r->{$cnode}} &&
                grep {$_ eq $node2} @{$aliases_r->{$cnode}}) {
                return 1;
            }
        }
        return 0;
    }
}


1;
