#!/usr/intel/bin/perl -w

# Translate CDL syntax netlist to SP or LGF syntax for LayGen.  Folds
# wide transistors.  Detects and declares input, output, inout pins.
#
# Copyright 2019 Intel
# Authors: Andrew Lines

use FileHandle;
use IPC::Open2;
use POSIX;
use Data::Dumper qw(Dumper);

# Output record separator
$" = " ";

# options and defaults
my $lgf      = 0;        # emit LGF file format
my $grid     = 1e-10;    # convert to Laygen units for LGF
my $length   = 14e-9;    # transistor length
my $gridW    = 30e-9;    # rounding grid for width
my $maxNmosW = 2*$gridW; # maximum fold width for NMOS for G1I
my $maxPmosW = 2*$gridW; # maximum fold width for PMOS for G1I
my $top;

# set up node renaming to match Laygen expectations
my $node_num=1;
my %map_node;

# usage banner
sub usage() {
    die "Usage: $0\n" .
        " [--lgf=$lgf]\n" .
        " [--top=cellname]\n" .
        " [--grid=$grid]\n" .
        " [--length=$length] [--gridW=$gridW]\n" .
        " [--maxNmosW=$maxNmosW] [--maxPmosW=$maxPmosW]\n" .
        " <top_cell> <in.cdl> <out.lgf|out.sp>\n";
}

# parse arguments
while (defined $ARGV[0] && $ARGV[0] =~ /^--(.*)=(.*)/) {
    $flag = $1;
    $val = $2;
    if    ($flag eq "lgf")      { $lgf = $val; }
    elsif ($flag eq "top")      { $top = $val; }
    elsif ($flag eq "length")   { $length = $val; }
    elsif ($flag eq "grid")     { $grid = $val; }
    elsif ($flag eq "gridW")    { $gridW = $val; }
    elsif ($flag eq "maxNmosW") { $maxNmosW = $val; }
    elsif ($flag eq "maxPmosW") { $maxPmosW = $val; }
    else { usage(); }
    shift @ARGV;
}
@ARGV == 2 or usage();
my $f_in  = "$ARGV[0]";
my $f_out = "$ARGV[1]";
$grid=1 unless ($lgf); # SP uses SI units

# report a fatal error
sub error_msg {
    die "$0: $f_in, line $.: $_[0]\n";
}

# Translate top cell name to library format
my $newtop = $f_out;
if ($lgf) { $newtop =~ s/\.lgf$//g; }
else      { $newtop =~ s/\.sp$//g; }

# Do linewise translation of CDL to SP/LGF
open IN,  "<$f_in"  or die "Can't read $f_in\n";
$line = <IN>;
$. = 0;
my $cell;
my @out;
my %portnodes;
my %localnodes;
my %g_nodes;
my %sd_nodes;
my %b_nodes;
my $num_mos = 0;
while ($line) {
    $next_line = <IN>;
    while (defined $next_line && $next_line =~ s/^\+/ /) {
        chomp $line;
        $line .= $next_line;
        $next_line = <IN>;
    }
    my $full = $line;
    if ($line =~ s/^\.SUBCKT\s+//i || $line =~ s/^\.SUBCIRCUIT\s+//i) {

        # clear everything
        if (!defined($top)) {
            @out=();
            %portnodes=();
            %localnodes=();
            %g_nodes=();
            %sd_nodes=();
            %b_nodes=();
            $num_mos=0;
        }

        # Begin Subcircuit Definition
        my @parameters = ();

        $line =~ s/(\S+)\s+//;
        $cell = $1;
        while ($line =~ s/(\S+)\s*//) {
            $arg = $1;
            if ($arg =~ /(\S+)=(\S+)/) {
                # Ignore parameters
            } else {
                # Node argument
                my $net=rename_net($arg);
                $portnodes{$net}=1 if (!defined($top) || $cell eq $top);
            }
        }

    } elsif ($line =~ s/^\.ENDS//i) {

        $cell = "";

    } elsif ($line =~ s/^M//i) {

        # MOSFET
        my %parameters = ();
        my @nodes = ();

        $line =~s/^(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)//
            or error_msg "Missing arguments in mosfet: $full";
        my $name   = $1;
        my $type   = $6;
        my $drain  = $2;
        my $gate   = $3;
        my $source = $4;
        my $bulk   = $5;
        $drain  = rename_net($drain);
        $gate   = rename_net($gate);
        $source = rename_net($source);
        $bulk   = rename_net($bulk);
        if (!defined($top) || $cell eq $top) {
            $localnodes{$drain}=1;
            $localnodes{$source}=1;
            $localnodes{$gate}=1;
            $localnodes{$bulk}=1;
            $g_nodes{$gate}=1;
            $sd_nodes{$drain}=1;
            $sd_nodes{$source}=1;
            $b_nodes{$bulk}=1;
        }

        # create parameter list in aspice order
        while ($line =~ s/(\S+)=//) {
            my $p = lc($1);
            my $v;
            if ($line =~ s/'([^']*)'\s+//) {
                $v = $1;
            } elsif ($line =~ s/(\S+)\s+//) {
                $v = $1;
            }
            $parameters{$p} = $v;
        }

        # subsitute some constants
        $parameters{"w"} =~ s/DIFF_PITCH/$gridW/g;
        $parameters{"w"} = eval($parameters{"w"});
        $parameters{"l"} =~ s/TRANSISTOR_LENGTH/$length/g;
        $parameters{"l"} = eval($parameters{"l"});
        my $x = $parameters{"w"};
        my $y = $parameters{"l"};
        $x *= $parameters{"m"} if (defined($parameters{"m"}));

        # compute folds
        my $w = POSIX::floor($parameters{"w"}/$gridW+0.5);
        my $maxW = $maxPmosW;
        $maxW = $maxNmosW if ($type =~ /^n/);
        $maxW = POSIX::floor($maxW/$gridW+0.5);
        my $folds = POSIX::ceil($w/$maxW);
        my $small = 0;
        my $mod = $w%$maxW;
        if ($mod!=0) { $small = $maxW - $mod; } # number of small folds

        # emit
        for ($f=0; $f<$folds; $f++) {
            if ($folds>1) { $parameters{"w"} = ($maxW-($f<$small)) * $gridW; }
            $w2 = $parameters{"w"}/$grid;
            $l2 = $parameters{"l"}/$grid;
            $type =~ /^(.)/;
            my $tc = $1;
            if (!defined($top) || $cell eq $top) {
                if ($lgf) {
                    push @out, "Device M${num_mos} nets=[ $drain $gate $source $bulk ] " .
                        "type=$tc model=$type w=$w2 l=$l2\n";
                } else {
                    push @out, "M${num_mos} $drain $gate $source $bulk " .
                        "$type w=${w2} l=${l2} m=1\n";
                }
                $num_mos++;
            }
        }

    } elsif ($line =~ s/^X//i) {
        # subcells not supported
        error_msg "cdl2sp expects flat transistors";
    } elsif ($line =~ m/^\*/) {
        # comment line, do nothing
    } elsif ($line =~ s/^\s*\n//i ) {
        # empty line with space, do nothing
    } else {
        # unknown line, error
        error_msg "Unknown line type: $line";
    }
    $line = $next_line;
}
close IN;

# classify nodes
my @inout;
my @input;
my @output;
foreach my $node (sort keys %portnodes) {
    if (defined($b_nodes{$node})) {} # power supplies are defined in arch file
#   elsif (defined($g_nodes{$node}) && defined($sd_nodes{$node})) { push @inout, $node; }
    elsif (defined($sd_nodes{$node})) { push @output, $node; }
    elsif (defined($g_nodes{$node})) { push @input, $node; }
}

# output
open OUT, ">$f_out" or die "Can't write $f_out\n";
if ($lgf) {
    print OUT "# Generated by cdl2sp --lgf\n";
    print OUT "\nCell $newtop bbox=0:0:0:0\n\n";
    foreach my $port (@inout)  { print OUT "Pin $port direction=InOut\n"; }
    foreach my $port (@input)  { print OUT "Pin $port direction=In\n"; }
    foreach my $port (@output) { print OUT "Pin $port direction=Out\n"; }
    print OUT "\n";
} else {
    print OUT "* Generated by cdl2sp\n";
    print OUT ".subckt $newtop";
    foreach my $node (sort keys %portnodes) {
        print OUT " $node";
    }
    print OUT "\n";
    if (@inout)  { print OUT "*  INOUT: "  . join(" ",@inout)  . "\n"; }
    if (@input)  { print OUT "*  INPUT: "  . join(" ",@input)  . "\n"; }
    if (@output) { print OUT "*  OUTPUT: " . join(" ",@output) . "\n"; }
}
foreach my $line (@out) {
    print OUT $line;
}
if (!$lgf) {
    print OUT ".ends $newtop\n";
}
close OUT;

# rename internal nodes that end in #
sub rename_net {
    my ($net) = (@_);
    if (defined $map_node{$net}) { $net = $map_node{$net}; }
    elsif ($net =~ /(\S+)#/g) {
        my $new_net = "n${node_num}";
        $map_node{$net}=$new_net;
        $node_num++;
        $net=$new_net;
    }
    return $net;
}
