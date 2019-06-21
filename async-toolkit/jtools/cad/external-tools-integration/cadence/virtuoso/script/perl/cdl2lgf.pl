#!/usr/intel/bin/perl -w

# Translate CDL syntax to LGF syntax for LayGen
# Folds transistors
# Copyright 2019 Intel
# Authors: Andrew Lines

use FileHandle;
use IPC::Open2;
use POSIX;
use Data::Dumper qw(Dumper);

# Output record separator
$" = " ";

# options and defaults
$grid     = 1e-10;    # convert to Laygen units
$length   = 14e-9;    # transistor length
$gridW    = 30e-9;    # rounding grid for width
$maxNmosW = 3*$gridW; # maximum fold width for NMOS
$maxPmosW = 3*$gridW; # maximum fold width for PMOS

# usage banner
sub usage() {
    die "Usage: $0\n" . 
        " [--grid=$grid]\n " .
        " [--length=$length] [--gridW=$gridW]\n" .
        " [--maxNmosW=$maxNmosW] [--maxPmosW=$maxPmosW]\n" .
        " <top_cell> <cdl_in> <snp_out>\n";
}

# parse arguments
while (defined $ARGV[0] && $ARGV[0] =~ /^--(.*)=(.*)/) {
    $flag = $1;
    $val = $2;
    if    ($flag eq "length")   { $length = $val; }
    elsif ($flag eq "grid")     { $grid = $val; }
    elsif ($flag eq "gridW")    { $gridW = $val; }
    elsif ($flag eq "maxNmosW") { $maxNmosW = $val; }
    elsif ($flag eq "maxPmosW") { $maxPmosW = $val; }
    else { usage(); }
    shift @ARGV;
}
@ARGV == 3 or usage();
my $top   = "$ARGV[0]";
my $f_in  = "$ARGV[1]";
my $f_out = "$ARGV[2]";

# report a fatal error
sub error_msg {
    die "$0: $f_in, line $.: $_[0]\n";
}

# Translate top cell name to library format
my $newtop = $f_out;
$newtop =~ s/\.lgf$//g;

# Do linewise translation of CDL to SNP
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
                $portnodes{$arg}=1 if ($cell eq $top);
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
        if ($cell eq $top) {
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
            my $suffix = "";
            if ($folds>1) {
                $suffix = "_${f}";
                $parameters{"w"} = ($maxW-($f<$small)) * $gridW;
            }
            $w2 = $parameters{"w"}/$grid;
            $l2 = $parameters{"l"}/$grid;
            $type =~ /^(.)/;
            my $tc = $1;
            $num_mos++ if ($cell eq $top);
            push @out, "Device ${name}${suffix} nets=[ $drain $gate $source $bulk ] " .
                "type=$tc model=$type w=$w2 l=$l2\n" if ($cell eq $top);
        }

    } elsif ($line =~ s/^X//i) {
        # subcells not supported
        error_msg "cdl2lgf expects flat transistors";
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

# output
open OUT, ">$f_out" or die "Can't write $f_out\n";
print OUT "# Generated by cdl2lgf\n";
print OUT "\nCell $newtop bbox=0:0:0:0\n\n";
foreach my $node (sort keys %portnodes) {
    my $dir="";
    if    (defined($b_nodes{$node})) { $dir = "InOut"; } # TODO: power?
    elsif (defined($g_nodes{$node}) && defined($sd_nodes{$node})) { $dir = "InOut"; }
    elsif (defined($g_nodes{$node})) { $dir = "In"; }
    elsif (defined($sd_nodes{$node})) { $dir = "Out"; }
    print OUT "Pin $node direction=$dir\n";
}
print OUT "\n";
foreach my $line (@out) {
    print OUT $line;
}
close OUT;
