#!/usr/intel/bin/perl -w

# Translate CDL syntax to SNP syntax for LayGen
# Folds transistors
# Copyright 2012 Intel
# Authors: Andrew Lines

use FileHandle;
use IPC::Open2;
use POSIX;

# Output record separator
$" = " ";

# options and defaults
$gridW = 0.042e-6;    # rounding grid for width
$maxNmosW = 2*$gridW; # maximum fold width for NMOS
$maxPmosW = 2*$gridW; # maximum fold width for PMOS

# usage banner
sub usage() {
    die "Usage: $0\n" . 
        " [--gridW gridW] [--maxNmosW nw] [--maxPmosW pw]\n" .
        " <top_cell> <cdl_in> <snp_out>\n";
}

# parse arguments
while (defined $ARGV[0] && $ARGV[0] =~ /^--(.*)/) {
    $flag = $1;
    if ($flag eq "gridW") {
        $gridW = $ARGV[1];
        shift @ARGV;
    } elsif ($flag eq "maxNmosW") {
        $maxNmosW = $ARGV[1];
        shift @ARGV;
    } elsif ($flag eq "maxPmosW") {
        $maxPmosW = $ARGV[1];
        shift @ARGV;
    } else {
        usage();
    }
    shift @ARGV;
}
@ARGV == 3 or usage();
my $top   = "$ARGV[0]";
my $f_in  = "$ARGV[1]";
my $f_out = "$ARGV[2]";
print "$top\n";

# report a fatal error
sub error_msg {
    die "$0: $f_in, line $.: $_[0]\n";
}

# map nodes
sub map_nodes {
    foreach $node (@_) {
        if ($node eq "Vdd") { $node = "vcc"; }
        elsif ($node eq "GND") { $node = "vss"; }
    }
}

# Do linewise translation of CDL to SNP
open IN,  "<$f_in"  or die "Can't read $f_in\n";
$line = <IN>;
$. = 0;
my $cell;
my @out;
my %portnodes;
my %localnodes;
my $num_mos = 0;
while ($line) {
    $next_line = <IN>;
    while (defined $next_line && $next_line =~ s/^\+/ /) {
        chomp $line;
        $line .= $next_line;
        $next_line = <IN>;
    }
    my $full = $line;
    if ($line =~ s/^\.SUBCKT\s+//i) {

        # Begin Subcircuit Definition
        my @parameters = ();

        $line =~ s/(\S+)\s+//;
        $cell = $1;
        $type = $1;
        #push @out, ".SUBCKT $type" if ($cell eq $top);
        while ($line =~ s/(\S+)\s*//) {
            $arg = $1;
            if ($arg =~ /(\S+)=(\S+)/) {
                # Parameter and default.
                # Default value saved in subc_arg will be used 
                # if an instance fails to provide all parameters
                #push @out, " $1=$2" if ($cell eq $top);
            } else {
                # Node argument
                #push @out, " $arg" if ($cell eq $top);
                map_nodes($arg);
                $portnodes{$arg}=1  if ($cell eq $top);
            }
        }
        #push @out, "\n" if ($cell eq $top);

    } elsif ($line =~ s/^\.ENDS//i) {

        $cell = "";
        #push @out, ".ENDS\n" if ($cell eq $top);

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
        map_nodes($drain,$source,$gate,$bulk);
        if ($cell eq $top) {
            $localnodes{$drain}=1;
            $localnodes{$source}=1;
            $localnodes{$gate}=1;
            $localnodes{$bulk}=1;
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

        # compute folds
        my $w = POSIX::ceil($parameters{"w"}/$gridW);
        my $maxW = $maxPmosW;
        $maxW = $maxNmosW if ($type =~ /^n/);
        $maxW = POSIX::ceil($maxW/$gridW);
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
            $w2 = $parameters{"w"}*1e6;
            $l2 = $parameters{"l"}*1e6;
            $num_mos++ if ($cell eq $top);
            push @out,  "M${name}${suffix} $drain $gate $source $bulk $type $w2 $l2 \$\$UNI\n"
                if ($cell eq $top);
        }

    } elsif ($line =~ s/^X//i) {

        # Call to Subcircuit
        my %parms = ();
        my @parms = ();
        my @nodes = ();
        $line =~ s/^(\S+)\s*// or error_msg "Call lacks instance name: $full";
        $name = $1;
        while ($line =~ s/(\S+)=(\S+)\s*//) {
            $parms{$1}=$2;
            push @parms, $1;
        }
        $line =~ s/(\S+)\s*$// or error_msg "Call lacks SUBCKT name: $full";
        $type = $1;
        @nodes = split /\s+/, $line;
        my $slash = pop @nodes; # FIXME: find better way to remove last "/"
        if ($slash ne "/") { # slash is optional before subcircuit name
            push @nodes, $slash;
        }
        push @out,  "X$name @nodes $type" if ($cell eq $top);
        foreach my $parm (@parms) {
            push @out,  " $parm=$parms{$parm}" if ($cell eq $top);
        }
        push @out,  "\n" if ($cell eq $top);

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
print OUT ".GLOBAL vcc vss\n";
my @ports;
foreach my $node (keys %portnodes) {
    unless ($node eq "vcc" || $node eq "vss") { push @ports, $node; }
}
print OUT "\$\$.MACRO $top @ports\n";
print OUT "\$ CELL: $top (default L = 0.028)\n";
print OUT "\$\$.INPUT @ports\n";
print OUT "\$\$.OUTPUT\n";
print OUT "\$\$.IOPUT\n";
my $num_localnodes = 0;
my @nodes;
foreach my $node (keys %localnodes) {
    if (!defined $portnodes{$node}) { push @nodes, $node; $num_localnodes++; }
}
print OUT "\$\$.DIC $num_localnodes $num_mos 0\n";
print OUT "\$\$ @nodes\n";
print OUT "\$\$.DICEND\n";
foreach my $line (@out) {
    print OUT $line;
}
print OUT "\$\$.EOM\n";
close OUT;
