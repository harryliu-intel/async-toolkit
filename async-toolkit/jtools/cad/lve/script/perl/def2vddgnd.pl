#!/usr/intel/bin/perl -l
# AAG
# $Id$
# $DateTime$

use strict;
use Getopt::Long;
my $verbose=0;
my $skillfile;
my $deffile;

my %options = (
    "verbose" => \$verbose,
    "skill=s" => \$skillfile,
    "def=s" => \$deffile,
);

sub usage {
    my ($msg)=@_;
    print STDERR "$msg" if defined $msg;
    print STDERR <<EU;
Usage: def2vddgnd.pl [--verbose] [--skill=<file>] [--def=<file>] [<deffile>]
    finds lower left position of Vdd/GND metal in the def
    output is a list or skill, if specified
EU
    exit 1;
}

GetOptions (%options) or usage;

my $units=1000;
$deffile = shift if ! defined $deffile;

open (P, "<$deffile");
while (<P>) {
    last if (/^SPECIALNETS/);
    if (/^UNITS/) {
        my @f=split;
        $units=$f[3];
        $units =~ s/;//;
    }
}
my %minx=();
my %miny=();
my $net;
my $maxmetal=0;
while (<P>) {
    last if (/^END S/);
    chomp;
    if (/^- (\S+)/) {
        $net = $1;
        $net = "GND" if $net eq "Vss";
        $net = "Vdd" if $net eq "VDD";
        next;
    }
    s/^[\s\+]+//;
    my @f=split;
    if ($f[1] =~ /^M/) {
        my $m = $f[1];
        $m =~ s/[A-Za-z]//g;
        $maxmetal = $m if $m > $maxmetal;
        for (my $n = 2; $n <= $#f; $n++) {
            if ($f[$n] eq "(") {
                my $x=$f[$n+1];
                my $y=$f[$n+2];
                if ($x ne "*" and $y ne "*") {
                    if (defined ($minx{"$net $f[1]"})) {
                        if (($x < $minx{"$net $f[1]"}) and ($y < $miny{"$net $f[1]"})) {
                            $minx{"$net $f[1]"} = $x;
                            $miny{"$net $f[1]"} = $y;
                        }
                    }
                    else {
                        $minx{"$net $f[1]"} = $x;
                        $miny{"$net $f[1]"} = $y;
                    }
                }
            }
        }
    }
}
if (defined ($skillfile)) {
    open (P, ">$skillfile") or usage "Error: Cannot open $skillfile";
    foreach my $net (sort keys %minx) {
        my ($n,$m)=split(/ /, $net);
        if ($m eq "M$maxmetal") {
            print P "procedure( fixDraw$n( )";
            printf P "  dbCreateLabel( wcv() list(\"$m\" \"pin\") %.3f:%.3f \"$n\" \"centerCenter\" \"R0\" \"stick\" 0.1)\n", $minx{$net}/$units, $miny{$net}/$units;
            print P ")\n";
        }
    }
    close P;
}
else {
    foreach my $net (sort keys %minx) {
        my ($n,$m)=split(/ /, $net);
        printf "$net %.3f:%.3f\n", $minx{$net}/$units, $miny{$net}/$units
            if ($m eq "M$maxmetal");
    }
}
