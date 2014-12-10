#!/usr/intel/bin/perl -l
# AAG
# $Id$
# $DateTime$

use POSIX;
use strict;

my $timing = shift;
my $paths = shift;
my $tau = shift;
$tau = 50 if ! defined $tau;

my %wire=();
my %dly=();
my @outputcaps;
open (P, "<$timing") or die "Cannot open timing file $timing";
while (<P>) {
    chomp;
    if (/output\s+(\S+)\s+(\S+)/) {
        my $pin = $1;
        my $wire= $2;
        $wire{$pin}=$wire;
    }
    if (/^path\s+(\S+)\s+(\S+)\s+"(\S+)"/) {
        my $ck=$1;
        my $pin=$2;
        my $dly=$3;
        $dly{$pin}=$dly;
    }
    if (/^outputCaps\s+(\S+)/) {
        @outputcaps=split(/,/,$1);
    }
}
open (P, "<$paths") or die "Cannot open path file $paths";
my $budget_cap;
my %budget_cap;
my $pin;
my $epin;
my $jslack;
my %jslack=();
while (<P>) {
    chomp;
    s/\s+$//;
    if (/slack=(\S+)ps.* budget_cap=(\S+)fF/i) {
        $budget_cap = $2/1000;
        $jslack=$1/1000;
    }
    if (/endnet=\/top\.(\S+)/) {
        $epin = $1;
    }
    if (! (/^\/\//) and /[-+]$/) {
        my $dpin = substr($_,length($_)-1,1);
        $pin = $epin.$dpin;
    }
    if ((/^$/) and defined ($epin)) {
        if (! defined ($budget_cap{$pin}) or $budget_cap{$pin} > $budget_cap) {
            $budget_cap{$pin}=$budget_cap;
        }
        if (! defined ($jslack{$pin}) or $jslack{$pin} > $jslack) {
            $jslack{$pin}=$jslack;
        }
        $epin=undef;
    }
}
if (defined ($epin)) {
    if (! defined ($budget_cap{$pin}) or $budget_cap{$pin} > $budget_cap) {
        $budget_cap{$pin}=$budget_cap;
    }
    if (! defined ($jslack{$pin}) or $jslack{$pin} > $jslack) {
        $jslack{$pin}=$jslack;
    }
}
foreach my $pin (sort keys %budget_cap) {
    my $spin = $pin;
    $spin =~ s/[-+]$//;
    $budget_cap{$pin} -= $wire{$spin};
    my $budget=0;
    my @dly=split(/,/,$dly{$pin});
    my $n = 0;
    for (; $n <= $#outputcaps and $outputcaps[$n] < $budget_cap{$pin}; $n++) {}
    # disallow cap > maxcap
    if ($n > $#outputcaps) {
        $budget=$dly[$#outputcaps];
    }
    elsif ($n == 0) {
        my $slope = ($dly[1]-$dly[0])/($outputcaps[1]-$outputcaps[0]);
        $budget = $dly[0]+($budget_cap{$pin}-$outputcaps[0])*$slope;
    }
    else {
        # interpolate
        my $slope = ($dly[$n]-$dly[$n-1])/($outputcaps[$n]-$outputcaps[$n-1]);
        $budget = $dly[$n-1]+($budget_cap{$pin}-$outputcaps[$n-1])*$slope;
    }
    my $zeroCapDelay=$dly[0]-($wire{$spin}-$outputcaps[0])*($dly[2]-$dly[0])/($outputcaps[2]-$outputcaps[0]);
    my $slack=$jslack{$pin}+$zeroCapDelay-$dly[0];
    printf "slack %.3f $pin %.4f\n", $tau/1000, $slack;
}
