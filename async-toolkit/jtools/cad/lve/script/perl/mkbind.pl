#!/usr/intel/bin/perl -l
# AAG
# $Id$
# $DateTime$

use strict;
use Getopt::Long;

my %cells;
my %xref;

sub usage {
    my ($msg) = @_;
    print STDERR $msg if $msg ne "";
print STDERR <<EU;
Usage: mkbind cdlfile
EU
exit 1;
}

my $cdl = shift;
usage if ! -s $cdl;

open (P, "<$cdl");
while (<P>) {
    chomp;
    if (/^.subckt/i) {
        my ($x, $y)=split;
        if ($y eq "") {
            $y = <P>;
            chomp $y;
            $y =~ s/^\+//;
            $y =~ s/^  *//;
            $y =~ s/ .*//;
        }
        $cells{$y}=1;
        $x=$y;
        $y =~ s/\./_/g;
        $y =~ s/-/_/g;
        $cells{$y} = 4 if $x ne $y;
        $xref{$y}=$x;
    }
}
close P;
open (P, "<cellName.map");
while (<P>) {
    chomp;
    next if /^#/;
    my ($dfii,$view,$gdsname)=split;
    $cells{$gdsname} |= 2;
    $xref{$gdsname} = $dfii;
}
foreach my $name (sort keys %cells) {
    print "C $xref{$name} $name" if ($cells{$name} == 6);
    print "C $name $name" if ($cells{$name} == 3);
}
