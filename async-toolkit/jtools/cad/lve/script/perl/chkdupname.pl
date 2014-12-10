#!/usr/intel/bin/perl -l
# AAG
# $Id$
# $DateTime$

use strict;

print "Usage: chkdupname <verilogfile>" if ! @ARGV or ! -f $ARGV[0];

if ($ARGV[0] =~ /\.gz$/) {
    open (P, "gunzip -c '$ARGV[0]' |");
}
else {
    open (P, "<$ARGV[0]");
}
my %array=();
my %scalars=();
my $module;
$/=";";
while (<P>) {
    chomp;
    if (/\nendmodule/) {
        foreach my $name (sort keys %array) {
            print "$module ARRAY $name" if $scalars{$name};
        }
        %array=();
        %scalars=();
    }
    if (/\nmodule/) {
        my @f=split;
        foreach my $n (0..$#f) {
            if ($f[$n] eq "module") {
                $module=$f[$n+1];
                last;
            }
        }
    }
    s/\s+/ /g;
    s/^ //;
    s/ $//;
    my $isarray=0;
    if (/^(input|output|inout|wire)/) {
        my @f=split;
        shift @f;
        if (($f[0] =~ /^\[\d+:\d+\]$/) and ! ($f[0] =~ /^\\/)) {
            $isarray=1;
            shift @f;
        }
        foreach my $f (@f) {
            $f =~ s/,//;
            $f =~ s/ //g;
            if (($f =~ /\]$/) and ! $isarray) {
                $f =~ s/\[.*//;
                $f =~ s/\\//;
                $scalars{$f}=1;
            }
            $array{$f}=1 if $isarray;
        }
    }
}
