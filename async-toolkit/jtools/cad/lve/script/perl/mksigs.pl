#!/usr/intel/bin/perl -l
# AAG
# $Id$
# $DateTime$

use strict;
use Getopt::Long;

my $envfile;
my $cdlfile;
my $sigfile;
my $verbose=0;

GetOptions (
    "env=s" => \$envfile,
    "cdl=s" => \$cdlfile,
    "sig=s" => \$sigfile,
    "verbose" => \$verbose,
) or die;

$envfile = shift if ! defined $envfile;
$cdlfile = shift if ! defined $cdlfile;
$sigfile = shift if ! defined $sigfile;

my $line;
my %env;
my %test;
my @nodes;
open (P, "<$envfile") or die "Error: Cannot open $envfile in mksigs";
while (<P>) {
    chomp;
    if (/^Xtest/) {
        $line = $_;
        $_=<P>;
        chomp;
        while (/^\+/) {
            s/^\+ */ /;
            $line .= $_;
            $_=<P>;
            chomp;
        }
        $line =~ s/  */ /g;
        @nodes=split(/ /, $line);
        shift @nodes;
        pop @nodes;
        foreach my $node (@nodes) {
            $node =~ s/_cell$//;
            $env{$node}=1;
        }
        last;
    }
}
close P;
open (P, "<$cdlfile") or die "Error: Cannot open $cdlfile in mksigs";
@nodes=();
while (<P>) {
    chomp;
    if (/^\.SUBCKT/) {
        $line = $_;
        $_=<P>;
        chomp;
        while (/^\+/) {
            s/^\+ */ /;
            $line .= $_;
            $_=<P>;
            chomp;
        }
        @nodes=split(/ /,$line);
        shift @nodes;
        shift @nodes;
    }
}
close P;
foreach my $node (@nodes) {
    $node =~ s/_cell$//;
    $test{$node}=1;
}
foreach my $node (sort keys %env) {
    print "v(Xenv.$node)";
}
foreach my $node (sort keys %test) {
    print "v(Xenv.Xtest.$node)";
}
print "i(V3m)";
exit 0;
