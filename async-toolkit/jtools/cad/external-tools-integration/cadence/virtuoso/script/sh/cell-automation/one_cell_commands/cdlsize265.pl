#!/usr/intel/bin/perl -l
# AAG
# $Id$
# $DateTime$

use strict;
use Getopt::Long;

my $verbose=0;
my $debug = 0;
# default layer mapping

my %options = (
    "verbose" => \$verbose,
    "debug" => \$debug,
);
my $prefix="outfile$$";

GetOptions ( %options ) or usage();

my $in = $ARGV[0];
my $tmpout=$in;
if ($tmpout =~ /\//) {
    $tmpout =~ s:/([^/]+$):/tmp$1:;
}
else {
    $tmpout = "tmp$tmpout";
}
usage() if ! defined $in;


sub usage {
    my ($msg)=@_;
    print STDERR "$msg" if defined $msg;
    print STDERR <<EU;
Usage: cdlsize265 [options] targetfile
    --verbose         : what is happening
    --debug           : keep temp files
EU
    exit 1;
}

open (P, "<$in");
my @lines=();
while (<P>) {
    chomp;
    my @f=split;
    foreach my $n (0..$#f) {
        $f[$n]="nmospd_sr" if ($f[$n] eq "nmos_sram");
        $f[$n]="pmospu_sr" if ($f[$n] eq "pmos_sram");
        if ($f[$n] =~ /=/) {
            if ($f[$n] =~ /^(\+)?(\S+)=(\S+)$/) {
                my $pl=$1;
                my $p=$2;
                my $v=$3;
                if ($v =~ /u$/) {
                    $v =~ s/u$//;
                    $v *= 1e-6;
                }
                if ($v != 0) {
                    if ($p =~ /^[np]w/i or $p =~ /^w$/i) {
                        $v *= 0.6;
                        $v += 0.03e-6 if $v < 1.2e-7;
                        $v = sprintf "%.8e", $v;
                        $v =~ s/0+e/e/;
                        $v =~ s/\.e/e/;
                        $f[$n] = "$pl$p=$v";
                    }
                    if ($p =~ /^[np]l/i or $p =~ /^l$/i) {
                        $v = $v * 0.6;
                        $v = sprintf "%.8e", $v - 0.018e-6;
                        $v =~ s/0+e/e/;
                        $v =~ s/\.e/e/;
                        $f[$n] = "$pl$p=$v";
                    }
                }
            }
        }
    }
    push @lines, join(" ", @f);
}

close P;
system "/bin/mv '$in' '$in.130'";
open (P, ">$in");
print P join("\n", @lines);
close P;
