#!/usr/intel/bin/perl -l
# AAG
# $Id$
# $DateTime$

use strict;
use Getopt::Long;

my %cache=();
my $maxlen=127;
my $verbose=0;
my $topcell="";

sub usage {
    my $msg = $_[0];
    print STDERR "$msg" if defined $msg;
    print STDERR <<EU;
Usage: fixhdrc [options] gds-file
    --maxlen=[127]    ; max name length
    --top-cell=name   ; name of top cell
    --verbose         ;
EU
exit 0;
}

GetOptions (
    "verbose" => \$verbose,
    "top-cell=s" => \$topcell,
    "maxlen=i" => \$maxlen,
) or usage;

my $file=shift;
usage if ! defined $file;
usage("$file is not readable") if ! -r $file;
usage("Need to define top cell") if $topcell eq "";
open (P, "rdgds '$file' |");
my $fixed=0;
if (length($topcell)>$maxlen) {
    $cache{$topcell}="TOP_CELL";
    $fixed=1;
}
else {
    $cache{$topcell}=$topcell;
}
while (<P> and ! $fixed ) {
    chomp;
    s/^  *//;
    my ($key,$obj)=split;
    if ($key eq "SNAME" or $key eq "STRNAME") {
        if (length($obj) > $maxlen) {
            $fixed=1;
        }
    }
}
close P;
if (! $fixed) {
    print "$topcell";
    exit 0;
}
open (P, "rdgds '$file' |");
open (Q, "| wrgds > '$file.tmp'");
select Q;
my $nextname=1;
my $foundtopcell=0;
while (<P>) {
    chomp;
    s/^  *//;
    my ($key,$obj)=split;
    if ($key eq "SNAME" or $key eq "STRNAME") {
        if (length($obj) > $maxlen) {
            if (! defined($cache{$obj})) {
                if ($obj eq $topcell) {
                    $cache{$obj}="TOP_CELL";
                    $foundtopcell++;
                }
                else {
                    $cache{$obj}=sprintf("SUBCELL_%04d", $nextname);
                    $nextname++;
                }
            }
            $foundtopcell=1 if $cache{$obj} eq "TOP_CELL";
            $_ = "$key $cache{$obj}";
        }
    }
    print;
}
close P;
select STDOUT;
close Q;
if ($verbose) {
    foreach my $key (sort keys %cache) {
        print STDERR "$key renamed to $cache{$key}";
    }
}
usage("$topcell not found in $file") if ! $foundtopcell;
unlink $file;
rename "$file.tmp","$file";
print "$cache{$topcell}";
