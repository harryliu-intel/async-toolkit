#!/usr/intel/bin/perl -l
# AAG
# $Id$
# $DateTime$

use strict;
use Getopt::Long;

foreach my $file (@ARGV) {
    open (P, "<$file");
    my $out=$file;
    $out =~ s/\.cast//;
    $out .= "_hvt.cast";
    open (Q, ">$out");
    while (<P>) {
        chomp;
        if (/^define\s+"([^"]+)"/) {
            my $st=$1;
            s/"$st"/"${st}_hvt"/;
        }
        if (/\s(gate|stack)/) {
            s/\(1\)/(3)/;
            s/\(1,1\)/(3,3)/;
        }
        if (/synthesis\.qdi\..*\.(\d)\s/) {
            my $st=$1;
            s/\.$st /\.${st}_hvt /;
        }
        if (/lib\..*\.(\d+)\s/) {
            my $st=$1;
            s/\.$st /\.${st}_hvt /;
        }
        if (/chip\..*\.(\d+)\s/) {
            my $st=$1;
            s/\.$st /\.${st}_hvt /;
        }
        print Q;
	    }
}
close P;
close Q;
