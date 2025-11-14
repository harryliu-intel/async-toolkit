#!/usr/intel/bin/perl -l
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0

# AAG
# $Id$
# $DateTime$

use strict;
use Getopt::Long;

foreach my $file (@ARGV) {
    open (P, "<$file");
    my $out=$file;
    $out =~ s/\.cast//;
    $out .= "_lvt.cast";
    open (Q, ">$out");
    while (<P>) {
        chomp;
        if (/^define\s+"([^"]+)"/) {
            my $st=$1;
            s/"$st"/"${st}_lvt"/;
        }
        if (/\s(gate|stack)/) {
            s/\(1\)/(2)/;
            s/\(1,1\)/(2,2)/;
        }
        if (/synthesis\.qdi\..*\.(\d)\s/) {
            my $st=$1;
            s/\.$st /\.${st}_lvt /;
        }
        if (/lib\..*\.(\d+)\s/) {
            my $st=$1;
            s/\.$st /\.${st}_lvt /;
        }
        if (/chip\..*\.(\d+)\s/) {
            my $st=$1;
            s/\.$st /\.${st}_lvt /;
        }
        print Q;
	    }
}
close P;
close Q;
