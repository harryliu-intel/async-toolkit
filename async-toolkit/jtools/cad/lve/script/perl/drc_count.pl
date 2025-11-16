#!/usr/intel/bin/perl -w
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0


my %count;
my %rules;
my $rule;
my $ready=1;
while (my $line = <STDIN> ){
    if ($ready && $line =~ /^ (\S+):/) { $rule = $1; $ready=0; }
    if ($line =~ /(\d+) violation[s]* found.$/) {
        $ready=1;
        my $c=$1;
        my $class;
        my $r=lc($rule);
        if ($r =~ /nodata/) { $class="nodata"; }
        elsif ($r =~ /^bm/ || $r =~ /^sm/ || $r =~ /^m/ || $r =~ /^bv/ || $r =~ /^sv/ || $r =~ /^v/) { $class="metvia"; }
        else { $class="other"; }
        if (defined($count{$class})) { $count{$class}+=$c; $rules{$class} .= "\t$rule\t$c\n"; }
        else                         { $count{$class} =$c; $rules{$class}  = "\t$rule\t$c\n"; }
    }
}

foreach my $key ("nodata", "other", "metvia") {
    defined($count{$key}) or next;
    print "$key violations: $count{$key}\n";
    print "$rules{$key}";
    print "\n";
}
