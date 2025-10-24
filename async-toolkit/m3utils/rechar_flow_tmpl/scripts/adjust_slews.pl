#!/usr/intel/bin/perl
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0


my $slew = 0;
my $factor = $ARGV[0] // 2;
while(<STDIN>) {
    if ($slew && /^(\s*{\s*)(.*)(\s*}.*)/) {
        my @slews = split(/ /, $2);
        for (my $i = 0; $i <= $#slews; $i++) {
            $slews[$i] *= $factor;
        }
        $_ = $1 . join(' ', @slews) . "$3\n";
    }
    $slew = 0;
    if (/explicit_points_slew/) {
        $slew = 1;
    }
    print;
}
