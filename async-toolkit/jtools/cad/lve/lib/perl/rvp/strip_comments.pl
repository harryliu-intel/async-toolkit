#!/usr/bin/perl -w -I..
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0


use rvp 7.54;


foreach my $f (@ARGV) {
    my $chunkRead = rvp->chunkReadInit($f,0);
    while ($chunk = rvp->chunkRead($chunkRead)) {
	print $chunk->{text} unless $chunk->{type} eq "comment";
    }
}


