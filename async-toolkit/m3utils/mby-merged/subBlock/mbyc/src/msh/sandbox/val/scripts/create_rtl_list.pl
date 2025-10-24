#!/usr/bin/perl
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0


while (<STDIN>) {
    next if ($_ =~ /\/sva\//);      # strip out bound assertion files in /sva/ directories
    print "lappend VERILOG_SOURCE_FILES $_";
}
