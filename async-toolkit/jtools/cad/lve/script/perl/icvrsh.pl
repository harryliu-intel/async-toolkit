#!/usr/intel/bin/perl
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0

use strict;
use warnings;
sub write_script {
    my ($command) = @_;
    my $script = "$$.sh";
    open my $fh, ">$script" || die;
    print $fh "#!/bin/sh\n";
    print $fh "touch $script.started\n";
    print $fh "$command\n";
    print $fh "rm $script\n";  # delete self
    close $fh;
    chmod 0755, $script;
    return $script;
}
my ($host, $cmd) = ($ARGV[0], $ARGV[1]);
my $temp = write_script($cmd);
my $started = "$temp.started";
unlink $started;
my @args = ('nbjob', 'prun', '--silent', '--daemon', '--preserve-slot', '--host', $host, "./$temp");
system(@args);
for my $i (1..10) {
    if (-f $started) {
        unlink $started;
        last;
    }
    sleep 15;
}
