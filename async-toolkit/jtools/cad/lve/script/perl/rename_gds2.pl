#!/usr/intel/bin/perl -w
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0


use strict;
use IPC::Open2;

# rename pin labels and instance names from cadence to gds2 namespace
my $text="";
my $n2pid=0;
my $i2pid=0;
my $valid=0;
my $propattr=0;
while (my $line = <STDIN>) {
    if    ($line =~ /^TEXT$/) { $text=$line; }
    elsif ($text ne "" && $line =~ /^STRING '(\S+)'$/) {
        my $net=$1;
        if ($net =~ /^[_0-9a-zA-Z\.\[\]]+$/) { # keep legal labels
            $net=n2convert($net);
            $line="STRING '$net'\n";
            $valid=1;
        }
        $text .= $line;
    }
    elsif ($text ne "" && $line =~ /^ENDEL$/) {
        $text .= $line;
        if ($valid) { print $text; }
        $text="";
        $valid=0;
    }
    elsif ($text ne "") { $text .= $line; }
    elsif ($line =~ /^PROPATTR (\d+)$/ ) { $propattr=$1; print $line; }
    elsif ($propattr==112 && $line =~ /^PROPVALUE instName=(\S+)$/ ) {
        my $inst=$1;
        $inst=i2convert($inst);
        print "PROPVALUE $inst\n";
    }
    else { print $line; }
}

# use Java rename to rename nodes from cadence to gds2
local(*N2RD,*N2WR);
sub n2convert {
    my $in = $_[0];
    if ($n2pid <= 0) {
        $n2pid=open2(\*N2RD,\*N2WR, "fulcrum rename --from=cadence --to=gds2 --type=node");
    }
    print N2WR "$in\n";
    my $out=<N2RD>;
    chomp $out;
    $out;
}

# use Java rename to rename instances from cadence to gds2
local(*I2RD,*I2WR);
sub i2convert {
    my $in = $_[0];
    if ($i2pid <= 0) {
        $i2pid=open2(\*I2RD,\*I2WR, "fulcrum rename --from=cadence --to=gds2 --type=instance");
    }
    print I2WR "$in\n";
    my $out=<I2RD>;
    chomp $out;
    $out;
}
