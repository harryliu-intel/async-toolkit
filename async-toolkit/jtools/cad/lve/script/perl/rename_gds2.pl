#!/usr/intel/bin/perl -w

use strict;
use IPC::Open2;

# rename pin labels from cadence to gds2 namespace
my $text="";
my $n2pid=0;
my $valid=0;
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
