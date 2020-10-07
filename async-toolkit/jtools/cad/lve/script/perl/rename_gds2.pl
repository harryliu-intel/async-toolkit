#!/usr/intel/bin/perl -w

use strict;
use IPC::Open2;

# rename pin labels from cadence to gds2 namespace
my $pintype=126;
my $text=0;
my $texttype=0;
my $err=0;
my $n2pid=0;
while (my $line = <STDIN>) {
    if    ($line =~ /^TEXT$/)            { $text=1; }
    elsif ($line =~ /^TEXTTYPE (\d+)$/ ) { $texttype=$1; }
    elsif ($line =~ /^ENDEL$/)           { $text=0; }
    elsif ($text==1 && $texttype==$pintype && $line =~ /^STRING '(\S+)'$/) {
        my $net=$1;
        $net=n2convert($net);
        $line="STRING '$net'\n";
    }
    print $line;
}
exit $err;

# use Java rename to rename nodes from cadence to gds2
local(*N2RD,*N2WR);
sub n2convert {
    my $in = $_[0];
    $in =~ s/\#/_/g; # hack
    $in =~ s/\$/_/g; # hack because renamer does not recognize '$', use '_' to be consistent with vs2cast
    local($_);
    if ($n2pid <= 0) {
        $n2pid=open2(\*N2RD,\*N2WR, "fulcrum rename --from=cadence --to=gds2 --type=node");
    }
    print N2WR "$in\n";
    my $out=<N2RD>;
    chomp $out;
    if ($out eq "") {
        $out=$in;
        $err++;
    }
    $out;
}
