#!/usr/intel/bin/perl -l
# AAG
# $Id$
# $DateTime$

use strict;
use Getopt::Long;

my $cast_path="";
my $cell="";
my $size=0;
my $javaflags="";

# these come from the INVINV characterization
# at tt 0.9V 125C

my @values=(
   ["0.0315,0.0545,0.1275,0.4005,1.4880",
   "0.0130,0.0250,0.0750,0.2850,1.1320",
   "0.0295,0.0510,0.1160,0.3600,1.3300",
   "0.0110,0.0220,0.0640,0.2420,0.9600"],
   ["0.0285,0.0475,0.1035,0.3055,1.1060",
   "0.0110,0.0210,0.0570,0.2110,0.8340",
   "0.0265,0.0425,0.0915,0.2630,0.9455",
   "0.0090,0.0170,0.0470,0.1700,0.6750"],
   ["0.0260,0.0395,0.0830,0.2295,0.8100",
   "0.0100,0.0170,0.0440,0.1530,0.6040",
   "0.0240,0.0365,0.0740,0.2030,0.7040",
   "0.0080,0.0150,0.0360,0.1260,0.4960"],
   ["0.0235,0.0355,0.0680,0.1775,0.6000",
   "0.0090,0.0150,0.0340,0.1130,0.4420",
   "0.0230,0.0335,0.0625,0.1590,0.5310",
   "0.0080,0.0130,0.0290,0.0940,0.3680"],
   ["0.0210,0.0300,0.0550,0.1340,0.4310",
   "0.0080,0.0120,0.0260,0.0800,0.3100",
   "0.0195,0.0270,0.0500,0.1205,0.3825",
   "0.0070,0.0100,0.0220,0.0670,0.2590"],
   ["0.0195,0.0265,0.0455,0.1045,0.3150",
   "0.0070,0.0110,0.0210,0.0590,0.2200",
   "0.0180,0.0245,0.0430,0.0950,0.2825",
   "0.0060,0.0090,0.0180,0.0500,0.1850"],
   ["0.0185,0.0235,0.0385,0.0800,0.2230",
   "0.0070,0.0090,0.0170,0.0420,0.1480",
   "0.0180,0.0230,0.0350,0.0740,0.2015",
   "0.0060,0.0080,0.0140,0.0360,0.1250"],
   ["0.0185,0.0210,0.0305,0.0595,0.1505",
   "0.0070,0.0080,0.0130,0.0290,0.0930",
   "0.0170,0.0195,0.0295,0.0555,0.1360",
   "0.0060,0.0070,0.0110,0.0250,0.0780"],
   ["0.0170,0.0185,0.0240,0.0400,0.0880",
   "0.0060,0.0070,0.0100,0.0180,0.0480",
   "0.0165,0.0180,0.0230,0.0375,0.0800",
   "0.0050,0.0060,0.0080,0.0150,0.0400"],
);

my @inputcaps=(
   0.0006,
   0.0006,
   0.0007,
   0.0007,
   0.0009,
   0.0011,
   0.0016,
   0.0024,
   0.0052,
);

sub usage {
    my ($msg)=@_;
    print STDERR "Error: $msg" if defined $msg;
    print STDERR <<EU;
Usage: mkdefaultlib [options]
    --cast-path=<path>   : cast path
    --cell=<fqcn>        : cast cell name
    --size=[0..8]        : equivalent INVINV size of outputs
    --java-flags=<flags> : Flags (e.g., --max-heap-size) used to invoke Java
                           programs
EU
    exit 1;
}

GetOptions (
    "cast-path=s" => \$cast_path,
    "cell=s" => \$cell,
    "size=i" => \$size,
    "java-flags=s" => \$javaflags,
) or usage;

usage("--size is from 0..$#values") if !defined $values[$size];
usage() if ($cell eq "" or $cast_path eq "");
my @vls=@{$values[$size]};
my $fh;
open ($fh, "cast_query $javaflags --cast-path='$cast_path' --no-recurse --no-header --task=external_nodes=di:im:re --cell='$cell' |");
my %ports=();
my %directives=();
while (<$fh>) {
    chomp;
    if (/(^[-+])/) {
        s/(^[-+])//;
        my $dir=$1;
        my $port=$_;
        next if $port eq "GND" or $port eq "Vdd";
        $ports{$port}=$dir eq "+" ? "output" : "input";
    }
}
open ($fh, ">$cell.timing");
select $fh;
print "area 1";
print "inputSlews 0.0";
print "outputCaps 0.0010,0.0040,0.0160,0.0640,0.2560";
foreach my $port (sort keys %ports) {
    if ($ports{$port} eq "output") {
        print "$ports{$port} $port";
        print "path CK+ $port+ \"$vls[0]\" \"$vls[1]\"";
        print "path CK+ $port- \"$vls[2]\" \"$vls[3]\"";
    }
    else {
        print "$ports{$port} $port $inputcaps[$size]";
    }
}

close $fh;
system "generatelib $javaflags --cast-path='$cast_path' --cell='$cell' --translate=cadence --timing-file='$cell.timing' --format=real > '$cell.lib'";
#system "generatelib $javaflags --cast-path='$cast_path' --cell='$cell' --translate=cadence --timing-file='$cell.timing' --tau=0.04 ";
