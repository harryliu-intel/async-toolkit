#!/usr/intel/bin/perl -l
# AAG
# $Id$
# $DateTime$

use strict;
use Getopt::Long;

my $gdsfile;
my $cellname="";
my $rulesFile;
my $techlib;
my $technology;
my $pdkroot;
my $cdlfile;
my $verbose=0;
my $debug=0;

my %options = (
    "gds-file=s" => \$gdsfile,
    "topcell=s" => \$cellname,
    "debug" => \$debug,
    "rulesFile=s" => \$rulesFile,
    "techlib=s" => \$techlib,
    "fulcrum-pdk-root=s" => \$pdkroot,
    "technology=s" => \$technology,
    "cdl-file=s" => \$cdlfile,
    "verbose" => \$verbose,
);

sub usage {
    print STDERR $_[0] if defined ($_[0]);
    print STDERR "Usage gds2cdl [options]";
    foreach my $opt (sort keys %options) {
        $opt =~ s/=.*/ <string>/;
        print "   --$opt";
    }
exit 1;
}

GetOptions ( %options ) or usage;

$verbose=1 if $debug;

usage "No gds file" if ! $gdsfile or ! -s $gdsfile;
my %sname;
my %strname;
my $rdgds = `which rdgds`;
chomp $rdgds;
$rdgds = "fulcrum rdgds" if ! $rdgds =~ m:/tools/bin/:;
if ( ! $cellname or $cellname eq "") {
    print STDERR "Finding top cell name" if $verbose;
    open (P, "$rdgds $gdsfile |");
    while (<P>) {
        chomp;
        s/^  *//;
        if (/^STRNAME /) {
            my ($x,$name)=split;
            $strname{$name}=1;
        }
        elsif (/^SNAME /) {
            my ($x,$name)=split;
            $sname{$name}++;
        }
    }
    close P;
    my $cnt=0;
    foreach my $name (keys %strname) {
        if ( ! $sname{$name}) {
            $cnt++;
            $cellname=$name;
        }
    }
    usage "Too many potential top cells in $gdsfile" if $cnt > 1;
    usage "No topcell found in $gdsfile" if $cnt == 0;
}
elsif ($debug) {
    open (P, "$rdgds $gdsfile |");
    while (<P>) {
        chomp;
        s/^  *//;
        if (/^STRNAME /) {
            my ($x,$name)=split;
            $strname{$name}=1;
        }
        elsif (/^SNAME /) {
            my ($x,$name)=split;
            $sname{$name}++;
        }
    }
    close P;
    usage "Topcell $cellname not found." if ! defined $strname{$cellname};
}
usage "No topcell found in $gdsfile" if $cellname eq "";
$pdkroot = $ENV{FULCRUM_PDK_ROOT} if ! $pdkroot or ! -d $pdkroot;
$rulesFile = "$pdkroot/share/Fulcrum/assura/extract.rul"
    if ! -f $rulesFile;
usage "No rules file found" if ! -f $rulesFile;
$techlib = "$pdkroot/share/Fulcrum/assura/assura_tech.lib"
    if ! -f $techlib;
usage "No techlib file found" if ! -f $techlib;
if ( ! $technology or $technology eq "" ) {
    print STDERR "Finding Technology in techfile" if $verbose;
    open (P, "<$techlib");
    while (<P>) {
        chomp;
        my @f=split;
        if ($f[0] eq "DEFINE") {
            if ($technology eq "") {
                $technology = "$f[1]";
            }
            else {
                usage "Multiple Technologies in techfile, must define on command line";
            }
        }
    }
}

my $prefix="/scratch/gds2cdl";
my $rsftemplate = <<ET;
avParameters(
  ?inputLayout ( "gds2" "$gdsfile" )
  ?cellName "$cellname"
  ?runName "gds2cdl$$"
  ?workingDirectory "/scratch";
  ?technology "$technology"
  ?techLib "$techlib",
  ?rulesFile "$rulesFile"
  ?preserveShapes t  ; added due to issues with 65nm sram cells
  ?avrpt t
)
avExtract()
avNX()
ET

my $assura = `which assura`;
chomp $assura;
if ( $assura =~ /ASSURA/) {
    $assura = "";
}
else {
    $assura = "assura ";
}

print STDERR "Writing temporary rsf file" if $verbose;
open (P, ">${prefix}$$.rsf");
print P $rsftemplate;
close P;
$cdlfile = "gds2cdl$$.cdl" if $cdlfile eq "";
my $rv;
print STDERR "Running assura" if $verbose;
my $logfile="/dev/null";
$logfile="${prefix}$$.log" if $debug;
$rv=system ("${assura}assura ${prefix}$$.rsf > $logfile");
usage "assura failed" if $rv;
print STDERR "Running vldbToSpice" if $verbose;
$rv=system ("${assura}vldbToSpice ${prefix}$$.ldb > $cdlfile");
usage "vldbToSpice failed" if $rv;
if ($debug) {
    print STDERR "Files located: $prefix$$.*";
}
else {
    print STDERR "Cleaning $prefix" if $verbose;
    system "rm -rf ${prefix}$$.*";
    unlink "spiceMap.txt";
}
print STDERR "$cdlfile written" if $verbose;
