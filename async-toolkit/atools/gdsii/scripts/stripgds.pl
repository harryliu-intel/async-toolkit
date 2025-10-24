#!/usr/intel/bin/perl -l
# AAG
# $Id$
# $DateTime$

use strict;
use Getopt::Long;

my $verbose=0;
my $infile;
my $outfile;
my %layers=(
#    "31;0" => 1,
#    "31;1" => 1,
#    "32;0" => 1,
#    "32;1" => 1,
#    "33;0" => 1,
#    "33;1" => 1,
#    "34;0" => 1,
#    "34;1" => 1,
#    "35;0" => 1,
#    "35;1" => 1,
#    "36;0" => 1,
#    "36;1" => 1,
#    "37;0" => 1,
#    "37;1" => 1,
#    "51;0" => 1,
#    "52;0" => 1,
#    "53;0" => 1,
#    "54;0" => 1,
#    "55;0" => 1,
#    "56;0" => 1,
);

my %empty=();
my $layer=-1;
my $type=0;
my $eltype="";
my $strname;
my $sname;
my $empty=1;
my $d;
my $geom=0;
my @geom=();
my $verbose=0;
my $force=0;
my $layerlist="";

sub usage {
    my ($msg)=@_;
    print STDERR "$msg" if (defined($msg) and $msg ne "");
    print STDERR <<EU;
Usage: stripgds --layerlist=[list] [--verbose] [--force] <gdsin> [<gdsout>]
EU
    exit 1;
}

my %options = (
    "verbose" => \$verbose,
    "layerlist=s" => \$layerlist,
    "force" => \$force,
);

GetOptions ( %options ) or usage();

my $layercount=0;
my $err=0;
foreach my $lpp (split(/[ ,]/,$layerlist)) {
    my ($l,$t)=split(/[;-]/,$lpp);
    if (defined ($l) and $l =~ /^\d+$/) {
        if (defined ($t) and $t =~ /^\d+$/) {
            $layers{"$l;$t"}=1;
        }
        else {
            # do all datatypes if type not specified
            $layers{"$l"}=1;
        }
        $layercount++;
    }
    else {
        print STDERR "$lpp is not a legal LPP";
        $err++;
    }
}
usage "No layers specified" if ! $layercount;
usage "Illegal Layers specified" if $err;
$infile = shift;
usage "No input specified" if ! defined ($infile) or ! -f $infile;
$outfile = shift;
if ( ! defined ($outfile) or $outfile eq "") {
    $outfile = $infile;
    $outfile =~ s/\.gds.*//;
    $outfile .= "_reduced.gds";
}
if (! $force and -f "$outfile") {
    select STDERR;
    $|=1;
    select STDOUT;
    print STDERR "use --force if you do not want this query.";
    printf STDERR "$outfile exists, overwrite? ";
    my $ans=<STDIN>;
    chomp $ans;
    if (! ($ans =~ /^y/i)) {
        exit 1;
    }
}

unlink "$outfile";
open (P, ">$outfile") or usage "Cannot create $outfile";
close P;

my $rdgds=`which rdgds 2>/dev/null`;
chomp $rdgds;
my $wrgds=`which wrgds 2>/dev/null`;
chomp $wrgds;
$rdgds = "fulcrum rdgds" if $rdgds eq "";
$wrgds = "fulcrum wrgds" if $wrgds eq "";

print STDERR "Starting pass 1" if $verbose;
open (P, "$rdgds '$infile' |");
while (<P>) {
    chomp;
    if (/^STRNAME/) {
        ($d, $strname) = split;
        $empty=1;
    }
    if (/^SNAME/) {
        $empty=0;
    }
    if (/^BOUNDARY/ or /^PATH/ or /^BOX/) {
        $geom = 1;
        $layer=$type=0;
    }
    if (/^LAYER/) {
        ($d, $layer)=split;
    }
    if (/^DATATYPE/) {
        ($d, $type)=split;
    }
    if (/^ENDEL/) {
        if ($geom) {
            my $v=sprintf( "%d;%d", $layer,$type);
            if ($layers{$v} or $layers{$layer}) {
                $empty=0;
            }
        }
        $layer = $type = 0;
        $geom=0;
    }
    if (/^ENDSTR/) {
        if ($empty) {
            $empty{$strname}=1;
            print STDERR "Empty $strname" if $verbose;
        }
    }
}
close P;
print STDERR "Starting Pass 2" if $verbose;
open (P, "$rdgds '$infile' |");
open (Q, "| $wrgds > '$outfile'");
select Q;
my $bgnstr="";
my @ref=();
my $inref=0;
my $instruct=0;
$empty=1;
$geom=0;
while (<P>) {
    chomp;
    if (/^BGNSTR/) {
        $instruct=1;
        $bgnstr=$_;
    }
    if (! $instruct) {
        print;
        next;
    }
    if (/^STRNAME/) {
        ($d, $strname) = split;
        $empty = $empty{$strname};
        if (! $empty) {
            print $bgnstr;
            print;
        }
    }
    elsif (/^[AS]REF/) {
        push @ref, $_;
        $inref=1;
    }
    elsif (/^PROP/) {
        next;
    }
    elsif ($inref) {
        push @ref, $_;
    }
    if (/^SNAME/) {
        ($d, $sname) = split;
    }
    elsif (/^BOUNDARY/ or /^PATH/ or /^BOX/) {
        $geom = 1;
        $layer=$type=0;
        push @geom, $_;
        next;
    }
    elsif ($geom) {
        push @geom, $_;
    }
    if (/^LAYER/) {
        ($d, $layer)=split;
    }
    elsif (/^DATATYPE/) {
        ($d, $type)=split;
    }
    elsif (/^ENDEL/) {
        if ($geom) {
            my $v=sprintf( "%d;%d", $layer,$type);
            if (($layers{$v} or $layers{$layer}) and ! $empty) {
                print join("\n", @geom);
            }
        }
        if ($inref) {
            print join("\n", @ref) if ! $empty{$sname} and ! $empty;
        }
        $layer = $type = 0;
        $inref=0;
        $geom=0;
        @geom=();
        @ref=();
    }
    elsif (/^ENDSTR/) {
        $instruct=0;
        print if ! $empty;
    }
}
close P;
