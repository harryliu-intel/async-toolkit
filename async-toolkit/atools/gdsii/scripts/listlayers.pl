#!/usr/intel/bin/perl -l
# AAG
# $Id$
# $DateTime$

use strict;

use Getopt::Long;
use IPC::Open2;

my $techfile="";
my $oatechfile="";
my $pdkroot;
my $verbose=0;
my $bystrname=0;

GetOptions (
    "tech-file=s" => \$techfile,
    "fulcrum-pdk-root=s" => \$pdkroot,
    "verbose" => \$verbose,
    "strname" => \$bystrname,
) or die;

$techfile="$pdkroot/share/Fulcrum/stream/strmin.tech"
    if (defined $pdkroot and -s "$pdkroot/share/Fulcrum/stream/strmin.tech");

$oatechfile="$pdkroot/share/Fulcrum/stream/strmin.layermap"
    if (defined $pdkroot and -s "$pdkroot/share/Fulcrum/stream/strmin.layermap");

if ( -s "$oatechfile") {
    print STDERR "Techfile: $oatechfile" if $verbose;
}
else {
    print STDERR "Techfile: $techfile" if $verbose;
}

my $strname="";

my %list=();
my %listg=();
my %listt=();
my %lookup=();
my %strname=();
my %strlist=();

sub list {
    my $maxg=4;
    my $maxt=4;
    foreach my $key (sort ncmp keys %list) {
        my ($l,$d)=split(/ /,$key);
        my $g=sprintf "%d", $listg{$key};
        $maxg=length($g) if (length($g) > $maxg);
        $g=sprintf "%d", $listt{$key};
        $maxt=length($g) if (length($g) > $maxt);
    }
    foreach my $key (sort ncmp keys %list) {
        my ($l,$d)=split(/ /,$key);
        printf "%-3d %3d %*d %*d %s\n",$l,$d,$maxg,$listg{$key},$maxt,$listt{$key},$lookup{"$l $d"};
    }
    undef %list;
}

sub ncmp {
    my ($a1,$b1,$a2,$b2);
    ($a1,$b1)=split(/ /,$a);
    ($a2,$b2)=split(/ /,$b);
    return $a1-$a2 if $a2-$a1;
    return $b1-$b2;
}

if ( -e "$oatechfile") {
    open (P, "<$oatechfile");
    while (<P>) {
        chomp;
        next if /^\s*#/;
        s/\s*#.*//;
        my ($name,$purpose,$layer,$type)=split;
        if (defined ($type)) {
            my $key="$layer $type";
            $lookup{$key}="$name $purpose";
        }
    }
    close P;
}
elsif ( -e "$techfile") {
    open (P, "<$techfile");
    my $instream=0;
    while (<P>) {
        chomp;
        s/;.*//;
        if (/streamLayers\(/) {
            $instream=1;
            next;
        }
        if ($instream) {
            s/^ *//;
            if (/^\)/) {
                $instream=0;
                last;
            }
            if (/"/) {
                s/[()"\t]/ /g;
                s/  */ /g;
                s/^ //;
                my @f=split(/ +/,$_);
                my $key = "$f[2] $f[3]";
                print STDERR "Duplicate layer line $. $key => $lookup{$key} => $f[0] $f[1]" if defined $lookup{$key} and $key ne "0 0";
                $lookup{"$key"}="$f[0] $f[1]";
            }
        }
    }
    close P;
}
my $set=0;
my $geom=0;
my $text=0;
my $layer=0;
my $datatype=0;
my $strname="";
my $pid;
foreach my $file (@ARGV) {
	$pid=open2(\*RD, \*WR, "aaggds '$file' 2>/dev/null");
	if ($bystrname) {
		print WR "lys\nquit\n";
	}
	else {
		print WR "ly\nquit\n";
	}
    $set=0;
    while  (<RD>) {
        chomp;
        if (/^STRNAME/) {
        	if ($bystrname) {
				list;
				undef %list;
				%list=();
				undef %listt;
				%listt=();
				undef %listg;
				%listg=();
        	}
        	print;
        }
        else {
        	my ($layer,$datatype,$geom,$text)=split;
        	$list{"$layer $datatype"}=1;
        	$listg{"$layer $datatype"}+= $geom;
        	$listt{"$layer $datatype"}+= $text;
        }
    }
}
if ($bystrname) {
	list;
	undef %list;
	%list=();
	undef %listt;
	%listt=();
	undef %listg;
	%listg=();
}
else {
	print "$ARGV[0]" if ($#ARGV == 0);
	my $maxg=4;
	my $maxt=4;
	foreach my $key (sort ncmp keys %list) {
		my ($l,$d)=split(/ /,$key);
		my $g=sprintf "%d", $listg{$key};
		$maxg=length($g) if (length($g) > $maxg);
		$g=sprintf "%d", $listt{$key};
		$maxt=length($g) if (length($g) > $maxt);
	}
	printf "Lay Type %*.*s %*.*s Name\n", $maxg,$maxg,"Geom", $maxt,$maxt,"Text";
	foreach my $key (sort ncmp keys %list) {
		my ($l,$d)=split(/ /,$key);
		printf "%-3d %3d %*d %*d %s\n",$l,$d,$maxg,$listg{$key},$maxt,$listt{$key},$lookup{"$l $d"};
	}
}
