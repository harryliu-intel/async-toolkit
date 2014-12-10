#!/usr/intel/bin/perl -l
# AAG
# $Id$
# $DateTime$

use strict;
use Getopt::Long;
use DB_File;

my $verbose = 0;
my $toolhome = "/home/local/common/fulcrum";

my %opts = (
    "v|verbose" => \$verbose,
    "toolhome=s" => \$toolhome,
);

GetOptions ( %opts );

my ($warning,$error) = (0,0);

my $color = 0;
sub red {
    printf (STDERR "%c[31m", 0x1b) if $color != 1;
    $color = 1;
}

sub blue {
    printf (STDERR "%c[34m", 0x1b) if $color != 2;
    $color = 2;
}

sub black {
    printf (STDERR "%c[00m", 0x1b) if $color;
    $color = 0;
}

sub announce {
    my $string = $_[0];
    black;
    print STDERR "$string" if $verbose;
}

sub error {
    my ($string,$warn) = @_;
    if ($warn) {
        blue;
        print STDERR $string;
        $warning++;
    }
    else {
        red;
        print STDERR $string;
        $error++;
    }
}

sub warning {
    my $string = $_[0];
    error $string, 1;
}

my $fulcrum = "$toolhome/config";
my %donotbuild;
announce "Loading donotbuild";
open (P, "<$fulcrum/donotbuild");
while (<P>) {
    chomp;
    $donotbuild{$_}=1;
}
my %pref;
my %pd;
my $arch=`uname -sm`;
chomp $arch;
$arch =~ s/ /-/g;
$fulcrum = "$fulcrum/$arch";
my $db = "$fulcrum/prefdb";
announce "Checking preferred vs. donotbuild";
dbmopen (%pref, "$db", undef) || die "Cannot open $db";
foreach my $exe (sort keys %pref) {
    chomp;
    my $pkg = $pref{$exe};
    if ($donotbuild{$pkg} == 1) {
        warning "Preferred refers to do not build package $pkg:$exe";
    }
    if (defined ($pd{$exe})) {
        error "Error: duplicate $exe at $pkg vs $pref{$exe}";
    }
    else {
        $pd{$exe}=$pkg;
    }
}
undef %pd;
my %all;
my %is;
$db = "$fulcrum/alldb";
announce "Checking all vs. donotbuild";
my %alldb;
my %allcnt;
my %warn;
dbmopen (%alldb, "$db", undef) || die "Cannot locate $db";
foreach my $key (sort keys %alldb) {
    my ($pkg,$exe)=split(/:/,$key);
    if (!defined ($is{"$pkg:$exe"}) and $exe ne "") {
        $is{"$pkg:$exe"}=1;
        $all{$exe} .= " $pkg";
        $allcnt{$exe}++;
        $all{$exe} =~ s/^ //;
        if ($donotbuild{$pkg} == 1) {
            warning "All refers to do not build package $pkg:$exe";
            $warn{"$pkg:$exe"} = 1;
        }
    }
}
my $db = "$fulcrum/releasedb";
my %reldb;
dbmopen (%reldb, "$db", undef) || die "Cannot locate $db";
announce "Checking release vs. donotbuild";
my %isr;
my %rel;
my %relcnt;
foreach my $key (sort keys %reldb) {
    my ($pkg,$exe)=split(/:/,$key);
    if (!defined ($isr{"$pkg:$exe"}) and $exe ne "") {
        $isr{"$pkg:$exe"}=1;
        $rel{$exe} .= " $pkg";
        $relcnt{$exe}++;
        $rel{$exe} =~ s/^ //;
        if ($donotbuild{$pkg} == 1) {
            error "Rel refers to do not build package $pkg:$exe", $warn{"$pkg:$exe"};
        }
    }
    elsif (defined ($isr{"$pkg:$exe"}) and $exe ne "") {
        warning "Duplicate release entry $pkg:$exe";
    }
}
announce "Checking preferred vs release";
foreach my $exe (keys %pref) {
    if (!( $rel{$exe} =~ /$pref{$exe}/) ) {
        error "No Released $pref{$exe}:$exe $rel{$exe}";
    }
}
announce "Checking all vs preferred";
foreach my $exe (sort keys %all) {
    if ($allcnt{$exe} > 1) {
        if ( ! defined ($pref{$exe})) {
            error "Preferred required for $all{$exe}:$exe", $warn{"$all{$exe}:$exe"};
        }
    }
    elsif (! defined ($pref{$exe})) {
        if (! $warn{"$all{$exe}:$exe"}) {
            warning "Pref desired for All $all{$exe}:$exe";
        }
    }
}
announce "Checking release vs. preferred";
foreach my $exe (sort keys %rel) {
    if ($relcnt{$exe} > 1) {
        if ( ! defined ($pref{$exe})) {
            error "Pref required for Release $exe:$rel{$exe}", $warn{"$rel{$exe}:$exe"};
        }
    }
    elsif (! defined ($pref{$exe})) {
        warning "Pref desired for Rel $rel{$exe}:$exe";
    }
}
announce "Checking preferred vs. all";
foreach my $key (sort keys %pref) {
    my ($pkg,$exe)=split(/:/,$key);
    if ($exe ne "" and ! ( "$all{$exe}" =~ /$pkg/) )  {
        error "Pref contains non-existent pkg/exe $key",$warn{"$pkg:$exe"};
    }
    if ($exe ne "" and ! ( "$rel{$exe}" =~ /$pkg/) )  {
        error "Rel contains non-existent pkg/exe $key", $warn{"$pkg:$exe"};
    }
}
if ($warning or $error) {
    announce sprintf "%d errors %d warnings", $error, $warning;
}
black;
