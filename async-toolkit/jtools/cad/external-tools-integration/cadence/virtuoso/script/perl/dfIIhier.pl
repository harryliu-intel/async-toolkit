#!/usr/intel/bin/perl -l
# AAG
# $Id: dfIIhier.pl,v 1.2 2008/10/09 17:18:17 aubrey Exp aubrey $
# $DateTime$

use strict;
use Getopt::Long;
use IPC::Open2;

my $dfIIdir;
my $cell;
my $lib;
my $pdk="";
my $lvl=0;
my $pdkdfII;
my %libdirs;
my $maxlvl = 32;
my $find;
my $view="layout";
my $conversion="cadence";
my $virtuoso_mode=0;

my %options = (
    "dfII-dir=s" => \$dfIIdir,
    "cell=s" => \$cell,
    "lib=s" => \$lib,
    "maxlevel=i" => \$maxlvl,
    "where=s" => \$find,
    "view=s" => \$view,
    "fulcrum-pdk-root=s" => \$pdk,
    "conversion=s" => \$conversion,
    "virtuoso_mode" => \$virtuoso_mode,
);

my $usage;

sub usage {
    my ($msg) = @_;
    print STDERR "$msg" if defined $msg;
    print $usage;
    exit 1;
}

my $pid=0;

sub convert {
    my ($in) = @_;
    if ($pid) {
        print WR $in;
        my $out=<RD>;
        chomp $out;
        $in = $out;
    }
    $in;
}

sub readcdslib {
    my ($file) = @_;
    my $root=$file;
    $root =~ s/\/[^\/]+$//;
    open (P, "<$file");
    while (<P>) {
        chomp;
        if (/^DEFINE/) {
            my ($d,$lib,$dir)=split;
            $lib =~ s/#2d/-/g;
            $lib =~ s/#2e/./g;
            $lib =~ s/#24/\$/g;
            print STDERR "$lib" if $lib =~ /#/;
            $libdirs{$lib}="$root/$dir";
        }
    }
}

GetOptions ( %options) or usage;

if ($conversion ne "cadence") {
    $pid = open2(\*RD, \*WR, "rename --type=cell --from=cadence --to=$conversion");
}

$cell = shift if @ARGV;
if (! defined($lib)) {
    $lib = $cell;
    $lib =~ s/\.[^\.]+\.[^\.]+$//;
}
$pdkdfII = "$pdk/share/Fulcrum/dfII" if defined($pdk) and -d $pdk;

readcdslib ("$pdkdfII/cds.lib");
readcdslib ("$dfIIdir/cds.lib.generated");

$usage = <<EU;
Usage: dfIIhier [options] <cell>
    Options
        cell             :[$cell] (does not need --cell)
        dfII-dir         :[$dfIIdir] root of dfII tree
        fulcrum-pdk-root :[$pdk]
        lib              :[$lib]
        where            :[$find] locate a cell in the hierarchy
        virtuoso_mode    :[$virtuoso_mode] output looks like virtuoso
EU

usage ("invalid dfII-dir [$dfIIdir]") if ! -s "$dfIIdir/cds.lib.generated";
usage ("invalid pdk [$pdk]") if $pdk ne "" and ! -d $pdk;
usage ("cell name required") if ! defined $cell;
$find =~ s/\./\\./g;
$find =~ s/\?/./g;
$find =~ s/\*/.*/g;

my @stack=();

sub printstack {
    my ($cell)=@_;
    my $lvl=0;
    print "=== $cell";
    foreach my $stack (@stack) {
        if ($virtuoso_mode) {
            printf "%*.*s$stack\n", 3*$lvl,3*$lvl," ";
        }
        else {
            my @f=split(/ /,$stack);
            printf "%*.*s$f[1] $f[0] $f[2]\n", 2*$lvl,2*$lvl," ";
        }
        $lvl++;
    }
}

sub subcells {
    my ($lib,$view,$cell)=@_;
    return if $lvl > $maxlvl;
    push @stack, "$cell $lib $view";
    printstack($cell) if $cell =~ /$find/ and defined($find);
    my $dir = $lib;
    my $xcell=convert($cell);
    if ($virtuoso_mode) {
        if ($lvl == 0) {
            print "                                         Design Hierarchy";
            print "****************************************************************************************************";
            print "Library     : $lib";
            print "Cell        : $xcell";
            print "View        : $view";
            print "Stop Level  : $maxlvl";
            print "****************************************************************************************************";
            print "";
        }
        else {
            printf "%*.*s$lib $xcell $view\n", 2*$lvl-2,2*$lvl-2," " if ! defined $find;
        }
    }
    else {
        printf "%*.*s$xcell $lib $view\n", 3*$lvl,3*$lvl," " if ! defined $find;
    }
    $dir =~ s/\./\//g;
    $dir = "$dfIIdir/$dir";
    my $celldir=$cell;
    $celldir =~ s/\./#2e/g;
    $celldir =~ s/-/#2d/g;
    $dir = $libdirs{$lib};
    print STDERR "$lib undefined" if ! defined $dir;
    local (*P,$_);
    if ( open (P, "<$dir/$celldir/$view/pc.db")) {
        while (<P>) {
            chomp;
            my ($sublib,$subcell,$subview)=split;
            if (defined ($subview)) {
                $lvl++;
                subcells($sublib,$subview,$subcell);
                $lvl--;
            }
        }
    }
    else {
        warn "Cannot open $dir/$celldir/$view/pc.db" if $view =~ /\./;
    }
    pop @stack;
}

subcells($lib,$view,$cell);
