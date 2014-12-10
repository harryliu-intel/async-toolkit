#!/usr/intel/bin/perl -l
# AAG
# $Id$
# $DateTime$

use strict;
use Getopt::Long;
my $verbose=0;

sub usage {
    my ($msg)=@_;
    print STDERR "$msg" if defined $msg;
    print STDERR <<EU;
Usage: df2d <options>
    --dfII-dir=<dir>      : the dfII-dir
    --cell=<cellname>     : the cell cast name
    --view=<viewname>     : the dfII cellview
    --lib=<libname>       : the dfII libname
    --fulcrum-pdk-root    : the pdk root
    --output-file=<df2,d> : the lve root directory
EU
exit 1;
}

my $dfIIdir="/mnt/fulcrum/bali/aubrey/bali/hw/layout/tsmc13/dfII";
my $cell;
my $view="layout";
my $lib;
my %libdir=();
my $outputfile;
my $pdk_root;
my @errs=();
my %skiplibs=("gate"=>1, "stack"=>1, "tsmc28"=>1);

GetOptions (
    "dfII-dir=s" => \$dfIIdir,
    "fulcrum-pdk-root=s" => \$pdk_root,
    "view=s" => \$view,
    "lib=s" => \$lib,
    "output-file=s" => \$outputfile,
    "cell=s" => \$cell,
    "verbose" => \$verbose,
) or usage;

$dfIIdir =~ s/\/$//;
defined $ARGV[0] or defined $cell or usage;
$cell = $ARGV[0] if defined $ARGV[0] and ! defined $cell;
-d $dfIIdir and -f "$dfIIdir/cds.lib.generated" or usage("Invalid dfII-dir");
length($view) or usage("View name seems invalid");
$lib=$cell;
$lib =~ s/\.[^\.]+\.[^\.]+$//;
length($lib) or usage("Lib name seems invalid");

open (P, "<$dfIIdir/cds.lib.generated") or usage("Cannot open cds.lib.generated");
while (<P>) {
    chomp;
    next if (! /^DEFINE/);
    my ($d,$lib,$dir)=split;
    $lib =~ s/#2e/./g;
    $lib =~ s/#2d/-/g;
    $lib =~ s/#24/\$/g;
    $libdir{$lib}=$dir;
}
close P;

sub cast2cadence {
    my ($lib,$name,$view)=@_;
    $name =~ s/\./#2e/g;
    $name =~ s/-/#2d/g;
    $name =~ s/\$/#24/g;
    if ( defined $libdir{$lib}) {
        $lib = $libdir{$lib};
    }
    else {
        push @errs, "Lib $lib does not exist in cds.lib.generated";
        $lib = "unknown";
    }
    "$dfIIdir/$lib/$name/$view";
}

my %cdbs=();

my %done=();
my $template=<<ET;
foutfile=outfile("tempskill.out")
(defun getCellMasters ( libName cellName viewName )
    (let (cellView)
        cellView=nrOpenCellViewReadable(libName cellName viewName)
        if( cellView 
        foreach( im cellView~>instanceMasters
            fprintf(foutfile "%s %s %s\\n" im~>libName im~>cellName im~>viewName)
            getCellMasters(im~>libName im~>cellName im~>viewName )))
        t
    )
)
getCellMasters( "LIBNAME" "CELLNAME" "VIEWNAME" )
close(foutfile)
ET
sub search {
    my ($lib, $cell, $view)=@_;
    return if (($lib =~ /^tsmc/) or $lib eq "stack" or $lib eq "gate");
    return if $done{"$lib:$cell:$view"};
    $done{"$lib:$cell:$view"}=1;
    my $df=cast2cadence($lib,$cell,$view);
    local (*P, $_);
    if ( -s "$df/layout.oa") {
        my @stat=stat("$df/layout.oa");
        my $mtimely=$stat[9];
        my $mtimedb=0;
        if ( -f "$df/pc.db") {
            @stat=stat("$df/pc.db");
            $mtimedb=$stat[9];
        }
        if ($mtimedb != $mtimely) {
            my $skill=$template;
            my $temp=$ENV{TMP};
            my $user=`whoami`;
            chomp $user;
            $temp="/scratch/$user" if $temp eq "";
            `mkdir -p $temp`;
            $temp=`mktemp -d "$temp/df2d.XXXXXX"`;
            chomp $temp;
            `mkcdswd --dfII-dir="$dfIIdir" --fulcrum-pdk-root="$pdk_root" --target-dir="$temp" --temp`;
            $skill =~ s/LIBNAME/$lib/;
            $skill =~ s/CELLNAME/$cell/;
            $skill =~ s/VIEWNAME/$view/;
            open (P, ">$temp/tempskill.il");
            print P $skill;
            close P;
            $cdbs{"$df/layout.oa"}=1;
            system("cd '$temp'; virtuoso -nograph -replay tempskill.il -log /dev/null >/dev/null 2>&1");
            system("/bin/sort","-u", "$temp/tempskill.out","-o", "$df/pc.db");
            system("/bin/touch","-r","$df/layout.oa","$df/pc.db");
            system("/bin/rm","-rf","$temp");
        }
        open (P, "<$df/pc.db");
        while (<P>) {
            chomp;
            next if (/^#/);
            my ($lib,$cell,$view)=split;
            next if $skiplibs{$lib};
            $df=cast2cadence($lib,$cell,$view);
            if ( -s "$df/layout.oa") {
                $cdbs{"$df/layout.oa"}=1;
            }
        }
        close P;
    }
    else {
        push @errs, "Not found in dfIIdir: df=$df lib=$lib cell=$cell view=$view";
    }
}

my $cadcell=`echo '$cell' | rename --max-heap-size=80m --type=cell --from=cast --to=cadence`;
chomp $cadcell;
search ($lib, $cadcell, $view);

my $celldir=$cell;
$celldir =~ s:\.:/:g;
if (open(P, ">$outputfile")) {
    select P;
}
print "CDBDEP_TARGET_FILES := $outputfile ";
print "\$(CDBDEP_TARGET_FILES): \\";
my $top = cast2cadence($lib,$cadcell,$view);
$top =~ s/#/\\#/g;
print "$top/layout.oa \\";
foreach my $cdb (sort keys %cdbs) {
    $cdb =~ s/#/\\#/g;
    print "$cdb \\" if ($cdb ne "$top/layout.oa");
}
if (@errs) {
    print STDERR "Error: ".join("\nError: ", @errs);
    exit 1;
}
0;
