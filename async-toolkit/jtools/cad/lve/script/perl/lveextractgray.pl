#!/usr/intel/bin/perl -l
# $Id$
# $DateTime$

use strict;
use Getopt::Long;

my %cells=();
my %need=();
my %leaf=();
my $options="";
my $task="extract";
my $lvedir="lve";
my %done=();
my $verbose=0;
my $dfIIdir="/mnt/fulcrum/alta/aubrey/alta/dfII";
my $castdir;
my $specdir;
my $latest="";
my $toolhome = "";
my $pdkroot;
my %validtasks = (
    "alint" => 1,
    "aspice" => 1,
    "extract" => 1,
    "lib" => 1,
);

sub usage {
    my ($msg)=@_;
    print STDERR $msg if defined $msg;
    print <<EU;
Usage: lveextractgray [options] cell
    --cast-dir=<path>        : normal cast-dir
    --spec-dir=<path>        : normal spec-dir
    --fulcrum-pdk-root=<dir> : pdk
    --dfII-dir=<directory>   : the dfII directory for the layout info
    --latest                 : use --latest for fulcrum
    --options='string'       : additional lve options
    --output-dir=<lvedir>    : the lve output dir
    --task=<task list>       : lve task list after extract, if any
    --toolhome=<dir>         : the toolhome (implies --latest)
    --verbose                : 
EU
    exit 1;

}

GetOptions (
    "dfII-dir=s" => \$dfIIdir,
    "fulcrum-pdk-root=s" => \$pdkroot,
    "latest" => \$latest,
    "options=s" => \$options,
    "output-dir=s" => \$lvedir,
    "task=s" => \$task,
    "toolhome=s" => \$toolhome,
    "verbose" => \$verbose,
    "cast-dir=s" => \$castdir,
    "spec-dir=s" => \$specdir,
) or usage;

usage "need dfII-dir" if ( ! -d "$dfIIdir");
usage "need cast-dir" if ( $castdir eq "");
usage "need spec-dir" if ( $specdir eq "");
usage "need fulcrum-pdk-root" if ( ! -d $pdkroot );

if ($latest == 1 or -d $toolhome) {
    $latest="--latest";
}
else {
    $latest = "";
}
if (defined ($toolhome) and -d $toolhome) {
    $toolhome = "--toolhome '$toolhome'";
}

my $topcell = shift;

usage if ! defined $topcell;

my @viewhier=( "lvsclean", "layout_tag", "layout_pg", "layout");

sub view {
    my ($cell)=@_;
    $cell =~ m:(.*)\.([^\.]+\.[^\.]+)$:;
    my $lib = $1;
    $lib =~ s/\./\//g;
    my $cdsname=`echo '$cell' | rename --type=cell --from=cast --to=cadence`;
    chomp $cdsname;
    $cdsname =~ s/\./#2e/g;
    $cdsname =~ s/\-/#2d/g;
    $cdsname =~ s/\$/#24/g;
    foreach my $view (@viewhier) {
        if (-s "$dfIIdir/$lib/$cdsname/$view/layout.cdb") {
            return $view;
        }
    }
    print die "Error: no default view found for $cell";
    "layout";
}

sub make {
    my ($makecell)=@_;
    $need{$makecell} =~ s/^  *//;
    $need{$makecell} =~ s/  *$//;
    my @subcells=split(/ /, $need{$makecell});
    my @subcellpaths=@subcells;
    foreach my $subcell (@subcellpaths) {
        my $view = view($subcell);
        $subcell =~ s/\./\//g;
        $subcell = "$lvedir/$subcell/$view/extracted/cell.aspice";
    }
    my $subcell=$makecell;
    my $view = view($makecell);
    $subcell =~ s/\./\//g;
    $subcell = "$lvedir/$subcell/$view/extracted/cell.aspice";
    print "#$makecell : $subcell";
    print "";
    if ($topcell eq $makecell and $task ne "extract") {
        print ".toptask : $subcell";
        print "\tlve --fulcrum-pdk-root='$pdkroot' --output-dir='$lvedir' --cast-dir='$castdir' --spec-dir='$specdir' --dfII-dir='$dfIIdir' $options --task=$task --graybox-mode=routed '$makecell'";
        print "";
    }
    print "$subcell : ".join("\\\n   ", @subcellpaths);
    if ($makecell eq $topcell) {
        print "\tlve --fulcrum-pdk-root='$pdkroot' --output-dir='$lvedir' --cast-dir='$castdir' --spec-dir='$specdir' --dfII-dir='$dfIIdir' $options --task=extract --graybox-mode=routed '$makecell'";
        print "\tif [ -s '$subcell' ]; then touch '$subcell'; fi";
    }
    else {
        print "\tlve --fulcrum-pdk-root='$pdkroot' --output-dir='$lvedir' --cast-dir='$castdir' --spec-dir='$specdir' --dfII-dir='$dfIIdir' $options --task=extract --graybox-mode=routed '$makecell'";
    }
    print "";
}

my @stack=();

# find cells already done
printf STDERR "Finding Subcell Tree... for $topcell" if $verbose;
if ( -f $topcell) {
    open (P, "<$topcell") or die;
    open (O, ">/dev/null");
}
else {
    open (P, "cast_query --cast-path='$castdir:$specdir' --no-header --task=subcell_tree --routed --cell='$topcell'|");
    open (O, ">$topcell.cache");
}
my $top=<P>;
chomp $top;
print O $top;
$topcell=$top;
push @stack,$top;
while (<P>) {
    chomp;
    next if (/\|$/);
    print O;
    s/ //g;
    m:(\|+):;
    my $depth=length($1);
    s/\|//g;
    s/[-+]//g;
    $top=$_;
    $cells{$stack[$depth]} .= " $top";
    $stack[$depth+1]=$top;
    $cells{$top}="" if (! defined ($cells{$top}));
}
close P;
close O;
print STDERR "Done" if $verbose;
my %c;
my @c;
my %n;
my $makefile="Makefile.$$";
print STDERR "Writing $makefile..." if $verbose;
open (M, ">$makefile");
select M;
foreach my $s (sort keys %cells) {
    $leaf{$s} = 1 if ($cells{$s} =~ /^ *$/);
    %c=();
    %n=();
    foreach my $c (split(/ /, $cells{$s})) {
        $c{$c}=1;
        $n{$c}=1 if ! $done{$c};
    }
    $cells{$s} = join(" ", (sort keys %c));
    $need{$s} = join(" ", (sort keys %n)) if (! $done{$s});
}
# needs to be the top rule
make($topcell);
# do all the leaf cells first
my @leaf=(sort keys %leaf);
while (@leaf) {
    my @l=();
    my $n=0;
    for ($n = 0; $n < 50 and @leaf; ) {
        my $c=shift @leaf;
        if (! $done{$c}) {
            push @l, $c;
            $n++;
        }
    }
    if (@l) {
        my @p=@l;
        foreach my $subcell (@p) {
            if ($task eq "gds2") {
                $subcell =~ s/\./\//g;
                $subcell = "$lvedir/$subcell";
                $subcell .= "/layout/cell.gds2";
            }
            else {
                $subcell =~ s/\./\//g;
                $subcell = "$lvedir/$subcell";
                $subcell .= "/layout/extracted/cell.aspice";
            }
        }
        print join(" \\\n", @p)." :";
        print "\tlve --fulcrum-pdk-root='$pdkroot' --output-dir='$lvedir' --cast-dir='$castdir' --spec-dir='$specdir' --dfII-dir='$dfIIdir' $options --task=$task '".join("' '", @l)."'";
        print "";
    }
}
# do the mid level cells
foreach my $s (sort keys %cells) {
    make($s) if ($s ne $topcell and ! defined $leaf{$s} and ! $done{$s});
}
select STDOUT;
close M;
print STDERR "Done" if $verbose;
sleep 1;
my @run=("make","-f","$makefile");
push @run,".toptask" if $task ne "extract";
system @run;
unlink $makefile if ! $verbose;
