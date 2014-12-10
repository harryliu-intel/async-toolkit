#!/usr/intel/bin/perl

use strict;
use Getopt::Long;
use CadenceLib;

sub usage
{
    print STDERR "Usage: check_hierarchy.pl\n";
    print STDERR "                --cdslib=<path to cds.lib> (optional)\n";
    print STDERR "                --library=<library>        (required)\n";
    print STDERR "                --cell=<cell name>         (required)\n";
    print STDERR "                --view=<top-level view>    (optional)\n";
    print STDERR "                --cast-path=<path>         (optional)\n";
    print STDERR "                --classpath=<path>         (optional)\n";
	exit;
}

my $cdslib = "~/cds_wd/cds.lib";
my $library;
my $cell;
my $view = "layout";
my $castpath = "~/hw/cast";
my $classpath = $ENV{CLASSPATH};

GetOptions ("cdslib=s" => \$cdslib,
            "library=s" => \$library,
            "view=s" => \$view,
            "cast-path=s" => \$castpath,
            "classpath=s" => \$classpath,
            "cell=s" => \$cell );

$ENV{CLASSPATH} = $classpath;

if (not defined $library)
{
    print STDERR "ERROR!  You must specify a --library\n";
    usage;
}

if (not defined $cell)
{
    print STDERR "ERROR!  You must specify a --cell\n";
    usage;
}

if( not -f $cdslib )
{
    print STDERR "Could not locate \"$cdslib\"\n\n";
    usage;
}

print STDERR "Getting Cadence instances...\n";
my $layout = CadenceLib::list_instances($cdslib,$library,$cell,$view);
my %cast;
print STDERR "Getting CAST instances...\n";
open JFLAT, "java com.avlsi.tools.jflat.JFlat --cast-version=2 --cast-path=$castpath --tool=instance --cell=$cell |" or die "Could not run jflat";
while(<JFLAT>)
{
    chomp;
    /([^ ]+)\s+([^ ]+)/;
    my $celltype = $1;
    my $inst = $2;
    $inst =~ s/,/][/;
    $cast{$inst} = $celltype;
}
close JFLAT;

my @castonly = ();
my @layoutonly = ();
my @wrongtype = ();

foreach my $inst(keys %{$layout})
{
    push @layoutonly, $inst if( not defined $cast{$inst} );
    push @wrongtype, $inst if( defined $cast{$inst} and ($layout->{$inst} ne $cast{$inst}));
}
foreach my $inst(keys %cast)
{
    push @castonly, $inst if( not defined $layout->{$inst} );
}

print "Instances found only in layout:\n";
foreach my $inst (sort @layoutonly)
{
    print "  $inst\n";
}
print "\n\n";
print "Instances found only in cast:\n";
foreach my $inst (sort @castonly)
{
    print "  $inst\n";
}
print "\n\n";
print "Cell type mismatches:\n";
foreach my $inst (sort @wrongtype)
{
    print "  $inst:  CAST has $cast{$inst}  LAYOUT has $layout->{$inst}\n";
}
