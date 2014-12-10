#!/usr/intel/bin/perl -l
# AAG
# $Id$
# $DateTime$

use strict;
use Getopt::Long;

my $verbose=0;
my $cell;
my $specdir;
my $castdir;
my $castpath;
my $wrap=-1;
my $maxheap="1800M";
my $check=0;

sub usage {
print "Usage $_[0]";
exit 1;
}

my %options = (
    "verbose" => \$verbose,
    "cell=s" => \$cell,
    "spec-dir=s" => \$specdir,
    "cast-dir=s" => \$castdir,
    "cast-path-s" => \$castpath,
    "wrap=i" => \$wrap,
    "max-heap-size=s" => \$maxheap,
    "check" => \$check,
);

GetOptions ( %options );

$cell=shift if ! defined $cell;
usage "cell" if ! defined $cell;
$cell =~ s/\//\./g;
$specdir=$castpath if ! defined $specdir;
$specdir =~ s/.*://;
$castdir=$castpath if ! defined $castdir;
$castdir =~ s/:.*//;
usage "$specdir" if ! -d $specdir;
$castpath="$castdir:$specdir" if ! defined $castpath;
my $specfile="$specdir/$cell";
$cell =~ /\.([^\.]+)$/;
my $subtype=$1;
$specfile =~ s/\./\//g;
$specfile .= ".cast";
my $specdir=$specfile;
$specdir =~ s/\/[^\/]+$//;
opendir (D, "$specdir");
my @f=sort(grep(/..\.cast$/, readdir(D)));
closedir D;
if ($#f > 0) {
    print STDERR "Error: more than one subtype, wrapping dangerous!";
    exit 1;
}
my $basecell=$cell;
$basecell=~s/\.[^\.]+$//;
open (P, "<$specfile") or die "Cannot open $specfile";
my @lines;
my @refine;
my @include=();
my $module;
my $subcells=0;
my @directives=();
my $prevdone=0;
my $prevlook=$basecell."_a";
while (<P>) {
    chomp;
    my $l=$_;
    if (/^module\s+(\S+)\s*;/) {
        $module=$1;
    }
    if (/^include/) {
        push @include,$_;
    }
    $subcells=1 if /\ssubcells\s/;
    while ($l =~ s/(<[+:\s]+\S+)//) {
        push @refine,$1;
    }
    if (/^\s*directives/) {
        push @directives, $_;
        push @lines, $_;
        while (<P>) {
            chomp;
            push @directives, $_;
            push @lines, $_;
            last if /^\s*\x7d/;
        }
        next;
    }
    if (/$prevlook/) {
        $prevdone=1;
        print STDERR "Error: wrap already done on this cell";
        exit 1;
    }
    push @lines, $_;
}
close P;
if ( ! -f "$cell.query") {
    system "cast_query --cast-path='$castpath' --max-heap-size=$maxheap --task=external_nodes=im:al,ports --cell='$basecell' --output='$cell.query'";
}
if ( ! -s "$cell.query") {
    print STDERR "Query failed for $cell";
    exit 1;
}
open (P, "<$cell.query") or die "Cannot open $cell.query";
my @ports=();
my @portnodes=();
my %portnodes=();
while (<P>) {
    chomp;
    my @f=split;
    if ($#f == 0) {
        my @f=split(/=/);
        push @portnodes,@f;
        $portnodes{$f[0]}=[@f];
    }
    elsif (/^[01] /) {
        s/\s+$//;
        push @ports, $_;
    }
}
close P;

my %names=();
foreach my $port (@ports) {
    my $name=$port;
    $name =~ s/.*\s//;
    $name =~ s/^[-+]//;
    $names{$name}=1;
}
sub canonical {
    my @ad=split(/\./, $a);
    my @bd=split(/\./, $b);
    if ($#ad != $#bd) {
        return $#ad-$#bd;
    }
    $a cmp $b;
}
# determine wrap required
my $needwrap=0;
foreach my $node (sort keys %portnodes) {
    my $w=0;
    my @f=();
    foreach my $nd (@{$portnodes{$node}}) {
        foreach my $name (sort keys %names) {
            if ($name eq $nd or $nd =~ /^$name\./) {
                push @f, $nd;
            }
        }
    }
    @f=sort canonical @f;
    if ($f[0] ne $node) {
        my $n=$node;
        my @w=sort canonical ($f[0],$node);
        while ($w[0] ne $f[0]) {
            $w++;
            $n = "z.$n";
            @w=sort canonical ($f[0], $n);
        }
        $needwrap=$w if $w > $needwrap;
    }
}
$wrap=$needwrap if $wrap<=0;
if ($wrap == 0) {
    print STDERR "No wrap required for $cell";
    exit 0;
}
else {
    print STDERR "Wrap of $wrap required for $cell";
    exit $wrap if $check;
}
foreach my $r (@refine) {
    $r =~ s/\s+/ /g;
    $r =~ s/^\s//;
    $r =~ s/\s$//;
}
my $basemodule=$module;
my $param="";
if ($basemodule =~ /(\S+)\((\S+)\)/) {
    $param="($2)";
    $basemodule=$1;
}
my $afile=$specfile;
$afile =~ s/\/[^\/]+$/\/a.cast/;
# do the 'a' cell, this contains the original content
open (P, ">$afile") or die "opening $afile: $!";
foreach my $l (@lines) {
    $_=$l;
    s/\s*$//;
    if (/^\s*define\s+.*\x7b/) {
        $_ = "define \"a\"() <: ${basemodule}_a$param \x7b";
        print P;
        next;
    }
    if (/^\s*define\s.*\($/) {
        $_ = "define \"a\"() (";
        print P;
        next;
    }
    if (/^\s*define\s/) {
        $_ = "define \"a\"()";
        print P;
        next;
    }
    s/<\+ routed/<+ unrouted/;
    if (/<:.*$basemodule/) {
        s/$basemodule/${basemodule}_a/;
    }
    print P;
}
print STDERR "$afile written" if $verbose;
my $topcast="$specfile.tmp";
# do the main cell
open (P, ">$topcast");
print P "";
print P "module $module;";
print P join("\n", @include);
print P "";
my $suffix=pack("c",96+$wrap);
print P "define \"$subtype\" () ".join(" ", @refine)." \x7b";
if ($subcells) {
    print P "  subcells \x7b";
    print P "    $module.$suffix z(";
    print P "    );";
    print P "  \x7d";
}
else {
    # subtypes
    print P "  subtypes \x7b";
    print P "    ${basemodule}_$suffix$param :>";
    print P "      ${basemodule}$param.$suffix z;";
    print P "  \x7d";
    print P join("\n", @directives);
}
print P "\x7d";
close P;
print STDERR "$topcast written" if $verbose;
# do the rest of the cells if needed
for(my $n=$wrap; $n > 1; $n--) {
    my $suffix1=pack("c",98+$wrap-$n);
    my $suffix2=pack("c",97+$wrap-$n);
    my $afile=$specfile;
    $afile =~ s/\/[^\/]+$/\/$suffix1.cast/;
    # do the 'a' cell, this contains the original content
    open (P, ">$afile") or die "opening $afile: $!";
    print P "";
    print P "module $module;";
    print P join("\n", @include);
    print P "";
    print P "define \"$suffix1\" () <: ${basemodule}_$suffix1$param \x7b";
    if ($subcells) {
        print P "  subcells \x7b";
        print P "    $module.$suffix1 z(";
        print P "    );";
        print P "  \x7d";
    }
    else {
        # subtypes
        print P "  subtypes \x7b";
        print P "    ${basemodule}_$suffix2$param :>";
        print P "      ${basemodule}$param.$suffix2 z;";
        print P "  \x7d";
        print P join("\n", @directives);
    }
    print P "\x7d";
    print STDERR "$afile written" if $verbose;
}
my @base=();
my $found=-1;
my @castmodule=();
if (! $subcells) {
    #must re-write the base cast
    my $basecast=$cell;
    $basecast =~ s/\.([^\.]+)\.[^\.]+$//;
    my $basename=$1;
    $basename =~ s/\(.*//;
    $basecast =~ s/\./\//g;
    $basecast = "$castdir/$basecast.cast";
    open (P, "<$basecast") or die "$basecast : $!";
    my $in=0;
    my $last="x";
    while (<P>) {
        chomp;
        if (/define\s+"?$basename"?[\s\(]/) {
            s/$basename/${basename}_a/;
            push @castmodule, $_;
            while (<P>) {
                chomp;
                push @castmodule, $_;
                last if /^\x7d/;
            }
            next;
        }
        push @base, $_ if $last ne "" or $_ ne "";
        $last=$_;
    }
    close P;
    open (P, ">$basecast.tmp");
    print P join("\n", @base);
    print P "";
    print P join("\n", @castmodule);
    print P "";
    for(my $n=$wrap; $n > 0; $n--) {
        my $suffix1="_".pack("c",97+$wrap-$n);
        my $suffix2="_".pack("c",98+$wrap-$n);
        $suffix2="" if $n==1;
        my $define=$castmodule[0];
        $define =~ s/${basename}_a/${basename}$suffix2/;
        $define =~ s/<.*//;
        if ($define =~ /\(.*\).*\(.*\)/) {
            $define =~ s/\s*\([^\(]+\)\s*$//;
        }
        $define =~ s/\s+$//;
        $define =~ s/\s*\($//;
        print P "$define (";
        my @declare=();
        foreach my $port (@ports) {
            next if ($port =~ /^1/);
            my $declare=$port;
            $declare =~ s/^0 //;
            push @declare, $declare;
        }
        print P "    ".join(";\n    ", @declare);
        print P "    ) <+ unrouted \x7b";
        print P "  subcells \x7b";
        my $cn=$castmodule[0];
        $cn =~ s/define //;
        $cn =~ s/\s*<.*//;
        $cn =~ s/${basename}_a/${basename}$suffix1/;
        $cn =~ s/"//g;
        $cn =~ s/\s*\(\s*$//;
        if ($cn =~ /\(.*\(/) {
            $cn =~ s/\s*\([^\(]+\)\s*$//;
        }
        $cn =~ s/\(\s*int\s+/(/;
        $cn =~ s/\s//g;
        $cn =~ s/\s*\(\s*\)\s*$//;
        print P "    ${cn} z(";
        @declare=();
        foreach my $port (@ports) {
            next if ($port =~ /^1/);
            my $declare=$port;
            $declare =~ s/.*\s//;
            $declare =~ s/^[-+]//;
            push @declare, $declare;
        }
        print P "      ".join(",\n      ", @declare);
        print P "    );";
        print P "  \x7d";
        print P "\x7d";
    }
    close P;
    print STDERR "$basecast.tmp written" if $verbose;
}
print STDERR "Warning: You must rename the tmp files to implement";
