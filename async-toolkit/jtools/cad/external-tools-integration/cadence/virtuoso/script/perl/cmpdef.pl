#!/usr/intel/bin/perl -l
# AAG
# $Id: cmpdef.pl,v 1.3 2008/01/23 16:38:12 aubrey Exp aubrey $
# $DateTime$

use strict;
use Getopt::Long;

my $all=0;
my $doext=1;
my $verbose=0;
my $progress=1;
my $spardir="";
my $vialefargcnt=0;
my %fills=();
my @vialef = (
    "\$spardir/chip/bali/lef/multi_cut_via.lef",
    "\$spardir/chip/bali/lef/non_default_rules.lef",
    "\$spardir/vendor/artisan/hs/gates/sc-x/lef/tsmc13fsg_hs_8lm.lef",
);

select STDERR;
$|=1;
select STDOUT;

sub usage {
    my ($msg)=@_;
    print STDERR "$msg" if $msg ne "";
    print STDERR <<EU;
Usage: cmpdef [options] def1 def2
    options
    --all            : intermediate dat files are more complete
    --noext          : do not include path extensions in comparison
    --verbose        : show more details
    --spar-dir=<dir> : root of spar tree to find default via lefs
    --vialef=<list>  : root of spar tree to find default via lefs

Notes:
This checks only the VIA, SPECIALNETS, FILLS, and NETS sections for now.

In FILLS it checks only RECT and not POLY.
EU
exit 1;
}

GetOptions (
    "all" => \$all,
    "noext" => sub { $doext = 0; },
    "verbose" => \$verbose,
    "spar-dir=s" => \$spardir,
    "vialef=s" => sub {
        @vialef=() if ($vialefargcnt == 0);
        push @vialef, split(/,/,$_[1]);
        $vialefargcnt++;
    },
) or usage;

usage "Too many (or too few) files." if $#ARGV != 1;

warn "Warning: should specify spar-dir"
    if $spardir eq "" and $vialef[0] =~ /\$spardir/;
warn "Warning: spar-dir does not exist $spardir" if ! -d $spardir;
foreach my $lef (@vialef) {
    eval "\$lef=\"$lef\"";
    warn "Warning: LEF file $lef does not exist" if ! -s $lef;
}

my ($file0,$file1)=@ARGV;


usage "$file0 does not exist." if ! -s $file0;
usage "$file1 does not exist." if ! -s $file1;

my $nr=0;
my %comp;
my %nets;
my %specialnets;
my %unmatched;
my %sunmatched;
my %vias=();
my %viadefined=();
my %netvias=();
my %netsegments=();
my @instances=();

my %start = (
    "NEW" => 1,
    "ROUTED" => 1,
    "FIXED" => 1,
    "COVER" => 1,
    "SHIELD" => 1,
);

my %skip = (
    "CLOCK" => 1,
    "DIST" => 1,
    "FIXEDBUMP" => 1,
    "GROUND" => 1,
    "NETLIST" => 1,
    "ORIGINAL" => 2,
    "PATTERN" => 2,
    "POWER" => 1,
    "PROPERTY" => 3,
    "SOURCE" => 1,
    "TAPER" => 1,
    "TIMING" => 1,
    "USE" => 2,
    "USER" => 1,
    "VOLTAGE" => 2,
    "WEIGHT" => 2,
);

local (*P, $_);

sub readvialef {
    my ($file)=@_;
    open (X, "<$file");
    while (<X>) {
        chomp;
        s/^[ \t]*//;
        if (/^VIA /) {
            my @f=split;
            $viadefined{$f[1]}=3;
        }
    }
    close X;
}

sub readvias {
    my @f;
    while (<P>) {
        chomp;
        last if (/^END VIAS/);
        my $lx=$_;
        my $mask = 1 << $nr;
        if (/^ *-/) {
            while (! ($lx =~ /;/)) {
                $_=<P>;
                chomp;
                s/^ *\+ */ /;
                $lx .= $_;
            }
        }
        $lx =~ s/  */ /g;
        $lx =~ s/^ *//;
        $lx =~ s/ $//;
        @f=split(/ /, $lx);
        shift @f;
        my $name=shift @f;
        my $geom = join " ", @f;
        $vias{$geom}[$nr]=$name;
        $viadefined{$name} |= $mask;
    }
}

sub readcomp {
    my @f;
    while (<P>) {
        chomp;
        last if (/^END COMP/);
        my $lx=$_;
        if (/^ *-/) {
            while (! ($lx =~ /;/)) {
                $_=<P>;
                chomp;
                s/^ *\+ */ /;
                $lx .= $_;
            }
        }
        $lx =~ s/  */ /g;
        $lx =~ s/^ *//;
        $lx =~ s/ $//;
        @f=split(/ /, $lx);
        shift @f;
        my $inst=shift @f;
        my $place = join " ", @f;
        $comp{$place}[$nr]=$inst;
    }
}

sub readpins {
    while (<P>) {
        chomp;
        last if (/^END PIN/);
    }
}

sub readblockages {
    while (<P>) {
        chomp;
        last if (/^END BLOCK/);
    }
}

sub readfills {
    my $mask = 1 << $nr;
    while (<P>) {
        chomp;
        s/  */ /g;
        s/^ //;
        last if (/^END FILL/);
        my $layer;
        my @data=();
        push @data, $_;
        while (! /;/) {
            $_=<P>;
            chomp;
            s/  */ /g;
            s/^ //;
            push @data, $_;
        }
        my $lx = join (" ", @data);
        $lx =~ s/  */ /g;
        $lx =~ s/^ //;
        @data = split(/ /,$lx);
        shift @data;
        my $layer;
        while ($data[0] ne "") {
            if ($data[0] eq "LAYER") {
                shift @data;
                $layer = shift @data;
                while ($data[0] ne "RECT" and $data[0] ne "POLYGON" and $data[0] ne "") {
                    shift @data;
                }
            }
            elsif ($data[0] eq "RECT") {
                my $type = shift @data;
                shift @data;
                my $x0 = shift @data;
                my $y0 = shift @data;
                shift @data;
                shift @data;
                my $x1 = shift @data;
                my $y1 = shift @data;
                shift @data;
                $fills{"$layer RECT $x0 $y0 $x1 $y1"} |= $mask;
            }
            elsif ($data[0] eq ";") {
                last;
            }
            else {
                print "$data[0]";
                exit 1;
            }
        }
    }
}

sub readspecialnets {
    my @f;
    my @data=();
    while (<P>) {
        chomp;
        last if (/^END SPECIALNE/);
        push @data, $_;
        if (/^ *-/) {
            while (! /;/) {
                $_=<P>;
                chomp;
                s/^ *\+ */ /;
                s/  */ /g;
                push @data, $_;
            }
        }
        my $lx = join(" ", @data);
        @data=();
        $lx =~ s/  */ /g;
        $lx =~ s/^ *//;
        $lx =~ s/ $//;
        @f=split(/ /, $lx);
        shift @f;
        my $net=shift @f;
        my $route = join " ", @f;
        $specialnets{$route}[$nr]=$net;
    }
}

sub readnets {
    my @f;
    while (<P>) {
        chomp;
        last if (/^END NET/);
        my $lx=$_;
        if (/^ *-/) {
            while (! ($lx =~ /;/)) {
                $_=<P>;
                chomp;
                s/^ *\+ */ /;
                $lx .= $_;
            }
        }
        $lx =~ s/  */ /g;
        $lx =~ s/^ *//;
        $lx =~ s/ $//;
        @f=split(/ /, $lx);
        shift @f;
        foreach my $f (@f) {
            $f = "ROUTED" if ($f eq "FIXED");
        }
        my $net=shift @f;
        my $route = join " ", @f;
        $nets{$route}[$nr]=$net;
    }
}

sub readdef {
    my ($file) = @_;
    if ($file =~ /\.gz/) {
        open (P, "gunzip -c $file |");
    }
    else {
        open (P, "<$file");
    }
    printf STDERR "Reading $file..." if $progress;
    while (<P>) {
        chomp;
        s/^  *//;
        readvias() if /^VIAS /;
        readnets() if /^NETS /;
        readspecialnets() if /^SPECIALNETS /;
        readcomp() if /^COMPONENTS /;
        readfills() if /^FILLS /;
    }
    close P;
    print STDERR "Done" if $progress;
}

foreach my $lef (@vialef) {
    readvialef ($lef);
}

readdef ($file0);
$nr++;
readdef ($file1);

# nets comparison
my $mismatchcnt=0;
printf STDERR "Finding mismatches..." if $progress;
foreach my $route (sort keys %nets) {
    if (! defined $nets{$route}[1] ) {
        $unmatched{$nets{$route}[0]}[0]=$route;
        $mismatchcnt++;
    }
    elsif (! defined $nets{$route}[0] ) {
        $unmatched{$nets{$route}[1]}[1]=$route;
        $mismatchcnt++;
    }
    elsif ($all) {
        $unmatched{$nets{$route}[0]}[0]=$route;
        $unmatched{$nets{$route}[1]}[1]=$route;
    }
}
$mismatchcnt /= 2;
print STDERR " found $mismatchcnt mismatches Done" if $progress;

undef %nets;
$mismatchcnt=0;
#specialnets comparison
printf STDERR "Finding specialnets mismatches..." if $progress;
foreach my $route (sort keys %specialnets) {
    if (! defined $specialnets{$route}[1] ) {
        $sunmatched{$specialnets{$route}[0]}[0]=$route;
        $mismatchcnt++;
    }
    elsif (! defined $specialnets{$route}[0] ) {
        $sunmatched{$specialnets{$route}[1]}[1]=$route;
        $mismatchcnt++;
    }
    elsif ($all) {
        $sunmatched{$specialnets{$route}[0]}[0]=$route;
        $sunmatched{$specialnets{$route}[1]}[1]=$route;
    }
}
undef %specialnets;
$mismatchcnt /= 2;
print STDERR " found $mismatchcnt mismatches Done" if $progress;


my $data0=$ARGV[0];
$data0 =~ s/\.def/.dat/;
my $data1=$ARGV[1];
$data1 =~ s/\.def/.dat/;
open (P0, ">$data0");
open (P1, ">$data1");

printf STDERR "Finding Fills Mismatches...";
foreach my $geom (sort keys %fills) {
    if ($fills{$geom} & 1) {
        print P0 "FILLS $geom";
    }
    if ($fills{$geom} & 2) {
        print P1 "FILLS $geom";
    }
}
print STDERR "Done";

printf STDERR "Finding Via Definition Mismatches...";
my $cvm=0;
foreach my $geom (sort keys %vias) {
    if (! defined ($vias{$geom}[1]) ) {
        print P0 "VIA $vias{$geom}[0] $geom";
    }
    elsif (! defined ($vias{$geom}[0])) {
        print P1 "VIA $vias{$geom}[1] $geom";
    }
    elsif ($all) {
        print P0 "VIA $vias{$geom}[0] $geom";
        print P1 "VIA $vias{$geom}[1] $geom";
    }
}
print STDERR "Done";

printf STDERR "Reporting %s special net data...", $all ? "all" : "mismatched"
    if $progress;
$mismatchcnt=0;
foreach my $net (sort keys %sunmatched) {
    $mismatchcnt++;
    sreport($net);
}
print STDERR " reported $mismatchcnt nets Done" if $progress;

printf STDERR "Reporting %s net data...", $all ? "all" : "mismatched" if $progress;
$mismatchcnt=0;
foreach my $net (sort keys %unmatched) {
    $mismatchcnt++;
    report($net);
}
print STDERR " reported $mismatchcnt nets Done" if $progress;

close P0;
close P1;
sleep 1;

open (P, "diff '$data0' '$data1' |");
my %difnets=();
my %diflay=();
my %difvia=();
my %difs=();
my $cmp=0;
while (<P>) {
    chomp;
    my @f=split;
    my $dir = 0;
    my $dir = 1 if $f[0] eq ">";
    if ($f[0] =~ /^[<>]$/) {
        $difs{$f[1]}++;
        $difnets{$f[2]}[$dir]=1 if $f[1] eq "NETS" or $f[1] eq "SPECIALNET";
        if (($f[1] eq "NETS" or $f[1] eq "SPECIALNET") and ($f[3] =~ /^METAL\d+$/ or $f[3] =~ /^M\d+$/)) {
            $diflay{$f[3]}[$dir]=1;
        }
        elsif ($f[1] eq "FILLS" and ($f[2] =~ /^METAL\d+$/ or $f[2] =~ /^M\d+$/)) {
            $diflay{$f[2]}[$dir]=1;
        }
        elsif ($f[1] eq "SPECIALNET" and ! ($f[3] =~ /^METAL\d+$/ or $f[3] =~ /^M\d+$/)) {
            $difvia{$f[3]}[$dir]=1;
        }
        elsif ($f[1] eq "NETS") {
            $difvia{$f[3]}[$dir]=1;
        }
        $cmp++;
    }
}
close P;
my $cd=0;
foreach my $net (sort keys %difs) {
    $cd++;
}
print STDERR "Difference Sections ($cd)";
foreach my $section (sort keys %difs) {
    print "  $section";
}
my $cn=0;
foreach my $net (sort keys %difnets) {
    $cn++;
}
print STDERR "Difference Nets ($cn)";
foreach my $net (sort keys %difnets) {
    print "  $net";
}
my $cl=0;
foreach my $lay (sort keys %diflay) {
    $cl++;
}
print STDERR "Difference Layers ($cl)";
foreach my $lay (sort keys %diflay) {
    print "  $lay";
}
my $cv=0;
foreach my $via (sort keys %difvia) {
    $cv++;
}
print STDERR "Difference Vias ($cv)";
foreach my $via (sort keys %difvia) {
    print "  $via";
}
if ($cmp > 0) {
    print STDERR "diff '$data0' '$data1' to see details";
}
exit ($cmp);

sub generate {
    my ($net, $fn) = @_;
    my $r0 = $unmatched{$net}[$fn];
    my @r0=split(/ /,$r0);
    my $n0=shift @r0;
    my $n0;
    my $ndr0="";
    my $bgnext="";
    my $endext="";
    my $mask = 1 << $fn;
    for ($n0 = 0; $n0 < $#r0 and ! $start{$r0[$n0]}; $n0++) {
        my $x = $r0[$n0];
        if ($x eq "NONDEFAULTRULE") {
            $ndr0 = $r0[$n0+1];
            $n0 += 2;
            $x = $r0[$n0];
        }
        push @{$instances[$fn]}, $x if $x ne '(' and $x ne ')';
    }
    my $x;
    my $y;
    my @xy=();
    # make a list of segments and vias
    for ($n0++; $n0 < $#r0 and $r0[$n0] ne ';';) {
        my $layer = $r0[$n0];
        $bgnext=$endext="";
        if (! $start{$layer} and ! ($layer =~ /^METAL/)) {
            $netvias{"$layer $xy[$#xy-1] $xy[$#xy]"} |= $mask;
            $layer = $r0[++$n0];
        }
        @xy=();
        $layer = $r0[++$n0] if $start{$layer};
        $layer =~ s/METAL//;
        my $ndr=$ndr0;
        if ($r0[$n0+1] eq "TAPER") {
            $ndr="";
            $n0++;
        }
        while ( $r0[++$n0] eq '(') {
            my $x0 = $r0[++$n0];
            my $y0 = $r0[++$n0];
            $y0 = $y if ($y0 eq "*");
            $x0 = $x if ($x0 eq "*");
            push @xy, $x0;
            push @xy, $y0;
            $x = $x0;
            $y = $y0;
            $n0++;
            if ($r0[$n0] ne ')') {
                if ($#xy == 1) {
                    $bgnext = $r0[$n0] if $doext;
                }
                else {
                    $endext = $r0[$n0] if $doext;
                }
                $n0++;
            }
        }
        if ($#xy > 2) {
            for (my $n = 0; $n < $#xy-2; $n+=2) {
                if ($n == 0 and $#xy == 3 and $bgnext ne "" and $endext ne "") {
                    $netsegments{"METAL$layer BGN=$bgnext $xy[$n] $xy[$n+1] $xy[$n+2] $xy[$n+3] END=$endext $ndr"} |= $mask;
                }
                elsif ($n == 0 and $bgnext ne "") {
                    $netsegments{"METAL$layer BGN=$bgnext $xy[$n] $xy[$n+1] $xy[$n+2] $xy[$n+3] $ndr"} |= $mask;
                }
                elsif ($n == $#xy-3 and $endext ne "") {
                    $netsegments{"METAL$layer $xy[$n] $xy[$n+1] $xy[$n+2] $xy[$n+3] END=$endext $ndr"} |= $mask;
                }
                else {
                    $netsegments{"METAL$layer $xy[$n] $xy[$n+1] $xy[$n+2] $xy[$n+3] $ndr"} |= $mask;
                }
            }
        }
        while ($skip{$r0[$n0]}) {
            $n0 += $skip{$r0[$n0]};
        }
    }
}

sub report {
    my ($net) = @_;
    %netsegments=();
    %netvias=();
    generate($net,0);
    generate($net,1);
    print "Mismatch instances $net" if join(" ", @{$instances[0]}) ne join(" ",@{$instances[1]});
    foreach my $via (sort keys %netvias) {
        my @f=split(/ /,$via);
        print STDERR "Warning: $f[0] not defined" if $viadefined{$f[0]} != 3;
        $viadefined{$f[0]} = 3;
        print P0 "NET $net $via" if ($netvias{$via} & 1);
        print P1 "NET $net $via" if ($netvias{$via} & 2);
    }
    foreach my $seg (sort keys %netsegments) {
        print P0 "NET $net $seg" if ($netsegments{$seg} & 1);
        print P1 "NET $net $seg" if ($netsegments{$seg} & 2);
    }
}

my %snetsegments;
my %snetvias;

sub sgenerate {
    my ($net,$fn)=@_;
    my $r0 = $sunmatched{$net}[$fn];
    my @r0=split(/ /,$r0);
    my $mask = 1 << $fn;
    my $x;
    my $y;
    my @xy=();
    my $n0 = 0;
    # skip to ROUTED/NEW/etc
    while ($r0[0] ne "" and ! $start{$r0[0]}) {
        shift @r0;
    }
    # make a list of segments and vias
    my $total=$#r0;
    for ($n0++; $n0 < $#r0 and $r0[$n0] ne ';';) {
        while (! ($start{$r0[$n0]}) and $r0[$n0] ne "") {
            $n0++;
        }
        last if $r0[$n0] eq "";
        $n0++ if $start{$r0[$n0]};
        my $layer = $r0[$n0];
        @xy=();
        $layer = $r0[++$n0] if $start{$layer};
        print STDERR "$layer" if ! $layer =~ /^METAL/;
        $layer =~ s/METAL//;
        my $width = $r0[++$n0];
        $n0++;
        if ($r0[$n0] ne "+" and $r0[$n0] ne "(") {
            printf STDERR "Expected '+' at $n0 of $net found:\n  ";
            for(my $n = $n0-10; $n < $n0+5;$n++) {
                if ($n == $n0) {
                    printf STDERR ":$r0[$n]: ";
                }
                else {
                    printf STDERR "$r0[$n] ";
                }
            }
            print STDERR "";
        }
        while ($r0[$n0] ne "" and $r0[$n0] ne "(") {
            $n0++;
        }
        $n0--;
        while ( $r0[++$n0] eq '(') {
            my $x0 = $r0[++$n0];
            my $y0 = $r0[++$n0];
            $y0 = $y if ($y0 eq "*");
            $x0 = $x if ($x0 eq "*");
            push @xy, $x0;
            push @xy, $y0;
            $x = $x0;
            $y = $y0;
            $n0++;
            if ($r0[$n0] ne ')') {
                print STDERR "Expected ')' at $n0 in $net";
            }
        }
        if ($#xy > 2) {
            for (my $n = 0; $n < $#xy-2; $n+=2) {
#                print STDERR "METAL$layer $width $xy[$n] $xy[$n+1] $xy[$n+2] $xy[$n+3]";
                $snetsegments{"METAL$layer $width $xy[$n] $xy[$n+1] $xy[$n+2] $xy[$n+3]"} |= $mask;
            }
        }
        elsif ($#xy == 1) {
#            print STDERR "VIA $r0[$n0]";
            $snetvias{"$r0[$n0] $xy[0] $xy[1]"} |= $mask;
        }
        while ($skip{$r0[$n0]}) {
            $n0 += $skip{$r0[$n0]};
        }
    }
}

sub sreport {
    my ($net) = @_;
    %snetsegments=();
    %snetvias=();
    sgenerate($net,0);
    sgenerate($net,1);
    foreach my $via (sort keys %snetvias) {
        my @f=split(/ /,$via);
        print STDERR "Warning: $f[0] not defined" if $viadefined{$f[0]} != 3;
        $viadefined{$f[0]} = 3;
        print P0 "SPECIALNET $net $via" if ($snetvias{$via} & 1);
        print P1 "SPECIALNET $net $via" if ($snetvias{$via} & 2);
    }
    foreach my $seg (sort keys %snetsegments) {
        print P0 "SPECIALNET $net $seg" if ($snetsegments{$seg} & 1);
        print P1 "SPECIALNET $net $seg" if ($snetsegments{$seg} & 2);
    }
}
