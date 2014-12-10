#!/usr/intel/bin/perl -l
# AAG
# $Id$
# $DateTime$

use strict;
use Getopt::Long;

my $verbose=0;
my $debug = 0;
my $topcell="";
# default layer mapping

my %options = (
    "verbose" => \$verbose,
    "debug" => \$debug,
    "topcell=s" => \$topcell,
);
my $prefix="outfile$$";

GetOptions ( %options ) or usage();

my $in = $ARGV[0];
my $out= $ARGV[1];
my $tmpout=$out;
if ($tmpout =~ /\//) {
$tmpout =~ s:/([^/]+$):/tmp$1:;
}
else {
$tmpout = "tmp$tmpout";
}
usage() if ! defined $out;


sub usage {
    my ($msg)=@_;
    print STDERR "$msg" if defined $msg;
    print STDERR <<EU;
Usage: size265 [options] infile outfile
    --topcell=<name>  : top cell name if ambiguous
    --verbose         : what is happening
    --debug           : keep temp files
EU
    exit 1;
}

my %layers=();
my %names=();
my %text=();
my %size=();
my $rul = <<ER;
drcExtractRules(
  layerDefs( "gds2"
      (nwi=layer(3 type(0)))
      (odi=layer(6 type(0)))
      (poi=layer(17 type(0)))
      (ppi=layer(25 type(0)))
      (npi=layer(26 type(0)))
      (coi=layer(30 type(0)))
      (m1i=layer(31 type(0)))
      (m2i=layer(32 type(0)))
      (m3i=layer(33 type(0)))
      (m4i=layer(34 type(0)))
      (m5i=layer(35 type(0)))
      (m6i=layer(36 type(0)))
      (m7i=layer(37 type(0)))
      (rodi=layer(49 type(0)))
      (dmi=layer(50 type(0)))
      (dm3i=layer(50 type(3)))
      (v1i=layer(51 type(0)))
      (v2i=layer(52 type(0)))
      (v3i=layer(53 type(0)))
      (v4i=layer(54 type(0)))
      (v5i=layer(55 type(0)))
      (v6i=layer(56 type(0)))
      (prbndi=layer(108 type(0)))
      (txti=layer(127 type(0)))
      (srwaivei=layer(186 type(0)))
      (srpassi=layer(186 type(1)))
      (m1ti=text(31 type(0)))
      (m2ti=text(32 type(0)))
      (m3ti=text(33 type(0)))
      (m4ti=text(34 type(0)))
      (m5ti=text(35 type(0)))
      (m6ti=text(36 type(0)))
      (m7ti=text(37 type(0)))
  )
  ; this is an attempt to resize OD only for narrow transistors
  a=geomSize(odi -0.059)
  b=geomSize(a 0.059)
  c=geomAndNot(odi b)
  od=geomOr(geomSize(c 0.015) odi)
  ; shrink poly only over expanded OD
  po=geomOr(geomSize(poi -0.009) geomAndNot(poi od))
)
ER

my $rsf = <<EF;
avParameters(?outputErrorLib t ?flagMalformed nil)

outFile( "gds2" "$prefix" "$out"

;  outLayer( a 101 type(0))
;  outLayer( b 102 type(0))
;  outLayer( c 103 type(0))
  outLayer( nwi 3 type(0))
  outLayer( od 6 type(0))
  outLayer( po 17 type(0))
  outLayer( ppi 25 type(0))
  outLayer( npi 26 type(0))
  outLayer( coi 30 type(0))
  outLayer( m1i 31 type(0))
  outLayer( m2i 32 type(0))
  outLayer( m3i 33 type(0))
  outLayer( m4i 34 type(0))
  outLayer( m5i 35 type(0))
  outLayer( m6i 36 type(0))
  outLayer( m7i 37 type(0))
  outLayer( rodi 49 type(0))
  outLayer( dmi 50 type(0))
  outLayer( dm3i 50 type(3))
  outLayer( v1i 51 type(0))
  outLayer( v2i 52 type(0))
  outLayer( v3i 53 type(0))
  outLayer( v4i 54 type(0))
  outLayer( v5i 55 type(0))
  outLayer( v6i 56 type(0))
  outLayer( prbndi 108 type(0))
  outLayer( txti 127 type(0))
  outLayer( srwaivei 186 type(0))
  outLayer( srpassi 186 type(1))
  outLayer( m1ti 31 type(0))
  outLayer( m2ti 32 type(0))
  outLayer( m3ti 33 type(0))
  outLayer( m4ti 34 type(0))
  outLayer( m5ti 35 type(0))
  outLayer( m6ti 36 type(0))
  outLayer( m7ti 37 type(0))
)

avParameters(
  ?inputLayout ( "gds2" "$tmpout" )
  ?cellName "TOPCELL"
  ?rulesFile "sizing$$.rul"
  ?viewName "layout"
  ?overwrite t
  ?avrpt t
  ?workingDirectory "."
  ?runName "$prefix"
)
EF


foreach my $line (split(/\n/, $rul)) {
    $_=$line;
    if (/layer\(/) {
        s/[(=)]/ /g;
        s/  */ /g;
        s/^ //;
        my ($n,$x,$l,$y, $t)=split;
        $layers{"$l $t"}=$n;
        $names{$n}=1;
    }
    elsif (/text\(/) {
        s/[(=)]/ /g;
        s/  */ /g;
        s/^ //;
        my ($n,$x,$l,$y,$t)=split;
        $text{"$l $t"}=$n;
        $names{$n}=1;
    }
    elsif (/=/) {
        s/ *=.*//;
        s/^ *//;
        $names{$_}=1;
    }
}

foreach my $line (split(/\n/, $rsf)) {
    $_=$line;
    if (/outLayer\(/ and ! /^ *;/) {
        s/[()]/ /g;
        s/  */ /g;
        s/^ //;
        my @f=split;
        print STDERR "Undefined outLayer $f[1]" if (! $names{$f[1]});
    }
}

sub ncmp {
    my ($al,$at)=split(/ /,$a);
    my ($bl,$bt)=split(/ /,$b);
    return 1 if $al > $bl;
    return -1 if $al < $bl;
    return 1 if $at > $bt;
    return -1 if $at < $bt;
    0;
}

open (P, "rdgds '$in' |");
open (Q, "| wrgds > '$tmpout'");
my $layer=0;
my $type=0;
my %strname;
my %sname;
my $text=0;
while (<P>) {
    chomp;
    s/^  *//;
    if (/^-?\d+,-?\d+$/) {
        s/ //g;
        my ($x,$y)=split(/,/,$_);
        $x /= 5;
        $y /= 5;
        my $xp = sprintf "%.0f", $x;
        my $yp = sprintf "%.0f", $y;
        $x = 3*$xp;
        $y = 3*$yp;
        $_="$x,$y";
        # checks for 5nm grid.
        print STDERR "Not int x $x line $." if $x != int($x);
        print STDERR "Not int y $y line $." if $y != int($y);
    }
    elsif (/,/) {
        print STDERR "Bad coordinate $_ or string";
    }
    if (/^BOUNDARY/ or /^PATH/) {
        $layer = -1;
        $text=0;
        $type = 0;
    }
    if (/^DATATYPE/) {
        my ($x,$y)=split;
        $type=$y;
        $text=0;
    }
    if (/^LAYER/) {
        my ($x,$y)=split;
        $layer=$y;
    }
    if (/^TEXT$/) {
        $text=1;
    }
    if (/^TEXTTYPE/) {
        my ($x,$y)=split;
        $type=$y;
        $text=1;
    }
    if (/^ENDEL/) {
        if ($layer > 0 and $type >= 0) {
            if (! defined ($layers{"$layer $type"})) {
                print STDERR "Warning: $layer;$type not predefined";
                $layers{"$layer $type"} = "$layer $type";
            }
            if ($text) {
                $text{"$layer $type"} = $layers{"$layer $type"};
            }
        }
        $layer = $type = -1;
        $text=0;
    }
    if (/^STRNAME/) {
        my ($x,$y)=split;
        $strname{$y}=1;
    }
    if (/^SNAME/) {
        my ($x, $y)=split;
        $sname{$y}++;
    }
    print Q;
}
close P;
close Q;
my $cnt=0;
if ($topcell ne "") {
    usage "Topcell $topcell not in GDS file" if ! defined($strname{$topcell});
}
else {
    foreach my $sn (keys %strname) {
        if (! $sname{$sn}) {
            $topcell = $sn;
            $cnt++;
        }
    }
    print STDERR "Ambiguous topcell, using $topcell" if $cnt > 1;
}
usage "No topcell found in $in" if $cnt==0;
$rsf =~ s/TOPCELL/$topcell/;
open P, ">sizing$$.rul";
select P;
print "$rul";
select STDOUT;
close P;
my $dir=".";
open P, ">sizing$$.rsf";
select P;
print "$rsf";
select STDOUT;
close P;

my $cmd="assura.31.6u1 assura 'sizing$$.rsf' > 'sizing$$.log'";
print $cmd if $verbose;
system "$cmd";
if ($debug) {
    system "sleep 1; rm '$prefix'.*";
}
else {
    system "rm 'sizing$$.'* '$prefix.'* '$tmpout'";
}
