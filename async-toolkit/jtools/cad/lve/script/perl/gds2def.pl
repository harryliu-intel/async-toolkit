#!/usr/intel/bin/perl -l
# AAG
# $Id$
# $DateTime$

use strict;
print STDERR $ARGV[0];
my $report=0;

my %layermap = (
    32 => "M2",
    33 => "M3",
    34 => "M4",
    35 => "M5",
    36 => "M6",
    37 => "M7",
    52 => "VIA2",
    53 => "VIA3",
    54 => "VIA4",
    55 => "VIA5",
    56 => "VIA6",
);

my %validvia = (
   "M10_M9" => 1,
   "M10_M9min" => 1,
   "M1_NW" => 1,
   "M1_NWmin" => 1,
   "M1_OD" => 1,
   "M1_ODmin" => 1,
   "M1_PO" => 1,
   "M1_POLY" => 1,
   "M1_POLYmin" => 1,
   "M1_SUB" => 1,
   "M1_SUBmin" => 1,
   "M2_M1" => 1,
   "M2_M1_2CUT_H" => 1,
   "M2_M1_2CUT_V" => 1,
   "M2_M1_H" => 1,
   "M2_M1_R0" => 1,
   "M2_M1_R90" => 1,
   "M2_M1_STACK" => 1,
   "M2_M1_V" => 1,
   "M2_M1c" => 1,
   "M2_M1min" => 1,
   "M3_M2" => 1,
   "M3_M2_2CUT_H" => 1,
   "M3_M2_2CUT_V" => 1,
   "M3_M2_H" => 1,
   "M3_M2_R0" => 1,
   "M3_M2_R90" => 1,
   "M3_M2_STACK" => 1,
   "M3_M2_V" => 1,
   "M3_M2c" => 1,
   "M3_M2min" => 1,
   "M4_M3" => 1,
   "M4_M3_2CUT_H" => 1,
   "M4_M3_2CUT_V" => 1,
   "M4_M3_2x2" => 1,
   "M4_M3_H" => 1,
   "M4_M3_R0" => 1,
   "M4_M3_R90" => 1,
   "M4_M3_STACK" => 1,
   "M4_M3_V" => 1,
   "M4_M3c" => 1,
   "M4_M3min" => 1,
   "M5_M4" => 1,
   "M5_M4_2CUT_H" => 1,
   "M5_M4_2CUT_V" => 1,
   "M5_M4_2x2" => 1,
   "M5_M4_H" => 1,
   "M5_M4_R0" => 1,
   "M5_M4_R90" => 1,
   "M5_M4_STACK" => 1,
   "M5_M4_V" => 1,
   "M5_M4c" => 1,
   "M5_M4min" => 1,
   "M6_M5" => 1,
   "M6_M5_2CUT_H" => 1,
   "M6_M5_2CUT_V" => 1,
   "M6_M5_2x2" => 1,
   "M6_M5_H" => 1,
   "M6_M5_R0" => 1,
   "M6_M5_R90" => 1,
   "M6_M5_STACK" => 1,
   "M6_M5_V" => 1,
   "M6_M5c" => 1,
   "M6_M5min" => 1,
   "M7_M6" => 1,
   "M7_M6_2CUT_H" => 1,
   "M7_M6_2CUT_V" => 1,
   "M7_M6_2x2" => 1,
   "M7_M6_H" => 1,
   "M7_M6_R0" => 1,
   "M7_M6_R90" => 1,
   "M7_M6_STACK" => 1,
   "M7_M6_V" => 1,
   "M7_M6c" => 1,
   "M7_M6min" => 1,
   "M8_M7" => 1,
   "M8_M7c" => 1,
   "M8_M7min" => 1,
   "M9_M8" => 1,
   "M9_M8c" => 1,
   "M9_M8min" => 1,
);

my $cutsize = 100;
my $space = 100;
my $ln;
my %vias = ();
my %metal=();
my %coords=();
my %coordx=();
my %coordy=();
my %xlist=();
my %ylist=();
my $xylist=();
my $analvias=0;

sub doall {
    # make a list of all x and y center lines
    foreach my $metal (keys %metal) {
        my ($dir,$layer,$h,$cl,$x1,$x2) = split(/ /,$metal);
        print $metal if $report;
        if ($dir eq "H") {
            $ylist{"$cl $layer"} .= " $x1,$x2";
            $ylist{"$cl $layer"} =~ s/^ //;
        }
        else {
            $xlist{"$cl $layer"} .= " $x1,$x2";
            $xlist{"$cl $layer"} =~ s/^ //;
        }
    }
    if ($report) {
        foreach my $x (sort keys %xlist) {
            print "$x X";
        }
        foreach my $y (sort keys %ylist) {
            print "$y Y";
        }
    }
    if ($analvias) {
        foreach my $via (keys %vias) {
            my $vl = $vias{$via};
            $vl =~ s/^via//;
            $vl =~ s/A.*//;
            my $vl1 = "M$vl";
            $vl++;
            my $vl2 = "M$vl";
            my ($cx,$cy,$ly) = split(/ /,$via);
            my $x;
            my $y;
            foreach my $xx (keys %xlist) {
                my ($xc, $l) = split(/ /,$xx);
                my @yl = split(/ /,$xlist{$xx});
                foreach my $xy (@yl) {
                    my ($y1,$y2) = split(/,/,$xy);
                    if ($xc == $cx and ($l eq $vl1 or $l eq $vl2) and
                        (($y1 <= $cy and $cy <= $y2) or ($y2 <= $cy and $cy <= $y1))) {
                        $x = $xc;
                        print STDERR "# MATCHx AT ($x) $y1 <= $cy <= $y2"; 
                        last;
                    }
                    else {
                        print STDERR "# UNMATCHx AT ($cx != $xc) or ! ($y1 <= $cy <= $y2)"; 
                    }
                }
                last if defined $x;
            }
            foreach my $yy (keys %ylist) {
                my ($yc, $l) = split(/ /,$yy);
                my @xl = split(/ /,$ylist{$yy});
                foreach my $xy (@xl) {
                    my ($x1,$x2) = split(/,/,$xy);
                    if ($yc == $cy and ($l eq $vl1 or $l eq $vl2) and
                        (($x1 <= $cx and $cx <= $x2) or ($x2 <= $cx and $cx <= $x1))) {
                        $y = $yc;
                        print STDERR "# MATCHy AT ($y) $x1 <= $cx <= $x2"; 
                        last;
                    }
                    else {
                        print STDERR "# UNMATCHy AT ($cy != $yc) or ! ($x1 <= $cx <= $x2)"; 
                    }
                }
                last if defined $y;
            }
            if (defined($x) and defined ($y)) {
                print STDERR "# MATCH $cx $cy $vl1 $vl2" if $report;
            }
            elsif (defined ($x)) {
                my $dy;
                my $ny;
                foreach my $yy (keys %ylist) {
                    my ($yc,$l) = split(/ /,$yy);
                    if (defined ($dy)) {
                        if (abs ($yc-$cy) < $dy and $dy < 100) {
                            $dy = abs($yc-$cy);
                            $ny = $yc;
                        }
                    }
                    elsif (abs($yc-$cy) < 100) {
                        $dy = abs($yc-$cy);
                        $ny = $yc;
                    }
                }
                printf STDERR "# %sMATCH ($cx) $cy=>$ny($dy) $vl1 $vl2\n",
                    defined($ny) ? "" : "NO" if $report or !defined ($ny);
            }
            elsif (defined ($y)) {
                my $dx;
                my $nx;
                foreach my $xx (keys %xlist) {
                    my ($xc,$l) = split(/ /,$xx);
                    if (defined ($dx)) {
                        if (abs ($xc-$cx) < $dx and $dx < 100) {
                            $dx = abs($xc-$cx);
                            $nx = $xc;
                        }
                    }
                    elsif (abs($xc-$cx) < 100) {
                        $dx = abs($xc-$cx);
                        $nx = $xc;
                    }
                }
                printf STDERR "# %sMATCH $cx=>$nx($dx) ($cy) $vl1 $vl2",
                    defined($nx) ? "" : "NO" if $report or !defined ($nx);
            }
            else {
                print STDERR "# MISMATCH $cx $cy $vl1 $vl2";
            }
        }
    }
    %coords=();
    %coordx=();
    %coordy=();
    my ($dir,$layer,$h,$cl,$x1,$x2);
    my @ext;
    my $prefix = "  + ROUTED";
    foreach my $metal (keys %metal) {
        print STDERR "MX $metal $metal{$metal}";
        ($dir,$layer,$h,$cl,$x1,$x2) = split(/ /,$metal);
        @ext=split(/ /,$metal{$metal});
        @ext=(0,0);
        if ($dir eq "H") {
            my $xx1 = $x1 + $ext[0];
            my $xx2 = $x2 - $ext[1];
            print " $prefix $layer $h + SHAPE STRIPE ( $xx1 $cl $ext[0] ) ( $xx2 * $ext[1] )";
            $coords{"$xx1 $cl $layer"}=1;
            $coords{"$xx2 $cl $layer"}=1;
            $coordx{"$xx1 $layer"} .= "'$metal' ";
            $coordx{"$xx2 $layer"} .= "'$metal' ";
            $coordy{"$cl $layer"} .= "'$metal' ";
        }
        else {
            my $xx1 = $x1 + $ext[0];
            my $xx2 = $x2 - $ext[1];
            print " $prefix $layer $h + SHAPE STRIPE ( $cl $xx1 $ext[0] ) ( * $xx2 $ext[1] )";
            $coords{"$cl $xx1 $layer"}=1;
            $coords{"$cl $xx2 $layer"}=1;
            $coordy{"$xx1 $layer"} .= "'$metal' ";
            $coordy{"$xx2 $layer"} .= "'$metal' ";
            $coordx{"$cl $layer"} .= "'$metal' ";
        }
        $prefix = "   NEW";
    }
    foreach my $via (keys %vias) {
        my ($cx,$cy,$ly) = split(/ /,$via);
        my $vl = $vias{$via};
        $vl =~ s/via//;
        $vl =~ s/A.*//;
        my $vl1 = "M$vl";
        $vl++;
        my $vl2 = "M$vl";
        print " $prefix $layer 0 + SHAPE STRIPE ( $cx $cy ) $vias{$via}";
        my $mcnt=0;
        printf STDERR "Vx :%s:%s:%s:%s:%s:$cx $cy $vl1 $vl2\n" if $report;
            $coords{"$cx $cy $vl1"},
            $coords{"$cx $cy $vl2"},
            $coordx{"$cx $vl1"},
            $coordx{"$cx $vl2"},
            $coordy{"$cy $vl1"},
            $coordy{"$cy $vl2"};
        if ($coords{"$cx $cy $vl1"} and $coords{"$cx $cy $vl2"}) {
            print STDERR "# VIA OK" if $report;
            $mcnt++;
        }
        if ($coordx{"$cx $vl1"}) {
            print STDERR "# VIA MATCH X $vl1" if $report;
            $mcnt++;
        }
        if ($coordx{"$cx $vl2"}) {
            print STDERR "# VIA MATCH X $vl2" if $report;
            $mcnt++;
        }
        if ($coordy{"$cy $vl1"}) {
            print STDERR "# VIA MATCH Y $vl1" if $report;
            $mcnt++;
        }
        if ($coordy{"$cy $vl2"}) {
            print STDERR "# VIA MATCH Y $vl2" if $report;
            $mcnt++;
        }
        if (! $mcnt) {
            print STDERR "# VIA COORDINATE MISMATCH" if $report;
        }
        $prefix = "   NEW";
    }
}

sub poly {
    my ($xylist,$layer) = @_;
    my @xy = split(/[ ,]/, $xylist);
    my ($minx,$miny,$maxx,$maxy) = ($xy[0],$xy[1],$xy[0],$xy[1]);
    for (my $n = 0; $n < $#xy; $n += 2) {
        $minx = $minx > $xy[$n] ? $xy[$n] : $minx;
        $miny = $miny > $xy[$n+1] ? $xy[$n+1] : $miny;
        $maxx = $maxx < $xy[$n] ? $xy[$n] : $maxx;
        $maxy = $maxy < $xy[$n+1] ? $xy[$n+1] : $maxy;
    }
    my $dx = abs($maxx-$minx);
    my $dy = abs($maxy-$miny);
    my $nx = int($dx/($cutsize+$space))+1;
    my $sx=$space;
    $sx = ($dx-$nx*$cutsize)/($nx-1) if $nx > 1;
    my $ny = int($dy/($cutsize+$space))+1;
    my $sy=$space;
    $sy = ($dy-$ny*$cutsize)/($ny-1) if $ny > 1;
    my $cx = ($minx+$maxx)/2;
    my $cy = ($miny+$maxy)/2;
    $layer -= 50;
    my $layer2 = $layer + 1;
    my $vianame="via${layer}Array_lef${nx}x${ny}x${sx}x${sy}";
    if ($nx == 1 and $ny == 1) {
        $vianame = "M${layer2}_M${layer}min";
    }
    elsif ($nx == 1) {
        $vianame = "M${layer2}_M${layer}_2CUT_V";
    }
    elsif ($ny == 1) {
        $vianame = "M${layer2}_M${layer}_2CUT_H";
    }
    elsif ($validvia{"M${layer2}_M${layer}_2x2"}) {
        $vianame = "M${layer2}_M${layer}_2x2";
    }
    elsif ($validvia{"M${layer2}_M${layer}min"}) {
        $vianame = "M${layer2}_M${layer}min";
    }
    print STDERR "Invalid Via Name $vianame" if (! $validvia{$vianame});
    $vias{"$cx $cy $layer"} = $vianame;
}

sub rect2path {
    my ($layer,@xy) = @_;
    for(my $n = 0; $n <= $#xy; $n++) {
        $xy[$n] = sprintf("%.0f", int($xy[$n]/2)*2);
    }
    my ($nx,$ny,$xx,$xy) = ($xy[0],$xy[1],$xy[0],$xy[1]);
    for (my $n = 0; $n < $#xy; $n += 2) {
        $nx = $nx > $xy[$n] ? $xy[$n] : $nx;
        $ny = $ny > $xy[$n+1] ? $xy[$n+1] : $ny;
        $xx = $xx < $xy[$n] ? $xy[$n] : $xx;
        $xy = $xy < $xy[$n+1] ? $xy[$n+1] : $xy;
    }
    $xy[0] = $nx;
    $xy[1] = $ny;
    $xy[2] = $xx;
    $xy[3] = $xy;
    my $w = abs($xy[0] - $xy[2]);
    my $h = abs($xy[1] - $xy[3]);
    my $width;
    my $length;
    if ($w > $h) {
        my $cl = ($xy[1]+$xy[3])/2;
        $metal{"H $layer $h $cl $xy[0] $xy[2]"}="0 0";
    }
    else {
        my $cl = ($xy[0]+$xy[2])/2;
        $metal{"V $layer $w $cl $xy[1] $xy[3]"}="0 0";
    }
}

my %layers;
# start with generated gds
foreach my $file (@ARGV) {
    my $cell = $file;
    $cell =~ s/500.*/500/;
    $cell =~ s:.*/::;
    $cell =~ s/\(/-L/;
    $cell =~ s/\)/-R/;
    open (P, "rdgds '$file' |") or die;
    while (<P>) {
        chomp;
        if (/^BOUNDARY/) {
            my $xy = "";
            my $layer = 0;
            my $dt = 0;
            while (<P>) {
                chomp;
                if (/^LAYER (\d+)/) {
                    $layer = $1;
                }
                elsif (/^DATATYPE (\d+)/) {
                    $dt = $1;
                }
                elsif (/,/) {
                    s/ //g;
                    $xy .= " $_";
                }
                last if /^ENDEL/;
            }
            $xy =~ s/^ //;
            print STDERR "$layer:$dt $xy";
            push @{$layers{"$layer;$dt"}}, $xy;
        }
        if (/^PATH/) {
            my $xy = "";
            my $layer = 0;
            my $dt = 0;
            my @y=();
            my @x=();
            my $w = 300;
            while (<P>) {
                chomp;
                if (/^LAYER (\d+)/) {
                    $layer = $1;
                }
                elsif (/^DATATYPE (\d+)/) {
                    $dt = $1;
                }
                elsif (/^WIDTH (\d+)/) {
                    $w=$1;
                }
                elsif (/,/) {
                    s/ //g;
                    my ($x,$y)=split(/,/,$_);
                    push @x, $x;
                    push @y, $y;
                }
                last if /^ENDEL/;
            }
            if ($x[0] == $x[1]) {
                $xy=sprintf("%d,%d %d,%d %d,%d %d,%d %d,%d",
                    $x[0]-$w/2,$y[0],
                    $x[0]-$w/2,$y[1],
                    $x[0]+$w/2,$y[1],
                    $x[0]+$w/2,$y[0],
                    $x[0]-$w/2,$y[0]);
            }
            else {
                $xy=sprintf("%d,%d %d,%d %d,%d %d,%d %d,%d",
                    $x[0],$y[0]-$w/2,
                    $x[1],$y[1]-$w/2,
                    $x[1],$y[1]+$w/2,
                    $x[0],$y[0]+$w/2,
                    $x[0],$y[0]-$w/2);
            }
            push @{$layers{"$layer;$dt"}}, $xy;
        }
    }
    close P;
# boundary
my $minx;
my $maxx;
my $miny;
my $maxy;
foreach my $xylist (@{$layers{"108;0"}}) {
    my @xy = split(/ /, $xylist);
    my ($x,$y) = split(/,/,$xy[0]);
    $minx = $maxx = $x;
    $miny = $maxy = $y;
    foreach my $xy (@xy) {
        my ($x,$y) = split(/,/,$xy);
        $minx = $x if $x < $minx;
        $miny = $y if $y < $miny;
        $maxx = $x if $x > $maxx;
        $maxy = $y if $y > $maxy;
    }
}
if ($minx < 0) {
    $minx = sprintf "-%.0f", -$minx;
}
else {
    $minx = sprintf "%.0f", $minx;
}
if ($miny < 0) {
    $miny = sprintf "-%.0f", -$miny;
}
else {
    $miny = sprintf "%.0f", $miny;
}
$maxx = sprintf "%.0f", $maxx;
$maxy = sprintf "%.0f", $maxy;
my $sizex = sprintf "%.0f", $maxx-$minx;
my $sizey = sprintf "%.0f", $maxy-$miny;
print "MACRO $cell";
print "   CLASS BLOCK ;";
print "   FOREIGN $cell $minx $miny ;";
print "   ORIGIN ",$minx," ",$miny," ;";
print "   SIZE $sizex BY $sizey ;";
print "SPECIALNETS 2 ;";
print "- Vdd ( * VDD )";
# throw away small chunks of metal
sub area {
    my ($lm) = @_;
    my ($minx, $miny, $maxx, $maxy);
    my @xy = split(/ /,$lm);
    foreach my $xy (@xy) {
        my ($x, $y) = split(/,/,$xy);
        if (defined ($minx)) {
            $minx = $x < $minx ? $x : $minx;
            $miny = $y < $miny ? $y : $miny;
            $maxx = $x > $maxx ? $x : $maxx;
            $maxy = $y > $maxy ? $y : $maxy;
        }
        else {
            $minx = $maxx = $x;
            $miny = $maxy = $y;
        }
    }
    ($maxx-$minx)*($maxy-$miny);
}

sub center {
    my ($lm) = @_;
    my ($minx, $miny, $maxx, $maxy);
    my @xy = split(/ /,$lm);
    foreach my $xy (@xy) {
        my ($x, $y) = split(/,/,$xy);
        if (defined ($minx)) {
            $minx = $x < $minx ? $x : $minx;
            $miny = $y < $miny ? $y : $miny;
            $maxx = $x > $maxx ? $x : $maxx;
            $maxy = $y > $maxy ? $y : $maxy;
        }
        else {
            $minx = $maxx = $x;
            $miny = $maxy = $y;
        }
    }
    (($maxx+$minx)/2,($maxy+$miny)/2);
}

foreach my $key (sort keys %layers) {
    my ($ly,$dt)=split(/;/,$key);
    next if ! ($layermap{$ly} =~ /^m/i);
    my @lm = ();
    foreach my $lm (@{$layers{$key}}) {
        if (area($lm) > 0.18*1e6) {
            push @lm, $lm;
        }
        else {
            print STDERR "Rejecting $key $lm";
        }
    }
    @{$layers{$key}}=@lm;
}

sub inside {
    my ($cx,$cy,$lm) = @_;
    my ($minx, $miny, $maxx, $maxy);
    my @xy = split(/ /,$lm);
    foreach my $xy (@xy) {
        my ($x, $y) = split(/,/,$xy);
        if (defined ($minx)) {
            $minx = $x < $minx ? $x : $minx;
            $miny = $y < $miny ? $y : $miny;
            $maxx = $x > $maxx ? $x : $maxx;
            $maxy = $y > $maxy ? $y : $maxy;
        }
        else {
            $minx = $maxx = $x;
            $miny = $maxy = $y;
        }
    }
    my $val = ($cx > $minx and $cx < $maxx and $cy > $miny and $cy < $maxy);
#    print STDERR "IXN2 ($cx > $minx and $cx < $maxx and $cy > $miny and $cy < $maxy) = $val";
    ($cx > $minx and $cx < $maxx and $cy > $miny and $cy < $maxy);
}

sub ixn {
    my ($lm, $key1, $key2) = @_;
    my $ix1=0;
    my $ix2=0;
    my ($cx,$cy) = center($lm);
    print STDERR "CNTER $cx $cy $lm";
    foreach my $lm (@{$layers{$key1}}) {
        $ix1 = 1 if inside ($cx,$cy,$lm);
    }
    foreach my $lm (@{$layers{$key2}}) {
        $ix2 = 1 if inside ($cx,$cy,$lm);
    }
#    print STDERR "ixn $lm $key1 $key2 $ix1 $ix2";
    ($ix1 and $ix2);
}

foreach my $key (sort keys %layers) {
    my ($ly,$dt)=split(/;/,$key);
    next if ! ($layermap{$ly} =~ /^v/i);
    my @lm = ();
    my $l1 = $ly - 50 + 30;
    my $l2 = $ly - 50 + 31;
    my $key1 = "$l1;$dt";
    my $key2 = "$l2;$dt";
    foreach my $lm (@{$layers{$key}}) {
#        print STDERR "IXN $key $lm $key1 $key2";
        if (! $analvias or ixn($lm,$key1,$key2)) {
            push @lm, $lm;
        }
        else {
            print STDERR "Rejecting $key $lm";
        }
    }
    @{$layers{$key}}=@lm;
}
# look for and reject vias which do not intersect
# metal top and bottom.
$ln=0;
%vias=();
%metal=();
foreach my $key (sort keys %layers) {
    next if ! ($key =~ /;0$/);
    my $ly = $key;
    $ly =~ s/;0//;
    next if ! defined $layermap{$ly};
    next if ! @{$layers{$key}};
    foreach my $xylist (@{$layers{$key}}) {
        if ($layermap{$ly} =~ /^m/i) {
            my @xy = split(/[, ]/,$xylist);
            rect2path($layermap{$ly}, @xy);
        }
        else {
            poly ($xylist,$ly);
        }
    }
}
#foreach my $metal (sort keys %metal) {
#    print "Vdd M $metal : $metal{$metal}";
#}
#foreach my $via (sort keys %vias) {
#    print "Vdd V $via $vias{$via}";
#}
doall();
print "  + USE POWER";
print "  ;";
print "- Vss ( * GND )";
$ln=0;
%vias=();
%metal=();
foreach my $key (sort keys %layers) {
    next if ! ($key =~ /;1$/);
    my $ly = $key;
    $ly =~ s/;1//;
    next if ! defined $layermap{$ly};
    next if ! @{$layers{$key}};
    foreach my $xylist (@{$layers{$key}}) {
        if ($layermap{$ly} =~ /^m/i) {
            my @xy = split(/[, ]/,$xylist);
            rect2path($layermap{$ly}, @xy);
        }
        else {
            poly ($xylist,$ly);
        }
    }
}
#foreach my $metal (sort keys %metal) {
#    print "GND M $metal : $metal{$metal}";
#}
#foreach my $via (sort keys %vias) {
#    print "GND V $via $vias{$via}";
#}
doall();
print "  + USE GROUND";
print "  ;";
print "END SPECIALNETS";
print "END MACRO";
}
