#!/usr/intel/bin/perl -l
# AAG
# $Id$
# $DateTime$

use strict;
use Getopt::Long;

sub min { $_[0] < $_[1] ? $_[0] : $_[1] }
sub max { $_[0] > $_[1] ? $_[0] : $_[1] }

my $gdsfile="";
my $printer="";
my $outfile="";
my $maxcolors=10;
my $minlayer=1;
my $maxlayer=8;
my $runassura=1;
my $runname="cov";
my $cellname="";
my $gridsize=4.8;
my $workingdir="./";
my $viewname="layout";
my $verbose=0;
my $mode="x";
my $layouttype="df2";
my $dfIIdir="";
my @rainbow;
my $pdkroot;

sub checkoptions {
    my $err=0;
    if (! ( $mode =~ /^[xpgd]/) ) {
        print STDERR "Illegal output mode '$mode', must be X11, gif, dwg, or ps";
        $err++;
    }
    if ($mode =~ /^p/i and $printer ne "") {
        my $rslt=`lpq -P$printer 2>/dev/null | grep -c ready`;
        chomp $rslt;
        if (! $rslt) {
            print STDERR "Printer $printer does not exist";
            $err++;
        }
    }
    if ($layouttype ne "df2" and ! -r "$gdsfile" and $runassura ) {
        print STDERR "gds2 mode requires gds file";
        $err++;
    }
    if (! -d "$workingdir" or ! -w "$workingdir" ) {
        print STDERR "Working directory '$workingdir' cannot be used";
        $err++;
    }
    if ( $cellname eq "" and $runassura ) {
        print STDERR "Cell name required";
        $err++;
    }
    if (! $runassura and ! -f "$runname.cov" ) {
        print STDERR "$runname.cov is missing";
        $err++;
    }
    $err;
}

my @unlink=(
    "alc", "ilc", "bbc", "bcr", "cnv",
    "dat", "dvc", "elf", "env", "erd",
    "err", "erx", "glt", "gmp", "gsm",
    "hdr", "lyr", "map", "msg", "nrc",
    "sum", "svi", "tmp", "trn", "xcn",
);

sub usage {
    my $cmd=$0;
    $cmd =~ s:.*/::;
    print STDERR <<EU;
Usage: $cmd [options] cellname
    --[no]assura        : run assura to get density
    --bins <#>          : number of bins in histogram
    --gds2-file <file>  : gds2 file if required
    --grid <microns>    : grid (default 4.8)
    --highest #         : top layer to analyze (1-8)
    --lowest #          : bottom layer to analyze (1-8)
    --mode (d|g|p|x)    : dwg, gif, ps(postscript), X11
    --run-name <name>   : run name prefix to use
    --verbose           : a little debugging info
    --view-name         : view name for df2 mode
    --source <name>     : one of df2 or gds2
    --working-dir <dir> : where to run from
EU
    exit 1;
}

my %options = (
    "assura!" => \$runassura,
    "bins=i" => \$maxcolors,
    "dfII-dir=s" => \$dfIIdir,
    "gds2-file=s" => \$gdsfile,
    "grid=f" => \$gridsize,
    "highest-layer=i"=> \$maxlayer,
    "lowest-layer=i" => \$minlayer,
    "mode=s" => \$mode,
    "printer=s" => \$printer,
    "P=s" => \$printer,
    "run-name=s" => \$runname,
    "verbose" => \$verbose,
    "view-name=s" => \$viewname,
    "source=s" => \$layouttype,
    "working-dir=s" => \$workingdir,
    "fulcrum-pdk-root=s" => \$pdkroot,
    "help" => sub { usage; },
);

sub dorainbow {
    my $n=int(($maxcolors+1)/2+0.51);
    my $r=0;
    my $g=0;
    my $b=0;
    my $j=0;
    for (my $i = 0; $i < ($maxcolors % 2 ? $n : $n-1); $i++) {
        $g = $i*255/($n/2);
        $b = ($n-1-$i)*255/($n/2);
        $rainbow[$j]=
            sprintf "#%02x%02x%02x", $r,$g>255 ? 255:$g,$b>255?255:$b;
        $j++;
    }
    for (my $i = 1; $i < $n; $i++) {
        $r = $i*255/($n/2);
        $g = ($n-1-$i)*255/($n/2);
        $rainbow[$j]=
            sprintf "#%02x%02x%02x", $r>255?255:$r,$g>255 ? 255:$g,$b>255?255:$b;
        $j++;
    }
}

sub assura {
    my $gdscellname;
    my $dfIIcellname;
    if ($layouttype eq "df2" ) {
        $dfIIcellname=`echo "$cellname" | rename --type=cell --from=cast --to=cadence`;
        chomp $dfIIcellname;
    }
    else {
        $gdscellname=`echo "$cellname" | rename --type=cell --from=cast --to=gds2`;
        chomp $gdscellname;
    }
    open (RSF, ">$runname.rsf");
    print RSF <<RSF;
avParameters(?outputErrorLib t ?flagMalformed nil)
outFile( "gds2" "${runname}err" "/dev/null"
RSF
for (my $n = $minlayer; $n <= $maxlayer; $n++) {
    print RSF " outLayer( m${n}cov 3${n} type(0))";
}
print RSF <<RSF;
)

avParameters(
RSF
if ($layouttype eq "df2") {
    my $lib = $dfIIcellname;
    $lib =~ s/\.[^\.]+$//;
    $lib =~ s/\.[^\.]+$//;
    print RSF "?inputLayout ( \"df2\" \"$lib\" )";
    print RSF "?cellName \"$dfIIcellname\"";
}
else {
    print RSF "?inputLayout ( \"gds2\" \"$gdsfile\" )";
    print RSF "?cellName \"$gdscellname\"";
}
print RSF <<RSF;
  ?rulesFile "$runname.rul"
  ?viewName "$viewname"
  ?overwrite t
  ?avrpt nil
  ?avrpt maxErrorShapesPerCell(20000000)
  ?workingDirectory "$workingdir"
  ?ignoreMissingOutLayer t
  ?runName "$runname"
)
RSF
    close RSF;
    open (RUL, ">$runname.rul");
    print RUL <<RUL;
drcExtractRules(
layerDefs( "gds2" ) ; to trick parser

layerDataTypeLowest=0
layerDataTypeHighest=1
layerNumberLowest=$minlayer
layerNumberHighest=$maxlayer
gridSize=$gridsize
types=list( "drawing" "dummy" "slot" )
RUL
if ($layouttype eq "df2") {
print RUL <<RUL;
ks="layerDefs( \\"df2\\" "
  for(j layerDataTypeLowest layerDataTypeHighest \\
    for(i layerNumberLowest layerNumberHighest ks=strcat(ks \\
    sprintf(nil "(m%d_%d = layer( \\"METAL%d\\" type(\\"%s\\"))) " i j i nth( j types))) i++)
  j++ )
  ks=strcat(ks ")")
  apply(stringToFunction(ks) nil)
RUL
}
else {
print RUL <<RUL;
ks="layerDefs( \\"gds2\\" "
  for(j layerDataTypeLowest layerDataTypeHighest \\
    for(i layerNumberLowest layerNumberHighest ks=strcat(ks \\
    sprintf(nil "(m%d_%d=layer(3%d type(%d))) " i j i j)) i++)
  j++ )
  ks=strcat(ks ")")
  apply(stringToFunction(ks) nil)
RUL
}
print RUL <<RUL;
ks = ""
    for(i layerNumberLowest layerNumberHighest ks=strcat(ks 
        sprintf(nil "(mo%d=geomOr(" i ))
            for(j layerDataTypeLowest layerDataTypeHighest \\
                ks=strcat(ks sprintf(nil "m%d_%d " i j ))
                    j++) ks=strcat(ks "))") i++)
    apply( stringToFunction(ks) nil)

    for(i layerNumberLowest layerNumberHighest
        list(ks=sprintf(nil
            "(md%d=drc( mo%d coverage > 0.05 windowSize = gridSize stepSize = gridSize ))" i i ) apply( stringToFunction(ks) nil) println(ks) )
            i++)
    for(i layerNumberLowest layerNumberHighest
        list(ks=sprintf(nil
            "(m%dcov=geomGetCoverage( md%d keep > 0.05 squareSize = gridSize ))" i i ) apply( stringToFunction(ks) nil) println(ks) )
            i++)
)
RUL
    close RUL;
    if ( ! -f "cds.lib" and $layouttype eq "df2" ) {
        if ( -d "$dfIIdir" ) {
            open (LIB, ">cds.lib");
            print LIB <<ELIB;
SOFTINCLUDE $dfIIdir/cds.lib.generated
SOFTINCLUDE \${FULCRUM_PDK_ROOT}/share/Fulcrum/dfII/cds.lib
SOFTINCLUDE cds.lib.user
ELIB
            close LIB;
        }
        else {
            print STDERR "Need dfII dir for creating cds.lib";
            exit 1;
        }
    }
    if (`which assura` =~ /ASSURA/) {
        system "assura $runname.rsf 1>/dev/null";
    }
    else {
        system "assura assura $runname.rsf 1>/dev/null";
    }
}

GetOptions ( %options ) or usage;
$ENV{FULCRUM_PDK_ROOT}=$pdkroot
    if  ! defined $ENV{FULCRUM_PDK_ROOT};

$cellname = $ARGV[0] if $cellname eq "" and defined $ARGV[0];
$gridsize = max ( $gridsize, 0.1);

$runname .= $layouttype;

$outfile = "$runname"."$layouttype" if $outfile eq "";

$maxcolors = max ($maxcolors, 2);
$minlayer = max (min ($maxlayer, $minlayer), 1);
$maxlayer = max (min ($maxlayer, 8), 1);

checkoptions and usage;
dorainbow;
chdir $workingdir;

if ( ( $runassura and assura )  or ! -f "$runname.cov" ) {
    print STDERR "Assura Failed";
    exit 1;
}
if ($runassura) {
    foreach my $ext (@unlink) {
        unlink "$runname.$ext";
    }
}
my %density=();
my ($minx,$miny,$maxx,$maxy);
my (@minx, @miny, @maxx, @maxy);
my %layers=();
open (P, "<$runname.cov");
my $rule=0;
my $cell;
my $layer;
my $dx=1e6;
my $dy=1e6;
while (<P>) {
    chomp;
    my @f=split;
    if (/^Cell/) {
        $cell=$f[3];
        $cell =~ s/_D_/./g;
        $cell =~ s/_U_/_/g;
    }
    if (/^Rule/) {
        $rule = $f[2];
        $layer = $f[4];
        $layer =~ s/geomGetCoverage.//;
        $layer =~ s/[a-z]//g;
        $layers{$layer}=$layer;
    }
    if ($f[1] eq "X") {
        $density{"$layer"}->{"$f[2]:$f[3]"}=$f[6];
        if (defined ($minx[$layer])) {
            $minx[$layer] = min $minx[$layer],$f[2];
            $maxx[$layer] = max $maxx[$layer],$f[4];
            $miny[$layer] = min $miny[$layer],$f[3];
            $maxy[$layer] = max $maxy[$layer],$f[5];
        }
        else {
            $dx = max(min($f[4]-$f[2],$dx),0);
            $dy = max(min($f[5]-$f[3],$dy),0);
            $minx[$layer] = $f[2];
            $miny[$layer] = $f[3];
            $maxx[$layer] = $f[4];
            $maxy[$layer] = $f[5];
        }
        if (defined ($minx)) {
            $minx = min $minx,$f[2];
            $maxx = max $maxx,$f[4];
            $miny = min $miny,$f[3];
            $maxy = max $maxy,$f[5];
        }
        else {
            $minx = $f[2];
            $miny = $f[3];
            $maxx = $f[4];
            $maxy = $f[5];
        }
    }
}
close P;
$dx = sprintf "%.3f", $dx;
$dy = sprintf "%.3f", $dy;
my $fill=0;
my $scale;

sub color {
    my ($color)=@_;
    if ($fill ne $color) {
        print "f $color";
        $fill = $color;
    }
}

sub rect {
    my ($x1,$y1,$x2,$y2,$f)=@_;
    color($rainbow[$f]);
    printf "r %.5f %.5f %.5f %.5f 1\n",
        $x1, $y1, $x2, $y2;
}

sub line {
    my ($x1,$y1,$x2,$y2,$w)=@_;
    color (0);
    printf "l %.5f %.5f %.5f %.5f $w\n",
        $x1, $y1, $x2, $y2;
}

sub text {
    my ($x1,$y1,$f,$t)=@_;
    color (0);
    printf "t %.5f %.5f $f \"$t\"\n", $x1, $y1, $t;
}

my $layer=0;
$scale = min 7/($maxx-$minx), 7/($maxy-$miny);
foreach my $layer (sort keys %density) {
    if ($minx[$layer] < $maxx[$layer] and $miny[$layer] < $maxy[$layer]) {
        if ($mode =~ /^d/i) {
            open (X, ">$outfile.$layer.dwg");
            print STDERR "Writing $outfile.$layer.dwg" if $verbose;
        }
        elsif ($mode =~ /^g/i) {
            open (X, "| gifdwg > $outfile.$layer.gif");
            print STDERR "Writing $outfile.$layer.gif" if $verbose;
        }
        elsif ($mode =~ /^p/i) {
            if ($printer ne "") {
                open (X, "| dwg -P$printer");
                print STDERR "Printing Layer METAL$layer" if $verbose;
            }
            else {
                open (X, "| dwg -o $outfile.$layer.ps");
                print STDERR "Writing $outfile.$layer.ps" if $verbose;
            }
        }
        else {
            open (X, "| xdwg");
            print STDERR "Displaying Layer METAL$layer" if $verbose;
        }
        select X;
        my $total=0;
        my @hist=();
        my $n=0;
        print "s 0 1";
        print "x 0";
        print "y 0";
        my %dn=%{$density{$layer}};
        foreach my $key (keys %dn) {
            my ($x,$y)=split(/:/,$key);
            my $d = $dn{$key};
            $d = max(int($d*$maxcolors-1e-6), 0);
            $hist[$d]++;
            $total++;
            rect (($x-$minx)*$scale,
                ($maxy-$y)*$scale,
                ($x+$dx-$minx)*$scale,
                ($maxy-$y-$dy)*$scale, $d);
        }
        my $dtx=0.6;
        my $caption=8.5;
        color (0);
        line (($maxx-$minx)*$scale+0.5, $maxy*$scale, -0.5, $maxy*$scale, 2);
        line (-$minx*$scale, ($maxy-$miny)*$scale+0.5, -$minx*$scale, -0.5, 2);
        text (0, $caption+0.6, 3, "METAL$layer $cell");
        for ($n = 0; $n < $maxcolors; $n++) {
            rect $n*$dtx, $caption, $n*$dtx+0.1, $caption+0.1, $n;
            text (($n-0.33)*$dtx, $caption+0.25, 1,
                sprintf ("%.2f-%.2f", $n/$maxcolors, ($n+1)/$maxcolors));
            text (($n)*$dtx, $caption+0.40, 2,
                sprintf ("%.0f%%", 100*($hist[$n]/$total)));
        }
        close X;
    }
}
