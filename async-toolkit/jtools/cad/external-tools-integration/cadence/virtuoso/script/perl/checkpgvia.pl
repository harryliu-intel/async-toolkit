#!/usr/intel/bin/perl -l
# AAG
# $Id: //depot/sw/main/cad/external-tools-integration/cadence/virtuoso/script/perl/plotdensity.pl#2 $
# $DateTime: 2009/03/31 16:37:03 $

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
my $dfIIdir="";
my @rainbow;
my $pdkroot;
my $dohist=0;
my $doplot=1;
my $v78=0;
my $dolayer=undef;

sub checkoptions {
    my $err=0;
    if (! ( $mode =~ /^[xpgdn]/) ) {
        print STDERR "Illegal output mode '$mode', must be X11, gif, dwg, or ps";
        $err++;
    }
    $doplot=0 if ($mode =~ /^n/);
    if ($mode =~ /^p/i and $printer ne "") {
        my $rslt=`lpq -P$printer 2>/dev/null | grep -c ready`;
        chomp $rslt;
        if (! $rslt) {
            print STDERR "Printer $printer does not exist";
            $err++;
        }
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
    "alc", "bbc", "bcr", "cnv", "dat", "dvc",
    "elf", "env", "erc", "erd", "err", "erx",
    "glt", "gmp", "gsm", "hdr", "ilc", "lyr",
    "map", "msg", "nrc", "sum", "svi", "tmp",
    "trn", "xcn",
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
    "working-dir=s" => \$workingdir,
    "fulcrum-pdk-root=s" => \$pdkroot,
    "noplot" => sub {$doplot=0;},
    "hist" => \$dohist,
    "help" => sub { usage; },
    "layer=s" => \$dolayer,
    "via78" => \$v78,
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
        $g = ($n-$i)*255/($n/2);
        $rainbow[$j]=
            sprintf "#%02x%02x%02x", $r>255?255:$r,$g>255 ? 255:$g,$b>255?255:$b;
        $j++;
    }
    $rainbow[0]="#000000";
    $rainbow[1]="#ff0000";
}

sub assura {
    my $gdscellname;
    my $dfIIcellname;
    $dfIIcellname=`echo "$cellname" | fulcrum rename --type=cell --from=cast --to=cadence`;
    chomp $dfIIcellname;
    my $lib = $dfIIcellname;
    $lib =~ s/\.[^\.]+$//;
    $lib =~ s/\.[^\.]+$//;
    if ( ! -f "assura_tech.lib") {
        open (ATL, ">assura_tech.lib");
        print ATL "DEFINE Assura_tsmc65 \${FULCRUM_PDK_ROOT}/share/Fulcrum/assura";
        close ATL;
    }
    my $setv78="";
    $setv78="?set (\"V78\")" if $v78;
    open (RSF, ">$runname.rsf");
    print RSF <<RSF;
avParameters(
  ?inputLayout ( "df2" "$lib" )
  ?cellName "$dfIIcellname"
  ?viewName "$viewname"
  ?runName "$runname"
  ?technology "Assura_tsmc65"
  ?techLib "./assura_tech.lib"
  ?rulesFile "$pdkroot/share/Fulcrum/drc/pgdensity/drc.rul"
  ?workingDirectory "$workingdir"
  ?overwrite t
  ?avrpt nil
  ?avrpt maxErrorShapesPerCell(20000000)
  ?avrpt t
  ?set ("NOTVIRTUOSO")
  $setv78
)
RSF
    if ( ! -f "cds.lib" ) {
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

$outfile = "$runname" if $outfile eq "";

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
my (%minx, %miny, %maxx, %maxy);
my %layers=();
open (P, "<$runname.cov");
my $cell;
my $layer;
my $dx=1e6;
my $dy=1e6;
my $ok=0;
while (<P>) {
    chomp;
    my @f=split;
    if (/^Cell/) {
        $cell=$f[3];
        $cell =~ s/_D_/./g;
        $cell =~ s/_U_/_/g;
    }
    if (/^Rule.*geomGetCoverage/) {
        $layer = $f[6];
        $layer =~ s/.*geomGetCoverage.//;
        $layer =~ s/\s.*//;
        $layer =~ s/_den//;
        $layers{$layer}=$layer;
        $ok=1;
    }
    elsif (/^Rule/) {
        $ok=0;
    }
    if ($f[1] eq "X" and $ok) {
        $density{"$layer"}->{"$f[2]:$f[3]"}=$f[6];
        if (defined ($minx{$layer})) {
            $minx{$layer} = min $minx{$layer},$f[2];
            $maxx{$layer} = max $maxx{$layer},$f[4];
            $miny{$layer} = min $miny{$layer},$f[3];
            $maxy{$layer} = max $maxy{$layer},$f[5];
        }
        else {
            $dx = max(min($f[4]-$f[2],$dx),0);
            $dy = max(min($f[5]-$f[3],$dy),0);
            $minx{$layer} = $f[2];
            $miny{$layer} = $f[3];
            $maxx{$layer} = $f[4];
            $maxy{$layer} = $f[5];
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

$scale = min 7/($maxx-$minx), 7/($maxy-$miny);
my $mindensity=undef;
my $maxdensity=undef;
if (defined($dolayer) and !defined($density{$dolayer})) {
    print STDERR "Unknown layer $dolayer";
    print STDERR "Available layers : ".join(" ", sort keys %density);
    exit 1;
}
foreach my $layer (sort keys %density) {
    my %dn=%{$density{$layer}};
    foreach my $key (keys %dn) {
        my ($x,$y)=split(/:/,$key);
        my $d = $dn{$key};
        if (defined($mindensity)) {
            $mindensity=min($mindensity,$d);
            $maxdensity=max($maxdensity,$d);
        }
        else {
            $maxdensity=$mindensity=$d;
        }
    }
}
my $range=$maxdensity-$mindensity;
$range=1 if ($range==0);
my %hist=();
foreach my $layer (sort keys %density) {
    next if (defined($dolayer) and $dolayer ne $layer);
    if ($minx{$layer} < $maxx{$layer} and $miny{$layer} < $maxy{$layer}) {
        if (! $doplot or ($mode =~ /^n/)) {
            open (X, ">/dev/null");
        }
        elsif ($mode =~ /^d/i) {
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
            $hist{$layer}->{$d}++;
            if ($d == 0) {
                $hist[0]++;
            }
            elsif ($d < 0.0048) {
                $hist[1]++;
                $d=1;
            }
            else {
                $d = 2+max(int(($d-$mindensity)/$range*($maxcolors-2)-1e-6), 0);
                $hist[$d]++;
            }
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
        text (0, $caption+0.6, 3, "$layer $cell");
        for ($n = 0; $n < $maxcolors; $n++) {
            rect $n*$dtx, $caption, $n*$dtx+0.1, $caption+0.1, $n;
            if ($n >= 2) {
            text (($n-0.33)*$dtx, $caption+0.25, 1,
                sprintf ("<=%.1f%s", (($n-1)/$maxcolors*$range+$mindensity)*4/0.004822,
                    $n == $maxcolors-1 ? " Vias" : ""));
            }
            elsif($n==0) {
            text (($n-0.33)*$dtx, $caption+0.25, 1, "0");
            }
            else {
            text (($n-0.33)*$dtx, $caption+0.25, 1, "<4");
            }
            text (($n)*$dtx, $caption+0.38, 2,
                sprintf ("%.1f%%", 100*($hist[$n]/$total)));
        }
        close X;
    }
}
if ($dohist) {
    select STDOUT;
    foreach my $layer (sort keys %hist) {
        next if (defined($dolayer) and $dolayer ne $layer);
        print "$layer";
        foreach my $d (sort keys %{$hist{$layer}}) {
            printf "%.1f $hist{$layer}->{$d}\n", $d/0.004822*4;
        }
    }
}
