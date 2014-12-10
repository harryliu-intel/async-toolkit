#!/usr/intel/bin/perl -l
# AAG
# $Id$
# $DateTime$

use strict;
use Getopt::Long;

my $cellname;
my $celllib;
my $spardir;
my $specdir;
my $castdir;
my $dfIIdir;
my $wrap = 3;
my $client;
my $refineparent;
my $maxheapsize='4G';
my $shortname;
my $flatname;
my $flat=0;
my $split = 0;
my $realname;
my $pdkroot;
my $jobs=1;
my $verbose=0;
my $noexec=0;
my $target="";
my $drcmem;
my $lvsmem;
my $verilog_block="rtl";
my $globalmake = "";
my $makeFlags;
my $subtype=500;

sub usage {
    my ($msg) = @_;
    print STDERR $msg if $msg ne "";
    print STDERR <<EU;
Usage: importSync [options]
    options:
   --cast-dir=<castdir>    : [$castdir] normal cast dir part of cast path
   --library=<cast lib>    : [$celllib] the cell library name
   --cell=<name>           : [$cellname] the fqcn
   --client=<p4 client>    : [$client] the p4 client to be used for p4 edit
   --subtype=<subtype>     : [$subtype] the subtype of the cell
   --dfII-dir=<dfIIdir>    : [$dfIIdir] the normal dfII directory root
   --drcmem=<size>         : [$drcmem] qsub mem size for drc
   --flatname=<verilog>    : [$flatname] a flat name if verilog is flattened
   --fulcrum-pdk-root=<>   : [$pdkroot] normal pdk root
   --jobs=<jobs>           : [$jobs] number of Make jobs. (May not work > 1)
   --lvsmem=<size>         : [$lvsmem] qsub mem size for lvs
   --max-heap-size=<size>  : [$maxheapsize] normal heap size spec
   --noexec                : [$noexec] gives make '-n' option
   --parent=<refinement>   : [$refineparent] the cast refinement parent for cells
   --shortname=<name>      : [$shortname] the short name if used
   --spar-dir=<spardir>    : [$spardir] the normal spar directory root
   --spec-dir=<specdir>    : [$specdir] the normal spec directory root
   --split                 : [$split] tells it to split the gds
   --target=<list>         : [$target] change default make target for testing
   --verbose               : [$verbose] verbose
   --verilog-block=<>      : [$verilog_block] name of verilog block to gen portmap
   --wrap=<number>         : [$wrap] usually 2 or 3 needed to fix canonical names
   --globalmake=<makefile> : [$globalmake] location of global make file
   --makeFlags=<flags>     : [$makeFlags] flags to pass to make
EU
exit 1;
}

sub checkmemarg {
    my ($arg) = @_;
    return 0 if ! ($arg =~ /^(\d+)([MG])$/);
    my $val = $1;
    my $mul = $2;
    $val *= 1e9 if $mul eq "G";
    $val *= 1e6 if $mul eq "M";
    ($val >= 2e9);
}

GetOptions (
    "cell=s" => \$cellname,
    "library=s" => \$celllib,
    "spar-dir=s" => \$spardir,
    "spec-dir=s" => \$specdir,
    "cast-dir=s" => \$castdir,
    "dfII-dir=s" => \$dfIIdir,
    "wrap=i" => \$wrap,
    "client=s" => \$client,
    "parent=s" => \$refineparent,
    "refineparent=s" => \$refineparent,
    "max-heap-size=s" => \$maxheapsize,
    "shortname=s" => \$shortname,
    "flatname=s" => \$flatname,
    "split" => \$split,
    "fulcrum-pdk-root=s" => \$pdkroot,
    "jobs=i" => \$jobs,
    "verbose" => \$verbose,
    "verilog-block=s" => \$verilog_block,
    "noexec" => \$noexec,
    "target=s" => \$target,
    "drcmem=s" => \$drcmem,
    "lvsmem=s" => \$lvsmem,
    "globalmake=s" => \$globalmake,
    "makeFlags=s" => \$makeFlags,
    "subtype=s" => \$subtype,
) or usage;

usage "--max-heap-size [$maxheapsize] is illegal or too small memory arg"
    if ! checkmemarg($maxheapsize);
$drcmem=$maxheapsize if $drcmem eq "";
$lvsmem=$maxheapsize if $lvsmem eq "";
usage "--drcmem [$drcmem] is illegal or too small memory arg" if ! checkmemarg($drcmem);
usage "--lvsmem [$lvsmem] is illegal or too small memory arg" if ! checkmemarg($lvsmem);
$target =~ s/[,\s]+/ /g;
my @targets = split(/ /, $target);
usage "Specify cell name" if ! $cellname =~ /\./;
if (! ($celllib =~ /\./)) {
    $celllib = $cellname;
    $celllib =~ s/\.[^\.]+\.[^\.]+$//;
}
if ( $client ne "") {
    my $p4root;
    open (P4, "p4 -u system -c $client client -o |");
    while (<P4>) {
        chomp;
        if (/^Root:/) {
            $p4root = $_;
            $p4root =~ s/^Root: *//;
            $p4root =~ s/["' \t]//g;
        }
        if (m://depot/:) {
            my ($depot,$target)=split;
            if ($spardir eq "" and m:/spar/:) {
                $spardir = $target;
                $spardir =~ s:/spar/.*:/spar:;
                $spardir =~ s://$client/:$p4root/:;
                $spardir =~ s://:/:g;
            }
            if ($specdir eq "" and m:/spec/:) {
                $specdir = $target;
                $specdir =~ s:/spec/.*:/spec:;
                $specdir =~ s://$client/:$p4root/:;
                $specdir =~ s://:/:g;
            }
            if ($dfIIdir eq "" and m:/dfII/:) {
                $dfIIdir = $target;
                $dfIIdir =~ s:/dfII/.*:/dfII:;
                $dfIIdir =~ s://$client/:$p4root/:;
                $dfIIdir =~ s://:/:g;
            }
            if ($castdir eq "" and m:/cast/:) {
                $castdir = $target;
                $castdir =~ s:/cast/.*:/cast:;
                $castdir =~ s://$client/:$p4root/:;
                $castdir =~ s://:/:g;
            }
        }
    }
    close P4;
    my $pdk;
    foreach my $x (split(/-/,$pdkroot)) {
        if ($x  =~ /^tsmc/) {
            $pdk = $x;
            last;
        }
    }
}
usage "Specify a cellname" if $cellname eq "";
usage "Not a spar dir $spardir" if ! -d $spardir;
usage "Not a cast dir $castdir" if ! -d $castdir;
usage "Not a spec dir $specdir" if ! -d $specdir;
usage "Not a dfII dir $dfIIdir"
    if (! -d $dfIIdir or ! -s "$dfIIdir/cds.lib.generated");
usage "wrap must be >= 0 and < 5" if $wrap < 0 or $wrap > 4;
usage "specify correct pdk root ($pdkroot)" if ! -d $pdkroot;
#usage "specify p4 client" if $client eq "";
if ($shortname eq "") {
    $shortname = $cellname;
    $flatname = "";
}
elsif ($flatname eq "") {
    $flatname = $shortname;
}
$realname = $cellname;
$realname .= "_real" if $split;
$refineparent = "$cellname"."_CELL" if ($refineparent eq "");

my $celllibdir = $celllib;
$celllibdir =~ s/\./\//g;
$celllibdir = "$dfIIdir/$celllibdir";
if ( ! -d "$celllibdir" or ! -s "$celllibdir/cdsinfo.tag" or ! -s "$celllibdir/prop.xx" ) {
    usage "dfII library $celllibdir does not exist in $dfIIdir,\n   create it first with the correct tech file, e.g. tsmc65.";
}
my $ingenerated=`grep -c " $celllib\$" "$dfIIdir/cds.lib.generated"`;
chomp $ingenerated;
if (! $ingenerated) {
    usage "must run fulcrum cdsp4sync --dfII-dir='$dfIIdir'";
}

my $argverbose = "";
$argverbose = "--verbose" if $verbose;

if ( -s "$spardir/chip/alta/FM/lvsdrc/Makefile") {
    $globalmake="$spardir/chip/alta/FM/lvsdrc/Makefile";
}
elsif ( -s "$spardir/chip/bali/FM/lvsdrc/Makefile") {
    $globalmake="$spardir/chip/bali/FM/lvsdrc/Makefile";
}
else {
    my @gm = `find $spardir -size +0 -path '*/lvsdrc/Makefile'`;
    chomp @gm;
    if ($#gm == 0) {
        $globalmake = $gm[0];
    }
    elsif ($#gm < 0) {
        usage "Cannot find global Makefile in $spardir";
    }
    else {
        print "Ambiguous global Makefile list:\n   ".join("\n   ", @gm);
        usage;
    }
}

if ($target ne "") {
    my %targets=();
    open (P, "$globalmake");
    my $hdrcresult=$cellname;
    $hdrcresult =~ s/\./_/g;
    $hdrcresult = "$hdrcresult.LAYOUT_ERRORS";
    while (<P>) {
        chomp;
        if ((/ : / or / :$/) and ! /:=/) {
            s/\s*:.*//;
            s/\$\(CELLNAME\)/$cellname/g;
            s/\$\(CDL\)/$shortname.cdl/;
            s/\$\(HDRCRESULT\)/$hdrcresult/;
            s/\$\(FLATNAME\)/$flatname/g;
            foreach my $f (split) {
                $targets{$f}=1;
            }
        }
    }
    close P;
    foreach my $tg (@targets) {
        if (! $targets{$tg}) {
            print STDERR "Unknown target $tg, use one of:";
            foreach my $t (sort keys %targets) {
                print STDERR "   $t";
            }
            usage;
        }
    }
}

my $maketemplate =
"CELLNAME := ARGCELLNAME
REALCELLNAME := ARGREALNAME
CELLLIB := ARGCELLLIB
SPECDIR := ARGSPECDIR
SPARDIR := ARGSPARDIR
CASTDIR := ARGCASTDIR
DFIIDIR := ARGDFIIDIR
SPLIT := ARGSPLIT
FLAT := ARGFLAT
WRAP := ARGWRAP
CLIENT := ARGCLIENT
VERBOSE := ARGVERBOSE
REFINEPARENT := ARGREFINEPARENT
MAX_HEAP_SIZE := ARGMAXHEAPSIZE
SHORTNAME := ARGSHORTNAME
FLATNAME := ARGFLATNAME
DRCMEM := ARGDRCMEM
LVSMEM := ARGLVSMEM
VERILOGBLOCK := ARGVERILOGBLOCK
SUBTYPE := ARGSUBTYPE

include $globalmake";

$maketemplate =~ s/ARGCELLNAME/$cellname/g;
$maketemplate =~ s/ARGREALNAME/$realname/g;
$maketemplate =~ s/ARGCELLLIB/$celllib/g;
$maketemplate =~ s/ARGSPECDIR/$specdir/g;
$maketemplate =~ s/ARGSPARDIR/$spardir/g;
$maketemplate =~ s/ARGCASTDIR/$castdir/g;
$maketemplate =~ s/ARGDFIIDIR/$dfIIdir/g;
$maketemplate =~ s/ARGVERBOSE/$argverbose/g;
$maketemplate =~ s/ARGWRAP/$wrap/g;
$maketemplate =~ s/ARGCLIENT/$client/g;
$maketemplate =~ s/ARGREFINEPARENT/$refineparent/g;
$maketemplate =~ s/ARGMAXHEAPSIZE/$maxheapsize/g;
$maketemplate =~ s/ARGSHORTNAME/$shortname/g;
$maketemplate =~ s/ARGFLATNAME/$flatname/g;
$maketemplate =~ s/ARGSPLIT/$split/g;
$maketemplate =~ s/ARGFLAT/$flat/g;
$maketemplate =~ s/ARGDRCMEM/$drcmem/g;
$maketemplate =~ s/ARGLVSMEM/$lvsmem/g;
$maketemplate =~ s/ARGVERILOGBLOCK/$verilog_block/g;
$maketemplate =~ s/ARGSUBTYPE/$subtype/g;

open (P, ">Makefile.importSync");
print P $maketemplate;
close P;

if ($noexec) {
    $noexec = "-n";
}
else {
    $noexec = "";
}
if (! defined ($makeFlags)) {
    $makeFlags="";
}
my $cmd= "make $noexec -j $jobs $makeFlags -f Makefile.importSync $target";
print $cmd if $verbose;
system ("$cmd");
if ($target eq "") {
print STDERR <<EM;
Done... Check drc.err and lvs.cls. If these are clean then
check in the changes in spec-dir and dfII-dir.
EM
}
