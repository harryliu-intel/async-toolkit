#!/usr/intel/bin/perl -l
# AAG
# $Id$
# $DateTime$

use strict;
use IPC::Open2;
use Getopt::Long;
my $user=$ENV{USER};
$user="nobody" if ($user eq "");

sub setfilemode {
    my ($file)= @_;
    if (defined ($ENV{LVE_FILEMODE}) and -f "$file" ) {
        chmod $ENV{LVE_FILEMODE}, "$file";
        if (defined($ENV{LVE_SGID}) and $ENV{LVE_SGID} == 1) {
            `chgrp $ENV{LVE_GID} "$file" &>/dev/null`;
        }
    }
}

sub setdirmode {
    my ($dir) = @_;
    if (defined ($ENV{LVE_DIRMODE}) and -d "$dir") {
        chmod $ENV{LVE_DIRMODE}, "$dir";
        if (defined($ENV{LVE_SGID}) and $ENV{LVE_SGID} == 1) {
            `chgrp $ENV{LVE_GID} "$dir" &>/dev/null`;
        }
    }
}

sub conon_path {
    my ($patharg)=@_;
    my $pwd=`pwd`;
    chomp $pwd;
    my $path=$patharg;
    my $tail="";
    if ( ! -d $path ) {
        $path =~ s:(/[^\/]+)$::;
        $tail = $1;
    }
    if (! -d $path ) {
        print STDERR "Warning: Cannot cononicalize $patharg" if $tail ne "";
        return $patharg;
    }
    chdir $path;
    $path=`pwd`;
    chomp $path;
    chdir $pwd;
    $path =~ s:^/mnt/fulcrum/home/:/home/:;
    $path .= $tail;
    $path;
}

my $err=0;
my $ipid=0;
my $cpid=0;
my $n2pid=0;
my $c2pid=0;
my $i2pid=0;

my $verbose=0;
my $maxnamelen=0;

local(*IRD,*IWR);
sub iconvert {
    my $in = $_[0];
    local($_);
    $in =~ s:/:.:g;
    $in =~ s:\\::g;
    # hack because renamer does not recognize '$', use '_' to be consistent with vs2cast
    $in =~ s/\$/_/g;
    if ($ipid <= 0) {
        $ipid=open2 (\*IRD,\*IWR, "rename --from=cadence --to=gds2 --type=instance");
    }
    print IWR "$in";
    $_=$in;
    $_=<IRD>;
    chomp $_;
    if ($_ eq "") {
        $_=$in;
        $err++;
    }
    $_;
}

local(*CRD,*CWR);
sub cconvert {
    my $in = $_[0];
    # hack because renamer does not recognize '$', use '_' to be consistent with vs2cast
    $in =~ s/\$/_/g;
    local($_);
    if ($cpid <= 0) {
        $cpid=open2 (\*CRD,\*CWR, "rename --from=cast --to=cadence --type=cell");
    }
    print CWR "$in";
    $_="$in";
    $_=<CRD>;
    chomp $_;
    if ($_ eq "") {
        $_=$in;
        $err++;
    }
    $_;
}

local(*C2RD,*C2WR);
sub c2convert {
    my $in = $_[0];
    # hack because renamer does not recognize '$', use '_' to be consistent with vs2cast
    $in =~ s/\$/_/g;
    local($_);
    if ($c2pid <= 0) {
        $c2pid=open2 (\*C2RD,\*C2WR, "rename --from=gds2 --to=cast --type=cell");
    }
    print C2WR "$in";
    $_="$in";
    $_=<C2RD>;
    chomp $_;
    if ($_ eq "") {
        $_=$in;
        $err++;
    }
    $_;
}

local(*I2RD,*I2WR);
sub i2convert {
    my $in = $_[0];
    # hack because renamer does not recognize '$', use '_' to be consistent with vs2cast
    $in =~ s/\$/_/g;
    local($_);
    if ($i2pid <= 0) {
        $i2pid=open2 (\*I2RD,\*I2WR, "rename --from=gds2 --to=cast --type=instance");
    }
    print I2WR "$in";
    $_="$in";
    $_=<I2RD>;
    chomp $_;
    if ($_ eq "") {
        $_=$in;
        $err++;
    }
    $_;
}

local(*N2RD,*N2WR);
sub n2convert {
    my $in = $_[0];
    # hack because renamer does not recognize '$', use '_' to be consistent with vs2cast
    $in =~ s/\$/_/g;
    local($_);
    if ($n2pid <= 0) {
        $n2pid=open2 (\*N2RD,\*N2WR, "rename --from=gds2 --to=cast --type=node");
    }
    print N2WR "$in";
    $_="$in";
    $_=<N2RD>;
    chomp $_;
    if ($_ eq "") {
        $_=$in;
        $err++;
    }
    $_;
}

sub get_lib_name {
    my $in = $_[0];
    $in =~ s/\.[^\.]+$//;
    $in =~ s/\.[^\.]+$//;
    $in;
}

my $template=<<TEMPLATE;
#dbuPerUU                          "0"
#donutNumSides                     "64"
#ellipseNumSides                   "64"
#hierDepth                         "32767"
#strmTextNS                        "cdba"
#viaCutArefThreshold               "0"
#arrayInstToScalar
#backupGdsLogFiles
case                               "preserve"
cellListFile                       ""
cellMap                            "CELLMAPTABLE"
cellNamePrefix                     ""
cellNameSuffix                     ""
#checkPolygon
convertDot                         "ignore"
convertPin                         "geometry"
#doNotPreservePcellPins
COMMENTPCELLSflattenPcells
COMMENTVIASflattenVias
fontMap                            ""
#ignoreLines
#ignorePcellEvalFail
labelCase                          "preserve"
labelDepth                         "1"
labelMap                           ""
layerMap                           "LAYERTABLE"
library                            "LIBNAME"
logFile                            "LOGFILE"
maxVertices                        "200"
#mergePathSegsToPath
#noConvertHalfWidthPath
noInfo                             ""
#noObjectProp
#noOutputTextDisplays
#noOutputUnplacedInst
noWarn                             ""
objectMap                          ""
outputDir                          "."
pathToPolygon
pinAttNum                          "0"
propMap                            "PROPMAP"
#propValueOnly
#rectToBox
refLibList                         "REFLIBLIST"
#replaceBusBitChar
#reportPrecisionLoss
#respectGDSIINameLimit
runDir                             "RUNDIR"
#snapToGrid
strmFile                           "GDSIINAME"
strmVersion                        "5"
summaryFile                        ""
# FIXME
techLib                            "731"
topCell                            "CELLNAME"
userSkillFile                      ""
viaMap                             ""
view                               "VIEWNAME"
warnToErr                          ""
TEMPLATE

my $libName;
my $dfIIdir;
my $castpath;
my $cell;
my $output;
my $bindfile="/dev/null";
my $pdkroot;
my $rootcellname="";
my $outbind;
my $cadencelog = "/dev/null";
my $assuralog = "/dev/null";
my $pipolog;
my $debug=0;
my $cleanup=1;
my $flattenpcells=0;
my $flattenvias=0;
my $scale=1.0;
my $bit64=0;
my $suppresspins=0;
my $maxheapsize = "1G";
my $jreargs = "-server";
my $jre = "";
my $cadenceName;
my $workingDir;
my $viewName="layout";
my $refLibList="";
my @skip_libs=();
my $srcViewName="layout";
my $sixtyfivemode=0;
my $tapeout=0;
my $layers="";
my $layertable="";
my $cellmaptable="";
my $noproperties=0;
my $use_tag=0;
my $tag_orientation="MXR90";
my $verilog=0;
my @defines=();

my %options = (
    "view-name=s" => \$srcViewName,
    "cell=s" => \$cell,
    "dfII-dir=s" => \$dfIIdir,
    "fulcrum-pdk-root=s" => \$pdkroot,
    "cast-path=s" => \$castpath,
    "working-dir=s" => \$workingDir,
    "output=s" => \$output,
    "bind-rul=s" => \$bindfile,
    "output-bind-file=s" => \$outbind,
    "output-root-cell-name=s" => \$rootcellname,
    "cadence-log=s" => \$cadencelog,
    "assura-log=s" => \$assuralog,
    "pipo-log=s" => \$pipolog,
    "verbose" => \$verbose,
    "debug" => \$debug,
    "flatten-pcells" => \$flattenpcells,
    "flatten-vias" => \$flattenvias,
    "scale=f" => \$scale,
    "64" => \$bit64,
    "suppress-pins" => \$suppresspins,
    "max-heap-size=s" => \$maxheapsize,
    "jre-args=s" => \$jreargs,
    "jre=s" => \$jre,
    "65mode:i" => \$sixtyfivemode,
    "tapeout" => \$tapeout,
    "layers=s" => \$layers,
    "max-name-length=i" => \$maxnamelen,
    "noproperties" => \$noproperties,
    "use-tag" => \$use_tag,
    "tag-orientation=s" => \$tag_orientation,
    "verilog" => \$verilog,
    "skip-libs=s" => sub { push @skip_libs,split(/,/,$_[1]);},
    "define=s" => \@defines,
);

my %help = (
    "layers" => "comma separated list of:\n".
                "                ### (all purposes for this layer)\n".
                "                ###;### (just this layer,purpose)\n".
                "                m### (all lpp for this mask)\n".
                "                * (all lpp's required for tapeout)\n".
                "                blank/missing (all lpp in tech file)",
    "view-name" => "the dfII view name",
    "cell" => "cell cast name",
    "dfII-dir" => "normal dfII-dir",
    "fulcrum-pdk-root" => "fulcrum pdk root path",
    "cast-path" => "normal cast path",
    "working-dir" => "usually the current directory",
    "output" => "output gds file name",
    "bind-rul" => "binding file, if specified",
    "output-root-cell-name" => "rename of top cell from default",
    "cadence-log" => "for layout.exe log file",
    "assura-log" => "for assura logging (65mode only)",
    "pipo-log" => "stream out log file",
    "verbose" => "more progress info",
    "debug" => "progress info plus keep temporary files",
    "flatten-pcells" => "stream out pcell flattening",
    "flatten-vias" => "stream out with via flattening",
    "scale=f" => "scale results (not sure this works)",
    "64" => "run in 64 bit mode",
    "suppress-pins" => "pipo suppress pins mode",
    "max-heap-size" => "for java sub-tasks",
    "jre-args" => "for java sub-tasks",
    "jre" => "for java sub-tasks",
    "65mode" => "used when translating cells, not for normal use",
    "tapeout" => "fast mode for tapeout",
    "noproperties" => "for smaller files, do not write properties",
    "skip-libs" => "comma separated list of libs to skip or 'all' to just to the top lib",
    "define" => "CAST defines, may be specified multiple times",
);

sub usage {
my $name=$0;
$name =~ s:.*/::;
printf <<EU;
Usage: $name [options]
  --cast-path=[${$options{"cast-path=s"}}]
  --cell=[${$options{"cell=s"}}]
  --dfII-dir=[${$options{"dfII-dir=s"}}]
  --fulcrum-pdk-root=[${$options{"fulcrum-pdk-root=s"}}]
optional...
EU
my $prefixlen=0;
foreach my $key (sort keys %options) {
    if ($key ne "cell=s" and $key ne "dfII-dir=s"
            and $key ne "fulcrum-pdk-root=s" and $key ne "cast-path=s" ) {
        my $opt = $key;
        if (ref($options{$key}) eq "SCALAR") {
            $prefixlen = length("$opt [${$options{$key}}]")
                if $prefixlen < length("$opt [${$options{$key}}]");
        }
    }
}
foreach my $key (sort keys %options) {
    if ($key ne "cell=s" and $key ne "dfII-dir=s"
            and $key ne "fulcrum-pdk-root=s" and $key ne "cast-path=s" ) {
        my $opt = $key;
        $opt =~ s/[:=].*//;
        if (ref($options{$key}) eq "SCALAR") {
            if ($opt eq $key) {
                printf "   %-*.*s $help{$opt}\n", $prefixlen,$prefixlen, "--$opt [${$options{$key}}]";
            }
            else {
                printf "   %-*.*s $help{$opt}\n", $prefixlen,$prefixlen, "--$opt=[${$options{$key}}]";
            }
        }
        else {
            printf "   %-*.*s $help{$opt}\n", $prefixlen,$prefixlen, "--$opt";
        }
    }
}
    exit 1;
}

sub check_readable_dir {
    my ($dir,$msg) = @_;
    if ( ! -r "$dir") {
        print STDERR "$msg: \"$dir\" is not a readable directory.";
        return 1;
    }
    0;
}

sub check_readable_file {
    my ($file,$msg) = @_;
    if ( ! -r "$file" or ! -s "$file" ) {
        print STDERR "$msg: \"$file\" is not readable.";
        return 1;
    }
    0;
}

sub check_empty_arg {
    my ($arg,$msg) = @_;
    if ($arg eq "") {
        print STDERR "$msg";
        return 1;
    }
    0;
}

sub check_writeable_file {
    my ($theFile,$msg) = @_;
    my $dir=`dirname $theFile`;
    chomp $dir;
    if (( ( ! ( -f "$theFile" and -w "$theFile" ) ) and
        ( ( ! ( -f "$theFile" ) ) and ( ! ( -d "$dir" and -w "$dir" and -x "$dir" ) ) ) )) {
        print STDERR "$msg";
        return 1;
    }
    0;
}

my @args=();
foreach my $f (@ARGV) {
    push @args, $f;
}

GetOptions ( %options ) or $err++; 
$viewName=$srcViewName;

if ( ! defined ($workingDir) ) {
    if ( defined $ENV{TMP} and -d $ENV{TMP}) {
        $workingDir = `mktemp -d "$ENV{TMP}/gdsIIWriteWorkingDir.XXXXXX"`;
        chomp $workingDir;
    }
    else {
        $workingDir="/scratch";
    }
}
`mkdir -p "$workingDir"` if ! -d "$workingDir";
$workingDir=conon_path($workingDir);
if ( ! ( -w "$workingDir" and -d "$workingDir") ) {
    $err++;
    print STDERR "Can't write and delete in $workingDir";
}
my $gdsIIDataDir = `mktemp -d "$workingDir/gdsIIWriteDataDir.XXXXXX"`;
chomp $gdsIIDataDir;
setdirmode $gdsIIDataDir;
my $cdsWD;
if (! $debug and defined ($ENV{TMP}) and $ENV{TMP} =~ /scratch/) {
    $cdsWD=`mktemp -d "$ENV{TMP}/gdsIIWrite.XXXXXX"`;
}
else {
    mkdir "/scratch/$user";
    $cdsWD=`mktemp -d "/scratch/$user/gdsIIWrite.XXXXXX"`;
}
chomp $cdsWD;
setdirmode $cdsWD;
$pdkroot = $ENV{FULCRUM_PDK_ROOT} if ( ! $pdkroot );
if ($layers ne "") {
    my $opened=0;
    my $err=0;
    my %strm;
    my %mask;
    my %lpp;
    open (TF, "<$pdkroot/share/Fulcrum/tech_lib.ascii") or die "Warning: Cannot open $pdkroot/share/Fulcrum/tech_lib.ascii";
    my $ld=0;
    while (<TF>) {
        chomp;
        s/\t/ /g;
        s/ *;.*//;
        s/^  *//;
        s/  */ /g;
        s/^ //;
        if (/^streamLayers\(/) {
            $ld=1;
            next;
        }
        if (/^\)/) {
            $ld=0;
            next;
        }
        if ($ld) {
            s/[\(\)"]/ /g;
            s/  */ /g;
            s/^ //;
            s/ $//;
            my ($name,$purpose,$nr,$dt,$tr)=split;
            next if $tr ne "t";
            next if $nr == 0;
            $strm{"$name $purpose"}="$nr;$dt";
            $strm{"$nr;$dt"} .= "$name $purpose ";
        }
    }
    close TF;
    open (MO, "<$pdkroot/share/Fulcrum/stream/maskops.txt") or die "Warning: Cannot open $pdkroot/share/Fulcrum/stream/maskops.txt";
    my $df2=0;
    my $gds=0;
    while (<MO>) {
        chomp;
        s/\t/ /g;
        s/  */ /g;
        s/^ //;
        s/ $//;
        next if /^$/;
        s/[\(\)]/ /g;
        s/  */ /g;
        my @f=split;
        my $mask=shift @f;
        foreach my $f (@f) {
            if ($f =~ /;/) {
                my ($l,$p)=split(/;/,$f);
                $mask{$mask} .= "$l;$p ";
                $lpp{"$l;$p"} .= "$mask ";
            }
        }
    }
    close MO;
    my $star=0;
    if ($layers eq "*" or $layers eq "**") {
        $star=1;
        $star=2 if $layers eq "**";
        $layers = "1";
        # required IP layer for full tapeout
        $mask{1024} = "63;63";
        $lpp{"63;63"} = "1024";
        for(my $n = 1; $n <= 255; $n++) {
            $layers .= ",$n";
        }
    }
    my %done;
    my %masksaffected;
    $layertable = "$gdsIIDataDir/layertable";
    $opened = 1;
    open (LT, ">$layertable") or $opened = 0;
    foreach my $lpp (split(/,/,$layers)) {
        my ($layer,$purpose)=split(/;/,$lpp);
        my $count=0;
        if ($layer =~ /^\d+$/ and ($purpose eq "" or $purpose =~ /^\d+$/)) {
            if ($purpose eq "") {
                for (my $p = 0; $p <= 255; $p++) {
                    my $lpp = "$layer;$p";
                    next if ! $strm{$lpp}; # if not in tech file
                    next if ! $lpp{$lpp} and $star != 2; # if not in mask ops
                    $strm{$lpp} =~ s/ $//;
                    my @an=split(/ /,$strm{$lpp});
                    foreach my $mask (split(/ /,$lpp{$lpp})) {
                        $masksaffected{$mask}=1;
                    }
                    for (my $n = 0; $n < $#an; $n += 2) {
                        my $string = "$an[$n] $an[$n+1] $layer $p";
                        print LT "$string" if ! $done{$string};
                        $done{$string}=1;
                        $count++;
                    }
                }
            }
            else {
                my $lpp="$layer;$purpose";
                if (! $strm{$lpp}) {
                    warn "$lpp is not in tech file";
                }
                if (! $lpp{$lpp} and $star != 2) {
                    warn "$lpp is not in mask ops";
                }
                if ($strm{$lpp} and ($lpp{$lpp} or $star == 2)) {
                    $strm{$lpp} =~ s/ $//;
                    my @an=split(/ /,$strm{$lpp});
                    foreach my $mask (split(/ /,$lpp{$lpp})) {
                        $masksaffected{$mask}=1;
                    }
                    for (my $n = 0; $n < $#an; $n += 2) {
                        my $string = "$an[$n] $an[$n+1] $layer $purpose";
                        print LT "$string" if ! $done{$string};
                        $done{$string}=1;
                        $count++;
                    }
                }
            }
            if ($count==0) {
                my $lpp = "$layer;$purpose";
                $lpp .= "*" if $purpose eq "";
                print STDERR "Warning: lpp $lpp does not exist"
                    if ! $star;
            }
        }
        elsif ($layer =~ /^m/i) {
            $layer =~ s/^m//;
#            $masksaffected{$layer} = 1 if defined $lpp{$layer};
            foreach my $lpp (split(/ /,$mask{$layer})) {
                my ($l,$p) = split(/;/, $lpp);
                $strm{$lpp} =~ s/ $//;
                my @an = split(/ /, $strm{$lpp});
                foreach my $mask (split(/ /, $lpp{$lpp})) {
                    $masksaffected{$mask}=1;
                }
                for (my $n = 0; $n < $#an; $n += 2) {
                    my $string = "$an[$n] $an[$n+1] $l $p";
                    print LT "$string" if ! $done{$string};
                    $done{$string}=1;
                    $count++;
                }
            }
        }
        else {
            print STDERR "$lpp is malformed or undefined in layer list";
            $err++;
        }
    }
    close LT;
    $layertable="" if ! $opened;
    if ($err) {
        unlink $layertable;
        exit 1;
    }
    if (! $star) {
        print "MASKS AFFECTED:" if %masksaffected;
        foreach my $mask (sort keys %masksaffected) {
            print "MASK $mask";
        }
    }
} else {
    $layertable = "$pdkroot/share/Fulcrum/stream/strmout.layermap";
}
$verbose = 1 if $debug;
print STDERR "$0 @args" if $verbose;
if ($verbose) {
    foreach my $opt (sort keys %options) {
        my $xx=$opt;
        $xx =~ s/=.*//;
        my $value="undef";
        my $value = ${$options{$opt}}
            if defined $options{$opt} && ref($options{$opt}) eq 'SCALAR';
        print "  --$xx=[$value]" if $value ne "";
    }
}
$err+=check_empty_arg "$cell", "The cell that you want to stream out must be specified.";
$err+=check_empty_arg "$dfIIdir", "You must specify the location of directory containing all the dfII data.";
$err+=check_readable_dir "$dfIIdir", "dfII dir" if $dfIIdir ne "";
$err+=check_readable_file "$dfIIdir/cds.lib.generated", "dfII cds.lib.generated"
    if $dfIIdir ne "";
$err+=check_empty_arg "$pdkroot", "You must specify the location of the fulcrum pdk you want to use.";
$err+=check_readable_dir "$pdkroot", "fulcrum pdk root" if $pdkroot ne "";
$err+=check_empty_arg "$castpath", "You must specify a cast-path";
foreach my $path (split (/:/,$castpath)) {
    $err+=check_readable_dir "$path", "Cast Directory";
}
#$err+=check_empty_arg "$workingDir", "You must specify a working-dir";
$err+=check_readable_dir "$workingDir", "Working Directory";
#$err+=check_empty_arg "$bindfile", "You must specify an output bind.rul filename.";
$err+=check_empty_arg "$cadencelog", "You must specify a cadence log file.";
$err+=check_empty_arg "$assuralog", "You must specify a assura log file.";
$err+=check_empty_arg "$srcViewName", "You must specify the view of,\"$cell\" that you want to stream to GDSII.";
my $assurarule = "$pdkroot/share/Fulcrum/assura/bind.rul";
#$err+=check_readable_file "$assurarule", "Cannot find Assura bind.rul file in PDK"
#    if $pdkroot ne "";
my $propmap = "$pdkroot/share/Fulcrum/stream/prop.map";
$err+=check_readable_file "$propmap", "Cannot find prop.map file in PDK"
    if $pdkroot ne "";
usage if $err;

$libName = get_lib_name $cell;
$dfIIdir=conon_path $dfIIdir;
$bindfile=conon_path $bindfile if $bindfile ne "/dev/null";
$output=conon_path $output if defined $output and $output ne "";
$cadencelog=conon_path $cadencelog if $cadencelog ne "/dev/null";
$assuralog=conon_path $assuralog if $assuralog ne "/dev/null";
$pipolog=conon_path $pipolog if defined $pipolog;

my %list=();
my %cbinding=();
my %gbinding=();
my %nbinding=();
my %ibinding=();

sub checkTagCell {
    my ($cadcellname,$srcViewName,$viewName) = @_;
    my $celldir=$cadcellname;
    $celldir =~ s/-/#2d/g;
    $celldir =~ s/\./#2e/g;
    $celldir = "$libName/$celldir";
    $celldir =~ s/\./\//g;
    $celldir = "$dfIIdir/$celldir";
    my @src=stat("$celldir/$srcViewName/layout.oa");
    my @tag=stat("$celldir/$viewName/layout.oa");
    return (! @tag) or $tag[9] < $src[9] or $tag[7]==0;
}

sub gethier {
    my($cell, $library)=@_;
    my $path=$cell;
    return if $library eq "symbolic";
    return if (defined ($list{$cell}));
    $path =~ s/\.\d+$//;
    $path =~ s/\.[^\.]+$//;
    $path =~ s:\.:/:g;
    $path = "$dfIIdir/$path";
    my $dir=$cell;
    $dir =~ s/\(/-L/g;
    $dir =~ s/\)/-R/g;
    $list{$cell}=1;
    $dir =~ s/\./#2e/g;
    $dir =~ s/-/#2d/g;
    $path = "$path/$dir/$srcViewName/pc.db";
    local(*P,$_);
    if (open (P, "<$path")) {
        while (<P>) {
            chomp;
            my ($lib,$name,$lib)=split;
            if (defined ($name)) {
                gethier ($name,$lib);
            }
        }
    }
}

my $generateGDSIIDataOptions="";
$generateGDSIIDataOptions .= " --cadence-name" if ( $cadenceName );
my $bit64arg = "";
$bit64arg = " --64" if $bit64;
$generateGDSIIDataOptions .= $bit64arg;

my $gdsIIDataOutput = `mktemp "$workingDir/gdsIIWriteDataOut.XXXXXX"`;
chomp $gdsIIDataOutput;
setfilemode $gdsIIDataOutput;

my $generateGDSIIData="generate_gdsII_data";
$generateGDSIIData .= " \"--bind-all-nodes\"";
$generateGDSIIData .= " \"--max-heap-size=$maxheapsize\"";
$generateGDSIIData .= " \"--jre-args=$jreargs\"";
$generateGDSIIData .= " \"--jre=$jre\"" if $jre ne "" and -d $jre;
$generateGDSIIData .= " \"--cast-path=$castpath\"";
$generateGDSIIData .= " \"--cell=$cell\"";
$generateGDSIIData .= " \"--bind-rul=$assurarule\""
    if ( -s "$assurarule" );
$generateGDSIIData .= " \"--output-dir=$gdsIIDataDir\"";
$generateGDSIIData .= " \"--output-root-cell-name=$rootcellname\"";
$generateGDSIIData .= join('', map { " \"--define=$_\"" } @defines);
$generateGDSIIData .= " $generateGDSIIDataOptions \&> \"$gdsIIDataOutput\"";

if (! $tapeout ) {
print "$generateGDSIIData" if $verbose;
`$generateGDSIIData`;
if ($? != 0) {
    $err = 1;
    print STDERR "\"$generateGDSIIData\" exited with status: $?\n";
    exit $err;
}

opendir (DD, "$gdsIIDataDir");
foreach my $il (grep(/\.il$/, readdir(DD))) {
    unlink "$gdsIIDataDir/$il";
}
closedir DD;

`/bin/cp '$gdsIIDataDir/bind.rul' '$bindfile'` if $bindfile ne "/dev/null";
open (D, "<$gdsIIDataDir/bind.rul");
my $cl="";
while (<D>) {
    chomp;
    my ($type,$dfii,$gds)=split;
    if (/^C/) {
        my $pipo=$dfii;
#        print STDERR;
        $pipo =~ s/[^a-zA-Z0-9_]/_/g;
        my $st=$gds;
        my $fst="undef";
        if ($verilog and $gds ne $rootcellname) {
            my $x=c2convert($gds);
            $fst=$x;
            $x =~ s/\(/_L_/g;
            $x =~ s/\)/_R_/g;
            $x =~ s/\./__/g;
            $x =~ s/[^A-Za-z0-9_]/_/g;
            $gds=$x;
        }
        $cbinding{$pipo}=$gds;
        $gbinding{$dfii}=$pipo;
#        print STDERR "BINDC $st => $fst => $gds";
#        print STDERR "BINDG $dfii => $pipo" if /SRAMFIFO32/;
        $cl=$gds;
    }
    elsif (/^N/) {
        if ($verilog) {
            my $x = n2convert($gds);
            $x =~ s/\(/_L_/g;
            $x =~ s/\)/_R_/g;
            $x =~ s/\./__/g;
            $x =~ s/[^A-Za-z0-9_]/_/g;
            $gds=$x;
        }
        $nbinding{"$cl $dfii"} = $gds;
    }
    if (/^I/) {
        if ($verilog) {
            my $x=i2convert($gds);
            $x =~ s/\(/_L_/g;
            $x =~ s/\)/_R_/g;
            $x =~ s/\./__/g;
            $x =~ s/[^A-Za-z0-9_]/_/g;
            $gds=$x;
        }
        $ibinding{"$cl $dfii"} = $gds;
    }
}
close D;
}
my $myrsf=$template;
# make sure we have the property maps correct
my %propmap;
if (! $noproperties) {
    open (P, "<$propmap");
    while (<P>) {
        chomp;
        if (/^\d/) {
            my ($nr,$type,$name)=split;
            $propmap{$type}[0]=$nr if (defined ($name));
            $propmap{$type}[1]=$name if (defined ($name));
        }
    }
    close P;
}
$pipolog = "$cdsWD/strmOut.log" if ($debug or ! defined $pipolog);
my $mkcdswd="mkcdswd \"--dfII-dir=$dfIIdir\" \"--fulcrum-pdk-root=$pdkroot\" \"--cast-path=$castpath\" \"--target-dir=$cdsWD\" \"--force\"";
if ( $verbose ) {
    print "Making cadence working directory.";
    print "$mkcdswd";
}
system "$mkcdswd";

if ($use_tag) {
    my $ft;
    my $pc=`echo '$cell' | rename --type=cell --from=cast --to=cadence`;
    chomp $pc;
    my @lib=split(/\./, $pc);
    pop @lib;
    pop @lib;
    my $lib = join("/", @lib);
    my $path=$pc;
    $path =~ s/\./#2e/g;
    $path =~ s/-/#2d/g;
    $path = "$dfIIdir/$lib/$path/custom_tag/layout.oa";
    if ( -s "$path" and -f "$path") {
        $viewName="custom_tag";
        open ($ft, ">$cdsWD/mktag.il");
        print $ft <<EFT;
exportTotemLef("$pc" "$viewName")
exit
EFT
        close $ft;
        system("cd '$cdsWD'; $ENV{IC_SCRIPT} layout -nograph -replay '$cdsWD/mktag.il' -log '$cdsWD/mktag.log'");
    }
    else {
        open ($ft, ">$cdsWD/mktag.il");
        print $ft <<EFT;
MakeLayoutTagCell("$pc" ?orientation "$tag_orientation" ?ViewName "$srcViewName" ?TargetViewName "${srcViewName}_tag" ?CastPath "$castpath" ?TempDir "$cdsWD" )
exit
EFT
        close $ft;
        system("cd '$cdsWD'; $ENV{IC_SCRIPT} layout -nograph -replay '$cdsWD/mktag.il' -log '$cdsWD/mktag.log'");
        $viewName="${srcViewName}_tag";
        if (checkTagCell($pc,$srcViewName,$viewName)) {
            print STDERR "Error: did not create $viewName properly";
            exit 1;
        }
        if ( -r "$cdsWD/$pc.ploc" and -d $workingDir ) {
            my $outputDir=$output;
            $outputDir =~ s:/[^/]+$::;
            `/bin/cp -p "$cdsWD/$pc.ploc" "$workingDir/cell.ploc"`;
            `/bin/cp -p "$cdsWD/$pc.ploc" "$outputDir/cell.ploc"`;
        }
    }
}
if ($tapeout and $rootcellname ne "") {
    my %cellmap=();
    my $pc=`echo '$cell' | rename --type=cell --from=cast --to=cadence`;
    chomp $pc;
    if ($maxnamelen > 0) {
        my $subcellnr=1;
        open (X, "dfIIhier --dfII-dir='$dfIIdir' --fulcrum-pdk-root='$pdkroot' --view='$srcViewName' '$pc' |") or warn "Cannot run dfIIhier";
        while (<X>) {
            chomp;
            s/^\s+//;
            my ($cn,$ln,$vn)=split;
            if (length($cn) > $maxnamelen and ! defined ($cellmap{"$cn $vn"})) {
                $cellmap{"$cn $vn"}=sprintf("SUBCELL%04d", $subcellnr++);
            }
        }
        close X;
    }
    my $pc=`echo '$cell' | rename --type=cell --from=cast --to=cadence`;
    chomp $pc;
    print "VIEWS $viewName $srcViewName";
    $cellmap{"$pc $viewName"}=$rootcellname;
    $cellmaptable = "$gdsIIDataDir/cellmaptable";
    open (P, ">$cellmaptable");
    foreach my $cn (sort keys %cellmap) {
        my @lib=split(/\./, $cn);
        pop @lib;
        pop @lib;
        my $lib=join(".", @lib);
        print P "$lib $cn $cellmap{$cn}";
    }
    close P;
}
$output = "$workingDir/$cell.gdsII" if ( ! $output );
my $tmpgds;
if (! $tapeout) {
    if (! $debug and defined ($ENV{TMP}) and $ENV{TMP} =~ /scratch/) {
        $tmpgds = `mktemp "$ENV{TMP}/gdsIIWritegds.XXXXXX"`;
    }
    else {
        $tmpgds = `mktemp "/scratch/$user/gdsIIWritegds.XXXXXX"`;
    }
    chomp $tmpgds;
    setfilemode $tmpgds;
}
else {
    $tmpgds = $output;
}
if ($flattenpcells) {
    $myrsf =~ s/COMMENTPCELLS//;
}
else {
    $myrsf =~ s/COMMENTPCELLS/#/;
}
if ($flattenvias) {
    $myrsf =~ s/COMMENTVIAS//;
}
else {
    $myrsf =~ s/COMMENTVIAS/#/;
}
my $dfcell=cconvert($cell);
close CWR;
close CRD;
waitpid $cpid,0;
my $pwd=`pwd`;
chomp $pwd;

if (@skip_libs) {
    my $fr;
    my $all=0;
    $refLibList="$pwd/refLibList$$.txt";
    open ($fr, ">$refLibList");
    foreach my $lib (@skip_libs) {
        $all=1 if $lib eq "all";
    }
    if ($all) {
        print $fr "XST_CDS_LIB";
#        open ($fl, "<$dfIIdir/cds.lib.generated");
#        while (<$fl>) {
#            chomp;
#            my ($d, $lib, $dir)=split;
#            $lib =~ s/#2e/./g;
#            print $fr $lib;
#        }
#        close $fl;
        print $fr "gate";
        print $fr "stack";
        print $fr "731";
    }
    else {
        print $fr join("\n", @skip_libs);
    }
    close $fr;
}
$dfcell =~ s/\(/-L/g;
$dfcell =~ s/\)/-R/g;
$myrsf =~ s:REFLIBLIST:$refLibList:g;
$myrsf =~ s:RUNDIR:$workingDir:g;
$myrsf =~ s/CELLNAME/$dfcell/g;
$myrsf =~ s/VIEWNAME/$viewName/g;
if ($tapeout or $noproperties) {
$myrsf =~ s/PROPMAP//g;
}
else {
$myrsf =~ s/PROPMAP/$propmap/g;
}
$myrsf =~ s/LIBNAME/$libName/g;
$myrsf =~ s:GDSIINAME:$tmpgds:g;
$myrsf =~ s/LOGFILE/$pipolog/g;
$myrsf =~ s:LAYERTABLE:$layertable:g;
$myrsf =~ s:CELLMAPTABLE:$cellmaptable:g;
my $rsffile;
if (! $debug and defined ($ENV{TMP}) and $ENV{TMP} =~ /scratch/) {
    $rsffile=`mktemp "$ENV{TMP}/template.XXXXXX"`;
}
else {
    $rsffile=`mktemp "/scratch/$user/template.XXXXXX"`;
}
chomp $rsffile;
setfilemode $rsffile;
open (P, ">>$rsffile") or die "Opening $rsffile : $!";
print P "$myrsf\n";
close P;
my $pipoout=">/dev/null";
$pipoout = "" if $verbose;
$bit64arg = " -64" if $bit64;
my $ldassumekernel="";
my $unamer = `uname -r`;
chomp $unamer;
$unamer =~ s/.*FC/FC/;
$unamer = substr($unamer,0,3);
$ldassumekernel = "LD_ASSUME_KERNEL=2.4.1"
    if ($unamer eq "FC2" and ! $bit64 and ! defined ($ENV{CDS_AUTO_64BIT}));
my $cmd = "cd '$cdsWD'; strmout $bit64arg -templateFile '$rsffile'";
print "$cmd" if $verbose;
open (X, "$cmd |");
while (<X>) {
    chomp;
    if (/There were.*error/) {
        my @f=split;
        $err += $f[3];
        print if ($err) and ! $verbose;
    }
    if (/^ERROR/) {
        $err++;
        print if ! $verbose;
    }
    print if $verbose;
}
close X;
unlink $refLibList;
if ($err) {
    print STDERR "Errors encountered in strmout, re-run in debug mode with pipo-log defined to see details.";
    exit $err;
}

# this is supposed to deal with the mangled names which
# pipo generates for pcells. It does not work right now
# but neither does the standard gdsIIWrite.
my %variant=();
my $gate;
open (P, "<$workingDir/xStrmOut_cellMap.txt");
while (<P>) {
    chomp;
#    if (/\slayout\s/ or /\ssymbolic\s/) {
#    if (/SRAMFIFO32/) {
        s/\s+/ /g;
        s/^\s//;
        my ($lib,$gate,$view,$cell)=split;
#        $cell =~ s/\)//;
#        $cell =~ s/[^a-zA-Z0-9_\$]/_/g;
        $variant{$cell}=$gbinding{$gate};
#        print STDERR "VARIANT $cell => $variant{$cell} $gate : $_";
#    }
}
close P;
# translate the gds
if (! $tapeout) {
open (P, "rdgds '$tmpgds' |");
open (Q, "| wrgds > '$output'");
my $strname="";
my $base="";
my $prop="";
while (<P>) {
    chomp;
    s/^ *//;
    if (/^STRNAME/ or /^SNAME/) {
        # translate structure names
        my ($s,$snpipo)=split;
        my $sn = $snpipo;
        $sn = $cbinding{$snpipo} if (defined ($cbinding{$sn}));
        print Q "$s $sn";
        $strname = $sn if (/^STRNAME/);
        $base = $strname;
        $base = $variant{$base} if defined $variant{$base};
#        print STDERR "BASE $strname => $base\n" if $strname =~ /SRAMFIFO32/;
        next;
    }
    elsif (/^TEXT/) {
        # dump text which does not agree with the port map of the structure
        my $el="";
        my $ok=1;
        my $iplayer=0;
        $el = "$_\n";
        while (<P>) {
            chomp;
            s/^ *//;
            if (/LAYER 63/) {
                $iplayer=1;
            }
            if (/^STRING/) {
                my $sn = $_;
                $sn =~ s/^STRING //;
                $sn =~ s/'//g;
                my $xx=$nbinding{"$base $sn"};
#                print STDERR "BIND $base $sn $xx" if $base =~ /SRAMFIFO32/ and $sn eq 'Vdd';
                $ok &= defined ($nbinding{"$base $sn"});
                $sn = $nbinding{"$base $sn"}
                    if defined ($nbinding{"$base $sn"}) and ! ($base =~ /^(gate|stack)/);
                $_ = "STRING '$sn'";
            }
            $el .= "$_\n";
            last if /^ENDEL/;
        }
        chomp $el;
#        $ok = 1 if $base =~ /SRAMFIFO32/;
        print Q $el if ($ok or $iplayer);
        next;
    }
    elsif (defined ($propmap{pin}) and /^PROPATTR $propmap{pin}[0]/ and $suppresspins) {
        # remove pins properties
        next;
    }
    elsif (defined ($propmap{pin}) and /^PROPATTR $propmap{pin}[0]$/) {
        # setup to include appropriate properties
        $prop = $_;
        next;
    }
    elsif (defined ($propmap{pin}) and /^PROPVALUE $propmap{pin}[1]=/) {
        next if $suppresspins;
        # check pin property for validity
        my ($s,$sn)=split;
        $sn =~ s/'//g;
        $sn =~ s/.*=//;
        if (defined ($nbinding{"$base $sn"})) {
            $sn = $nbinding{"$base $sn"};
            print Q "$prop\n$s termName=$sn";
        }
        next;
    }
    elsif (defined ($propmap{inst}) and /^PROPATTR $propmap{inst}[0]$/) {
        $prop = $_;
        next;
    }
    elsif (defined ($propmap{inst}) and /^PROPVALUE $propmap{inst}[1]=/) {
        my ($s,$sn)=split;
        # convert instance property names
        $sn =~ s/'//g;
        $sn =~ s/.*=//;
        $sn =~ s/^\|//g;
        # should this be converted??
        $sn = iconvert($sn);
        print Q "$prop\n$s instName=$sn";
#        print Q "$s instName=$sn";
        next;
    }
    if (/^LIBNAME/) {
        $_ = "LIBNAME GDSIIWRITE0.DB";
    }
    print Q;
}
close IWR;
close IRD;
waitpid $ipid,0;
close P;
close Q;
}
if ($sixtyfivemode) {
    system "/bin/mv '$output' '$output.130'";
    # avoid problems with ',' in path
    my $workdir=$output;
    $workdir =~ s:/([^/]+)$::;
    my $outfile=$1;
    my $rv=system "( cd '$workdir'; size265 '$outfile.130' '$outfile')";
    print STDERR "Error: size265 $output.130 $output $rv" if $rv;
}
print STDERR "One or more errors encountered" if ($err);

END {
    if (defined ($outbind)) {
        # create binding file
        if ( -s "$assurarule") {
            system "/bin/cp '$pdkroot/share/Fulcrum/assura/bind.rul' '$outbind'";
        }
        else {
            system "/bin/touch '$outbind'";
        }
        system "chmod +w '$rootcellname.bind'";
        open (O, ">>$outbind");
        if (-f "$workingDir/cellName.map") {
            open (P, "<$workingDir/cellName.map");
            while (<P>) {
                chomp;
                next if /^#/;
                my ($out,$v,$in)=split;
                print O "C $out $in";
            }
        }
        if (-f "$cellmaptable") {
            open (P, "<$cellmaptable");
            while (<P>) {
                chomp;
                next if /^#/;
                my ($out,$v,$in)=split;
                print O "C $out $in";
            }
        }
#        my $pc=`echo '$cell' | rename --type=cell --from=cast --to=cadence`;
#        chomp $pc;
#        print O "C $pc $rootcellname";
        close P;
        close O;
    }
    if ($cleanup) {
        if ($err) {
            unlink "$output";
        }
        unlink "$tmpgds" if ! $tapeout;
        unlink "$gdsIIDataOutput";
        `/bin/rm -rf "$gdsIIDataDir"`;
        unlink "$rsffile";
        unlink "$workingDir/pipo_xout_info";
        unlink "$workingDir/cellName.map";
        `/bin/rm -rf "$cdsWD"`;
    }
    exit $err;
}
