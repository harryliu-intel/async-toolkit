#!/usr/intel/bin/perl -l
# AAG
# $Id$
# $DateTime$

use strict;
use IPC::Open2;
use Getopt::Long;
my $gnd="GND";
my $vdd="Vdd";
my $default_prefix="";
my $debug=0;
my $skip_cds_lib_generated=0;
my $multipletopcells=0;

my $template = <<ET;
#dbuPerUU                          "0"
#hierDepth                         "32767"
#maxCellsInTargetLib               "20000"
#numThreads                        "1"
#strmTextNS                        "cdba"
#append
#arrayInstToScalar
attachTechFileOfLib                "TECHLIB"
case                               "preserve"
cellMap                            "BINDFILE"
#checkPolygon
#convertPathToPathSeg
#detectVias
#disableLocking
excludeMapToVia                    ""
fontMap                            ""
#ignoreBoxes
#ignoreZeroWidthPath
#keepStreamCells
labelCase                          "preserve"
layerMap                           "LAYERMAP"
library                            "LIBNAME"
loadTechFile                       ""
logFile                            "LOGFILE"
#mergeUndefPurposToDrawing
noInfo                             ""
#noOverwriteCell
noWarn                             ""
objectMap                          ""
pinAttNum                          "0"
propMap                            "propQ.map"
propSeparator                      ","
refLibList                         "REFLIB"
#replaceBusBitChar
#reportPrecisionLoss
runDir                             "."
scaleTextHeight                    "0.0500"
skipUndefinedLPP
#snapToGrid
strmFile                           "GDSFILE"
strmTechGen                        ""
summaryFile                        ""
techRefs                           ""
topCell                            "TOPCELL"
#translateNode
userSkillFile                      ""
viaMap                             ""
view                               "layout"
warnToErr                          ""
ET

my $gdpid=open2(\*GDOUT,\*GDIN, "rename","--type=cell","--from=cadence","--to=cast");
my %gds=();
my %cells=();
my $default_prefix="";
my $verbose=0;
my $library="";
my $pdkroot="";
my $bindfile="";
my $subtype = 0;
my $force=0;
my $topcell;
my $dfIIdir;
my %xref;
my $dopipo=0;
my $tech_vias=0;

sub usage {
    print "Error: $_[0]" if defined $_[0];
    print <<EU;
Usage: importGDS [options] gdsfile
    --verbose
    --prefix=<name prefix, usually same as library by default>
    --library=<destination lib name>
    --fulcrum-pdk-root=<needed if pwd dir is not a cdswd
    --dfII-dir=<needed if pwd dir is not a cdswd
    --bind-file=<file> use if not using default name
    --subtype=[$subtype]
    --gnd=[$gnd]
    --vdd=[$vdd]
    --dopipo run pipo automatically
    --tech-vias (experimental, dangerous if misused)
    --isflat (special if you KNOW the cell is flat)
   [--force]
    gdsfile
EU
exit 1;
}

my %options = (
    "force" => \$force,
    "verbose" => \$verbose,
    "prefix=s" => \$default_prefix,
    "library=s" => \$library,
    "fulcrum-pdk-root=s" => \$pdkroot,
    "dfII-dir=s" => \$dfIIdir,
    "bind-file=s" => \$bindfile,
    "topcell=s" => \$topcell,
    "subtype=i" => \$subtype,
    "dopipo" => \$dopipo,
    "tech-vias" => \$tech_vias,
    "gnd=s" => \$gnd,
    "vdd=s" => \$vdd,
    "isflat" => \$skip_cds_lib_generated,
);

GetOptions ( %options ) or usage;

usage "Need --library" if $library eq "";

if ( $default_prefix eq "") {
    $default_prefix = $library;
}
if ( !defined ($subtype)) {
    $subtype=400;
}
if ( ! -f "cds.lib") {
    my $wd=`pwd`;
    chomp $wd;
    print "mkcdswd --target-dir=$wd --fulcrum-pdk-root=$pdkroot --dfII-dir=$dfIIdir"
        if $verbose;
    system "mkcdswd --target-dir=$wd --fulcrum-pdk-root=$pdkroot --dfII-dir=$dfIIdir";
}
if ($library ne "") {
    my $cdsname=$library;
    $cdsname =~ s/\./#2e/g;
    my $exists=`grep -c "^DEFINE $cdsname " cds.lib 2>/dev/null`;
    chomp $exists;
    if ($exists == 0) {
        $exists=`grep -c "^DEFINE $cdsname " $dfIIdir/cds.lib.generated 2>/dev/null`;
    }
    printf "Library $library %s\n", $exists ? "EXISTS" : "Does Not Exist" if $verbose;
    if ($exists == 0) {
        open (P, ">>cds.lib");
        print P "UNDEFINE $cdsname";
        print P "DEFINE $cdsname $library";
        close P;
        if ($library ne "" and ! ($library =~ /^\./)) {
           print "/bin/rm -rf '$library'" if $verbose;
           system "/bin/rm -rf '$library'";
           print "mkdir '$library'";
           mkdir "$library";
        #    print "/bin/cp -rp template '$library'" if $verbose;
        #    system "/bin/cp -rp template '$library'";
        }
    }
}

sub gdsread {
    my ($file)=@_;
    local(*Q);
    my $top = "";
    my %ref=();
    my %string=();
    local(*P,$_);
    if ( -f "$file.cells" and ! $force) {
        print STDERR "READING $file.cells" if $verbose;
        open (P, "<$file.cells");
        open (Q, ">/dev/null");
    }
    else {
        print STDERR "READING $file" if $verbose;
        open (P, "rdgds $file |");
        open (Q, ">$file.cells");
    }
    my $last="";
    while (<P>) {
        chomp;
        s/^  *//;
        my ($key,$name)=split;
        if ($key eq "STRNAME") {
            print Q if ! defined $gds{$name};
            $gds{$name}=1;
            $cells{$name}=1;
        }
        if ($key eq "SNAME") {
            print Q if ! defined $ref{$name};
            $ref{$name}++;
            $cells{$name}=1;
        }
        if ($key eq "STRING") {
            $name =~ s/'//g;
            print Q if ! defined $string{$name};
            $string{$name}=1;
        }
        $last=$_;
    }
    close P;
    close Q;
    if (! defined ($topcell)) {
        my $top="";
        my $topcnt = 0;
        foreach my $key (keys %gds) {
            if (! defined ($ref{$key}) ) {
                $top=$key;
                $topcnt++;
            }
        }
        if ($topcnt > 1) {
            print STDERR "Multiple topcells found";
            $multipletopcells=1;
            $topcell="";
        }
        elsif ($topcnt == 0 or $top eq "") {
            print STDERR "No topcell found";
        }
        else {
            $topcell = $top;
        }
    }
    print STDERR "Warning: NO $gnd pin" if ! defined $string{$gnd};
    print STDERR "Warning: NO $vdd pin" if ! defined $string{$vdd};
}

sub readlib {
    my ($lib,$path)=@_;
    local(*P,$_,*D);
    return if ($skip_cds_lib_generated and $lib =~ /cds\.lib\.generated/);
    print STDERR "READING $lib" if $verbose;
    open (P, "<$lib");
    $path = `dirname $lib` if ( ! defined ($path));
    chomp $path;
    while (<P>) {
        chomp;
        next if (/^#/);
        my ($cmd,$libname,$pathname)=split;
        if ($cmd eq "SOFTINCLUDE" and -f $libname) {
            $libname = "$path/$libname" if ( ! ($libname =~ m:^/:));
            readlib($libname);
        }
        elsif ($cmd eq "DEFINE" and $lib ne "cds.lib") { # don't use local files
            $libname =~ s/#2e/./g;
            #next if $libname eq $library;
            # the following hack is until there is a command line option
            next if $libname =~ /^chip\.tc4\./;
            next if $libname =~ /^chip\.alta\./;
            next if $libname =~ /^synthesis/;
            $pathname = "$path/$pathname" if ( ! ($pathname =~ m:^/:));
            opendir (D, "$pathname");
            my @dirs=sort( readdir (D));
            closedir D;
            $|=1;
            foreach my $dir (@dirs) {
                next if (($dir =~ /^[RS]/) or ! ($dir =~ /#/)); # can only be regfile/sram subcells
                if ( -s "$pathname/$dir/layout/layout.oa") {
                    my $dfIIcell1 = $dir;
                    $dfIIcell1 =~ s/#2e/./g;
                    $dfIIcell1 =~ s/#2d/-/g;
                    $dfIIcell1 =~ s/#24/\$/g;
                    my $dfIIcell2;
                    my $cell1;
                    my $cell2;
                    my $cell3;
                    undef $cell1;
                    undef $cell2;
                    undef $cell3;
                    print "A $dfIIcell1" if $debug;
                    print GDIN  "$dfIIcell1";
                    $cell1=<GDOUT>;
                    chomp $cell1;
                    $dfIIcell2=$dfIIcell1;
                    print "B $cell1" if $debug;
                    # note: assumes numeric subtype
                    $dfIIcell2 =~ s/\.\d+$//;
                    $dfIIcell2 =~ s/.*\.//;
                    print GDIN  "$dfIIcell2";
                    $cell2=<GDOUT>;
                    chomp $cell2;
                    print "B $cell2" if $debug;
                    my $xref1=$cell1;
                    my $xref2=$cell2;
                    my $xref3=$cell1;
                    # pipo strmout names
                    $xref1 =~ s/[^A-Za-z0-9_]/_/g;
                    if ($xref{$xref1}[0] > 0) {
                        my $subtype1 = $dfIIcell1;
                        $subtype1 =~ s/.*\.//;
                        my $subtype0 = $xref{$xref1}[1];
                        $subtype0 =~ s/.*\.//;
                        if ($dfIIcell1 eq $xref{$xref1}[1]) {
                            $xref{$xref1}[0]--;
                        }
                        elsif (($subtype1 =~ /^\d+$/) and ($subtype0 =~ /^\d+$/) and
                                $subtype1 < $subtype0) {
                            $xref{$xref1}[2] = $xref{$xref1}[1];
                            $xref{$xref1}[1] = $dfIIcell1;
                            print "ST1a:$xref{$xref1}[1]:$xref{$xref1}[2]" if $debug;
                        }
                        elsif (($xref{$xref1}[1] cmp $dfIIcell1) > 0) {
                            $xref{$xref1}[2] = $xref{$xref1}[1];
                            $xref{$xref1}[1] = $dfIIcell1;
                            print "ST1b:$xref{$xref1}[1]:$xref{$xref1}[2]" if $debug;
                        }
                        else {
                            $xref{$xref1}[2] = $dfIIcell1;
                            print "ST1c:$xref{$xref1}[1]:$xref{$xref1}[2]" if $debug;
                        }
                    }
                    else {
                        $xref{$xref1}[1]=$dfIIcell1;
                    }
                    $xref{$xref1}[0]++;
                    $xref2 =~ s/[^A-Za-z0-9_]/_/g;
                    if ($xref{$xref2}[0] > 0) {
                        my $subtype1 = $dfIIcell1;
                        $subtype1 =~ s/.*\.//;
                        my $subtype0 = $xref{$xref2}[1];
                        $subtype0 =~ s/.*\.//;
                        if ($dfIIcell1 eq $xref{$xref2}[1]) {
                            $xref{$xref2}[0]--;
                        }
                        elsif (($subtype1 =~ /^\d+$/) and ($subtype0 =~ /^\d+$/) and
                                $subtype1 < $subtype0) {
                            $xref{$xref2}[2] = $xref{$xref2}[1];
                            $xref{$xref2}[1] = $dfIIcell1;
                            print "ST2a:$xref{$xref2}[1]:$xref{$xref2}[2]" if $debug;
                        }
                        elsif (($xref{$xref2}[1] cmp $dfIIcell1) > 0) {
                            $xref{$xref2}[2] = $xref{$xref2}[1];
                            $xref{$xref2}[1] = $dfIIcell1;
                            print "ST2b:$xref{$xref2}[1]:$xref{$xref2}[2]" if $debug;
                        }
                        else {
                            $xref{$xref2}[2] = $dfIIcell1;
                            print "ST2c:$xref{$xref2}[1]:$xref{$xref2}[2]" if $debug;
                        }
                    }
                    else {
                        $xref{$xref2}[1]=$dfIIcell1;
                    }
                    $xref{$xref2}[0]++;

                    $xref3 =~ s/-L/_L_/g;
                    $xref3 =~ s/-R/_R_/g;
                    $xref3 =~ s/\./__/g;
                    if ($xref{$xref3}[0] > 0) {
                        my $subtype1 = $dfIIcell1;
                        $subtype1 =~ s/.*\.//;
                        my $subtype0 = $xref{$xref3}[1];
                        $subtype0 =~ s/.*\.//;
                        if ($dfIIcell1 eq $xref{$xref3}[1]) {
                            $xref{$xref2}[0]--;
                        }
                        elsif (($subtype1 =~ /^\d+$/) and ($subtype0 =~ /^\d+$/) and
                                $subtype1 < $subtype0) {
                            $xref{$xref3}[2] = $xref{$xref3}[1];
                            $xref{$xref3}[1] = $dfIIcell1;
                            print "ST2a:$xref{$xref3}[1]:$xref{$xref3}[2]" if $debug;
                        }
                        elsif (($xref{$xref3}[1] cmp $dfIIcell1) > 0) {
                            $xref{$xref3}[2] = $xref{$xref3}[1];
                            $xref{$xref3}[1] = $dfIIcell1;
                            print "ST2b:$xref{$xref3}[1]:$xref{$xref3}[2]" if $debug;
                        }
                        else {
                            $xref{$xref3}[2] = $dfIIcell1;
                            print "ST2c:$xref{$xref3}[1]:$xref{$xref3}[2]" if $debug;
                        }
                    }
                    else {
                        $xref{$xref3}[1]=$dfIIcell1;
                    }
                    $xref{$xref3}[0]++;

                    # always xref to self
                    if ($xref{$dfIIcell1}[0] > 0) {
                        my $subtype1 = $dfIIcell1;
                        $subtype1 =~ s/.*\.//;
                        my $subtype0 = $xref{$dfIIcell1}[1];
                        $subtype0 =~ s/.*\.//;
                        if ($dfIIcell1 eq $xref{$dfIIcell1}[1]) {
                            $xref{$dfIIcell1}[0]--;
                        }
                        elsif (($subtype1 =~ /^\d+$/) and ($subtype0 =~ /^\d+$/) and
                                $subtype1 < $subtype0) {
                            $xref{$dfIIcell1}[2] = $xref{$dfIIcell1}[1];
                            $xref{$dfIIcell1}[1] = $dfIIcell1;
                            print "ST3a:$xref{$dfIIcell1}[1]:$xref{$dfIIcell1}[2]" if $debug;
                        }
                        elsif (($xref{$dfIIcell1}[1] cmp $dfIIcell1) > 0) {
                            $xref{$dfIIcell1}[2] = $xref{$dfIIcell1}[1];
                            $xref{$dfIIcell1}[1] = $dfIIcell1;
                            print "ST3b:$xref{$dfIIcell1}[1]:$xref{$dfIIcell1}[2]" if $debug;
                        }
                        else {
                            $xref{$dfIIcell1}[2] = $dfIIcell1;
                            print "ST3c:$xref{$dfIIcell1}[1]:$xref{$dfIIcell1}[2]" if $debug;
                        }
                    }
                    else {
                        $xref{$dfIIcell1}[1]=$dfIIcell1;
                    }
                    $xref{$dfIIcell1}[0]++;
                    print "XREF 1 $xref1 => $dfIIcell1" if $verbose;
                    print "XREF 2 $xref2 => $dfIIcell1" if $verbose;
                    print "XREF 3 $xref3 => $dfIIcell1" if $verbose;
                    print "XREF 4 $dfIIcell1 => $dfIIcell1" if $verbose;
                }
            }
        }
        elsif ($cmd eq "INCLUDE") {
            printf "INCLUDE $path $libname\n" if $verbose;
            $libname = "$path/$libname" if ( ! ($libname =~ m:^/:));
            readlib($libname);
        }
    }
}

my $file;
$file = $ARGV[0] if defined $ARGV[0];
usage if (! defined ($file));
if ( ! -s "textFontTable.txt" or $force) {
    open (T, ">textFontTable.txt");
    print T "#Cadence Font Stream Font Scale Factor\nstick 0 0.1";
    close T;
}
if ( ! -s "propQ.map" or $force) {
    open (T, ">propQ.map");
    print T <<T;
40	scalarinst	instName
42	scalarinst	netName
40	arrayinst	instName
42	arrayinst	netName
42	path	netName
42	rect	netName
42	polygon	netName
T
    close T;
}
if ($bindfile eq "") {
    $bindfile=$file;
    $bindfile =~ s/\.gds.*/.bind/;
}
gdsread("$file");
readlib("cds.lib");
$xref{$topcell}[0]=1;
$xref{$topcell}[1]=$default_prefix;

my $writebind=0;
my %bind;
print STDERR "Reading $bindfile" if $verbose;
if ( -s $bindfile ) {
    if (open (P, "<$bindfile")) {
        while (<P>) {
            chomp;
            my @f=split;
            if ($#f == 3) {
                $bind{$f[3]}=$f[0];
            }
        }
    }
}
if ((! -s "$bindfile") or $force) {
    if (open P,">$bindfile") {
        $writebind=1;
        select P;
        print STDERR "Writing $bindfile" if $verbose;
    }
    else {
        print STDERR "Cannot open bind file for writing, reverting to reading it.";
    }
}
my %symbolic=();
if ($tech_vias) {
    # override xrefs with the pdk techlib cells, experimental!
    opendir D, "$pdkroot/share/Fulcrum/dfII";
    my @dfiles=grep(/^tsmc/, readdir(D));
    closedir D;
    chomp @dfiles;
    if ($#dfiles == 0) {
        opendir D, "$pdkroot/share/Fulcrum/dfII/$dfiles[0]";
        my @dirs=readdir(D);
        closedir D;
        foreach my $dir (@dirs) {
            if ( -d "$pdkroot/share/Fulcrum/dfII/$dfiles[0]/$dir/symbolic" or
                 -d "$pdkroot/share/Fulcrum/dfII/$dfiles[0]/$dir/layout") {
                $xref{$dir}[0] = 1;
                $xref{$dir}[1] = $dir;
                $symbolic{$dir} = 1 if -d "$pdkroot/share/Fulcrum/dfII/$dfiles[0]/$dir/symbolic";
            }
        }
    }
    else {
        print STDERR "Warning: No techlib found";
    }
}
my $fatal=0;
foreach my $cell (sort keys %cells) {
    print STDERR "CELL $cell" if $verbose;
    if (! $writebind and ! defined ($bind{$cell})) {
        print STDERR "Error: $cell is not in $bindfile";
        $fatal++;
    }
    if (defined ($xref{$cell})) {
        my $xrefcell=$xref{$cell}[1];
        if ($xref{$cell}[0] > 1 and $default_prefix ne "") {
            my $x1 = $xref{$cell}[1];
            my $x2 = $xref{$cell}[2];
            $x1 =~ s/\.[^\.]+$//;
            $x2 =~ s/\.[^\.]+$//;
            if ($x1 ne $x2) {
                $xrefcell =~ s/^[^\.]+\./$default_prefix./;
            }
        }
        if ( ! ($xrefcell =~ /\./)) {
        }
        elsif ($xrefcell =~ /_(VIA[a-zA-Z0-9_]+)$/i) {
            $xrefcell = $default_prefix."_".$1;
        }
        elsif ($xrefcell =~ /via/i) {
            $xrefcell =~ s/$library/$default_prefix/;
        }
        elsif (! ($xrefcell =~ /\./)) {
            $xrefcell = "${default_prefix}.$xrefcell";
        }
        if ($writebind) {
            if (defined($bind{$cell})) {
               if( $bind{$cell}=~/(\.[0-9A-Z_of]+\.[0-9]+)$/ ){
                  my $thisLib=$bind{$cell};
                  $thisLib=~s/$1//g; 
                  print "$thisLib $bind{$cell} layout $cell";
	       } else {
                  print "$library $bind{$cell} layout $cell";
	       }
            }
            else {
                if (defined $symbolic{$cell}) {
                    print "tsmc28 $xrefcell symbolic $cell";
                }
                else {
		    if ($cell =~ /^a28lvt_/){  print "vendor.avago.lvt vendor.avago.lvt.$cell.0 layout $cell";}
                    elsif ($cell =~ /^a28hvt_/){ print "vendor.avago.hvt vendor.avago.hvt.$cell.0 layout $cell";}
                    elsif ($cell =~ /^a28_/){ print "vendor.avago.svt vendor.avago.svt.$cell.0 layout $cell";}
                    elsif ($cell =~ /globals__wires__a28_fillnw4_a1_/){ print "globals $xrefcell layout $cell";}
                    else { 
          	       print STDERR "$xrefcell\n";
                       if( $xrefcell=~/(\.[0-9A-Zof_]+\.[0-9]+)$/ ){
                             my $thisLib=$xrefcell;
                             $thisLib=~s/$1//g; 
                             print "$thisLib $xrefcell layout $cell";
                             print STDERR "$thisLib $xrefcell layout $cell";
	               } else {
                             print "$library $xrefcell layout $cell";
                             print STDERR "$library $xrefcell layout $cell";
	               }
                    }
                }
            }
        }
        if ($xref{$cell}[0] != 1) {
            print STDERR "Possible Ambiguous reference $cell => $xref{$cell}[1] ($xref{$cell}[0])\n";
            for (my $n = 2; $n < $xref{$cell}[0]; $n++) {
                print STDERR "$xref{$cell}[$n]";
            }
        }
    }
    elsif ($default_prefix eq "") {
        print "; not found $cell";
    }
    elsif ($cell eq $topcell) {
        if ($writebind) {
            if ($cell =~ /\./) {
                print "$library $cell layout $cell";
            }
            elsif ($cell =~/__/) {
                my $cadence_cell=$cell;
                $cadence_cell=~s/_L_/-L/g;
                $cadence_cell=~s/_R_/-R/g;
                $cadence_cell=~s/__/\./g;
                print "$library $cadence_cell layout $cell";
            }
            else {
                print "$library $default_prefix.$cell.$subtype layout $cell";
            }
        }
    }
    else {
        # default subtype of 0 for non top cells
#        print "$default_prefix.$cell.0 layout $cell"
        my $xrefcell=$cell;
        if (substr($xrefcell,0,length($topcell)+1) eq "${topcell}_" and $default_prefix ne "") {
            $xrefcell =~ s/^${topcell}_/$default_prefix./;
        }
        if ($xrefcell =~ /_(VIA[a-zA-Z0-9_]+)$/i) {
            $xrefcell = $default_prefix."_".$1;
        }
        elsif ($xrefcell =~ /via/i) {
            $xrefcell =~ s/$library/$default_prefix/;
        }
        elsif (! ($xrefcell =~ /\./)) {
            $xrefcell = "$default_prefix.$xrefcell.$subtype";
        }
        print "$library $xrefcell layout $cell"
            if $writebind or $verbose;
    }
}
select STDOUT;
close P;
if ($fatal) {
    print STDERR "Fatal Error in Bind File";
    exit 1;
}
sleep 1;
`sort -u -o '$bindfile' '$bindfile'` if $writebind;
close GDIN;
close GDOUT;
waitpid ($gdpid,0);
if (! $multipletopcells && (! defined ($topcell) or $topcell eq "")) {
    $topcell = $file;
    $topcell =~ s/\.gds.*//;
}
print "TOPCELL $topcell" if $verbose;
$template =~ s/GDSFILE/$file/;
$template =~ s/TOPCELL/$topcell/g;
$template =~ s/LIBNAME/$library/;
$template =~ s/BINDFILE/$bindfile/;
$template =~ s/TECHLIB/tsmc28/g;
$template =~ s/LAYERMAP/$pdkroot\/share\/Fulcrum\/stream\/strmin.layermap/;
my $logfile=$file;
$logfile =~ s/\.gds\*/.log/g;
$logfile = "strmIn.log" if $logfile eq $file;
$template =~ s/LOGFILE/$logfile/g;
if ($skip_cds_lib_generated) {
    $template =~ s/REFLIB//;
}
else {
    my $reflib = $bindfile;
    $reflib =~ s/\.[^\.]*$//;
    $reflib .= ".reflib";
    open (P, ">$reflib");
    print P "globals\n";
    print P "vendor.avago.svt\n";
    print P "vendor.avago.lvt\n";
    print P "vendor.avago.hvt\n";
    print P "lib.synchronous.conversion.probe\n";
    print P "lib.synchronous.conversion.util\n";
    print P "lib.synchronous.conversion.sizing\n";
    print P "lib.synchronous.conversion.s2a\n";
    print P "lib.synchronous.conversion.a2s\n";
    close P;    
    $template =~ s/REFLIB/$reflib/;
}
my $rsffile = $bindfile;
$rsffile =~ s/\.[^\.]*$//;
$rsffile .= ".template";
open (P, ">$rsffile");
print P $template;
close P;
if ( ! defined ($ENV{FULCRUM_PDK_ROOT})) {
    $ENV{FULCRUM_PDK_ROOT} = $pdkroot;
}
print "Streaming in...";
print "$ENV{IC_SCRIPT} strmin -templateFile $rsffile"
    if ! $dopipo or $verbose;
system "$ENV{IC_SCRIPT} strmin -templateFile $rsffile 2>\&1 | grep Translation"
    if $dopipo;
# create thumbnails
open (P, ">thumbnails.il");
print P "hiGenerateThumbnails(?lib \"$library\")";
close P;
print "Making thumbnails...";
print "$ENV{IC_SCRIPT} layout -nograph -replay thumbnails.il -log thumbnails.log"
    if !$dopipo or $verbose;
system "$ENV{IC_SCRIPT} layout -nograph -replay thumbnails.il -log thumbnails.log"
    if $dopipo;
