#!/usr/intel/bin/perl -l
# AAG
# $Id$
# $DateTime$

use strict;
use Getopt::Long;

my $tmpwd;
my $verbose=0;
my $debug=0;

#hack for 65nm
my %rho = (
    "rm1" => 0.160,
    "rm2" => 0.140,
    "rm3" => 0.140,
    "rm4" => 0.140,
    "rm5" => 0.140,
    "rm6" => 0.140,
    "rm7" => 0.140,
    "rm8" => 0.022,
    "rppolywo" => 690,
);

END {
print STDERR "Cleaning up" if $verbose;
`/bin/rm -rf $tmpwd` if defined $tmpwd and ! $debug
}

my $library;
my $sub_type=1;
my $refinement_parent="";
my $gnd_node="VSS";
my $vdd_node="VDD";
my @implied=();
my %implied=();
my $max_heap_size="2G";
my $topcell;
my $cdl;
my $gds;
my $pdkroot;
my $dfIIdir;
my $force=0;
my $merge=0;
my $casthome="";
my $castpath="";
my $specpath="";
my $client;
my $change;

my %devicetranslation = (
   "N" => "nmos",
   "NCH" => "nmos",
   "NCHPD_SR" => "nmospd_sr",
   "NCHPG_SR" => "nmospg_sr",
   "NCH_HVT" => "nmos_hvt",
   "NCH_SRAM" => "nmos_sram",
   "P" => "pmos",
   "PCH" => "pmos",
   "PCHPU_SR" => "pmospu_sr",
   "PCH_HVT" => "pmos_hvt",
   "PCH_SRAM" => "pmos_sram",
   "NDIO" => "ndio",
   "PDIO" => "pdio",
   "n" => "nmos",
   "nch" => "nmos",
   "nchpd_sr" => "nmospd_sr",
   "nchpg_sr" => "nmospg_sr",
   "nch_hvt" => "nmos_hvt",
   "nch_sram" => "nmos_sram",
   "p" => "pmos",
   "pch" => "pmos",
   "pchpu_sr" => "pmospu_sr",
   "pch_hvt" => "pmos_hvt",
   "pch_sram" => "pmos_sram",
   "ndio" => "ndio",
   "pdio" => "pdio",
);

sub usage {
    my ($msg) = @_;
    print STDERR "$msg" if defined $msg;
    print <<EU;
Usage importSR [options] <cdlfile> <gdsfile>
    options
    cell               : the name of the top cell
    dfII-dir           : dfII cadence root
    force              : force overwrite of data
    fulcrum-pdk-root   : the pdk root
    gnd-node           : the cdl GND node
    implied            : extra implied node(s)
    lib                : the cast library name
    max-heap-size      : normal java max-heap-size
    merge              : merge with existing directory
    refinement-parent  : the cast refinement parent
    sub-type           : the desired subtype for this cell
    vdd-node           : the cdl Vdd node
    verbose            : show progress
    debug              : debug mode, does not remove tmp directories
EU
exit 1;
}

GetOptions (
    "lib=s" => \$library,
    "sub-type=s" => \$sub_type,
    "refinement-parent=s" => \$refinement_parent,
    "gnd-node=s" => \$gnd_node,
    "vdd-node=s" => \$vdd_node,
    "implied=s" => sub {
        push @implied, split(/,/,$_[1]);
        foreach my $i (@implied) {
            $implied{$i}=1;
        }
    },
    "max-heap-size=s" => \$max_heap_size,
    "cell=s" => \$topcell,
    "fulcrum-pdk-root=s" => \$pdkroot,
    "dfII-dir=s" => \$dfIIdir,
    "verbose" => \$verbose,
    "debug" => \$debug,
    "force" => \$force,
    "merge" => \$merge,
    "castpath=s" => \$castpath,
    "specpath=s" => \$specpath,
    "change=i" => \$change,
    "client=s" => \$client,
) or usage;

$verbose = 1 if $debug;
$cdl = shift if defined $ARGV[0];
$gds = shift if defined $ARGV[0];
usage("Specify CDL file") if ! -s $cdl;
usage("Specify GDS file") if ! -s $gds;

if (! defined $topcell) {
    $topcell = `awk '/SUBCKT/ { name=\$2 } END { print name }' '$cdl'`;
    chomp $topcell;
    if ($topcell =~ /^REG/) {
        $library = "vendor.artisan.memory.regfile" if ! defined $library;
        $refinement_parent="REGFILE" if ! defined $refinement_parent;
    }
    elsif ($topcell =~ /^SRAM/) {
        $library = "vendor.artisan.memory.sram" if ! defined $library;
        $refinement_parent="SRAM" if ! defined $refinement_parent;
    }
}

my $path=$library;
$path =~ s/\./\//g;
my $cpath = $path;
$cpath =~ s/\/[^\/]+$//;
my $lname=$path;
$lname =~ s/.*\///;
$casthome=$topcell if $casthome eq "";
if ($castpath eq "") {
    $castpath="$casthome/cast/$cpath";
}
else {
    $castpath="$castpath/$cpath";
}
if ($specpath eq "") {
    $specpath="$casthome/spec/$path";
}
else {
    $specpath="$specpath/$path";
}
if ((-d "$castpath" or -d "$specpath") and ! $force and ! $merge) {
    usage "$castpath or $specpath exists, use --force to overwrite.\n".
        "or --merge to Merge";
}
if ( ! $merge ) {
    `/bin/rm -rf "$castpath" "$specpath"`;
}

# create the cast and spec files
`mkdir -p "$castpath"` if ! -d "$castpath";
`mkdir -p "$specpath"` if ! -d "$specpath";

print STDERR "Creating Cast and Spec files" if $verbose;
open (P, "<$cdl") or die "Cannot open $cdl";
my %subckts=();
my %lines=();
my $ln="";
my $subckt="";
my @lines=();
my %types=();
my %gnd=();
my %vdd=();
my %order=();
my %place=();
while (<P>) {
    chomp;
    next if (/^\*/);
    s/ \$.*//;
    if (/^\+/) {
        s/^\+/ /;
        $ln .= $_;
        next;
    }
    my $lx = $_;
    $_ = $ln;
    $ln = $lx;
    s/  */ /g;
    if (/^.global/i) {
        s/ \$.*//;
        my @f=split;
        shift @f;
        foreach my $f (@f) {
            $implied{$f}=1 if $f ne $gnd_node and $f ne $vdd_node;
        }
    }
    if (/^\.subckt/i) {
        my @f=split;
        shift @f;
        $subckt = shift @f;
        $gnd{$subckt}=$vdd{$subckt}=-1;
        foreach my $n (0..$#f) {
            $gnd{$subckt} = $n if $f[$n] eq "gnd_node";
            $vdd{$subckt} = $n if $f[$n] eq "vdd_node";
            $order{"$subckt:$n"}=$f[$n];
        }
        $subckts{$subckt}=[@f];
        @lines=();
        push @lines, $_;
        next;
    }
    if (/^\.ends/i) {
        push @lines, $_;
        $lines{$subckt}=[@lines];
        $subckt = "";
        next;
    }
    if ($subckt ne "") {
        # look for subckt resistors
        if(/^x/i and /w=/i and /l=/i) {
            s/^x/R/i;
            my @f=split;
            my $w=0;
            my $l=0;
            my $m=1;
            my $type=$f[3];
            $type =~ tr/A-Z/a-z/;
            my $r=0;
            foreach my $f (@f) {
                if ($f =~ /w=(.*)/i) {
                    $w = $1;
                    $w =~ tr/A-Z/a-z/;
                    $w =~ s/u$/e-6/;
                }
                if ($f =~ /l=(.*)/i) {
                    $l = $1;
                    $l =~ tr/A-Z/a-z/;
                    $l =~ s/u$/e-6/;
                }
                if ($f =~ /m=(.*)/i) {
                    $m = $1;
                    $m =~ tr/A-Z/a-z/;
                    $m =~ s/u$/e-6/;
                }
            }
            if ($l > 0 and $w > 0 and $m > 0) {
                $r = sprintf "%g", $l/$w/$m*$rho{$type};
                $_="$f[0] $f[1] $f[2] $r \$MODEL=$f[3] $f[4] $f[5] $f[6]";
            }
        }
        if (/^x/i) {
            if (defined($types{$subckt}) and $types{$subckt} ne "subtypes") {
                print STDERR "Warning: Mixed type in subckt $subckt";
            }
            $types{$subckt}="subtypes";
        }
        elsif (/^[a-z]/i) {
            if (defined($types{$subckt}) and $types{$subckt} ne "netlist") {
                print STDERR "Warning: Mixed type in subckt $subckt";
            }
            $types{$subckt}="netlist";
        }
        push @lines, $_;
    }
}
if ($ln =~ /^\.ends/i) {
    push @lines, $ln;
    $lines{$subckt}=[@lines];
}
close P;
foreach my $subckt (sort keys %subckts) {
    my @f=@{$subckts{$subckt}};
    my $bus="";
    my @p=();
    my $min=0;
    my $max=0;
    foreach my $f (sort @f) {
        next if $f eq $gnd_node or $f eq $vdd_node or defined $implied{$f};
        if ($f =~ /(.*)\[(\d+)]$/) {
            my $b=$1;
            my $c=$2;
            if ($b ne $bus and $bus ne "") {
                push @p, "$bus\[$min..$max\]";
                $min = $max = $c;
            }
            $bus = $b;
            $max = $c if $c > $max;
            $min = $c if $c < $min;
        }
        else {
            if ($bus ne "") {
                push @p, "$bus\[$min..$max\]";
                $min = $max = 0;
            }
            push @p, $f;
            $bus="";
        }
    }
    if ($bus ne "") {
        push @p, "$bus\[$min..$max\]";
        $min = $max = 0;
        $bus = "";
    }
    $subckts{$subckt}=[@p];
    my $seq=0;
    foreach my $p (@p) {
        if ($p =~ /(.*)\[(\d+)..(\d+)\]/) {
            my $name=$1;
            my $min=$2;
            my $max=$3;
            for (my $n = $min; $n <= $max; $n++) {
                $place{"$subckt:$name\[$n\]"} = $seq++;
            }
        }
        else {
            $place{"$subckt:$p"} = $seq++;
        }
    }
}

my $copyright= <<EC;
/* Copyright 2006 Fulcrum Microsystems.  All rights reserved.
 * \$Id:\$
 * \$DateTime:\$
 * \$Author:\$
 */
/* Automatically generated.  Modify at your own risk. */
EC

my %existing = ();
if ($merge and -e "$castpath/$lname.cast") {
    open (P, "<$castpath/$lname.cast");
    my @lines=();
    my $type="";
    while (<P>) {
        chomp;
        if (/^define/ and ! /<: NULL/) {
            my @f=split;
            $type=$f[1];
            $type =~ s/\(.*//;
            $type =~ s/"//g;
            @lines=($_);
            next;
        }
        if (@lines and /^\x7d/) {
            push @lines, $_;
            $existing{$type}=[@lines];
            @lines=();
        }
        push @lines, $_ if (@lines);
    }
    close P;
}

sub dop4 {
    my ($file) = @_;
    if (! -f "$file") {
        `touch "$file"`;
    }
    if (defined ($change) and defined ($client) and -f "$file") {
        my $cmd;
        if ( -w "$file") {
            $cmd = "p4 -c $client add -c $change '$file'";
        }
        else {
            $cmd = "p4 -c $client edit -c $change '$file'";
        }
        print STDERR "$cmd";
        `$cmd`;
        `chmod +w "$file"`;
        $cmd = "p4 -c $client add -c $change '$file'";
        print STDERR "$cmd";
        `$cmd`;
    }
}

dop4("$castpath/$lname.cast");
open (P, ">$castpath/$lname.cast") or die "$!";
select P;
print "$copyright";
print "module $library;";
my $impliedlist=" ";
foreach my $node (sort keys %implied) {
    $impliedlist .= "-$node, ";
}
$impliedlist =~ s/ $//;
$impliedlist = "" if $impliedlist eq " ";
print <<ER;

/*** Refinement parent for all contained cells ***/
define $refinement_parent()()(node$impliedlist -$vdd_node, -$gnd_node) <+ synchronous <: NULL {
    directives {
    /**
     * SIGNOFF(lines):
     * Don't try to jlvs/jauto the auto-generated prs.
     **/ 
    prs_netlist_mismatch_ok = true;
    fragment = true;
    fixed_size = true;
    routed = true;
    }
    env empty subcells {}
}
ER

foreach my $type (sort keys %existing) {
    if (! defined $subckts{$type}) {
        print "/* $type */";
        print join("\n", @{$existing{$type}});
        print "";
    }
}
foreach my $subckt (sort keys %subckts ) {
    if ($subckt eq $topcell ) {
        print "/* $subckt */";
        printf "define \"$subckt\"()( node";
        my $n = 0;
        foreach my $p (@{$subckts{$subckt}}) {
            printf "," if $n;
            printf " -+$p";
            $n++;
        }
        print " ) <: $refinement_parent {\n}";
    }
}
select STDOUT;
close P;


foreach my $subckt (keys %subckts) {
    mkdir "$specpath/$subckt";
    dop4("$specpath/$subckt/$sub_type.cast");
    open P, ">$specpath/$subckt/$sub_type.cast" or die "$!";
    select P;
    print $copyright;
    print "module $library.$subckt;";
    printf "define \"$sub_type\"()( node";
    my $n = 0;
    foreach my $p (@{$subckts{$subckt}}) {
        printf "," if $n;
        printf " -+$p";
        $n++;
    }
    if ($subckt eq $topcell) {
        print " ) <: $library.$subckt {";
    }
    else {
        print " ) <: $library.$refinement_parent {";
    }
    if ($types{$subckt} eq "netlist") {
        print "  netlist {";
        foreach my $line (@{$lines{$subckt}}) {
             next if ($line =~ /^\.subckt/i) or ($line =~ /^\.ends/i);
             if ($line =~ /^m/i) {
                foreach my $nm (keys %devicetranslation) {
                    $line =~ s/ $nm / $devicetranslation{$nm} /;
                }
                my @f=split(/ /, $line);
                for ($n = 0; $n < 5; $n++) {
                   $f[$n] =~ s/([A-Za-z0-9])-([A-Za-z0-9])/${1}_M_${2}/g;
                }
                for ($n = 5; $n <= $#f; $n++ ) {
                    $f[$n] =~ tr/A-Z/a-z/;
                }
                $line = join (" ", @f);
             }
             elsif ($line =~ /^d/i) {
                foreach my $nm (keys %devicetranslation) {
                    $line =~ s/ $nm / $devicetranslation{$nm} /;
                }
                $line =~ s/area=//i;
                my @f=split(/ /,$line);
                for ($n = 0; $n < 3; $n++) {
                   $f[$n] =~ s/([A-Za-z0-9])-([A-Za-z0-9])/${1}_M_${2}/g;
                }
                $f[4] =~ tr/A-Z/a-z/;
                $f[5] =~ tr/A-Z/a-z/;
                $line="$f[0] $f[1] $f[2] $f[3] area=$f[4] $f[5]";
             }
             elsif ($line =~ /^x/i) {
                $line =~ s/([A-Za-z0-9])-([A-Za-z0-9])/${1}_M_${2}/g;
                my @f=split(/ /, $line);
                $f[$#f] = "$library.$f[$#f].$sub_type";
                $line = "@f";
            }
            print "    $line";
        }
    }
    else {
        print "  subcells {";
        # collect nodes
        my %nodes=();
        my @nodes=();
        my %pins=();
        foreach my $p (@{$subckts{$subckt}}) {
            $pins{$p}=1;
        }
        foreach my $line (@{$lines{$subckt}}) {
            next if ($line =~ /^\.subckt/i) or ($line =~ /^\.ends/i);
            my @f=split(/ /, $line);
            for ( my $n = 1; $n < $#f; $n++) {
                $nodes{$f[$n]}=1;
            }
        }
        {
            my $bus="";
            my $min=0;
            my $max=0;
            foreach my $f (sort keys %nodes) {
                if ($f =~ /(.*)\[(\d+)]$/) {
                    my $b=$1;
                    my $c=$2;
                    if ($b ne $bus and $bus ne "") {
                        push @nodes, "$bus\[$min..$max\]";
                        $min = $max = $c;
                    }
                    $bus = $b;
                    $max = $c if $c > $max;
                    $min = $c if $c < $min;
                }
                else {
                    if ($bus ne "") {
                        push @nodes, "$bus\[$min..$max\]";
                        $min = $max = 0;
                    }
                    push @nodes, $f;
                    $bus="";
                }
            }
            if ($bus ne "") {
                push @nodes, "$bus\[$min..$max\]";
            }
        }
        foreach my $node (@nodes) {
            print "    node $node;"
                if (! defined $pins{$node}) and
                    ($node ne "$vdd_node") and ($node ne "$gnd_node") and ! defined ($implied{$node});;
        }
        foreach my $line (@{$lines{$subckt}}) {
            next if ($line =~ /^\.subckt/i) or ($line =~ /^\.ends/i);
            if ($line =~ /^x/i) {
               my @f=split(/ /, $line);
               my $ref=$f[$#f];
               $f[$#f] = "$library.$f[$#f].$sub_type";
               $line = "$f[$#f] $f[0](";
               shift @f;
               pop @f;
               my @p=();
               foreach my $n (0..$#f) {
                  my $p = $order{"$ref:$n"};
                  if (! defined ($p)) {
                      print STDERR "Warning: Undefined order $ref:$n $f[$n] $p"
                  }
                  else {
                      if ($p ne "$gnd_node" and $p ne "$vdd_node" and ! $implied{$p}) {
                          my $seq = $place{"$ref:$p"};
                          print STDERR "Warning: Undefined place $ref:$p"
                            if ! defined $seq;
                          $p[$seq]=$f[$n];
                      }
                   }
               }
               $line .= join (", ", @p);
               $line .= ");";
            }
            print "    $line";
        }
    }
    print "  }";
    print "}\n";
    select STDOUT;
    close P;
}

# create the dfII files

open (P, ">$topcell.bind");
print P "$library.$topcell.$sub_type layout $topcell";
close P;

my $user=`whoami`;
chomp $user;
mkdir "/scratch/$user/";
$tmpwd = `mktemp -d /scratch/$user/isr.XXXXXX`;
chomp $tmpwd;
`mkcdswd --target-dir="$tmpwd" --dfII-dir="$dfIIdir" --fulcrum-pdk-root="$pdkroot"`;

my $pwd=`pwd`;
chomp $pwd;
open (C, ">>$tmpwd/cds.lib");
my $cdslib=$library;
$cdslib =~ s/\./#2e/g;
print C "UNDEFINE $cdslib";
print C "DEFINE $cdslib $pwd/$library";
close C;
`/bin/rm -rf "$pwd/$library"`;
`mkdir -p "$pwd/$library"`;


my $rsf ="
streamInKeys = list(nil
	'runDir			\"$tmpwd\"
	'inFile			\"$gds\"
	'primaryCell		\"$topcell\"
	'libName		\"$library\"
	'techfileName		\"$pdkroot/share/Fulcrum/stream/strmin.tech\"
	'scale			0.001000
	'units			\"micron\"
	'errFile		\"$pwd/${topcell}_pipo.log\"
	'refLib			nil
	'hierDepth		32
	'maxVertices		1024
	'checkPolygon		nil
	'snapToGrid		nil
	'arrayToSimMosaic	t
	'caseSensitivity	\"preserve\"
	'textCaseSensitivity	\"preserve\"
	'zeroPathToLine		\"lines\"
	'convertNode		\"ignore\"
	'keepPcell	nil
	'replaceBusBitChar	nil
	'skipUndefinedLPP	nil
	'ignoreBox		nil
	'mergeUndefPurposToDrawing		nil
	'reportPrecision	nil
	'keepStreamCells		nil
	'attachTechfileOfLib		\"\"
	'runQuiet		nil
	'noWriteExistCell		nil
	'NOUnmappingLayerWarning		nil
	'comprehensiveLog		t
	'ignorePcellEvalFail		nil
	'appendDB		nil
	'genListHier		nil
	'skipDbLocking		nil
	'skipPcDbGen		nil
	'cellMapTable		\"$pwd/$topcell.bind\"
	'layerTable		\"\"
	'textFontTable		\"\"
	'restorePin		0
	'propMapTable		\"\"
	'propSeparator		\",\"
	'userSkillFile		\"\"
	'rodDir			\"\"
	'refLibOrder			\"\"
)
";

print STDERR "Streaming in $gds" if $verbose;
open (P, ">$topcell.rsf");
print P $rsf;
close P;
chdir $tmpwd;
`pipo strmin "$pwd/$topcell.rsf"`;
open (T, "<$pdkroot/share/Fulcrum/dfII/cds.lib") or warn "Cannot open cds.lib";
$_=<T>;
my ($d,$techlib,$x)=split;
close T;
if ($techlib =~ /tsmc/) {
    print STDERR "Attaching $techlib to local $library" if $verbose;
    open (T, ">attach.il");
    print T "techBindTechFile(ddGetObj(\"$library\") \"$techlib\")";
    close T;
    unlink "$pwd/$library/techfile.cds";
    `layout -nograph -replay attach.il`;
}
chdir $pwd;

# check in the files
my $cmd="mkdir -p '$dfIIdir/$path'";
`$cmd` if ! -d "$dfIIdir/$path";
$cmd = "p4 -c $client edit -c $change '$dfIIdir/$path/...'";
print STDERR "$cmd";
`$cmd`;
$cmd = "/bin/cp -rp '$pwd/$library/'* '$dfIIdir/$path'";
print STDERR "$cmd";
`$cmd`;
$cmd = "find '$dfIIdir/$path/' -type f | xargs p4 -c $client add -c $change -f";
print STDERR "$cmd";
`$cmd`;
$cmd = "p4 -c $client revert -a -c $change '$dfIIdir/$path/...'";
print STDERR "$cmd";
`$cmd`;
