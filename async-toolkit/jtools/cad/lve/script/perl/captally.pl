#!/usr/intel/bin/perl -w
# AAG
# $Id$
# $DateTime$

use Getopt::Long;
use strict;
use IPC::Open2;

# This does a hierarchical search of a spice deck. It desires a
# deck with just subckts. The name of the cell to be analyzed
# is taken to be the top level (not the first) cell in the
# spice deck unless the top cell is specified on the command
# line.

$|=1;
select STDERR;
$|=1;
select STDOUT;

my $verbose=0;
my $total=0;  # flag to print total only
my $totalC=0; # total capacitance for above
my $cdl;      # name of cdl/spice file to analyze
my $library;  # name of a library of subckts file
my $lvedir;   # a path to lve dir where lib cells may be found

my $topcell;  # name of top cell if not 'the topcell' of spice deck
my $pdk="";   # needed to calculate gate cap
my $wire=0;   # flag to look at wire cap only
my $gate=0;   # flag to look at gate cap only
my %nodes;    # list of nodes to be found
my $nodes=0;  # flag to say to look at the above list
my $gds2=0;   # flag to use gds to cast name translation
my $cadence=0;   # flag to use gds to cast name translation
my $localnodes=0;  # flag to print internal nodes as well as pins
my $dodarea=0;     # capture source/drain area per node

my %options = (
    "cdl=s" => \$cdl,
    "cell=s" => \$topcell,
    "fulcrum-pdk-root=s" => \$pdk,
    "gate" => sub {$gate=1; $wire=0;},
    "gds2" => \$gds2,
    "cadence" => \$cadence,
    "library=s" => \$library,
    "lve-dir=s" => \$lvedir,
    "localnodes" => \$localnodes,
    "nodes=s" => sub { $nodes=1; my @n=split(/,/,$_[1]); foreach my $n (@n) {$nodes{$n}=1;}},
    "total" => \$total,
    "verbose" => \$verbose,
    "wire" => sub {$wire=1; $gate=0;},
    "area" => \$dodarea,
    "sdarea" => \$dodarea,
);

GetOptions ( %options ) or usage_exit();
my $npid=0; # for the rename process, so it can be killed
my $ipid=0; # for the rename process, so it can be killed

if ($gds2) {
    $npid = open2(\*RDFH, \*WTFH, "rename", "--type=node", "--from=gds2", "--to=cast");
    $ipid = open2(\*RDFI, \*WTFI, "rename", "--type=instance", "--from=gds2", "--to=cast");
    die "Cannot open renamer\n" if $npid <= 0 or $ipid <= 0;
}
elsif ($cadence) {
    $npid = open2(\*RDFH, \*WTFH, "rename", "--type=node", "--from=cadence", "--to=cast");
    $ipid = open2(\*RDFI, \*WTFI, "rename", "--type=instance", "--from=cadence", "--to=cast");
    die "Cannot open renamer\n" if $npid <= 0 or $ipid <= 0;
}

my %subckts;    # list of the subckt definitions
my %instances;  # list of instances and cap info

# recursively looks thru the instances to print the node caps
# prints local nodes, if requested, and returns total cap

sub dolocalnodes {
    my ($subckt,$instance)=@_;
    my @localnodes=@{$instances{$instance}->{ilocalnodes}};
    my @localcaps=@{$instances{$instance}->{ilocalcaps}};
    my @localdarea=@{$instances{$instance}->{ilocaldarea}};
    my $cap=0;
    foreach my $n (0..$#localcaps) {
        my $nd = "";
        $nd = "$instance." if $instance ne "";
        $nd .= "$localnodes[$n]";
        if (! ($instance =~ /!/ or $localnodes[$n] =~ /!/) and ( ! $nodes or $nodes{$nd})) {
            my $cs=0;
            my $ca=0;
            eval "\$cs = $localcaps[$n]";
            eval "\$ca = $localdarea[$n]" if defined $localdarea[$n];
            $ca = 0 if ! defined $ca;
            if ($dodarea) {
                printf "X $nd %.3g %.3g\n", $cs, $ca if ! $total;
            }
            else {
                printf "$nd %.3g\n", $cs if ! $total;
            }
            $cap += $cs;
        }
    }
    foreach my $subckt (@{$subckts{$subckt}->{subckts}}) {
        my @f=split(/ /,$subckt);
        my $name;
        my $ins=$f[0];
        $ins =~ s/^X//i;
        $ins = insttranslate($ins);
        $ins = "$instance.$ins" if ($instance ne "");
        for (my $n = $#f; $n >= 0; $n--) {
            if (! ($f[$n] =~ /=/)) {
                $name = $f[$n];
                last;
            }
        }
        $cap += dolocalnodes($name,$ins);
    }
    $cap;
}

# gds renamer called here

sub nodetranslate {
    my ($node)=@_;
    if ($npid) {
        print WTFH $node."\n";
        $node=<RDFH>;
        chomp $node;
        $node =~ s:X([^/]+)/:$1/:g;
    }
    $node =~ s:/:.:g;
    $node;
}

sub insttranslate {
    my ($inst)=@_;
    if ($ipid) {
        print WTFI $inst."\n";
        $inst=<RDFI>;
        chomp $inst;
        $inst =~ s:X([^/]+)/:$1/:g;
    }
    $inst =~ s:/:.:g;
    $inst;
}

my $process_config=$pdk;
$process_config .= "/share/Fulcrum/jauto/process.config";
if ( ! -s $process_config) {
    $process_config = $pdk;
    $process_config .= "/share/Fulcrum/jauto/process.config";
}
my $transistor_config=$pdk;
$transistor_config .= "/share/Fulcrum/jauto/transistors.config";
if ( ! -s $transistor_config) {
    $transistor_config = $pdk;
    $transistor_config .= "/share/Fulcrum/jauto/transistors.config";
}
$transistor_config="" if ! -s $transistor_config;
usage_exit("Bad or No pdk specified") if ( ! -f "$process_config");
$cdl = shift @ARGV if ( ! defined ($cdl) ); # --cdl not required, assumed when not present
usage_exit("CDL File not defined or not present") if (! defined ($cdl) or ( ! -s "$cdl" ));
usage_exit("Too many Args") if $#ARGV >= 0;

# find the capacitance factors from PDK
my $unitnmosgatecapacitance;
my $unitpmosgatecapacitance;
my $unitnmosdiffusioncapacitance;
my $unitpmosdiffusioncapacitance;
# these are from spice and not in pdk
my $unitnmosdiffusionareacapacitance=1.133e-3;
my $unitnmosdiffusionperimetercapacitance=1.125e-10;
my $unitpmosdiffusionareacapacitance=9.584e-4;
my $unitpmosdiffusionperimetercapacitance=1.020e-10;

sub read_config {
    my ($file) = @_;
    local(*P);
    open (P, "<$file") or die "Cannot open $file\n";
    while (<P>) {
        chomp;
        s/ //g;
        my ($param,$val)=split(/=/,$_);
        if (defined ($param) and ($param =~ /unit(N|P)mos(Gate|Diffusion|DiffusionArea|DiffusionPerimeter)Capacitance\[1\]/i)) {
            $param =~ tr/A-Z/a-z/;
            $param =~ s/\[\d+\]//;
            eval "\$$param = $val";
            print "$param=$val\n" if $verbose;
        }
        if (defined ($param) and $param eq "config") {
            read_config($val);
        }
    }
    close P;
}

read_config ($process_config);
#open (P, "<$process_config") or die "Cannot open $process_config";
#while (<P>) {
#    chomp;
#    s/ //g;
#    my ($param,$val)=split(/=/,$_);
#    if (defined ($param) and ($param =~ /unit[NP]mosGateCapacitance/i)) {
#        $param =~ tr/A-Z/a-z/;
#        eval "\$$param = $val";
#        last if defined ($unitnmosgatecapacitance) and defined ($unitpmosgatecapacitance);
#    }
#}
#close P;
#if ( $transistor_config ne "" and ! (defined ($unitnmosgatecapacitance) and
#        defined ($unitpmosgatecapacitance))) {
#    open (P, "<$transistor_config");
#    while (<P>) {
#        chomp;
#        s/ //g;
#        my ($param,$val)=split(/=/,$_);
#        if (defined ($param) and ($param =~ /unit[NP]mosGateCapacitance/i)) {
#            $param =~ s/\[\d+\]//;
#            $param =~ tr/A-Z/a-z/;
#            eval "\$$param = $val";
#            last if defined ($unitnmosgatecapacitance) and defined ($unitpmosgatecapacitance);
#        }
#    }
    close P;
#}
die "unitNmosGateCapacitance not defined in pdk" if (! defined ($unitnmosgatecapacitance) );
die "unitPmosGateCapacitance not defined in pdk" if (! defined ($unitpmosgatecapacitance) );
die "unitNmosDiffusionCapacitance not defined in pdk" if (! defined ($unitnmosdiffusioncapacitance) );
die "unitPmosDiffusionCapacitance not defined in pdk" if (! defined ($unitpmosdiffusioncapacitance) );
    
sub usage_exit {
    my $msg = $_[0];
    print STDERR "Usage: captally --fulcrum-pdk-root <pdk-path> --cdl <spice-file>\n";
    print STDERR "       --cdl=<filename>     - Name of cdl file to parse.\n";
    print STDERR "       --cell=<cellname>    - Name of the top cell\n";
    print STDERR "       --gate               - Include only gate cap.\n\n";
    print STDERR "       --gds2               - Use GDS->CAST name translation.\n\n";
    print STDERR "       --cadence            - Use CADENCE->CAST name translation.\n\n";
    print STDERR "       --library=<filename> - Name of library where subckts are.\n";
    print STDERR "       --localnodes         - Also print internal local nodes.\n";
    print STDERR "       --node=<list>        - Include only comma separated node list.\n\n";
    print STDERR "       --total              - Just print the total in pF.\n";
    print STDERR "       --verbose            - Show progress and other messages.\n\n";
    print STDERR "       --wire               - Include only wire cap.\n\n";
    print STDERR "       --sdarea             - listing of s/d area.\n\n";
    print STDERR "$msg\n" if defined ($msg);
    exit 1;
}

my %called; # keeps track for finding topcell

my $subckt="";
my %pinnr;
my @cap;
my @darea;
my @caps;
my @pins;
my @localnodes=();
my @localcaps=();
my @localdarea=();
my %localnr=();

# the whole spice deck goes into memory (in effect)
sub read_file {
    my ($file)=@_;
    local(*FILE);
    open (FILE, "<$file") or warn "Cannot open $file";
    print STDERR "Reading $file\n" if $verbose;
    my $ln=<FILE>;
    while (<FILE>) {
        chomp;
        # do line unwrapping
        while (/^\+/) {
            $ln .= " ".substr($_,1);
            $_=<FILE>;
            chomp;
        }
        my $lx = $_;
        $_ = $ln;
        $ln = $lx;
        s: / : :g;  # cdl's tend to have these extra '/' in the file
        my @f=split;
        if (! defined ($f[0])) {
            next;
        }
        if ($f[0] =~ /^\.subckt$/i ) {
            # start subckt definition
            $subckt=$f[1];
            shift @f;
            shift @f;
            undef @pins;
            undef @cap;
            undef @darea;
            undef @localnodes;
            undef @localcaps;
            undef @localdarea;
            my %params=();
            undef %pinnr;
            undef %localnr;
            if (defined ($subckts{$subckt}) and $verbose ) {
                warn "Duplicate subckt definition $subckt in $file";
            }
            foreach my $n (0..$#f) {
                if ($f[$n] =~ /=/) {
                    my ($param,$val)=split(/=/,$f[$n]);
                    $val =~ s/'//g;
                    if ($val =~ /u$/i) {
                        $val =~ s/u$//i;
                        $val = "$val*1e-6";
                    }
                    elsif ($val =~ /n$/i) {
                        $val =~ s/n$//i;
                        $val = "$val*1e-9";
                    }
                    $params{$param}=$val;
                }
                else {
                    $f[$n] = nodetranslate($f[$n]);
                    $pinnr{$f[$n]}=$n;
                    push @cap,"0";
                    push @darea,"0";
                    push @pins,$f[$n];
                }
            }
            $subckts{$subckt}->{pins}=[@pins];
            %{$subckts{$subckt}->{params}}=%params;
            undef %params;
            next;
        }
        if ($f[0] =~ /^\.ends/i) {
            # end subckt definition
            if ($subckt ne "") {
                $subckts{$subckt}->{caps}=[@cap];
                $subckts{$subckt}->{darea}=[@darea];
                $subckts{$subckt}->{localnodes}=[@localnodes];
                $subckts{$subckt}->{localcaps}=[@localcaps];
                $subckts{$subckt}->{localdarea}=[@localdarea];
                $subckts{$subckt}->{localnr}={%localnr};
                print STDERR "$subckt $#cap\n" if $verbose;
            }
            undef @cap;
            undef @darea;
            undef @localnodes;
            undef @localcaps;
            undef @localdarea;
            $subckt="";
        }
        if ($f[0] =~ /^x/i) {
            # a subckt call
            my $sn=$#f;
            my %params;
            for (my $n = 1; $n <= $#f; $n++) {
                if ($f[$n] =~ /=/) {
                    if ($sn eq $#f) {
                        $sn = $n-1;
                    }
                    my ($param,$val)=split(/=/,$f[$n]);
                    $val =~ s/'//g;
                    if ($val =~ /u$/i) {
                        $val =~ s/u$//i;
                        $val = "$val*1e-6";
                    }
                    elsif ($val =~ /n$/i) {
                        $val =~ s/n$//i;
                        $val = "$val*1e-9";
                    }
                    $params{$param}=$val;
                }
            }
            push @{$subckts{$subckt}->{subckts}},$_;
            $called{$f[$sn]}=1;;
        }
        if ($f[0] =~ /^c/i and ! $gate) {
            # a real capacitor
            my $node1=$f[1];
            $node1 =~ s/:.*//;
            $node1=nodetranslate($node1);
            my $node2=$f[2];
            $node2 =~ s/:.*//;
            $node2=nodetranslate($node2);
            if (defined ($pinnr{$node1})) {
                $cap[$pinnr{$node1}] .= "+$f[3]";
                if (defined ($pinnr{$node2}) and $node1 ne $node2 and $node2 ne "0" ) {
                    $cap[$pinnr{$node2}] .= "+$f[3]";
                }
            }
            elsif (defined ($pinnr{$node2})) {
                $cap[$pinnr{$node2}] .= "+$f[3]";
            }
            if (! defined ($pinnr{$node1})) {
                if (! defined ($localnr{$node1}) ) {
                    $localnr{$node1} = $#localnodes+1;
                    $localnodes[$localnr{$node1}]=$node1;
                    $localcaps[$localnr{$node1}]="$f[3]";
                }
                else {
                    $localcaps[$localnr{$node1}] .= "+$f[3]";
                }
            }
            if (! defined ($pinnr{$node2}) and $node1 ne $node2 and $node2 ne "0") {
                if (! defined ($localnr{$node2}) ) {
                    $localnr{$node2} = $#localnodes+1;
                    $localnodes[$localnr{$node2}]=$node2;
                    $localcaps[$localnr{$node2}]="$f[3]";
                }
                else {
                    $localcaps[$localnr{$node2}] .= "+$f[3]";
                }
            }
        }
        if ($f[0] =~ /^m/i and ! $wire) {
            # a mos transistor, we only count gate cap for now
            my $w=0;
            my $l=0;
            my $m = 1;
            my $ad=0;
            my $as=0;
            my $ps=0;
            my $pd=0;
            for (my $n = 6; $n <= $#f; $n++) {
                my ($p,$v)=split(/=/,$f[$n]);
                $v =~ s/'//g;
                if ($v =~ /u$/i) {
                    $v =~ s/u$//i;
                    $v = "($v)*1e-6";
                }
                elsif ($v =~ /n$/i) {
                    $v =~ s/n$//i;
                    $v = "($v)*1e-9";
                }
                elsif ($v =~ /p$/i) {
                    $v =~ s/p$//i;
                    $v = "($v)*1e-12";
                }
                if ($p =~ /^w$/i) {
                    $w = "($v)";
                }
                if ($p =~ /^l$/i) {
                    $l = "($v)";
                }
                if ($p =~ /^m/i) {
                    $m = $v;
                }
                if ($p =~ /^ad/) {
                    $ad = $v;
                }
                if ($p =~ /^as/) {
                    $as = $v;
                }
                if ($p =~ /^pd/) {
                    $pd = $v;
                }
                if ($p =~ /^ps/) {
                    $ps = $v;
                }
            }
            my $gm;
            my $dm;
            my $pm;
            my $am;
            if ($f[5] =~ /^n/i) {
                $gm = $unitnmosgatecapacitance;
                $dm = $unitnmosdiffusioncapacitance;
                $am = $unitnmosdiffusionareacapacitance;
                $pm = $unitnmosdiffusionperimetercapacitance;
            }
            else {
                $gm = $unitpmosgatecapacitance;
                $dm = $unitpmosdiffusioncapacitance;
                $am = $unitpmosdiffusionareacapacitance;
                $pm = $unitpmosdiffusionperimetercapacitance;
            }
            my $gateC = "($w*$l*$m*$gm)";
            my $srcC = "($w*$m*$dm)";
            my $drnC = "($w*$m*$dm)";
            my $srcA = "($w*$m*0.4e-6)";
            my $drnA = "($w*$m*0.4e-6)";
            if ($as ne "0" or $ad ne "0") {
                $srcC = "($as*$m*$am+($ps-$w)*$m*$pm)";
                $drnC = "($ad*$m*$am+($pd-$w)*$m*$pm)";
                print "$drnC\n" if $verbose;
                $srcA = "$as";
                $drnA = "$ad";
            }
            $as=$ad=$ps=$pd=$srcC=$drnC=$srcA=$drnA=0 if ! $dodarea;
            my $srcx;
            my $drnx;
            eval "\$srcx=$srcC";
            eval "\$drnx=$drnC";
            my $gatenode = $f[2];
            $gatenode =~ s/:.*//;
            $gatenode = nodetranslate($gatenode);
            if (defined ($pinnr{$gatenode})) {
                if (defined ($cap[$pinnr{$gatenode}]) and $cap[$pinnr{$gatenode}] ne "") {
                    $cap[$pinnr{$gatenode}] .= "+$gateC";
                }
                else {
                    $cap[$pinnr{$gatenode}] = "$gateC";
                    $darea[$pinnr{$gatenode}] = "0";
                }
            }
            if (! defined ($pinnr{$gatenode}) ) {
                if (! defined ($localnr{$gatenode})) {
                    $localnr{$gatenode} = $#localnodes+1;
                    $localnodes[$localnr{$gatenode}]=$gatenode;
                    $localcaps[$localnr{$gatenode}]="$gateC";
                    $localdarea[$localnr{$gatenode}]="0";
                }
                else {
                    $localcaps[$localnr{$gatenode}] .= "+$gateC";
                }
            }
            my $srcnode = $f[3];
            $srcnode =~ s/:.*//;
            $srcnode = nodetranslate($srcnode);
            if (defined ($pinnr{$srcnode})) {
                if (defined ($cap[$pinnr{$srcnode}]) and $cap[$pinnr{$srcnode}] ne "") {
                    $cap[$pinnr{$srcnode}] .= "+$srcC";
                    $darea[$pinnr{$srcnode}] .= "+($as*$m)";
                }
                else {
                    $cap[$pinnr{$srcnode}] = "$srcC";
                    $darea[$pinnr{$srcnode}] .= "($as*$m)";
                }
            }
            if (! defined ($pinnr{$srcnode}) ) {
                if (! defined ($localnr{$srcnode})) {
                    $localnr{$srcnode} = $#localnodes+1;
                    $localnodes[$localnr{$srcnode}]=$srcnode;
                    $localcaps[$localnr{$srcnode}]="$srcC";
                    $localdarea[$localnr{$srcnode}]="$srcA";
                }
                else {
                    $localcaps[$localnr{$srcnode}] .= "+$srcC";
                    $localdarea[$localnr{$srcnode}] .= "+$srcA";
                }
            }
            my $drnnode = $f[1];
            $drnnode =~ s/:.*//;
            $drnnode = nodetranslate($drnnode);
            if (defined ($pinnr{$drnnode})) {
                if (defined ($cap[$pinnr{$drnnode}]) and $cap[$pinnr{$drnnode}] ne "") {
                    $cap[$pinnr{$drnnode}] .= "+$drnC";
                    $darea[$pinnr{$drnnode}] .= "+($ad*$m)";
                }
                else {
                    $cap[$pinnr{$drnnode}] = "$drnC";
                    $darea[$pinnr{$drnnode}] .= "($ad*$m)";
                }
            }
            if (! defined ($pinnr{$drnnode}) ) {
                if (! defined ($localnr{$drnnode})) {
                    $localnr{$drnnode} = $#localnodes+1;
                    $localnodes[$localnr{$drnnode}]=$drnnode;
                    $localcaps[$localnr{$drnnode}]="$drnC";
                    $localdarea[$localnr{$drnnode}]="$drnA";
                }
                else {
                    $localcaps[$localnr{$drnnode}] .= "+$drnC";
                    $localdarea[$localnr{$drnnode}] .= "+$drnA";
                }
            }
        }
        if ($f[0] =~ /^\.inc/i) {
            $f[1] =~ s/"//g;
            $f[1] =~ s/'//g;
            if ( ( ! -s "$f[1]" ) and ($f[1] =~ /\.spice_gds2$/)) {
                my $incfile=$f[1];
                $incfile =~ s/spice_gds2/spice_include/;
                my $topfile = $f[1];
                $topfile =~ s/spice_gds2/spice_topcell/;
                read_file($incfile);
                read_file($topfile);
            }
            else {
                read_file($f[1]);
            }
        }
    }
    close FILE;
    # assume there might not be a subckt definition
    # DOES NOT HANDLE hierarchical subckt definitions, but I have not seen these
    # in any automatically generated file
    if ($subckt ne "") {
        $subckts{$subckt}->{caps}=[@cap];
        $subckts{$subckt}->{darea}=[@darea];
        $subckts{$subckt}->{localnodes}=[@localnodes];
        $subckts{$subckt}->{localcaps}=[@localcaps];
        $subckts{$subckt}->{localdarea}=[@localdarea];
        $subckts{$subckt}->{localnr} = {%localnr};
        print STDERR "$subckt $#cap\n" if $verbose;
    }
}

# recursive look at the full hierarchy

sub tally_file {
    my ($topcell,$instance,%argparams)=@_;
    $instance = "" if ! defined $instance;
    my @ilocalnodes=();
    my @ilocalcaps=();
    my @ilocaldarea=();
    my %ilocalnr=();
    my @c=();
    my @a=();
    my %pins;
    %argparams=() if ! %argparams;
    if (defined ($subckts{$topcell}->{params})) {
        my %default_params=%{$subckts{$topcell}->{params}};
        my @p = (keys %default_params);
        foreach my $s (@p) {
            $argparams{$s}=$default_params{$s} if (! defined $argparams{$s});
        }
    }
    my @s = (keys %argparams);
    print STDERR "params $topcell" if $verbose;
    foreach my $s (sort {$b cmp $a} @s) {
        print STDERR " $s=$argparams{$s}" if $verbose;
    }
    print STDERR "\n" if $verbose;
    my @darea=();
    if (defined ($subckts{$topcell}->{darea})) {
        @darea=@{$subckts{$topcell}->{darea}};
    }
    my @caps=();
    if (defined ($subckts{$topcell}->{caps})) {
        @caps = @{$subckts{$topcell}->{caps}};
    }
    my @pins=();
    if (defined ($subckts{$topcell}->{pins})) {
        @pins = @{$subckts{$topcell}->{pins}};
    }
    if ($#caps != $#pins) {
        print STDERR "Error: count mismatch $topcell $#caps $#pins\n";
        print STDERR "@pins\n";
        exit 1;
    }
    foreach my $n (0..$#pins) {
        $pins{$pins[$n]}=$n;
    }
    # evaluate subckt connections
    foreach my $subckt (@{$subckts{$topcell}->{subckts}}) {
        my @f = split(/ /,$subckt);
        my $ins = shift @f;
        $ins =~ s/^X//;
        $ins = insttranslate($ins);
        if ($instance ne "") {
            $ins = "$instance.$ins";
        }
        my $name;
        my %params;
        my %subpins;
        my @subpins;
        for (my $n = $#f; $n >= 0; $n--) {
            if ($f[$n] =~ /=/) {
                my ($param,$val)=split(/=/,$f[$n]);
                $val =~ s/'//g;
                if ($val =~ /u$/i) {
                    $val =~ s/u$//i;
                    $val = "$val*1e-6";
                }
                elsif ($val =~ /n$/i) {
                    $val =~ s/n$//i;
                    $val = "$val*1e-9";
                }
                $params{$param}=$val;
            }
            else {
                if (! defined ($name)) {
                    $name = $f[$n];
                }
                else {
                    $f[$n] = nodetranslate($f[$n]);
                    unshift @subpins,$f[$n];
                    $subpins{$f[$n]}=$n;
                }
            }
        }
        my ($subcap,$subdarea) = tally_file ($name, $ins, %params);
        my @subcap=split(/:/, $subcap);
        my @subdarea=split(/:/, $subdarea);
        if ($#subcap != $#subpins) {
            print STDERR "Error: wrong cap count on $name $#subcap, $#subpins\n";
            print STDERR "$subckt\n";
            print STDERR "@subcap\n";
            print STDERR "@subpins\n";
        }
        foreach my $n (0..$#subpins) {
            if (defined ($subcap[$n])) {
                my $subnode=$subpins[$n];
                $subnode =~ s/:.*//;
                if (defined ($pins{$subnode})) {
                    $caps[$pins{$subnode}] .= "+($subcap[$n])";
                    $darea[$pins{$subnode}] .= "+($subdarea[$n])";
                }
                else {
                    if (defined ($ilocalnr{$subnode})) {
                        $ilocalcaps[$ilocalnr{$subnode}] .= "+($subcap[$n])";
                        $ilocaldarea[$ilocalnr{$subnode}] .= "+($subdarea[$n])";
                    }
                    else {
                        $ilocalnr{$subnode} = $#ilocalnodes+1;
                        $ilocalnodes[$ilocalnr{$subnode}] = $subnode;
                        $ilocalcaps[$ilocalnr{$subnode}] = "($subcap[$n])";
                        $ilocaldarea[$ilocalnr{$subnode}] .= "($subdarea[$n])";
                    }
                }
            }
        }
    }
    # evaluate internal pin caps
    my $n=0;
    foreach my $pin (@pins) {
        my $cs = "$caps[$n]";
        my $ca = "$darea[$n]";
        # reverse sort to avoid incorrect substitutions
        foreach my $param (sort {$b cmp $a} keys %argparams) {
            $cs =~ s/$param/$argparams{$param}/g;
            $ca =~ s/$param/$argparams{$param}/g;
        }
        my $cap;
        eval "\$cap = $cs";
        my $darea;
        eval "\$darea = $ca";
        print STDERR "CAP $cs\n" if $verbose;
        print STDERR "AREA $ca\n" if $verbose;
        print STDERR "$pin $cap\n" if $verbose;
        push @c, $cap;
        push @a, $darea;
        $n++;
    }
    my @localnodes=@{$subckts{$topcell}->{localnodes}};
    my @localcaps=@{$subckts{$topcell}->{localcaps}};
    my @localdarea=@{$subckts{$topcell}->{localdarea}};
    my %localnr=%{$subckts{$topcell}->{localnr}};
    foreach my $n (0..$#localnodes) {
        my $node=$localnodes[$n];
        my $cs=$localcaps[$n];
        my $ca=$localdarea[$n];
        $ca = 0 if ! defined $ca;
        foreach my $param (sort {$b cmp $a} keys %argparams) {
            $cs =~ s/$param/$argparams{$param}/g;
            $ca =~ s/$param/$argparams{$param}/g;
        }
        my $cap;
        eval "\$cap = $cs";
        my $darea=0;
        eval "\$darea = $ca" if defined $ca;
        if (defined ($ilocalnr{$node})) {
            $ilocalcaps[$ilocalnr{$node}] .= "+($cap)";
            $ilocaldarea[$ilocalnr{$node}] .= "+($darea)";
        }
        else {
            $ilocalnr{$node} = $#ilocalnodes+1;
            $ilocalnodes[$ilocalnr{$node}] = $node;
            $ilocalcaps[$ilocalnr{$node}] = "($cap)";
            $ilocaldarea[$ilocalnr{$node}] = "($darea)";
        }
    }
    $instances{$instance}->{ilocalnodes}=[@ilocalnodes];
    $instances{$instance}->{ilocalcaps}=[@ilocalcaps];
    $instances{$instance}->{ilocaldarea}=[@ilocaldarea];
    $instances{$instance}->{ilocalnr}={%ilocalnr};
#    $subckts{$topcell}->{localnodes}=[@localnodes];
#    $subckts{$topcell}->{localcaps}=[@localcaps];
#    $subckts{$topcell}->{localdarea}=[@localdarea];
#    $subckts{$topcell}->{localnr} = {%localnr};
    if ($total) {
        $totalC=0;
        foreach my $n (0..$#c) {
            $totalC += $c[$n] if ( ! $nodes or $nodes{$pins[$n]});
        }
    }
    join(":",@c),join(":",@a);
}

read_file ($cdl);
# check to see if any subckts are missing
my $ok=1;
my $need;
my %need;
do {
    $need=0;
    %need=();
    $ok=1;
    foreach my $subckt (sort keys %called) {
        if (! defined ($subckts{$subckt})) {
            $need++;
            $need{$subckt}=1;
        }
    }
    if ($need > 0) {
        if (defined ($library) and $library ne "" and -f "$library") {
            read_file ($library);
            $library="";
        }
        elsif (defined($lvedir) and -d $lvedir) {
            foreach my $subckt (keys %need) {
                my $celldir=$subckt;
                $celldir=~s/\./\//g;
                my $spicefile = `find "$lvedir/$celldir" -mindepth 3 -maxdepth 3 -name cell.spice | head -1`;
                chomp $spicefile;
                if ( -f $spicefile) {
                    read_file ($spicefile);
                    $ok = 0 if ( ! defined ($subckts{$subckt}));
                }
                else {
                    $ok = 0;
                    print STDERR "Error: Cannot locate a spice file for $subckt\n";
                    print STDERR "       in $lvedir/$celldir\n";
                }
            }
        }
        else {
            $ok = 0;
        }
    }
} while ($need > 0 and $ok);
if ($need) {
    print STDERR "Error: Required subcircuits not defined.\n";
    print STDERR join("\n", (sort keys %need));
    die "\n";
}
print STDERR "Calculating\n" if $verbose;
my $topcnt=1;
if (!defined ($topcell)) {
    $topcnt=0;
    foreach my $subckt (sort keys %subckts) {
        if (!defined ($called{$subckt})) {
            $topcell = $subckt;
            $topcnt++;
        }
    }
}
if ($topcnt == 1) {
    print STDERR "Top cell is $topcell\n" if $verbose;
}
elsif ($topcnt) {
    die "too many top cells found, $topcnt";
}
else {
    die "no top cell defined or found";
}
my ($caps,$darea) = tally_file ($topcell);
@caps=split(/:/, $caps);
@darea=split(/:/, $darea);
@pins=@{$subckts{$topcell}->{pins}};
@localnodes=@{$instances{""}->{ilocalnodes}};
@localcaps=@{$instances{""}->{ilocalcaps}};
@localdarea=@{$instances{""}->{ilocaldarea}};
%localnr=%{$instances{""}->{ilocalnr}};
if ($#pins != $#caps) {
    print STDERR "Error: Wrong local count $#localnodes, $#localcaps\n";
}
if ($total) {
    $totalC += dolocalnodes($topcell,"") if $localnodes;
    print "Total Cap $totalC\n";
}
else {
    foreach my $n (0..$#caps) {
        if ($dodarea) {
            printf "$pins[$n] %.3g %.3g\n", $caps[$n], $darea[$n] if (! $nodes or $nodes{$pins[$n]}) and $caps[$n] > 0;
        }
        else {
            printf "$pins[$n] %.3g\n", $caps[$n] if (! $nodes or $nodes{$pins[$n]}) and $caps[$n] > 0;
        }
    }
    dolocalnodes ($topcell,"") if $localnodes;
}

END {
    if ($npid) {
        close WTFH;
        close RDFH;
        waitpid $npid, 0;
    }
    if ($ipid) {
        close WTFI;
        close RDFI;
        waitpid $ipid, 0;
    }
}
