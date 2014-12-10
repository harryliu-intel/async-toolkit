#!/usr/intel/bin/perl -l
# AAG
# $Id$
# $DateTime$

my $description = <<ED;
# This is to import from Qualcore. It may be pretty specific
# to that task. It only generates a spec file to be renamed
# 400.cast and place in the correct place but the output is
# to stdout
ED

# TODO TYPES with parens
use strict;
use Getopt::Long;
use IPC::Open2;
#use Cast::JavaServer;

my $profile=0;
use Time::HiRes qw( gettimeofday tv_interval);

my $cpid = open2(\*RD, \*WR, "rename --from=cadence --to=cast --type=cell");

sub cad2cast {
    return $_[0] if $_[0] =~ /\(/;
    print WR $_[0];
    my $new=<RD>;
    chomp $new;
    $new;
}

my %isportdefault=("VddA"=>1,"_RESET"=>1);

my @castlines=();

my %openpins=();
my $library;
my $cdl;
my $portfile;
my $errs=0;
my $castdir;
my $specdir;
my $bindfile="";
# could not find this one automatically
my @addimport=("lib.synchronous.schannel");
my %imports=();
my %ports=();
my %implied=();
my %bind=();
my %bindnet=();
my %dummynetassign=();
my $subtype = 0;
my $flat=0;
my %portdirection=();
my %castfound=();

sub fixup {
    my @lines=@_;
    my %length=();
    for ( my $n = 0; $n <= $#lines; $n++) {
        $_=$lines[$n];
        %length=() if (/module/);
        if (/^ *node\[(\d+)\] *([^ ;]+)/) {
            # node declarations, both ports and local
            my $a = $2;
            my $l = $1;
            $a =~ s/^[-+]//;
            $length{$a}=$l;
#            print STDERR "FND1 $a\[$l\]";
        }
        elsif (/^ *node *([^\[ ;]+)\[(\d+)\.\.(\d+)\]/) {
            # more node declarations, both ports and local
            my $a = $1;
            my $n = $2;
            my $x = $3;
            $a =~ s/^[-+]//;
            $x++;
            $length{$a}=$x;
#            print STDERR "FND2 $a\[$x\]";
        }
        if (m:^ *([^ ]+) *// node ([^\[]+)\[(\d+)\]:) {
            # ports of subcells
            my @list=();
            my $sig = $1;
            my $port=$2;
            my $pdx = $3;
            my $size = $pdx+1;
            my $comma=$sig;
            $comma=~s/[^,]//g;
            $sig =~ s/,$//;
            my $sdx = -1;
            if ($sig =~ /([^\[]+)\[(\d+)\]/) {
                $sdx = $2;
                $sig = $1;
            }
            my $i=0;
            my $ok = 1;
            my $sg = $sig;
#            print STDERR "FND3 $sig\[$sdx\] $port\[$pdx\]";
            while ($lines[$n+$i] =~ m:^ *([^ ]+) *// node $port\[(\d+)\]:) {
                $sig = $1;
                $pdx = $2;
                $sdx = -1;
                $comma=$sig;
                $comma=~s/[^,]//g;
                $sig =~ s/,$//;
                if ($sig =~ /([^\[]+)\[(\d+)\]$/) {
                    $sdx = $2;
                    $sig = $1;
                }
#                print STDERR "FND4 $sig\[$sdx\] $port\[$pdx\]";
                $ok = 0 if ($pdx != $sdx or $sig ne $sg);
                $size = $pdx + 1 if ($ok and $size <= $pdx);
                push @list, "$sig $sdx $port $pdx";
                $i++;
            }
            my $match=$ok;
            if ($size != $length{$sig}) {
                $ok = 0;
            }
            if ($size == 1) {
                if (defined ($length{$sig})) {
                    if ($length{$sig} != $size) {
                        print "      $sig\[$sdx\]$comma // node[$size] $port";
                    }
                    else {
                        print "      $sig\[0\]$comma // node[$size] $port";
                    }
                }
                else {
                    print "      $sig$comma // node[$size] $port";
                }
            }
            elsif ($ok) {
                print "      $sig$comma // node[$size] $port";
            }
            elsif ($match) {
                my $max=$size-1;
                print "      $sig\[0..$max\]$comma // node[$size] $port";
            }
            else {
                my @port=();
                foreach my $l (@list) {
                    my ($sig,$sdx,$port,$pdx)=split(/ /,$l);
                    $port[$pdx]=$sig;
                    $port[$pdx]="$sig\[$sdx\]" if $sdx >= 0;
                }
                print "      \x7b $port[0],";
                my $x=0;
                for ($x = 1; $x < $#port; $x++) {
                    print "        $port[$x],";
                }
                print "        $port[$x]\x7d$comma // node[$size] $port";
            }
            $n += $#list;
        }
        elsif (/^( *)node ([^ \[]+)\[(\d+)\];/) {
            my $sp=$1;
            my $node=$2;
            my $ndx =$3;
            my $min = $ndx;
            my $max = $ndx;
            my $i = 0;
            while (/^ *node $node\[(\d+)\];/) {
                $ndx = $1;
                $min = $ndx if $ndx < $min;
                $max = $ndx if $ndx > $max;
                $i++;
                $_=$lines[$n+$i];
            }
            if ($min == 0) {
                $max++;
                $length{$node}=$max;
                print "${sp}node[$max] $node;";
            }
            else {
                $length{$node}=$max+1;
                print "${sp}node $node\[$min..$max\];";
            }
            $n += $i-1;
        }
        else {
            print;
        }
    }
}

sub fixup1 {
    my @lines=@_;
    foreach my $n (0..$#lines) {
        $a = $lines[$n];
        $lines[$n] =~ s/ *$//;
        $lines[$n] =~ s/_L_/\[/g;
        $lines[$n] =~ s/_R_/\]/g;
        $lines[$n] =~ s/\[\d+\]\[/\[/;
        $lines[$n] =~ s/\[(\d+)\]_/${1}_/;
        print STDERR "CHANGE $a : $lines[$n]" if $a ne $lines[$n];
    }
    my %length=();
    for ( my $n = 0; $n <= $#lines; $n++) {
        $_=$lines[$n];
        %length=() if (/module/);
        if (/\]_/) {
            s/[\[\]]//g;
        }
        if (/^ *node\[(\d+)\] *([^ ;]+)/) {
            my $a = $2;
            my $l = $1;
            $a =~ s/^[-+]//;
            $length{$a}=$l;
        }
        elsif (/^\ *node *([^\[ ;]+)\[(\d+)\.\.(\d+)\]/) {
            my $a = $1;
            my $n = $2;
            my $x = $3;
            $a =~ s/^[-+]//;
            $length{$a}=$x+1;
        }
        if (m:^ *([^ ]+) *// node ([^\[]+)\[(\d+)\]:) {
            my @list=();
            my $sig = $1;
            my $port=$2;
            my $pdx = $3;
            my $size = $pdx+1;
            my $comma=$sig;
            $comma=~s/[^,]//g;
            $sig =~ s/,$//;
            my $sdx = -1;
            if ($sig =~ /([^\[]+)\[(\d+)\]/) {
                $sdx = $2;
                $sig = $1;
            }
            my $i=0;
            my $ok = 1;
            my $sg = $sig;
            while ($lines[$n+$i] =~ m:^ *([^ ]+) *// node $port\[(\d+)\]:) {
                $sig = $1;
                $pdx = $2;
                $sdx = -1;
                $comma=$sig;
                $comma=~s/[^,]//g;
                $sig =~ s/,$//;
                if ($sig =~ /([^\[]+)\[(\d+)\]$/) {
                    $sdx = $2;
                    $sig = $1;
                }
                $ok = 0 if ($pdx != $sdx or $sig ne $sg);
                push @list, "$sig $sdx $port $pdx";
                $i++;
            }
            my $match=$ok;
            if ($size != $length{$sig}) {
                $ok = 0;
            }
            if ($size == 1) {
                if (defined ($length{$sig})) {
                    if ($length{$sig} != $size) {
                        print "      $sig\[$sdx\]$comma // node[$size] $port";
                    }
                    else {
                        print "      $sig\[0\]$comma // node[$size] $port";
                    }
                }
                else {
                    print "      $sig$comma // node[$size] $port";
                }
            }
            elsif ($ok) {
                print "      $sig$comma // node[$size] $port";
            }
            elsif ($match) {
                my $max=$size-1;
                print "      $sig\[0..$max\]$comma // node[$size] $port";
            }
            else {
                my @port=();
                foreach my $l (@list) {
                    my ($sig,$sdx,$port,$pdx)=split(/ /,$l);
                    $port[$pdx]=$sig;
                    $port[$pdx]="$sig\[$sdx\]" if $sdx >= 0;
                }
                print "      \x7b $port[0],";
                my $x=0;
                for ($x = 1; $x < $#port; $x++) {
                    print "        $port[$x],";
                }
                print "        $port[$x]\x7d$comma // node[$size] $port";
            }
            $n += $#list;
        }
        elsif (/^( *)node ([^ \[]+)\[(\d+)\];/) {
            my $sp=$1;
            my $node=$2;
            my $ndx =$3;
            my $min = $ndx;
            my $max = $ndx;
            my $i = 0;
            while (/^ *node $node\[(\d+)\];/) {
                $ndx = $1;
                $min = $ndx if $ndx < $min;
                $max = $ndx if $ndx > $max;
                $i++;
                $_=$lines[$n+$i];
            }
            if ($min == 0) {
                $max++;
                $length{$node}=$max;
                print "${sp}node[$max] $node;";
            }
            else {
                $length{$node}=$max+1;
                print "${sp}node $node\[$min..$max\];";
            }
            $n += $i-1;
        }
        else {
            print;
        }
    }
}

sub readportdirection {
    my ($file) = @_;
    local *X;
    open (X, "<$file");
    my $module;
    while (<X>) {
        chomp;
        my ($x,$y)=split;
        if (/^MODULE/) {
            $module=$y;
            next;
        }
        if (defined ($y)) {
            if ($x eq "input") {
                $x = "-";
            }
            elsif ($x eq "output") {
                $x = "+";
            }
            else {
                $x = "+-";
            }
            $portdirection{"$module $y"}=$x;
        }
    }
}
# sort port list
sub sortary {
    my $an  = $a;
    $an =~ s/\[.*//;
    my $bn = $b;
    $bn =~ s/\[.*//;
    my $av = $a;
    $av =~ s/.*\[//;
    $av =~ s/\]//;
    my $bv = $b;
    $bv =~ s/.*\[//;
    $bv =~ s/\]//;
    if ($an eq $bn) {
        return $bv - $av;
    }
    $an cmp $bn;
}

sub readbindfile {
    my ($file)=@_;
    local (*P,$_);
    if (open (P, "<$file") ) {
        while (<P>) {
            chomp;
            my ($cell, $instance, $pin, $signal) = split;
            $bind{"$cell $instance $pin"}=$signal if defined $signal;
        }
    }
}

sub createnodesx {
    my $name = $_[0];
    my $file = "$name.nodesx";
    return if ( -s "$file" );
    local(*P,*Q,$_);
    open (P, "cast_query --task=external_nodes=im:xr --max-heap-size=1G '--cast-path=$castdir:$specdir' '--cell=$name' |");
    open (Q, ">$file");
    $_=<P>;
    while (<P>) {
        chomp;
        print Q;
    }
    close P;
    close Q;
}

sub usage {
    print STDERR "$_[0]" if defined $_[0];
    my $cmd = $0;
    $cmd =~ s:.*/::;
    print STDERR <<EU;
Usage: $cmd [options] fqcellname > $subtype.cast
    --cast-dir=<dir>
    --spec-dir=<dir>
    [--add-import=<name>]
    [--cdl=<name>]
    [--portfile=<name>]
    [--library=<name>]
    [--bindfile=<name>]
$description
EU
exit 1;
}

GetOptions(
    "add-import=s" => sub { push @addimport, $_[1];},
    "cast-dir=s" => \$castdir,
    "cdl-file=s" => \$cdl,
    "port-file=s" => \$portfile,
    "library=s" => \$library,
    "bindfile=s" => \$bindfile,
    "spec-dir=s" => \$specdir,
    "subtype=i" => \$subtype,
) or usage;

usage "cast-dir needed" if ! defined $castdir or  ! -d "$castdir";
usage "spec-dir needed" if ! defined $specdir or ! -d "$specdir";
usage if ! defined $ARGV[0];
my $arg=$ARGV[0];
my $name=$arg;
$name =~ s/\.(\d+)$//;
my $argsubtype=$1;
my $argcell = $name;
my $deflib=( defined ($library) and $library ne "");
$library = $name if ! $deflib;
$name =~ s/.*\.//;
$library =~ s/\.$name$// if ! $deflib;
$cdl="$name.sp" if ! $cdl;
if ($bindfile ne "" and -r $bindfile ) {
    readbindfile($bindfile);
}
usage "Cannot read $cdl or need to define cdl-file"
    if ( ! -f "$cdl" or ! -r "$cdl" );

foreach my $add (@addimport) {
    $imports{"$add.*"}=1;
}

my %defchans=();

sub getdefchanrefs {
    local (*P,$_);
    open (P, "find '$castdir' -name \\*.cast | xargs grep '^ *defchan ' |");
    while (<P>) {
        chomp;
        s:$castdir/::;
        s/ *defchan/defchan/;
        my ($path,$chan)=split;
        $path =~ s/:.*//;
        $path =~ s:/:.:g;
        $path =~ s/\.cast//;
        $chan =~ s/\(.*//;
        $defchans{$chan}=$path;
    }
    close P;
}

getdefchanrefs;

sub dumcmp {
    my $xa=$a;
    my $xb=$b;
    $xa=$a;  # to prevent warnings
    $xb=$b;
    $xa=~ s/dummy//;
    $xb =~ s/dummy//;
    $xa-$xb;
}
my %definesubckts=();
my %external=();
my %channeldefs=();

sub getexternal {
    my ($castcell)=@_;
    return if defined $external{$castcell};
    local(*Q, *Y, $_);
    if ( -r "$castcell.query.external") {
        open (Q, "<$castcell.query.external");
        open (Y, ">/dev/null");
    }
    else {
        open (Q, "cast_query --cast-path='$castdir:$specdir' --task=external_nodes=all --cell='$castcell' --no-header 2>/dev/null |");
        open (Y, ">$castcell.query.external");
    }
    while (<Q>) {
        chomp;
        print Y;
        push @{$external{$castcell}}, $_;
    }
    close Q;
    close Y;
}

my %ecanonical=();
my %rcanonical=();

sub getports {
    my ($cell)=@_;
    my $castcell=$cell;
    $castcell =~ s/\.[^\.]+$//;
    return if (defined ($ports{$cell}));
    print STDERR "getports($cell) = $castcell";
    my @ports;
    my @implied;
    my @external=();
    %ecanonical=();
    %rcanonical=();
    # first priority, find port list from cast
    if ($cell =~ /\./) { # cannot be cast file without .'s
        if ( -r "$cell.query.ports") {
            open (P, "<$cell.query.ports");
            open (X, ">/dev/null");
        }
        else {
            open (P, "cast_query --cast-path='$castdir:$specdir' --task=external_nodes=im:xr,ports --cell='$castcell' --no-header 2>/dev/null |");
            open (X, ">$cell.query.ports");
        }
        my $portlist;
        my @castlines=();
        while (<P>) {
            chomp;
            print X;
            print STDERR "getports: query $cell" if $. == 1;
            if (/^[01] /) {
                my ($implied, $type, $name)=split;
                my $dir = "";
                if ($name =~ s/^([-+])//) {
                    $dir = $1;
                }
                if ($name =~ s/\[(\d+)\.\.(\d+)\]$//) {
                    my $lo=$1;
                    my $hi=$2;
                    if ($lo eq "0") {
                        $hi++;
                        $type = "$type\[$hi\]";
                    }
                    else {
                        $type = "$type\[$lo..$hi\]";
                    }
                }
                if ( ! ($type =~ /^node/) and ! defined ($channeldefs{"$type"})) {
                    foreach my $ex (@external) {
                        my ($en) = split(/ /,$ex);
                        my $p=$en;
                        $p =~ s/\..*//;
                        $p =~ s/\[.*//;
                        if ($p eq $name) {
                            my $ch=$en;
                            $ch =~ s/$name//;
                            print STDERR "CHD $name $en $ch $cell $type";
                            push @{$channeldefs{"$type"}}, $ch;
                        }
                    }
                }
                push @castlines, "define \"XXXX\"() (" if ! @castlines;
                if ($implied) {
                    push @implied, [$type, $name, $dir];
                }
                else {
                    push @castlines, "    $type $dir$name";
                    push @ports, [$type, $name, $dir];
                }
                print STDERR "getports $cell $type $name";
            }
            else {  # this comes first from cast_query
                push @external, $_;
                my ($p,$a)=split;
                $ecanonical{$p}=$a;
                $rcanonical{$a}=$p;
            }
        }
        close P;
        close X;
        if (@ports) {
            $castfound{$cell}=[@castlines];
            $castfound{$castcell}=[@castlines] if $castcell ne $cell;
            $ports{$cell}=[@ports];
            $implied{$cell}=[@implied];
            return;
        }
        else {
            print STDERR "getports: cannot cast_query for $cell";
        }
    }
    if (defined ($definesubckts{$cell})) {
        my @pl = split(/ /,${@{$definesubckts{$cell}}}[0]);
        shift @pl;
        shift @pl;
        foreach my $port (sort sortary @pl) {
            push @ports, ["node",$port];
        }
        $ports{$cell}=[@ports] if @ports;
        print STDERR "getports (cdl) $cell : ", join(" ", @pl);
        return;
    }
    my $localcell=$cell;
    $localcell =~ s/\.\d+$//;
    $localcell =~ s/.*\.//;
    if (defined ($definesubckts{$localcell})) {
        my @pl = split(/ /,${@{$definesubckts{$localcell}}}[0]);
        shift @pl;
        shift @pl;
        foreach my $port (sort sortary @pl) {
            push @ports, ["node",$port];
        }
        $ports{$cell}=[@ports] if @ports;
        print STDERR "getports (cdl) $cell : ", join(" ", @pl);
        return;
    }
    print STDERR "getports: No defined SUBCKT nor cast for $cell";
}

sub readcdl {
    my ($file)=@_;
    local(*P,$_);
    my $ln="";
    my $subckt="";

    open (P, "<$file") or die "Cannot open $file";
    $ln = <P>;
    chomp $ln;
    while (<P>) {
        chomp;
        s/  *$//;
        if (/^$/ or /^\*/ or /^\$/ or /^\.include/i ) {
            next;
        }
        if (/^\+/) {
            s/^\+/ /;
            $ln .= $_;
        }
        else {
            $ln =~ s/  */ /g;
            if ($ln =~ /^\.subckt/i) {
                my @f=split(/ /,$ln);
                $subckt=cad2cast($f[1]);
                $f[1] = $subckt;
                push @{$definesubckts{$subckt}}, join(" ", @f);
            }
            elsif ($ln =~ /^\.ends/i) {
                push @{$definesubckts{$subckt}},$ln;
                $subckt="";
            }
            elsif ($subckt ne "") {
                if ($ln =~ /^x/i) {
                    my @f = split(/ /,$ln);
                    $f[1] = cad2cast($f[1]);
                    push @{$definesubckts{$subckt}}, join(" ", @f);
                }
                else {
                    push @{$definesubckts{$subckt}},$ln;
                }
            }
            $ln = $_;
        }
    }
    push @{$definesubckts{$subckt}},$ln if $ln ne "" and $subckt ne "";
}

readcdl "$cdl";
close WR;
close RD;
waitpid $cpid, 0;

if (! defined($portfile)) {
    $portfile = $cdl;
    $portfile =~ s/\.[^\.]*$//;
    $portfile = "$portfile.ports";
}
readportdirection($portfile);
$"=" ";

my %cast=();
my %spec=();
my %cxref=();
my %sxref=();
# find all cells in the spec tree
# there are likely duplicates, especially with multiple subtypes
open (P, "find '$specdir' -name \\*.cast |");
while (<P>) {
    chomp;
    s:$specdir/::;
    s/:/ /;
    s:/:.:g;
    s/\.cast$//;
    my $spec=$_;
    my $cast=$_;
    $cast =~ s/\.[^\.]+$//;
    my $name = $cast;
    $name =~ s/.*\.//;
    $sxref{$name}=$spec;
    $cxref{$name}=$cast;
}
close P;
my $portmap=$cdl;
$portmap =~ s/\.[^\.]+$/.portmap/;
if ( -r $portmap ) {
    open (P, "<$portmap");
    open (Q, ">/dev/null");
}
else {
    open (P, "generate_port_mapping --cast-path='$castdir' --cell='$argcell' --verilog-block=rtl |");
    open (Q, ">$portmap");
}
my %verilog=();
my %castname=();
while (<P>) {
    chomp;
    print Q;
    my ($verilog,$cast)=split;
    $verilog{$cast}=$verilog;
    $castname{$verilog}=$cast;
}
close P;
close Q;
# find the cast file for this cell
my $castfile="$library";
$castfile =~ s/\./\//g;
$castfile = $castdir."/".$castfile.".cast";
if ( ! -f $castfile ) {
    print STDERR "Cannot find original cast file at $castfile";
    exit 1;
}
# generate list of imports required
print STDERR "Reading $castfile";
my %needimport=();
open (P, "<$castfile");
while (<P>) {
    chomp;
    s/^ *//;
    last if (/^define/);
    if (/^import /) {
        my @f=split;
        my $import=$f[1];
        $import =~ s/;//;
        $imports{$import}=1;
    }
}
close P;
my %nodes=();

sub getnodes {
    my ($subckt)=@_;
    %nodes=();
    foreach my $line (@{$definesubckts{$subckt}}) {
    print STDERR "SUBCKT $subckt";
        if ($line =~ /^X/i and $line =~ /\$PINS/) {
            my @f=split(/ /,$line);
            shift @f;
            my $name=shift @f;
            shift @f;  # $PINS
            print STDERR "XLINE $name : $cast{$name} : $sxref{$name}";
            if ( ! defined ($cast{$name}) and defined ($sxref{$name})) {
                $cast{$name} = $sxref{$name};
            }
            if ( ! defined ($cast{$name}) and $name =~ /\./) {
                $cast{$name} = $name;
            }
            if ( ! defined ($cast{$name})) {
                print STDERR "Cannot find cast name for $name";
                $cast{$name}=$name;
            }
            getports($cast{$name});
            if (! defined ($spec{$name})) {
                if (defined ($cast{$name})) {
                    $spec{$name}=$cast{$name};
                }
                elsif (defined ($sxref{$name})) {
                    $spec{$name} = $sxref{$name};
                }
                else {
                    $spec{$name} = $name;
                }
            }
            foreach my $call (@f) {
                my ($pin,$net)=split(/=/,$call);
                if ($subckt eq "EPL" and defined ($castname{"$net"})) {
                    $net = $castname{"$net"};
                }
                else {
                    $net =~ s/\//_slash_/g;
                    $net =~ s/\./_dot_/g;
                    $net =~ s/\[/_L_/g;
                    $net =~ s/\]/_R_/g;
                    $nodes{"$net"}="node $net";
                }
            }
        }
    }
}

# prints a file containing hierarchical cast which has to be parsed after
# to split into the various modules
my $module;
my $lastimport=0;
my %hasimports=();
my $keep=0;
# get all of the ports for subckts from cast if possible
getports("$argcell.$argsubtype");
foreach my $top (keys %definesubckts) {
    if ($argcell eq "$library.$top") {
        getports("$library.$top.$argsubtype");
    }
    elsif ($top =~ /\./) {
        getports("$top");
    }
    else {
        getports("$library.$top.0");
    }
}
# print each module one at a time from the cdl file
my %isport=();
foreach my $top (sort keys %definesubckts) {
    %bindnet=();
    %isport=%isportdefault;
    getnodes($top);
    my $name="";
    my $min = 10000;
    my $max = -1;
    my $castcell="$library.$top";
    my $st = $subtype;
    if ($top =~ /\./) {
        $castcell = $top;
        $castcell =~ s/\.([^\.]+)$//;
        $st = $1;
    }
    elsif ("$library.$top" eq "$argcell") {
        $st = $argsubtype;
    }
    my $tc = "$castcell.$st";
    foreach my $x (sort keys %castfound) {
        print STDERR "CFX $x";
    }
    push @castlines, "module $castcell;";
    my $separatefromparent=0;
    if ($castcell =~ /lib.synchronous.conversion.v3.SCAN_A2S\(\d+\)/) {
        push @castlines, "";
        push @castlines, "import lib.synchronous.conversion.v3.base.*;";
        $separatefromparent=1;
    }
    push @castlines, "";
    push @castlines, "define \"$st\"() (";
    # make sure the necessary data has been gathered
    # for this cell
    getports("$tc");
    if (defined ($castfound{$tc})) {
        # print cast header from cast information, most accurate
        my @plines=@{$castfound{$tc}};
        foreach my $pn (1..$#plines-1) {
            push @castlines, "$plines[$pn];";
        }
        push @castlines, "$plines[$#plines]";
        my $parent = $tc;
        $parent =~ s/\.[^\.]+$//;
        if ($separatefromparent) {
            push @castlines, "  )(node -Vdd, -GND, -_RESET, +debug_ack) <+ cdc_nofloorplan <+ cdc_cell <: NULL \x7b";
        }
        else {
            push @castlines, "  ) <: $parent \x7b";
        }
    }
    else {
        # find ports from cdl, not accurate, but is all we have left.
        my @ports = split(/ /,${@{definesubckts{$top}}}[0]);
        shift @ports;
        shift @ports;
        foreach my $port (sort sortary @ports) {
            # ports sorted by name and by array index
            my $nm = $port;
            $nm =~ s/\[.*//;
            my $nv = $port;
            $nv =~ s/.*\[//;
            $nv =~ s/\]//;
            $isport{$nm}=1;
            if ($name ne $nm and $name ne "") {
                my $dir = "+-";
                $dir = $portdirection{"$top $name"} if defined ($portdirection{"$top $name"});
                if ($min == 0 and $max > 0) {
                    # a full array
                    $max++;
                    push @castlines, "    node[$max] $dir$name;";
                    foreach my $nn (0..$max-1) {
                        $isport{"$nm\[$nn\]"};
                    }
                }
                elsif ($min <= $max) {
                    # a partial array
                    push @castlines, "    node $dir$name\[$min..$max\];";
                    foreach my $nn ($min..$max) {
                        $isport{"$nm\[$nn\]"};
                    }
                }
                else {
                    # just a node
                    push @castlines, "    node $dir$name;";
                }
                $name = $nm;
                if ($nv =~ /^\d+$/) {
                    $max = $min = $nv;
                }
                else {
                    $max = -1 if defined $nv;
                    $min = 10000 if defined $nv;
                }
            }
            $max = $nv if (($nv =~ /^\d+$/) and ($nv > $max));
            $min = $nv if ($nv =~ /^\d+$/) and $nv < $min;
            $name = $nm;
        }
        my $dir = "+-";
        $dir = $portdirection{"$top $name"} if defined ($portdirection{"$top $name"});
        if ($min == 0 and $max > 0) {
            $max++;
            push @castlines, "    node[$max] $dir$name";
        }
        elsif ($min <= $max) {
            push @castlines, "    node $dir$name\[$min..$max\]";
        }
        else {
            push @castlines, "    node $dir$name";
        }
        push @castlines, "  ) <+ synchronous <: PRIMITIVE \x7b";
    }
    # from cast ports, determine the names which are ports
    foreach my $ip (@{$implied{$tc}}) {
        my ($t,$n,$d)=@{$ip};
        $isport{$n}=1;
        print STDERR "IMPLIED $n $tc";
    }
    foreach my $p (@{$ports{$tc}}) {
        my ($t,$port)=@{$p};
        $isport{$port}=1;
        if ($t =~ /^node\[(\d+)\]/) {
            my $s = $1;
            foreach my $i (0..$s-1) {
                $castname{"$port\[$i\]"} = "$port\[$i\]";
                $isport{"$port\[$i\]"}=1;
            }
        }
        elsif ( ! ($t =~ /^node/)) {
            if (defined ($channeldefs{$t})) {
                foreach my $ch (@{$channeldefs{$t}}) {
                    $castname{"$port.$ch"}="$port.$ch";
                    $isport{"$port.$ch"}=1;
                }
            }
        }
    }
    # find the dummy channels and nodes and assign them to the
    # correct instance pin combinations
    my $dummystart=100001;
    my $dummynet=$dummystart;
    %dummynetassign=();
    foreach my $line (@{$definesubckts{$top}}) {
        # look at all instances now
        if ($line =~ /^x/i) {
            my @f=split(/ /,$line);
            my $inst=shift @f;
            my $name=shift @f;
            shift @f; # $PINS
            my %alias=();
            my %unalias=();
            # special handling for CDCs from Tahoe
            createnodesx($name)
                if ($name =~ /_CDC/ and ! -f "$name.nodesx");
            if (open (Q, "<$name.nodesx")) {
                while (<Q>) {
                    chomp;
                    my ($ext,$al)=split;
                    $alias{$ext}=$al;
                    $unalias{$al}=$ext;
                }
                close Q;
            }
            $cast{$name}="$name" if ( ! defined ($cast{$name}));
            $inst =~ s/\[/_L_/g;
            $inst =~ s/\]/_R_/g;
            $inst =~ s/[\/\.]/_slash_/g;
            $inst =~ s/^x//i;
            my %net=();
            my %matched=();
            # go thru the list of pin=net assignments
            foreach my $call (@f) {
                my ($pin,$net)=split(/=/,$call);
                if ($top eq "EPL" and defined ($castname{"$net"})) {
                    $net = $castname{"$net"};
                }
                elsif ( ! $isport{$net}) {
                    $net =~ s/\[/_L_/g;
                    $net =~ s/\]/_R_/g;
                    $net =~ s/\//_slash_/g;
                    $net =~ s/\./_dot_/g;
                }
                if (! defined ($unalias{$pin})) {
                    $unalias{$pin}=$pin;
                    $alias{$pin}=$pin;
                }
                $net{$unalias{$pin}}=$net;
                $matched{$unalias{$pin}}=0;
            }
            my $cn=$cast{$name};
            # names without dots
            if (! ($cn =~ /\./)) {
                $cn = "$library.$cn.0";
                $cast{$name}=$cn;
            }
            my @ps=();
            if (defined $ports{$cn}) {
                @ps=@{$ports{$cn}};
            }
            elsif (defined ($ports{$name})) {
                @ps=@{$ports{$name}};
            }
            my $net;
            foreach my $ps (@ps) {
                my ($type,$pn)=@{$ps};
                if (defined($bind{"$name $inst $pn"})) {
                    $bindnet{"$inst $pn"} = $bind{"$name $inst $pn"};
                }
                elsif (defined ($bind{"$name * $pn"}) ) {
                    $bindnet{"$inst $pn"} = $bind{"$name * $pn"};
                }
                if (! defined ($net{$pn})) {
                    if (defined ($bindnet{"$inst $pn"})) {
                        $net=$bindnet{"$inst $pn"};
                        $nodes{$net} = "$type ".$net;
                    }
                    elsif ($pn eq "GND" or $pn eq "Vdd") {
                    }
                    else {
                        if ($type eq "node" and ! ($pn =~ /\[/) ) {
                            if ($pn ne "QN" and
                                ! ($pn eq "PO" and $name =~ /^pnl_/) and
                                ! ($pn eq "DIN" and $name =~ /^pnl_/)) {
                                $openpins{"dummy$dummynet"}="$pn $type $name $inst";
                                print STDERR "DN OP1 $name $inst $pn $type dummy$dummynet";
                            }
                        }
                        else {
                            $openpins{"dummy$dummynet"}="$pn $type $name $inst";
                            print STDERR "DN OP2 $name $inst $pn $type dummy$dummynet";
                        }
                        $net="dummy".$dummynet;
                        if ($pn =~ /\[(.*)/) {
                            $net .= '['.$1;
                        }
                        $nodes{"$net"}="$type $net";
                        print STDERR "DN OP3 $name $inst $pn $type $net";
                    }
                    print STDERR "DA '$top $inst $pn' = $dummynet";
                    $dummynetassign{"$inst $pn"}=$dummynet;
                    $bindnet{"$inst $pn"}="dummy$dummynet";
                    $dummynet++;
                }
            }
        }
    }
    # add nodes from bind file
    foreach my $b (keys %bind) {
        my $n = $bind{$b};
        $n =~ s/\..*//;
        $n =~ s/\[.*//;
        $nodes{$bind{$b}}="node $bind{$b}"
            if ($bind{$b} ne "GND" and $bind{$b} ne "Vdd" and ! $isport{$n});
    }
    # now the body of the cell
    push @castlines, "  subcells {";
    # print nodes
    foreach my $node (sort keys %nodes) {
        my $n = $node;
        $n =~ s/\..*//;
        $n =~ s/_L_.*// if $n =~ /_R_$/;
        $n =~ s/\[.*//;
        my $string = $nodes{$node};
        push @castlines, "    $string;"
            if ($node ne "GND" and $node ne "Vdd" and ! $isport{$n});
        print STDERR "Skipping $string because port in $top" if $isport{$n};
    }
    push @castlines, "    aclk = debug_ack;" if $separatefromparent;
    $dummynet=$dummystart;
    my %dumxrf;
    # print netlist
    foreach my $line (@{$definesubckts{$top}}) {
        if ($line =~ /^X/) {
            my @f=split(/ /,$line);
            my $inst=shift @f;
            my $name=shift @f;
            shift @f; # $PINS
            my %alias=();
            my %unalias=();
            if (open (Q, "<$name.nodesx")) {
                while (<Q>) {
                    chomp;
                    my ($ext,$al)=split;
                    $alias{$ext}=$al;
                    $unalias{$al}=$ext;
                }
                close Q;
            }
            $inst =~ s/\[/_L_/g;
            $inst =~ s/\]/_R_/g;
            $inst =~ s/[\/\.]/_slash_/g;
            $inst =~ s/^x//i;
            my $i = $inst;
            $i =~ s/_slash_X$//;
            $i =~ s/_$//;
            if (!($spec{$name} =~ /\./)) {
                $spec{$name} = "$library.$spec{$name}.0";
            }
            # instance declaration
            push @castlines, "    $spec{$name} $i(";
            my %net=();
            my %matched=();
            foreach my $call (@f) {
                my ($pin,$net)=split(/=/,$call);
                if ($top eq "EPL" and defined ($castname{"$net"})) {
                    print STDERR "NET 0 $pin $net $inst";
                    $net = $castname{"$net"};
                }
                elsif (! $isport{$net}) {
                    $net =~ s/\[/_L_/g;
                    $net =~ s/\]/_R_/g;
                    $net =~ s/\//_slash_/g;
                    $net =~ s/\./_dot_/g;
                }
                if (! defined ($unalias{$pin})) {
                    $unalias{$pin}=$pin;
                    $alias{$pin}=$pin;
                }
                $net{$unalias{$pin}}=$net;
                print STDERR "NET 1 $pin $unalias{$pin} $net $inst";
                $matched{$unalias{$pin}}=0;
            }
            my $cn=$cast{$name};
            my @ps=();
            if (defined $ports{$cn}) {
                @ps=@{$ports{$cn}};
            }
            elsif (defined ($ports{$name})) {
                @ps=@{$ports{$name}};
            }
            my @dxrf=(); # dummy cross ref
            my %reassigned=();
            # special cases
            if (! defined ($net{GND}) and defined ($bindnet{"$inst GND"})) {
                $net{GND} = $bindnet{"$inst GND"};
            }
            if (! defined ($net{GND}) and defined ($bind{"$name $inst GND"})) {
                $net{GND} = $bind{"$name $inst GND"};
            }
            if (! defined ($net{GND})) {
                $net{GND} = "GND";
            }
            if (! defined ($net{Vdd}) and defined ($bindnet{"$inst Vdd"})) {
                $net{Vdd} = $bindnet{"$inst Vdd"};
            }
            if (! defined ($net{Vdd}) and defined ($bind{"$name $inst Vdd"})) {
                $net{Vdd} = $bind{"$name $inst Vdd"};
            }
            if (! defined ($net{Vdd})) {
                $net{Vdd} = "Vdd";
            }
            if (! defined ($net{VDD}) and defined ($bindnet{"$inst VDD"})) {
                $net{VDD} = $bindnet{"$inst VDD"};
            }
            if (! defined ($net{VDD}) and defined ($bind{"$name $inst VDD"})) {
                $net{VDD} = $bind{"$name $inst VDD"};
            }
            if (! defined ($net{VDD})) {
                $net{VDD} = "Vdd"; # note: NOT VDD but Vdd
            }
            foreach my $n (0..$#ps) {
                my $ps=$ps[$n];
                my ($type,$pn)=@{$ps};
                next if (! defined ($type));
                if (! defined ($net{$pn})) {
                    if (defined ($dummynetassign{"$inst $pn"})) {
                        $dummynet=$dummynetassign{"$inst $pn"};
                        $net{$pn}="dummy$dummynet";
                        push @dxrf, [$pn,$net{$pn}];
                        print STDERR "DXRF $inst $pn dummmy$dummynet";
                    }
                    else {
                        print STDERR "$inst $pn not assigned dummy pin!";
                    }
                }
                # override
                if (defined($bindnet{"$inst $pn"}) and ($net{$pn} ne $bindnet{"$inst $pn"})) {
                    $reassigned{$net{$pn}}=1 if $net{$pn} =~ /^dummy/;
                    $net{$pn}=$bindnet{"$inst $pn"};
                }
                my $atype=$type;
                $atype =~ s/\[.*//;
                $atype =~ s/\(.*//;
                my $deftype=$defchans{"$atype"};
                $deftype = $atype if !defined $deftype;
                if ("$atype" ne "node" and
                    ! ("$deftype" =~ /^standard\./) and
                    ! defined ($imports{"$deftype.*"}) and
                    ! defined ($imports{"$deftype.$atype"})) {
                    print STDERR "Need to import $deftype.$atype";
                    $errs++;
                    $needimport{"$deftype.*"}=1 if defined $deftype;
                    $imports{"$deftype.*"}=1 if defined $deftype;
                }
                my $string = $net{$pn};
                my $s = "      $string";
                $s .= "," if $n < $#ps;
                push @castlines, "$s // $type $pn";
                $matched{$pn}=1 if ! ($net{$pn} =~ m/^dummy1/);
            }
            if ($cast{$name} =~ /\.lvttl\./) {
                push @castlines, "    )($net{VDD}, $net{GND});";
                $matched{VDD}=1;
                $matched{GND}=1;
            }
            elsif ($cast{$name} =~ /\.lvds\./) {
                push @castlines, "    )($net{VDD}, $net{GND});";
                $matched{VDD}=1;
                $matched{GND}=1;
            }
            elsif ($cast{$name} =~ /\.cdc\./) {
                push @castlines, "    )(Vdd, GND, $net{_reset});";
                $matched{_reset}=1;
            }
            elsif ($cast{$name} =~ /sIPCell/) {
                push @castlines, "    )(VddX, VddA, GND);";
                $matched{vdd}=1;
                $matched{vddA}=1;
                $matched{vss}=1;
            }
            elsif ($cast{$name} =~ /^lib.cam.sync_tcam.TCAM/) {
                push @castlines, "    )(Vdd, GND, $net{_reset});";
                $matched{VDD}=1;
                $matched{VSS}=1;
                $matched{_reset}=1;
            }
            elsif ($cast{$name} =~ /^vendor.artisan.memory/) {
                push @castlines, "    )(Vdd, GND);";
                $matched{VDD}=1;
                $matched{VSS}=1;
            }
            else {
                my @imp=();
                my $mismatch=0;
                my $np=0;
                foreach my $pn (sort keys %matched) {
                    $mismatch=1 if (! $matched{$pn});
                }
#                if ($mismatch and defined ($implied{$cast{$name}}))
                if (defined ($implied{$cast{$name}})) {
                    foreach my $ip (@{$implied{$cast{$name}}}) {
                        my ($t,$n,$d)=@{$ip};
                        if ($n eq "Vdd" or $n eq "GND") {
                            $matched{$n}=1;
                            push @imp, $n;
                        }
                        elsif ($n eq "VDD") {
                            push @imp, "Vdd";
                            $matched{$n}=1;
                            $np=1;
                        }
                        elsif ($n eq "vss" or $n eq "VSS") {
                            push @imp, "GND";
                            $matched{$n}=1;
                            $np=1;
                        }
                        elsif (defined ($net{$n})) {
                            push @imp, $net{$n};
                            $matched{$n}=1;
                            $np=1 if $n ne $net{$n};
                        }
                        else {
                            push @imp, "_";
                            $np = 1;
                        }
                    }
                }
#                while ($imp[$#imp] eq "_") {
#                    pop @imp;
#                }
                if (@imp and $np) {
                    my $s = "    )(";
                    foreach my $n (0..$#imp-1) {
                        $s .= "$imp[$n], ";
                    }
                    push @castlines, $s."$imp[$#imp]);";
                }
                else {
                    push @castlines, "    );";
                }
            }
            # write out the dummy net assignments
            foreach my $dxrf (@dxrf) {
                my ($pn,$net)=@{$dxrf};
                my $pfx="A $pn";
                my $print = 0;
                $pn =~ s/\[.*\]//;
                foreach my $px (keys %net) {
                    my $px1 = $px;
                    $px1 =~ s/[^A-Za-z0-9_].*//;
                    my $px2 = $px;
                    $px2 =~ s/$px1//;
                    if ($px2 =~ /\[\d+_\d+\]/ ) { # handle two dimensions
                        $px2 =~ s/\[(\d+)_(\d+)\]/[$1,$2]/;
                        print STDERR "D $px2\n" if $print;
                    }
                    my $px3=$px2;
                    if ($px3 =~ /\.d\[(\d+)\]$/) {
                        $px3 =~ s/\.d\[(\d+)\]$/.$1/;
                    }
                    print STDERR "A $pn:$px:$px1:$px2:$net{$px}:$net" if $print;
                    if ($px1 eq $pn and $net{$px} ne $net) {
                        print STDERR "B $px $pn ${net}$px2" if $print;
                        print STDERR "C ${net}$px2=$net{$px};" if $print;
                        if (substr($net{$px}, length($net{$px})-length($px3)) eq $px3 and length($px3)) {
                            my $opx = substr($net{$px},0,length($net{$px})-length($px3));
                            if ( defined ($dumxrf{$net})) {
                                if ($dumxrf{$net} ne $opx) {
                                    print STDERR "AERR $net $opx $dumxrf{$net}" if $print;
                                }
                            }
                            else {
                                $dumxrf{$net} = $opx;
                            }
                            print STDERR "$pfx $net $px2 $net{$px} $px" if $print;
                        }
                        print STDERR "DXR $inst $top ${net}$px2=$net{$px}";
                        push @castlines, "   ${net}$px2=$net{$px};" if ! $reassigned{$net};
                        $openpins{$net}=0;
                        $matched{$px}=1;
                    }
                }
            }
            foreach my $pn (sort keys %matched) {
                if (! $matched{$pn}) {
                    my $found = 0;
                    if (defined ($implied{$cast{$name}})) {
                        foreach my $ip (@{$implied{$cast{$name}}}) {
                            my ($t,$n,$d)=@{$ip};
                            if ($n eq $pn) {
                                print STDERR "Unmatched implied pin $name $inst $pn $net{$pn}";
                                $found = 1;
                            }
                        }
                    }
                    else {
                        print STDERR "no implied data for $cast{$name}";
                    }
                    print STDERR "Unmatched $cast{$name} $inst $pn $net{$pn}" if ! $found;
                    $errs++;
                }
            }
        }
    }
    push @castlines, "  \x7d";
    push @castlines, "\x7d";
}
foreach my $name (sort keys %implied) {
    print STDERR "IMPLIED $name";
}
open (OPEN, ">openpins.txt");
foreach my $key (sort keys %openpins) {
    my ($pin,$type,$cell,$inst)=split(/ /,$openpins{$key});
    print STDERR "pin $pin type $type of $cell inst $inst is open" if ($openpins{$key} ne "0");
    print OPEN "pin $pin type $type of $cell inst $inst is open" if ($openpins{$key} ne "0");
}
close OPEN;
print STDERR "Completed with $errs error(s)" if $errs;
#print join("\n", @castlines);
fixup (@castlines);
exit $errs;
