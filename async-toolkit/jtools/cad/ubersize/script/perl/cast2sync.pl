#!/usr/intel/bin/perl -l
# AAG
# $Id$
# $DateTime$

use strict;
use Getopt::Long;
use IPC::Open2;

# start from the TOP cast definition and generate
# the verilog->rtl wrapper (without files) and a
# skeleton of the core module and the complete
# top.v file

#my $todo <<TODO;
# TODO

BEGIN {
    my $package_root = $0;
    my $exe = $package_root;
    $exe =~ s:.*/::;
    if (! ($package_root =~ m:^/:)) {
        my $pwd = `pwd`;
        chomp $pwd;
        $package_root = $pwd;
        $package_root .= "/$0";
        $package_root =~ s:$exe$::;
        $package_root =~ s://:/:g;
        chdir $package_root;
        $package_root = `pwd`;
        chomp $package_root;
        chdir $pwd;
    }
    else {
        $package_root =~ s:/bin/$exe::;
    }
    if ( -d $package_root ) {
        push @INC, "$package_root/lib/perl";
    }
}

use Perforce qw(getdirsfromp4client);

my $info=0;
my $warn=0;
my $err =0;
my $debug=0;
my %cports=();
my %types=();
my %count=();
my %canon=();
my %canondir=();
my %full=();
my @out=();
my $sramchains=0;
my $syncchains=1;
my $wrapper_top=undef;
my $verbose=0;
my $core_module = undef;
my $mid_module = undef;
my $core_verilog = "xc.v";
my $top_verilog = "xt.v";
my $cast_out = "x.cast";
my @includes = ();
my $cast_dir;
my $spec_dir;
my $spar_dir;
my $dfII_dir;
my $core_rtl;
my $default_freq = 515;
my $client;
my $autogen=0;
my $pdk_root;
my $pwd=`pwd`;
my $transitionspercycle=26;
chomp $pwd;
my %nogroupchan = (
    CDC_LS => 1,
    CDC_RS => 1,
    SCAN_LS => 1,
    SCAN_RS => 1,
);
my @msg=();

my $renamepid = open2(\*RDC, \*WDC, "rename --type=cell --from=cast --to=cadence");

my %rtlports=();
my @rtlports=();
my $core_rtl_module;

sub readrtlports {
    my ($core_rtl)=@_;
    if (defined ($core_rtl)) {
        my $module;
        my $ports;
        my $frtl;
        if (open ($frtl, "<$core_rtl")) {
            my $savslash=$/;
            $/=";";
            $_=<$frtl>;
            while ((! /module/) and $_ ne "") {
                $_=<$frtl>;
            }
            s/\s/ /g;
            s/.*module\s*//;
            s/;$//;
            s/\s*\(\s*/(/g;
            s/\s*\)\s*/)/g;
            ($module,$ports)=split(/[()]/, $_);
            $core_rtl_module=$module;
            $ports =~ s/ //g;
            @rtlports=split(/,/,$ports);
            while (<$frtl>) {
                chomp;
                s/\s+/ /g;
                s/^\s//;
                s/\s$//;
                if(/^(input|output) /) {
                    my $direction=$1;
                    s/[\s,]+/ /g;
                    my @f=split;
                    shift @f;
                    my $type = shift @f;
                    $type = shift @f if $type eq "wire" or $type eq "reg";
                    my $l = -1;
                    my $r = -1;
                    if ($type =~ /^\[(\d+):(\d+)\]$/) {
                        $l = $1;
                        $r = $2;
                    }
                    else {
                        unshift @f, $type;
                        $type = "";
                    }
                    my $port;
                    while ($port = shift @f) {
                        $rtlports{$port}="$direction $type";
                        $rtlports{$port} =~ s/\s$//;
                    }
                }
            }
            close $frtl;
            $/=$savslash;
        }
        else {
            error("Error: Cannot open $core_rtl");
            $err++;
        }
    }
}

sub childerrstr {
    # convert $? to a more readable error string; see "system" in perlfunc
    my $result;
    if ($? == -1) {
        $result = "failed to execute: $!";
    }
    elsif ($? & 127) {
        $result = sprintf("child died with signal %d, %s coredump", ($? & 127),
                          ($? & 128) ? 'with' : 'without');
    }
    else {
        $result = sprintf("child exited with value %d", $? >> 8);
    }
    return $result;
}

sub cellcast2cadence {
    my ($name) = @_;
    print WDC "$name";
    $name = <RDC>;
    chomp $name;
    $name;
}

sub error {
    my ($msg) = @_;
    my $severe = 0;
    if ($msg =~ /^info/i) {
        $info++;
    }
    elsif ($msg =~ /^warning/i) {
        $warn++;
        $severe=1;
    }
    elsif ($msg =~ /^error/i) {
        $err++;
        $severe=1;
    }
    else {
        $msg = "Info: $msg";
        $info++;
    }
    print STDERR $msg if $verbose or $severe;
    push @msg, $msg if $verbose;
}

my %options = (
    "verbose" => \$verbose,
    "sram-scan-chain-count=i" => \$sramchains,
    "sync-scan-chain-count=i" => \$syncchains,
    "core-module=s" => \$core_module,
    "mid-module=s" => \$mid_module,
    "cell=s" => \$wrapper_top,
    "debug" => \$debug,
    "top-verilog=s" => \$top_verilog,
    "core-verilog=s" => \$core_verilog,
    "cast-out=s" => \$cast_out,
    "cast-dir=s" => \$cast_dir,
    "spec-dir=s" => \$spec_dir,
    "spar-dir=s" => \$spar_dir,
    "dfII-dir=s" => \$dfII_dir,
    "core-rtl=s" => \$core_rtl,
    "client=s" => \$client,
    "auto-generate" => \$autogen,
    "fulcrum-pdk-root=s" => \$pdk_root,
    "default-cdc-freq=f" => \$default_freq,
    "transitionspercycle=i" => \$transitionspercycle,
);

my $usage = <<EU;
   --verbose                   verbose mode
   --sram-scan-chain-count=<n> in case sram scan chains not yet defined
   --sync-scan-chain-count=<n> in case sram sync chains > 1
   --core-module=<name>        name of core module, defaults to the base name
   --cell=<fqcn>               name of the top cell in cast
   --debug                     debug mode
   --top-verilog=<name>        name of top verilog for P\&R, defaults to xt.v
   --core-verilog=<name>       core verilog file, defaults to xc.v
   --cast-out=<name.cast>      new top cast, defaults to x.cast
   --cast-dir=<dir>            normal cast dir
   --dfII-dir=<dir>            normal dfII dir
   --spec-dir=<dir>            normal spec dir
   --spar-dir=<dir>            normal spar dir
   --client=<name>             p4 client name
   --core-rtl=<verilog>        core verilog to check port names
   --auto-generate             automatically generate cdcs and cdc timing files
   --fulcrum-pdk-root=<dir>    pdk root
   --mid-module=<name>         the mid cell module name if not auto generated
   --default-cdc-freq=<MHz>    default frequency if cast has no directive.
   --transitionspercycle=<26>  transitions per cycle
EU

sub usage {
    my ($msg) = @_;
    print STDERR "$msg" if defined $msg;
    print $usage;
    exit 1;
}

sub checkusage {
    my %usage=();
    foreach my $line (split(/\n/, $usage)) {
        if ($line =~ /\s+--/) {
            $line =~ s/^\s+--//;
            $line =~ s/[=\s].*//;
            $usage{$line}=1;
        }
    }
    foreach my $option (sort keys %options) {
        $option =~ s/=.*//;
        $usage{$option} |= 2;
    }
    my $ok=1;
    foreach my $option (sort keys %usage) {
        print "$option missing from usage message" if $usage{$option} == 2;
        print "$option missing from options" if $usage{$option} == 1;
        $ok = 0 if $usage{$option} != 3;
    }
    return if $ok;
    print STDERR "Developer: Usage message and/or options invalid, fix before release!";
    exit 1;
}

checkusage ();

GetOptions ( %options ) or usage;

my %dirs=(
    "cast_dir" => $cast_dir,
    "spec_dir" => $spec_dir,
    "spar_dir" => $spar_dir,
    "dfII_dir" => $dfII_dir,
);

readrtlports($core_rtl);

getdirsfromp4client($client,\%dirs,$verbose);
$cast_dir=$dirs{cast_dir};
$spec_dir=$dirs{spec_dir};
$spar_dir=$dirs{spar_dir};
$dfII_dir=$dirs{dfII_dir};
usage "Error: no cast-dir: $cast_dir" if $cast_dir eq "";
usage "Error: no spec-dir: $spec_dir" if $spec_dir eq "";
usage "Error: no spar-dir: $spar_dir" if $spar_dir eq "";
usage "Error: no dfII-dir: $dfII_dir" if $dfII_dir eq "";
usage "Error: Invalid cast-dir: $cast_dir" if ! -d $cast_dir;
usage "Error: Invalid spec-dir: $spec_dir" if ! -d $spec_dir;
usage "Error: Invalid spar-dir: $spar_dir" if ! -d $spar_dir;
usage "Error: Invalid dfII-dir: $dfII_dir" if ! -f "$dfII_dir/cds.lib.generated";
if ($autogen and ( ! ( -w "$spar_dir/lib/synchronous/conversion/v3" ) or ! ( -w $spec_dir ) ) ) {
    print STDERR "Warning: for autogenerate, must be writable: $spar_dir/lib/synchronous/conversion/v3" if ( ! -w "$spar_dir/lib/synchronous/conversion/v3" );
    print STDERR "Warning: for autogenerate, spec-dir must be writable: $spec_dir" if ( ! -w $spec_dir );
    $autogen=0;
    print STDERR "   ... Autogenerate turned off";
}

usage("Error: Too many scan chains, must be <= 8")
    if $syncchains + $sramchains > 8;
usage("Error: Invalid sync chain count") if $syncchains < 1;
usage("Error: Invalid sram chain count") if $syncchains < 0;
usage("Error: No cell defined") if ! defined $wrapper_top;


my $cast_path = "$cast_dir:$spec_dir";

my $default_tau = int(1e6/$transitionspercycle/$default_freq);

my $cell = $wrapper_top;
$cell =~ s/\.\d+$//;
if (! defined ($core_module)) {
    $core_module = $cell;
    $core_module =~ s/.*\.//;
    $core_module .= "_CORE";
}
if (! defined ($mid_module)) {
    $mid_module = $cell;
    $mid_module =~ s/.*\.//;
    $mid_module .= "_MID";
}
my %counted=();

# required port definitions
my %required = (
#   "0 lib.serial.scan.ChanDft -CDC_LS[0..3]" => 0,
#   "0 lib.serial.scan.ChanDft +CDC_RS[0..3]" => 0,
   "0 node -CDC_SCAN_ACTIVE" => 0,
   "0 node -CLK" => 0,
   "0 standard.channel.e1of4 -SCAN_LS" => 0,
   "0 standard.channel.e1of4 +SCAN_RS" => 0,
   "1 node -Vdd" => 0,
   "1 node -GND" => 0,
   "1 node -_RESET" => 0,
);

# top verilog port to top cast port connections
my %vtlkup_required=(   # from required channels
   "CDC_SCAN_ACTIVE" => "CDC_SCAN_ACTIVE",
   "CLK" => "CLK",
   "SCAN_LS__d" => "{<,i:4: SCAN_LS.d[i]> }",
   "SCAN_LS__e" => "SCAN_LS.e",
   "SCAN_RS__d" => "{<,i:4: SCAN_RS.d[i]> }",
   "SCAN_RS__e" => "SCAN_RS.e",
   "CHIP_RESET_N" => "_RESET",
#   "MODULE_RESET_N" => "MODULE_RESET_N",
);

my $MODULE_RESET_N = "CHIP_RESET_N";
# top verilog port to top cast port connections
my %vtlkup=();

# top verilog port to top cast port connections, expanded
# filled in later
my %vtalkup = ();

# top verilog port types
my %vtype=();

# top verilog port directions
my %vdir=();

# to cross check the groups with the ports
my %expandedports = ();

# add to vtlkup
sub vtlkup {
    my ($port, $string, $msg) = @_;
    return if $port eq "";
    print STDERR "XXX $port $string $msg" if defined $msg;
    if (defined ($vtlkup{$port}) and $string ne $vtlkup{$port}) {
        error "Error: duplicate entry for vtlkup\{$port\}, $vtlkup{$port} -> $string\n       using $string";
    }
    $vtlkup{$port}=$string;
    my @list = getlist($string);
    @{$vtalkup{$port}}=[@list];
    $vtype{$port} = "";
    $vtype{$port} = sprintf "[%d:0]", $#list if $#list > 0;
    $vtype{$port} = "[0:0]" if $#list == 0 and $list[0] =~ /\]$/;
    foreach my $name (@list) {
        $expandedports{$name}=1;
    }
}

# include required ports
foreach my $port (keys %vtlkup_required) {
    vtlkup($port, $vtlkup_required{$port});
}

# to order ports

sub vcmp {
    my ($ap,$an,$as,$bp,$bn,$bs) = (-1,-1,-1,-1,-1);
    if ($a =~ m:([^\[]+)\[(\d+)\](.*):) {
        $ap = $1;
        $an = $2;
        $as = $3;
    }
    else {
        return $a cmp $b;
    }
    if ($b =~ m:([^\[]+)\[(\d+)\](.*):) {
        $bp = $1;
        $bn = $2;
        $bs = $3;
    }
    else {
        return $a cmp $b;
    }
    return -1 if ($an < $bn) and $ap eq $bp and $as eq $bs;
    return 1 if ($an > $bn) and $ap eq $bp and $as eq $bs;
    $a cmp $b;
}

my @cports=();
my @topports=();
my $cdccnt=0;
my %cdcbits=();
my %cdcchannels=();
my %cdcdirection=();
my %castdirection=();
my %grouplookup=();
my %cdcarray;
my %port_group=();
my %group_directive=();
my %nodes=();
my %channels=(); # all nodes/e1of channels from cast
my %group_frequency=();
my %group_clock=();

sub read_cast {
    my ($cell)=@_;
    my @ports=();
    my %dir=();
    my %matched=();
    my %rdir=( "" => "", "+" => "-", "-" => "+");
    my %localchannels=();

    # read the data from the cast header
    @cports=`cast_query --cast-path='$cast_path' --task=external_nodes=im:di:al,ports,paramdirectives=port_group,paramdirectives=group_frequency,paramdirectives=group_clock --no-header --no-recurse --cell='$cell'`;
    chomp @cports;
    # creates the following %required noted, @topports, %canon, %castdirection
    foreach my $line (@cports) {
        if (($line =~ /(.*)\s+port_group:.*group_frequency:/) and $1 eq $cell) {
            my ($x, @params) = split(/ /, $line);
            foreach my $list (@params) {
                my @list = split(/:/, $list);
                my $directive = shift @list;
                if ($directive eq "port_group") {
                    foreach my $item (@list) {
                        my ($channel,$group) = split(/=/,$item);
                        push @{$port_group{$group}}, $channel;
                        $group_directive{$channel}=$group;
                    }
                }
                elsif ($directive eq "group_frequency") {
                    foreach my $item (@list) {
                        my ($group,$freq) = split(/=/,$item);
                        if ($group eq "DEFAULT") {
                            $default_freq = $freq;
                            $default_tau = int(1e6/$transitionspercycle/$default_freq);
                        }
                        $group_frequency{$group}=$freq;
                    }
                }
                elsif ($directive eq "group_clock") {
                    foreach my $item (@list) {
                        my ($group,$clock) = split(/=/,$item);
                        $group_clock{$group}=$clock;
                    }
                }
            }
            next;
        }
        else {
            my ($implied,$type,$name)=split(/ /, $line);
            if (defined ($name)) {
                # the actual port full channel definitions
                if (defined ($required{$line})) {
                    $required{$line}=1;
                    push @topports, "$type $name" if ! $implied;
                    next;
                }
                if ($line =~ /^0 lib.serial.scan.ChanDft [-\+]CDC_[LR]S\[0\.\.(\d+)\]$/) {
                    my $chcnt = $1 + 1;
                    push @topports, "$type $name" if ! $implied;
                    next;
                }
                if ($implied) {
                    error "Warning: Unrecognized implied port $line\n";
                    next;
                }
                if ($name eq "-MODULE_RESET_N") {
                    $MODULE_RESET_N="MODULE_RESET_N";
                    $vtlkup_required{"MODULE_RESET_N"}="MODULE_RESET_N";
                    vtlkup("MODULE_RESET_N","MODULE_RESET_N");
                }
                push @topports, "$type $name";
                $name =~ m/([-+])(.*)/;
                my $dir=$1;
                $name=$2;
                my @names = ($name);
                if ($name =~ /\.\./) {
                    @names = ();
                    if ($name =~ m:([^\[]+)\[(\d+)\.\.(\d+)\]$:) {
                        my $nm = $1;
                        my $b = $2;
                        my $e = $3;
                        for (my $n = $b; $n <= $e; $n++) {
                            push @names, "$nm\[$n\]";
                        }
                    }
                    elsif ($name =~ m:([^\[]+)\[(\d+)\.\.(\d+),(\d+)\.\.(\d+)\]$:) {
                        my $nm = $1;
                        my $b1 = $2;
                        my $e1 = $3;
                        my $b2 = $4;
                        my $e2 = $5;
                        for (my $n = $b1; $n <= $e1; $n++) {
                            for (my $j = $b2; $j <= $e2; $j++) {
                                push @names, "$nm\[$n,$j\]";
                            }
                        }
                    }
                }
                foreach my $name (@names) {
                    my $local = $name;
                    my $suffix = "";
                    if ($local =~ /\[/) {
                        $suffix = $local;
                        $suffix =~ s/.*\[/[/;
                        $local =~ s/\[.*//;
                    }
                    if (defined $canon{$local}) {
                        $name = "$canon{$local}";
                    }
                    else {
                        error "Error: No canonical name for :$name:";
                    }
                    $cports{$name}=$dir;
                    $types{$name}=$type;
                    foreach my $full (keys %full) {
                        my $local=$full;
                        $local =~ s/[\.\[].*//;
                        if ($full{$full} eq $dir and $local eq $name) {
                            $count{$name}++ if ! $counted{$full};
                            $counted{$full}=1;
                        }
                    }
                }
            }
            else {
                my $dir = $implied;
                $dir =~ m/([-+])(.*)/;
                $dir=$1;
                my $list=$implied;
                $list =~ s/^[-+]//;
                my @list=split(/=/,$list);
                my $canon=$list[0];
                push @ports, $canon;
                $dir{$canon}=$dir;
                $nodes{$canon}=1;
                foreach my $name (@list) {
                    $castdirection{$name}=$dir;
                    $canon{$name}=$canon;
                    $full{$canon}=$dir;
                }
                $canon =~ s/[\[\.].*//;
                $canon{$canon}=$canon;
            }
        }
    }
    foreach my $port (sort @ports) {
        next if ($port eq "Vdd" or $port eq "GND");
        if ($port =~ /\.e$/) {
            my %list=();
            my $base = $port;
            $base =~ s/\.e$//;
            $base =~ s/\./\\./g;
            $base =~ s/\[/\\[/g;
            $base =~ s/\]/\\]/g;
            my $max=-1;
            # nasty N^2 loop, but not that long actually
            foreach my $p (@ports) {
                if ($p =~ /^$base\.(.*)/) {
                    my $suffix=$1;
                    $list{$suffix}++;
                    if (($suffix =~ /^\d+$/) and $max < $suffix) {
                        $max = $suffix;
                    }
                }
            }
            my $valid=1;
            if ($max == -1) {
                # enable with no data rail
                $valid=0;
            }
            else {
                # make sure that all elements of array are accounted for
                for (my $n = 0; $n <= $max; $n++) {
                    if (! defined ($list{$n})) {
                        $valid=0;
                    }
                }
            }
            if ($valid) {
                $max++;
                $base = $port;
                $base =~ s/\.e$//;
                if ($max > 4) {
                    error "Error: Cannot handle and e1of$max in $base";
                }
                $localchannels{$base}=["e1of$max",$rdir{$dir{$port}}];
                foreach my $suffix (keys %list) {
                    $matched{$base.".".$suffix}=$port;
                }
            }
            else {
                error "Warning: Seemningly invalid channel with enable $port";
            }
        }
    }
    # put nodes into the list
    foreach my $node (@ports) {
        $localchannels{$node}=["node", $dir{$node}] if ! defined $matched{$node};
    }
    # decode channel arrays
    my %arrs=();
    foreach my $base (sort keys %localchannels) {
        my ($e1of,$dir)=@{$localchannels{$base}};
        if ($base =~ /(.*)\[(\d+)\]$/) {
            my $nb=$1;
            my $ndx=$2;
            if (defined ($arrs{$nb})) {
                my ($min,$max)=@{$arrs{$nb}};
                $min = $ndx if $min > $ndx;
                $max = $ndx if $max < $ndx;
                $arrs{$nb}=[$min,$max];
            }
            else {
                $arrs{$nb}=[$ndx,$ndx];
            }
        }
        elsif ($base =~ /(.*)\[(\d+),(\d+)\]$/) {
            my $nb=$1;
            my $ndx1=$2;
            my $ndx2=$3;
            if (defined ($arrs{$nb})) {
                my ($min1,$max1,$min2,$max2)=@{$arrs{$nb}};
                $min1 = $ndx1 if $min1 > $ndx1;
                $max1 = $ndx1 if $max1 < $ndx1;
                $min2 = $ndx2 if $min2 > $ndx2;
                $max2 = $ndx2 if $max2 < $ndx2;
                $arrs{$nb}=[$min1,$max1,$min2,$max2];
            }
            else {
                $arrs{$nb}=[$ndx1,$ndx1,$ndx2,$ndx2];
            }
        }
        else {
            $channels{$base}=$localchannels{$base};
        }
    }
    foreach my $arr (sort keys %arrs) {
        my ($min1,$max1,$min2,$max2)=@{$arrs{$arr}};
        if (defined ($max2)) {
            my $base=$arr;
            $base =~ s/\[\d+,\d+\]$//;
            my ($type,$dir)=@{$localchannels{"$base\[$min1,$min2\]"}};
            if ($min1 == 0 and $min2 == 0) {
                $max1++;
                $max2++;
                $type = "$type\[$max1,$max2\]";
            }
            else {
                $type = "$type\[$min1..$max1,$min2..$max2\]";
            }
            $channels{"$arr"}=[$type,$dir];
        }
        else {
            my $base=$arr;
            $base =~ s/\[\d+\]$//;
            my ($type,$dir)=@{$localchannels{"$base\[$min1\]"}};
            if ($min1 == 0) {
                $max1++;
                $type = "$type\[$max1\]";
            }
            else {
                $type = "$type\[$min1..$max1\]";
            }
            $channels{"$arr"}=[$type,$dir];
        }
    }
}

my %groups=();
my %cdcbitlookup = (
    1 => 0,
    2 => 1,
    3 => 2,
    4 => 2,
);

my %groupchannels=();
sub setupgroup {
    my ($group)=@_;
    my $cdcbits=0;
    my $cdcchannels=0;
    my @groupchannels=();
    my $d = "${group}__d";
    my $e = "${group}__e";
    my $dls = "${group}_LS__d";
    my $drs = "${group}_RS__d";
    my $els = "${group}_LS__e";
    my $ers = "${group}_RS__e";
    my @d=();
    my @e=();
    my $dbracket=0;
    my $ebracket=0;
    my $ndx = 0;
    foreach my $set (@{$groups{$group}}) {
        my ($channel,$type) = @{$set};
        if ($type =~ m/e1of(\d+)\[(\d+)\]$/) {
            my $e1of = $1;
            my $size = $2;
            $cdcbits += 2*($size == 0 ? 1 : $size);
            $cdcchannels += ($size == 0 ? 1 : $size);
            if ($size > 0) {
                for (my $n = 0; $n < $size; $n++) {
                    push @groupchannels, $e1of;
                }
                if ($e1of == 1) {
                    push @d, "<,i:$size: ${channel}\[i\].0 >";
                    $dbracket=1;
                }
                else {
                    push @d, "<,i:$size:<,j:$e1of:  ${channel}\[i\].d\[j\] >>";
                    $ebracket=$dbracket=1;
                }
                push @e, "<,i:$size:  ${channel}\[i\].e >";
                $ebracket=1;
                for (my $i = 0; $i < $size; $i++) {
                    for (my $j = 0; $j < $e1of; $j++) {
                        if ($e1of == 1) {
                            $cdcarray{$group}[$ndx++] = "${channel}\[$i\].0";
                            $grouplookup{"${channel}\[$i\].0"}=$group;
                        }
                        else {
                            $cdcarray{$group}[$ndx++] = "${channel}\[$i\].$j";
                            $grouplookup{"${channel}\[$i\].$j"}=$group;
                        }
                    }
                }
            }
            else {
                push @groupchannels, $e1of;
                if ($e1of == 1) {
                    push @d, "${channel}.0";
                }
                else {
                    push @d, "<,j:$e1of:  ${channel}.d\[j\] >";
                    $dbracket=1;
                }
                push @e, "${channel}.e";
                for (my $j = 0; $j < $e1of; $j++) {
                    if ($e1of == 1) {
                        $cdcarray{$group}[$ndx++] = "${channel}.0";
                        $grouplookup{"${channel}.0"}=$group;
                    }
                    else {
                        $cdcarray{$group}[$ndx++] = "${channel}.$j";
                        $grouplookup{"${channel}.$j"}=$group;
                    }
                }
            }
        }
        elsif ($type =~ m/e1of(\d+)$/) {
            my $e1of = $1;
            my $size = 0;
            $cdcbits += 2*($size == 0 ? 1 : $size);
            $cdcchannels += ($size == 0 ? 1 : $size);
            if ($size > 0) {
                for (my $n = 0; $n < $size; $n++) {
                    push @groupchannels, $e1of;
                }
                if ($e1of == 1) {
                    push @d, "<,i:$size: ${channel}\[i\].0 >";
                    $dbracket=1;
                }
                else {
                    push @d, "<,i:$size:<,j:$e1of:  ${channel}\[i\].d\[j\] >>";
                    $ebracket=$dbracket=1;
                }
                push @e, "<,i:$size:  ${channel}\[i\].e >";
                $ebracket=1;
                for (my $i = 0; $i < $size; $i++) {
                    for (my $j = 0; $j < $e1of; $j++) {
                        if ($e1of == 1) {
                            $cdcarray{$group}[$ndx++] = "${channel}\[$i\].0";
                            $grouplookup{"${channel}\[$i\].0"}=$group;
                        }
                        else {
                            $cdcarray{$group}[$ndx++] = "${channel}\[$i\].$j";
                            $grouplookup{"${channel}\[$i\].$j"}=$group;
                        }
                    }
                }
            }
            else {
                push @groupchannels, $e1of;
                if ($e1of == 1) {
                    push @d, "${channel}.0";
                }
                else {
                    push @d, "<,j:$e1of:  ${channel}.d\[j\] >";
                    $dbracket=1;
                }
                push @e, "${channel}.e";
                for (my $j = 0; $j < $e1of; $j++) {
                    if ($e1of == 1) {
                        $cdcarray{$group}[$ndx++] = "${channel}.0";
                        $grouplookup{"${channel}.0"}=$group;
                    }
                    else {
                        $cdcarray{$group}[$ndx++] = "${channel}.$j";
                        $grouplookup{"${channel}.$j"}=$group;
                    }
                }
            }
        }
        else {
            error "Error: not e1of type! $type";
        }
    }
    $dbracket = $ebracket = 1 if $#d > 0;
    if ($dbracket) {
        vtlkup($d, "{".join(", ", @d)."}");
    }
    else {
        vtlkup($d, join(", ", @d));
    }
    if ($ebracket) {
        vtlkup($e, "{".join(", ", @e)."}");
    }
    else {
        vtlkup($e, join(", ", @e));
    }
    # the CDC Scan Ports
    vtlkup($dls, "{<,i:3: CDC_LS\[$cdccnt\].d\[i\] >}");
    vtlkup($els, "CDC_LS\[$cdccnt\].e");
    vtlkup($drs, "{<,i:3: CDC_RS\[$cdccnt\].d\[i\] >}");
    vtlkup($ers, "CDC_RS\[$cdccnt\].e");
    $cdcbits{$group} = $cdcbits;
    $cdcchannels{$group} = $cdcchannels;
    $groupchannels{$group}=[@groupchannels];
    $cdccnt++;
}

sub generate_cdc_groups {
    my ($cell)=@_;
    foreach my $channel (sort keys %group_directive) {
        error "Warning: port_group channel $channel is not defined in cast"
            if (! defined $channels{$channel});
        my @list=();
        my $pattern="";
        my $cnt=0;
        my @vals=();
        my @ndx=();
        my %last=();
        my $minndx=undef;
        my $maxndx=undef;
        my ($chantype,$dir)=@{$channels{$channel}};
        $channels{$channel} = [$chantype,$dir,$group_directive{$channel}];
        push @{$groups{$group_directive{$channel}}}, [$channel, $chantype];
    }
    foreach my $group (sort keys %groups) {
        setupgroup($group);
    }
    foreach my $group (keys %channels) {
        next if $nogroupchan{$group};
        my ($chantype,$dir,$groupname)=@{$channels{$group}};
        if ( ! defined($groupname) and ($chantype =~ /e1of/)) {
            my $groupname = "ASYNC_$group";
            $groupname =~ s/[\[\]\.]/_/g;
            error "Adding automatic cdc group $groupname";
            $channels{$group} = [$chantype,$dir,$groupname];
            $groups{$groupname}[0] = [$group, $chantype];
            setupgroup($groupname);
        }
    }
}

# expand the port mapping list
sub getlist {
    my ($string) = @_;
    $string =~ s/\s//g;
    $string =~ s/^\x7b//;
    $string =~ s/\x7d$//;
    my $n=0;
    while ($string =~ /(<[^<>]+>)/) {
        my $substr = $1;
        my $delim=undef;
        my $var = undef;
        my $size= undef;
        my $arg = undef;
        my @exp = ();
        if ($substr =~ m/<(.)(.):(\d+):(.*)>/) {
            $delim=$1;
            $var = $2;
            $size= $3;
            $arg = $4;
        }
        else {
            print "No match in $substr";
        }
        for(my $n = 0; $n < $size; $n++) {
            my $v = $arg;
            $v =~ s/$var/$n/g;
            $v =~ s/\.d\[$n\]$/.$n/;
            push @exp, $v;
        }
        my $sub = join ("$delim", @exp);
        $n++;
        $string =~ s/\Q$substr\Q/$sub/;;
        $string =~ s/\s//g;
        my @list = split(/,/,$string);
    }
    my @list = split(/,/,$string);
    return @list;
}

# sets bit 2 for all cast ports
sub expandports {
    $expandedports{Vdd}=1;
    $expandedports{GND}=1;

    foreach my $line (@cports) {
        my @f=split(/ /, $line);
        next if $line =~ /^\d /;
        next if $f[0] eq "$cell";
        my $dir = $line;
        $dir =~ m/([-+])(.*)/;
        $dir=$1;
        my $list=$line;
        $list =~ s/^[-+]//;
        my @list=split(/=/,$list);
        my $canon=$list[0];
        my $ematch=0;
        foreach my $name (@list) {
            if (defined ($expandedports{$name})) {
                $expandedports{$name} |= 2;
                $cdcdirection{$grouplookup{$name}} = $dir if (defined ($grouplookup{$name}));
                $ematch=1;
            }
            elsif (defined ($channels{$name}) and $channels{$name}[0] eq "node") {
                vtlkup($name,$name);
                $ematch=1;
            }
        }
        $expandedports{$list[0]} = 2 if ! $ematch;
    }
}

sub setdirection {
    foreach my $group (keys %groups) {
        my $d = "${group}__d";
        my $e = "${group}__e";
        my $dls = "${group}_LS__d";
        my $drs = "${group}_RS__d";
        my $els = "${group}_LS__e";
        my $ers = "${group}_RS__e";
        $castdirection{$d}=$cdcdirection{$group};
        $castdirection{$e}=$cdcdirection{$group} eq "-" ? "+" : "-";
        $castdirection{$dls}="-";
        $castdirection{$els}="+";
        $castdirection{$drs}="+";
        $castdirection{$ers}="-";
    }
    # from required ports
    $castdirection{"SCAN_LS__d"}="-";
    $castdirection{"SCAN_LS__e"}="+";
    $castdirection{"SCAN_RS__d"}="+";
    $castdirection{"SCAN_RS__e"}="-";
    $castdirection{"CHIP_RESET_N"}="-";
#    $castdirection{"MODULE_RESET_N"}="-";
}

read_cast($cell);
generate_cdc_groups($cell);
expandports();
setdirection();

my %addednodes=();
my %missing=();
my %warns=();
foreach my $name (sort vcmp keys %expandedports) {
    my $group = $name;
    $group =~ s/\..*//;
    $group =~ s/\[.*//;
    if ($expandedports{$name} == 1) {
        if (defined ($channels{$name}) and $channels{$name}[0] =~ /^node/) {
            $addednodes{$name}=$channels{$name}[1];
        }
        elsif (! defined $warns{$group}) {
            my $string = "Error: $name not in cast";
            my $c=$cdccnt-1;
            $string .= "\n       Probably array size too small, need 0..$c"
                if ($group =~ /^CDC_[RL]S.*/);
            error($string);
            $warns{$group}=1 if $group =~ /^CDC_[RL]S$/;
        }
    }
    if ($expandedports{$name} == 2) {
        if ($name =~ /\./) {
            if (defined $group_directive{$group}) {
                if ($group eq "CDC_LS" and ($name =~ /\.3$/)) {
                    error "Warning: node $name missing from port_group directive $group\n$group should now be e1of3";
                }
                else {
                    error "Warning: node $name missing from port_group directive $group";
                }
            }
            elsif (defined $channels{$group}) {
#                if ( ! defined $warns{$group}) {
                    my $string = "Warning: node $name missing from group $group";
                    if ($name =~ /^CDC_[RL]S.*\.3$/) {
                        $string .= "\n   $group should be e1of3 now"
                    }
                    elsif ($group =~ /^CDC_[RL]S$/) {
                        $string .= "\n   Probably array size too large in cast $cdccnt"
                    }
                    error ($string);
                    $warns{$group}=1 if $group =~ /^CDC_[RL]S$/;
#                }
            }
            else {
                if ($name =~ /(.*)(\[\d+\])$/) {
                    my $pre = $1;
                    my $post = $2;
                    $addednodes{$pre}=$channels{$pre}[1];
                }
                else {
                    $group = "ASYNC_$group";
                    print "Missing: $group $name";
                    push @{$missing{$group}}, $name if ! ($name =~ /\.e$/);
                }
            }
        }
        else {
            my $base = $name;
            $base =~ s/\[.*]$//;
            if (defined $channels{$base}) {
                my ($type,$dir)=@{$channels{$base}};
                if (! defined $addednodes{$base}) {
                    error "$base is $type, passed directly";
                    $addednodes{$base}=$dir;
                }
            }
            else {
                error "$name is node, passed directly";
                $addednodes{$name}=$castdirection{$name};
            }
        }
    }
}

sub gengrp {
    my ($prefix,$maxeof,$maxndx,@grp)=@_;
    if ($prefix ne "") {
        $maxeof++;
        if ($maxndx >= 0) {
            $maxndx++;
            push @grp, "$prefix e1of$maxeof\[$maxndx\]";
        }
        else {
            push @grp, "$prefix e1of$maxeof\[0\]";
        }
    }
    @grp;
}

sub gen {
    my @nodes=@_;
    my $lastprefix = "";
    my $name=undef;
    my $sub=undef;
    my $ndx=undef;
    my $eof=undef;
    my $maxndx=-1;
    my $maxeof=-1;
    my @grp=();
    while (@nodes) {
        if ($nodes[0] =~ /(.*)\[(\d+)\]\.(\d+)$/) {
            $name=$1;
            $ndx=$2;
            $eof=$3;
            my $prefix = "$name";
            if ($prefix ne $lastprefix) {
                @grp = gengrp($lastprefix, $maxeof, $maxndx, @grp);
                $maxeof = $maxndx = -1;
            }
            $lastprefix = $prefix;
            $maxeof = $eof if $maxeof < $eof;
            $maxndx = $ndx if $maxndx < $ndx;
        }
        elsif ($nodes[0] =~ /(.*)\.(\d+)$/) {
            $name=$1;
            $eof=$2;
            my $prefix = "$name";
            if ($prefix ne $lastprefix) {
                @grp = gengrp($lastprefix, $maxeof, $maxndx, @grp);
                $maxeof = $maxndx = -1;
            }
            $lastprefix = $prefix;
            $lastprefix = $prefix;
            $maxeof = $eof if $maxeof < $eof;
        }
        elsif ($nodes[0] =~ /(.*)\[\d+\]$/) {
            my $arr = $1;
            if (defined ($channels{$arr}) and $channels{$arr}[0] =~ /^node\[\d+\]$/) {
                print "Defined array $nodes[0]?";
            }
            else {
                print "1: What to do with $nodes[0]?";
            }
        }
        else {
            # this must be assumed to be a simple node and treated as such
            print "2: What to do with $nodes[0]?";
        }
        shift @nodes;
    }
    if ($lastprefix ne "") {
        @grp = gengrp($lastprefix, $maxeof, $maxndx, @grp);
        $maxeof = $maxndx = -1;
    }
    @grp;
}

foreach my $group (sort keys %missing) {
    my $added = 0;
    foreach my $def (gen(@{$missing{$group}})) {
        my ($name, $eof) = split(/ /,$def);
        push @{$groups{$group}},[$name, $eof];
        print STDERR "Adding $name $def to $group";
        $added++;
    }
    if ($added) {
        setupgroup($group);
        error "Adding automatic group $group, with $added element".
            ($added > 1 ? "s" : "");
    }
    else {
        error "Error: Cannot find method to add missing $group";
    }
}
my $lastbase="";
my $b=undef;
my $e=undef;
foreach my $name (sort keys %addednodes) {
    my $basename = $name;
    $basename =~ s/\[.*\]$//;
    if ($basename ne $lastbase) {
        if (defined ($b)) {
            vtlkup($lastbase, $lastbase);
        }
        else {
            vtlkup($lastbase, $lastbase);
        }
    }
    $lastbase = $basename;
    if ($name =~ /\[(\d+)\]/) {
        if (defined($b)) {
            $b = $1 if $1 < $b;
            $e = $1 if $1 > $e;
        }
        else {
            $b = $e = $1;
        }
    }
}
vtlkup($lastbase, $lastbase) if $lastbase ne "";
%warns=();
foreach my $line (sort keys %required) {
    if (! $required{$line}) {
        my $group = $line;
        $group =~ s/\..*//;
        $group =~ s/\[.*//;
        if (! defined $warns{$group}) {
            my $string = "Error: Required port $line is not in cast";
            $string .= "\n       Probably array size too small"
                if ($line =~ /^CDC_[RL]S/);
            error ($string);
            $warns{$group}=1 if $group =~ /^CDC_[RL]S$/;
        }
    }
}
my %v=();
my @vclk=();
my @vreset=();
my @vscan_en=();
my @vscan_mode=();
my $incnt = 0;
my $outcnt = 0;
foreach my $name (sort keys %cports) {
    if ($count{$name} == 0) {
        error "Count 0 for $name";
        next;
    }
    my $lg = log($count{$name})/log(2);
    if ($lg - int($lg) != 0) {
        $lg = int($lg) + 1;
    }
    $lg = 1 if $lg == 0;
    if ($cports{$name} eq "-") {
        $incnt += $lg;
    }
    elsif ($cports{$name} eq "+") {
        $outcnt += $lg;
    }
}
# top verilog ports
my %vtports=();
my %vmports=( # from scan converter
    CLK => "input",
    SCAN_EN => "input",
    SCAN_IDL => "input [7:0]",
    SCAN_IN => "input [7:0]",
    SCAN_MODE => "input",
    SCAN_OUT => "output [7:0]",
    CDC_RESET_N => "input",
    CORE_RESET_N => "input",
);
# core verilog ports
my %vcports=( # from scan converter
    CLK => "input",
    RESET_IN_N => "input",
    SCAN_EN => "input",
    SCAN_IN => "input",
    SCAN_MODE => "input",
    SCAN_OUT => "output",
    SCAN_IDL => "input",
);
if ($syncchains > 1) {
    $vcports{SCAN_IN} = sprintf "input [%d:0]", $syncchains-1;
    $vcports{SCAN_IDL} = sprintf "input [%d:0]", $syncchains-1;
    $vcports{SCAN_OUT} = sprintf "output [%d:0]", $syncchains-1;
}
foreach my $port (keys %addednodes) {
    my ($type,$dir)=@{$channels{$port}};
    $vmports{$port} = $vcports{$port} =
        $addednodes{$port} eq "-" ? "input" : "output";
    if ($type =~ /^node\[(\d+)\]$/) {
        my $n = $1;
        $n--;
        $vmports{$port} = $vcports{$port} = "$vcports{$port} [$n:0]";
    }
    elsif ($type =~ /node\[(\d+)..(\d+)\]$/) {
        $type =-~ s/node//;
        $vmports{$port} = $vcports{$port} = "$vcports{$port} $type";
    }
}
my %newwires=();
foreach my $cdc (keys %group_clock) {
    if ($group_clock{$cdc} =~ /core\/(.*)/) {
        my $addedcoreport=$1;
        $vcports{$addedcoreport} = "output";
        $group_clock{$cdc} = "$addedcoreport";
        $newwires{$addedcoreport} = 1;
    }
}

sub writecast {
    my ($file)=@_;
    my @vp=();
    my $width=0;
    foreach my $vp (keys %vtlkup) {
        $width = length($vp) if $width < length($vp);
    }
    foreach my $vp (sort keys %vtlkup) {
        my $vpt = cast2verilogrename($vp);
        push @vp, sprintf ("        .%-*.*s (%s)", $width,$width,$vpt, $vtlkup{$vp});
    }
    error ("Writing $file");
    my $fcast;
    open ($fcast, ">$file");
    select $fcast;
    my $tname=$cell;
    $tname =~ s/(.*)\.//;
    my $mname=$1;
    print "";
    # temp
    $mname = "$cell";
    $mname =~ s/\.[^\.]+$//;
    print "module $mname;";
    print "";
    my %topchan=();
    foreach my $tp (@topports) {
        if (($tp =~ /^node /) or ($tp =~ /^standard\.channel\./) ) {
            $tp =~ s/^standard.channel.//;
            next;
        }
        my ($channel,$name)=split(/ /, $tp);
        $channel =~ s/[\(\[].*//;
        if (! $topchan{$channel}) {
            print "import $channel;";
        }
        $tp =~ s/^$channel\.//;
        $tp =~ s/^[^ ]*\.//;
        $topchan{$channel}=1;
    }
    print "";
    my $indent=length("define $tname\(\)\(");
    $indent = sprintf("%-*.*s", $indent,$indent,"");
    printf "define $tname\(\)\(".join (";\n$indent", @topports);
    print ") \x7b";
    print "  verilog \x7b";
    print "    rtl \x7b";
    print "      $wrapper_top (";
    print join(",\n", @vp);
    print "      ):";
    print "      '$top_verilog',";
    print "      '$core_rtl'";
    print "      ;";
    print "    \x7d";
    print "  \x7d";
    print "\x7d";
    select STDOUT;
    close $fcast;
}

writecast ("$cast_out");

# write the core skeleton
foreach my $port (keys %vtype) {
    my $dir="xinput";
    if (defined ($channels{$port})) {
        my ($type,$chdir,$group)=@{$channels{$port}};
        $dir = $chdir eq "-" ? "input" : "output";
        $type =~ s/node//;
        if ($type =~ /^\[(\d+)\]$/) {
            my $w=$1;
            $w--;
            $type = "[$w:0]";
        }
        $vtports{$port} = "$dir $type";
        $vtports{$port} =~ s/\s+$//;
    }
    else {
        $dir = $castdirection{$port} eq "-" ? "input" : "output" if defined ($castdirection{$port});
        if ($dir eq "xinput") {
            foreach my $p (keys %castdirection) {
                if ($p =~ /^$port[\.\[]/) {
                    $dir = $castdirection{$p} eq "-" ? "input" : "output";
                    last;
                }
            }
        }
        if ($dir ne "xinput") {
            $vtports{$port} = "$dir $vtype{$port}";
            $vtports{$port} =~ s/\s+$//;
        }
    }
    if (! defined $vtports{$port}) {
        error "Error: Cannot determine direction of $port for top verilog";
    }
}
if ($sramchains > 0 ) {
    $vcports{SCAN_SRAM_IDL} =
        sprintf "input [%d:0]", $sramchains - 1;
    $vcports{SCAN_SRAM_IN} =
        sprintf "input [%d:0]", $sramchains - 1;
    $vcports{SCAN_SRAM_OUT} =
        sprintf "output [%d:0]", $sramchains - 1;
}
foreach my $group (sort keys %groups) {
    my $cgroup = $group;
    $cgroup =~ s/^ASYNC_//;
    $group_clock{$group} = "CLK" if ! defined $group_clock{$group};
    my $size = 0;
    my $last;
    my $cnt=0;
    my $isnode=1;
    foreach my $x (@{$groups{$group}}) {
        my ($name,$type)=@{$x};
        $cnt++;
        if ($type =~ /e1of(\d+)\[(\d+)\]/) {
            $last=$1;
            if ($1 <= 2) {
                $size += $2;
            }
            else {
                $size += $2*2;
            }
            $isnode=0;
        }
        elsif ($type =~ /e1of(\d+)$/) {
            $last=$1;
            $isnode = 0 if $1 > 2;
#            if ($1 <= 2) {
#                $size += 1;
#            }
#            else {
                $size += 2;
#            }
        }
    }
    $size--;
    if ($size > 2 or ($size > 0 and !$isnode)) {
        $vcports{"${cgroup}"} = $cdcdirection{$group} eq '-' ? "input [$size:0]" : "output [$size:0]";
    }
    else {
        $vcports{"${cgroup}"} = $cdcdirection{$group} eq '-' ? "input" : "output";
    }
    $vcports{"${cgroup}_V"} = $cdcdirection{$group} eq '-' ? "input" : "output";
    $vcports{"${cgroup}_E"} = $cdcdirection{$group} eq '-' ? "output" : "input";
}
if ($debug) {
    error "Writing xt.ports";
    my $fdbg;
    open ($fdbg, ">xt.ports");
    print $fdbg join("\n", (sort keys %vtports));
    close $fdbg;
    error "Writing xfan.rc";
    open ($fdbg, ">xfan.rc");
    foreach my $port (sort keys %vtports) {
        my ($dir,$size)=split(/ /,$vtports{$port});
        $dir =~ s/put//;
        if ($dir eq "in") {
            $dir = "out";
        }
        else {
            $dir = "in";
        }
        if (defined $size) {
            if ($size =~ /^\[(\d+):(\d+)\]$/) {
                my $min = $2;
                my $max = $1;
                for (my $n = $min; $n <= $max; $n++) {
                    print $fdbg "puts \"$port\[$n\]\"\n";
                    print $fdbg "set result [fan$dir -structural {$port\[$n\]}]\n";
                    print $fdbg "puts \$result";
                }
            }
        }
        else {
            print $fdbg "puts $port";
            print $fdbg "set result [fan$dir -structural $port]\n";
            print $fdbg "puts \$result";
        }
    }
    close $fdbg;
    error "Writing xc.ports";
    open ($fdbg, ">xc.ports");
    my @vcn = (sort keys %vcports);
    my %vcn=();
    foreach my $f (@vcn) {
        my $x = $f;
        $x =~ s/^ASYNC_//;
        $vcn{$x}=$f;
        $f = $x;
    }
    my @vc=();
    foreach my $port (sort @vcn) {
        push @vc, "$vcports{$vcn{$port}} $port;";
    }
    print $fdbg join ("\n", @vc);
    close $fdbg;
}

sub cast2verilogrename {
    my ($name) = @_;
    $name =~ s/\./_/g;
    $name =~ s/\[/_l_/g;
    $name =~ s/\]/_r_/g;
    $name;
}

error "Writing $top_verilog... core $core_module";
my $fvlg;
open ($fvlg, ">$top_verilog");
select $fvlg;
my $ccore_module = cellcast2cadence($core_module);

print <<EH;
`timescale 1ns / 10ps
/****************************************************************************
 * Synchronous core of: $core_module
 ***************************************************************************/
EH
print "module \\$ccore_module (";
my @ports = keys %vcports;
foreach my $port (@ports) {
    $port = cast2verilogrename($port);
}
print "    ".join(",\n    ", @ports);
print "  );";

foreach my $vport (sort keys %vcports) {
    my $pport = cast2verilogrename($vport);
    print "  $vcports{$vport} $pport;";
}
if (defined ($core_rtl_module) and $core_rtl_module ne "") {
    print "  /* CORE RTL INSTANTIATED, MODIFY AS REQUIRD */";
    my $inst=$core_rtl_module;
    $inst =~ tr/A-Z/a-z/;
    print "  $core_rtl_module $inst(";
    my @xy=();
    foreach my $vport (sort keys %vcports) {
        if (defined($rtlports{$vport})) {
            my $pport=cast2verilogrename($vport);
            push @xy, ".$pport($pport)";
        }
    }
    print "    ".join(",\n    ", @xy);
    print "  );";
}
print "endmodule";

my %swidth = ( 1 => 1, 2 => 1, 3 => 2, 4 => 2);
my %sync=();
error "Writing... mid $mid_module";
my $cmid_module = cellcast2cadence($mid_module);
foreach my $cdc (sort keys %groups) {
    my @sync=();
    my @async=();
    my $sbit=0;
    my $abit=0;
    my $corename = $cdc;
    $corename =~ s/^ASYNC_//;
    foreach my $x (@{$groups{$cdc}}) {
        my ($name,$type)=@{$x};
        $name = cast2verilogrename($name);
        my $e1of;
        my $size;
        if ( $type =~ /^e1of(\d+)\[(\d+)\]$/) {
            $e1of=$1;
            $size=$2;
        }
        elsif ( $type =~ /^e1of(\d+)$/) {
            $size=1;
            $e1of=$1;
        }
        else {
            error "Error: bad type $type in group $cdc";
        }
        $vmports{"${cdc}_RS__e"} = "input";
        $vmports{"${cdc}_LS__e"} = "output";
        $vmports{"${cdc}_LS__d"} = "input ".$vtype{"${cdc}_LS__d"};
        $vmports{"${cdc}_RS__d"} = "output ".$vtype{"${cdc}_RS__d"};
        my $msize=$size-1;
        $vmports{"${cdc}__e"} = ($cdcdirection{$cdc} eq "-" ? "output" : "input")." ".$vtype{"${cdc}__e"};
        $msize=2*$size-1;
        $vmports{"${cdc}__d"} = ($cdcdirection{$cdc} eq "+" ? "output" : "input")." ".$vtype{"${cdc}__d"};
    }
}
# check for existence of necessary CDCs
my @required = (
    "$spar_dir/lib/synchronous/conversion/v3/netlist/FQCN.v",
    "$spar_dir/lib/synchronous/conversion/v3/netlist/lib.synchronous.conversion.v3.CELLNAME.500.cdl",
    "$spar_dir/lib/synchronous/conversion/v3/def/lib.synchronous.conversion.v3.CELLNAME.500.def",
    "$spar_dir/lib/synchronous/conversion/v3/rtl/FQCN.v",
    "$spar_dir/lib/synchronous/conversion/v3/atpg/FQCN.v",
    "$spar_dir/lib/synchronous/conversion/v3/netlist/FQCN.v",
    "$spec_dir/lib/synchronous/conversion/v3/CELLNAME/500.cast",
);

my $sdcrequired="$spar_dir/lib/synchronous/conversion/v3/sdc/FQCN.TAUps.info";

my $okcdc=0;
my %okcdc=();
my %oksdc=();
foreach my $cdc (sort keys %groups) {
    my ($channel,$type)=@{$groups{$cdc}};
    my $dcnt = $cdcbits{$cdc};
    $dcnt += $dcnt % 2;
    my $dir=$cdcdirection{$cdc} eq "-" ? "A2S" : "S2A";
    my $cn=sprintf "SCAN_%s(%d)", $dir, $dcnt;
    my $fqcn=sprintf "lib.synchronous.conversion.v3.%s.%d", $cn, 500;
    my @r=@required;
    $okcdc{$cn}=1;
    foreach my $file (@r) {
        $file =~ s/FQCN/$fqcn/g;
        $file =~ s/CELLNAME/$cn/g;
        if (! -f $file ) {
            print STDERR "$file missing";
            $okcdc=0;
            $okcdc{$cn}=0;
        }
    }
    my $tau = $default_tau;
    $tau = int(1e6/$transitionspercycle/$group_frequency{$cdc}+0.5) if defined $group_frequency{$cdc};
    my $gf = $default_freq;
    $gf = $group_frequency{$cdc} if defined $group_frequency{$cdc};
    my $file = $sdcrequired;
    $file =~ s/FQCN/lib.synchronous.conversion.v3.$cn/g;
    $file =~ s/CELLNAME/$cn/g;
    $file =~ s/TAU/$tau/g;
    $oksdc{"$cn $gf $tau"}=$file if ! -s $file;
}
foreach my $cn (sort keys %okcdc) {
    if (! $okcdc{$cn}) {
        if ($autogen) {
            print STDERR "Generating $cn";
            my $cmd = "compile_cdc";
            if (defined ($client)) {
                $cmd .= " --client='$client'";
            }
            else {
                $cmd .= " --cast-dir='$cast_dir'";
                $cmd .= " --spec-dir='$spec_dir'";
                $cmd .= " --spar-dir='$spar_dir'";
            }
            my $dl="$$";
            $cmd .= " --fulcrum-pdk-root='$pdk_root'";
            $cmd .= " --out-dir='$spar_dir/lib/synchronous/conversion/v3'";
            $cmd .= " --tmp-dir='/scratch/tmp$dl'";
            $cmd .= " --verbose" if $verbose;
            $cmd .= " --subtype-max=500";
            $cmd .= " '$cn'";
            print STDERR $cmd if $verbose;
            `$cmd`;
            `/bin/rm -rf '/scratch/tmp$dl/'`;
        }
        else {
            print STDERR "Need to generate CDC $cn";
        }
    }
}
foreach my $key (sort keys %oksdc) {
    my ($cn,$gf, $tau) = split(/ /,$key);
    if ($autogen) {
        print STDERR "Generating SDC values for $cn, F=$gf, Tau=$tau";
        my $cmd="generate_sdcinfo";
        $cmd .= " --fulcrum-pdk-root='$pdk_root'";
        $cmd .= " --cast-dir='$cast_dir'";
        $cmd .= " --spec-dir='$spec_dir'";
        $cmd .= " --dfII-dir='$dfII_dir'";
        $cmd .= " --cdc-fqcn='lib.synchronous.conversion.v3.$cn.500'";
        $cmd .= " --tau=$tau";
        $cmd .= " --output='$oksdc{$key}'";
        my $filedir=$oksdc{$key};
        $filedir =~ s/\/[^\/]+$//;
        `mkdir -p '$filedir'` if ! -d $filedir;
        my $edit=0;
        if (defined($client) and -e $oksdc{$key} and ! -w $oksdc{$key}) {
            $edit=1;
            print STDERR "p4 -c '$client' edit '$oksdc{$key}'" if $verbose;
            `p4 -c '$client' edit '$oksdc{$key}'`;
        }
        print STDERR "$cmd" if $verbose;
        my $rv=`$cmd 2>\&1`;
        if ($? != 0) {
            print STDERR "Error running $cmd: " . childerrstr();
            $err++;
        }
        my @rv = split(/\n/,$rv);
        foreach my $el (@rv) {
            if ($el =~ /Error/) {
                print STDERR $el;
                $err++;
            }
        }
        if (defined($client) and -e $oksdc{$key} and -w $oksdc{$key} and ! $edit) {
            print STDERR "p4 -c '$client' add '$oksdc{$key}'" if $verbose;
            `p4 -c '$client' add '$oksdc{$key}'`;
        }
        if ( ! -e $oksdc{$key}) {
            error "Error: did not generate SDC data from $key";
        }
    }
    else {
        error "Info: Need to generate SDC values for $cn, F=$gf, Tau=$tau";
    }
}

print "\nmodule \\$cmid_module (";
my @ports = keys %vmports;
foreach my $port (@ports) {
    $port = cast2verilogrename($port);
}
print "    ".join(",\n    ", sort @ports);
print "  );";

foreach my $vport (sort keys %vmports) {
    my $pport = cast2verilogrename($vport);
    print "  $vmports{$vport} $pport;";
}
foreach my $wire (sort keys %newwires) {
    print "  wire       $wire;";
}
my $scan_daisy=0;
print "\n  // the cdcs";
my @cdcxref=();
my %swidth = ( 1 => 1, 2 => 1, 3 => 2, 4 => 2);
my %sync=();
foreach my $cdc (sort keys %groups) {
    my ($channel,$type)=@{$groups{$cdc}};
    my @sync=();
    my @async=();
    my $sbit=0;
    my $abit=0;
    my $corename = $cdc;
    $corename =~ s/^ASYNC_//;
    foreach my $x (@{$groups{$cdc}}) {
        my ($name,$type)=@{$x};
        $name = cast2verilogrename($name);
        my $e1of;
        my $size;
        if ( $type =~ /^e1of(\d+)\[(\d+)\]$/) {
            $e1of=$1;
            $size=$2;
        }
        elsif ( $type =~ /^e1of(\d+)$/) {
            $size=1;
            $e1of=$1;
        }
        else {
            error "Error: bad type $type in group $cdc";
        }
        if ($size * $swidth{$e1of} == 1) {
            printf "  wire ${name};\n";
        }
        else {
            printf "  wire [%d:0] ${name};\n", $size*$swidth{$e1of}-1;
        }
        my $x=0;
        for(my $n = 0; $n < $size; $n++) {
            my $j;
            for ($j = 0; $j < $swidth{$e1of}; $j++) {
                if ($size * $swidth{$e1of} > 1) {
                    push @sync, "$name\[$x\]";
                }
                else {
                    push @sync, "$name";
                }
                $x++;
                $sbit++;
            }
            for (; $j < 2; $j++) {
                if ($cdcdirection{$cdc} eq "+") {
                    push @sync, "";
                }
                else {
                    push @sync, "1'b0";
                }
            }
            for ($j = 0; $j < $e1of; $j++) {
                if ($e1of == 1 and $size == 1) {
                    push @async, "${cdc}__d";
                }
                else {
                    push @async, "${cdc}__d\[$abit\]";
                }
                $abit++;
            }
            for (; $j < 4; $j++) {
                if ($cdcdirection{$cdc} eq "-") {
                    push @async, "1'b0";
                }
                else {
                    push @async, "";
                }
            }
        }
    }
    $sync{$cdc}=[@sync];
    my $dcnt = $cdcbits{$cdc};
    my $ename = "${cdc}_E";
    my $vname = "${cdc}_V";
    my $dname = "${cdc}";
    my @vdata=($dname);
    $dcnt += $dcnt % 2;
    my %data=();
    foreach my $data (@vdata) {
        my $n = $data;
        $n =~ s/\[.*//;
        $data{$n}=1;
    }
    my $width=0;
    for (my $n = 0; $n < $dcnt/2; $n++) {
        $width += $groupchannels{$cdc}[$n] > 2 ? 2 : 1;
    }
#    printf "  wire [%d:0] ${corename};\n", $width-1;
    printf "  wire ${corename}_V;\n";
    printf "  wire ${corename}_E;\n";
    printf "  // $cdc L %s core, R %s top\n",
        $cdcdirection{$cdc} eq "-" ? "to" : "from",
        $cdcdirection{$cdc} eq "+" ? "to" : "from";
    # declaration
    my $tau = $default_tau;
    $tau = int(1e6/$transitionspercycle/$group_frequency{$cdc}+0.5) if defined $group_frequency{$cdc};
    my $cdcinst="mid/CDC_${cdc}_${tau}ps";
    my $cdctype=sprintf "lib.synchronous.conversion.v3.SCAN_%s-L%d-R.%d",
        $cdcdirection{$cdc} eq "-" ? "A2S" : "S2A",
        $dcnt,
        500;
    printf "  \\lib.synchronous.conversion.v3.SCAN_%s-L%d-R.%d  %s(\n",
        $cdcdirection{$cdc} eq "-" ? "A2S" : "S2A",
        $dcnt,
        500,
        "CDC_${cdc}_${tau}ps";
    my $cndx=0;
    # the sync side
    for (my $n = 0; $n < $dcnt; $n++) {
        if ($sync[$n] eq "" or ($sync[$n] =~ /1'/)) {
            $sync[$n] = $cdcdirection{$cdc} eq "-" ? "" : "1'b0";
        }
        push @cdcxref, sprintf "$cdctype $cdcinst %s.d[$n] $sync[$n]", $cdcdirection{$cdc} eq "-" ? "R" : "L";
        printf "    .\\%s.d[$n]  ( %s ),\n", $cdcdirection{$cdc} eq "-" ? "R" : "L", $sync[$n];
    }
    push @cdcxref, "$cdctype $cdcinst ${corename}_V";
    printf "    .\\%s.v  ( \\${corename}_V ),\n", $cdcdirection{$cdc} eq "-" ? "R" : "L", "";
    push @cdcxref, "$cdctype $cdcinst ${corename}_E";
    printf "    .\\%s.e  ( \\${corename}_E ),\n", $cdcdirection{$cdc} eq "-" ? "R" : "L", "";
    # the async side
    my $max = "${cdc}__d";
    $max = $vtports{$max};
    if ($max =~ /\[(\d+):0\]/) {
        $max = $1;
    }
    else {
        $max = 1e6;
    }
    my $dndx=0;
    for (my $n = 0; $n < $dcnt/2; $n++) {
        for (my $j = 0; $j < 4; $j++) {
            push @cdcxref, sprintf "$cdctype $cdcinst %s[$n].$j $async[$dndx]", $cdcdirection{$cdc} eq "-" ? "L" : "R";
            printf "    .\\%s[$n].$j  ( $async[$dndx] ),\n", $cdcdirection{$cdc} eq "-" ? "L" : "R";
            $dndx++;
        }
        if ($dcnt/2 <= 1) {
            push @cdcxref, sprintf "$cdctype $cdcinst %s[$n].e ${cdc}__e", $cdcdirection{$cdc} eq "-" ? "L" : "R", "";
            printf "    .\\%s[$n].e  ( ${cdc}__e ),\n", $cdcdirection{$cdc} eq "-" ? "L" : "R";
        }
        else {
            push @cdcxref, sprintf "$cdctype $cdcinst %s[$n].e ${cdc}__e\[$n\]", $cdcdirection{$cdc} eq "-" ? "L" : "R";
            printf "    .\\%s[$n].e  ( ${cdc}__e\[$n\] ),\n", $cdcdirection{$cdc} eq "-" ? "L" : "R";
        }
    }
    # the async scan ports
    for (my $n = 0; $n < 3; $n++) {
        push @cdcxref, "$cdctype $cdcinst LS.$n ${cdc}_LS__d[$n]";
        printf "    .\\LS.$n     ( ${cdc}_LS__d[$n] ),\n";
        push @cdcxref, "$cdctype $cdcinst RS.$n ${cdc}_RS__d[$n]";
        printf "    .\\RS.$n     ( ${cdc}_RS__d[$n] ),\n";
    }
    push @cdcxref, "$cdctype $cdcinst LS.e ${cdc}_LS__e";
    printf "    .\\LS.e     ( ${cdc}_LS__e ),\n";
    push @cdcxref, "$cdctype $cdcinst RS.e ${cdc}_RS__e";
    printf "    .\\RS.e     ( ${cdc}_RS__e ),\n";
    # the sync scan ports
    push @cdcxref, "$cdctype $cdcinst SCAN.EN SCAN_EN";
    push @cdcxref, "$cdctype $cdcinst SCAN.MODE SCAN_MODE";
    push @cdcxref, sprintf "$cdctype $cdcinst SCAN.IN %s", $scan_daisy ? "DAISY$scan_daisy" : "SCAN_IN[$sramchains]";
    printf "    .\\SCAN.EN   ( SCAN_EN ),\n";
    printf "    .\\SCAN.MODE ( SCAN_MODE ),\n";
    printf "    .\\SCAN.IN   ( %s ),\n", $scan_daisy ? "DAISY$scan_daisy" : "SCAN_IN[$sramchains]";
    $scan_daisy++;
    push @cdcxref, "$cdctype $cdcinst SCAN.OUT DAISY$scan_daisy";
    printf "    .\\SCAN.OUT  ( DAISY$scan_daisy ),\n";
    # reset port
    push @cdcxref, "$cdctype $cdcinst _RESET CDC_RESET_N";
    printf "    .\\_RESET     ( CDC_RESET_N ),\n";
    # clock port
    push @cdcxref, "$cdctype $cdcinst CLK $group_clock{$cdc}";
    printf "    .\\CLK       ( $group_clock{$cdc} )\n";
    print "  );";
}
my $finfo;
open ($finfo, ">$wrapper_top.net.info");
print $finfo join("\n", @cdcxref);
close $finfo;
# core
# core nodes
# core ASYNC translations to/from CDCs
my %done=();
my $ccore_module = cellcast2cadence($core_module);
my $openndx = 1001;
my @core=();
push @core, "  \\$ccore_module  core(";
if ($syncchains == 1) {
    push @core, "    .SCAN_IN ( DAISY$scan_daisy ),";
}
else {
    my @in = ();
    push @in, "DAISY$scan_daisy";
    for (my $n = 1 + $sramchains; $n < $sramchains+$syncchains; $n++) {
        push @in, "SCAN_IN\[$n\]";
    }
    push @core, "    .SCAN_IN ( { ".join(",\n           ", reverse @in)." } ),";
}
$done{SCAN_IN}=1;
if ($syncchains == 1) {
    push @core, "    .SCAN_IDL ( SCAN_IDL[$sramchains] ),";
    push @core, "    .SCAN_OUT ( SCAN_OUT[$sramchains] ),";
}
else {
    my @in = ();
    for (my $n = $sramchains; $n < $sramchains+$syncchains; $n++) {
        push @in, "SCAN_IDL\[$n\]";
    }
    push @core, "    .SCAN_IDL ( { ".join(",\n           ", reverse @in)." } ),";
    @in = ();
    for (my $n = $sramchains; $n < $sramchains+$syncchains; $n++) {
        push @in, "SCAN_OUT\[$n\]";
    }
    push @core, "    .SCAN_OUT ( { ".join(",\n           ", reverse @in)." } ),";
}
$done{SCAN_OUT}=1;
$done{SCAN_IDL}=1;
push @core, "    .SCAN_MODE ( SCAN_MODE ),";
$done{SCAN_MODE}=1;
push @core, "    .SCAN_EN ( SCAN_EN ),";
$done{SCAN_EN}=1;
if ($sramchains) {
    my @list=();
    for (my $n = 0; $n < $sramchains; $n++) {
        push @list, "SCAN_IN\[$n\]";
    }
    push @core, sprintf "    .SCAN_SRAM_IN ( { ".join(",\n     ", reverse @list)." } ),\n";
    $done{SCAN_SRAM_IN}=1;
    @list=();
    for (my $n = 0; $n < $sramchains; $n++) {
        push @list, "SCAN_IDL\[$n\]";
    }
    push @core, sprintf "    .SCAN_SRAM_IDL ( { ".join(",\n     ", reverse @list)." } ),\n";
    $done{SCAN_SRAM_IDL}=1;
    @list=();
    for (my $n = 0; $n < $sramchains; $n++) {
        push @list, "SCAN_OUT\[$n\]";
    }
    push @core, sprintf "    .SCAN_SRAM_OUT ( { ".join(",\n     ", reverse @list)." } ),\n";
    $done{SCAN_SRAM_OUT}=1;
}
push @core, "    .CLK ( CLK ),";
$done{CLK}=1;
push @core, "    .RESET_IN_N ( CORE_RESET_N ),";
$done{RESET_IN_N}=1;
my @added=();
foreach my $port (sort keys %vcports) {
    $port =~ s/^ASYNC_//;
    next if defined ($done{$port});
    my $pport = cast2verilogrename($port);
    my $type = $vcports{$port};
    my $size=1;
    if ($type =~ /\[(\d+):0\]/) {
        $size = $1+1;
    }
    if (defined ($sync{$port})) {
        my $len=@{$sync{$port}};
        print STDERR "Error: internal mismatch between port lengths of $port($pport) ($len vs. $size)"
            if $len != $size;
        foreach my $p (@{$sync{$port}}) {
            if ($p eq "" or ($p =~ /^1'/)) {
                if ($vcports{$port} =~ /^output/) {
                    $p = "xopen$openndx";
                    $openndx++;
                }
                else {
                    $p = "1'b0";
                }
            }
        }
        push @added, "    .$pport ( { ".join(",\n            ", reverse @{$sync{$port}})." } )";
    }
    else {
        push @added, "    .$pport ( $pport )";
    }
}
push @core, join(",\n", @added);
push @core, "  );";
for (my $n = 1001; $n < $openndx; $n++) {
    print "  wire xopen$n;";
}
print join("\n", @core);
print "endmodule";

# write the top skeleton
error "Writing top $wrapper_top";
my $ccell = cellcast2cadence ($cell);
my $cwrapper_top = cellcast2cadence($wrapper_top);

foreach my $inc (@includes) {
    print "`include \"$inc\"";
}
print "\nmodule \\$cwrapper_top  (";
my @ports = keys %vtports;
foreach my $port (@ports) {
    $port = cast2verilogrename($port);
}
print "    ".join(",\n    ", @ports);
print "  );";

foreach my $vport (sort keys %vtports) {
    my $pport = cast2verilogrename($vport);
    print "  $vtports{$vport} $pport;";
}
print "  wire       cdc_reset_n, core_reset_n;";
print "  wire [7:0] scan_in;";
print "  wire [7:0] scan_out;";
print "  wire [7:0] scan_idl;";
print "  wire       scan_mode, scan_en, clk;";
print "";
print "  // the scan converter";
print "  \\lib.dft.converter.SYNC_CONVERTER_ARRAY_8.1051 sync_conv(";
print "    .CDC_RESET_N     (cdc_reset_n),";
print "    .CDC_SCAN_ACTIVE (CDC_SCAN_ACTIVE),";
print "    .CLK             (clk),";
print "    .CORE_RESET_N    (core_reset_n),";
print "    .EXT_CLK         (CLK),";
print "    .MODULE_RESET_N  ($MODULE_RESET_N),";
print "    .SCAN_EN         (scan_en),";
print "    .SCAN_IDL        (scan_idl),";
print "    .SCAN_IN         (scan_in),";
print "    .SCAN_MODE       (scan_mode),";
print "    .SCAN_OUT        (scan_out),";
print "    ._RESET          (CHIP_RESET_N),";
print "    .\\L[0].0         (SCAN_LS__d[0]),";
print "    .\\L[0].1         (SCAN_LS__d[1]),";
print "    .\\L[0].2         (SCAN_LS__d[2]),";
print "    .\\L[0].3         (SCAN_LS__d[3]),";
print "    .\\L[0].e         (SCAN_LS__e),";
print "    .\\R[0].0         (SCAN_RS__d[0]),";
print "    .\\R[0].1         (SCAN_RS__d[1]),";
print "    .\\R[0].2         (SCAN_RS__d[2]),";
print "    .\\R[0].3         (SCAN_RS__d[3]),";
print "    .\\R[0].e         (SCAN_RS__e)";
print "  );";
my $scan_daisy=0;
my @added=();
foreach my $cdc (sort keys %groups) {
    push @added, "    .${cdc}_LS__d (${cdc}_LS__d)";
    push @added, "    .${cdc}_LS__e (${cdc}_LS__e)";
    push @added, "    .${cdc}_RS__d (${cdc}_RS__d)";
    push @added, "    .${cdc}_RS__e (${cdc}_RS__e)";
    push @added, "    .${cdc}__d (${cdc}__d)";
    push @added, "    .${cdc}__e (${cdc}__e)";
}
my %done=();
my %done=( "GND" => 1, "_RESET" => 1, "CDC_SCAN_ACTIVE" => 1, "Vdd" => 1);
my $cmid_module = cellcast2cadence($mid_module);
print "  \\$cmid_module  mid(";
print "    .SCAN_IN ( scan_in ),";
$done{SCAN_IN}=1;
print "    .SCAN_OUT ( scan_out ),";
$done{SCAN_OUT}=1;
print "    .SCAN_MODE ( scan_mode ),";
$done{SCAN_MODE}=1;
print "    .SCAN_EN ( scan_en ),";
$done{SCAN_EN}=1;
print "    .SCAN_IDL ( scan_idl ),";
$done{SCAN_IDL}=1;
print "    .CLK ( clk ),";
$done{CLK}=1;
print "    .CORE_RESET_N ( core_reset_n ),";
print "    .CDC_RESET_N ( cdc_reset_n ),";
$done{CORE_RESET_N}=1;
$done{MODULE_RESET_N}=1;
# XY
foreach my $node (sort keys %channels) {
    my ($type,$dir)=@{$channels{$node}};
    next if $type ne "node";
    next if $done{$node};
    my $pport = cast2verilogrename($node);
    push @added, "    .$pport ($pport)";
    $done{$node}=1;
}
foreach my $port (keys %addednodes) {
    my $pport=cast2verilogrename($port);
    push @added, "    .$pport ($pport)" if ! $done{$port};
}
print join(",\n", sort @added);
# core
# core nodes
# core ASYNC translations to/from CDCs
print "  );";
print "endmodule";
select STDOUT;
close $fvlg;
# check core rtl ports
if (defined ($core_rtl)) {
    my %def=();
    $def{SCAN_IDL}=1;
    foreach my $port (@rtlports) {
        error ("Error: Port mismatch within $core_rtl: $port is missing")
            if (! defined $rtlports{$port});
        $def{$port}=1;
    }
    foreach my $port (sort keys %rtlports) {
        error ("Error: Port mismatch within $core_rtl: $port is missing")
            if (! defined $def{$port});
    }
    foreach my $port (sort keys %vcports) {
        my $cport=$port;
        $cport =~ s/\[/_l_/;
        $cport =~ s/\]/_r_/;
        $cport =~ s/\./_/g;
        if (defined ($rtlports{$cport})) {
            error("Error: Port $port in cast implied core does not match type $rtlports{$cport}(rtl) vs. $vcports{$port}(core)")
                if $rtlports{$cport} ne $vcports{$port};
        }
        else {
            error("Warning: Port $port in cast pass down but not in rtl") if ! $def{$port};
        }
    }
}
# write_nondefault_rules
my $fndr;
open ($fndr, ">$wrapper_top.ndr.def") or error("Error: Cannot open $wrapper_top.ndr.def");
error("Writing $wrapper_top.ndr.def") if $verbose;
print $fndr "VERSION 5.5 ;";
print $fndr "NAMESCASESENSITIVE ON ;";
print $fndr "DIVIDERCHAR \"/\" ;";
print $fndr "BUSBITCHARS \"<>\" ;";
print $fndr "DESIGN $wrapper_top ;";
my $fdat;
open $fdat, "<$wrapper_top.net.info";
my %net=();
my %cell=();
my %cast=();
while (<$fdat>) {
    chomp;
    my ($c,$i,$p,$n)=split;
    if (!defined ($cast{$c})) {
        $cast{$c} = `echo "$c" | fulcrum rename --type=cell --from=cadence --to=cast`;
        chomp $cast{$c};
    }
    $c = $cast{$c};
    $cell{$i}=$c;
    $n = "" if ! defined $n;
    $net{"$c $i $p"}=$n;
}
my $file;
my @ndr=();
foreach my $inst (sort keys %cell) {
    $file = "cast2def --cast-path='$cast_path' --format=def --outfile=/dev/stdout --cell='$cell{$inst}'|";
    my $fdef;
    print STDERR "Generating NDRs for $cell{$inst}" if $verbose;
    open ($fdef, "$file") or die;
    while (<$fdef>) {
        chomp;
        if (/^\s*-\s+/) {
            my @tokens=split(/\s+/);
            # code taken from cdc_def written by youxin. The reason for ignoring
            # these nodes is unknown.
            if((($tokens[2] eq "L.e")) ||
               (($tokens[2] eq "L.v")) ||
               (($tokens[2] =~ /^L\.d\[\d+\]$/)) ||
               (($tokens[2] eq "R.v")) ||
               (($tokens[2] eq "R.e")) ||
               (($tokens[2] =~ /^R\.d\[\d+\]$/)) ||
               ($tokens[2] eq "SCAN.IN") ||
               ($tokens[2] eq "SCAN.OUT") ||
               ($tokens[2] eq "SCAN.EN") ||
               ($tokens[2] eq "SCAN.MODE") ||
               ($tokens[2] eq "CLK") ||
               ($tokens[2] eq "_RESET") ||
               ($tokens[2] eq "Vdd") ||
               ($tokens[2] eq "t.M") ||
               ($tokens[2] eq "ac_tree.tree._r[0][0]") ||
               ($tokens[2] eq "debug_ack") ||
               (($tokens[2] =~ /dpu4_top\.Reset/)) ||
               (($tokens[2] =~ /dpu8\[\d+\]\.Reset/)) ||
               (($tokens[2] =~ /dpu4_top\.mbuf\.Reset/)) ||
               (($tokens[2] =~ /dpu4_top\.mbuf\._Reset/)) ||
               (($tokens[2] =~ /dpu4_top\.cbuf\.Reset/)) ||
               (($tokens[2] =~ /dpu4_top\.cbuf\._Reset/)) ||
               (($tokens[2] =~ /dpu8\[\d+\]\.mbuf\.Reset/)) ||
               (($tokens[2] =~ /dpu8\[\d+\]\.mbuf\._Reset/)) ||
               (($tokens[2] =~ /dpu8\[\d+\]\.cbuf\.Reset/)) ||
               (($tokens[2] =~ /dpu8\[\d+\]\.cbuf\._Reset/)) ||
               (($tokens[2] eq "t.S")) ||
               ($tokens[2] =~ /gnd/i)) {
                my $orig=$tokens[2];
                if (defined ($net{"$cell{$inst} $inst $tokens[2]"})) {
                    $tokens[2] = $net{"$cell{$inst} $inst $tokens[2]"};
                    push @ndr, "  ".join(" ", @tokens). " # removed by Youxin's code, because $orig"
                        if ($tokens[2] ne "" and $tokens[2] ne "1'b0");
                }
                else {
                    $tokens[2] = "$inst/$tokens[2]";
                    push @ndr, "  ".join(" ", @tokens). " # removed by Youxin's code, because $orig";
                }
            } elsif (defined ($net{"$cell{$inst} $inst $tokens[2]"})) {
                $tokens[2] = $net{"$cell{$inst} $inst $tokens[2]"};
                push @ndr, "  ".join(" ", @tokens)
                    if ($tokens[2] ne "" and $tokens[2] ne "1'b0");
            }
            else {
                $tokens[2] = "$inst/$tokens[2]";
                push @ndr, "  ".join(" ", @tokens);
            }
        }
    }
}
printf $fndr "NETS %d ;\n",$#ndr+1;
print $fndr join("\n", @ndr);
print $fndr "END NETS";
print $fndr "END DESIGN";
close $fndr;
if ($verbose or $warn or $err ) {
    print STDERR "$info Information, $warn Warning, $err Error messages.";
}
