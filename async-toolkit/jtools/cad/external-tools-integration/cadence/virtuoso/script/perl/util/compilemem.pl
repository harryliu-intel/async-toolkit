#!/usr/intel/bin/perl -l
# AAG
# $Id$
# $DateTime$

# TODO:
#   fix libs according to bug 11911
#   something wrong with p4 stuff in here
#   better handling of no p4 client

use strict;
use Getopt::Long;
use IPC::Open2;

my %ARGS = (
   "acroread" => "acroread",
   "asvm" => "on",
   "atf" => "off",
   "back_biasing" => "off",
   "bits" => 32,
   "bmux" => "off",
   "bus_notation" => "on",
   "check_instname" => "off",
   "corners" => "",
   "cust_comment" => "",
   "diodes" => "on",
   "dnw" => "off",
   "dpccm" => "on",
   "drive" => 4,
   "ema" => "on",
   "frequency" => 700,
   "horiz" => "met3",
   "left_bus_delim" => "[",
   "mux" => 1,
   "name_case" => "upper",
   "pin_space" => "0.0",
   "pipeline" => "off",
   "power_gating" => "off",
   "power_type" => "rings",
   "prefix" => "",
   "pwr_gnd_rename" => "",
   "rcols" => 1,
   "redundancy" => "off",
   "retention" => "on",
   "right_bus_delim" => "]",
   "ring_width_core" => "1.2",
   "ring_width_periphery" => "1.2",
   "rrows" => 0,
   "ser" => "none",
   "synopsys.libname" => "",
   "top_layer" => "",
   "vclef-fp.inst2ring" => "blockages",
   "vclef-fp.site_def" => "off",
   "vert" => "met4",
   "words" => 512,
   "wp_size" => 8,
   "write_mask" => "off",
   "write_thru" => "on",
);
my $verbose=0;

my $norun=0;
my $type="";
my %allnames;
my $user=`whoami`;
chomp $user;
my $change;
my $library="vendor.artisan.memory.sram";
my $libpath="vendor/artisan/memory/sram";
my $inspecfile;
my $instname;

my %compilerpath=(
   "RD" => "rf_2p_adv",
   "RS" => "rf_sp_adv",
   "SD" => "sram_dp_adv",
   "SS" => "sram_sp_adv",
);

my %in = (
    "SS" => "SRAM",
    "SD" => "SRAM",
    "RS" => "REGFILE",
    "RD" => "REGFILE",
);


my $compilerroot= "/mnt/fulcrum/local/vendors/artisan/tsmc65/aci";
my $castpath;
my $specpath;
my $sparpath;
my $dfIIpath;
my $client;
my $clientpath="";
my $interactive=0;
my $pdkroot="";

# NOTE: First in array is default option!

my %coptions=(
#    "acroread" => ["acroread"], # ??
    "asvm" => ["on","off"], # address setup violation modeling
    "atf" => ["off","on"], # Advanced Test Features (WBT and RDT)
    "back_biasing" => ["off"], # VPW and VNW pins added to control back bias
    "bits" => ["SS,SD=2..72","RS=8..144","RD=2..144"], # depends on mux width ***
    "bmux" => ["off","on"], # bist access mux
    "bus_notation" => ["on","off"], # use correct bus notation
    "check_instname" => ["off","on"], # needs to be off
    "corners" => ["RS,RD,SS,SD=ss_0p9v_125c,ff_1p1v_125c,ff_1p1v_0c,tt_1p0v_25c"],
    "cust_comment" => [""],
    "dpccm" => ["on","off"], # dual port only for modeling for clock colisions
    "diodes" => ["on","off"], # almost certainly needs to be on
    "drive" => ["SS,SD=6","RS,RD=4"],
    "ema" => ["on","off"],
    "frequency" => ["500..700"], # only used for power
    "horiz" => ["SS,SD=met2;met3;met4","RS,RD=met2;met3"],
#    "inside_ring_type" => ["VDD","GND"], # default??
    "left_bus_delim" => ["["], # obvious
    "mux" => ["SS=8;16;32","SD=4;8;16","RS,RD=1;2;4"],  # from compiler error report ***
    "name_case" => ["upper"],  # just use upper case
    "pin_space" => ["0.0"], # between pins of core and inner power ring
    "pipeline" => ["off","on"],
    "power_gating" => ["off","on"], # core and periphery are separated
    "power_type" => ["rings","ArtiGrid"], # use rings
    "prefix" => [""],
    "pwr_gnd_rename" => ["VDDPE:VDDPE,VDDCE:VDDCE,VSSE:GND"],
    "rcols" => ["SS=0;2;16","SD=0;2","RS,RD=1"],
    "redundancy" => ["off","on"],
    "retention" => ["on","off"], # on for power gating
    "right_bus_delim" => ["]"], # obvious
    "ring_width_core" => ["1.2"], # >= 1/2 width of power grid
    "ring_width_periphery" => ["1.2"], # >= 1/2 width of power grid
    "rrows" => [0,2,4],
#    "se_immunity" => ["off","on"], # add deep nwell for better alpha immunity
    "ser" => ["none","1bd1bd","2bd1bc"], # store error codes write_mask off
    "synopsys.libname" => ["*"], # should be changed, IMHO
    "top_layer" => ["SS,RD=m5-m9","SD=m5-9","RS=met5-9"],
    "vclef-fp.inst2ring" => ["blockages","pin"], # power is blockage or pin
    "vclef-fp.site_def" => ["off","on"], # does lef have a site definition?
    "vert" => ["SS,SD=met2;met3;met4","RS,RD=met2;met3"],
#    "wae" => ["RD,RS=off;on"], # register file only?
    "words" => ["SS=512..16384","SD=256..4096","RS=8..512","RD=8..256"],  # 512-16384, divisble by 2*mux ***
    "wp_size" => ["1..36"],
    "write_mask" => ["off","on"],
    "write_thru" => ["SS,SD,RS=off;on","RD=off"],
);

sub fixlib {
    my ($file,$cell) = @_;
    local *P;
    local $_;
    if (open (P, "<$file")) {
        my @lines = ();
        while (<P>) {
            chomp;
            push @lines, $_;
        }
        close P;
        open (P, ">$file");
        for (my $n = 0; $n <= $#lines; $n++) {
            $_=$lines[$n];
            if (/UNDEFINED/) {
                if (/voltage_map/) {
                    if (/,\s*0\s*\)/) {
                        $lines[$n] =~ s/UNDEFINED/GND/;
                    }
                    else {
                        $lines[$n] =~ s/UNDEFINED/VDDCE/;
                    }
                }
                if (/related_power_pin/ or /related_pg_pin/) {
                    $lines[$n] =~ s/UNDEFINED/VDDCE/;
                }
                if (/related_ground_pin/) {
                    $lines[$n] =~ s/UNDEFINED/GND/;
                }
            }
            while ($lines[$n] =~ /pg_pin\(/) {
                my $i = 0;
                my $undefined = "VDDCE";
                while (! ($lines[$n+$i] =~ /\x7d/)) {
                    $i++;
                    $undefined = "GND" if $lines[$n+$i] =~ /pg_type.*ground/;
                }
                for (my $j = 0; $j <= $i; $j++) {
                    $lines[$n+$j] =~ s/UNDEFINED/$undefined/;
                }
                $n += $i+1;
            }
#            print P $lines[$n];
        }
        print P join("\n", @lines);
        close P;
    }
}

sub setoptionsfromspecfile {
    my ($file) = @_;
    local *P, $_;
    if (open (P, "<$file")) {
        while (<P>) {
            chomp;
            next if /^#/;
            my ($arg,$value)=split(/=/,$_);
            if (defined ($ARGS{$arg})) {
                eval("\$ARGS{$arg}=\"$value\"");
            }
            else {
                eval("\$$arg=\"$value\"");
            }
        }
    }
    $type = substr($instname, 0, 1);
    my $x = $instname;
    $x =~ s/_R90//;
    $type .= substr($x,length($x)-2, 1);
}

sub check_options {
    my $err=0;
    foreach my $key (sort keys %coptions) {
        if (defined ($ARGS{$key})) {
            my $ok=0;
            my @ok=@{$coptions{$key}};
            if ($ok[0] eq "*") {
                $ok = 1;
            }
            else {
                foreach my $v (@ok) {
                    if ($v eq $ARGS{$key}) { $ok=1;}
                    elsif ($v =~ /^(\d+)\.\.(\d+)$/) {
                        $ok = 1 if $ARGS{$key} >= $1 and $ARGS{$key} <= $2;
                    }
                    elsif ($v =~ /=/) {
                        my ($ty,$val)=split(/=/,$v);
                        my @ty = split(/,/,$ty);
                        foreach my $t (@ty) {
                            if ($t eq $type) {
                                if ($val =~ /^(\d+)\.\.(\d+)$/) {
                                    $ok = 1 if $ARGS{$key} >= $1 and $ARGS{$key} <= $2;
                                }
                                elsif ($val =~ /;/) {
                                    my @v=split(/;/,$val);
                                    foreach my $vx (@v) {
                                        $ok=1 if ($ARGS{$key} eq $vx);
                                    }
                                }
                                elsif ($ARGS{$key} ne $val and $val ne "*") {
                                    print STDERR "Warning: $key ($ARGS{$key}) changed to $val"
                                        if $verbose and $ARGS{$key} ne "";
                                    $ARGS{$key} = $val;
                                    $ok=1;
                                }
                                else {
                                    $ok=1;
                                }
                            }
                        }
                    }
                }
            }
            if ($ok == 0 and $ok[0] ne "*") {
                if ($#ok == 0 and ! ($ok[0] =~ /^\d+\.\.\d+$/)) {
                    print STDERR "Warning: $key ($ARGS{$key}) changed to '$ok[0]'"
                        if $verbose and $ARGS{$key} ne "";
                    $ARGS{$key}=$ok[0];
                }
                else {
                    print STDERR "Error: $key ($ARGS{$key}) is not one of '".join("','", @ok)."'";
                    $err++;
                }
            }
        }
    }
    if ($ARGS{words} % ($ARGS{mux}*2) != 0 ) {
        print STDERR "Words must be divisible by 2*mux (2 * $ARGS{mux})";
        $err++;
    }
    if ($type eq "RS" and $ARGS{mux} == 1 and ($ARGS{wp_size} % 4 != 0)) {
        print STDERR "wp_size must be divisible by 4 for mux = 1 for Single Port Register File";
        $err++;
    }
    elsif ($type eq "RS" and $ARGS{mux} == 2 and ($ARGS{wp_size} % 2 != 0)) {
        print STDERR "wp_size must be divisible by 2 for mux = 2 for Single Port Register File";
        $err++;
    }
    if ($ARGS{wp_size} < 1 or $ARGS{wp_size} > 36 or $ARGS{wp_size} > $ARGS{bits}-1) {
        print STDERR "wp_size out of range ( 1 < wp_size < min(36,bits-1) )";
        $err++;
    }
    if ($err) {
        print STDERR "Failed with $err errors";
        exit 1;
    }
}

my %options = (
   "bits=i" => \$ARGS{bits},
   "change=i" => \$change,
   "drive=i" => \$ARGS{drive},
   "frequency=i" => \$ARGS{frequency},
   "mux=i" => \$ARGS{mux},
   "rcols=i" => \$ARGS{rcols},
   "rrows=i" => \$ARGS{rrows},
   "words=i" => \$ARGS{words},
   "wp_size=i" => \$ARGS{wp_size},
   "write_mask=s" => \$ARGS{write_mask},
   "write_thru=s" => \$ARGS{write_thru},
   "type=s" => \$type,
   "ser=s" => \$ARGS{ser},
   "compiler-root=s" => \$compilerroot,
#   "vert=s" => \$ARGS{vert},
#   "horiz=s" => \$ARGS{horiz},
   "norun" => \$norun,
   "verbose" => \$verbose,
   "client=s" => \$client,
   "interactive" => \$interactive,
   "fulcrum-pdk-root=s" => \$pdkroot,
   "spec=s" => \$inspecfile,
);

my %optdes = (
    "compiler-root" => "[$compilerroot]",
    "norun" => " show command, do not run all of the long running tools",
    "type" => " one of SS,SD,RS,RD",
    "change" => " preset p4 change number",
    "client" => " p4 client name",
    "interactive" => " run gui with preset defaults",
);

sub usage {
    my ($msg)=@_;
    print STDERR "$msg" if defined $msg;
    my $ml=0;
    foreach my $opt (sort keys %options) {
        my ($o,$v)=split(/=/,$opt);
        $ml = length($o) if $ml < length($o);
    }
    $ml += 2;
    print STDERR "Usage: compilemem [options]";
    print STDERR " Options:";
    foreach my $opt (sort keys %options) {
        my ($o,$v)=split(/=/,$opt);
        if ($v eq "i") {
            $v = "integer";
        }
        elsif ($v eq "s") {
            $v = "string";
        }
        else {
            $v = "flag";
        }
        if (defined ($optdes{$o})) {
            $v .= " $optdes{$o}";
        }
        elsif (defined ($coptions{$o})) {
            $v .= " (".join(",", @{$coptions{$o}}).")";
        }
        $v =~ s/  */ /g;
        printf STDERR "   %-*.*s : %s\n", $ml,$ml, "--$o", $v;
    }
    exit 1;
}

GetOptions ( %options ) or usage();

if (defined($inspecfile) and -f "$inspecfile") {
    setoptionsfromspecfile($inspecfile);
}

$type =~ tr/a-z/A-Z/;
usage ("Unknown type $type") if ! defined $in{$type};

$instname="$in{$type}$ARGS{words}_$ARGS{bits}_M$ARGS{mux}_".substr($type,1,1)."P" if $instname eq "";
if ($type =~ /^R/) {
    $ARGS{horiz}="met2" if ($ARGS{horiz} eq "met4");
    $ARGS{vert}="met2" if ($ARGS{vert} eq "met4");
}
if (defined $client) { # check client exists
    my $ok=0;
    open (P, "p4 -u system clients |");
    while (<P>) {
        chomp;
        my @f=split;
        if ($f[1] eq $client) {
            $ok=1;
            last;
        }
    }
    close P;
    if (!$ok) {
        undef $client;
        warn ("Client $client does not exist.");
    }
}
if ($verbose) {
    if (defined ($client)) {
        print STDERR "Client $client ok";
    }
    else {
        print STDERR "Client not specified";
    }
}
if (defined $client) {
    if (! defined ($change) or ! ($change =~ /^\d+$/)) {
        my $pid = open2 (\*RD, \*WR, "p4 -c $client -u $user change -i");
        usage ("Cannot create p4 change number") if !$pid;
        my $changespec=<<EC;
Change: new
Client: $client
User:   $user
Status: new
Description:
    p4 change nr for $instname
EC
        print WR $changespec;
        close WR;
        $change = <RD>;
        chomp $change;
        if ($change =~ /Change (\d+) created/) {
            $change = $1;
            print STDERR "Created change number $change";
        }
        else {
            kill $pid;
            waitpid $pid, 0;
            print STDERR $changespec;
            usage  ("Change number not created: $change");
        }
        kill $pid;
        waitpid $pid, 0;
    }
    open (P, "p4 -c $client client -o |");
    while (<P>) {
        chomp;
        if (/^Root:/) {
            my @f=split;
            $clientpath=$f[1];
        }
        if (m://depot/hw/cast: and ! defined $castpath) {
            s/^[ \t]*//;
            my @f = split;
            $f[1] =~ s://$client/:$clientpath/:;
            $castpath=$f[1];
            $castpath =~ s:/cast/.*:/cast:;
        }
        if (m://depot/hw/layout/tsmc\d\d/dfII: and ! defined $dfIIpath) {
            s/^[ \t]*//;
            my @f = split;
            $f[1] =~ s://$client/:$clientpath/:;
            $dfIIpath=$f[1];
            $dfIIpath =~ s:/dfII/.*:/dfII:;
        }
        if (m://depot/hw/layout/tsmc\d\d/spar: and ! defined $sparpath) {
            s/^[ \t]*//;
            my @f = split;
            $f[1] =~ s://$client/:$clientpath/:;
            $sparpath=$f[1];
            $sparpath =~ s:/spar/.*:/spar:;
        }
        if (m://depot/hw/layout/tsmc\d\d/spec: and ! defined $specpath) {
            s/^[ \t]*//;
            my @f = split;
            $f[1] =~ s://$client/:$clientpath/:;
            $specpath=$f[1];
            $specpath =~ s:/spec/.*:/spec:;
        }
    }
    my $ok=1;
    if (! defined $castpath) {
        print STDERR "Cannot find cast path in $client";
        $ok=0;
    }
    if (! defined $dfIIpath) {
        print STDERR "Cannot find dfII path in $client";
        $ok=0;
    }
    if (! defined $specpath) {
        print STDERR "Cannot find spec path in $client";
        $ok=0;
    }
    if (! defined $sparpath) {
        print STDERR "Cannot find spar path in $client";
        $ok=0;
    }
    usage() if ! $ok;
    if ($verbose) {
        print STDERR "castpath: $castpath";
        print STDERR "specpath: $specpath";
        print STDERR "dfIIpath: $dfIIpath";
        print STDERR "sparpath: $sparpath";
    }
}

usage "Type must be specified." if length($type) != 2;
$type =~ tr/a-z/A-Z/;
$type = substr($type,0,2);
#open (P, "<params");
#while (<P>) {
#    chomp;
#    s/=(.*)//;
#    my $val=$1;
#    $val =~ s/ *#.*//;
#    my $name=$_;
#    $allnames{$name}=$val;
#    if ($val =~ /\x7b/) {
#        $val =~ s/\x7b(.*)\x7d/$1/;
#        my @f=split(/,/,$val);
#        if ($f[0] ne "??") {
#            $allnames{$name}=$val;
#        }
#    }
#    if (defined($ARGS{$name})) {
#        $allnames{$name}=$ARGS{$name};
#    }
#}
#close P;
if (! defined ($compilerpath{$type})) {
    usage "Illegal type $type";
}
if ($type =~ /^S/) {
    $ARGS{"synopsys.libname"} = "vendor.artisan.memory.sram.$instname.1";
}
else {
    $ARGS{"synopsys.libname"} = "vendor.artisan.memory.regfile.$instname.1";
}
check_options();

# write the spec files
open (S, ">$instname.spec") or die "Cannot write spec file.";
print S "#user spec file from Fulcrum compilemem";
my $dt=localtime(time);
print S "#$dt";
print S "instname=$instname";

$ARGS{check_instname}="off";
foreach my $name (sort keys %coptions) {
    next if $name eq "instname";
    if ($type =~ /S$/ and $name eq "dpccm") { next;}
    if ($type ne "SD" and $name eq "dnw") { next;}
    if (defined ($ARGS{$name})) {
        print S "$name=$ARGS{$name}";
    }
    elsif (defined ($coptions{$name})) {
        my $t = "-$name '${@{$coptions{$name}}}[0]'";
        print S "$name=${@{$coptions{$name}}}[0]";
        print STDERR "Using internal $t";
    }
    else {
        print STDERR "Using default -$name=$allnames{$name}";
        print S "$name=$allnames{$name}";
    }
}
close S;

if ($interactive) {
    print STDERR "Running interactive, be sure to save the spec file! (Utilities->Write Spec)";
    system ("$compilerroot/$compilerpath{$type}/bin/$compilerpath{$type} -spec '$instname.spec'");
    open (S, "<$instname.spec");
    while (<S>) {
        chomp;
        next if (/^#/);
        my ($name,$value)=split(/=/,$_);
        $ARGS{$name}=$value if $name ne "pwr_gnd_rename";
    }
    close S;
    rename "$instname.spec", "$instname.spec.orig";
    my $newinstname="$in{$type}$ARGS{words}_$ARGS{bits}_M$ARGS{mux}_".substr($type,1,1)."P";
    if ($newinstname ne $instname) {
        print STDERR "Warning: Changing instname to $newinstname";
        $instname=$newinstname;
    }
    print STDERR "Done..";
}


#must re-write pwr_gnd_rename not preserved.
open (S, ">$instname.spec") or die "Cannot write spec file.";
print S "#user spec file from Fulcrum compilemem";
my $dt=localtime(time);
print S "#$dt";
print S "instname=$instname";

$ARGS{check_instname}="off";
foreach my $name (sort keys %coptions) {
    next if $name eq "instname";
    if ($type =~ /S$/ and $name eq "dpccm") { next;}
    if ($type ne "SD" and $name eq "dnw") { next;}
    if (defined ($ARGS{$name})) {
        print S "$name=$ARGS{$name}";
    }
    elsif (defined ($coptions{$name})) {
        my $t = "-$name '${@{$coptions{$name}}}[0]'";
        print S "$name=${@{$coptions{$name}}}[0]";
        print STDERR "Using internal $t";
    }
    else {
        print STDERR "Using default -$name=$allnames{$name}";
        print S "$name=$allnames{$name}";
    }
}
close S;

open (S, ">${instname}_R90.spec") or die "Cannot write spec file.";
print S "#user spec file from Fulcrum compilemem";
my $dt=localtime(time);
print S "#$dt";
print S "instname=${instname}_R90";

if ($type =~ /^S/) {
    $ARGS{"synopsys.libname"} = "vendor.artisan.memory.sram.${instname}_R90.1";
}
else {
    $ARGS{"synopsys.libname"} = "vendor.artisan.memory.regfile.${instname}_R90.1";
}
$ARGS{check_instname}="off";
foreach my $name (sort keys %coptions) {
    next if $name eq "instname";
    if ($type =~ /S$/ and $name eq "dpccm") { next;}
    if ($type ne "SD" and $name eq "dnw") { next;}
    if (defined ($ARGS{$name})) {
        if ($name eq "horiz") {
            print S "vert=$ARGS{$name}";
        }
        elsif ($name eq "vert") {
            print S "horiz=$ARGS{$name}";
        }
        else {
            print S "$name=$ARGS{$name}";
        }
    }
    elsif (defined ($coptions{$name})) {
        if ($name eq "horiz") {
            print S "vert=${@{$coptions{$name}}}[0]";
        }
        elsif ($name eq "vert") {
            print S "horiz=${@{$coptions{$name}}}[0]";
        }
        else {
            print S "$name=${@{$coptions{$name}}}[0]";
        }
        print STDERR "Using internal $name=${@{$coptions{$name}}}[0]";
    }
    else {
        if ($name eq "horiz") {
            print S "vert=$allnames{$name}";
        }
        elsif ($name eq "vert") {
            print S "horiz=$allnames{$name}";
        }
        else {
            print S "$name=$allnames{$name}";
        }
        print STDERR "Using default -$name=$allnames{$name}";
    }
}
close S;

my $cmd="$compilerroot/$compilerpath{$type}/bin/$compilerpath{$type} all -spec '$instname.spec'";
print $cmd if $verbose;
my $err=0;
if (! $norun) {
    open(C, "$cmd 2>\&1 |");
    while (<C>) {
        chomp;
        $err++ if /Error/;
        print if /Error/ or $verbose;
    }
    close C;
}
die("Run failed") if $err;
$cmd="$compilerroot/$compilerpath{$type}/bin/$compilerpath{$type} all -spec '${instname}_R90.spec'";
print $cmd if $verbose;
my $err=0;
if (! $norun) {
    open(C, "$cmd 2>\&1 |");
    while (<C>) {
        chomp;
        $err++ if /Error/;
        print if /Error/ or $verbose;
    }
    close C;
}
die("Run failed") if $err;
my $changecount=0;
my $sparlibpath=$libpath;
$sparlibpath =~ s:/[^/]+$::;
if ($instname =~ /^REG/) {
    $library =~ s/sram/regfile/;
    $libpath =~ s/sram/regfile/;
}
foreach my $sfx ("", "_R90") {
    my $targetdir="$sparpath/$sparlibpath/${instname}$sfx";
    `mkdir -p "$targetdir"`;
    foreach my $filetype (".dat", ".cdl", ".gds2", ".ps", ".spec", ".v", ".vclef", "_ff_1p1v_125c_syn.lib", "_ff_1p1v_0c_syn.lib", "_ss_0p9v_125c_syn.lib", "_tt_1p0v_25c_syn.lib") {
        my $sourcefile="$instname$sfx$filetype";
        my $targetfile="$targetdir/$instname$sfx$filetype";
        my $p4target=$targetfile;
        $p4target =~ s/#/%2e/g;
        if ( -s "$sourcefile") {
            if ($sourcefile =~ /\.lib/) {
                fixlib ($sourcefile,"${instname}$sfx");
            }
            if ($sourcefile =~ /\.vclef$/) {
                my $fl;
                my @lines;
                if (open ($fl, "<$sourcefile")) {
                    while (<$fl>) {
                        chomp;
                        next if /NAMESCASESENSITIVE/;
                        if (/FOREIGN/) {
                            my $ln=$_;
                            $ln =~ s/^\s+//;
                            my @f=split;
                            next if ($#f < 3);
                        }
                        push @lines, $_
                    }
                    close $fl;
                    open ($fl, ">$sourcefile");
                    print $fl join("\n", @lines);
                    close $fl;
                }
            }
            my $edit=0;
            if (defined ($client) and -f "$targetfile" and ! -w "$targetfile") {
                print STDERR "p4 -c $client edit  -c $change \"$p4target\"";
                `p4 -c $client edit  -c $change "$p4target"`;
                unlink $targetfile;
            }
            `/bin/cp -p "$sourcefile" "$targetfile"`;
            print STDERR "p4 -c $client add  -c $change \"$p4target\"";
            `p4 -c $client add  -c $change "$p4target"` if defined($client);
            $changecount++;
        }
        else {
            print STDERR "$sourcefile does not exist";
        }
    }
}
if (! $changecount and defined($client) and $change =~ /^\d+$/) {
    print STDERR "Deleting p4 change $change" if $verbose;
    `p4 -c $client -u $user change -d $change`;
}
#elsif ( $norun and $change =~ /^\d+$/ and defined $client) {
#    `p4 -c $client -u $user revert -c $change //depot/...`;
#    `p4 -c $client -u $user change -d $change`;
#
#}
#print STDERR "Files changed:";
#`p4 -c $client -u $user change -o $change | grep //depot 1>\&2`;
#print STDERR "Run the following commands:";
#print STDERR "p4 -c $client -u $user submit -c $change";
my $verbosearg="";
$verbosearg = "--verbose" if $verbose;
my $refineparent="SRAM";
foreach my $sfx ("", "_R90") {
print STDERR "importSR $verbosearg --refinement-parent $refineparent --fulcrum-pdk-root '$pdkroot' --change '$change' --client '$client' --dfII-dir '$dfIIpath' --implied VDDPE --gnd-node GND --vdd-node VDDCE --sub-type 1 --lib '$library' --castpath '$castpath' --specpath '$specpath' --merge '$sparpath/$sparlibpath/${instname}$sfx/${instname}$sfx.cdl' '$sparpath/$sparlibpath/${instname}$sfx/${instname}$sfx.gds2'" if $verbose;
`importSR $verbosearg --refinement-parent $refineparent --fulcrum-pdk-root '$pdkroot' --change '$change' --client '$client' --dfII-dir '$dfIIpath' --implied VDDPE --gnd-node GND --vdd-node VDDCE --sub-type 1 --lib '$library' --castpath '$castpath' --specpath '$specpath' --merge '$sparpath/$sparlibpath/${instname}$sfx/${instname}$sfx.cdl' '$sparpath/$sparlibpath/${instname}$sfx/${instname}$sfx.gds2'` if ! $norun;
}
# add the generation of .db files
foreach my $sfx ("", "_R90") {
    my $targetdir="$sparpath/$sparlibpath/${instname}$sfx";
    `mkdir -p "$targetdir"`;
    foreach my $filetype ("_ff_1p1v_125c_syn.lib", "_ff_1p1v_0c_syn.lib", "_ss_0p9v_125c_syn.lib", "_tt_1p0v_25c_syn.lib") {
        my $sourcefile="$instname$sfx$filetype";
        my $targetfile="$targetdir/$instname$sfx$filetype";
        my $dbsourcefile = $sourcefile;
        $dbsourcefile =~ s/\.lib$/.db/;
        $targetfile =~ s/\.lib$/.db/;
        `compile_lib "$sourcefile" "$dbsourcefile"`;
        if ( -s $dbsourcefile ) {
            my $edit=0;
            if (defined ($client) and -f "$targetfile" and ! -w "$targetfile") {
                print STDERR "p4 -c $client edit  -c $change \"$targetfile\"";
                `p4 -c $client edit  -c $change "$targetfile"`;
                unlink $targetfile;
                $edit=1;
            }
            `/bin/cp -p "$dbsourcefile" "$targetfile"`;
            print STDERR "p4 -c $client add  -c $change \"$targetfile\"" if ! $edit;
            `p4 -c $client add  -c $change "$targetfile"` if defined($client) and ! $edit;
            $changecount++;
        }
        else {
            print STDERR "$sourcefile does not exist";
        }
    }
}
