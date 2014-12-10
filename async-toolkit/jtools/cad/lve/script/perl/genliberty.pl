#!/usr/intel/bin/perl -w
# AAG
# $Id$
# $DateTime$

use Getopt::Long;
use IPC::Open2;
use strict;

#
# Substitutes a variable "var" (e.g. "i") in an index spec (e.g. "[i,j]")
# with a particular value "val" (e.g. "3").
#
sub substitute_path_variable {
    my ($index,$var,$val) = @_;
    $index =~ s/([\W\[])$var([\W\]])/$1$val$2/g;
    return $index;
}
 
sub array_clock {
    my ($clk,$pin)=@_;
    if ($clk =~ /\[/ and $pin =~ /\[/) {
        $clk =~ m/\[(\d+)\]/;
        my $clkarr=$1;
        $pin =~ m/\[(\d+)\]/;
        my $pinarr=$1;
        $clk =~ s/\[$clkarr\]/[$pinarr]/;
    }
    $clk;
}

my %specpins;

my %validenv = (
   "ASSURA_SCRIPT" => 1,
   "AUTOLOAD_DIRS" => 1,
   "CLASSPATH" => 1,
   "FULCRUMCONFIG" => 1,
   "FULCRUM_PACKAGE_ROOT" => 1,
   "FULCRUM_PDK_ROOT" => 1,
   "GROUP" => 1,
   "HERC_SCRIPT" => 1,
   "HOME" => 1,
   "HSM_SCRIPT" => 1,
   "HSP_SCRIPT" => 1,
   "ICC_SCRIPT" => 1,
   "IC_SCRIPT" => 1,
   "IUS_SCRIPT" => 1,
   "LANG" => 1,
   "LC_COLLATE" => 1,
   "LDV_SCRIPT" => 1,
   "LD_LIBRARY_PATH" => 1,
   "LM_LICENSE_FILE" => 1,
   "LOGNAME" => 1,
   "NASSDA_WAIT_LICENSE" => 1,
   "P4CONFIG" => 1,
   "P4EDITOR" => 1,
   "P4PORT" => 1,
   "PATH" => 1,
   "PERL5LIB" => 1,
   "RC_SCRIPT" => 1,
   "SEV_SCRIPT" => 1,
   "SOC_SCRIPT" => 1,
   "SPB_SCRIPT" => 1,
   "STAR_SCRIPT" => 1,
   "SUPPORTED" => 1,
   "UI_DIRS" => 1,
   "USER" => 1,
   "VENDOR" => 1,
   "VIRTUOSO_BIN" => 1,
   "VIRTUOSO_HOME" => 1,
);

#
# Reassigns all node indices in a path with some other value determined
# by the funcref function pointer.
#
sub reassign_path_indices {
    my $pathref = shift;
    my $funcref = shift;
    my @newpath = ();
    foreach my $node (@{$pathref}) {
        my $newnode = $node;
        my $offset = 0;
        my $i = 0;
        do {
            $i = index $newnode, "[", $offset;
            if ($i >= 0) {
                my $j = index $newnode, "]", $i;
                die "Bad path node name '$newnode'.\n" if ($j == -1);
                my $index = substr($newnode, $i, $j-$i+1);
                substr($newnode, $i, $j-$i+1) = &{$funcref}($index,@_);
            }
            $offset = $i+1;
        }
        while ($i != -1);
        #print "Evaluated $node to $newnode\n";
        push @newpath, $newnode;
    }
    @{$pathref} = @newpath;
}
 
#
# For a given numeric index spec (e.g. "[10/2,4*2]"), evaluates the 
# expressions to single numbers (e.g. "[5,8]").
#
sub evaluate_path_indices {
    my ($index) = @_;
    $index =~ s/[\[\]]//g;
    my $new_index = "";
    foreach my $term (split(/\,/,$index)) {
        $new_index .= eval "int($term)";
        $new_index .= ",";
    }
    $new_index =~ s/,$//;
    return "[$new_index]";
}


#
# Copies path reference to a new path w/ specified orientation (0:dn,1:up)
#
sub orient_path {
    my ($path,$dir) = @_;
    my @newpath = @{$path};
    my @dirstr = ("-","+");
    for my $i (0..$#newpath) {
        $newpath[$i] =~ s/[-+]?$/$dirstr[$dir]/;
        $dir = 1-$dir;
    }
    return \@newpath;
}

sub expand_path {
    my ($pname,$path) = @_;
    my @expathlist = ();
    my $pbase = "";
    my $bidir = 0;
    $bidir = 1 if (${$path}[0] !~ /[-+]$/);
    if ($pname =~ /^[^\[]+$/) {
        $pbase = $pname;
        if (!$bidir) {
            push @expathlist, [$pname,$path];
        }
        else {
            push @expathlist, ["${pname}_up",orient_path($path,1)];
            push @expathlist, ["${pname}_dn",orient_path($path,0)];
        }
    }
    elsif ($pname =~ /^([^\[]+)\[([^\]]+)\]$/) {
        # pbase[i:li..hi,j:lj..hj,...]
        $pbase = $1;
        my @pranges = split(/,/,$2);
        # Determine variable names and lo/hi bounds of each loop index
        my $numpaths = 1;
        my @ranges = ();
        foreach my $prange (@pranges) {
            if ($prange =~ /^(\w+):(\d+)\.\.(\d+)$/) {
                push @ranges, [ $1, $2, $3 ];
                $numpaths *= ($3-$2+1);
            }
            elsif ($prange =~ /^(\w+):(\d+)$/) {
                push @ranges, [ $1, 0, $2-1 ];
                $numpaths *= $2;
            }
            else {
                warn "Bad path range in $pname. Skipping.\n";
            }
        }
        # Expand paths
        for my $i (0..$numpaths-1) {
            my $divisor = 1;
            my @expath = @{$path};
            my $expelem = "[";
            for my $range (@ranges) {
                my ($var,$lo,$hi) = @{$range};
                my $val = $lo + ($i/$divisor)%($hi-$lo+1);
                $divisor *= ($hi-$lo+1);
                reassign_path_indices(\@expath,\&substitute_path_variable,
                                      $var,$val);
                $expelem .= "$val,";
            }
            reassign_path_indices(\@expath,\&evaluate_path_indices);
            $expelem =~ s/,$/\]/;
            if (!$bidir) {
                push @expathlist, [$pbase . $expelem,\@expath];
            }
            else {
                push @expathlist, ["${pbase}_up$expelem",
                                   orient_path(\@expath,1)];
                push @expathlist, ["${pbase}_dn$expelem",
                                   orient_path(\@expath,0)];
            }
            #print "Expanded $expname: @expath\n";
        }
    }
    else {
        warn "Bad path name $pname. Skipping.\n";
    }
    return ($pbase,@expathlist);
}
my $cachedir = "cache";
mkdir "$cachedir" if ( ! -d "$cachedir");
$"=",";
my $datestr;
my $year;
{
    my @f=localtime(time);
    $datestr=localtime(time);
    $year=$f[5]+1900;
}
my %writtenpin;

delete $ENV{DISPLAY};

sub checkqueue {
    my ($name)=@_;
    my $count=0;
    local($_,*P);
    open (P, "qstat |");
    while (<P>) {
        my @f=split;
        if ($#f > 1 and $f[2] eq $name) {
            $count++;
        }
    }
    $count;
}

$|=1;

#-------------------------------------------------------------------------------
#       Program usage
#-------------------------------------------------------------------------------
my $voltage = 1.08;
my $temp = 125;
my $corner = "tt";
my $Gpvt = sprintf "%s_%s_%s", $voltage, $temp, $corner;
my %libmode = (
    "tt" => "typical",
    "ff" => "min",
    "ss" => "max",
    "" => "fulcrum",
);
my %opcond = (
    "tt" => "slow",
    "ff" => "fast",
    "ss" => "slow",
    "" => "slow",
);

# if you change this, my must change get_delay_w_slew function too!
my $input_threshold_pct_fall="50.0";
my $input_threshold_pct_rise="50.0";
my $output_threshold_pct_fall="50.0";
my $output_threshold_pct_rise="50.0";

# place this at the beginning of the lib file.
my $lib_header = <<"HEADER";
/* \$Id\$
 * Copyright (c) $year Fulcrum Microsystems, Inc. All Rights Reserved.
 *
 * CONFIDENTIAL AND PROPRIETARY DATA OF Fulcrum Microsystems, Inc.
 *
 * This file contains valuable trade secrets and proprietary information
 * of Fulcrum Microsystems, Inc., and is protected by U.S. and
 * international laws and/or treaties.
 *
 * The copyright notice(s) in this file does not indicate actual or intended
 * publication of this file.
 *
 * created $datestr
 *
 * process (\$corner), voltage (\$voltageV), temp (\$temp)
 *
 * corner: \$voltage_\$temp_\$corner
 */
/* table model: cell-delay */
library(\$libmode) {
 
  /* general attributes */
  delay_model : table_lookup;
  in_place_swap_mode : match_footprint;
  library_features(report_delay_calculation);
 
  /* documentation attributes */
  revision : 1.3;
  date : "$datestr";
  comment : "Copyright (c) $year Fulcrum Microsystems, Inc. All Rights Reserved."
 
  /* unit attributes */
  time_unit : "1ns";
  voltage_unit : "1V";
  current_unit : "1mA";
  capacitive_load_unit (1.0,pf);
  pulling_resistance_unit : "1kohm";
  leakage_power_unit : "1pW";
  /* operation conditions */
  nom_process     : 1;
  nom_temperature : \$temp;
  nom_voltage     : \$voltage;
  operating_conditions(\$opcond) {
    process     : 1;
    temperature : \$temp;
    voltage     : \$voltage;
    tree_type   : balanced_tree
  }
  default_operating_conditions : \$opcond;
                                                                                                   
  /* threshold definitions */
  slew_lower_threshold_pct_fall : 33.3;
  slew_upper_threshold_pct_fall : 66.6;
  slew_lower_threshold_pct_rise : 33.3;
  slew_upper_threshold_pct_rise : 66.6;
  input_threshold_pct_fall      : INPUT_THRESHOLD_PCT_FALL;
  input_threshold_pct_rise      : INPUT_THRESHOLD_PCT_RISE;
  output_threshold_pct_fall     : OUTPUT_THRESHOLD_PCT_FALL;
  output_threshold_pct_rise     : OUTPUT_THRESHOLD_PCT_RISE;

  /* default attributes */
  default_leakage_power_density : 0.0;
  slew_derate_from_library      : 1.0;
  default_cell_leakage_power    : 0.0;
  default_fanout_load           : 1.0;
  default_output_pin_cap        : 0.0;
  default_inout_pin_cap         : 0.00158;
  default_input_pin_cap         : 0.00158;
  default_max_transition        : 1.02;

HEADER

# place this at the end of the lib file
my $lib_trailer = <<"TRAILER";
  /* these are just place holders to make lib reading work */
  cell(INV) { 
    area : 1;
    pin("A") {
      direction : input;
    }
    pin("Y") {
      direction : output;
      function : "(!A)";
    }
  }
  cell(NAND2) { 
    area : 1;
    pin("A") {
      direction : input;
    }
    pin("B") {
      direction : input;
    }
    pin("Y") {
      direction : output;
      function : "(!(A B))";
    }
  }
TRAILER

my @slew_rates;
my @cap_values;
my $delayTau;
my $cell_name  = "";
my $useqsub=10;
my $qsub_options="-p -2";
my $qsubarch="x86_64";
chomp $qsubarch;

# set default values for some args not set above

#@slew_rates = ("10", "16", "28", "44", "76", "138", "264", "516", "1020");
@cap_values = ("0.79f", "2.054f", "4.74f", "10.112f", "20.856f", "42.186f", "85.32f");
#@slew_rates = ("10","16","28", "44", "76", "138", "264", "516", "1020");
@slew_rates = ("28", "44", "76", "138", "264", "516", "1020");
#@cap_values = ("0.00079e-12");
#$delayTau = "10,25,64,199,745,1472";
$delayTau = "10,16,25,40,64,110,199,381,745,1472";

sub convertcaps {
    foreach my $n (0..$#cap_values) {
        my $cp = suffixcvt ($cap_values[$n]);
        $cap_values[$n] = $cp;
    }
}

my $usage_string = <<"USAGE";

  --cell=<cell_name>     Cell name (e.g. core.nexus.nevada.modules.CDC.1000)
  --cast-dir=<dir>       Cast directory (no default)
  --cast-path=<dir>      Cast directory (alternative arg for cast-dir)
  --tasks=<list>         list is or or more of sweep, pathalyze, lib

Grid Options
  [--qsub=n]             number of concurrent qsub jobs, 0=do not use qsub
  [--qsub-options=<list> options to pass to qsub, default '-p -2'

Used by more than one task
  [--base-dir=<dir>]     Directory to output info. (Default is \$PWD)
  [--cap-values=<list>   List of Cap Load values, (in Fd), default:
                            @cap_values
  [--config=<file>]      Include config file
  [--include=<file>]      Include config file
  [--noexecute]          Do not actually execute anything important except lib
  [--slew-rates=<list>]  List of slew rates in pS, default:
                           @slew_rates
  [--voltage=f]          Set voltage (float)
  [--corner=s]           Set corner (ss, ff, or tt)
  [--temp=f]             Set temperature (float)

LVE Sweep Options
  [--dfII-dir=<dir>]     Layout subtype spec directory (no default)
  [--spec-dir=<dir>]     Directory to new spec tree.
  [--lve-args=string]    LVE args in addition to standard
  [--lve-mode=<mode>]    LVE Mode, nogeometry, estimated, extracted: default estimated
  [--lve-view=<view>]    LVE View, floorplan, layout: default floorplan
  [--delayTau=<list>]    --delayTau arg for lve sweep (in pS). default:
                           $delayTau
Pathalyze Options
  [--lve-dir=<dir>]      Alint results directory for subcells
  [--spec-file=file]     Spec file for Pathalyze
  [--fulcrum-pdk-root=<dir>
                         PDK root, default from running fulcrum

Liberty Generation Options
  [--bind-file=<file>    File binding cast names to lib names, optional

Verbosity options:
  [--progress]           Show Progress telltales
  [--verbose]            Be verbose, but not as verbose as debug
  [--debug]              Debug mode, full verbosity

USAGE

my $cast_dir  = "";
my $dfII_dir  = "";
my $pdk_dir   = "";
my $size_dir  = "";
my $spec_dir  = "";
my $spec_file = "";
my $lve_dir   = "";
my $lve_args  = "";
my $base_dir  = "$ENV{PWD}";

my $spec;
my $debug = 0;
my $bind_file="";

my $verbose;
my $progress=0;
my $lve_mode = "estimated";
my $lve_view = "floorplan";
my $noexecute = 0;
my %tasks = (
    "pathalyze" => 0,
    "sweep" => 0,
    "lib" => 0,
);

my %valid_corner = (
    "ss" => 1,
    "ff" => 1,
    "tt" => 1,
);

my %options = (
    "bind-file=s" => \$bind_file,
    "cap-values=s" => sub { @cap_values = split (/[ ,]/, $_[1]); },
    "slew-rates=s" => sub { @slew_rates = split (/[ ,]/, $_[1]);},
    "cast-dir=s" => \$cast_dir,
    "cast-path=s" => \$cast_dir,
    "debug" => sub { $verbose=$debug=$progress=1;},
    "delayTau=s", \$delayTau,
    "dfII-dir=s" => \$dfII_dir,
    "base-dir=s" => \$base_dir,
    "spec-dir=s" => \$spec_dir,
    "spec-file=s" => \$spec_file,
    "lve-args=s" => sub { $lve_args .= " $_[1]"; },
    "lve-dir=s" => \$lve_dir,
    "cell=s" => \$cell_name,
    "config=s" => sub { read_config($_[1]); },
    "include=s" => sub { read_config($_[1]); },
    "fulcrum-pdk-root=s" => \$pdk_dir,
    "lve-mode=s" => \$lve_mode,
    "lve-view=s" => \$lve_view,
    "progress" => \$progress,
    "tasks=s" => sub { foreach my $t (split(/,/,$_[1])) {
                         my $found=0;
                         foreach my $s (keys %tasks) {
                            if ( $s =~ /^$t/) {
                                $tasks{$s}=1;
                                $found=1;
                            }
                         }
                         die "Task arg $t not recogized" if ! $found;
                       }
                     },
    "qsub=i" => \$useqsub,
    "qsub-options=s" => sub { $qsub_options .= " $_[1]";},
    "noexecute" => \$noexecute,
    "verbose" => sub { $verbose=$progress=1;},
    "voltage=f" => sub { $voltage=$_[1];
                       $voltage=~ s/[a-z]//gi;
                       $Gpvt = sprintf "%s_%s_%s", $voltage, $temp, $corner;
                     },
    "temp=f" => sub { $temp=$_[1];
                    $temp=~ s/[a-z]//gi;
                    $Gpvt = sprintf "%s_%s_%s", $voltage, $temp, $corner;
                  },
    "corner=s" => sub { $corner=$_[1];
                        $corner=~ tr/A-Z/a-z/;
                        if (! $valid_corner{$corner} ) { usage_exit("Invalid corner $corner"); }
                        $Gpvt = sprintf "%s_%s_%s", $voltage, $temp, $corner;
                    },
);

my $err = 0;
my %nodes;
my @outnodes;
my @innodes;
my @localnodes;
my %path_defs;
my %path_defs_names;
my %param_defs;
my @param_defs;
my %pmcname;
my %pmccell;
my %delay_val  = ();
my %slew_val   = ();

sub readbind {
    my ($file)=@_;
    local (*P,$_);
    if ($file ne "" and open (P, "<$file")) {
        while (<P>) {
            chomp;
            my ($type,$castname,$pmcname)=split;
            if ($type eq "n") {
                $pmcname{$castname}=$pmcname;
            }
            elsif ($type eq "c") {
                $pmccell{$castname}=$pmcname;
            }
        }
        close P;
    }
}

sub suffixcvt {
    my ($val,$mul)=@_;
    $mul=1 if ! $mul;
    my %suffix = (
        "" => 1,
        "a" => 1e-18,
        "f" => 1e-15,
        "p" => 1e-12,
        "n" => 1e-9,
        "u" => 1e-6,
        "m" => 1e-3,
    );

    my $suffix = "";
    if ($val =~ /[afpnum]f$/) {
        $val =~ s/f$//;
    }
    $val =~ m/([afpnum])$/;
    if (defined ($1) and defined ($suffix{$1})) {
        $suffix = $1;
    }
    my $num = $val;
    $num =~ s/[afpnum]$//;
    $num *= $suffix{$suffix}*1e12;
    $num = sprintf "%9f", $num;
    if ($num =~ /\./) {
        $num =~ s:00*$::;
    }
    if ($num =~ /\.$/) {
        $num .= "0";
    }
    $num =~ s/ //g;
    if ($mul == 1) {
        $num .= "e-12";
    }
    $num;
}

my $accummsg="";
sub usage_exit {
    my ($msg)=@_;
    $accummsg .= "FATAL: $msg\n" if $msg and $msg ne "die";
    my $f = $0;
    $f =~ s:.*/::;
    if ($msg eq "die" and length($accummsg)) {
        print STDERR "$accummsg";
        print STDERR "Usage: $f [args]...\n";
        $"=",";
        print STDERR $usage_string;
        exit 1;
    }
}

#-------------------------------------------------------------------------------
#       Read environment file using GetOptions
#-------------------------------------------------------------------------------

sub read_config {
    my ($fileName) = @_;
    local(*ENVFILE);
    my @args;
    undef @args;
    open(ENVFILE,"$base_dir/$fileName") ||
        die("*Error> cannot open the file \"$fileName\".\n"); 
    while (<ENVFILE>) {
        chomp;
        s/^  *//;
        s/  *$//;
        if (! m/^--/) {
            $_ = "--".$_;
        }
        push (@args, $_);
    }
    close(ENVFILE);
    my @tmp=@ARGV;
    @ARGV = @args;
    GetOptions ( %options ) or die "Syntax error in $fileName";
    @ARGV = @tmp;
    1;
}

sub get_localnodes {
    local(*P,*O);
    my @nodes;
    # note: path set by fulcrum script
    my $routed = "";
    $routed = "--routed" if $lve_mode eq "extracted";
    my $cmd= "jflat $routed --cell='$cell_name' --cast-path='$cast_dir:$spec_dir' --tool=local-nodes";
    my $queryfile="$cachedir/$cell_name.$lve_mode.localnodes";
    my (@stat)=stat("$queryfile");
    if ( $#stat < 7 or $stat[7] < 1) {
        print "starting jflat for localnodes.\n" if $progress;
        print $cmd."\n" if $verbose;
        open (P, "$cmd |");
        open (O, ">$queryfile") or die "Cannot create $queryfile.";
        while (<P>) {
            chomp;
            if (/^Local nodes/) {
                next;
            }
            print O "$_\n";
            my ($node) = split;
        }
        close P;
        close O;
        print "jflat for localnodes is done.\n" if $progress;
    }
    else {
        print "skipping cast_query for localnodes.\n" if $progress;
    }
    open (P, "<$queryfile") or die "Cannot read $queryfile.";
    while (<P>) {
        chomp;
        print "$_\n" if $debug;
        my ($node) = split;
        push @nodes, $node;
    }
    close P;
    @nodes;
}

sub getnodes {
    local(*P, *O);
    # note: path set by fulcrum script
    my $cmd= "cast_query --cell='$cell_name' --cast-path='$cast_dir:$spec_dir' --task=external_nodes=di:al:im:re";
    my $nodecnt;
    my $queryfile="$cachedir/$cell_name.query.external";
    my (@stat)=stat("$queryfile");
    if ( $#stat < 7 or $stat[7] < 1) {
        print "starting cast_query for external nodes.\n" if $progress;
        print $cmd."\n" if $verbose;
        open (P, "$cmd |");
        open (O, ">$queryfile") or die "Cannot create $queryfile";
        while (<P>) {
            chomp;
            if (/External/) {
                next;
            }
            print O "$_\n";
        }
        close P;
        close O;
        print "cast_query for external nodes is done.\n" if $progress;
    }
    else {
        print "skipping cast_query for external nodes.\n" if $progress;
    }
    open P, "<$queryfile" or die "Cannot read $queryfile";
    while (<P>) {
        chomp;
        print $_."\n" if $debug;
        my $dir;
        $dir = substr($_,0,1);
        if ( $dir ne "-" and $dir ne "+" ) {
            $dir = "";
        }
        else {
            $_ = substr ($_,1);
        }
        s/=.*//; # handle new cast query format
        $nodes{$_}=$dir;
        $pmcname{$_}=$_;
        $nodecnt++;
    }
    close P;
    $nodecnt;
}

sub get_inputs {
    my ($num_inputs) = @_;
    my @inpins = ();
    foreach my $in (sort keys %nodes) {
        if ($nodes{$in} eq "" or $nodes{$in} eq "-") {
            push @inpins, $in;
        }
    }
    return @inpins;
}

sub get_outputs {
    my @outpins   = ();
    foreach my $in (sort keys %nodes) {
        if ($nodes{$in} eq "" or $nodes{$in} eq "+") {
            push @outpins, $in;
        }
    }
    return @outpins;
}

sub get_loadoutputs {
    my @outpins   = ();
    foreach my $in (sort keys %nodes) {
        if ($nodes{$in} eq "" or $nodes{$in} eq "+") {
            push @outpins, $in;
        }
    }
    return @outpins;
}

#-------------------------------------------------------------------------------
#       lve_sweep - 
#-------------------------------------------------------------------------------

my %localpropsnodes=();

sub lve_sweep {
    print "starting lve_sweep.\n" if $progress;
    my $localprops = "$cachedir/$cell_name.$lve_mode.localprops";
    my $alint_in = "$cachedir/$cell_name.$lve_mode.alint.in";
    my $todo       = "$base_dir/$cachedir/$cell_name.todo";

    # characterize every output or bidirect node
    foreach my $node (keys %nodes) {
        $localpropsnodes{$node}=1 if $nodes{$node} ne "-";
    }

    write_props($localprops) if ( ! -s "$localprops");
    write_alint_in ($alint_in) if (! -s "$alint_in" );
    if ( ! -s ($todo)) {
        open  TODO, ">$todo";
        print TODO $cell_name;
        close TODO;
    }

    my $lve  = "lve";
    $lve    .= " --fulcrum-pdk-root=$pdk_dir";
    $lve    .= " --dfII-dir='$dfII_dir' --cast-dir='$cast_dir'";
    $lve    .= " --spec-dir='$spec_dir'";
#    $lve    .= " --qsub=1 --jobs=$useqsub --mem=1024M" if ($useqsub);
#    $lve    .= " --qsub=0 if (! $useqsub);
    $lve    .= " --qsub=1 --jobs=16 --qsubarch=lx24-amd64"; # allow multiple lve's but not lve forking lve
    $lve    .= " --verbose=1" if $verbose;
    $lve    .= " --sort=1";
    $lve    .= " --estimated-view=$lve_view --mode=$lve_mode" if $lve_mode =~ m/^est/;
    $lve    .= " --mode=nogeometry" if $lve_mode =~ /^nog/;
    $lve    .= " --extracted-view=$lve_view --mode=extracted" if $lve_mode =~ /^ext/;
    $lve    .= " --delayTau=$delayTau";
    $lve    .= " --delayCC=0 --bumpCC='' ";
    $lve    .= " --true=$voltage --corner=$corner --temp=$temp";
    $lve    .= " $lve_args" if $lve_args ne "";
    my $name = "lve$$";
    my $qrsh="";
    $qrsh = "/usr/local/grid/bin/$qsubarch/qsub -cwd $qsub_options -N $name"
        if ($useqsub);
    my $i    = 1;
    foreach my $cap (@cap_values) {
        my $alintaspfile = "$cachedir/alint_${cap}_${Gpvt}_$cell_name.asp";
        my $dir     = "${cap}";
        my $pwd     = "$ENV{PWD}";

        write_alintaspfile($alintaspfile, $cap);

        # note: path set by fulcrum script
        my $lve_cmd  = "$lve --task=alint ";
        $lve_cmd .= "--output-dir='$base_dir/$dir' ";
        $lve_cmd .= "--alint-asp='$pwd/$alintaspfile' ";
        $lve_cmd .= "--alint-in='$pwd/$alint_in' " if -s $alint_in;
        $lve_cmd .= "--cell-localprops='$pwd/$localprops' ";
        $lve_cmd .= "--include='$todo' ";

        print "$lve_cmd\n" if $verbose;
        if (! $noexecute) {
            if ($qrsh ne "") {
                while (checkqueue($name) >= $useqsub) {
                    sleep 1;
                }
                open (G, "| $qrsh 2>/dev/null 1>/dev/null");
                print G "#!/bin/bash\n";
                foreach my $env ( keys %ENV) {
                    print G "export $env=\"$ENV{$env}\"\n"
                        if ($validenv{$env} );
                }
                print G "$lve_cmd\n";
                close G;
            }
            else {
                system("$lve_cmd 2>/dev/null 1>/dev/null");
            }
        }
        $i++;
    }
    if (($tasks{pathalyze} or $tasks{lib}) and checkqueue($name)) {
        print "Waiting for lve sweep to finish.\n";
        while (checkqueue($name)) {
            sleep 2;
        }
    }
    print "lve_sweep is done!\n" if $progress;
}

#-------------------------------------------------------------------------------
#       pathalyze - 
#-------------------------------------------------------------------------------

sub pathalyze {
    print "starting pathalyze.\n" if $progress;
    my $name="path$$";
    my $qrsh="";
    $qrsh = "/usr/local/grid/bin/$qsubarch/qsub -cwd $qsub_options -N $name"
        if ($useqsub);

    # note: path set by fulcrum script
    my $command  = "pathalyze";
    $command    .= " --verbose" if $verbose;
    $command    .= " --routed" if $lve_mode eq "extracted";
    $command    .= " --spec-dir=$spec_dir";
    $command    .= " --cast-dir=$cast_dir:$spec_dir";
    $command    .= " --view=$lve_view --mode=$lve_mode";
    $command    .= " --voltage=$voltage --temp=$temp --corner=$corner";

    my ($cell_dir) = $cell_name;
    $cell_dir =~ tr/\./\//;

    foreach my $cap (@cap_values) {
        my $local_lve_dir   = "$base_dir/$cap";
        $local_lve_dir .= ":$lve_dir" if $lve_dir ne "";
        my $done_dir  = "$local_lve_dir/$cell_dir/$lve_view/$lve_mode/alint/";
        $done_dir    .= "$corner/${voltage}V/${temp}C/";
#            if (! -e "$done_dir/alint.done") {
#                #- need a wait statement if alint done in background
#                print "*Error> cannot find \"alint.done\" in $done_dir\n";
#                next;
#            }

        foreach my $input_slew (@slew_rates) {
            my $path_dir = $cell_name;
            $path_dir =~ s/\./\//g;
            $path_dir = "$base_dir/$cap/$path_dir/pathalyze/$lve_view/$lve_mode/$corner/${voltage}V/${temp}C/slew_$input_slew";
            if (! -f "$path_dir/result" ) { # to allow just running one case
                system "mkdir -p '$path_dir'" if ( ! $noexecute );
                system "touch '$path_dir/result'" if ( ! $noexecute );
                my $pathalyze = "$command --input-slew=$input_slew";
                $pathalyze   .= " --path-dir='$path_dir'";
                $pathalyze   .= " --root-subtype='$cell_name'";
                $pathalyze   .= " --lve-dir='$local_lve_dir'";
                $pathalyze   .= " --verbose "if $verbose;
                print "$pathalyze '$spec'\n" if $verbose;
                if (! $noexecute) {
                    if ($qrsh ne "") {
                        while (checkqueue($name) >= $useqsub) {
                            sleep 1;
                        }
                        open (G, "| $qrsh");
                        foreach my $env ( keys %ENV) {
                            if ($validenv{$env} ) {
                                print G "export $env=\"$ENV{$env}\"\n";
                            }
                        }
                        print G "$pathalyze '$spec'\n";
                        close G;
                        print STDOUT "fulcrum $pathalyze '$spec'\n" if $verbose;
                    }
                    else {
                        system("$pathalyze '$spec' 1>/dev/null 2>/dev/null");
                    }
                }
            }
        }
    }
    if ($tasks{lib} and checkqueue($name)) {
        print "Waiting for pathalyze to finish.\n";
        while (checkqueue($name)) {
            sleep 2;
        }
    }
    print "pathalyze is done!\n" if $progress;
}

#-------------------------------------------------------------------------------
#       gen_lib_delay - 
#-------------------------------------------------------------------------------

# reads the result files and populates the above hash arrays;

sub fill_slew_arrays {
    undef %delay_val;
    undef %slew_val;
    my $result_path;
    my ($delay_list,$slew_list,$param_list);
    my @delay_vals;
    my @slew_val;
    my @param_list;
    my $resultcount=-2;
    foreach my $input_slew (@slew_rates) {
        foreach my $cap (@cap_values) {
            my $path_dir = $cell_name;
            $path_dir =~ s/\./\//g;
            $path_dir = "$base_dir/$cap/$path_dir/pathalyze/$lve_view/$lve_mode/$corner/${voltage}V/${temp}C/slew_$input_slew";
            $result_path = "$path_dir/result";
            ($delay_list,$slew_list,$param_list) = get_result($result_path);
            @delay_vals = split(/\s+/, $delay_list);
            $resultcount = $#delay_vals if $resultcount == -2;
            print "Warning: Mismatch in result file's value counts in $path_dir\n"
                if $#delay_vals != $resultcount;
            print "Error: No result data in $result_path\n"
                if $#delay_vals < 0;
            @slew_val  = split(/\s+/, $slew_list);
            @param_list = split(/\s+/, $param_list);
            foreach my $n (0..$#param_list) {
                push @{$delay_val{$input_slew,$param_list[$n]}}, $delay_vals[$n];
                push @{$slew_val{$input_slew,$param_list[$n]}},  $slew_val[$n];
            }
        }
    }
}


#-------------------------------------------------------------------------------
#       gen_area - generates area from cast query
#-------------------------------------------------------------------------------

sub gen_area {
    my $castpath = "$cast_dir";
    $castpath .= ":$spec_dir" if -d $spec_dir;
    # note: path set by fulcrum script
    my $query    = "cast_query --cast-path='$castpath' --cell='$cell_name'";
    local (*P, $_, *O);

    my $query_cmd1  = "--task=transistors,density";

    my $area_val = 1000000000000;
    my $queryfile = "$cachedir/$cell_name.query.area";
    if ( ! ( -f "$queryfile" ) ) {
        print "$query $query_cmd1\n" if $verbose;
        open P, "$query $query_cmd1 |";
        open O, ">$queryfile";
        $_=<P>;
        print O $_;
        while (<P>) {};
        close P;
        close O;
    }
    open P, "<$queryfile";
    while (<P>) {
        chomp;
        my @split_val = split(/\s+/, $_);
        $area_val = $area_val * $split_val[$#split_val] * $split_val[$#split_val-1];
    }

    print "AREA $area_val\n" if $verbose;
    return $area_val;
}

#-------------------------------------------------------------------------------
#       gen_capacitance
#-------------------------------------------------------------------------------

sub gen_capacitance {
    local(*P,$_,*O);

    my $cell_dir = $cell_name;
    $cell_dir =~ tr/\./\//;

    my @cap_val    = ();
    my %innodes;
    foreach my $node (@innodes) {
        $innodes{$node}=1;
    }
    my $queryfile="$cachedir/$cell_name.$lve_view.$lve_mode.captally";
    my (@stat)=stat("$queryfile");
    if ( $#stat < 7 or $stat[7] < 1) {
        my $spice_file = "$base_dir/$cap_values[0]/$cell_dir/$lve_view/$lve_mode/cell.spice";
        $spice_file = "$base_dir/$cap_values[0]/$cell_dir/$lve_view/$lve_mode/cell.spice_gds2"
            if $lve_mode eq "extracted";
        die "Cannot open $spice_file" if (! -r "$spice_file");
        print "starting captally.\n" if $progress;
        my $captally = "captally";
        $captally .= " --cdl '$spice_file'";
        $captally .= " --fulcrum-pdk-root '$pdk_dir'";
        print "$captally\n" if $verbose;
        local(*RDFH,*WTFH);
        my $pid;
        if ($lve_mode eq "extracted") {
            $pid=open2(\*RDFH, \*WTFH, "rename", "--type=node", "--from=gds2", "--to=cast");
        }
        open (P, "$captally |");
        open (O, ">$queryfile");
        while (<P>) {
            chomp;
            my ($pin,$cap,$x)=split;
            # rename gds names to cast names
            if ($lve_mode eq "extracted") {
                print WTFH $pin."\n";
                $pin = <RDFH>;
                chomp $pin;
            }
            if (! defined ($x) and defined ($cap) and defined ($innodes{$pin})) {
                print O "$pin $cap\n";
            }
        }
        close P;
        close O;
        if ($lve_mode eq "extracted") {
            close WTFH;
            close RDFH;
            waitpid $pid, 0;
        }
        print "captally done.\n" if $progress;
    }
    else {
        print "skipping captally.\n" if $progress;
    }
    open P, "<$queryfile";
    my %cap;
    while (<P>) {
        chomp;
        my ($pin,$cap,$x)=split;
        if (! defined ($x) and defined ($cap) and defined ($innodes{$pin})) {
            $cap{$pin}=sprintf "%.4g", $cap;
        }
    }
    foreach my $inpin (@innodes) {
        push @cap_val, $cap{$inpin};
    }
    close P;
    undef %cap;
    undef %innodes;
    @cap_val;
}

#-------------------------------------------------------------------------------
#       Misc Subroutines
#-------------------------------------------------------------------------------

my %specnodes;

sub write_props {
    my ($localprops) = @_;
    local(*PROPS);
    open  PROPS, ">$localprops";
    foreach my $node (sort keys %localpropsnodes) {
        print PROPS "$node 0 1000 1000 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1\n";
    }
    close PROPS;
}

sub write_alint_in {
    my ($file) = @_;
    if ( -f $file ) {
        return;
    }
    local(*ALINTIN);
    open  ALINTIN, ">$file";
    print ALINTIN <<EH;
leaky_file "leaky.nodes"
bumpCC
delayCC 0
bumpTau 4e-11
delayTau 5e-12 1e-11 2e-11 4e-11 8e-11 1.6e-10 3.2e-10 6.4e-10 1.28e-09 2.52e-09
EH
    foreach my $node (sort keys %localpropsnodes) {
        print ALINTIN "output \"$node/out\"\n";
        print ALINTIN "outerr \"$node/err\"\n";
        print ALINTIN "alint $node\n";
    }
    print ALINTIN <<ET;
output "alint_parallel/alint.done.0"
echo done
output
outerr
ET
    close ALINTIN;
}

sub write_alintaspfile {
    my ($alintaspfile,$cap) = @_;
    my @nodes_withcap = get_loadoutputs();
    local (*CAPFILE);
    open  CAPFILE, ">$alintaspfile";
    print CAPFILE ".true=$voltage;\n";
    print CAPFILE ".temperature=$temp;\n";
    print CAPFILE ".corner \"$corner\";\n";
    print CAPFILE ".include \"tech.asp\";\n";
    print CAPFILE ".include \"cell.aspice\";\n";
    print CAPFILE ".include \"noprs.asp\";\n";
    print CAPFILE ".poststep=5e-12;\n";
    print CAPFILE ".max_bump_fanin_aggressors=2;\n";
    print CAPFILE ".max_delay_fanin_aggressors=1;\n";
    print CAPFILE ".prstau=4e-11;\n";

    foreach my $node (@nodes_withcap) {
        print CAPFILE "cap (\"$node\")($cap);\n";
    }

    close CAPFILE;
}

sub get_slew_index {
    my (@slew_rates) = @_;
    my $index1   = "";
    foreach my $input_slew (@slew_rates) {
        $index1 .= ", " unless ($index1 eq "");
        $input_slew = $input_slew / 1000;
        $index1 .= $input_slew;
    }
    return $index1;
}

sub get_cap_index {
    my (@cap_values) = @_;
    my $index2   = "";
    foreach my $cap (@cap_values) {
        $index2 .= ", " unless ($index2 eq "");
        my ($cap_val,$scale_val) = split(/e-1/,$cap);
        $index2 .= $cap_val;
    }
 
    return $index2;
}

# The /2 ratio depends on the threshold value, see above
sub get_delay_w_slew {
    my($input_slew,$plist)=@_;
    my @delays=();
    if (defined $delay_val{$input_slew,$plist}) {
        @delays=@{$delay_val{$input_slew,$plist}};
    }
    else {
        print STDOUT "No delay data for $input_slew $plist\n";
    }
    my @outslew=();
    if (defined $slew_val{$input_slew,$plist}) {
        @outslew=@{$slew_val{$input_slew,$plist}};
    }
    else {
        print STDERR "No slew data for $input_slew $plist\n";
    }
    foreach my $dly (0..$#delays) {
        # probably should just leave this
        $outslew[$dly] = 1000 if $outslew[$dly] eq "error";
        $delays[$dly] = 1000 if $delays[$dly] eq "error";
        $delays[$dly] += $outslew[$dly]/2;
        $delays[$dly] -= $input_slew/2;
    }
    @delays;
}

sub get_value {
    my (@values) = @_;
    my $val_string = "\"";
    my $prev_value = "";
    my $value = 0;

    for (my $i = 0; $i < $#values; $i++) {
        # probably should just leave this
        $values[$i] = 1000 if $values[$i] eq "error";
        $value = $values[$i]/1000;
        $val_string .= "$value, ";
    }
    # probably should just leave this
    $values[$#values] = 1000 if $values[$#values] eq "error";
    $value = $values[$#values]/1000;
    $val_string .= "$value\"";

    return $val_string;
}

sub get_result {
    my $path = shift;
    my $delayList="";
    my $slewList="";
    my $pinList="";
    my $errfound=0;
    open(RSTFILE,"$path") || 
        die("*Error> cannot open the result file $path.\n");
    # First two lines are header
    $pinList = <RSTFILE>;
    $pinList = <RSTFILE>;
    $pinList = "";
    # Parse the list of parameters, delays, and slew rates.  Strange
    # data structures inherited from old code based on the original result
    # file format.
    while (<RSTFILE>) {
        my ($param, $delay, $slew) = split /\s+/, $_;
        if ($pinList ne "") {
            $pinList   .= " ";
            $delayList .= " ";
            $slewList  .= " ";
        }
        $pinList   .= $param;
        print STDERR "Error found in $path\n"
            if $delay eq "error" or $slew eq "error";
        $delayList .= $delay;
        $slewList  .= $slew;
    }
    close(RSTFILE);
    print "Error: result file is incomplete $path\n", $errfound=1
        if ( $pinList eq "" or $delayList eq "" or $slewList eq "") and ! $errfound;
    return($delayList,$slewList,$pinList);
}

sub get_cap_value {
    my ($capfile) = @_;
    local(*CAPFILE);
    open(CAPFILE,"$base_dir/$cachedir/$capfile") || 
        die("*Error> cannot open the file \"$capfile\".\n");
    while (<CAPFILE>) {
        if (/^Total.load/) {
            my @splitval = split(/\s+/, $_);
            close(CAPFILE);
            return $splitval[5];
        }
    }
    close(CAPFILE);
    print("*Warning> cannot find the capacitance value.\n");
    return 0;
}

#-------------------------------------------------------------------------------
#       gen_library
#-------------------------------------------------------------------------------

sub gen_library {

    print "Generate liberty spec...\n" if $progress;

    #- generate .info file.
    my $area_val = gen_area();
    my @cap_val  = gen_capacitance();

    #- generate timing file and .lib file.
    my $name = "${cell_name}";
    $name = $pmccell{$cell_name} if defined $pmccell{$cell_name};
    $name .= "_$Gpvt";
    $name =~ s/\./_/g;
    gen_lib_library("${name}.lib",$area_val,@cap_val);
}

#-------------------------------------------------------------------------------
#       gen_lib_library
#-------------------------------------------------------------------------------
sub getpaths {
    my ($p)=@_;
    my $pd = $param_defs{$p};
    $pd =~ s/[a-z]+\(/ /g;
    $pd =~ s/\)/ /g;
    $pd =~ s/\(/ /g;
    $pd =~ s/ *[-+] */ /g;
    $pd =~ s/  */ /g;
    $pd =~ s/^ //;
    $pd =~ s/ $//;
    my @pth=split(/[, ]/,$pd);
    if ($debug) {
        local($")=':';
        print "PARAM $p : @pth\n" if $debug;
    }
    my %rp;
    foreach my $pth (@pth) {
        if ($pth eq "") {
            next;
        }
        my @b;
        my @a = ${path_defs_names{$pth}};
        if (defined ($a[0])) {
            @b = @{$a[0]};
        }
        if (! defined ($b[0])) {
            if (defined ($param_defs{$pth})) {
                foreach my $px (getpaths($pth)) {
                    if ($px ne "") {
                        $rp{$px}=1;
                    }
                }
            }
            else {
                print "Error: Undefined $pth on $p\n" if $pth ne "0";
            }
        }
        else {
            $rp{$pth}=1;
        }
    }
    (sort keys %rp);
}

sub setuptime {
    my ($fp,$pin,$clk,$dparam,$cparam,$tag)=@_;
    my $n = $#slew_rates+1;
    my @dummy;
    for (my $i = 0; $i < $n; $i++) {
        $dummy[$i]=0;
    }
    if ($clk =~ /\[/ and $pin =~ /\[/) {
        $clk =~ m/\[(\d+)\]/;
        my $clkarr=$1;
        $pin =~ m/\[(\d+)\]/;
        my $pinarr=$1;
        $clk =~ s/\[$clkarr\]/[$pinarr]/;
    }
    print $fp "      timing() {\n";
    print $fp "        related_pin : \"$pmcname{$clk}\";\n";
    print $fp "        timing_type : ${tag}_rising;\n";
    # note: our database does not distinguish rising from falling
    foreach my $rf ("rise","fall") {
        print $fp "        ${rf}_constraint(${tag}_template_${n}x$n) {\n";
        for (my $x = 1; $x <= 2; $x++) {
        print $fp "          index_$x (\"";
            my $v;
            for (my $y = 0; $y < $n-1; $y++) {
                $v = sprintf "%.3f", $slew_rates[$y]/1000;
                print $fp "$v, ";
            }
            $v = sprintf "%.3f", $slew_rates[$n-1]/1000;
            print $fp "$v\");\n";
        }
        print $fp "          values ( \\\n";
        foreach my $nd (0..$n-1) {
            my @dvalues=@dummy;
#            @dvalues = @{$delay_val{$slew_rates[$nd],$dparam}} if defined $dparam;
            @dvalues = get_delay_w_slew($slew_rates[$nd],$dparam) if defined $dparam;
            my @cvalues;
            my $val_string = "\"";
            foreach my $nc (0..$n-1) {
                @cvalues=@dummy;
#                @cvalues = @{$delay_val{$slew_rates[$nc],$cparam}} if defined $cparam;
                @cvalues = get_delay_w_slew($slew_rates[$nc],$cparam) if defined $cparam;
                if ($tag eq "hold") {
                    $val_string .= (sprintf "%.4f", ($cvalues[0]-$dvalues[0])/1000).", ";
                }
                else {
                    $val_string .= (sprintf "%.4f", ($dvalues[0]-$cvalues[0])/1000).", ";
                }
    #            $val_string .= (sprintf "%.4f", ($cvalues[$nd])/1000).", ";
            }
            $val_string =~ s/, $/"/;
            print $fp "             $val_string";
            print $fp ", \\\n" if $nd < $n-1;
            print $fp ");\n" if $nd >= $n-1;
        }
        print $fp "        }\n";
    }
    print $fp "      }\n";
}

sub gen_lib_library {
    my ($outfile, $area, @cap_val)=@_;
    my %clkpin;
    print "starting lib file generation.\n" if $progress;
    print "Writing $outfile ...\n" if $progress;
    local(*P);
    open (P, ">$outfile") or die "Cannot write to $outfile";
    # generate the header
    my $hdr = "$lib_header";
    $hdr =~ s/\$voltage/$voltage/g;
    $hdr =~ s/\$temp/$temp/g;
    $hdr =~ s/\$corner/$corner/g;
    $hdr =~ s/\$libmode/$libmode{$corner}/g;
    $hdr =~ s/\$opcond/$opcond{$corner}/g;
    $hdr =~ s/INPUT_THRESHOLD_PCT_FALL/$input_threshold_pct_fall/g;
    $hdr =~ s/INPUT_THRESHOLD_PCT_RISE/$input_threshold_pct_rise/g;
    $hdr =~ s/OUTPUT_THRESHOLD_PCT_FALL/$output_threshold_pct_fall/g;
    $hdr =~ s/OUTPUT_THRESHOLD_PCT_RISE/$output_threshold_pct_rise/g;
    print P "$hdr";
    # these two loops are not really necessary, there is only one template
    # Generate the template(s) for the delays
    # make the table templates
    print P <<EONE;
  lu_table_template(delay_template_1x2) {
    variable_1 : total_output_net_capacitance;
    index_1 ("1000, 1001");
  }
EONE
    foreach my $n1 ($#slew_rates+1) {
        foreach my $n2 ($#cap_values+1) {
            my @n=($n1,$n2);
            if ($n1 > 1 or $n2 > 1) {
                print P "  lu_table_template(delay_template_${n1}x$n2) {\n";
                if ($n1 > 1 and $n2 > 1) {
                    print P "    variable_1 : input_net_transition;\n";
                    print P "    variable_2 : total_output_net_capacitance;\n";
                    for (my $nx = 1; $nx <= 2; $nx++) {
                        print P "    index_$nx (\"";
                        for (my $ny = 1; $ny < $n[$nx-1]; $ny++) {
                            printf P "%d, ", 1000+$ny-1;
                        }
                        printf P "%d\");\n", 1000+$n[$nx-1]-1;
                    }
                }
                elsif ($n2 > 1) {
                    print P "    variable_1 : total_output_net_capacitance;\n";
                    print P "    index_1 (\"";
                    for (my $ny = 1; $ny < $n2; $ny++) {
                        printf P "%d, ", 1000+$ny-1;
                    }
                    printf P "%d\");\n", 1000+$n2-1;
                }
                else {
                    print P "    variable_1 : input_net_transition;\n";
                    print P "    index_1 (\"";
                    for (my $ny = 1; $ny < $n1; $ny++) {
                        printf P "%d, ", 1000+$ny-1;
                    }
                    printf P "%d\");\n", 1000+$n1-1;
                }
                print P "  }\n";
            }
        }
    }
    # generate templates for setup and hold
    foreach my $template ("setup", "hold") {
        foreach my $n1 ($#slew_rates+1) {
            if ($n1 > 1) {
                print P "  lu_table_template(${template}_template_${n1}x$n1) {\n";
                print P "    variable_1 : constrained_pin_transition;\n";
                print P "    variable_2 : related_pin_transition;\n";
                for (my $nx = 1; $nx <= 2; $nx++) {
                    print P "    index_$nx (\"";
                    for (my $ny = 1; $ny < $n1; $ny++) {
                        printf P "%d, ", 1000+$ny-1;
                    }
                    printf P "%d\");\n", 1000+$n1-1;
                }
            }
            print P "  }\n";
        }
    }
    # find all pins thru the params
    my %outnodes;
    my %outpins;   # for combinational logic
    my %inpins;    # for setup/hold
    my %innodes;
    my %parampins;
    my %pathpins;

    $"=':';
    foreach my $node (@outnodes) {
        $outnodes{$node}=1;
    }
    foreach my $node (@innodes) {
        $innodes{$node}=1;
    }
    # sort thru the paths and generate a list of output pins
    # and generate a list of paths which have only input pins (relevant
    # for setup and hold times)
    foreach my $p (sort keys %param_defs) {
        my @b;
        my $direction;
        my $unate;
        my $outpin;
        my $inpin;
        my %pp = ();
        foreach my $pth (getpaths($p)) {
            my @b;
            my @a = ${path_defs_names{$pth}};
            if (defined ($a[0])) {
                @b = @{$a[0]};
            }
            if (! defined ($b[0])) {
                if (defined ($param_defs{$pth})) {
                    print "Error: Param not Path $pth in $p\n";
                }
                else {
                    print "Error: Undefined $pth on $p\n";
                }
                next;
            }
            # convert these paths to their appropriate directions
            # and their appropriate pin names
            my @dir;
            $b[0] =~ m/([-\+])$/;
            $dir[0] = $1;
            $b[$#b] =~ m/([-\+])$/;
            $dir[1] = $1;
            $b[0] =~ s/[-\+]$//;
            $b[0] =~ s/^\(//;
            $b[0] =~ s/\)$//;
            $b[$#b] =~ s/[-\+]$//;
            my @px;
            my @px2d;
            undef @px;
            undef @px2d;
            foreach my $py (keys %path_defs) {
                if ($py =~/^$pth\[/) {
                    $py =~ s/.*\[//;
                    # this is not very good, does not handle multi dimensions well XXX
                    $py =~ m/^([a-z]):(\d+)\.\.(\d+)]/;
                    if (defined ($3)) {
                        $px[0] = $2;  # the lower limit
                        $px[1] = $3;  # upper limit
                        $px[2] = $1;  # the variable names
                    }
                    else {
                        $py =~ m/^([a-z]):(\d+)]/;
                        if (defined ($2)) {
                            $px[0] = 0;    # the lower limit
                            $px[1] = $2-1; # upper limit
                            $px[2] = $1;   # the variable names
                        }
                        else { # try for two dimensions
                            $py =~ m/^([a-z]):(\d+)\.\.(\d+),/;
                            if (defined ($3)) {
                                $px[0] = $2;  # the lower limit
                                $px[1] = $3;  # upper limit
                                $px[2] = $1;  # the variable names
                            }
                            else {
                                $py =~ m/^([a-z]):(\d+),/;
                                if (defined ($2)) {
                                    $px[0] = 0;    # the lower limit
                                    $px[1] = $2-1; # upper limit
                                    $px[2] = $1;   # the variable names
                                }
                            }
                            $py =~ m/,([a-z]):(\d+)\.\.(\d+)]/;
                            if (defined ($3)) {
                                $px2d[0] = $2;  # the lower limit
                                $px2d[1] = $3;  # upper limit
                                $px2d[2] = $1;  # the variable names
                            }
                            else {
                                $py =~ m/,([a-z]):(\d+)]/;
                                if (defined ($2)) {
                                    $px2d[0] = 0;    # the lower limit
                                    $px2d[1] = $2-1; # upper limit
                                    $px2d[2] = $1;   # the variable names
                                }
                            }
                        }
                    }
                    last;
                }
            }
            if (($b[0] =~ m/\[.*[ij].*]/) or ($b[$#b] =~ m/\[.*[ij].*]/)) {
                # this is to expand the variables
                if (! defined ($px[1])) {
                    print "Error: parsing $b[0] $b[$#b] because $pth not found in path_defs\n";
                    exit 1;
                }
                else {
                    # this is not very good, does not handle multi dimensions well XXX
                    if (defined ($px2d[0])) {
                        for (my $i = $px[0]; $i <= $px[1]; $i++) {
                            for (my $j = $px2d[0]; $j <= $px2d[1]; $j++) {
                                my $p1=$b[0];
                                $p1 =~ m/\[(.*)\]/;
                                if (defined ($1)) {
                                    my @arg = split(/,/,$1);
                                    foreach my $arg (@arg) {
                                        $arg =~ s/$px[2]/$i/g;
                                        $arg =~ s/$px2d[2]/$j/g;
                                        eval "\$arg = $arg";
                                    }
                                    $"=',';
                                    $p1 =~ s/\[.*\]/[@arg]/;
                                }
                                my $p2=$b[$#b];
                                $p2 =~ m/\[([^\]]*)\]/;
                                if (defined ($1)) {
                                    my @arg = split(/,/,$1);
                                    foreach my $arg (@arg) {
                                        $arg =~ s/$px[2]/$i/g;
                                        $arg =~ s/$px2d[2]/$j/g;
                                        eval "\$arg = $arg";
                                    }
                                    $"=',';
                                    $p2 =~ s/\[[^\]]*\]/[@arg]/;
                                }
                                #print "AA $b[0] $p1 $b[$#b] $p2\n" if $debug;
                                if (defined ($outnodes{$p2})) {
                                    push @{$outpins{$p2}}, [$p1,$p,$dir[0],$dir[1],$pth];
                                    print "OUT1 $p2 $p1,$p,$dir[0],$dir[1],$pth\n" if $debug;
                                }
                                elsif (defined ($innodes{$p1})) {
                                    push @{$inpins{$p1}}, [$p2,$p,$dir[0],$dir[1],$pth];
                                    $pp{$p1} = 1;
                                }
                                if (defined ($innodes{$p1}) or defined ($outnodes{$p2})) {
                                    print "$p => $pth = $p1 => $p2\n" if $debug;
                                }
                            }
                        }
                    }
                    else {
                        for (my $i = $px[0]; $i <= $px[1]; $i++) {
                            my $p1=$b[0];
                            $p1 =~ m/\[(.*)\]/;
                            if (defined ($1)) {
                                my @arg = split(/,/,$1);
                                foreach my $arg (@arg) {
                                    $arg =~ s/$px[2]/\$i/g;
                                    eval "\$arg = $arg";
                                }
                                $"=',';
                                $p1 =~ s/\[.*\]/[@arg]/;
                            }
                            my $p2=$b[$#b];
                            $p2 =~ m/\[([^\]]*)\]/;
                            if (defined ($1)) {
                                my @arg = split(/,/,$1);
                                foreach my $arg (@arg) {
                                    $arg =~ s/$px[2]/\$i/g;
                                    eval "\$arg = $arg";
                                }
                                $"=',';
                                $p2 =~ s/\[[^\]]*\]/[@arg]/;
                            }
                            #print "AA $b[0] $p1 $b[$#b] $p2\n" if $debug;
                            if (defined ($outnodes{$p2})) {
                                push @{$outpins{$p2}}, [$p1,$p,$dir[0],$dir[1],$pth];
                                print "OUT2 $p2 $p1,$p,$dir[0],$dir[1],$pth\n" if $debug;
                            }
                            elsif (defined ($innodes{$p1})) {
                                push @{$inpins{$p1}}, [$p2,$p,$dir[0],$dir[1],$pth];
                                $pp{$p1} = 1;
                            }
                            if (defined ($innodes{$p1}) or defined ($outnodes{$p2})) {
                                print "$p => $pth = $p1 => $p2\n" if $debug;
                            }
                        }
                    }
                }
            }
            elsif (defined ($innodes{$b[0]}) or defined ($outnodes{$b[$#b]})) {
                if (defined ($outnodes{$b[$#b]})) {
                    push @{$outpins{$b[$#b]}}, [$b[0],$p,$dir[0],$dir[1],$pth];
                    print "OUT3 $b[$#b] $b[0],$p,$dir[0],$dir[1],$pth\n" if $debug;
                }
                elsif (defined ($innodes{$b[0]})) {
                    push @{$inpins{$b[0]}}, [$b[$#b],$p,$dir[0],$dir[1],$pth];
                    $pp{$b[0]}=1;
                }
                print "$p => $pth = $b[0] => $b[$#b]\n" if $debug;
            }
        }
        my @pp=(sort keys %pp);
        $parampins{$p}=[ @pp ];
        undef %pp;
        undef @pp;
    }
    my %pins;
    my %cap;
    @outnodes = sort keys %outnodes;
    @innodes = sort keys %innodes;
    # label the pin as input/output/inout
    foreach my $pin (@outnodes) {
        if ($pin ne "") {
            $pins{$pin} = "output";
        }
        else {
            warn "Empty pin name";
        }
    }
    my $n = 0;
    foreach my $pin (@innodes) {
        if (defined ($pins{$pin}) and $pins{$pin} eq "output") {
            $pins{$pin} = "inout";
        }
        elsif (!defined ($pins{$pin}) or $pins{$pin} ne "inout") {
            $pins{$pin} = "input";
        }
        $cap{$pin} = sprintf "%.4g", $cap_val[$n] if defined ($cap_val[$n]);
        $n++;
    }
    foreach my $pin (keys %cap) {
        $cap{$pin}=10e-12 if ! defined $specpins{$pin};
    }
    # sort thru all of the input pins with no output
    my %setup_data;
    my %setup_clk;
    my %hold_data;
    my %hold_clk;
    my %other;
    # setup for setup and hold times
    foreach my $inpin (sort keys %inpins) {
        my %li;
        undef %li;
        my $setupd;
        my $setupc;
        my $holdd;
        my $holdc;
        foreach my $list (@{$inpins{$inpin}}) {
            my ($outpin,$param,$dir0,$dir1,$pth)=@{$list};
            my @list=@{$parampins{$param}};
            $"=" ";
            my $l="$param @list";
            if ($param =~ /(setup|hold)_(data|clk)$/ and ! defined ($li{$l})) {
                $li{$l}=1;
                print "PP $param $inpin" if $debug;
                foreach my $pin (@list) {
                    print " $pin" if $pin ne $inpin and $debug;
                }
                print "\n" if $debug;
                if ($1 eq "setup") {
                    if ($2 eq "data") {
                        if (! defined ($setupd)) {
                            $setupd = $param;
                        }
                        elsif (length($param) < length($setupd)) {
                            $setupd = $param;
                        }
                        $setup_data{$inpin}=$setupd;
                    }
                    else {
                        if (! defined ($setupc)) {
                            $setupc = $param;
                        }
                        elsif (length($param) < length($setupc)) {
                            $setupc = $param;
                        }
                        $setup_clk{$inpin}=$setupc;
                    }
                }
                elsif ($1 eq "hold") {
                    if ($2 eq "data") {
                        if (! defined ($holdd)) {
                            $holdd = $param;
                        }
                        elsif (length($param) < length($holdd)) {
                            $holdd = $param;
                        }
                        $hold_data{$inpin}=$holdd;
                    }
                    else {
                        if (! defined ($holdc)) {
                            $holdc = $param;
                        }
                        elsif (length($param) < length($holdc)) {
                            $holdc = $param;
                        }
                        $hold_clk{$inpin}=$holdc;
                    }
                }
                else {
                    $other{$param}=[ $inpin, @list ];
                }
            }
        }
    }
    print "  cell $cell_name\n" if $debug;
    my $pc=$cell_name;
    $pc = $pmccell{$cell_name} if defined $pmccell{$cell_name};
    $pc =~ s/\(/-L/g;
    $pc =~ s/\)/-R/g;
    print P "  cell(\"$pc\") {\n";
    print P "    area : $area;\n";
    fill_slew_arrays;
    foreach my $pin (sort keys %outpins) {
        my @list = @{$outpins{$pin}};
        my %unate = ();
        my %param = ();
        my %un    = ();
        my @related_pin = ();
        my @pindirection= ();
        my @paramlist   = ();
        foreach my $pair (@list) {
            my ($related_pin,$param,$path,$type);
            my @dir;
            ($related_pin,$param,$dir[0],$dir[1],$path)=@{$pair};
            if (!defined ($innodes{$related_pin})) {
                print "Skipping $pin -> $related_pin\n" if $debug;
                next;
            }
            $"=" ";
            $un{"$related_pin @dir"}=1;
            $param{"$related_pin:$param"}=1;
            if (defined ($unate{$related_pin})) {
                if ($dir[0] ne $dir[1] and $unate{$related_pin} eq "positive_unate") {
                    $unate{$related_pin} = "non_unate";
                }
                if ($dir[0] eq $dir[1] and $unate{$related_pin} eq "negative_unate") {
                    $unate{$related_pin} = "non_unate";
                }
            }
            else {
                if ($dir[0] ne $dir[1]) {
                    $unate{$related_pin} = "negative_unate";
                }
                else {
                    $unate{$related_pin} = "positive_unate";
                }
            }
            push @related_pin, $related_pin;
            push @pindirection, $dir[1] eq "+" ? "rise" : "fall";
            push @paramlist, $param;
            print "RP $related_pin => $pin : $param @dir\n" if $debug;
        }
        print "    pin $pin\n" if $debug;
        print P "    pin(\"$pmcname{$pin}\") {\n";
        $writtenpin{$pin}=1;
        print P "      direction : output;\n";
        if (defined ($cap{$pin}) and $cap{$pin} ne "" and $cap{$pin} > 0.0) {
            printf P "      capacitance : %.3f;\n", $cap{$pin}*1e12;
        }
        my %type;
        my %cnt;
        my %rp;
        foreach my $f (sort keys %un) {
            my ($rp,$dir0,$dir1)=split(/ /,$f);
            $cnt{$rp}++;
            if (!defined ($rp{$rp})) {
                $rp{$rp}="${dir0}$dir1";
                $type{$rp}="";
            }
            elsif (substr($rp{$rp},0,1) eq $dir0) {
                if ($dir0 eq "+") {
                    $type{$rp}="rising_edge";
                }
                else {
                    $type{$rp}="falling_edge";
                }
            }
            else {
#                $type{$rp}="Unknown $rp{$rp} $dir0$dir1";
                $type{$rp}="";
            }
        }
        my $index1 = get_slew_index(@slew_rates);
        my $index2 = get_cap_index(@cap_values);

        my $last_relpin="";
        my %dupkey = ();
        my @ignore;
        foreach my $i (0..$#related_pin) {
            my $related_pin = $related_pin[$i];
            my $pindirection = $pindirection[$i];
            $ignore[$i]=0;
            push @{$dupkey{"$related_pin:$pindirection"}}, $paramlist[$i];
        }
        foreach my $key (keys %dupkey) {
            my @plist=@{$dupkey{$key}};
            my ($related_pin,$pindirection)=split(/:/,$key);
            if ($#plist > 0) {
                print "DIRINFO    $pmcname{$pin} $related_pin $pindirection @plist\n"
                    if $debug;
            }
        }
        undef %dupkey;
        foreach my $i (0..$#related_pin) {
            my $related_pin = $related_pin[$i];
            if (defined ($dupkey{"$related_pin:$pindirection[$i]"})) {
                my $param = $dupkey{"$related_pin:$pindirection[$i]"};
                print "Warning: Duplicate pin direction data pin $pin, rel pin $related_pin\n".
                     "  Skipping duplicate entries: $paramlist[$i] keeping $param\n" if $debug;
                next;
            }
            $dupkey{"$related_pin:$pindirection[$i]"}=$paramlist[$i];
            if ($last_relpin ne "$related_pin") {
                print P "      }\n" if ($i != 0);
                print P "      timing() {\n";
                print P "        related_pin : \"$pmcname{$related_pin}\";\n";
                print P "        timing_sense : $unate{$related_pin};\n";
                print P "        timing_type : $type{$related_pin};\n" if ($type{$related_pin} ne "");
                $last_relpin = $related_pin;
            }
            printf P "        cell_${pindirection[$i]}(delay_template_%dx%d) {\n",
                $#slew_rates+1,$#cap_values+1;
            print P "          index_1 (\"$index1\");\n";
            print P "          index_2 (\"$index2\");\n";
            print P "          values ( \\\n";
            foreach my $input_slew (@slew_rates) {
                my $string   = get_value(get_delay_w_slew($input_slew,$paramlist[$i]));
                print P "  , \\\n" unless ($input_slew == $slew_rates[0]);
                print P "            $string";
            }
            print P " );\n";
            print P "        }\n";
            printf P "        ${pindirection[$i]}_transition(delay_template_%dx%d) {\n",
                $#slew_rates+1,$#cap_values+1;
            print P "          index_1 (\"$index1\");\n";
            print P "          index_2 (\"$index2\");\n";
            print P "          values ( \\\n";
            foreach my $input_slew (@slew_rates) {
                my $string   = get_value(@{$slew_val{$input_slew,$paramlist[$i]}});
                print P "  , \\\n" unless ($input_slew == $slew_rates[0]);
                print P "            $string";
            }
            print P " );\n";
            print P "        }\n";
        }
        print P "      }\n" if ($#related_pin >= 0);
        print P "    }\n";
    }
    my %isclock;
    # search all inputs to find the clock pins
    foreach my $pin (sort keys %inpins) {
        if (defined ($setup_data{$pin})) {
            my $dataparam = $setup_data{$pin};
            my $clkparam = $dataparam;
            $clkparam =~ s/_data/_clk/;
            my @clk;
            if (defined ($parampins{$clkparam})) {
                @clk = @{$parampins{$clkparam}};
                if (! defined ($clk[0]) ) {
                    print "Error: Clock not defined for $clkparam\n";
                }
                else {
                    my $clk=array_clock($clk[0],$pin);
                    $isclock{$clk}=1;
                    $clkpin{$pin}=$clk;
                }
            }
            else {
                $clkparam =~ s/_setup_/_hold_/;
                if (defined ($parampins{$clkparam})) {
                    @clk = @{$parampins{$clkparam}};
                    if (! defined ($clk[0]) ) {
                        print "Error: Clock not defined for $clkparam\n";
                    }
                    else {
                        my $clk=array_clock($clk[0],$pin);
                        $isclock{$clk}=1;
                        $clkpin{$pin}=$clk;
                    }
                }
                else {
                    print "Error: Clock not defined for $dataparam\n";
                }
            }
        }
    }
    foreach my $pin (sort keys %inpins) {
        next if (($pin eq "Vdd") or ($pin eq "GND"));
        print "    pin $pin\n" if $debug;
        print P "    pin(\"$pmcname{$pin}\") {\n";
        $writtenpin{$pin}=1;
        print P "      direction : $pins{$pin};\n";
        printf P "      clock : %s;\n", $isclock{$pin} ? "true" : "false";
        if (defined ($cap{$pin}) and $cap{$pin} ne "" and $cap{$pin} > 0.0) {
            printf P "      capacitance : %.3f;\n", $cap{$pin}*1e12;
        }
        my $clk;
        if (defined ($setup_data{$pin})) {
            my $dataparam = $setup_data{$pin};
            $clk = $clkpin{$pin};
            if (! defined ($clk)) {
                print "Error: Setup Cannot find clock for $pin ($dataparam)\n";
                $clk="";
            }
            else {
                print "PX Setup Data $pin CLK $clk\n" if $debug;
                setuptime(*P, $pin,$clk,$setup_data{$pin},$setup_clk{$clk},"setup");
                # do setuptime printing here
            }
            if (defined ($hold_clk{$clk})) {
                # do hold time printing here
                setuptime(*P, $pin,$clk,$hold_data{$pin},$hold_clk{$clk},"hold");
                print "PX Hold time for $pin\n" if $debug;
            }
        }
        print P "    }\n";
    }
    foreach my $pin (sort keys %nodes) {
        next if (($pin eq "Vdd") or ($pin eq "GND"));
#        printf STDERR "$pin $pins{$pin} %d\n", defined $setup_data{$pin};
        if (! defined ($writtenpin{$pin})) {
            print "    pin $pin\n" if $debug;
            print P "    pin(\"$pmcname{$pin}\") {\n";
            print P "      direction : $pins{$pin};\n";
            printf P "      clock : %s;\n", $isclock{$pin} ? "true" : "false";
            if (defined ($cap{$pin}) and $cap{$pin} ne "" and $cap{$pin} > 0.0) {
                printf P "      capacitance : %.3f;\n", $cap{$pin}*1e12;
            }
            print P "    }\n";
        }
    }
    print P "  }\n\n";
#    print P "$lib_trailer";
    print P "}\n";
    close P;
    print "lib file generation is done\n" if $progress;
}

foreach my $opt (sort keys %options) {
    $opt =~ s/=.*/=/;
    if (!($usage_string =~ m:\n *\[--$opt: or $usage_string =~ m:\n *--$opt:) ) {
        warn "$opt not in help";
        $err++;
    }
}
foreach my $opt (split (/\n/, $usage_string) ) {
    if ($opt =~ m/--([-A-Za-z0-9_]+)/) {
        my $sopt=$1;
        if (!defined ($options{"$sopt=s"}) and
            !defined ($options{"$sopt=i"}) and
            !defined ($options{"$sopt=f"}) and
            !defined ($options{"$sopt"}) ) {
            warn "$sopt in help but not a legal option.";
            $err++;
        }
    }
}
if ($err) {
    die;
}

#-------------------------------------------------------------------------------
#       Global variables, paths and configuration
#-------------------------------------------------------------------------------

GetOptions ( %options ) or usage_exit("");

#check options
# check CAP values
convertcaps();
foreach my $cap (@cap_values) {
    if ($cap <= 1e-17 or $cap >= 1e-3) {
        print "Cap of $cap is unreasonable.\n";
        $err++;
    }
}
foreach my $tau (@slew_rates) {
    if ($tau < 1 or $tau > 10000) {
        print "Slew Rate of ${tau}pS is not valid.\n";
        $err++;
    }
}

foreach my $tau (split(/,/,$delayTau)) {
    if ($tau < 1 or $tau > 10000) {
        print "Slew Rate of ${tau}pS is not valid.\n";
        $err++;
    }
}

usage_exit("bad pdk $pdk_dir") if ( ! -d $pdk_dir );

$spec = $spec_file;
$spec="" if ! defined ($spec);
usage_exit ("Spec file not defined or readable $spec")
    if ( ! ( -f $spec ) or ! ( -r $spec ) ) and $tasks{pathalyze};

usage_exit("--cell arg missing")
    if ($cell_name eq "");

usage_exit("--cast-dir arg missing")
    if ( $cast_dir eq "");

usage_exit("no tasks specified")
    if ( !$tasks{pathalyze} and ! $tasks{sweep} and ! $tasks{lib} );

usage_exit("Too many cap or slew or delayTau errors to continue")
    if ($err);

usage_exit("LVE Sweep requires --spec-dir=$spec_dir")
    if ($tasks{sweep} and ! -d "$spec_dir");

usage_exit("Pathalyze requires --lve-dir and --spec-file")
    if ($tasks{pathalyze} and ("$lve_dir" eq "" or ! ( -f "$spec_file") ) );

usage_exit("die");

print "Parsing $spec.\n" if $progress;
parse_path_spec($spec);
#- get timing_sense and function from timing_sense '.ts' file.
foreach my $p (@param_defs) {
    my @f=@{$p};
    $param_defs{$f[0]}=$f[1];
}
print "Finding nodes to analyze.\n" if $progress;
my $nn=0;
foreach my $p (keys %path_defs) {
    local($")=",";
    my $exp="";
    my @var;
    my @start;
    my @stop;
    if ($p =~ m:\[([^\]]+)\]:) {
        $exp = $1;
        my @exp=split(/,/,$exp);
        foreach my $ex (@exp) {
            my ($var,$e1)=split(/:/,$ex);
            my $start;
            my $stop;
            if ($e1 =~ /\.\./) {
                $e1 =~ s/\.\./ /;
                ($start,$stop)=split(/ /,$e1);
            }
            else {
                $start = 0;
                $stop = $e1-1;
            }
            push (@var,$var);
            push (@start,$start);
            push (@stop,$stop);
        }
    }
    if (1) {
        my ($pb,@ex)=expand_path($p,$path_defs{$p});
        foreach my $expref (@ex) {
            my $expname=${$expref}[0];
            my $path=${$expref}[1];
            foreach my $node (@{$path}) {
                $node =~ s/^\(//;
                $node =~ s/[-\+]$//;
                $node =~ s/\)$//;
                $specnodes{$node}=1;
            }
        }
        $nn++;
    }
    else {
        my $nnode=-1;
        foreach my $node (@{$path_defs{$p}}) {
            $nnode++;
            next if ($nnode == 0);
            $node =~ s/[-\+]$//;
            $node =~ s/^\(//;
            $node =~ s/\)$//;
            if ($node =~ m:\[([^\]]+)\]:) {
                if ($#var >= 1) {  # ignore if no variable
                    my (@nexpr) = split(/,/,$1);
                    my $n1 = $node;
                    $n1 =~ s/\[.*//;
                    for (my $i = $start[0]; $i <= $stop[0]; $i++) {
                        my @pexpr=@nexpr;
                        for (my $endx = 0; $endx <= $#nexpr; $endx++) {
                            $pexpr[$endx] =~ s/$var[0]/$i/g;
                        }
                        if (defined ($start[1])) {
                            for (my $j = $start[1]; $j <= $stop[1]; $j++) {
                                my @qexpr = @pexpr;
                                for (my $endx = 0; $endx <= $#nexpr; $endx++) {
                                    $qexpr[$endx] =~ s/$var[1]/$j/g;
                                    eval "\$qexpr[\$endx] = int($qexpr[$endx])";
                                }
                                print "ADD $n1"."[@qexpr]\n" if $debug;
                                $specnodes{$n1."[@qexpr]"} = 1;
                            }
                        }
                        else {
                            print "ADD $n1"."[@pexpr]\n" if $debug;
                            $specnodes{$n1."[@pexpr]"} = 1;
                        }
                    }
                }
            }
            else {
                #print "NOMATCH $node\n";
                print "ADD $node\n" if $debug;
                $specnodes{$node}=1;
            }
        }
    }
}
foreach my $node (sort keys %specnodes) {
    next if ($lve_mode eq "estimated" and $node =~ m:/:);
    print "SPECNODE $node\n" if $debug;
    my $xnode=$node;
    $xnode =~ s:/:.:g;
    $specpins{$xnode}=1;
}

#-------------------------------------------------------------------------------
#       Define input and output pins.
#-------------------------------------------------------------------------------

if ( ! getnodes ) {
    die "Error: No nodes found!";
}

readbind ($bind_file);

@outnodes      = get_outputs();
@innodes       = get_inputs();
@localnodes    = get_localnodes();

#-------------------------------------------------------------------------------
#       Program
#-------------------------------------------------------------------------------

#- generate library.
lve_sweep() if ($tasks{sweep});
pathalyze() if ($tasks{pathalyze});
gen_library() if ($tasks{lib});

#-------------------------------------------------------------------------------
# Parses path timing specification, sets path_defs and param_defs global
# maps.
#-------------------------------------------------------------------------------
sub parse_path_spec {
    my ($file) = @_;
    open (SPEC, $file) || die "Couldn't read $file.\n";
    my $dir = "";
    if ($file =~ /^(.*)\/[^\/]+$/) {
        $dir = $1;
    }
    my $line = "";
    my $num = 0;
    my @include_files = ();
    while (<SPEC>) {
        $line .= $_; chomp $line;
        $line =~ s/\s*(\#.*)?$/ /;
        $num++;
        if ($line =~ s/\s*;\s*$//) {
            $line =~ s/^\s*//;
            if ($line =~ /^path\s+([\w\[.,:\]]+)\s*=\s*(.*)$/) {
                my $pth=$1;
                my $spc=$2;
                @{$path_defs{$pth}} = split /\s+/, $spc;
                @{$path_defs_names{$pth}} = split /\s+/, $spc;
                print "PATH $pth @{$path_defs{$pth}}\n" if $debug;
                my $abr=$pth;
                $abr =~ s/ *\[.*//;
                if ($abr ne $pth) {
                    @{$path_defs_names{$abr}} = split /\s+/, $spc;
                }
            }
            elsif ($line =~ /^param\s+(\w+)\s*=\s*(.*)$/) {
                my $param=$1;
                my $defn=$2;
                $defn =~ s/\s+//g;
                push @param_defs, [ $param, $defn ];
                $defn =~ s/[a-zA-Z]*\(/ /g;
                $defn =~ s/,/ /g;
                $defn =~ s/\)//g;
                $defn =~ s/  */ /g;
                $defn =~ s/^ //;
                my @defn = split(/ /,$defn);
                foreach my $n (1..$#defn) {
                    if ($defn[0] eq $defn[$n]) {
                        print "Warning: $file: duplicate path or parameter in $param\n";
                    }
                }
                $"=";";
            }
            elsif ($line =~ /^include\s+(\S+)$/) {
                if ($dir eq "" || $1 =~ /^\//) {
                    push @include_files, $1;
                }
                else {
                    push @include_files, "$dir/$1";
                }
            }
            elsif ($line =~ /^cell /) {
                # ignore cell line
            }
            elsif ($line !~ /^\s*$/) {
                warn "Unrecognized syntax in statement ending on line $num " .
                     "in $file\n";
            }
            if ($line =~ /\s+path\s+/ || $line =~ /\s+param\s+/ ||
                $line =~ /\s+include\s+/) {
                die "Error: Semicolon missing in statement ending on line " .
                    $num . " in $file\n";
            }
            $line = "";
        }
    }
    close SPEC;
    foreach my $inc (@include_files) {
        parse_path_spec($inc);
    }
}
