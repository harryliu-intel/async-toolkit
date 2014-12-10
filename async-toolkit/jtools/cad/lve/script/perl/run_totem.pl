#!/usr/intel/bin/perl -l
# AAG
# $Id$
# $DateTime$

use strict;
use Getopt::Long;

my $cast_dir="";
my $spec_dir="";
my $fqcn="";
my $env="digital";
my $true="0.9";
my $view="layout";
my $corner="tt";
my $temp="125";
my $lve_dir="";
my $work_dir="";
my $fulcrum_pdk_root="";
my $measure_nodes;
my $prsDelay=200e-12;
my $capLoad=0;
my $print_nodes;
my $portprops="";
my $dynsimulationtime="6n";
$ENV{TOT_SCRIPT}="tot" if ! defined $ENV{TOT_SCRIPT};

sub usage {
    my ($msg)=@_;
    print STDERR "$msg" if defined $msg;
    print STDERR "Usage: run_totem <options>";
    exit 1;
}

sub my_system {
    my @cmd=@_;
    print join(" ", @cmd);
    system(@cmd);
}

GetOptions (
    "cast-dir=s" => \$cast_dir,
    "spec-dir=s" => \$spec_dir,
    "fqcn=s" => \$fqcn,
    "env=s" => \$env,
    "true=s" => \$true,
    "view=s" => \$view,
    "work-dir=s" => \$work_dir,
    "lve-dir=s" => \$lve_dir,
    "corner=s" => \$corner,
    "temp=s" => \$temp,
    "print-nodes=s" => \$print_nodes,
    "measure-nodes=s" => \$measure_nodes,
    "fulcrum-pdk-root=s" => \$fulcrum_pdk_root,
    "prs-delay=s" => \$prsDelay,
    "cap-load=s" => \$capLoad,
    "port-props=s" => \$portprops,
    "dynamic-simulation-time=s" => \$dynsimulationtime,
) or usage();
$true =~ s/V//;
$temp =~ s/C//;
my $gdsfqcn=`echo "$fqcn" | rename --type=cell --from=cast --to=gds2`;
chomp $gdsfqcn;

mkdir "$work_dir";
mkdir "$work_dir/dynamic_run";
mkdir "$work_dir/design_data";
mkdir "$work_dir/design_data/aplmmx_dynamic";
mkdir "$work_dir/design_data/ploc";
# note lve totem-mode extract has been run

my $gdsfqcn = $gdsfqcn;
my $fqcn_path = $fqcn;
$fqcn_path =~ s/\./\//g;

### aplmmx.config ###
my $file = "$work_dir/design_data/aplmmx_dynamic/aplmmx.config";
open (FH, "> $file") or die "Cannot open $file";
print FH<<EOF;
# Top cell
TOP_CELL  $gdsfqcn

# Nets which need to be analyzed
VDD_NETS {
 Vdd  $true
}
GND_NETS {
 GND  0
}

# Voltage sources in the spice testbench
VOLTAGE_SOURCES {
 GND   V0m
 Vdd   V3m
}

# macro type memory/analog
MACRO_TYPE    analog

#netlist with XY location
SPICE_NETLIST    $lve_dir/$fqcn_path/$view/totem/cell.spf

# Spice Model library
DEVICE_MODEL_LIBRARY    $fulcrum_pdk_root/share/Fulcrum/apache/spice.lib $corner

#Type of spice simulator
SPICE_SIMULATOR         hsim

#Spice simulation output waveform
# the following was to be just temporary to fix aplmmx hanging issue, change back to fsdb if fixed
SIM_OUTPUT_FILE      $lve_dir/$fqcn_path/$view/totem/hsim/$env/$corner/${true}V/${temp}C/10ns/hsim.ta0

# Define state name and capture window to use from simulation
# Windows of interest from above spice waveform
CUSTOM_STATE_SIM_TIME {
# <state> <start time> <end time>
WRITE "" "" 4e-9 10e-9
}

GDS_FILE $lve_dir/$fqcn_path/$view/cell.gds2
GDS_MAP_FILE $fulcrum_pdk_root/share/Fulcrum/apache/gds_layer.map

# DSPF_NETLIST ../simulation/chip_D_alta_D_fsched_D_esched_D_lock_D_LOCK_L_76_R__D_1000.spf.mod ../gds/chip_D_alta_D_fsched_D_esched_D_lock_D_LOCK_L_76_R__D_1000.placement_info.mod
DSPF_NETLIST $lve_dir/$fqcn_path/$view/totem/cell.spf $lve_dir/$fqcn_path/$view/totem/cell.placement_info

EOF
close FH;
chmod (0644, "$file");


### run_dynamic.tcl ###
my $file = "$work_dir/dynamic_run/run_dynamic.tcl";
open (FH, "> $file") or die "Cannot open $file";
print FH<<EOF;
# Import and design
import gsr ./dynamic.gsr 

# Setup the design
setup design

# Calculate power
perform pwrcalc

# Power grid extraction with capacitance
perform extraction -power -ground -c

setup  pad  -power -r 0.0  -c 0.0  -ground -r 0.0  -c 0.0
setup  wirebond  -power -r 0.00 -c 0.0 -l 0.0  -ground -r 0.00 -c 0.0 -l 0.0
setup  package  -r 0.0  -c 0.0  -l 0.0

# Dynamic IR analysis
perform analysis -dvd

#Export db
export db dynamic.db

EOF
close FH;
chmod (0775, "$file");


### irdrop.gds.config ###
my $file = "$work_dir/dynamic_run/irdrop.gds.config";
open (FH, "> $file") or die "Cannot open $file";
print FH<<EOF;
# Top cell name
TOP_CELL  $gdsfqcn

# Cell type memory/analog
MACRO_TYPE     analog

# GDS
GDS_FILE $lve_dir/$fqcn_path/$view/cell.gds2
# Layer map
GDS_MAP_FILE $fulcrum_pdk_root/share/Fulcrum/apache/gds_layer.map

# XY location file from aplmmx
XTOR_MAP_FILE  $work_dir/design_data/aplmmx_dynamic/$gdsfqcn.gdsmmx

# nets which needs to be traced
VDD_NETS {
 Vdd {
  Vdd
  }
} 
GND_NETS {
  GND {
    GND
  }
}

USE_TOP_LEVEL_TEXT_ONLY 1
GENERATE_PLOC USE_TEXT_LABEL

CONTACT_RESISTANCE      1
SNAP_THRESHOLD_DISTANCE 1.4

EOF
close FH;
chmod (0644, "$file");



### dynamic.gsr ###
my $file = "$work_dir/dynamic_run/dynamic.gsr";
open (FH, ">$file") or die "Cannot open $file";
print FH<<EOF;
# GDS  
GDSII_FILE {
  $gdsfqcn ./irdrop.gds.config full top
}
NATIVE_GDS_SUPPORT 1

# APL
APL_FILES {
  ../design_data/aplmmx_dynamic/$gdsfqcn.spcurrent current
  ../design_data/aplmmx_dynamic/$gdsfqcn.cdev cdev
}

# Vdd and GND
GND_NETS {
  GND  0
}
VDD_NETS {
  Vdd  $true
}

# Tech
TECH_FILE  $fulcrum_pdk_root/share/Fulcrum/apache/tsmc28.tech
GSC_FILE  $fulcrum_pdk_root/share/Fulcrum/apache/cell_mmx.gsc

# Temp
TEMPERATURE    $temp

# Pads and labels
ADD_PLOC_FROM_TOP_DEF    1

# Dynamaic stuff FIXME
CMM_CREATE_PINS 1
DYNAMIC_SIMULATION_TIME $dynsimulationtime
DYNAMIC_TIME_STEP 10p
FREQ 166666667

# EM
EM_MODE avg
TEMPERATURE_EM    110
EM_REPORT_PERCENTAGE   0
EM_REPORT_LINE_NUMBER   100000

# IR Drop
TOTEM_IR_REPORT_LINE_NUMBER   100000
VIA_IR_REPORT   1

EOF
close FH;
chmod (0775, "$file");

chdir "$work_dir/design_data/aplmmx_dynamic";
my $rv=my_system "$ENV{TOT_SCRIPT} aplmmx -s aplmmx.config";

$file="$gdsfqcn.aplmmx";
my $mod_file="$gdsfqcn.aplmmx_mod";
open (FH, "<$file") or die "Cannot open $file";
open (FH_MOD, "> $mod_file") or die "Cannot open $mod_file";
while (<FH>) {
  chomp;
  s/v3m/V3m/g;
  s/v5m/V5m/g;
  s/xcell/Xenv.Xtest/;
  print FH_MOD;
}
close FH;
close FH_MOD;

# hsim
my $out_dir="$lve_dir/$fqcn_path/$view/totem/hsim/$env/$corner/${true}V/${temp}C/10ns";
my @nodes=();
open P, "<$portprops";
while (<P>) {
    chomp;
    if (/^\+(\S+)/) {
        my $nd=`echo "$1" | rename --type=node --from=cast --to=gds2`;
        chomp $nd;
        push @nodes, $nd;
    }
}
if ($capLoad > 0 and @nodes) {
    $capLoad="--cap-load='$capLoad'";
    $capLoad .= " --out-nodes='".join(",", @nodes)."'";
}
else {
    $capLoad="";
}
my_system ("mkdir -p '$out_dir'");
my_system ("hsim --run-directory='$work_dir/hsim' --fulcrum-pdk-root='$fulcrum_pdk_root' --cell-spice-file='$lve_dir/$fqcn_path/$view/totem/cell.spf' --env-spice-file='$lve_dir/$fqcn_path/jflat.routed/hsim/$env' --extra-includes='$work_dir/design_data/aplmmx_dynamic/$gdsfqcn.aplmmx_mod' --accurate=0 --hsim-vdd=2 --prs-cap=1e-13 --prs-delay=$prsDelay --prs-min-res=30 --prs-max-res=1e9 --rc-reduction=1 --minC=5e-18 --minR=10 --run-time=10e-9 --process-corner=$corner --vdd=$true --print-nodes='Xenv.*,Xenv.Xtest.*' --measure-nodes='$measure_nodes' --delete=0 --lve-root-dir='$lve_dir' --totem-mode=1 --sub-lve-root-dir='$lve_dir' --output-file='$out_dir/hsim.out' --temp=$temp $capLoad '$gdsfqcn' '${gdsfqcn}_U_env' 1>'$out_dir/hsim.tmp' 2>'$out_dir/hsim.err'");
#my_system("cp -p '$work_dir/hsim/hsim.mt' '$out_dir/hsim.mt'");
my_system("cp -p '$work_dir/hsim/hsim.fsdb' '$out_dir/hsim.fsdb'");
# the following (thru close FA0) was to be just temporary to fix aplmmx hanging issue
#my_system("tot wvconv -fsdb2raw '$work_dir/hsim/hsim.fsdb' '$out_dir/hsim.ta0'");
open (FT, "<$work_dir/hsim/hsim.tbl");
open (FAO, ">$out_dir/hsim.ta0");
while (<FT>) {
    chomp;
    s/\s+/ /g;
    if (/^Variables:\s*(.*)/) {
        print FAO "Variables:";
        print FAO $1;
        next;
    }
    print FAO;
}
close FT;
close FAO;
chdir("$work_dir/design_data/aplmmx_dynamic");
my_system("$ENV{TOT_SCRIPT} aplmmx -n aplmmx.config");
#;## Check for errors
chdir("$work_dir/dynamic_run");
my_system("$ENV{TOT_SCRIPT} totem -b run_dynamic.tcl");
my_system("cp -p 'adsRpt/Dynamic/$gdsfqcn.ir.worst' '$out_dir/cell.ir_worst'");
my_system("cp -p 'adsRpt/Dynamic/$gdsfqcn.em.worst.avg' '$out_dir/cell.em_worst_avg'");
my $errs=`/bin/ls -tr adsRpt/Error/totem.err.* 2>/dev/null | tail -1`;
chomp $errs;
if ( -f $errs) {
    my_system("cp -p '$errs' '$out_dir/totem.err'");
}
# the following was to be just temporary anyway
unlink "$out_dir/hsim.ta0";
