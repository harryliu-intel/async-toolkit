#!/usr/intel/bin/perl -w

# find relevant packaged tools and libraries
BEGIN {
    $lve_root = $0;
    my $exe = $lve_root;
    $exe =~ s:.*/::;
    if (! ($lve_root =~ m:^/:)) {
        my $pwd = `pwd`;
        chomp $pwd;
        $lve_root = $pwd;
        $lve_root .= "/$0";
        $lve_root =~ s:$exe$::;
        $lve_root =~ s://:/:g;
        chdir $lve_root;
        $lve_root = `pwd`;
        chomp $lve_root;
        chdir $pwd;
    }
    else {
        $lve_root =~ s:/bin/$exe::;
    }
    @INC = ("$lve_root/lib/perl", @INC);
}
use LveUtil;

# variable declarations and defaults
my $sim="hspice";
my $out_nodes="";
my @out_nodes=();
my $totem_mode=0;
my @extra_includes=();
my @measure_nodes=();
my $xa_level = 5;
my $output_dir = ".";
my @pdk_include = ("spice/default.sp","spice/rc.sp"); # PDK files to include
my $run_directory = ".";
my $del = 0;
my $prscap    = 1e-13;
my $prsdelay  = 10e-12;
my $prsmaxres = 1e6;
my $prsminres = 10;
my $cap_load  = 0;
my $minR = 0.1;
my $minC = 1e-16;
my $process_corner = "tt";
my $seed = 0;
my $vdd = 0.9;
my $temp = 25;
my $sigma_factor = 1;
my $time           = 20e-9;
my $start_time     = 2e-9; # how long to reset circuit
my $measure_offset = 4e-9; # how much longer to wait before measuring
my $rc_reduce = 1;
my $reset_offset = 0;
my $power_window = "";
my $env_spice_file = "";
my $accurate = 0;

# TODO: spice model information should be in PDK
# NOTE: uses env $hspice_model_root $hspice_model $PDMI_LIB $hspice_lib_models

# usage banner
sub usage() {
    $usage  = "USAGE: hsim  [args] cell env\n";
    $usage .= "  Args includes:\n";
    $usage .= "    --cell-spice-file=<file-name>\n";
    $usage .= "    [--env-spice-file=<file-name>] unless self-oscillating\n";
    $usage .= "    [--run-directory=<dir-name>]\n";
    $usage .= "    --delete=$del\n";
    $usage .= "    --output-dir=<dir-name> (if not specified it places output in pwd)\n";
    $usage .= "    --minR=$minR\n";
    $usage .= "    --minC=$minC\n";
    $usage .= "    --vdd=$vdd\n";
    $usage .= "    --temp=$temp\n";
    $usage .= "    --rc-reduction=$rc_reduce\n";
    $usage .= "    --_RESET-env-cell-offset=[time in ns ] (offset between cell reset and env reset)\n";
    $usage .= "    --run-time=$time\n";
    $usage .= "    --process-corner=$process_corner\n";
    $usage .= "    --fulcrum-pdk-root=<path> (differs from tech to tech)\n";
    $usage .= "    --sim=$sim [hsim|xa|hspice]\n";
    $usage .= "    --accurate=$accurate\n";
    $usage .= "    --prs-cap=$prscap\n";
    $usage .= "    --prs-delay=$prsdelay\n";
    $usage .= "    --prs-max-res=$prsmaxres\n";
    $usage .= "    --prs-min-res=$prsminres\n";
    $usage .= "    --start-time=$start_time\n";
    $usage .= "    --measure-nodes=(, separated list of nodes with full subckt path specified)\n";
    $usage .= "    --power-window=(t1,t2) ( window for power measurements, default is 6ns to run-time)\n";
    $usage .= "    --lve-root-dir=<path> (Where is the lve root?)\n";
    $usage .= "    --sub-lve-root-dir=<path> (Where is the lve root?)\n";
    $usage .= "    --totem-mode (for running totem dynamic\n";
    $usage .= "    --sigma-factor (to vary corner limits, 0..1)\n";
    $usage .= "    --extra-includes (for files needed for running totem in hsim)\n";
    die "$usage";
}

# parse command line arguments
while (defined $ARGV[0] && $ARGV[0] =~ /^--(.*)/) {
    ($flag, $value) = split("=",$1);
    $value=1 if ! defined $value;
    if ($flag eq "cell-spice-file")  {
        $cell_spice_file= $value;
    } elsif ($flag eq "env-spice-file")  {
        $env_spice_file= $value;
    } elsif ($flag eq "run-directory")  {
        $run_directory= $value;
    } elsif ($flag eq "delete") {
        $del = $value;
    } elsif ($flag eq "output-dir")  {
        $output_dir= $value;
    } elsif ($flag eq "minR") {
      	$minR = $value;
    } elsif ($flag eq "minC") {
       	$minC = $value;
    } elsif ($flag eq "vdd") {
      	$vdd = $value;
    } elsif ($flag eq "hsim-vdd") {
      	$hsimVdd = $value;
    } elsif ($flag eq "power-window") {
      	$power_window = $value;
    } elsif ($flag eq "temp") {
       	$temp = $value;
    } elsif ($flag eq "accurate") {
       	$accurate = $value;
    } elsif ($flag eq "prs-cap") {
        $prscap = $value;
    } elsif ($flag eq "prs-delay") {
        $prsdelay = $value;
    } elsif ($flag eq "prs-max-res") {
        $prsmaxres = $value;
    } elsif ($flag eq "prs-min-res") {
        $prsminres = $value;
    } elsif ($flag eq "start-time") {
        $start_time = $value;
    } elsif ($flag eq "_RESET-env-cell-offset") {
       	$reset_offset = $value;
    } elsif ($flag eq "run-time") {
       	$time = $value;
    } elsif ($flag eq "rc-reduction") {
       	$rc_reduce = $value;
    } elsif ($flag eq "process-corner") {
      	$process_corner = $value;
    } elsif ($flag eq "fulcrum-pdk-root") {
       	$pdk_root = $value;
    } elsif ($flag eq "lve-root-dir") {
       	$lve_root_dir = $value;
    } elsif ($flag eq "sub-lve-root-dir") {
       	$sub_lve_root_dir = $value;
    } elsif ($flag eq "measure-nodes") {
        @measure_nodes = split(",",$value);
    } elsif ($flag eq "sim") {
        $sim = $value;
    } elsif ($flag eq "totem-mode") {
        $totem_mode = $value;
    } elsif ($flag eq "sigma-factor") {
        $sigma_factor = $value;
    } elsif ($flag eq "extra-includes") {
        @extra_includes=split(/:/,$value);
    } elsif ($flag eq "cap-load") {
        $cap_load=$value;
    } elsif ($flag eq "out-nodes") {
        @out_nodes=split(/,/,$value);
    } else {
        print "ERROR: argument --${flag}=${value} not recognized.\n";
	&usage();
    }
    shift @ARGV;
}

# usage
@ARGV == 2 or usage();
defined $pdk_root or die "Must specify fulcrum-pdk-root";
defined $cell_spice_file or die "Need an input spice description of a cell";

# derived
my $cell_name = $ARGV[0];
my $env_name = $ARGV[1];
my $escaped_cell_name="\Q$cell_name\E";
my $bsim = "tech";
my $Vlo="Vdd*0.45";
my $Vhi="Vdd*0.55";
my $V50="Vdd*0.5";
my $V10="Vdd*0.1";
my $V90="Vdd*0.9";
if ($accurate) { $xa_level = 7; }

# handle monte-carlo
if ($process_corner =~ /(\S+)_(\d+)/ ) {
    $process_corner = $1;
    $seed = $2;
}
my $monte_carlo = "";
$monte_carlo = "SWEEP MONTE=1" if ($seed>0);
my $option_seed = "";
$option_seed = ".option seed=$seed\n" if ($seed>0);

########################################## Run ##########################################

# start run_file
my_system("mkdir -p '$run_directory'");
chdir("$run_directory");
my $run_file = "run.sp";
open RUN_FILE,">$run_file";
print RUN_FILE "* $sim\n" .
    "* cell_file=$cell_spice_file\n" ."* env_file=$env_spice_file\n" .
    "* cell_name=$cell_name\n" . "* env_name=$env_name\n" . "\n";

#################################### HSIM specific ######################################

if ($sim eq "hsim") {
    print RUN_FILE<<EOF;
* HSIM options
.option PDMI_LIB='\$PDMI_LIB'
.option POST=fsdb PROBE=1
.option spice
.option warnlimit=20
.param HSIMOUTPUTTSTEP=10e-12
.param HSIMVDD=$hsimVdd
.param HSIMOUTPUT=fsdb

EOF
    if ($accurate) { # use super-accurate parameters
        print RUN_FILE<<EOF;
.param HSIMSPEED=0
.param HSIMSPICE=3
.param HSIMANALOG=3
.param HSIMFCM=2
.param HSIMAMOS=1
.param HSIMABMOS=1
.param HSIMBMOS=1
.param HSIMDIODECURRENT=2
.param HSIMCC=1
.param HSIMPOSTL=0
.param HSIMSTEADYCURRENT=1e-15
.param HSIMALLOWEDDV=1e-3
.dc temp poi 1 $temp

EOF
    } elsif ($rc_reduce==1) { # enable RC reductions and default accuracy
        print RUN_FILE<<EOF;
.param HSIMRMIN=$minR
.param HSIMVSRCRMIN=$minR
.param HSIMCMIN=$minC
.param HSIMPOSTL=1
.param HSIMRCRTAU=1p

EOF
    }
    if ($totem_mode) {
        print RUN_FILE ".param HSIMOUTPUTTBL=rawfile\n" if ($totem_mode);
        foreach my $file (@extra_includes) {
            print RUN_FILE ".include '$file'\n";
        }
    }
}

###################################### XA specific ########################################

if ($sim eq "xa") {
    print RUN_FILE<<EOF;
* XA options
.option POST=fsdb PROBE=1
.option XA_CMD="set_sim_level -level $xa_level"
.option XA_CMD="set_wildcard_rule -match* one"
.option XA_CMD="set_message_option -limit 100"
EOF
}
# sneaky bug: this option prevents waveforms from being written to the fsdb file
print RUN_FILE ".OPTION XA_CMD=\"set_monte_carlo_option -simulate_nominal 0\"\n" if ($seed>0);
print RUN_FILE "\n";

################################### HSPICE specific #######################################

if ($sim eq "hspice") {
    print RUN_FILE<<EOF;
* HSPICE options
.option POST=csdf PROBE=1
.option runlvl=5
.option warnlimit=20
.option dcon=1
.param ceil(x)='x<0 ? int(x) : int(x) < x ? int(x) + 1 : int(x)'

EOF
}

####################################### Common ############################################

# PDK include files
print RUN_FILE "* Include\n";
foreach my $file (@pdk_include) {
    my $f = "$pdk_root/share/Fulcrum/$file";
    print RUN_FILE ".include '$f'\n" if (-e $f);
}
print RUN_FILE "\n";

# standard settings and transistor models
print RUN_FILE<<EOF;
* Settings
.param Vdd=$vdd
.param temp=$temp
.temp $temp

* Model
${option_seed}.option search='\$hspice_model_root'
.lib '\$hspice_model' $process_corner
.param sigma_factor=$sigma_factor
EOF

# only turn on case-sensitivity after reading models!
if ($sim eq "xa") { print RUN_FILE ".option XA_CMD=\"set_sim_case -case sensitive\"\n"; }

# spice file search paths
if (defined($lve_root_dir) and -d "$lve_root_dir/spicelib") {
    print RUN_FILE ".option search='$lve_root_dir/spicelib'\n";
}
if (defined($sub_lve_root_dir) and -d "$sub_lve_root_dir/spicelib") {
    print RUN_FILE ".option search='$sub_lve_root_dir/spicelib'\n";
}

# capacitive load on outputs
if (@out_nodes and $cap_load > 0) {
    foreach my $node (@out_nodes) {
        print RUN_FILE "C$node Xenv.Xtest.$node 0 $cap_load\n";
    }
}

# handle start time, reset offset, power window
$time += $start_time;
my $time_cell=$start_time;
my $time_env=$start_time;
if ($reset_offset < 0)  { $time_cell = $time_cell - $reset_offset; }
else                    { $time_env  = $time_env  + $reset_offset; }
my $power_window_start = $time_cell + $measure_offset;
my $power_window_stop = $time;
if($power_window=~/(\d+)\,(\d+)/)  {
    $power_window_start = $1;
    $power_window_stop  = $2;
}

# common setup
print RUN_FILE<<EOF;

* Circuit
.include '$cell_spice_file'

* Power supplies
V0m GND      0 0
V1m Vdd      0 pwl (0 0 0.5ns Vdd)
V2m Vdd_env  Vdd 0
V3m Vdd_cell Vdd 0
V4m _RESET   0 pwl (0 0 $time_cell 0 '$time_cell+1ns' Vdd)

EOF

if (!($env_spice_file eq "")) {
## auto generated env file specified
    print RUN_FILE<<EOF;
* Environment
.include '$env_spice_file'
.param Vlo='$Vlo'
.param Vhi='$Vhi'
.param PrsCap=$prscap
.param PrsMaxRes=$prsmaxres
.param PrsMinRes=$prsminres
.param PrsDelay=$prsdelay
Xenv GND GND Vdd_env Vdd_cell _RESET _RESET $env_name

EOF
} else  {
## self oscillating input cell.spice
    print RUN_FILE<<EOF;
Xenv GND Vdd_cell _RESET $cell_name

EOF
}

## simulate
print RUN_FILE "* Simulate\n";
print RUN_FILE ".tran 1ps $time $monte_carlo\n\n";

### measure frequency and slew
foreach $node (@measure_nodes)  {
    print RUN_FILE "* Average Cycle time and Frequency over various intervals\n";
    for (my $n = 1; $n<=64; $n*=2) {
        my $end = 1+$n;
        print RUN_FILE ".probe v($node)\n";
        print RUN_FILE ".measure tran Cycle${n}_${node}\n";
        print RUN_FILE "+trig v(${node}) val='$V50' td=$power_window_start fall=1\n";
        print RUN_FILE "+targ v(${node}) val='$V50' td=$power_window_start fall=$end\n";
        print RUN_FILE ".measure tran Freq${n}_${node}\n";
        print RUN_FILE "+PARAM='$n/Cycle${n}_${node}'\n";
        print RUN_FILE "\n";
    }
    print RUN_FILE<<EOF;
* Rise/Fall time
.measure tran RiseTime_${node}
+trig v(${node}) val='$V10' td=$power_window_start rise=1
+targ v(${node}) val='$V90' td=$power_window_start rise=1
.measure tran FallTime_${node}
+trig v(${node}) val='$V90' td=$power_window_start fall=1
+targ v(${node}) val='$V10' td=$power_window_start fall=1

* Rise/Fall slew rate
.measure tran RiseSlew_${node}
+derivative v(${node})
+when v(${node})='$V50' td=$power_window_start rise=1
.measure tran FallSlew_${node}
+derivative v(${node})
+when v(${node})='$V50' td=$power_window_start fall=1

EOF
}

### branch V3m powering Circuit under Test
print RUN_FILE<<EOF;
* Power measurements
.probe i(V3m)
.measure tran avg_curr avg par('-i(V3m)') from=$power_window_start to=$power_window_stop
.measure tran max_curr max par('-i(V3m)') from=$power_window_start to=$power_window_stop
.measure tran avg_power PARAM='(avg_curr*Vdd)'
.measure tran max_power PARAM='(max_curr*Vdd)'

EOF

print RUN_FILE ".end\n";
close(RUN_FILE);

################################# Create run script #####################################

open SCRIPT,">run" or die "Can't write to run script";
print SCRIPT "#!/bin/bash\n";
print SCRIPT "export hspice_lib_models=\$hspice_lib_models\n";
print SCRIPT "export PDMI_LIB=\$PDMI_LIB\n";
if ($sim eq "hspice") {
    my $hsim_mc = "";
    $hsim_mc = "-monte 1" if ($seed>0);
    print SCRIPT "$ENV{HSP_SCRIPT} hspice -case 1 -i run.sp $hsim_mc > hspice.log 2>\&1\n";
} elsif ($sim eq "hsim") {
    print SCRIPT "$ENV{HSM_SCRIPT} hsim -case 1 -i run.sp\n";
} elsif ($sim eq "xa") {
    print SCRIPT "$ENV{XA_SCRIPT} xa run.sp\n";
}
close(SCRIPT);

################################## Run the script #######################################

my_system("chmod +x run");
my_system("$run_directory/run");

################################# Copy results back #####################################

if (!($output_dir eq $run_directory)) {
    my_system("cp -f run    '$output_dir/run'");
    my_system("cp -f run.sp '$output_dir/run.sp'");
    if ($sim eq "hspice") {
        my_system("cp -f run.mt0    '$output_dir/hspice.out'");
        my_system("cp -f hspice.log '$output_dir/hspice.log'");
        my_system("cp -f run.tr0    '$output_dir/hspice.csdf'");
    }
    elsif ($sim eq "hsim") {
        if ($seed>0) {
            my_system("cp -f hsim.mc.mt '$output_dir/hsim.out'");
        } else {
            my_system("cp -f hsim.mt '$output_dir/hsim.out'");
        }
        my_system("cp -f hsim.log  '$output_dir/hsim.log'");
        my_system("cp -f hsim.fsdb '$output_dir/hsim.fsdb'");
    }
    elsif ($sim eq "xa") {
        my_system("cp -f xa.meas '$output_dir/xa.out'");
        my_system("cp -f xa.log  '$output_dir/xa.log'");
        my $fsdb = $seed>0 ? 'xa.m1.fsdb' : 'xa.fsdb';
        my_system("cp -f $fsdb '$output_dir/xa.fsdb'");
    }
}

# cleanup
if ($del==1) {
    my_system("rm -rf '$run_directory'");
}
