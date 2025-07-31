#!/usr/intel/bin/perl
use strict;
use warnings;
use Getopt::Long;
use Pod::Usage;
use File::Spec::Functions qw/:ALL/;
use FindBin;
use Cwd qw/abs_path/;
use Cwd qw/chdir/;
use File::Spec::Functions;
use Scalar::Util qw(looks_like_number);
#use Scalar::MoreUtils qw( define );

#Make sure all the appropriate environment variables are defined
die "ERROR: undefined \$SPAR_DIR\n"  unless (defined($ENV{SPAR_DIR}));
die "ERROR: undefined \$CAST_PATH\n" unless (defined($ENV{CAST_PATH}));
die "ERROR: undefined \$VERILOG_DIR\n" unless (defined($ENV{VERILOG_DIR}));
die "ERROR: undefined \$SOURCE_VERILOG_DIR\n" unless (defined($ENV{SOURCE_VERILOG_DIR}));


# Command line options for simulation
my ($cell, $env, $level, $corner, $outdir, $simv, $tcl, $t_run, $t_reset, $perf, $depth,
    $fsdb, $verdi, @simv_args, $verbose, $help);
# Command line options to pass through to gen_model
my (@cast_defines, @c2v_args, @gen_args, @vcs_args, @power);

#Now collect the remaining options (if applicable)
GetOptions("level=s"     => \$level,
           "corner=s"    => \$corner,
           "outdir=s"    => \$outdir,
           "simv=s"      => \$simv,
           "tcl=s"       => \$tcl,
           "t-run=s"     => \$t_run,
           "t-reset=s"   => \$t_reset,
           "perf=s"      => \$perf,
           "power=s"     => \@power,
           "depth=s"     => \$depth,
           "define=s"    => \@cast_defines,
           "c2v-args=s"  => \@c2v_args,
           "gen-args=s"  => \@gen_args,
           "vcs-args=s"  => \@vcs_args,
           "simv-args=s" => \@simv_args,
           "fsdb=s"      => \$fsdb,
           "verdi!"      => \$verdi,
           "verbose!"    => \$verbose,
           "help!"       => \$help) || pod2usage(2);

pod2usage(-verbose => 1) if $help;

#Process (and error check) the remaining ARGV (which should be just 1)
if (@ARGV < 1) {die "ERROR: Missing required argument \"cell_fqcn:env\"\n";}
else {
    #The first argument should be of the form "cell_fqcn:env"
    my $first_arg = shift @ARGV;
    #Check if ':' appears exactly once in the first argument
    if ($first_arg =~ tr/:// != 1) {die "ERROR: First argument \"$first_arg\" should be of the form \"cell_fqcn:env\"\n";}
    ($cell, $env) = split(/:/, $first_arg);
}
if (@ARGV != 0) {die "ERROR: There are remaining unknown arguments: @ARGV \n";}


# If outdir is provided, check to make sure it exists
my $wd;
if ($outdir) {
    #Working directory is specified
    if (-d $outdir) {$wd = $outdir;}
    else {die "Specified output directory \"$outdir\" does not exist"}
    if ($verbose) {print "Working directory provided: $outdir\n";}
}


#Check if simv was specified and exists
#NOTE: simv is not relative to the workding directory
if ($simv) {
    #An existing simv is specified, use that for simulation
    if ($outdir) {
        if (! -e catfile($outdir, $simv)) {die "Specified simv in output directory \"$outdir/$simv\" does not exist";}
    }
    else {        
        if (! -e $simv) {die "Specified simv \"$simv\" does not exist";}
    }
}

#If verdi and output directory specified, skip compile and run verdi directly, 
if ($verdi && $fsdb) {
    #check if fsdb file exists
    my $verdi_com;
    if ($outdir) {
        if (! -e catfile($outdir, $fsdb)) {die "ERROR: Can't open FSDB file \"$outdir/$fsdb\"";}
        if ($simv) {$verdi_com = "vcs verdi3 verdi -nologo -ssf $outdir/$fsdb -dbdir $outdir/${simv}.daidir &";}
        else {$verdi_com = "vcs verdi3 verdi -nologo -ssf $outdir/$fsdb -dbdir $outdir/simv.daidir &";}
    }
    else {
        if (! -e $fsdb) {die "ERROR: Can't open FSDB file \"$fsdb\"";}
        if ($simv) {$verdi_com = "vcs verdi3 verdi -nologo -ssf $fsdb -dbdir ${simv}.daidir &";}
        else {$verdi_com = "vcs verdi3 verdi -nologo -ssf $fsdb -dbdir simv.daidir &";}
    }
    if ($verbose) {print("$verdi_com\n");}
    system($verdi_com)==0 or die "ERROR: verdi failed\n";
    exit 0;
}

#Figure out cosimulation specification
my $cosim;
my $gls_dir;
my $env_cosim="{subcells,prs,csp-standard.attributes.csp_model{csp}}";
if (!$level) {
    print "WARNING: No simulation level specified, defaulting to \"lo\"\n";
    $level = "lo";
}
if ($level eq "hi") {
    $cosim= "{csp,subcells,prs-standard.attributes.prs_model{subcells,prs,csp}}";
}
elsif ($level eq "lo") {
    $cosim= "{verilog.rtl,verilog.cast2rtl,subcells,prs,csp-standard.attributes.verilog_model{verilog.rtl,verilog.cast2rtl,verilog.gate,subcells,prs}-standard.attributes.csp_model{csp}-standard.attributes.prs_model{subcells,prs,csp}}";
}
elsif ($level eq "fpga") {
    $gls_dir= "$ENV{VERILOG_DIR}/fpga";
    $cosim= "{verilog.rtl,verilog.gate,subcells,prs,csp-standard.attributes.csp_model{csp,subcells,prs}-standard.attributes.verilog_model{verilog.rtl,verilog.gate,subcells,prs}}";
}
elsif ($level eq "syn") {
    $gls_dir= "$ENV{VERILOG_DIR}/syn";
    $cosim= "{verilog.rtl,verilog.gate,subcells,prs,csp-standard.attributes.csp_model{csp,subcells,prs}-standard.attributes.verilog_model{verilog.rtl,verilog.gate,subcells,prs}}";
}
elsif ($level eq "apr" || $level eq "sdf") {
    $gls_dir= "$ENV{VERILOG_DIR}";
    $cosim= "{verilog.rtl,verilog.gate,subcells,prs,csp-standard.attributes.csp_model{csp,subcells,prs}-standard.attributes.verilog_model{verilog.rtl,verilog.gate,subcells,prs}}";
    if ($level eq "sdf" && !$corner) {
        print "WARNING: No corner specified for sdf simulation. Default corner \"max\" will be used\n";
        $corner= "max";
    }
}
else { die "ERROR: Unrecognized level $level. Must be {\"hi\", \"lo\", \"fpga\", \"syn\", \"apr\", \"sdf\"}"; }

if ($verbose) {print "Cell=$cell, Cosim=$cosim\n";}
my $cosim_spec = "$cell$cosim:$env";


#Check if tcl file was specified and exists
if ($tcl) {
    #An existing tcl file is specified, use that for simulation
    if (-e $tcl) {$tcl = abs_path($tcl);}
    else {die "Specified simv \"$tcl\" does not exist";}
}

#If output directory was not specified, create one
if (!$outdir) {
    my $curtime = `workweek --format="%Y%m%d_%H%M"`;
    chomp($curtime);
    my $cell_base = (split(/\./, $cell))[-1];
    if (looks_like_number($cell_base)) {
        $cell_base = (split(/\./, $cell))[-2];
    }
    if ($level eq "sdf") {
        $wd = "vcs.${cell_base}_${env}.${level}_${corner}.${curtime}";
    }
    else {
        $wd = "vcs.${cell_base}_${env}.${level}.${curtime}";
    }
    mkdir $wd or die "Can't mkdir $wd: $!\n";
    chmod 0750, $wd;
    system("rm -if vcs_latest; ln -s $wd vcs_latest") == 0 or die "Symlink failed\n";
}
if ($verbose) {print "Changing to working directory: $wd\n";}
chdir($wd) or die "$!";

my $fulcrum = "fulcrum";
$fulcrum = $ENV{MY_FULCRUM} if (defined($ENV{MY_FULCRUM}));

my $instdir = $ENV{'FULCRUM_PACKAGE_ROOT'};
my $runtime = "$instdir/share/cast2verilog";

if (!$t_reset) {
    if ($verbose) {print "WARNING: No reset duration specified, defaulting to 10us\n";}
    $t_reset = "10us";
}
if (!$t_run) {
    if ($verbose) {print "WARNING: No run time specified, defaulting to 100us\n";}
    $t_run = "100us";
}

#Add t_reset to vcs_args
push @vcs_args, "+define+CAST2VERILOG_RESET_DURATION=$t_reset";

#If no simv was specified, generate verilog and compile it
if (!$simv) {
    #No simv specified, will need to generate one
    #First: generate the verilog model
    my $gen_cmd="$fulcrum --latest gen_model --width=1024 --beh --kdb " .
        "--cast-path=\"$ENV{CAST_PATH}\" " .
        "--spar-dir=\"$ENV{SPAR_DIR}\" " .
        "--cosim=\"$cosim_spec\" " .
        join('', map { "\Q$_\E " } @gen_args) .
        join('', map { "--define=\Q$_\E " } @cast_defines) .
        join('', map { "--vcs-args=\Q$_\E " } @vcs_args) .
        join('', map { "--c2v-args=\Q$_\E " } @c2v_args) .
        "--define=standard.attributes.bd_controller.ExtraTimingMargin:10000 " .
        "--define=standard.attributes.standard_cell.USE_LIBERTY_PRS:true " ;
    if (@power) {
        $gen_cmd .= join('', map { "--power=\Q$_\E " } @power);
    }
    if ($gls_dir) {
        $gen_cmd .= "--gls-dir=$gls_dir ";
        if ($level eq "sdf") {
            $gen_cmd .= "--sdf=$corner ";
        }
        if ($perf) {
            $gen_cmd .= "--perf ";
        }
    }
    print "Running gen_model...\n";
    if ($verbose) {print("$gen_cmd\n");}
    system($gen_cmd)==0 or die "ERROR: gen_model failed in $wd\n";

    #Second: compile the verilog model to a simv file
    print "Running run-vcs...\n";
    my $com_cmd = "verdi3 vcs ./run-vcs.sh >& vcs.log";
    if ($verbose) {print("$com_cmd\n");}
    system($com_cmd)==0 or die "ERROR: Compiling model failed in $wd\n";

    $simv = "./simv";
}

#If no fsdb name was specified, default to trace.fsdb
if (!$fsdb) {
    if ($verbose) {print "WARNING: No FSDB file specified, defaulting to trace.fsdb\n";}
    $fsdb = "trace.fsdb";
}

#If no tcl file was specified, create default one
if (!$tcl) {
    $tcl = "./run.tcl";
    open TCL, ">$tcl" or die;
    if ($level eq "sdf") {
        print TCL "source $runtime/sdf_workarounds.tcl\n";
    }
    print TCL <<EOF;
fsdbDumpfile $fsdb
fsdbDumpvars 0 TESTBENCH +all
EOF
    if ($level eq "sdf") {
        print TCL "reset_tcheck $t_reset\n";
    }
    print TCL <<EOF;
run ${t_run}
exit
EOF
}

#Run the simulation
print "Running simulation...\n";
my $sim_cmd = "verdi3 $simv -fgp=num_threads:4 +vcs+initreg+0 +fsdb+delta -ucli -do $tcl @simv_args >& sim.log";
print("$sim_cmd\n");
system($sim_cmd)==0 or die "ERROR: Simulation failed in $wd\n";

#If perf, run critical path analysis
if ($perf) {
    my $crit_cmd = "./run-crit.sh $perf";
    if ($depth) { $crit_cmd .= " -d $depth"; }
    print "Running critical path analysis...\n";
    if ($verbose) {print("$crit_cmd\n");}
    system($crit_cmd)==0 or die "ERROR: critical.py failed in $wd\n";
}

#if verdi is specified after compile and simulation, run verdi in the output directory
if ($verdi) {
    print "Running Verdi...\n";
    my $verdi_com = "vcs verdi3 verdi -nologo -ssf $fsdb -dbdir ${simv}.daidir &";
    if ($verbose) {print("$verdi_com\n");}
    system($verdi_com)==0 or die "ERROR: verdi failed\n";
    exit 0;
}

__END__
=head1 NAME

sim_block.pl - Wrapper for compiling and running Verilog VCS models

=head1 SYNOPSIS

sim_block.pl FQCN:ENV [options]

 Options:
   --level         The level of simulation {"hi", "lo", "fpga", "syn", "apr", "sdf"}
   --corner        Select which SDF file to use (if running sdf level simulation). Default is "max"
   --outdir        Output directory for simulation files
   --simv          Specify the simv file to execute (skip compiling a new one)
   --tcl           Specify the tcl file to run. Defaults to run.tcl
   --t-run         Specify the run time. Defaults to 50us
   --perf          Specify starting node and time for critical path analysis
   --power         Specify name and node to monitor for PTPX power analysis
   --verdi         Run verdi after simulation (or open an existing FSDB)
   --fsdb          Specify the name of the FSDB file. Defaults to trace.fsdb
   --define        Override CAST variables; can be specified any number of times
   --c2v-args      Flags to cast2verilog; can be specified any number of times
   --vcs-args      Flags to VCS; can be specified any number of times
   --gen-args      Flags to gen_model; can be specified any number of times
   --simv-args     Flags to simv; can be specified any number of times
   --help          Prints this help message
=cut
