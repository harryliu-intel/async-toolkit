#!/usr/intel/bin/perl -w
# $Id$
# $DateTime$
# $Author$
use strict;


use Cwd;
use Cwd 'abs_path';
use IPC::Open2;
use File::Spec;

my $pwd=getcwd();
my $working_dir = "$pwd";
my $gdsii="";
my $gdsii_list="";
my $icv_options="";
my @include_before=();
my $threads=2;
my $mem=1;
my $jobs=0;
my $flow="drcd";
my $pdk_root="";
my $icv_runset_path="$ENV{PDK_CPDK_PATH}/runsets/icvtdr";
my $oasis = 1;
my $format = "GDSII";

# map flows to runsets
my %default_runset;
$default_runset{""}="drcd";
$default_runset{"lden"}="denall";
$default_runset{"gden"}="denall";
$default_runset{"cden"}="denall";
$default_runset{"cmden"}="denall";

sub usage {
    my ($msg) = @_;
    print STDERR "$msg\n" if defined $msg;
    my $usage  = "Usage: drc [args] cell\n";
    $usage .= "    --working-dir=[$working_dir]\n";
    $usage .= "    --icv-runset-path=[$icv_runset_path] (DRC runset path)\n";
    $usage .= "    --gds2-file=[$gdsii] (Provide source layout by gdsii file.)\n";
    $usage .= "    --gds2-list=[$gdsii_list] (Provide source layout by a file list contains gdsii file)\n";
    $usage .= "    --icv-options=[$icv_options] (Extra ICV command options)\n";
    $usage .= "    --include-before=[@include_before] (Prepend directory to include search path)\n";
    $usage .= "    --threads=[$threads] (ICV thread)\n";
    $usage .= "    --jobs=[$jobs] (Netbatch jobs; if specified, --threads is per job)\n";
    $usage .= "    --mem=[$mem] (Memory in GB, if Netbatch enabled)\n";
    $usage .= "    --flow=[$flow] (DRC runset selection. Default will run $flow)\n";
    $usage .= "    --oasis=[$oasis] (output oasis format)\n";
    $usage .= "    --fulcrum-pdk-root=[$pdk_root]\n";
    $usage .= "    --help (Shows this menu)\n";
    die "$usage\n";
}


while (defined $ARGV[0] and $ARGV[0] =~ /^--(.*)/) {
    my ($flag, $value) = split("=",$1,2);
    $value=1 if ! defined $value;
    if ($flag eq "working-dir") {
            $working_dir = $value;
    } elsif ($flag eq "gds2-file") {
            $gdsii = $value;
    } elsif ($flag eq "gds2-list") {
            $gdsii_list = $value;
    } elsif ($flag eq "threads") {
        $threads = $value;
    } elsif ($flag eq "jobs") {
        $jobs = $value;
    } elsif ($flag eq "mem") {
        $mem = $value;
    } elsif ($flag eq "flow") {
        $flow = $value;
    } elsif ($flag eq "oasis") {
        $oasis = $value;
    } elsif ($flag eq "icv-runset-path") {
        $icv_runset_path = $value;
    } elsif ($flag eq "icv-options") {
        $icv_options = $value;
    } elsif ($flag eq "include-before") {
        unshift @include_before, $value;
    } elsif ($flag eq "fulcrum-pdk-root") {
            $pdk_root = $value  if(defined $value);
    } elsif ($flag eq "help") {
        &usage();
    } else {
        print STDERR "Error: argument --${flag}=${value} not recognized.\n";
        &usage();
    }
    shift @ARGV;
}

my %drc_runsets;
my @flows=split(',',$flow);

@ARGV == 1 or usage("No Cell");
my $cell_name = shift;
-d $working_dir ne "" or $working_dir = $pwd;
chomp $working_dir;
$pdk_root="$ENV{FULCRUM_PDK_ROOT}" if ( ! ( -d $pdk_root ) and -d $ENV{FULCRUM_PDK_ROOT});
-d $pdk_root or usage("fulcrum-pdk-root improperly defined");
$gdsii = $cell_name . ".gds" if (!$oasis && $gdsii eq "" && $gdsii_list eq "");
$gdsii = $cell_name . ".oas" if ( $oasis && $gdsii eq "" && $gdsii_list eq "");
if (! -e $gdsii) {
    die "Layout file " . $gdsii . " not found.\n";
}
$format = "OASIS" if ($oasis);

##########################################################################
#                               DRC                                      #
##########################################################################
main();





##########################################################################
sub main{
  foreach my $f (@flows) {
    my $drc_run_dir="$working_dir/$f";
    system('mkdir', '-p', "$drc_run_dir"); 
    run_drc($f,$drc_run_dir, $icv_runset_path, $drc_runsets{$f});
  }
}




sub run_drc {
   my ($flow,$run_dir,$icv_runset_path,$runset)=@_;
   my $cmd_file="$run_dir/drc.cmd";
   open(CF, ">$cmd_file") or die "Cannot write to $cmd_file\n";

   my $cmd_config="$pdk_root/share/Fulcrum/icv/drc/drc_cmd.config";
   my $process_name="";
   my $dotprocess_name="";
   $runset="";
   open(CMD_CFG, "$cmd_config") or die "Cannot read $cmd_config\n";
   while(my $line=<CMD_CFG>) {
       my @data = split("=", $line);
       if($data[0] =~ "PROCESS_NAME") {
	   $process_name=$data[1];
       } elsif($data[0] =~ "DOTP_NAME") {
	   $dotprocess_name=$data[1];
       }
   }
   chomp $process_name;
   chomp $dotprocess_name;
   close(CMD_CFG);

   #check if flow is valid
   my %drc_runsets;

   # first check if a standalone runset exists
   $runset=$icv_runset_path . "/PXL/StandAlone/" . $flow . ".rs";
   if ( -e $runset) {
       print "\nRunset path: " . $runset;
       $drc_runsets{$flow}=$runset;
   } else {
       # if invalid flow is specified, use default_runset with a flag
       if (defined($default_runset{$flow})) { $runset=$default_runset{$flow}; }
       else                                 { $runset=$default_runset{""}; }
       print "\nStandalone runset $flow not found, reverting to $runset";
       $runset=$icv_runset_path . "/PXL/StandAlone/${runset}.rs";
   }

   my @all_includes = (
       @include_before,
       ".",
       "$pdk_root/share/Fulcrum/icv/drc",
       "$pdk_root/share/Fulcrum/icv/lvs",
       "$icv_runset_path/PXL_ovrd",
       "$icv_runset_path/PXL",
       "$ENV{PDK_CPDK_PATH}/libraries/icv/libcells",
       "$icv_runset_path/util/dot1/HIP",
       "$icv_runset_path/util/Cadnav",
       "$icv_runset_path/util/denplot",
       "$run_dir"
   );
   my $all_includes = join(" \\\n", map { "-I $_" } @all_includes);

   print CF <<ET;
#!/usr/intel/bin/tcsh -f
setenv _ICV_RSH_COMMAND $ENV{'FULCRUM_PACKAGE_ROOT'}/bin/icvrsh
$ENV{'ICV_SCRIPT'} 'icv' $all_includes \\
-D NOCLD \\
-vue \\
-D _drMaxError=100000000 \\
-D _drCOVER_BY_BCID=_drYES \\
-D _drICFBCIDEXCEPTION=_drYES \\
-D _drUSENDG=_drNO \\
-D _drUSERDEFINESUIN \\
-D _drCaseSensitive \\
-D _drPROCESS=_dr$dotprocess_name \\
-D _drSELECT_$flow \\
-D _drPROJECT=_drnone \\
-D _drPROCESSNAME=$process_name \\
-f $format \\
ET
   if ($jobs > 0) {
     print CF "-dp \\\n" .
              "-dphosts \$NB_PARALLEL_JOB_HOSTS \\\n";
   } else {
     print CF "-dp$threads \\\n" .
              "-turbo \\\n";
   }

   print CF "$icv_options \\\n" if (defined $icv_options ne "");
   if(-r $gdsii) {
      print CF "-i \'".abs_path($gdsii)."\' \\\n";
   }elsif(-r $gdsii_list) {
      open(FL,"<$gdsii_list");
      while(<FL>){
        chomp;
        s/\s+//g;
        next if ($_ eq "");
        print CF "-i \'".abs_path($_)."\' \\\n";
      }
      close(FL);
   }
   print CF "-c $cell_name \\\n";
   print CF "$runset\n";
    close(CF);
    `chmod +x $cmd_file`;
    my $cmd;
    if ($jobs > 0) {
        my $slots = $jobs * $threads;
        $cmd = "nbjob run " .
               "--parallel slots=$slots,slots_per_host=$threads,exit_on_master_finish=true ".
               "--class-reservation cores=1,memory=$mem $cmd_file";
    } else {
        $cmd = $cmd_file;
    }
    system("cd '$run_dir';  $cmd");
}

