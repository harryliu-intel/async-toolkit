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
my $icv_options;
my $threads=2;
my $flow="drcd";
my $pdk_root="";
my $icv_runset_path="/nfs/sc/proj/ctg/mrl108/mrl/tools/rdt/kits/p1273_14.2.1/runsets/icvalidator/verification_runsets/latest";

sub usage {
    my ($msg) = @_;
    print STDERR "$msg\n" if defined $msg;
    my $usage  = "Usage: lvs [args] cell\n";
    $usage .= "    --working-dir=[$working_dir]\n";
    $usage .= "    --icv-runset-path=[$icv_runset_path] (DRC runset path)\n";
    $usage .= "    --gds2-file=[$gdsii] (Provide source layout by gdsii file.)\n";
    $usage .= "    --gds2-list=[$gdsii] (Provide source layout by a file list contains gdsii file)\n";
    $usage .= "    --icv-options=[$icv_options] (Extra ICV command options)\n";
    $usage .= "    --threads=[$threads] (ICV thread)\n";
    $usage .= "    --flow=[$flow] (DRC runset selection. Default will run $flow)\n";
    $usage .= "    --fulcrum-pdk-root=[$pdk_root]\n";


    print STDERR "$usage\n";
    exit 1;
}


while (defined $ARGV[0] and $ARGV[0] =~ /^--(.*)/) {
    my ($flag, $value) = split("=",$1);
    $value=1 if ! defined $value;
    if ($flag eq "working-dir") {
            $working_dir = $value;
    } elsif ($flag eq "gds2-file") {
            $gdsii = $value;
    } elsif ($flag eq "gds2-list") {
            $gdsii_list = $value;
    } elsif ($flag eq "threads") {
        $threads = $value;
    } elsif ($flag eq "flow") {
        $flow = $value;
    } elsif ($flag eq "icv-runset-path") {
        $icv_runset_path = $value;
    } elsif ($flag eq "icv-options") {
        $icv_options = $value;
    } elsif ($flag eq "fulcrum-pdk-root") {
            $pdk_root = $value  if(defined $value);
    } else {
        print STDERR "Error: argument --${flag}=${value} not recognized.\n";
        &usage();
    }
    shift @ARGV;
}


@ARGV == 1 or usage("No Cell");
my $cell_name = shift;
-d $working_dir ne "" or $working_dir = $pwd;
chomp $working_dir;
$pdk_root="$ENV{FULCRUM_PDK_ROOT}" if ( ! ( -d $pdk_root ) and -d $ENV{FULCRUM_PDK_ROOT});
-d $pdk_root or usage("fulcrum-pdk-root improperly defined");

#check if flow is valid
my %drc_runsets;
my @flows=split(',',$flow);
foreach my $f (@flows) {
  my $runset="$icv_runset_path/StandAlone/dotOne/$f.1.rs";
  if ( -e $runset) {
    $drc_runsets{$f}=$runset;
  }else{
    die "ERROR: Cannot find DRC runset:$runset\n";
  }
}
##########################################################################
#                               DRC                                      #
##########################################################################
main();





##########################################################################
sub main{
  foreach my $f (keys %drc_runsets){
    my $drc_run_dir="$working_dir/$f";
    system('mkdir', '-p', "$drc_run_dir"); 
    run_drc($drc_run_dir, $drc_runsets{$f});
  }
}




sub run_drc {
   my ($run_dir,$runset)=@_;
   system('cp','-rp', "$icv_runset_path/CPYDB","$run_dir");
   my $cmd_file="$run_dir/drc.cmd";
   open(CF, ">$cmd_file") or die "Cannot write to $cmd_file\n";
   print CF <<ET;
#!/usr/intel/bin/tcsh -f
setenv DR_CPYDB $run_dir/CPYDB
$ENV{'ICV_SCRIPT'} 'icv' -I . \\
-I $icv_runset_path/PXL_ovrd \\
-I $icv_runset_path/PXL \\
-I $icv_runset_path/StandAlone/dotOne \\
-I $icv_runset_path/util/dot1/HIP \\
-I $icv_runset_path/util/Cadnav \\
-I $icv_runset_path/util/denplot \\
-I $run_dir \\
-D NOCLD \\
-vue \\
-dp$threads \\
-turbo \\
-D _drMaxError=100000000 \\
-D _drUSENDG=_drNO \\
-f GDSII \\
ET

   print CF "$icv_options \\\n" if (defined $icv_options and $icv_options ne "");
   if(-r $gdsii) {
      print CF "-i ".abs_path($gdsii)." \\\n";
   }elsif(-r $gdsii_list) {
      open(FL,"<$gdsii_list");
      while(<FL>){
        chomp;
        s/\s+//g;
        next if ($_ eq "");
        print CF "-i ".abs_path($_)." \\\n";
      }
      close(FL);
   }
    print CF "-c $cell_name \\\n";
    print CF "$runset\n";
    close(CF);
    `chmod +x $cmd_file`;
    system("cd '$run_dir';  $cmd_file");
}

