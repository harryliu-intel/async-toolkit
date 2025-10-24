package UserCode;

use strict;
use warnings;

use RTLUtils;
use ToolConfig;
use File::Basename;
use Cwd;

sub prescripts{
    my %args = @_;
    my $self = $args{self};
#    FlowSpec::buildman_setup(@_);
    my $status = 0;
    my $status1 = 0;
    my $logfile   = "$self->{targetLog}";
    my $cmd;
    my $collage_area = &ToolConfig::ToolConfig_get_tool_env_var("collage", "COLLAGE_WORK");
#    my $fusegen_area = &ToolConfig::ToolConfig_get_tool_env_var("ipconfig/fc", "SOCFUSEGEN_OUTPUTDIR") . "\/$ENV{DUT}\/fusegen\/output\/fuse";
    my $verif_gen_dir = "$ENV{MODEL_ROOT}\/verif\/gen";

    #print "Cleaning up collage $collage_area\n";
    #$status = system("rm -rf $collage_area");
#    if (!(-e $fusegen_area)) {
#      print "Initiate fusegen directory $fusegen_area\n";
#      $status = system("mkdir -p $fusegen_area");
#    }
# TODO klee18
#    print "Cleaning up $verif_gen_dir \n";
#    $status1 = system("rm -rf $verif_gen_dir\/*.sv*");
#    $status1 = system("rm -rf $verif_gen_dir\/*.hdl*");

    return ($status | $status1);
}

1;
