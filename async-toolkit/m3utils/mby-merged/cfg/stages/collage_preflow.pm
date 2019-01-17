package collage_preflow;
use RTLUtils;
use ToolConfig;
use lib ToolConfig_get_tool_path("buildman") . '/lib';
use bman_flow;
use strict;
use warnings;

use Utils;
use List::Util qw(first);
use bman_base;

@collage_preflow::ISA = ("StageBase", "bman_flow", "bman_base");

########## Stage Constructor ##########
sub new {
    my $class      = shift;
    my $stagevars  = shift;
    my $scopedvars = shift;

    # Define stage options here
    my %priorInterfaceVars =
      ( defined $stagevars->InterfaceVars ) ? %{ $stagevars->InterfaceVars } : ();
    $stagevars->InterfaceVars( { %priorInterfaceVars, } );
    # allow arguments pass thru to VCS.
    $stagevars->enable_passthru(1);
    # call base constructor
    my $self = $class->SUPER::new( $stagevars, $scopedvars );
}

########## Stage Body ##########
sub run {
    my $self       = shift;
    my $scopedVars = $self->{scoped_vars};
    my $status = 0;
    my $cmd;
    my $logfile   = "$self->{targetLog}";

    $status =
      bman_flow::new( $scopedVars->{flowName} . "_flow" )->pre_flow( $scopedVars, $self );

      # ############################################
      # Custom Stage Code
      # ###########################################

      if ($ENV{ENABLE_UPF_GEN} == 1) {
        #$cmd = "echo COLLAGE_PRE_FLOW";
        $cmd = "/usr/intel/bin/tcsh -c $ENV{MODEL_ROOT}/tools/upf/scripts/hip_power_pin_spec_gen/RUN.GENERATE.HIP_POWER_PINS";
        $status = $self->shell_exec( JOBS=>[{ cmd=>$cmd, log=>$logfile, },], );
      }

      # ############################################
      # Custom Stage Code Ends here
      # ###########################################

    $self->{run_status} = $status;
    return $status;
}

sub help {
}

1;
