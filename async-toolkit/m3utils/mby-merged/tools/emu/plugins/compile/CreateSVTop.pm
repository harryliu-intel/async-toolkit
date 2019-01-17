##############################################################################
# CreateSVTop.pm
#
# To create Top level wrapper file at pre_flow stage
#
# Original Author: Jackie Yan
# Original Date: May 7, 2015
#
# Copyright (c) 2015 Intel Corporation
# Intel Confidential
#
##############################################################################
package CreateSVTop;
use strict;
use warnings;

use emubuild::common::Plugins;

use RTLUtils;
use Utils;
use Data::Dumper;
use File::Basename;

##############################################################################
# new
#
# Creates this plugin object and registers it for the framework with its
# particular resulting callback function on a particular set of events.
# Most of this function does not need to be changed by user, except
# for registering the event to function.
##############################################################################
sub new {
    # This code at the top is always the same, and required for the framework.
    my ($type, %options) = @_;
    my $plugin_self      = ();
    $plugin_self->{Name} = $type;
    bless($plugin_self, $type);

    # Register the create_svtop function for callback before and after
    # every global stage. Remember to return 0 on success, non-zero on failure.
    &emubuild::common::Plugins::RegisterForEvent(
        'PreFlowChecks', # USER: Name of Event
        \&create_svtop, # USER: Local function to call
        $plugin_self,
        $plugin_self->{Name});
    return $plugin_self;
}

##############################################################################
# create_svtop
#
# Looks up the name of the stage in question, as well as checking to see
# if it is enabled in the USER section of the YML file.
##############################################################################
sub create_svtop {
    my $plugin_self = shift;
    my $event = shift;
    my %args = (
        global_instance => undef,
        @_,
    );

    # Because we have the actual global_instance as self, we are able to
    # treat usage of this basically like being inside the stage. This is
    # both powerful and dangerous.
    my $status = 0;
    my $self = $args{global_instance};

    # Get the object to query the yml.
    my $global_yml_query = $self->get_scoped_var('global_yml');

    # Check to see if this user plugin is enabled. You might also use the
    # stage name as a determining factor of what you do.
    #
    # When looking up global YML entries, remember they are located under GLOBAL:USER:
    if ($global_yml_query->has_field("GLOBAL:USER:CREATE_SVTOP:ENABLE")) {
        my $enable = $global_yml_query->get_scalar("GLOBAL:USER:CREATE_SVTOP:ENABLE") // "NO";
        my $overwrite = $global_yml_query->get_scalar("GLOBAL:USER:CREATE_SVTOP:OVERWRITE") // "NO";
        my $result_path = $self->get_scoped_var('model_root')."/tools/emu";

        ## Setup input file path
        my $input_file = $self->get_scoped_var('model_root')."/source/fxr/fxr.sv";
        if ($global_yml_query->has_field("GLOBAL:USER:CREATE_SVTOP:INPUT_FILE")) {
            $input_file = $self->get_scoped_var('model_root')."/".$global_yml_query->get_scalar("GLOBAL:USER:CREATE_SVTOP:INPUT_FILE");
        }
        ## Check for input file existence
        if (!-e $input_file) {
            print_info("CreateSVTop: Input File Does not exist skipping run");
            last;
        }


        ## Setup svtop file path
        my $svtop_file = $self->get_scoped_var('model_root')."/tools/emu/fxr_top.vs";
        if ($global_yml_query->has_field("GLOBAL:USER:CREATE_SVTOP:SVTOP_FILE")) {
            $svtop_file = $self->get_scoped_var('model_root')."/".$global_yml_query->get_scalar("GLOBAL:USER:CREATE_SVTOP:SVTOP_FILE");
            $result_path = dirname($svtop_file);
        }

        ## Check for svtop file existence
        if (-e $svtop_file && ($overwrite =~ m/^NO$/) ) {
            print_info("CreateSVTop: Top level file exists, skipping re-create");
            $enable = "NO";
        }

        if ($enable =~ m/^YES$/) {
            #Setup Script path and dut name
            my $script = $self->get_scoped_var('model_root')."/tools/emu/scripts/create_svtop";
            if ($global_yml_query->has_field("GLOBAL:USER:CREATE_SVTOP:SCRIPT")){
                $script = $self->get_scoped_var('model_root')."/".$global_yml_query->get_scalar("GLOBAL:USER:CREATE_SVTOP:SCRIPT");
            }
            my $dut = "fxr";
            if ($global_yml_query->has_field("GLOBAL:ACE_METAFILES:USER_ID:MODEL_NAME")){
                $dut = $global_yml_query->get_scalar("GLOBAL:ACE_METAFILES:USER_ID:MODEL_NAME");
            }

            #Execute Command
            my $cmd = "$script -proj $dut -input_file $input_file -result_path $result_path";
            print_info("CreateSVTop: Creating top level file $svtop_file");
            $status = &RTL_execCmd($cmd);
            print_info("CreateSVTop: Completed with exit status $status");
        }
    }

    return $status;
}

# do not delete:
1;
