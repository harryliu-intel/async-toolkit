################################################################################
# Intel Top Secret
################################################################################
# Copyright (C) 2010, Intel Corporation.  All rights reserved.
#
# This is the property of Intel Corporation and may only be utilized
# pursuant to a written Restricted Use Nondisclosure Agreement
# with Intel Corporation.  It may not be used, reproduced, or
# disclosed to others except in accordance with the terms and
# conditions of such agreement.
###################################################################################

use ToolConfig;
use lib ToolConfig_get_tool_path("buildman") . '/lib';
use StageManager;

BEGIN {
    StageManager::register_stage(
        'genrtl',    # Stage instance name (library name and '_' prepended)
        {   stage    => 'genrtl',    # Actual stage namesy
            type     => 'lib',       # Operates on library/model/..
            opts     => {},          # Other stage options
            flow     => 'genrtl',    # Top-level flow name
            requires => [
                'GEN_CONFIG',        # Tags that must be present and non-empty
            ],
            consumes => [
                'GENERATED',         # source 'types 'consumed
                'GEN_CONFIG',
                'VLOG_HDR_FILES',
                'NEBULON_VHDR_GEN',
                'VLOG_FILES_GEN',
            ],
            # adding 'GENCC_OUT' so collage stage can depend on this genrtl stage; https://hsdes.intel.com/appstore/article/#/2206194695
            produces => [ 'VLOG_FILES', 'SVERILOG_FILES', 'GENCC_OUT'],    # source 'types' produced
#            produces => [ 'VLOG_FILES', 'SVERILOG_FILES'],    # source 'types' produced
            use_prebuilt      => 0,    # Run only for libraries in local scope
            dependent_configs => 1,
        }
    );
}

######################################################################
# Flow definition
######################################################################
package genrtl_flow;
use ToolConfig;
use lib ToolConfig_get_tool_path("buildman") . '/lib';
use bman_flow;
use RTLUtils;
@genrtl_flow::ISA = qw ( bman_flow );

package genrtl;
use RTLUtils;
use ToolConfig;
use Data::Dumper;
use File::Basename;
use File::Path;
use Utils;
use GenUtils;
use bman_lib_base;
#use lib ToolConfig_get_tool_path("buildman") . '/lib';
#use lib ToolConfig_get_tool_path("bman_stages");
require "$ENV{MODEL_ROOT}/cfg/stages/BmanDebugUtils.pm";
use strict;
use warnings;

@genrtl::ISA = ("bman_lib_base");

########## Stage Constructor ##########
sub new {
    my $class      = shift;
    my $stagevars  = shift;
    my $scopedvars = shift;

    # Define stage options here
    $stagevars->InterfaceVars();
    # allow arguments pass thru to VCS.
    $stagevars->enable_passthru(1);
    # call base constructor
    my $self = $class->SUPER::new( $stagevars, $scopedvars );
}

sub is_runnable($$) {
    my $bt_libobj   = shift;
    my $scoped_vars = shift;
    my $runnable    = 0;
    return 0 if ( !$scoped_vars->{egc} );
    if (   ( $bt_libobj->stage_manager->has_stage_requires( "genrtl", $bt_libobj->tag_names ) )
        && ( $bt_libobj->stage_manager->has_stage_consumes( "genrtl", $bt_libobj->tag_names ) )
        && ( $bt_libobj->is_gen_cfg_enabled() ) ) {
        $runnable = 1;

    }
    return $runnable;
}

########## to_propagate ########
sub to_propagate (%) {
    my %args   = @_;
    my $tree   = $args{-obj};
    my $models = $args{-models};
    my $libs   = $args{-libs};
    my @prop_libs;
    my $prop_active = 1;
    foreach my $lib ( @{$libs} ) {
        my $libtree = $tree->tree->lib($lib);
        if ( $libtree->has_attributes( -attr => '-gen_config' ) ) {
            push @prop_libs, $lib;
        }
    }
    return ( $prop_active, $models, \@prop_libs );
}

########## Stage Body ##########
sub setup {
    my $self           = shift;
    my $flow_ptr       = shift;
    my $status         = $self->SUPER::setup($flow_ptr);
    my $flowVars       = $self->{flow_vars};
    my $scopedVars     = $self->{scoped_vars};
    my $logprefix      = $self->{log_prefix};
    my $targetRoot     = $scopedVars->{build_target};
    my $dut            = $scopedVars->{dut};
    my $udf_facade     = $self->{udf_facade};
    my @models_to_comp = @{ $self->get_clo()->get_option( -flag => "-models_to_compile" ) };
    my @genrtl_cfgs =
      @{ &get_genrtl_cfgs( -models => \@models_to_comp, -designDB => $self->{designDB} ) };

    my $spaths               = $udf_facade->get_searchpaths_obj();
    my $cur_scope            = $udf_facade->get_scope();
    my $demo                 = $self->get_clo()->get_option( -flag => '-demo' );
    my $quiet                = $self->get_clo()->get_option( -flag => '-quiet' ) unless ($demo);
    my $generate_results_dir = $targetRoot . '/GenRTL';
    my $report_status =
      $spaths->find_file( -file => "bin/report_compile_status.pl", -scope => $cur_scope );
    if ( $self->get_clo()->get_option( -flag => '-gen_results_ipdir' ) ) {
        $generate_results_dir = $self->get_clo()->get_option( -flag => '-gen_results_ipdir' );
    }
    RTL_mkDir($generate_results_dir);

    print_info "===> Running Code Generation\n";

    # -----------------------------------------------------------
    # Grab the data stored in the UDF_GenRTL
    # -----------------------------------------------------------
    my $genRTL = $udf_facade->get_udf_ref( -category => "GenRTL" );
    my $gen_cfg_str = $self->get_hdlspec_attr( -attr => '-gen_config' );
    $gen_cfg_str =~ s/\s*//g;
    my @gen_cfgs = split( /,/, $gen_cfg_str );
    foreach my $gen_cfg (@gen_cfgs) {
        next if ( $gen_cfg !~ /\w+/ );
        my $gen_scope = $cur_scope;
        my $cfg_str   = $gen_cfg;
        if ( $gen_cfg =~ /(\w+)\:\:(\w+)/ ) {
            $gen_scope = $1;
            $gen_cfg   = $2;
        }
        my $config = $genRTL->get_config( -scope => $gen_scope, -config => $gen_cfg );
        my $default_enb =
          ( $config->{enable_config} =~ /\<\s*(\S+)\s*\>/ )
          ? $self->get_clo()->get_option( -flag => $1 )
          : $config->{enable_config};

        my $config_results_dir = "$generate_results_dir/$gen_scope/$gen_cfg";

        # dump out unfolded and unresolved view of DesignDB
        my $runscript_data = BmanDebugUtils::generate_metadata_files(
            -udf_facade => $udf_facade,
            -scope      => $gen_scope,
            -dir        => $config_results_dir,
            -config     => $config,
            -prereqs    => $self->get_clo()->get_option( -flag => '-dump_rtl_include_prereqs' ),
        );
        if ( $config->{dump_rtl} =~ /\S+/ ) {
            $self->set_env(
                META_DATA          => $runscript_data->{meta_data},
                META_DATA_UNFIL    => $runscript_data->{meta_data_unfiltered},
                META_DATA_UNFOLDED => $runscript_data->{meta_data_unfolded_unresolved},
            );
        }
        foreach my $env ( @{ $config->{gen_setenv} } ) {
            next if ( $env =~ /^\s*$/ );
            $env =~ s/\s*=\s*/=/g;
            if ( $env !~ /^\s*(\S+?)=(.*)$/ ) {
                print_fatal(
                    "ERROR: Incorrect GenRTL 'gen_setenv =>[ '$env' ]' format used for '${gen_scope}::$gen_cfg' configuration\n\t CORRECT format: gen_setenv =>[ '<var>=<value>' ]\n"
                );
            }
            $env =~ /^\s*(\S+)=(.*)$/;
            $self->set_env( $1 => $2 );
            print_info "Setting ENV $1=$2\n";
        }
        my @jobs;
        my $logdir = "$generate_results_dir/log";
        my $log    = $logdir . "/GenRTL__${gen_scope}.${gen_cfg}.log";
        push @jobs, "echo mkdir -p $config_results_dir";
        push @jobs, "mkdir -p $config_results_dir";
        push @jobs, "echo mkdir -p $logdir";
        push @jobs, "mkdir -p $logdir";
        push @jobs, "echo cd $config_results_dir";
        push @jobs, "cd $config_results_dir";

        foreach my $cmd ( @{ $config->{gen_commands} } ) {
            next unless ( $cmd =~ /\S+/ );
            push @jobs,
              $self->resolve_command_values(
                -scope      => $gen_scope,
                -cmd        => $cmd,
                -spaths     => $spaths,
                -search_exe => $self->get_clo()->get_option( -flag => '-gen_search_exe' )
              );
        }
        my $job = join( "\n", @jobs );

        $self->{run_status} = 0;
        # -----------------------------------------------------------
        # Build the command/s to run the ACTUAL code gen flow
        # -----------------------------------------------------------
        $self->shell_gen_script(
            JOBS => [ { cmd => $job, log => $log } ],
            DESC => "genrtl run"
        );
    }

    ## Make sure nfs slowness does not cause failure
    $self->sync_dir($generate_results_dir);
    $flow_ptr->{setup_ok} = 1 unless $status;
    return $status;
}

sub run {
    my $self = shift;
    $self->{run_status} = $self->shell_exec_script( DESC => "genrtl run" );
    return $self->{run_status};
}
#-------------------------------------------------------------------------------
sub resolve_command_values {
    my ( $self, %args ) = @_;
    my $scope  = $args{-scope};
    my $cmd    = $args{-cmd};
    my $spaths = $args{-spaths};
    $cmd =~ s/\<\s+/</g;
    $cmd =~ s/\s+\>/>/g;
    $cmd =~ s/\s*:\s*:\s*/::/g;

    my ( $exe, $exe_args, $resolved_cmd );
    if ( $args{-search_exe} != 1 ) {
        # do not verify executables
        $exe_args     = $cmd;
        $resolved_cmd = "";
    }
    elsif ( $cmd =~ /^\s*source\s+\S+\s*/ ) {
        # executble is a unix source command
        $exe_args = $cmd;
        $exe_args =~ s/^\s*source\s+//;
        $resolved_cmd = "source ";
    }
    elsif ( $cmd =~ /^\s*(\${\S+}.*)\s+/ ) {
        # executble specified with the Shell script variable
        $resolved_cmd = $1;
        $exe_args     = $cmd;
        $exe_args =~ s/^\s*(\${\S+}.*)\s+//;
    }
    else {
        $cmd =~ /^\s*(\S+)\s*/;
        $exe = $spaths->find_executable( $1, $scope );
        $exe_args = $cmd;
        $exe_args =~ s/^\s*\S+\s+//;
        $resolved_cmd = $exe;
    }

    foreach ( split /\s+/, $exe_args ) {
        if (/\<(\S+)::(\S+)\>/) {
            my $flag_val = $self->get_clo()->get_option( -flag => "$2", -scope => $1 );
            $flag_val = join( ',', map( "'$_'", @{$flag_val} ) ) if ( ref($flag_val) eq 'ARRAY' );
            s/\<(\S+)::(\S+)\>/${flag_val}/;
        }
        elsif (/\<(\S+)\>/) {
            my $flag_val = $self->get_clo()->get_option( -flag => $1 );
            $flag_val = join( ',', map( "'$_'", @{$flag_val} ) ) if ( ref($flag_val) eq 'ARRAY' );
            s/\<(\S+)\>/${flag_val}/;
        }
        elsif (/FIND_FILE:(\S+)\s*/) {
            my $found_file = $spaths->find_file( -file => $1, -scope => $scope );
            s/FIND_FILE:(\S+)\s*/${found_file}/;
        }
        elsif (/FIND_DIR:(\S+)\s*/) {
            my $found_dir = $spaths->find_directory( -dir => $1, -scope => $scope );
            s/FIND_DIR:(\S+)\s*/${found_dir}/;
        }

        $resolved_cmd .= " $_";
    }
    return $resolved_cmd;
}

sub get_genrtl_cfgs {
    my %args     = @_;
    my @models   = @{ $args{-models} };
    my $designDB = $args{-designDB};
    my @genrtl_cfgs;
    foreach my $model (@models) {
        my $model_spec = $designDB->get_model_spec($model);
        if (   ( $model_spec->{enable_generate_rtl} )
            && ( ref( $model_spec->{enable_generate_rtl} ) eq 'ARRAY' ) ) {
            print_fatal(
                "enable_generate_rtl is now convereted to be a boolean attribute inside model definition from array reference. Please fix it in $model"
            );
        }
        if (   ( exists $model_spec->{enable_generate_rtl} )
            && ( ( $model_spec->{enable_generate_rtl} ) ) ) {
            if ( ( exists $model_spec->{gen_rtl} ) && ( ref( $model_spec->{gen_rtl} ) eq 'ARRAY' ) )
            {
                push @genrtl_cfgs, @{ $model_spec->{gen_rtl} };
            }
            else {
                print_fatal(
                    "Model $model has genrtl enabled but correct gen_rtl entry for enabled configurations not present."
                );
            }
        }

    }
    @genrtl_cfgs = keys { map { $_ => 1 } @genrtl_cfgs };
    return \@genrtl_cfgs;
}

#---------------------------------------------------------------------------------------------------
# stage-type-specific configuration setting accessed by get_stage_type_var
#---------------------------------------------------------------------------------------------------

sub strict_env {
    return 1;
}

sub help {
}
#--------------------------------------------------------------------------------
1;
