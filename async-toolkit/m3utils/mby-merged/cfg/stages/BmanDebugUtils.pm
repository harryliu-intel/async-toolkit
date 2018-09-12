package BmanDebugUtils;
use StageBase;
use ToolConfig;
use RTLUtils;
use Data::Dumper;
use File::Basename;
use File::Path;
use File::Copy;
use Cwd;
use Utils;
use Storable qw(dclone);
use strict;
#use warnings FATAL => 'all';
use warnings;

use OneCfg::UDF::Policy::Engine;
use OneCfg::UDF::Policy::Reference;
use ToolConfig;
use Scalar::Util qw(looks_like_number);
use OneCfg::UDF::Policy::DesignDB::SearchPathsLimit;

#** @class BmanDebugUtils
# @brief BmanDebugUtils::new
#*

#** @method new()
# @brief Sub BmanDebugUtils::new constructor
#*
sub new {
    my $class = shift;
    my $self  = {};
    bless( $self, $class );
    return $self;
}

#** @method debug_utils()
# @brief Sub BmanDebugUtils::debug_utils. Caller Bman_bman.pm
# @details calls debug_show for show command
# 	   calls debug_rtl and dump_ctl for generation of filelist.pl and *.ctl files
# @return the status of file generation
#*
sub debug_utils {
    my $flow_ptr = shift;
    my $clo      = shift;
    my $designDB = shift;
    debug_show( $flow_ptr, $designDB );
    if ( $clo->get_option( -flag => '-enable_dump_rtl' ) ) {

        my $target_staticRoot = $flow_ptr->{scoped_vars}->{build_target_static};
        my $targetDir         = "$target_staticRoot/pre_dump_rtl";
        my $facade            = $flow_ptr->{udf_facade};
        my $top_scope         = $facade->get_scope();

        RTL_mkDir($targetDir);
        my $dump_rtl_out = dump_rtl(
            -dir        => $targetDir,
            -scope      => $top_scope,
            -udf_facade => $facade
        );
        if ( $dump_rtl_out->{status} == 1 ) {
            print_fatal
              "Failed to create dump rtl filelist before Buildtree creation with -enable_dump_rtl switch\n";
        }
    }

    if ( $clo->get_option( -flag => '-enable_dump_ctl' ) ) {
        my $target_staticRoot = $flow_ptr->{scoped_vars}->{build_target_static};
        my $targetDir         = "$target_staticRoot/pre_dump_ctl";
        my $facade            = $flow_ptr->{udf_facade};
        my $top_scope         = $facade->get_scope();

        RTL_mkDir($targetDir);
        my $pre_dump_ctl_status = dump_ctl( $targetDir, $top_scope, $facade, $flow_ptr->get_clo() );
        if ( $pre_dump_ctl_status == 1 ) {
            print_fatal
              "Failed to create dump ctl files before Buildtree creation with -enable_dump_ctl switch\n";
        }
    }
    #call for show commands before buildtree creation.
    return 0;
}

#** @method dump_ctl()
# @brief Sub BmanDebugUtils::dump_ctl It generates filelist.pl and *.ctl
# @return It return the status whether the file is formed or not.
#*
sub dump_ctl {
    my ( $targetDir, $top_scope, $facade, $clo ) = @_;
    my $status = 0;

    my $dumpfile      = $clo->get_option( -flag => '-opt_dumpfile' );
    my $use_timestamp = $clo->get_option( -flag => "-use_timestamp" );
    my $dut_name      = ToolConfig::get_facet('dut');
    unless ( $dumpfile =~ /^\// ) {
        if ($use_timestamp) {
            my $time_stamp = Gizmos::Misc::get_time_stamp();
            $dumpfile = "$targetDir/" . $dut_name . ".onecfg_$time_stamp.ctl";
        }
        else {
            $dumpfile = "$targetDir/" . $dut_name . ".$dumpfile";
        }
    }

    local $Data::Dumper::Indent   = 1;
    local $Data::Dumper::Sortkeys = 1;

    my %dump;
    my $udf_list = $facade->get_udf_manager()->get_udf_list();
    foreach my $udf ( @{$udf_list} ) {
        $udf->dump( \%dump );
    }

    my $spath = $facade->get_udf_manager->get_search_paths;
    $dump{SearchPaths} = dclone $spath->dump() if ( defined $spath );

    my $subscopes = $facade->get_udf_manager->get_subscopes();
    $dump{SubScopes} = dclone $subscopes->dump() if ( defined $subscopes );

    $dump{Options} = dclone $clo->get_raw_data();

    my $data = Data::Dumper->Dump( [ \%dump ], [qw(*ACE_VAR_DUMP)] );

    my $ofh = new FileHandle ">$dumpfile";
    if ( defined $ofh ) {
        print_info("DEBUG DUMP> Dumping Virtual UDF to file '$dumpfile'");
        print $ofh "#", "-" x 34, "-*-perl-*-", "-" x 35, "\n";
        print $ofh "# Generated:  ", `date`;
        print $ofh "# By       :  $ENV{USER}\n";
        print $ofh "#", "-" x 79, "\n";
        print $ofh $data;
    }
    else {
        $status = 1;
        print_error("DEBUG DUMP> Could not open OneCfg CTL '$dumpfile' for writing - $!\n");
    }
    return $status;
}

#** @method dump_csv()
# @brief Sub BmanDebugUtils::dump_csv it reads all the udf files and list out all the ivars in .csv format
# @returns nothing
#*
sub dump_csv {
    my $ivars    = shift;
    my $csv_file = shift;
    $ivars->dump_csv_ivar_data($csv_file);
    return 0;

}

#** @method print_flag_values()
# @brief Sub BmanDebugUtils::print_flag_values caller Bman_bman.pm
# @return It returns nothing just prints information
#*
sub print_flag_values {
    my $clo = shift;

    print_info("FLAG/IVAR values:");
    my $ivar_names = $clo->get_option( -flag => "-flags_to_print" );
    foreach my $ivar_name ( @{$ivar_names} ) {
        my $ivar = $clo->get_option( -flag => "$ivar_name" );
        next if ( !defined $ivar );
        my $ivar_value = ( ref($ivar) eq "ARRAY" ) ? "(" . join( ',', @{$ivar} ) . ")" : $ivar;
        print_info("$ivar_name : $ivar_value");
    }
}

#** @method print_facet_values()
# @brief Sub BmanDebugUtils::print_facet_values caller Bman_bman.pm
# @return it returns nothing just prints information
#*
sub print_facet_values {
    my $clo = shift;

    print_info("FACET values:");
    my $facet_names = $clo->get_option( -flag => "-facets_to_print" );
    foreach my $facet_name ( @{$facet_names} ) {
        next if ( !ToolConfig::facet_exists($facet_name) );
        my $facet_value = ToolConfig::get_facet($facet_name);
        if ( !defined($facet_value) ) {
            $facet_value = "undef";
        }
        print_info("$facet_name : $facet_value");
    }
}

#** @method smart_help()
# @brief Sub BmanDebugUtils::smart_help gives information regarding any input ivar
# @param interfacevar (input any interface var)
# @returns prints help for interface ivar which is given as input
#*
sub smart_help {
    my $ivars = shift;
    my $clo   = shift;
    my $helpText;
    $helpText = $ivars->get_help(
        -toolname      => "flowbee",
        -level         => $clo->get_option( -flag => "-smart_help" ),
        -show_examples => 1,
        -show_hidden   => $clo->get_option( -flag => "-show_hidden" ),
        -help_level    => $clo->get_option( -flag => "-help_level" ),
    );
    my $hlpManager = OneCfg::HelpMan->new($helpText);
    $hlpManager->print_help(
        -usageMode => 0,
        -color     => $clo->get_option( -flag => "-color" ),
        -verbose   => $clo->get_option( -flag => "-help_verbose" ),
    );

}

#** @method check_gen_cfg_enabled (%args)
# @brief checks if given GenRTL config is enabled
# @details handles the gen_enable_config_ovr given in the cmdline or through udf.
# checks if the given config is present in the overrides and returns the override value (1|0)
# @param args An argument hash with keys:
# -default    =>  default enable value for a config.
# -scope      => scope of GenRTL config
# -config     => name of GenRTL config
# -udf_facade => udf_facade object
# @return boolean value (enable/disable) for given config.
#*
sub check_gen_cfg_enabled {
    my %args       = @_;
    my $enabled    = $args{-default};
    my $scope      = $args{-scope};
    my $config     = $args{-config};
    my $udf_facade = $args{-udf_facade};
    my @enb_overrides =
      $udf_facade->get_clo()->get_array_option( -flag => '-gen_enable_config_ovr' );
    foreach (@enb_overrides) {
        s/\s*//g;
        chomp;
        unless (/^\S+::\S+=[0|1]$/) {
            print_fatal
              "ERROR: Incorrect flag definition for '-gen_enable_config_ovr = ['$_'],'.\n\t correct format is '<scope>::<config>=[0|1]'\n";
        }
        if (/${scope}::${config}=(\d)/) {
            $enabled = $1;
        }
    }
    return $enabled;
}

#** @method dump_rtl()
# @brief Sub BmanDebugUtils::dump_rtl It generates filelist.pl and *.ctl
# @return It returns the status whether the file is generated or not.
#*
sub dump_rtl {
    my %args       = @_;
    my $id         = $args{-id} // "dump_rtl";
    my $targetDir  = $args{-dir};
    my $scope      = $args{-scope};
    my $udf_facade = $args{-udf_facade};
    my $dump_file  = $args{-dump_file};
    my $filter     = $args{-filter};
    my $dump_view  = $args{-dump_view};
    my $resolve    = $args{-resolve};
    my $clo        = $udf_facade->get_clo();
    my $entity     = $args{-dump_rtl};
    print_info "dump_rtl in gen_commands, Type, Entity: $args{-dump_rtl},
    $entity \n " if exists $args{-dump_rtl};
    print_info "Directory $args{-dir}, Filename , $args{-dump_file} \n "
      if exists $args{-dir} && $dump_file;

    my $status = 0;

    my $ignore_generated = $clo->get_option( -flag => "-ignore_generated" );
    $filter //= $clo->get_option( -flag => "-filter" ) // "Identity";

    if ( !defined $dump_file ) {
        $dump_file =
          ( $clo->get_option( -flag => "-filelist_dump_name" ) )
          ? $clo->get_option( -flag => "-filelist_dump_name" )
          : undef;

        if ( defined $dump_file ) {
            unless ( $dump_file =~ /^\// ) {
                $dump_file = "$targetDir/$dump_file";
            }
        }
    }

    my @entity_list;
    if ( defined $entity ) {
        push @entity_list, $entity;
    }
    elsif ( defined $clo->get_option( -flag => "-dump_rtl_entity" ) ) {
        my $rtl_entity = $clo->get_option( -flag => "-dump_rtl_entity" );
        push @entity_list, $rtl_entity;
    }
    else {
        my @models = @{ $clo->get_option( -flag => "-models_to_compile" ) };
        foreach my $model (@models) {
            $model = 'M:' . $model;
            push @entity_list, $model;
        }
    }
    if ( !@entity_list ) {
        print_fatal
          "No entity provided for dump_rtl. Please provide using -dump_rtl_entity or define -models_to_compile.\n";
    }
    my $require_entity_type = ( $clo->get_option( -flag => "-require_entity_type" ) ) ? 1 : 0;
    my %ddb_args = (
        -id          => $id,
        -scope       => $scope,
        -udf_manager => $udf_facade->get_udf_manager(),
        -options     => $clo,
        -use_filter  => 1,
        -filter      => $filter,
        -dolog       => 0,
    );
    if ($dump_view) {
        $ddb_args{-dump_view} = $dump_view;
    }

    my $designDB = new OneCfg::DesignDB( %ddb_args, );

    if ($resolve) {
        $designDB->create_default_modelviews( -resolve => 1, -exit_on_error => 1 );
    }
    else {
        $designDB->create_default_modelviews( -unfold => 1, -exit_on_error => 1 );
    }
    my $filelist;
    foreach my $rtl_entity (@entity_list) {
        $filelist = $designDB->dump_rtl(
            -require_entity_type => $require_entity_type,
            -entity              => $rtl_entity,
            -filename            => $dump_file,
            -include_prereqs     => $args{-include_prereqs},
            -res_dir             => $targetDir,
        );
        if ( -e $filelist ) {
            print_info("DEBUG DUMP> RTL dumpfile for $rtl_entity at '$filelist'");
        }
        else {
            print_error(
                "DEBUG DUMP> Run failed (status=$status) - did not create dumpfile '$filelist' \n"
            );
            $status = 1;
        }
    }
    my $dump_rtl_out = {
        status   => $status,
        filelist => $filelist,
    };
    return $dump_rtl_out;
}

#** @method debug_show()
# @brief Sub BmanDebugUtils::debug_show All show commands are added here.
# @return nothing. It prints the show command output.
#*
sub debug_show {
    #adding all show commands
    my ( $flow_ptr, $designDB ) = @_;
    if ( $flow_ptr->get_clo()->get_option( -flag => "-show_models" ) ) {
        my $lib_hash = $designDB->get_libs_by_model();    # OK
        foreach my $c ( sort keys %{$lib_hash} ) {
            print_info "\to $c\n";
        }

    }

    if ( $flow_ptr->get_clo()->get_option( -flag => "-show_scope" ) ) {
        my $scope = $flow_ptr->{udf_facade}->get_scope();
        print_info "Top scope: $scope\n";
    }
    if ( $flow_ptr->get_clo()->get_option( -flag => "-show_filters" ) ) {
        my $hdlspec = $flow_ptr->{udf_facade}->get_udf_ref( -category => "HDLSpec" );
        $hdlspec->show_filters();    # OK
    }
    if ( $flow_ptr->get_clo()->get_option( -flag => "-show_defined_compilers" ) ) {
        my $hdlspec = $flow_ptr->{udf_facade}->get_udf_ref( -category => "HDLSpec" );
        my $compiler_hash = $hdlspec->get_hdl_compilers_hash();
        print_info "Defined Compilers:\n";
        foreach my $c ( sort keys %{$compiler_hash} ) {
            print_info "\to $c\n";
        }

    }

    if ( $flow_ptr->get_clo()->get_option( -flag => "-show_taggable_attributes" ) ) {
        my $hdlspec = $flow_ptr->{udf_facade}->get_udf_ref( -category => "HDLSpec" );
        my $rtl_definers = $hdlspec->get_rtl_definers();
        print_info( Data::Dumper->Dump( [$rtl_definers], ["*Taggable_Attributes"] ) );

    }

    if ( $flow_ptr->get_clo()->get_option( -flag => "-show_search_path" ) ) {
        my $root_path = $flow_ptr->{udf_facade}->get_searchpaths_obj()->get_all_root_search_path();
        print_info( Data::Dumper->Dump( [$root_path], ["*root_search_path"] ) );
    }

    if ( $flow_ptr->get_clo()->get_option( -flag => "-show_file" ) ) {
        my $show_file = $flow_ptr->get_clo()->get_option( -flag => "-show_file" );
        my $scope     = $flow_ptr->get_clo()->get_option( -flag => "-show_file_scope" )
          if ( $flow_ptr->get_clo()->get_option( -flag => "-show_file_scope" ) );
        $scope = "all" if ( !defined $scope );
        my $spath = $flow_ptr->{udf_facade}->get_searchpaths_obj();
        my $search_file =
          $spath->get_file_info( -show_file => $show_file, -spaths => $spath, -scope => $scope );
        print_info("search_file= $search_file ");
    }

    if ( $flow_ptr->get_clo()->get_option( -flag => "-show_help_groups" ) ) {

        my $ivars = $flow_ptr->{udf_facade}->get_udf_ref( -category => "InterfaceVars" );
        $ivars->show_help_categories();
    }

    if ( $flow_ptr->get_clo()->get_option( -flag => "-show_inc_path" ) ) {
        print_info "", Data::Dumper->Dump( [ \@INC ], ["*INC"] );
    }

    if ( $flow_ptr->get_clo()->get_option( -flag => "-show_available_prototypes" ) ) {
        my $ivars = $flow_ptr->{udf_facade}->get_udf_ref( -category => "InterfaceVars" );
        $ivars->print_prototype_data();
    }

    if ( my $show_lib_opts = $flow_ptr->get_clo()->get_option( -flag => "-show_libs" ) ) {
        my $model = ( $show_lib_opts =~ /\+M(.*)/ ) ? $1 : '';

        $designDB->create_custom_modelview(
            -name     => 'show_libs',
            -models   => $flow_ptr->get_clo()->get_option( -flag => "-models_to_compile" ),
            -resolve  => 1,
            -testsets => [],
        );
        $designDB->show_libs(
            -name      => 'show_libs',
            -show_libs => $show_lib_opts,
            -model     => $model,
        );

    }
    if ( my $show_lib_opts = $flow_ptr->get_clo()->get_option( -flag => "-show_libs_relation" ) ) {
        my $model = $show_lib_opts =~ /\+M(.*)/ ? $1 : '';

        $designDB->create_custom_modelview(
            -name     => 'show_libs_relation',
            -models   => $flow_ptr->get_clo()->get_option( -flag => "-models_to_compile" ),
            -resolve  => 1,
            -testsets => [],
        );
        $designDB->show_libs(
            -name      => 'show_libs_relation',
            -show_libs => $show_lib_opts,
            -model     => $model,
            -tabular   => 1,
        );
    }
    return;

}

#** @method generate_metadata_files (%args)
# @brief generates metadata filelists
# @details calls dump_rtl function and generates three different flavors of metadata filelists.
# 1. unfolded_unresolved, 2. unfiltered, 3. filtered resolved
# @param args An argument hash with keys:
# -config     => GenRTL config hash
# -dir        =>  directory where filelists to be generated
# -scope      => scope of GenRTL config
# -udf_facade => udf_facade object
# @return returns hash with paths to all three flavors of metadata filelists.
#*
sub generate_metadata_files {
    my %args    = @_;
    my $config  = $args{-config};
    my $dir     = $args{-dir};
    my $scope   = $args{-scope};
    my $prereqs = $args{-prereqs};
    my $runscript_data;
    my $dump_rtl_out;
    if ( $config->{dump_rtl} =~ /\S+/ ) {
        $dump_rtl_out = dump_rtl(
            %args,
            -filter    => "Identity",
            -dump_file => "$dir/$scope.$config->{dump_rtl}.filelist_unfolded_unresolved.pl",
            -dump_rtl  => $config->{dump_rtl},
        );
        $runscript_data->{meta_data_unfolded_unresolved} = $dump_rtl_out->{filelist};

        # Dump out unfiltered view of DesignDB:
        $dump_rtl_out = dump_rtl(
            %args,
            -dump_rtl        => $config->{dump_rtl},
            -filter          => "Identity",
            -dump_view       => "unfiltered",
            -include_prereqs => $prereqs,
            -dump_file       => "$dir/$scope.$config->{dump_rtl}.filelist_unfiltered.pl",
        );
        $runscript_data->{meta_data_unfiltered} = $dump_rtl_out->{filelist};

        $dump_rtl_out = dump_rtl(
            %args,
            -dump_rtl        => $config->{dump_rtl},
            -filter          => $config->{filter},
            -include_prereqs => $prereqs,
            -dump_file       => "$dir/$scope.$config->{dump_rtl}.filelist.pl",
        );
        $runscript_data->{meta_data} = $dump_rtl_out->{filelist};
    }
    return $runscript_data;
}

#** @method find_library($library_name)
# @brief prints out all instances of given $library_name in all scopes
#   See OneCfg::UDF::HDLSpec::find_library
#*
sub find_library {
    my ( $design_db, $library_name ) = @_;
    print_info("searching for library '$library_name'");
    my @list = $design_db->find_library($library_name);
    if ( !@list ) {
        print_info("library '$library_name' is not found");
    }
    else {
        foreach my $ref (@list) {
            print_info( sprintf( "found %s:%s", $ref->{'scope'}, $ref->{'name'} ) );
        }
    }
    exit 0;
}

#** @method map_library($library_name)
# @brief decodes $library_name as (scope:name) and prints out all possible
#   paths this library can reach the top scope. See OneCfg::UDF::HDLSpec::trace_library_usage
#*
sub map_library {
    my ( $design_db, $library_name ) = @_;
    print_info("mapping library '$library_name'");
    my ( $scope, $name ) = ( $library_name =~ m/ \A ([^:]+):(.*) \z /x );
    if ( !defined $scope or !defined $name ) {
        print_error("library name for mapping must be in format: <scope>:<library name>");
        exit 1;
    }
    my @list = $design_db->decode_library_map( $design_db->resolve_library_map( $name, $scope ) );
    if ( !@list ) {
        print_info("library '$library_name' mapping to the top scope is not found");
    }
    else {
        foreach my $ref (@list) {
            print_info( "mapping: " . $ref );
        }
    }
    exit 0;
}

#** @method generate_merged_hdlspec_file()
# @brief generates a text file of merged OneCfg HDLSpec data
#*
sub generate_merged_hdlspec_file {
    my ( $flow_ptr, $design_db ) = @_;

    my $filename = "merged_hdlspec";
    if ( $flow_ptr->get_clo()->get_option( -flag => "-use_timestamp" ) ) {
        $filename .= '_' . Gizmos::Misc::get_time_stamp();
    }
    $filename .= '.pl';

    $filename =
      File::Spec->join( $flow_ptr->{scoped_vars}->{build_target_static}, "debug_dump", $filename );
    print_info("generating merged hdlspec data in '$filename'");
    $design_db->save_merged_hdlspec($filename);
    exit 0;
}

#** @method validate_policies()
# @brief perform UDF validation checks
#*
sub validate_policies {
    my ( $flow_ptr, $design_db ) = @_;

    my $severity;
    if ( $flow_ptr->get_clo()->get_option( -flag => '-strict_severity_policy' ) ) {
        $severity = $OneCfg::UDF::Policy::Engine::SEVERITY_ERROR;
    }

    my $engine = OneCfg::UDF::Policy::Engine->new( design_db => $design_db );
    my %rules = (
        '-severity_policy_duplicated_lib_specs' => OneCfg::UDF::Policy::Reference->new(
            class     => 'OneCfg::UDF::Policy::HDLSpec::DuplicatedLibrary',
            design_db => $design_db
        ),
        '-severity_policy_sip_shared_lib_sub_lib_dependency' =>
          OneCfg::UDF::Policy::Reference->new(
            class     => 'OneCfg::UDF::Policy::HDLSpec::SIPSharedLibDependency',
            design_db => $design_db
          ),
        '-severity_policy_exported_library_dependency' => OneCfg::UDF::Policy::Reference->new(
            class     => 'OneCfg::UDF::Policy::HDLSpec::ExportedLibraryDependency',
            design_db => $design_db
        ),
    );
    while ( my ( $flag, $policy ) = each %rules ) {
        print_debug("processing $policy");
        $engine->process_policy( $policy,
            $severity // $flow_ptr->get_clo()->get_option( -flag => $flag ) );
    }
    my $policy = create_search_paths_limit_policy($design_db);
    $policy->process();
    $engine->append_policy($policy);
    $engine->report();
}

#** @method create_search_paths_limit_policy ( $design_db )
# @brief creates SearchPathsLimit policy instance
#   and configure it based on ToolConfig data
#*
sub create_search_paths_limit_policy {
    my ($design_db) = @_;
    my $policy = OneCfg::UDF::Policy::DesignDB::SearchPathsLimit->new( design_db => $design_db );
    _set_search_paths_limit_policy(
        $policy, 'buildman', 'SEARCH_PATHS_LIMIT_ERROR',
        OneCfg::UDF::Policy::DesignDB::SearchPathsLimit::ERROR_LIMIT,
        OneCfg::UDF::Policy::DesignDB::SearchPathsLimit::NON_SCOPE
    );
    _set_search_paths_limit_policy(
        $policy, 'buildman', 'SEARCH_PATHS_LIMIT_WARNING',
        OneCfg::UDF::Policy::DesignDB::SearchPathsLimit::WARNING_LIMIT,
        OneCfg::UDF::Policy::DesignDB::SearchPathsLimit::NON_SCOPE
    );

    foreach my $scope ( $design_db->get_hdlspec()->get_scopes() ) {
        _set_search_paths_limit_policy( $policy, 'ipconfig/' . $scope,
            'SEARCH_PATHS_LIMIT_ERROR',
            OneCfg::UDF::Policy::DesignDB::SearchPathsLimit::ERROR_LIMIT, $scope );
        _set_search_paths_limit_policy( $policy, 'ipconfig/' . $scope,
            'SEARCH_PATHS_LIMIT_WARNING',
            OneCfg::UDF::Policy::DesignDB::SearchPathsLimit::WARNING_LIMIT, $scope );
    }
    return $policy;
}

#** @method _set_search_paths_limit_policy ( $policy, $tool_name, $variable_name, $message_type, $scope )
# @brief set the $policy limit based on ToolConfig data.
#   this is a helper function of create_search_paths_limit_policy.
#*
sub _set_search_paths_limit_policy {
    my ( $policy, $tool_name, $variable_name, $message_type, $scope ) = @_;
    if ( my $value = ToolConfig::get_tool_var( $tool_name, $variable_name ) ) {
        if ( looks_like_number($value) ) {
            $policy->set_scope_limit( $scope, $message_type, $value );
        }
        else {
            print_fatal(
                sprintf(
                    "ToolConfig '%s' tool has invalid '%s' variable value: '%s', "
                      . "expected a number",
                    $tool_name, $variable_name, $value
                )
            );
        }
    }
}

1;
