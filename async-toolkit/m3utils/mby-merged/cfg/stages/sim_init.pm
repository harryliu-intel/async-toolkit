######################################################################
# PRE VCS Stage
#
# Output from stage is logged to sim_init/log/sim_init.log
#
######################################################################
#####################################################################
# Main Package
######################################################################
use ToolConfig;
use lib ToolConfig_get_tool_path("buildman") . '/lib';
use StageManager;

package sim_init;
use bman_lib_base;
use bman_base;
use RTLUtils;
use ToolConfig;
use File::Basename;
use File::Path;
use File::Copy;
use File::Slurp;

use Utils;
use List::Util qw(first);
use warnings;
use strict;
use BmanBuildStageManager;
use parent "bman_lib_base";

sub package_init {
  my $dutconnect = (&ToolConfig::get_facet("ace_model_name") =~ /^dft/) ? 'vtpsim_connect' : 'dutconnect';
  my $gen_testlist = (&ToolConfig::get_facet("ace_model_name") =~ /^dft/) ? 'gen_dft_testlist' : 'gen_testlist';
  StageManager::register_stage(
          $dutconnect, # Stage instance name (library name and '_' prepended)
          { stage     =>   $dutconnect,          # Actual stage name
            type      =>   'lib',                # Operates on library/model/..
            opts      =>   {},                   # Other stage options
            flow      =>   'sim_init',                # Top-level flow name
            requires  => [ 'ENABLE_SIM_INIT'
                         ],                      # Tags that must be present and non-empty
            consumes  => [ 'VLOG_FILES_GEN',
                         ],                      # source 'types 'consumed
            produces  => [ 'VLOG_FILES',],                 # source 'types' produced
            use_prebuilt => 0,                   # Run only for libraries in local scope
          },
  );
  StageManager::register_stage(
          'gen_spf_files', # Stage instance name (library name and '_' prepended)
          { stage     =>   'gen_spf_files',          # Actual stage name
            type      =>   'lib',                # Operates on library/model/..
            opts      =>   {},                   # Other stage options
            flow      =>   'sim_init',                # Top-level flow name
            requires  => [ 'ENABLE_SIM_INIT'
                         ],                      # Tags that must be present and non-empty
            consumes  => [ 'VLOG_FILES_GEN',
                         ],                      # source 'types 'consumed
            produces  => [ 'VLOG_FILES',],                 # source 'types' produced
            use_prebuilt => 0,                   # Run only for libraries in local scope
          },
  );
  StageManager::register_stage(
          $gen_testlist, # Stage instance name (library name and '_' prepended)
          { stage     =>   $gen_testlist,        # Actual stage name
            type      =>   'lib',                # Operates on library/model/..
            opts      =>   {},                   # Other stage options
            flow      =>   'sim_init',            # Top-level flow name
            requires  => [ 'ENABLE_SIM_INIT'
                         ],                      # Tags that must be present and non-empty
            consumes  => [ 
                         ],                      # source 'types 'consumed
            produces  => [ 'VLOG_FILES','INIT_DONE'],                 # source 'types' produced
            use_prebuilt => 0,                   # Run only for libraries in local scope
          },
      );
## GK
  StageManager::register_stage(
          'sb_decode_gen', # Stage instance name (library name and '_' prepended)
          { stage     =>   'sb_decode_gen',        # Actual stage name
            type      =>   'lib',                # Operates on library/model/..
            opts      =>   {},                   # Other stage options
            flow      =>   'sim_init',            # Top-level flow name
            requires  => [ 'ENABLE_SIM_INIT',
                         ],                      # Tags that must be present and non-empty
            consumes  => [
                         ],                      # source 'types 'consumed
            produces  => [ 'VLOG_FILES',],                 # source 'types' produced
            use_prebuilt => 0,                   # Run only for libraries in local scope
          },
  );
## GK
  
  StageManager::register_stage(
          'gen_subip', # Stage instance name (library name and '_' prepended)
          { stage     =>   'gen_subip',          # Actual stage name
            type      =>   'lib',                # Operates on library/model/..
            opts      =>   {},                   # Other stage options
            flow      =>   'sim_init',                # Top-level flow name
            requires  => [ 'ENABLE_SIM_INIT',
                         ],                      # Tags that must be present and non-empty
            consumes  => [ 
                         ],                      # source 'types 'consumed
            produces  => [ 'VLOG_FILES',],                 # source 'types' produced
            use_prebuilt => 0,                   # Run only for libraries in local scope
          },
  );
  StageManager::register_stage(
          'gen_default_liblist', # Stage instance name (library name and '_' prepended)
          { stage     =>   'gen_default_liblist',        # Actual stage name
            type      =>   'lib',                # Operates on library/model/..
            opts      =>   {},                   # Other stage options
            flow      =>   'sim_init',                # Top-level flow name
            requires  => [ 'ENABLE_SIM_INIT',
                         ],                      # Tags that must be present and non-empty
            consumes  => [ 'VLOG_FILES_GEN',
                         ],                      # source 'types 'consumed
            produces  => [ 'VLOG_FILES'],                 # source 'types' produced
            use_prebuilt => 0,                   # Run only for libraries in local scope
          },
  );
}

#####################################################################
# Flow Package
######################################################################
package sim_init_flow;
use ToolConfig;
use lib ToolConfig_get_tool_path("buildman") . '/lib';
use bman_flow;
use bman_base;
use bman_lib_base; ## TODO
use RTLUtils;
use Utils;
use List::Util qw(first);
@sim_init_flow::ISA = qw ( bman_flow bman_base );

sub setup {
  my $self = shift;
  my $flow_ptr = shift;

  my $status = 0;
  $status = $self->SUPER::setup($flow_ptr);
  $flow_ptr->{setup_ok} = 1 unless $status;

}

sub pre_flow {
  my $self = shift;
  my $scopedVars   = shift;
  my $stage_obj = shift;
 
  # Reference to pre_flow 'StageBase' object
#  my $stage_pre_flow = shift;

  my $status = $self->SUPER::pre_flow($scopedVars);

  # setup
#  $self->{DesignDB} = $self->setupDesignDB($stage_obj);
#  my $designDB = $self->{DesignDB};
#  @libList = @{$designDB->get_libs_for_model($model)};
#  my $libraryName = $self->get_clo()->get_option(-flag => "-library");
  #my $dut = &ToolConfig::get_facet( 'dut' );
  #$self->{outDir}     = $self->{scoped_vars}->{targetRoot} . "/$dut" . "/sim_init";
#  $self->{modelName}  = $self->get_clo()->get_option( -flag => '-this_model', -scope => 'NON_SCOPED' );
  #my $verifGenDir = "$ENV{MODEL_ROOT}/verif/gen";

  #opendir ( DIR, $verifGenDir ) || die "Error in opening dir $verifGenDir\n";
  # DO NOT remove the files under verif/gen
#  while ( (my $filename = readdir(DIR))) {
#    if ( !($filename =~ /.*\.(hdl|sv|svh)$/)) {
#      next;
#    }
#    $filename = $verifGenDir."/".$filename;
#    if(-d $filename) {
#        if (!rmtree( $filename, 0, 1 )) {
#            $self->info( ' ' );
#            $self->fatal( "Could not delete directory '$filename' : $!" );
#        }
#    }
#    elsif(-f $filename) {
#        $self->info( 'Attempting to remove '.$filename ); 
#        if (!unlink( $filename )) {
#            $self->info( ' ' );
#            $self->fatal( "Could not delete file '$filename' : $!" );
#        }
#    }
#  }

  return $status;
}


#####################################################################
# DUTconnect Stage
######################################################################
package dutconnect;

use bman_lib_base;
use bman_base;
use RTLUtils;
use ToolConfig;
use File::Basename;
use File::Path;
use File::Copy;
use File::Slurp;

use Utils;
use List::Util qw(first);
use warnings;
use strict;


@dutconnect::ISA = ("bman_lib_base");

# Base class
use parent qw( bman_model_base );


########## Stage Body ##########

sub dutconnect_setup {
    my $self = shift;
    my $status = 0;

    $status = $self->func_dutconnect_setup();

    return $status;
}

sub func_dutconnect_setup {
    my $self = shift;
    my $cmd;
    my $status = 0;
    #my $targetDir    = $self->{outDir};
    my $logfile   = "$self->{targetLog}"; ## TODO
    my $aceroot_path = $self->{scoped_vars}->{build_target_static};

    #Assemble the CMD
    $cmd = "$ENV{MODEL_ROOT}/scripts/verif/DutConnect.pl $ENV{MODEL_ROOT}/scripts/verif/DutConnect.cfg $aceroot_path";
    $status = $self->shell_exec( JOBS=>[{ cmd=>$cmd, log=>$logfile, },], );
    #if ($status) {
    #  my $siminit_flow_lockfile = $self->{scoped_vars}->{"build_target_static"} . "/" . &ToolConfig::ToolConfig_get_general_var("siminit_flow_lockfile");      
    #  system("rm -rf $siminit_flow_lockfile");
    #}

    return $status;
}

sub run {
    my $self = shift;

    my $flowVars     = $self->{scoped_vars};
    my $scopedVars   = $self->{scoped_vars};
    #my $targetRoot   = $self->{scoped_vars}->{targetRoot};
    #my $targetDir    = $self->{outDir};

    #my $testlist_hdl = "$ENV{MODEL_ROOT}/verif/gen/fc_test.hdl";
    #my $sim_init_log  = "$targetDir/gen_testlist.log";
    #my $run_dir      = '/tmp/' . $ENV{USER} . '/sim_init_run/' . $$;
    my $status = 0;

    # dutconnect
    $status = $self->dutconnect_setup();
    #$self->sync_dir($targetDir) ;

    $self->{run_status} = $status ;
    return $status;

}


#####################################################################
# VTPSIMconnect Stage
######################################################################
package vtpsim_connect;

use bman_lib_base;
use bman_base;
use RTLUtils;
use ToolConfig;
use File::Basename;
use File::Path;
use File::Copy;
use File::Slurp;

use Utils;
use List::Util qw(first);
use warnings;
use strict;


@vtpsim_connect::ISA = ("bman_lib_base");

# Base class
use parent qw( bman_model_base );


########## Stage Body ##########

sub vtpsim_connect_setup {
    my $self = shift;
    my $status = 0;

    $status = $self->gen_vtpsim_connections();

    return $status;
}

sub gen_vtpsim_connections {
    my $self = shift;
    my $cmd;
    my $status;
    #my $targetDir    = $self->{outDir};
    my $logfile   = "$self->{targetLog}"; ## TODO

    $cmd = "$ENV{MODEL_ROOT}/scripts/verif/dft/vtpsim_connect.pl $ENV{MODEL_ROOT} $ENV{MODEL_ROOT}/scripts/verif/dft/vtpsim_connect.cfg";
    $status = $self->shell_exec( JOBS=>[{ cmd=>$cmd, log=>$logfile, },], );

    return $status;
}

sub run {
    my $self = shift;

    my $flowVars     = $self->{scoped_vars};
    my $scopedVars   = $self->{scoped_vars};
    #my $targetRoot   = $self->{scoped_vars}->{targetRoot};
    #my $targetDir    = $self->{outDir};

    #my $testlist_hdl = "$ENV{MODEL_ROOT}/verif/gen/fc_test.hdl";
    #my $sim_init_log  = "$targetDir/gen_testlist.log";
    #my $run_dir      = '/tmp/' . $ENV{USER} . '/sim_init_run/' . $$;
    my $status = 0;

    # vtpsim_connect
    $status = $self->vtpsim_connect_setup();
    #$self->sync_dir($targetDir) ;

    $self->{run_status} = $status ;
    return $status;

}

### GK

#####################################################################
# gen_spf_files  Stage
######################################################################
package gen_spf_files;

use bman_lib_base;
use bman_base;
use RTLUtils;
use ToolConfig;
use File::Basename;
use File::Path;
use File::Copy;
use File::Slurp;

use Utils;
use List::Util qw(first);
use warnings;
use strict;


@gen_spf_files::ISA = ("bman_lib_base");

# Base class
use parent qw( bman_model_base );


########## Stage Body ##########

sub gen_spf_files_setup {
    my $self = shift;
    my $status = 0;

    $status = $self->gen_spf_collateral();

    return $status;
}

sub gen_spf_collateral {
    my $self = shift;
    my $cmd;
    my $status;
    #my $targetDir    = $self->{outDir};
    my $logfile   = "$self->{targetLog}"; ## TODO

    #$cmd = "$ENV{MODEL_ROOT}/scripts/verif/vtpsim_connect.pl $ENV{MODEL_ROOT} $ENV{MODEL_ROOT}/scripts/verif/vtpsim_connect.cfg";
    #$cmd = "$SPF_ROOT/bin/spf_perl_pp --tapSpecFile $ENV{MODEL_ROOT}/subIP/tap/verif/rdl/spf/tap_top_level.spfspec --testSeqFile $ENV{MODEL_ROOT}/subIP/chassis_dft_val_global/v20171027/spf_sequences.1p0/tap/sub_system/tap_espf_test_template.espf --var "tap=all_taps" --var "test_type=idcode" --itppFile all_tap_idcode.itpp";
    RTL_msg(msg=> "TODO  Add SPF commands here until content is ready\n");    
    $status = $self->shell_exec( JOBS=>[{ cmd=>$cmd, log=>$logfile, },], );

    return $status;
}

sub run {
    my $self = shift;

    my $flowVars     = $self->{scoped_vars};
    my $scopedVars   = $self->{scoped_vars};
    #my $targetRoot   = $self->{scoped_vars}->{targetRoot};
    #my $targetDir    = $self->{outDir};

    #my $testlist_hdl = "$ENV{MODEL_ROOT}/verif/gen/fc_test.hdl";
    #my $sim_init_log  = "$targetDir/gen_testlist.log";
    #my $run_dir      = '/tmp/' . $ENV{USER} . '/sim_init_run/' . $$;
    my $status = 0;
    
    $status = $self->gen_spf_files_setup();
    
    $self->{run_status} = $status ;
    return $status;

}

#####################################################################
# IOSF_SB_TRK_Decode Stage
######################################################################
package sb_decode_gen;

use bman_lib_base;
use bman_base;
use RTLUtils;
use ToolConfig;
use File::Basename;
use File::Path;
use File::Copy;
use File::Slurp;

use Utils;
use List::Util qw(first);
use warnings;
use strict;

@sb_decode_gen::ISA = ("bman_lib_base");

# Base class
use parent qw( bman_model_base );

########## Stage Body ##########

sub sb_decode_gen {
    my $self = shift;
    my $cmd;
    my $logfile   = "$self->{targetLog}"; ## TODO
    my $status = 0;
    my $sbr_ROOT = ToolConfig::ToolConfig_get_tool_path('ipconfig/IOSF_SBC_RTR'); 
    # my $aceroot_path = ToolConfig::get_general_var("aceroot_dpath");
    my $aceroot_path = $ENV{MODEL_ROOT};

    $cmd = "$ENV{MODEL_ROOT}/scripts/verif/dft/gen_sbid_decode.pl $aceroot_path $sbr_ROOT/tools/collage/network/tools/collage/tb_specs/sbr_fabric.csv $sbr_ROOT/tools/collage/network/tools/collage/tb_specs/tb_connection.txt";
    $status = $self->shell_exec( JOBS=>[{ cmd=>$cmd, log=>$logfile, },], );
    
    return $status;
}

sub run {
    my $self = shift;
    my $enable_flag = $self->get_clo()->get_option(-flag => "-enable_sim_init");

    my $flowVars     = $self->{scoped_vars};
    my $scopedVars   = $self->{scoped_vars};
    #my $targetRoot   = $self->{scoped_vars}->{targetRoot};
    #my $targetDir    = $self->{outDir};

    #my $testlist_hdl = "$ENV{MODEL_ROOT}/verif/gen/fc_test.hdl";
    #my $sim_init_log  = "$targetDir/gen_testlist.log";
    #my $run_dir      = '/tmp/' . $ENV{USER} . '/sim_init_run/' . $$;
    my $status = 0;

    # sb_decode_gen
    $status = $self->sb_decode_gen();
    #$self->sync_dir($targetDir) ;

    $self->{run_status} = $status ;
    return $status;

}
### GK








#####################################################################
# Testlist Generation Stage
######################################################################
package gen_testlist; 
use bman_lib_base;
use bman_base;
use RTLUtils;
use ToolConfig;
use File::Basename;
use File::Path;
use File::Copy;
use File::Slurp;

use Utils;
use List::Util qw(first);
use warnings;
use strict;

@gen_testlist::ISA = ("bman_lib_base");

# Base class
use parent qw( bman_model_base );

########## Stage Body ##########
sub gen_func_testlist {
    my $self = shift;

    my $model_name     = &ToolConfig::get_facet( 'ace_model_name' );

    my $model_tag = "";

    if ($model_name =~ /^fc/) {
      $model_tag = "fc";
    } else {
      $self->fatal( "Fatal processing functional model name : '$model_name' : $!" );
    }
    my $testPattern    = exists $ENV{TEST_SEARCH_PATTERN} ? $ENV{TEST_SEARCH_PATTERN} : "*.svh";
    my $testPkgHdlFile = $model_tag . "_test_pkg.hdl";
    my $genHdlFile     = $model_tag . "_test.hdl";
    my $genSvhFile     = uc($model_tag) . "TestList.svh";

    my $genDir = $self->{scoped_vars}->{build_target_static}. "/verif/gen";
    &RTL_mkDir($genDir) if(not -e $genDir);

    my $testsHdlFile = "$genDir/$genHdlFile";
    my $testsSvhFile = "$genDir/$genSvhFile";

    my @testList = `find $ENV{MODEL_ROOT}/verif/tests/fc -name "$testPattern"`;
    push(@testList, `find $ENV{MODEL_ROOT}/verif/tests/glsfunc -name "$testPattern"`);
    push(@testList, `find $ENV{MODEL_ROOT}/verif/tests/misc -name "$testPattern"`);

    if (updateCheck($testsSvhFile, \@testList, $testsHdlFile)) {

        RTL_msg(msg=> "Creating $genSvhFile and $genHdlFile\n");

        # Comment header to remind people that these files are generated
        write_file($testsSvhFile, "// Generated file by gen_func_testlist\n\n");

        write_file($testsHdlFile, "# Generated file by gen_func_testlist\n\n");
        append_file($testsHdlFile, "\$hdl_spec = {\n\n");
        append_file($testsHdlFile, "    -vlog_incdirs => [\n");

        foreach (sort @testList) {
            chomp($_);
            # Splits the UNIX path into an array so that we can pull just the
            #   last few bits of the path that we need easily.
            my @dirArray = split(/\//, $_);

            # Only care about test names that match directory name
            if (grep /$dirArray[-2]/, $dirArray[-1]) {
                # SVH filmsg=> e only needs the file name
                append_file($testsSvhFile, "\`include \"$dirArray[-1]\"\n");

                # HDL file needs only the path to the file and not the file itself
                append_file($testsHdlFile, "        '$dirArray[-6]/$dirArray[-5]/$dirArray[-4]/$dirArray[-3]/$dirArray[-2]',\n");
            }
        }

        append_file($testsHdlFile, "    ],\n\n");
        append_file($testsHdlFile, "    -hdl_spec => [\n");
        append_file($testsHdlFile, "        'verif/tests/base/$testPkgHdlFile',\n");
        append_file($testsHdlFile, "    ],\n");
        append_file($testsHdlFile, "};");

    } else {
        RTL_msg(msg=> "No tests added or removed.  Not regenerating testlists.\n");
    }

    return 0;
}

sub updateCheck {
    my $svhFile      = $_[0];
    my @testList     = @{$_[1]};
    my $hdlFile      = $_[2];

    # If the files don't exist, obviously we have to re-create them
    if ((! -e $hdlFile) | (! -e $svhFile)) {
        RTL_msg(msg=> "Missing generated file\n");
        return 1;
    }

    open(SVHFILE, "<$svhFile");
    my @lines = <SVHFILE>;
    close(SVHFILE);

    # Checks that each test that we found in the tests directory exists in the
    #   test list SVH file
    foreach (@testList) {
        chomp($_);
        my @dirArray = split(/\//, $_);

        # Only care about test names that match directory name
        if (grep /$dirArray[-2]/, $dirArray[-1]) {
            if (!(grep /$dirArray[-1]/, @lines)) {
                RTL_msg(msg=> "\nNew test(s) found\n\n");
                return 1;
            }
        }
    }

    # Checks that each test in the test list SVH file exists in our tests
    #   directory
    foreach (@lines) {
        my @tempArray = split(/\"/, $_);

        if (!(grep /$tempArray[-2]/, @testList)) {
            RTL_msg(msg=> "\nTest(s) removed\n\n");
            return 1;
        }
    }

    return 0;
}

sub run {
    my $self = shift;

    my $flowVars     = $self->{scoped_vars};
    my $scopedVars   = $self->{scoped_vars};
    #my $targetRoot   = $self->{scoped_vars}->{targetRoot};
    #my $targetDir    = $self->{outDir};
    my $enable_flag = $self->get_clo()->get_option(-flag => "-enable_sim_init");

    #my $testlist_hdl = "$ENV{MODEL_ROOT}/verif/gen/fc_test.hdl";
    #my $sim_init_log  = "$targetDir/gen_testlist.log";
    #my $run_dir      = '/tmp/' . $ENV{USER} . '/sim_init_run/' . $$;
    my $status = 0;

    # testlist generation
    $status = $self->gen_func_testlist();

    $self->{run_status} = $status;
    return $status;

}

#####################################################################
# DFX Testlist Generation Stage
######################################################################
package gen_dft_testlist; 
use bman_lib_base;
use bman_base;
use RTLUtils;
use ToolConfig;
use File::Basename;
use File::Path;
use File::Copy;
use File::Slurp;

use Utils;
use List::Util qw(first);
use warnings;
use strict;

@gen_dft_testlist::ISA = ("bman_lib_base");

# Base class
use parent qw( bman_model_base );

########## Stage Body ##########
sub gen_dft_testlist {
    my $self = shift;
    my $model_tag = "dft";

    my $test_pattern      = exists $ENV{TEST_SEARCH_PATTERN} ? $ENV{TEST_SEARCH_PATTERN} : "*.svh";
    my $test_pkg_hdl_file = $model_tag . "_test_pkg.hdl";
    my $gen_svh_file      = $model_tag . "_test_list.svh";
    my $gen_hdl_file      = $model_tag . "_test.hdl";

    #my $gen_dir = "$ENV{MODEL_ROOT}//gen";
    my $gen_dir = $self->{scoped_vars}->{build_target_static}. "/verif/gen";
    &RTL_mkDir($gen_dir) if(not -e $gen_dir);

    my $tests_svh_file = "$gen_dir/$gen_svh_file";
    my $tests_hdl_file = "$gen_dir/$gen_hdl_file";

    my @test_list = `find $ENV{MODEL_ROOT}/verif/dft_tb/tests -name "$test_pattern"`;

    my $svh_file_str = "// Generated file by gen_dft_testlist\n\n";
    my $hdl_file_str = "# Generated file by gen_dft_testlist\n\n\$hdl_spec = {\n\n    -vlog_incdirs => [\n";

    foreach (sort @test_list) {
        chomp($_);

        # Splits the UNIX path into an array so that we can pull just the
        #   last few bits of the path that we need easily.
        my @dir_array = split(/\//, $_);
        my $file_name = pop @dir_array;   # pop off filename so only directories remain
        my $test_dir  = $dir_array[-1];   # store test directory

        # Only care about test names that match directory name
        if ($file_name eq "${test_dir}.svh") {
            # SVH file only needs the file name
            $svh_file_str .= "\`include \"$file_name\"\n";

            # HDL file needs only the path to the dir and not the file itself
            $hdl_file_str .= "        '" . join("/",@dir_array) . "',\n";
        }
    }

    $hdl_file_str .= "    ],\n\n    -hdl_spec => [\n        'verif/dft_tb/tests/base/$test_pkg_hdl_file',\n    ],\n};";

    if(not -e $tests_svh_file or not -e $tests_hdl_file) {
        RTL_msg(msg=> "Creating new $gen_svh_file and $gen_hdl_file\n");
        write_file($tests_svh_file, $svh_file_str);
        write_file($tests_hdl_file, $hdl_file_str);
        return 0;
    }

    open(SVHFILE, "<$tests_svh_file");
    my $prev_svh_file_str = join("", <SVHFILE>);
    close(SVHFILE);

    if($prev_svh_file_str ne $svh_file_str) {
        RTL_msg(msg=> "Overwriting existing $gen_svh_file and $gen_hdl_file\n");
        write_file($tests_svh_file, $svh_file_str);
        write_file($tests_hdl_file, $hdl_file_str);
        return 0;
    }

    RTL_msg(msg=> "No tests added or removed.  Not regenerating testlists.\n");
    return 0;
}

sub run {
    my $self = shift;

    my $flowVars     = $self->{scoped_vars};
    my $scopedVars   = $self->{scoped_vars};
    #my $targetRoot   = $self->{scoped_vars}->{targetRoot};
    #my $targetDir    = $self->{outDir};
    my $enable_flag = $self->get_clo()->get_option(-flag => "-enable_sim_init");

    #my $testlist_hdl = "$ENV{MODEL_ROOT}/verif/gen/fc_test.hdl";
    #my $sim_init_log  = "$targetDir/gen_dft_testlist.log";
    #my $run_dir      = '/tmp/' . $ENV{USER} . '/sim_init_run/' . $$;
    my $status = 0;

    # testlist generation
    $status = $self->gen_dft_testlist();

    $self->{run_status} = $status;
    return $status;

}

#####################################################################
# SubIP links generation Stage
######################################################################
package gen_subip;
use bman_lib_base;
use bman_base;
use RTLUtils;
use ToolConfig;
use File::Basename;
use File::Path;
use File::Copy;
use File::Slurp;

use Utils;
use List::Util qw(first);
use warnings;
use strict;

@gen_subip::ISA = ("bman_lib_base");

########## Stage Body ##########
sub run {
    my $self = shift;
    my $enable_flag = $self->get_clo()->get_option(-flag => "-enable_sim_init");

    my $flowVars     = $self->{scoped_vars};
    my $scopedVars   = $self->{scoped_vars};
    #my $targetRoot   = $self->{scoped_vars}->{targetRoot};
    #my $targetDir    = $self->{outDir};

    #my $sim_init_log  = "$targetDir/gen_subip.log";
    #my $run_dir      = '/tmp/' . $ENV{USER} . '/sim_init_run/' . $$;
    my $status = 0;

    RTL_cd($ENV{MODEL_ROOT});

    # subip link
    &subip_setup();

    $self->{run_status} = $status ;
    return $status;

}

sub subip_setup {

  my $subipDir = "$ENV{MODEL_ROOT}/subIP";
  if (!-d $subipDir ){
    &RTL_mkDir($subipDir);
  }

  my $ipconfigRef =   ToolConfig::ToolConfig_get_tool("ipconfig");
  my @iplist = sort keys %{$ipconfigRef->{SUB_TOOLS}};

  foreach my $ip (@iplist) {
    my $path =  ToolConfig::ToolConfig_get_tool_path("ipconfig/$ip");
    if (defined $path && !($ip eq $ENV{DUT})) {
      print_info("Linking dir %s %s \n", $ip, $path);
      if (-e $path && -d $path) {
        my $ipln = $subipDir . "/" . $ip;
        system ("ln","-snf",$path,$ipln);
      }
    }
  }

  return 0;
}

#####################################################################
# Y2K Config generation Stage
######################################################################
package gen_default_liblist;
use bman_lib_base;
use bman_base;
use RTLUtils;
use ToolConfig;
use File::Basename;
use File::Path;
use File::Copy;
use File::Slurp;
#use Data::Dumper;

use Utils;
use List::Util qw(first);
use warnings;
use strict;

@gen_default_liblist::ISA = ("bman_lib_base");

########## Stage Body ##########
sub run {
    my $self = shift;
    my $enable_flag = $self->get_clo()->get_option(-flag => "-enable_sim_init");
    my @allowed_model_list = ("fc_lite", "fc", "fc_64", "fc_64_serdes", "fc_8", "fc_8_serdes"); #only models in this listed are enabled.
    
    my $status = 0;
    my $ip_stub_lib = "soc_ip_stub_lib";
    my @model_list = @{$self->get_clo()->get_option( -flag => '-this_model', -scope => 'NON_SCOPED' )};

    my $gen_dir = $self->{scoped_vars}->{build_target_static}. "/verif/gen";
    &RTL_mkDir($gen_dir) if(not -e $gen_dir);

    foreach my $this_model (@model_list) {
      if (grep {$_ eq $this_model} @allowed_model_list) {
      #print_info (Dumper($this_model));
    
      my @model_libs = @{ $self->{designDB}->get_libs_for_model($this_model) };
      #my $default_liblist_inc_file = "verif/gen/".$this_model."_config_include.svh";;
      
      #my $rel_path = $self->{scoped_vars}->{build_target_static}. "/verif/gen";
      #$rel_path =~ s/$ENV{MODEL_ROOT}\/?//;
      # TBD klee18 my $default_liblist_inc_file = $gen_dir . "/" .$this_model."_config_include.svh";;
      my $default_liblist_inc_file = $gen_dir . "/fc_config_include.svh";;
      
      RTL_cd($ENV{MODEL_ROOT});
      
      write_file($default_liblist_inc_file, "    default liblist ");
      foreach my $mlib (@model_libs) {
        if (!($mlib eq $ip_stub_lib)) {
          append_file($default_liblist_inc_file, "$mlib ");
        }
      }
      append_file($default_liblist_inc_file," $ip_stub_lib\n\n");
        }
      }
    
    $self->{run_status} = $status ;
    return $status;

}

sub help {
}

1;
