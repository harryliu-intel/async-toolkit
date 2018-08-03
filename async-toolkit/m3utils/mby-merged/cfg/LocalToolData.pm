# vim: noai : ts=3 : sw=3 : expandtab : ft=perl

# DO NOT REMOVE!!!  Learn to code warning-free perl instead.
#
use warnings FATAL => 'all';
use lib "$ENV{RTL_PROJ_TOOLS}/proj_utils/nhdk/latest/GetNB";
use GetNB;

package ToolData;

my $nb_object                                                                = GetNB->new({key=>"build::mby"});

$general_vars{aceroot_dpath}                                                 = "$MODEL_ROOT/target/&get_facet(dut)/aceroot";

$ToolConfig_tools{ace_utils}{OTHER}{printcmd}                                = "0";
$ToolConfig_tools{ace_utils}{OTHER}{cleanenvrc}                              = "0";

$ToolConfig_tools{'rtltools'}{SUB_TOOLS}{flowbee}{OTHER}{enable_gkmnm}       = "1";
$ToolConfig_tools{'rtltools'}{OTHER}{dont_stop_feeder}                       = "1";
$ToolConfig_tools{'rtltools'}{OTHER}{resource_def}                           = "$ENV{MODEL_ROOT}/cfg/resource.xml";

$ToolConfig_tools{flowbee}{OTHER}{default_dut}                               = "mby";

$ToolConfig_tools{'febe3'}{OTHER}{resource_def}                              = "$ENV{MODEL_ROOT}/cfg/resource.xml";
$ToolConfig_tools{'febe3'}{'SUB_TOOLS'}{'lintra'}{'VERSION'}                 = "&get_tool_version(lintra)";
$ToolConfig_tools{'febe3'}{'SUB_TOOLS'}{'lintra'}{'PATH'}                    = "&get_tool_path(lintra)";

#####################################################
#    Configure Environment variables for Trex/Run   #
#####################################################
$ToolConfig_tools{runtools}{ENV}{TREX_PACKAGES}                              = "&get_tool_path(casa/casa_utils)/trex/casa_TREX.pm";
$ToolConfig_tools{runtools}{OTHER}{'task_additional_policy_module_paths_l'}  = "&get_tool_path(casa/casa_utils)/trex/casa_per_regression.pm";
$ToolConfig_tools{runtools}->{OTHER}->{repo_trex_output}                     = "results/%DUT%/";
$ToolConfig_tools{runtools}{ENV}{JASPERGOLD_UXDB_PATH}                       = "&get_tool_env_var(jaspergold,JASPERGOLD_UXDB_PATH)";
$ToolConfig_tools{runtools}{ENV}{JASPERGOLD_UXDB_ARGS}                       = "&get_tool_env_var(jaspergold,JASPERGOLD_UXDB_ARGS)";
$ToolConfig_tools{runtools}{ENV}{JASPERGOLD_VER}                             = "&get_tool_version(jaspergold)/";
$ToolConfig_tools{runtools}{ENV}{JG_VERSION_LATEST}                          = "&get_tool_version(jaspergold)";

#####################################################
#    Configure Environment variables for Nebulon    #
#####################################################
$ToolConfig_tools{nebulon}{VERSION}                                          = 'd18ww24.4';

#####################################################
#    Configure Environment variables for VCS/VERDI  #
#####################################################
$ToolConfig_tools{verdi3}{VERSION}                                          = "N-2017.12-1";
$ToolConfig_tools{vcs}{VERSION}                                             = "N-2017.12-1";
$ToolConfig_tools{vcsmx}{VERSION}                                           = "N-2017.12-1";
$ToolConfig_tools{vcs}{ENV}{VCS_HOME}                                       = '&get_tool_path()';

#####################################################
#    Configure Environment variables for Bman       #
#####################################################
$ToolConfig_tools{buildman}{ENV}{OVM_HOME}                                   = "&get_tool_path(ovm)";
$ToolConfig_tools{buildman}{ENV}{XVM_HOME}                                   = "&get_tool_path(xvm)";
$ToolConfig_tools{buildman}{ENV}{UVM_HOME}                                   = "&get_tool_path(uvm)";
$ToolConfig_tools{buildman}{ENV}{SAOLA_HOME}                                 = "&get_tool_path(saola)";
$ToolConfig_tools{buildman}{ENV}{INTC_LIB_HOME}                              = "&get_tool_path(INTC_LIB_SCOREBOARD)";
$ToolConfig_tools{buildman}{ENV}{CTECH_LIB_NAME}                             = "CTECH_v_rtl_lib";
#$ToolConfig_tools{buildman}{ENV}{CTECH_LIB_PATH}                             = "/p/hdk/cad/ctech/v15ww50e";
$ToolConfig_tools{buildman}{ENV}{NEBULON_DIR}                                = "&get_tool_path(nebulon)";
$ToolConfig_tools{buildman}{ENV}{ACE_HOME}                                   = "&get_tool_path(ace)";
$ToolConfig_tools{buildman}{ENV}{VCS_TARGET_ARCH}                            = "suse64";
$ToolConfig_tools{buildman}{ENV}{VCS_HOME}                                   = "&get_tool_path(vcsmx)";
# If needed to hard code override
#$ToolConfig_tools{buildman}{ENV}{VCS_HOME}                                  = "N-2017.12-1";
#$ToolConfig_tools{buildman}{ENV}{VCS_HOME}                                  = "/p/hdk/rtl/cad/x86-64_linux26/synopsys/vcsmx/N-2017.12-1";
$ToolConfig_tools{buildman}{ENV}{MBY_ROOT}                                   = $ENV{MODEL_ROOT};
$ToolConfig_tools{buildman}{ENV}{SNPSLMD_LICENSE_FILE}                       = "&get_tool_getLf(synopsys)";
$ToolConfig_tools{buildman}{ENV_APPEND}{PATH}                                = ":&get_tool_path(ace)/bin:"; ## solve saola issue ( unknown os)
$ToolConfig_tools{buildman}{ENV}{JASPERGOLD_VER}                             = "&get_tool_version(jaspergold)/";
$ToolConfig_tools{buildman}{ENV}{JASPERGOLD_UXDB_PATH}                       = "&get_tool_env_var(jaspergold,JASPERGOLD_UXDB_PATH)";
$ToolConfig_tools{buildman}{ENV}{JASPERGOLD_UXDB_ARGS}                       = "&get_tool_env_var(jaspergold,JASPERGOLD_UXDB_ARGS)";

# Natural Docs hook to call cfg/bin/doc_me as a preflow to vcs
$ToolConfig_tools{'buildman'}{SUB_TOOLS}{'flowbee'}{OTHER}{USERCODE} .= ":$ENV{MODEL_ROOT}/cfg/stages/UserCode.pm";

$ToolConfig_tools{dc_shell} = {
    VERSION    =>  "I-2013.12-SP5-6",
    PATH       => "/p/hdk/cad/designcompiler/&get_tool_version()",
    ENV_APPEND  => {
        'PERLLIB'   => "&get_tool_path()/Trex_Modules",
    },
};

$ToolConfig_tools{meta} = {
    PATH => "$ENV{RTL_PROJ_TOOLS}/meta/nhdk/&get_tool_version()",
    VERSION => "18.08.03",
    ENV_APPEND  => {
        'PATH'  => "&get_tool_path()/bin",
    },

};

$ToolConfig_tools{cm3} = {
    VERSION => "d5.10.0-20180711",
    PATH => "$ENV{RTL_CAD_ROOT}/opensource/cm3/&get_tool_version()",
    EXEC => "&get_tool_path()/bin/cm3",
};


$ToolConfig_tools{onesource} = {
   PATH => "/p/hdk/rtl/cad/x86-64_linux30/dt/OneSourceBundle/&get_tool_version()",
   VERSION => '17.48.5p3',
};

$ToolConfig_tools{bluespec} = {
   PATH => "/p/hdk/rtl/cad/x86-64_linux30/bluespec/bluespec/&get_tool_version()",
   VERSION => '2017.07.A',
   ENV_APPEND => {
      LM_LICENSE_FILE => '4464@bluespec01p.elic.intel.com',
      PATH => "&get_tool_path()/bin",
   },
   ENV => {
      BLUESPEC_HOME => "&get_tool_path()",
      BLUESPECDIR => "&get_tool_path()/lib",
   }
};

$ToolConfig_tools{'buildman'}{SUB_TOOLS}{'stages'}{SUB_TOOLS}{'default'}{OTHER}{netbatch_resource} = {
      cores => "1C",
      jobsPerDelegate => 1,
      local => 0,
      maxDelegates => 40,
      mem => "4G",
      os => "SLES11SP4",
      priority => 1,
      qslot => $nb_object->{qslot},
      queue => $nb_object->{pool},
      submissionArgs => "",
      tag => "dynamic",
    };

$ToolConfig_tools{buildman}{OTHER}{UDFS} = ["&get_tool_path(buildman)/udf/buildman.udf",
    "&get_tool_path(bman_stages)/stages_attributes.udf"];

# Jasper
$ToolConfig_tools{jaspergold} = {
    #VERSION => '2017.03p002',
    VERSION => '2017.06p002__XLM17.04',
    PATH => "$RTL_CAD_ROOT/jasper/jaspergold/&get_tool_version()",
    EXEC => "&get_tool_path()/bin/jg -proj ${$}_jgproject",
    ENV_PREPEND  => {
                    # For releases 2015.03p002 and earlier
        'TEMPUSD_LICENSE_FILE'   => '29040@jasper01p.elic.intel.com',
                   # For releases 2015.06 and later
        'CDS_LIC_FILE' => '5280@cadence24p.elic.intel.com',
        PATH => "&get_tool_path()/bin",
    },
    SUB_TOOLS => {        "intel_jasper_library" => "&get_tool(intel_jasper_library)",    },
    OTHER => {        'HDK_CATEGORY' => 'Formal Property Verification (FPV)',
                      'HDK_PRODUCT' => 'JasperGold',
                      'HDK_RELEASE_NOTES' => 'http://goto/jasper',
                      # [2015ww34] TSETUP options added based on advice from mmaidmen
                      'TSETUP_TOOLNAME' => 'jasper',
                      'TSETUP_VENDOR' => 'jasper',
                      'TSETUP_IP_VERSIONS' => {}    },
         ENV_OVERRIDE      => {
                'IJL_ROOT' => "/p/hdk/rtl/cad/x86-64_linux30/jasper/intel_jasper_library/3.1",
                },
};

# Adding stage foo
$ToolConfig_tools{"bman_stages"} = {
    VERSION => "14.06.15",
    PATH => "$ENV{RTL_PROJ_TOOLS}/bman_stages/nhdk/&get_tool_version()",
};
$ToolConfig_tools{"foo"} = {
   VERSION => "&get_tool_version(bman_stages)",
   PATH => "&get_tool_path(bman_stages)",
   OTHER   => {
               enable_stage_caching => 0,
               enable_stage_digest => 0,
               modules => "&get_tool_path(foo)/foo.pm",
               stage_digest_rules => ["acebuild", "foo"],
             },
};

push @{$ToolConfig_tools{'buildman'}{SUB_TOOLS}{'flowbee'}{OTHER}{'modules'}}, "&get_tool_var('foo','modules')";

$ToolConfig_tools{stage_bman_sgcdc} = {
  OTHER   => {
               enable_stage_caching => 0,
               enable_stage_digest => 0,
               modules => "&get_tool_path(buildman)/stages/sgcdc.pm",
               stage_digest_rules => ["acebuild", "sgcdc"],
             },
  PATH    => "&get_tool_path(buildman)",
  VERSION => "&get_tool_version(buildman)",
};

$ToolConfig_tools{stage_bman_genrtl}{OTHER}{modules} = "$ENV{MODEL_ROOT}/cfg/stages/genrtl.pm";
$ToolConfig_tools{jasper_utils} = {
  PATH    => "$ENV{RTL_PROJ_TOOLS}/jasper_utils/nhdk/&get_tool_version()",
  VERSION => "14.06.20",
};

$ToolConfig_tools{feedtools}{ENV}{JASPER_UTILS}= "&get_tool_path(jasper_utils)";

$ToolConfig_tools{"mgm"} = {
    VERSION => "1.8_try5",
    PATH => "$ENV{RTL_PROJ_TOOLS}/mgm/nhdk/&get_tool_version()",
    #PATH => "/nfs/sc/disks/nhdk_da.work.001/belfere/1.8_dev1",
    MGM_ARGS => {
        BLOCKS => {
            mby => ["mby",],
        },
        PHYSICAL_PARAMS => "$ENV{MODEL_ROOT}/tools/mgm/mby_physical_params.csv",
        REPORT_DIR => "$ENV{MODEL_ROOT}/target/".&ToolConfig::get_facet("dut")."/mgm_run/rtl",
        #REPORT_DIR => "$ENV{MODEL_ROOT}/target/mgm/rtl",
        PREFIX => "mby_prefix",
    },
    EXEC => "&get_tool_path()/bin/mgm",
    ENV => {
        MGM_ROOT => "&get_tool_path()",
        MGM_VER  => "&get_tool_version()",
        MGM_RTL  =>  "&get_tool_path()/rtl",
    },
    ENV_APPEND => {
        PATH => "&get_tool_path()/bin:/p/com/eda/intel/puni/2.7", #TODO: fix later to HDK puni
    },
};

$ToolConfig_tools{"mono"} = {
    VERSION => "5.2.0.224",
    PATH => "/usr/intel/pkgs/mono/&get_tool_version()",
    EXEC => "&get_tool_path()/bin/mono",
};

$ToolConfig_tools{"NaturalDocs"} = {
    VERSION => "1.52",
    #VERSION => "2.1.0.dev",     # This version in missing inline Image support, reverted back to older version until next rev available
    PATH => "$ENV{RTL_CAD_ROOT}/natural_docs/NaturalDocs/&get_tool_version()",
    # Version 1.52 runs in perl
    EXEC => "perl &get_tool_path()/NaturalDocs",
    # Version 2.xx runs with mono
    #EXEC => "&get_tool_exec(mono) &get_tool_path()/NaturalDocs.exe",
};

$ToolConfig_tools{"sbt"} = {
    VERSION => "1.1.2",
    PATH => "/usr/intel/pkgs/sbt/&get_tool_version()",
    EXEC => "&get_tool_path()/bin/sbt -Dhttp.proxyHost=proxy-us.intel.com -Dhttp.proxyPort=912 -Dhttps.proxyHost=proxy-us.intel.com -Dhttps.proxyPort=912 -java-home &get_tool_path('java') -sbt-dir /tmp/$ENV{USER}/dot_sbt -ivy /tmp/$ENV{USER}/dot_ivy -sbt-boot /tmp/$ENV{USER}/dot_sbt/boot",
    ENV_APPEND => {
      PATH => "&get_tool_path()/bin",
    },
    ENV => {
      JAVA_OPTS => "-Dhttp.proxyHost=proxy-us.intel.com -Dhttp.proxyPort=912 -Dhttps.proxyHost=proxy-us.intel.com -Dhttps.proxyPort=912",
      SBT_OPTS => "-java-home &get_tool_path('java') -sbt-dir /tmp/$ENV{USER}/dot_sbt -ivy /tmp/$ENV{USER}/dot_ivy -sbt-boot /tmp/$ENV{USER}/dot_sbt/boot"
    }
};

$ToolConfig_tools{"idea"} = {
    VERSION => "2018.1.4",
    PATH => "/usr/intel/pkgs/intellij/&get_tool_version()",
    EXEC => "&get_tool_path()/bin/idea.sh",
};

$ToolConfig_tools{"java"} = {
    VERSION => "1.8.0.151",
    PATH => "/usr/intel/pkgs/java/&get_tool_version()",
    EXEC => "&get_tool_path()/bin/java",
    ENV_APPEND => {
      JAVA_HOME => "&get_tool_path()",
    }
};


$ToolConfig_tools{"protobuf"} = {
    VERSION => "3.5.1",
    PATH => "/p/hdk/rtl/cad/x86-64_linux30/google/protobuf/&get_tool_version()",
};

$ToolConfig_tools{"regs2html"} = {
    VERSION => "18.05.14",
    PATH => "$ENV{RTL_PROJ_TOOLS}/regs2html/nhdk/&get_tool_version()",
};
