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
#    Collage related definitions                    #
#####################################################
$ToolConfig_tools{coretools}{VERSION} = "N-2017.12-SP1-2";
$ToolConfig_tools{collage}{VERSION} = "4.10";
$ToolConfig_tools{collage_intf_def}{VERSION} = "3.7.6";
$ToolConfig_tools{collage_intf_def}{PATH} = "/p/hdk/rtl/cad/x86-64_linux30/intel/collage_intf_def/&get_tool_version('collage_intf_def')";
$ToolConfig_tools{collage}{ENV}{COLLAGE_INTF_DEF} = "&get_tool_path('collage_intf_def')";
$ToolConfig_tools{collage}{ENV}{COLLAGE_WORK} = "$MODEL_ROOT/target/&get_facet(dut)/collage/work/soc";
$ToolConfig_tools{collage}{ENV}{COLLAGE_CFG} = "$MODEL_ROOT/tools/collage/configs";
$ToolConfig_tools{collage}{ENV}{COLLAGE_DESIGN} = "soc";
$ToolConfig_tools{collage}{ENV}{CHASSIS_ID} = "config_soc";
$ToolConfig_tools{collage_utils}->{VERSION} = 'v13ww42a';
$ToolConfig_tools{collage}{OTHER}{collage_base_dir} = "&get_facet(dut)/collage";
$ToolConfig_tools{collage}{OTHER}{collage_work_dir} = "work";
#$ToolConfig_tools{collage}{OTHER}{collage_corekit_dir} = "ip_kits";
$ToolConfig_tools{collage}{OTHER}{collage_log_dir} = "log";
##$ToolConfig_tools{collage}{OTHER}{collage_reports_dir} = "reports";
$ToolConfig_tools{collage}{OTHER}{collage_rtl_dir} = "work/soc/gen/source/rtl";
$ToolConfig_tools{collage_build_cmd} = { # This is a conditional run of collage. It depends upon: collage_cache_cmd, collage_cmd
      EXEC => "$MODEL_ROOT/scripts/bin/common/run_collage.pl",
};
$ToolConfig_tools{collage_cache_cmd} = { # Can we avoid running collage by copying the output from a central cache?
    EXEC => q($vte_automation_ROOT/gk/bin/cache.pl -task collage -prj mst -get -alt gen),
};
$ToolConfig_tools{collage_cmd} = { # This runs collage, unconditionally
      EXEC => "rm -rf $MODEL_ROOT/src/gen/collage/*;mkdir -p $MODEL_ROOT/src/gen/collage;&get_tool_path('coretools')/bin/coreAssembler -timeout 5 -shell -x \'source &get_tool_path('collage')/core/common/tcl/collage_init.tcl\' -f $MODEL_ROOT/tools/collage/configs/config_soc/assemble/assembler.soc.tcl",
};
#rkoganti.  Updated Flowbee version to fix a bug with deps not working
# when a stage is default_active off
$ToolConfig_tools{rtltools}{SUB_TOOLS}{flowbee}{VERSION} = "1.01.08";
$ToolConfig_tools{rtltools}{SUB_TOOLS}{flowbee}{PATH} = "$RTL_PROJ_TOOLS/flowbee/master/&get_tool_version(rtltools/flowbee)";
$ToolConfig_tools{rtltools}{SUB_TOOLS}{flowbee}{OTHER}{modules} = [ "&get_tool_path(stage_ace)",
                              "&get_tool_path(stage_ace_command)",
                              "&get_tool_path(stage_collage_assemble)",
                              ];
$ToolConfig_tools{rtltools}{SUB_TOOLS}{flowbee}{OTHER}{default_dut} = "mby";
$ToolConfig_tools{runtools}{OTHER}{default_dut} = "mby";

$ToolConfig_tools{buildman}{SUB_TOOLS}{collage} = $ToolConfig_tools{collage};

push(@{$ToolConfig_tools{buildman}{SUB_TOOLS}{flowbee}{OTHER}{modules}}, "$MODEL_ROOT/cfg/stages/collage_preflow.pm");
push(@{$ToolConfig_tools{buildman}{SUB_TOOLS}{flowbee}{OTHER}{modules}}, "$MODEL_ROOT/cfg/stages/collage_postflow.pm");
push(@{$ToolConfig_tools{buildman}{SUB_TOOLS}{flowbee}{OTHER}{modules}}, "$MODEL_ROOT/cfg/stages/preflow_stage.pm.template");
$ToolConfig_tools{stage_bman_collage}{OTHER}{pre_flow} = { 
                                                           "(.default.)" => "collage_preflow",
                                                         };
$ToolConfig_tools{stage_bman_collage}{OTHER}{post_flow} = { 
                                                            "(.default.)" => "collage_postflow",
                                                          };
$ToolConfig_tools{buildman}{SUB_TOOLS}{stages}{SUB_TOOLS}{collage_postflow}{OTHER}{relevant_tools} = [qw( collage )];
### End collage related updates ***

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
$ToolConfig_tools{runtools}{ENV}{DESIGNWARE_HOME}                            = "&get_tool_path(vipsvt)";
$ToolConfig_tools{runtools}{OTHER}{repo_trex_output}                         = "$MODEL_ROOT/regression/&get_facet(dut)/tests";

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
$ToolConfig_tools{buildman}{OTHER}{pre_flow} = "UserCode::ndocs";

$ToolConfig_tools{vipsvt} = {
    VERSION    => "O-2018.06",
    PATH       => "$ENV{RTL_CAD_ROOT}/synopsys/designware/&get_tool_version()",
    ENV        => { SVT_VERSION     => "&get_tool_version()",
                    DESIGNWARE_HOME => "&get_tool_path()" },
    ENV_APPEND => { PATH            => "&get_tool_path()/bin" },
};

$ToolConfig_tools{dc_shell} = {
    VERSION    =>  "I-2013.12-SP5-6",
    PATH       => "/p/hdk/cad/designcompiler/&get_tool_version()",
    ENV_APPEND  => {
        'PERLLIB'   => "&get_tool_path()/Trex_Modules",
    },
};

$ToolConfig_tools{meta} = {
    PATH => "$ENV{RTL_PROJ_TOOLS}/meta/nhdk/&get_tool_version()",
    VERSION => "18.08.14",
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

# Added sim_init stage for automating FC collaterals generation
$ToolConfig_tools{stage_bman_sim_init}{OTHER}{modules} = "$ENV{MODEL_ROOT}/cfg/stages/sim_init.pm";
push(@{$ToolConfig_tools{buildman}{SUB_TOOLS}{flowbee}{OTHER}{modules}},  "&get_tool_var(stage_bman_sim_init, modules)");

$ToolConfig_tools{feedtools}{ENV}{JASPER_UTILS}= "&get_tool_path(jasper_utils)";

$ToolConfig_tools{"mgm"} = {
    VERSION => "2.31",
    PATH => "$ENV{RTL_PROJ_TOOLS}/mgm/nhdk/&get_tool_version()",
    MGM_ARGS => {
        BLOCKS => {
            mby => ["parser",
                    "classifier",
                    "ppe_stm",
                   ],
        },
        PHYSICAL_PARAMS => "$ENV{MODEL_ROOT}/tools/mgm/mby_physical_params.csv",
        REPORT_DIR => "$ENV{MODEL_ROOT}/target/".&ToolConfig::get_facet("dut")."/mgm_run/rtl",
        #REPORT_DIR => "$ENV{MODEL_ROOT}/target/mgm/rtl",
        PREFIX => "mby",
	CMO  => [
	    "/tmp/dummy"
	    ],
    },
    EXEC => "&get_tool_path()/bin/mgm",
    ENV => {
        MGM_ROOT => "&get_tool_path()",
        MGM_VER  => "&get_tool_version()",
        MGM_RTL  =>  "&get_tool_path()/rtl",
    },
    ENV_APPEND => {
     PATH => "&get_tool_path()/bin", 
    },

    OTHER => {
            LOCAL_ASIC_MEMORIES => "YES",
    }, 
};
$ToolConfig_tools{buildman}{ENV}{MGM_ROOT}  = "&get_tool_path(mgm)";
$ToolConfig_tools{runtools}{ENV}{MGM_ROOT}  = "&get_tool_path(mgm)";
$ToolConfig_tools{buildman}{ENV}{MGM_TSMC_N7}  = "YES";



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
    EXEC => "&get_tool_path()/bin/sbt  -J-Xmx2G  -Dhttp.proxyHost=proxy-chain.intel.com -Dhttp.proxyPort=911 -Dhttps.proxyHost=proxy-chain.intel.com -Dhttps.proxyPort=911 -java-home &get_tool_path('java') -sbt-dir /tmp/$ENV{USER}/dot_sbt -ivy /tmp/$ENV{USER}/dot_ivy -sbt-boot /tmp/$ENV{USER}/dot_sbt/boot",
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

# for Cadence PCIe BFMs
$ToolConfig_tools{cdn_vip_root} = { VERSION => 'vipcat_11.30.057-08_Aug_2018_10_14_18',
                                  PATH    => "$ENV{RTL_CAD_ROOT}/cadence/vipcat/&get_tool_version()",
                                };
$ToolConfig_tools{vipcat} = { PATH => "&get_tool_path(cdn_vip_root)",
                              OTHER => {
                                 CDS_ARCH => "lnx86",
                                 VIPCAT_LIBS => "&get_tool_path(cdn_vip_root)/tools.lnx86/lib/64bit",
                              },
                            };
$ToolConfig_tools{denali} = { VERSION => 'vipcat_11.30.057-08_Aug_2018_10_14_18',
                            #VERSION => '&get_tool_version(cdn_vip_root)',
                              PATH    => "$ENV{RTL_CAD_ROOT}/cadence/vipcat/&get_tool_version()/tools/denali_64bit",
                              #PATH    => "&get_tool_path(cdn_vip_root)/tools/denali_64bit",
                              OTHER => {
                                DENALI_LIBS => "&get_tool_path()/verilog",
                                VIPCAT_LIBS => "&get_tool_path()/tools.lnx86/lib/64bit",
                              },
                            };
#VIPCAT update
$ToolConfig_tools{buildman}{ENV}{CDN_VIP_ROOT} = "&get_tool_path(cdn_vip_root)";
$ToolConfig_tools{buildman}{ENV}{DENALI} = "&get_tool_path(denali)";

1;
