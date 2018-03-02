
package ToolData;
use strict;
use warnings;
use File::Basename qw(fileparse);

######################################################################
# NOTE: Use these variables instead of $ENV when possible
# Try to use &get_env_var(VARIABLE_NAME) in hash string entries otherwise
# Also, please use variables instead of raw absolute paths.
# Add needed variables in this list from baseline_tools/GeneralVars.pm
######################################################################
use vars
  qw(%ToolConfig_ips
     %ToolConfig_tools
     $MODEL_ROOT
     $IP_MODELS
     $IP_RELEASE
     $IP_RELEASES
     $RTL_PROJ_TOOLS
     $RTL_PROJ_DATA
   );

use lib $ToolConfig_tools{iptooldataextras}{PATH};
use IPToolDataExtras qw(import_files do_with_warn);

$ToolConfig_tools{ipconfig} = {
  PATH => "$MODEL_ROOT",
  VERSION => 'n/a',
  OTHER => {
    IMPORT  => ["cfg/mby_IPToolData.pm",],
      UDF_MAP => {
            'mby' => {
              'UDF' => [
                    "$MODEL_ROOT/cfg/ace/mby.udf",
                    ],
                    'SCOPE' => 'mby',
        ACERC => {
            ENABLE_AUTO_POP_DEP_LIBS => "0",
            ENABLE_RECURSIVE_DEP_LIBS => "1",
                                }
      }, ## mby
    }, ## UDF_MAP
  }, ## OTHER

  SUB_TOOLS => \%ToolConfig_ips,
};



push (@{$ToolConfig_tools{mby}{OTHER}{IMPORT}}, ());
$ToolConfig_tools{mby}{PATH} = "";
IPToolDataExtras::import_files("mby",\%ToolConfig_tools);
IPToolDataExtras::import_files("ipconfig",\%ToolConfig_tools);

$ToolConfig_ips{mby}{OTHER}{LIBS} = [
            "&get_tool_path()/cfg",
            "&get_tool_path()/cfg/ace/lib",
            "&get_tool_path(ipconfig/mby_tlm)/cfg/ace/lib/",
];

push (@{$ToolConfig_ips{mby}{OTHER}{SUB_SCOPES}}, (
             "vcc_modeling",
             "iosf_sideband_vc",
             "iosf_primary_bfm", 
             "chassis_pg_vc",
             "ip_ccu_vc",
             "chassis_reset",
));

push (@{$ToolConfig_ips{mby}{OTHER}{SEARCH_PATHS}}, (
            "&get_tool_path(ipconfig/chassis_reset)",
            "&get_tool_path(ipconfig/iosf_sideband_vc)",
            "&get_tool_path(ipconfig/iosf_primary_bfm)/ph4",
            "&get_tool_path(ipconfig/vcc_modeling)",
            "&get_tool_path(ipconfig/chassis_pg_vc)",
            "&get_tool_var(ipconfig/ip_ccu_vc, SEARCH_PATHS)",
            "&get_tool_var(ipconfig/sva_lib_ip, SEARCH_PATHS)",
));

$ToolConfig_ips{chassis_pg_vc} = {
    PATH => "$ENV{IP_RELEASES}/ChassisPowerGatingVIP/&get_tool_version()",
    VERSION => "ChassisPowerGatingVIP_2013WW30",
    OTHER   => {
        SEARCH_PATHS   => [ "&get_tool_path()", ],
    },
};

$ToolConfig_ips{vcc_modeling} = {
    PATH => "$ENV{IP_RELEASES}/VccModeling/&get_tool_version()",
    VERSION => "VccModeling_v130227",
    OTHER   => {
        SEARCH_PATHS   => [ "&get_tool_path()", ],
    },
};
$ToolConfig_ips{iosf_primary_bfm} = {
    PATH => "$ENV{IP_RELEASES}/iosf_primary_bfm/&get_tool_version()",
    VERSION => "iosf_primary_bfm",
    OTHER   => {
        SEARCH_PATHS   => [ "&get_tool_path()", 
        "&get_tool_path()/ph4",
        ],
    },
};
$ToolConfig_ips{iosf_sideband_vc} = {
    PATH => "$ENV{IP_RELEASES}/iosf_sideband_vc/&get_tool_version()",
    VERSION => "iosf_sideband_vc_template",
    OTHER   => {
        SEARCH_PATHS   => [ "&get_tool_path()", ],
    },
};

$ToolConfig_ips{ip_ccu_vc} = {
    PATH => "$ENV{IP_RELEASES}/ip-ccu-vc/&get_tool_version()",
    VERSION => "ip-ccu-vc-2013WW21r130521",
    OTHER   => {
        SEARCH_PATHS   => [ "&get_tool_path()/ccu_vc_WW21", ],
    },};

$ToolConfig_ips{chassis_reset} = {
    PATH => "$ENV{IP_RELEASES}/chassis_reset_pkg/&get_tool_version()",
    VERSION => "chassis_reset_pkg_2013WW30",
    OTHER   => {
        SEARCH_PATHS   => [ "&get_tool_path()", ],
    },
};
$ToolConfig_tools{INTC_LIB_SCOREBOARD} = {
  PATH => "$ENV{RTL_CAD_ROOT}/intel/intc_lib_scoreboard/&get_tool_version()",
  VERSION => "v20141128",
    OTHER   => {
        SEARCH_PATHS   => [ "&get_tool_path()", ],
    },
};
$ToolConfig_ips{sva_lib_ip} = {
  PATH => "/nfs/site/disks/hdk.cad.1/linux_2.6.16_x86-64/sva_lib/&get_tool_version()/SVA_LIB",
  VERSION => "6.0p1",
    OTHER   => {
        SEARCH_PATHS   => [ "&get_tool_path()", ],
    },
};

$ToolConfig_tools{nebulon}{VERSION} = 'd18ww08.3';


$ToolConfig_tools{buildman}{ENV}{OVM_HOME} = "&get_tool_path(ovm)";
$ToolConfig_tools{buildman}{ENV}{XVM_HOME} = "&get_tool_path(xvm)";
$ToolConfig_tools{buildman}{ENV}{UVM_HOME} = "&get_tool_path(uvm)";
$ToolConfig_tools{buildman}{ENV}{SAOLA_HOME} = "&get_tool_path(saola)";
$ToolConfig_tools{buildman}{ENV}{INTC_LIB_HOME} = "&get_tool_path(INTC_LIB_SCOREBOARD)";
$ToolConfig_tools{buildman}{ENV}{CTECH_LIB_NAME} = "CTECH_v_rtl_lib";
#$ToolConfig_tools{buildman}{ENV}{CTECH_LIB_PATH} = "/p/hdk/cad/ctech/v15ww50e";
$ToolConfig_tools{buildman}{ENV}{NEBULON_DIR} = "&get_tool_path(nebulon)";
$ToolConfig_tools{buildman}{ENV}{ACE_HOME} = "&get_tool_path(ace)";
$ToolConfig_tools{buildman}{ENV}{VCS_TARGET_ARCH} = "suse64";
$ToolConfig_tools{buildman}{ENV}{VCS_HOME} = "&get_tool_path(vcsmx)";
$ToolConfig_tools{buildman}{ENV}{MBY_ROOT} = $ENV{MODEL_ROOT};
$ToolConfig_tools{buildman}{ENV}{SNPSLMD_LICENSE_FILE} = "&get_tool_getLf(synopsys)"; 
$ToolConfig_tools{buildman}{ENV_APPEND}{PATH} = ":&get_tool_path(ace)/bin:"; ## solve saola issue ( unknown os)


$ToolConfig_tools{buildman}{ENV}{JASPERGOLD_VER}  = "&get_tool_version(jaspergold)/";
$ToolConfig_tools{buildman}{ENV}{JASPERGOLD_UXDB_PATH}  = "&get_tool_env_var(jaspergold,JASPERGOLD_UXDB_PATH)";
$ToolConfig_tools{buildman}{ENV}{JASPERGOLD_UXDB_ARGS}  = "&get_tool_env_var(jaspergold,JASPERGOLD_UXDB_ARGS)";


1;
