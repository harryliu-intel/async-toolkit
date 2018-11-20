# vim: noai : ts=3 : sw=3 : expandtab : ft=perl

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
         'default' => {
            'UDF'   => ["$MODEL_ROOT/cfg/ace/fc.udf",],
            'SCOPE' => 'mby',
            ACERC => {
               ENABLE_AUTO_POP_DEP_LIBS  => "0",
               ENABLE_RECURSIVE_DEP_LIBS => "1",
            },
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
   "&get_tool_path()/cfg/ace",
];

push (@{$ToolConfig_ips{mby}{OTHER}{SUB_SCOPES}}, (
   "CTECH",
   "CTECH_EXP",
   "vcc_modeling",
   "iosf_sideband_vc",
   "iosf_primary_bfm",
   "chassis_pg_vc",
   "ip_ccu_vc",
   "chassis_reset",
   "shdv",
));

push (@{$ToolConfig_ips{mby}{OTHER}{SEARCH_PATHS}}, (
   "&get_tool_path(ipconfig/chassis_reset                 )",
   "&get_tool_path(ipconfig/iosf_sideband_vc              )",
   "&get_tool_path(ipconfig/iosf_primary_bfm              )/ph4",
   "&get_tool_path(ipconfig/vcc_modeling                  )",
   "&get_tool_path(ipconfig/chassis_pg_vc                 )",
   "&get_tool_var(ipconfig/ip_ccu_vc,         SEARCH_PATHS)",
   "&get_tool_var(ipconfig/sva_lib_ip,        SEARCH_PATHS)",
	  "&get_tool_path(ipconfig/SVA_LIB)",
   "&get_tool_var(ipconfig/shdv,              SEARCH_PATHS)",
   "&get_tool_path(ipconfig/CTECH)",
   "&get_tool_path(ipconfig/CTECH_EXP)",
));

$ToolConfig_ips{CTECH} = {
   VERSION => "c2v18ww33a_dcg",
   PATH => "/p/hdk/cad/ctech/&get_tool_version()",
   OTHER   => {
      SEARCH_PATHS   => ["&get_tool_path()",],
   },
   ENV => {
      ctech_ROOT => "&get_tool_path()",
   },
}; 

$ToolConfig_ips{CTECH_EXP} = {
   VERSION => "ctech_exp_c2v18ww33a_dcg",
   PATH => "/p/hdk/cad/ctech/&get_tool_version()",
   OTHER   => {
      SEARCH_PATHS   => ["&get_tool_path()",],
   },
   ENV => {
      ctech_exp_ROOT => "&get_tool_path()",
   },
}; 
$ToolConfig_tools{ctech}{PATH} = "&get_tool_path(ipconfig/CTECH)";
$ToolConfig_tools{ctech}{VERSION} = "&get_tool_version(ipconfig/CTECH)";

$ToolConfig_ips{chassis_pg_vc} = {
   PATH => "$ENV{IP_RELEASES}/ChassisPowerGatingVIP/&get_tool_version()",
   VERSION => "ChassisPowerGatingVIP_2013WW30",
   OTHER   => {
      SEARCH_PATHS   => ["&get_tool_path()",],
   },
};

$ToolConfig_ips{vcc_modeling} = {
   PATH => "$ENV{IP_RELEASES}/VccModeling/&get_tool_version()",
   VERSION => "VccModeling_v130227",
   OTHER   => {
      SEARCH_PATHS   => ["&get_tool_path()",],
   },
};

$ToolConfig_ips{iosf_primary_bfm} = {
   PATH => "$ENV{IP_RELEASES}/iosf_primary_bfm/&get_tool_version()",
   VERSION => "iosf_primary_bfm",
   OTHER   => {
      SEARCH_PATHS   => ["&get_tool_path()",
                         "&get_tool_path()/ph4",
      ],
    },
};

$ToolConfig_ips{iosf_sideband_vc} = {
   PATH => "$ENV{IP_RELEASES}/iosf_sideband_vc/&get_tool_version()",
   VERSION => "iosf_sideband_vc_template",
   OTHER   => {
      SEARCH_PATHS   => ["&get_tool_path()",],
   },
};

$ToolConfig_ips{ip_ccu_vc} = {
   PATH => "$ENV{IP_RELEASES}/ip-ccu-vc/&get_tool_version()",
   VERSION => "ip-ccu-vc-2013WW21r130521",
   OTHER   => {
      SEARCH_PATHS   => [ "&get_tool_path()/ccu_vc_WW21", ],
   },
};

$ToolConfig_ips{chassis_reset} = {
   PATH => "$ENV{IP_RELEASES}/chassis_reset_pkg/&get_tool_version()",
   VERSION => "chassis_reset_pkg_2013WW30",
   OTHER   => {
      SEARCH_PATHS   => ["&get_tool_path()",],
   },
};

$ToolConfig_tools{INTC_LIB_SCOREBOARD} = {
   PATH => "$ENV{RTL_CAD_ROOT}/intel/intc_lib_scoreboard/&get_tool_version()",
   VERSION => "v20141128",
   OTHER   => {
      SEARCH_PATHS   => ["&get_tool_path()",],
   },
};


$ToolConfig_ips{SVA_LIB} = {
    PATH    => "/p/hdk/rtl/cad/x86-64_linux30/dt/sva_lib/&get_tool_version()",
        VERSION => "15.4p5_shOpt64",
    
},

$ToolConfig_tools{buildman}{ENV}{SVA_LIB}  = "&get_tool_path(ipconfig/SVA_LIB)";


$ToolConfig_ips{sva_lib_ip} = {
   PATH => "/nfs/site/disks/hdk.cad.1/linux_2.6.16_x86-64/sva_lib/&get_tool_version()/SVA_LIB",
   VERSION => "6.0p1",
   OTHER   => {
      SEARCH_PATHS   => ["&get_tool_path()",],
   },
};

$ToolConfig_ips{shdv} = {
   PATH => "$ENV{IP_MODELS}/shdv/&get_tool_version()",
   VERSION => "shdv-dev-x0-18ww46b",
   OTHER   => {
      SEARCH_PATHS   => ["&get_tool_path()",],
      IMPORT         => ["cfg/shdv_IPToolData.pm",],
   },
};

$ToolConfig_ips{fdo_tools} = {
   PATH => "$ENV{RTL_PROJ_TOOLS}/fdo_tools/nhdk/&get_tool_version()",
   VERSION => "18.00.07",
   OTHER   => {
      SEARCH_PATHS   => ["&get_tool_path()",],
      IMPORT         => ["cfg/shdv_IPToolData.pm",],
   },
};

$ToolConfig_ips{VTE_TR_UVM} = {
    VERSION => "vte-tr-uvm-18ww38a1",
    PATH    => "$ENV{IP_RELEASES}/vte-tr-uvm/&get_tool_version()",
    OTHER   => {
         SEARCH_PATHS   => "&get_tool_path()",
    },
    ENV     => {
         VTE_TR_UVM_ROOT => "&get_tool_path()",
    },
};


# for Cadence PCIe BFMs
$ToolConfig_ips{cdn_vip_root} = { VERSION => 'vipcat_11.30.057-08_Aug_2018_10_14_18',
                                  PATH    => "$ENV{RTL_CAD_ROOT}/cadence/vipcat/&get_tool_version()",
                                };
$ToolConfig_ips{vipcat} = { PATH => "&get_tool_path(cdn_vip_root)",
                              OTHER => {
                                 CDS_ARCH => "lnx86",
                                 VIPCAT_LIBS => "&get_tool_path(cdn_vip_root)/tools.lnx86/lib/64bit",
                              },
                            };
$ToolConfig_ips{denali} = { VERSION => 'vipcat_11.30.057-08_Aug_2018_10_14_18',
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
