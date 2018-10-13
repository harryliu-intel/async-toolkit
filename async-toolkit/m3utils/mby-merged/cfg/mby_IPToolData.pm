package ToolData;
use strict;
use warnings;

######################################################################
# NOTE: Use these variables instead of $ENV when possible
# Try to use &get_env_var(VARIABLE_NAME) in hash string entries otherwise
# Also, please use variables instead of raw absolute paths.
# Add needed variables in this list from baselin_tools/GeneralVars.pm
######################################################################
use vars
   qw(%ToolConfig_ips
      %ToolConfig_tools
      $MODEL_ROOT
      $IP_MODELS
      $IP_RELEASES
      $RTL_PROJ_TOOLS
   );

use File::Basename;
use lib $ToolConfig_tools{iptooldataextras}{PATH};
use IPToolDataExtras qw(import_files get_version_from_path);

my $dirname = dirname(dirname(__FILE__));
$dirname = `/usr/intel/bin/realpath $dirname`;
chomp($dirname);


$ToolConfig_ips{mby} = {
   PATH    => "$dirname",
   VERSION => &get_version_from_path($dirname),
   OTHER   => {
      LIBS                => ["&get_tool_path()/cfg/ace/lib",
                              "&get_tool_var(ipconfig/eth_port, LIBS)",
      ],
      IMPORT              => [],
      SEARCH_PATHS        => ["&get_tool_path()",
                              "&get_tool_path()/cfg",
                              "&get_tool_path()/cfg/ace",
                              "&get_tool_path()/subBlock/mbyc",
                              "&get_tool_var(ipconfig/eth_port, SEARCH_PATHS)",
                              "&get_tool_path()/target/&get_facet(dut)",
      ],
      lintra_waiver_dirs  => [],
      SUB_SCOPES          => ["eth_port",
                              "&get_tool_var(ipconfig/eth_port, SUB_SCOPES)",
      ],
      TEST_PATTERNS       => ["verif/mby/formal/tests","tools/lint/tests",],
    },
    ENV => {
      SOC_DUT             => "&get_facet(dut)", # used in DutConnect.pl
    },
};
######################################################################
# Executes the import statement above
######################################################################
IPToolDataExtras::import_files("mby", \%ToolConfig_ips);

#Added for Cadence PCIe bfms
$ToolConfig_ips{mby}{ENV}{LD_LIBRARY_PATH} .=  join(':',
                                                    "&get_tool_var(denali, DENALI_LIBS)",
                                                    "&get_tool_var(vipcat, VIPCAT_LIBS)",
                                                  );

my $epl_version = "eth_port-dev-x0-18ww39c";

$ToolConfig_ips{epc} = {
   PATH    => "/nfs/sc/disks/sc_mby_00055/layhockk/mby/work_root/MBY/$epl_version",
   VERSION => "$epl_version",
   OTHER   => {
      SEARCH_PATHS => ["&get_tool_path()",],
      IMPORT       => ["cfg/eth_port_IPToolData.pm",],
   },
};
######################################################################
# Executes the import statement above
######################################################################
IPToolDataExtras::import_files("epc", \%ToolConfig_ips);

1;
