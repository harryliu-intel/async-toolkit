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
        LIBS => [
            "&get_tool_path()/cfg/ace/lib",
            "&get_tool_var(ipconfig/mby_tlm,LIBS)",
        ],
        ##Need to update once hif and etm will provide reusable <IP>_IPToolData.pm files.
        IMPORT => [
        ],

        SEARCH_PATHS   => [
            "&get_tool_path()",
            "&get_tool_path()/cfg",
            "&get_tool_path()/cfg/ace",
        ],
        lintra_waiver_dirs  => [
        ],
         SUB_SCOPES          => [   "mby_tlm",
				    #"&get_tool_var(ipconfig/mby_tlm, SUB_SCOPES)",
                                ],
        TEST_PATTERNS => [
           "verif/mby/jasper/tests/"
        ],
    },

     ENV => {
            },

};



##Need to further check how to provide subBlock integration.
$ToolConfig_ips{mby_tlm} = {
    PATH    => "$MODEL_ROOT/subBlock/tlm",
    OTHER => {
        LIBS => ["&get_tool_path()/cfg/ace/lib"],
        SEARCH_PATHS => [
            "&get_tool_path()",
            "&get_tool_path(ipconfig/iosf_primary_bfm)"."/ph4",
        ],
    },
};




1;
