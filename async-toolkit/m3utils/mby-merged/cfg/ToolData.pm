# DO NOT REMOVE!!!  Learn to code warning-free perl instead.
#
use warnings FATAL => 'all';

package ToolData;

use File::Basename;

use Exporter ();
@ISA    = qw(Exporter);
@EXPORT = qw(
  %ToolConfig_int
  %general_vars
  %ToolConfig_tools
  %onecfg
);

$onecfg{Facet} = {
    values => {
        dut => [qw(
                    mby 
                    fc_lite

        )],
        ace_model_name => [qw( fc fc_lite fc_effm fc_nlp dft_no_dut dft_rtl_dbg dft_rtl_quick dft_gls_dbg dft_gls_quick )],
    },
    defaults => {
        dut => "mby",
        ace_model_name => "fc",
    },
};

######################################################################
#
# general_vars hash stores general tool data which doesn't belong to
# any specific tool. These variables are used internally in the
# flow and are not set in the Unix environment.
#
# Hash entries that are arrays need to be declared as:
#   $general_vars{<entry>} = [ <item1>,<item2> ];
#
######################################################################
our %general_vars = ();

$general_vars{tooldatas} = [
];


######################################################################
#
# ToolConfig_tools hash stores data about tools need by the RTL
# simulation environment.
#
# If a tool version changes, please change the corresponding entry in
# this file.
#
#    **** All tool and subtool names must be lowercase. ****
#
######################################################################
our %ToolConfig_tools = ();
our %ToolConfig_int   = ();

######################################################################
#
# OneConfig
# This must be on cfg/ToolData.pm for the bootstrapping to operate
#   correctly
#
######################################################################
$ToolConfig_tools{onecfg} = {
    VERSION => '1.02.07',
    PATH => "$RTL_PROJ_TOOLS/onecfg/master/&get_tool_version()",
    OTHER => {
        IMPORTS => [
            "&get_tool_path(baseline)/GeneralVars.pm",
            "&get_tool_path(baseline)/Baseline_ToolData.pm",
	    "IPToolData.pm",
            "LocalToolData.pm",
            #"$ENV{MODEL_ROOT}/cfg/RTLToolData.pm",
        ],
    },
};

###############################################################################
# IPPackaging tool
###############################################################################
$ToolConfig_tools{iptooldataextras} = {
        VERSION => "16.37.02",
        PATH    => "$RTL_PROJ_TOOLS/IPToolDataExtras/nhdk/16.37.02",
};




######################################################################
#
# Baseline Tools
# GeneralVars contains common variable settings and general_vars setup
# BaselineToolData contains common tool settings
#
######################################################################
$ToolConfig_tools{baseline} = {
    VERSION => '1.5.2',
    PATH => "$RTL_PROJ_TOOLS/baseline_tools/master/&get_tool_version(baseline)",
};



$ToolConfig::MyVersion = 9;

1;

### EOF ###

