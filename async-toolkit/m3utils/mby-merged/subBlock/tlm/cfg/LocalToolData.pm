# DO NOT REMOVE!!!  Learn to code warning-free perl instead.
#
use warnings FATAL => 'all';

package ToolData;
$general_vars{aceroot_dpath} = "$MODEL_ROOT/target/&get_facet(dut)/aceroot";

$ToolConfig_tools{ace}{VERSION} = "2.02.02";
$ToolConfig_tools{ace_utils}{OTHER}{printcmd} = "0";
$ToolConfig_tools{ace_utils}{OTHER}{cleanenvrc} = "0";

$ToolConfig_tools{vcsmx}{VERSION} = "H-2013.06-SP1-12"; # "I-2014.03-SP1-5";
#$ToolConfig_tools{vcsmx}{VERSION} = " "I-2014.03-SP1-5";
$ToolConfig_tools{lintra}{VERSION} =  "14.4p14_shOpt64";
#$ToolConfig_tools{'lintra'}{VERSION} =  "15.3p20_shOpt64";
$ToolConfig_tools{'lintra-rules'}{VERSION} =  "1.02.01.14_4";
#$ToolConfig_tools{'lira'}{VERSION} =  "14.4p14_64"; #"15.3p8_64";
$ToolConfig_tools{'lira'}{VERSION} =  "14.4p14_shOpt64"; #"15.3p8_64";
$ToolConfig_tools{'uvm'}{VERSION} = "1.2";
$ToolConfig_tools{'ovm'}{VERSION} = "2.1.2_2a";
$ToolConfig_tools{'saola'}{VERSION} = "16.1.04";

# --- Added options to get GK  to work correctly .. requested by Guy/Tom 
$ToolConfig_tools{'rtltools'}{SUB_TOOLS}{flowbee}{OTHER}{enable_gkmnm} = "1";
$ToolConfig_tools{'rtltools'}{OTHER}{dont_stop_feeder} = "1";


$ToolConfig_tools{'rtltools'}{OTHER}{resource_def} = "$ENV{MODEL_ROOT}/cfg/resource.xml";
$ToolConfig_tools{'febe3'}{OTHER}{resource_def} = "$ENV{MODEL_ROOT}/cfg/resource.xml";

$ToolConfig_tools{'febe3'}{'SUB_TOOLS'}{'lintra'}{'VERSION'} = "15.3p20_shOpt64";
$ToolConfig_tools{'febe3'}{'SUB_TOOLS'}{'lintra'}{'PATH'} = "/p/hdk/rtl/cad/x86-64_linux30/dt/lintra/&get_tool_version()";

$ToolConfig_tools{runtools}{ENV}{TREX_PACKAGES}= "&get_tool_path(casa/casa_utils)/trex/casa_TREX.pm";

                                     
$ToolConfig_tools{'dc_shell'}{'VERSION'} = "I-2013.12-SP5-6";
$ToolConfig_tools{dc_shell} = {
    VERSION    =>  "I-2013.12-SP5-6",
    PATH       => "/p/hdk/cad/designcompiler/&get_tool_version()",
    ENV_APPEND  => {
        'PERLLIB'   => "&get_tool_path()/Trex_Modules",
    },
};
