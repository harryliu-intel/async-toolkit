#!/bin/tcsh -f

set my_tcl = $argv

set psetupbase = `ToolConfig.pl get_tool_path psetup`
set collage_ver      = `ToolConfig.pl get_tool_version collage`
set COLLAGE_INTF_DEF = `ToolConfig.pl get_tool_env_var collage COLLAGE_INTF_DEF`

setenv SNPSLMD_LICENSE_FILE 26586@synopsys10p.elic.intel.com:26586@synopsys17p.elic.intel.com:26586@synopsys50p.elic.intel.com:26586@synopsys33p.elic.intel.com 

setenv COLLAGE_WORK $cwd/output
mkdir  $COLLAGE_WORK
setenv COLLAGE_IP_KITS $cwd/ip_kits


source $psetupbase/bin/setupTool collage   $collage_ver

collage_cb_shell -timeout_count 10 -f $my_tcl 

# cleanup 
rm -rf log output rt_shell_command.log
