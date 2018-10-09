#!/bin/tcsh -f

# only parameter is to pass in pointer to builder tcl script
set my_tcl = $argv

set psetupbase       = `ToolConfig.pl get_tool_path psetup`
set collage_ver      = `ToolConfig.pl get_tool_version collage`
set collage_intf_def = `ToolConfig.pl get_tool_path collage_intf_def`

setenv COLLAGE_WORK $cwd/output
setenv COLLAGE_IP_KITS $cwd/ip_kits
setenv COLLAGE_INTF_DEF $collage_intf_def

mkdir  $COLLAGE_WORK

source $psetupbase/bin/setupTool collage   $collage_ver

collage_cb_shell -timeout_count 10 -f $my_tcl 

# cleanup
rm -rf output rt_shell_command.log

