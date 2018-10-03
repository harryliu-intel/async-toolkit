#!/bin/tcsh -f

set my_tcl = $argv

# taken out of the MST environment
set setupTool_ver    = 6.0
set coretools_ver    = L-2016.09-SP1 
set collage_ver      = 3.18
set collage_intf_ver = 3.5

setenv SNPSLMD_LICENSE_FILE 26586@synopsys10p.elic.intel.com:26586@synopsys17p.elic.intel.com:26586@synopsys50p.elic.intel.com:26586@synopsys33p.elic.intel.com 

setenv COLLAGE_WORK $cwd/output
mkdir  $COLLAGE_WORK
setenv COLLAGE_IP_KITS $cwd/ip_kits


source /p/hdk/rtl/proj_tools/psetup/master/v15ww09b/$setupTool_ver/bin/setupTool collage   $collage_ver

setenv COLLAGE_INTF_DEF /p/hdk/rtl/cad/x86-64_linux30/intel/collage_intf_def/$collage_intf_ver

collage_cb_shell -timeout_count 10 -f $my_tcl 

# cleanup 
rm -rf log output rt_shell_command.log
