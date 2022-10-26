#!/bin/sh -x

mkdir timing

if [ "a" == "b" ]; then
    /nfs/site/disks/tfc_fe_zsc7_01/mnystroe/applications.design-automation.memory.lamb/gtr/../m3utils/liberty/AMD64_LINUX/editliberty \
        -scm /nfs/site/disks/tfc_fe_zsc7_01/mnystroe/applications.design-automation.memory.lamb/gtr/../m3utils/liberty/src/types.scm \
        -scm /nfs/site/disks/tfc_fe_zsc7_01/mnystroe/applications.design-automation.memory.lamb/gtr/../m3utils/liberty/src/liberty-utils.scm \
        -scm /nfs/site/disks/tfc_fe_zsc7_01/mnystroe/applications.design-automation.memory.lamb/gtr/../libscaler/scm/liberty-scaler.scm \
        -scm /nfs/site/disks/tfc_fe_zsc7_01/mnystroe/applications.design-automation.memory.lamb/gtr/../libscaler/scm/do-scale.scm \
        -w 10 -d 4 -tech n3e \
        -template /nfs/site/disks/tfc_fe_zsc7_01/mnystroe/applications.design-automation.memory.lamb/gtr/../libscaler/templates/cdp_lamb_1w1sr_template.lib \
        -cellname cdp_lamb_n3ehd_1r1w1c_4d_10b \
        -name cdp_lamb_n3ehd_1r1w1c_4d_10b_ssgnp_0p675_0_0_cworst_CCworst_T \
        -path timing/cdp_lamb_n3ehd_1r1w1c_4d_10b_ssgnp_0p675_0_0_cworst_CCworst_T.lib \
        -sitemp 0 -mttemp 0 -v 0.675 \
        -sicorner -3 -rcorner 0 -ccorner -1.5 \
        -pvtname ssgnp_0p675_0_0_cworst_CCworst_T \
        -pow cell_leakage_power 63.2583628795 \
        -pinpow2 clk '!ren&!wen' 0.000179208287327 \
        -pinpow2 clk 'ren&!wen' 0.00537982976905 \
        -pinpow2 clk '!ren&wen' 0.010385439932 \
        -pinpow2 clk 'ren&wen' 0.016144073313
fi


/nfs/site/disks/tfc_fe_zsc7_01/mnystroe/applications.design-automation.memory.lamb/gtr/../m3utils/liberty/AMD64_LINUX/editliberty \
    -scm /nfs/site/disks/tfc_fe_zsc7_01/mnystroe/applications.design-automation.memory.lamb/gtr/../m3utils/liberty/src/types.scm \
    -scm /nfs/site/disks/tfc_fe_zsc7_01/mnystroe/applications.design-automation.memory.lamb/gtr/../m3utils/liberty/src/liberty-utils.scm \
    -scm /nfs/site/disks/tfc_fe_zsc7_01/mnystroe/applications.design-automation.memory.lamb/gtr/../libscaler/scm/liberty-scaler.scm \
    -scm /nfs/site/disks/tfc_fe_zsc7_01/mnystroe/applications.design-automation.memory.lamb/gtr/../libscaler/scm/do-scale.scm \
    -w 10 -d 4 -tech n3e \
    -template /nfs/site/disks/tfc_fe_zsc7_01/mnystroe/applications.design-automation.memory.lamb/gtr/../libscaler/templates/cdp_lamb_1w1ar_template.lib \
    -cellname cdp_lamb_n3ehd_1r1w1c_4d_10b \
    -name cdp_lamb_n3ehd_1r1w1c_4d_10b_ssgnp_0p675_0_0_cworst_CCworst_T \
    -path timing/cdp_lamb_n3ehd_1r1w1c_4d_10b_ssgnp_0p675_0_0_cworst_CCworst_T.lib \
    -sitemp 0 -mttemp 0 -v 0.675 \
    -sicorner -3 -rcorner 0 -ccorner -1.5 \
    -pvtname ssgnp_0p675_0_0_cworst_CCworst_T \
    -pow cell_leakage_power 63.2583628795 \
    -pinpow2 clk '!wen' 0.000179208287327 \
    -pinpow2 clk 'wen' 0.010385439932  \
    -comb_read

/nfs/site/disks/tfc_fe_zsc7_01/mnystroe/applications.design-automation.memory.lamb/gtr/../m3utils/liberty/AMD64_LINUX/editliberty \
    -scm /nfs/site/disks/tfc_fe_zsc7_01/mnystroe/applications.design-automation.memory.lamb/gtr/../m3utils/liberty/src/types.scm \
    -scm /nfs/site/disks/tfc_fe_zsc7_01/mnystroe/applications.design-automation.memory.lamb/gtr/../m3utils/liberty/src/liberty-utils.scm \
    -scm /nfs/site/disks/tfc_fe_zsc7_01/mnystroe/applications.design-automation.memory.lamb/gtr/../libscaler/scm/liberty-scaler.scm \
    -scm /nfs/site/disks/tfc_fe_zsc7_01/mnystroe/applications.design-automation.memory.lamb/gtr/../libscaler/scm/do-scale.scm \
    -w 10 -d 128 -tech n3e \
    -template /nfs/site/disks/tfc_fe_zsc7_01/mnystroe/applications.design-automation.memory.lamb/gtr/../libscaler/templates/cdp_lamb_1w1ar_template.lib \
    -cellname cdp_lamb_n3ehd_1r1w1c_128d_10b \
    -name cdp_lamb_n3ehd_1r1w1c_128d_10b_ssgnp_0p675_0_0_cworst_CCworst_T \
    -path timing/cdp_lamb_n3ehd_1r1w1c_128d_10b_ssgnp_0p675_0_0_cworst_CCworst_T.lib \
    -sitemp 0 -mttemp 0 -v 0.675 \
    -sicorner -3 -rcorner 0 -ccorner -1.5 \
    -pvtname ssgnp_0p675_0_0_cworst_CCworst_T \
    -pow cell_leakage_power 63.2583628795 \
    -pinpow2 clk '!wen' 0.000179208287327 \
    -pinpow2 clk 'wen' 0.010385439932  \
    -comb_read





