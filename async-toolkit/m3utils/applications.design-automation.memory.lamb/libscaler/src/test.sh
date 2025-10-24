#!/bin/sh

/nfs/site/disks/tfc_fe_zsc7_01/mnystroe/applications.design-automation.memory.lamb/gtr/../m3utils/liberty/AMD64_LINUX/editliberty \
-scm /nfs/site/disks/tfc_fe_zsc7_01/mnystroe/applications.design-automation.memory.lamb/gtr/../m3utils/liberty/src/types.scm \
-scm /nfs/site/disks/tfc_fe_zsc7_01/mnystroe/applications.design-automation.memory.lamb/gtr/../m3utils/liberty/src/liberty-utils.scm \
-scm /nfs/site/disks/tfc_fe_zsc7_01/mnystroe/applications.design-automation.memory.lamb/gtr/../libscaler/scm/liberty-scaler.scm \
-w 10 -d 4 -tech n3b -template ../templates/cdp_lamb_1w1afr_template.lib -name cdp_lamb_n3bhd_1r1w1c_4d_10b -path cdp_lamb_n3bhd_1r1w1c_4d_10b.lib -temp 25 -v 0.675 -sicorner -3 -rcorner -1.5 -ccorner -1.5 -pvtname 0p675_ssgnp_rcworst
