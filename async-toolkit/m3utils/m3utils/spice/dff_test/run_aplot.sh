#!/bin/sh -x
LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:/nfs/site/disks/tfc_fe_zsc7_01/mnystroe/aplot
export LD_LIBRARY_PATH

/nfs/site/disks/tfc_fe_zsc7_01/mnystroe/aplot/aplot $*
