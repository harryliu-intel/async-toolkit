#!/bin/sh -x
NB="nbjob run --target sc_normal --qslot /bfn/fe --class \"SLES12&&1G\""
GLITCH=/nfs/sc/disks/bfn_pd_cb_02/mnystroe/m3utils/glitch/AMD64_LINUX/glitch
LIMIT="-limit 64"

for file in *.glitch; do
    ${NB} ${GLITCH} ${LIMIT} -f ${file}
done
