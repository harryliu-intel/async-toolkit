#!/bin/sh -x
GLITCH=/nfs/sc/disks/bfn_pd_cb_02/mnystroe/m3utils/glitch/AMD64_LINUX/glitch
LIMIT="-limit 64"

cd rundir

for dir in *; do

    cd ${dir}

    for file in *.glitch; do
        echo ${dir}/${file}
        time ${GLITCH} ${LIMIT} -f ${file}
    done

    cd ..
    
done
