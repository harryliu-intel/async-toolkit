#!/bin/sh
GLITCH=/nfs/sc/disks/bfn_pd_cb_02/mnystroe/m3utils/glitch/AMD64_LINUX/glitch
LIMIT="-limit 64"

cd rundir

cat << EOF 
JobsTask {
  WorkArea /nfs/sc/disks/bfn_pd_cb_02/mnystroe/m3utils/glitch/geneqs/src/nb.out
  Queue ${EC_SITE}_normal {
    Qslot /bfn/fe
  } 
  Jobs {

EOF

for dir in *; do

    cd ${dir}
    where=`pwd`

    for file in *.glitch; do
        echo "    nbjob run --class \"SLES12&&1G\" ${GLITCH} ${LIMIT} -f ${where}/${file}"
    done

    cd ..
    
done

cat << EOF
  }
}

EOF
