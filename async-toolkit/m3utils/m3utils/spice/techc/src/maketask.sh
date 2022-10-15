#!/bin/sh

ROOTDIR=/nfs/site/disks/zsc3_fon_fe_0001/mnystroe/m3utils/spice/techc
SRCDIR=${ROOTDIR}/src
BINDIR=${ROOTDIR}/AMD64_LINUX
PROG=${BINDIR}/techc
TEMPLATE=${SRCDIR}/ckt.sp
DATE=`date -Is`
RUNDIR=${SRCDIR}/nb.run-${DATE}



mkdir ${RUNDIR}

taskfile=full-${DATE}.task

cat > ${taskfile} <<EOF
    
JobsTask {
  WorkArea ${SRCDIR}/nb.run

  Queue zsc3_express {
    Qslot /XCC/LBM/SD
  } 
  Jobs {

EOF

# make all the tasks

tasknum=0

for tech in "n5" "1276p4"; do
    if [ "${tech}" == "n5" ]; then
        trantypes="elvt ulvt ulvtll lvt lvtll svt svtll"
    else
        trantypes="ulvt lvt svt"
    fi

    for tran in ${trantypes}; do
        echo "nbjob run --log-file ${RUNDIR}/${tasknum}.log ${PROG} -tech ${tech} -corn tt -tran ${tran} -topo tsmc -mode dyn -all -simu xa -T ${TEMPLATE} -volt 0.35 -temp 25 -d ${RUNDIR}/${tasknum}.run -C" >> $taskfile

        tasknum=`expr $tasknum + 1`
    done
done





cat >> ${taskfile} <<EOF
       }
}
EOF
