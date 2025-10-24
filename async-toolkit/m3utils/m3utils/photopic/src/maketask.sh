#!/bin/sh
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0


DATE=`date -Is`
RUNDIR=`pwd`/nb.run-${DATE}
ROOTDIR=/nfs/site/disks/zsc3_fon_fe_0001/mnystroe/m3utils/photopic
SRCDIR=${ROOTDIR}/src
BINDIR=${ROOTDIR}/AMD64_LINUX
PROG=${BINDIR}/photopic

nb_queue=zsc3_normal
nb_qslot=/XCC/LBM/RTL

ccts="2100 2200 2400 2700 3000 3500 4000 4400 5000 5500 5800 6000 6500 7000 10000"
cris="60 70 80 82 85 90 92 93 94 95 96 97 98 99"
r9s="-100 0 40 50 60 70 80 90 95 99"

taskfile=full-${DATE}.task

mkdir ${RUNDIR}

cat > ${taskfile} <<EOF
    
JobsTask {
  WorkArea ${RUNDIR}
  SubmissionArgs --class SLES12SP5

  Queue ${nb_queue} {
    Qslot ${nb_qslot}
  } 
  Jobs {

EOF

step=1

# make all the tasks

tasknum=0

for r9 in ${r9s}; do
for cct in ${ccts}; do
    for cri in ${cris}; do
        echo "#!/bin/sh -x" > ${RUNDIR}/${tasknum}.sh
        echo "hostname" >> ${RUNDIR}/${tasknum}.sh
        echo "pwd" >> ${RUNDIR}/${tasknum}.sh

        torun="${PROG} -run ${cct} ${cri} ${r9} -scm -scmfile ${SRCDIR}/photopic.scm"

        echo "${torun}"               >> ${RUNDIR}/${tasknum}.sh

        chmod +x ${RUNDIR}/${tasknum}.sh
        
        tasknum=`expr $tasknum + 1`
    done
done
done

echo "${tasknum} jobs"

launchnum=0

while [ "${launchnum}" -lt "${tasknum}" ]; do
    echo "nbjob run --log-file ${RUNDIR}/${launchnum}.log ${SRCDIR}/launcher.sh ${RUNDIR} ${launchnum} ${step}"    >> $taskfile
    launchnum=`expr $launchnum + $step`
done

cat >> ${taskfile} <<EOF
       }
}
EOF

rm -f nb.latest
ln -sf ${RUNDIR} nb.latest
ln -sf ${taskfile} latest.task
