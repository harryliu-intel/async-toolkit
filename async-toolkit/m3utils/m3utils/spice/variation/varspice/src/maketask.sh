#!/bin/sh

#
# Collect data on randomly chosen ring oscillator parameters
# Intel P1278.3 XOR ring oscillators
#

ROOTDIR=${M3UTILS}/spice/variation/varspice
SRCDIR=${ROOTDIR}/src
BINDIR=${ROOTDIR}/AMD64_LINUX
PROG=${BINDIR}/vary
TEMPLATE=${M3UTILS}/spice/variation/varosc/src/circuit.sp

#
# transistor type and strength...
#
thresh=lvt
z=1

#
# N samples
#
samples=100000

#
# Netbatch settings (env. vars must be set in calling env)
#
nb_qslot=${NBQSLOT}
nb_pool=${NBPOOL}

#
# file naming conventions
#
DATE=`date -Is`
RUNDIR=`pwd`/nb.run-${DATE}
taskfile=full-${DATE}.task

mkdir ${RUNDIR}

cat > ${taskfile} <<EOF
    
JobsTask {
  WorkArea ${RUNDIR}
  SubmissionArgs --class SLES12SP5

  Queue ${nb_pool} {
    Qslot ${nb_qslot}
  } 
  Jobs {

EOF

tasknum=0

echo "${samples} jobs"

while [ "${tasknum}" -lt "${samples}" ]; do
    echo "nbjob run --log-file ${RUNDIR}/${tasknum}.log ${PROG} -single -thresh ${thresh} -z ${z} -T ${TEMPLATE} -r ${RUNDIR}/${tasknum} -N"    >> $taskfile

    tasknum=`expr $tasknum + 1`
done

cat >> ${taskfile} <<EOF
       }
}
EOF

rm -f nb.latest
ln -sf ${RUNDIR} nb.latest
ln -sf ${taskfile} latest.task
