#!/bin/sh

ROOTDIR=/nfs/site/disks/zsc3_fon_fe_0001/mnystroe/m3utils/spice/techc
SRCDIR=${ROOTDIR}/src
BINDIR=${ROOTDIR}/AMD64_LINUX
PROG=${BINDIR}/techc
TEMPLATE=${SRCDIR}/ckt.sp
DATE=`date -Is`
RUNDIR=${SRCDIR}/nb.run-${DATE}

nb_queue=zsc3_express
nb_queue=zsc3_normal
nb_qslot=/XCC/LBM/SD
nb_qslot=/XCC/LBM/RTL


mkdir ${RUNDIR}

taskfile=full-${DATE}.task

cat > ${taskfile} <<EOF
    
JobsTask {
  WorkArea ${RUNDIR}

  Queue ${nb_queue} {
    Qslot ${nb_qslot}
  } 
  Jobs {

EOF

# make all the tasks

tasknum=0

for corn in "ss" "tt" "ff"; do
for mode in "dyn" "leak"; do
for temp in -40 -20 0 25 50 75 100 125 150; do
for volt in 0.19 0.21 0.23 0.25 0.27 0.29 0.31 0.33 0.35 0.40 0.45 0.50 0.55 0.60 0.65 0.70 0.75 0.80 0.85 0.90 0.95 1.00; do
for tech in "n5" "1276p4" "n3" "n3e"; do

    if [ "${tech}" == "n5" ]; then
        trantypes="elvt ulvt ulvtll lvt lvtll svt svtll"
    elif [ "${tech}" == "n3e" ]; then
        trantypes="elvt ulvt ulvtll lvt lvtll svt"
    else
        trantypes="ulvt lvt svt"
    fi

    for tran in ${trantypes}; do
        echo "nbjob run --log-file ${RUNDIR}/${tasknum}.log ${PROG} \
              -tech ${tech} -corn ${corn} -tran ${tran} -topo intc \
              -mode ${mode} -all -simu xa -T ${TEMPLATE} \
              -volt ${volt} -temp ${temp} \
              -d ${RUNDIR}/${tasknum}.run -C" >> $taskfile

        tasknum=`expr $tasknum + 1`
    done
    
done
done
done
done
done


cat >> ${taskfile} <<EOF
       }
}
EOF
