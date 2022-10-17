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

step=10

corners="ss tt ff"

temps="-40 -20 0 25 50 75 100 125 150"

volts="0.19 0.21 0.23 0.25 0.27 0.29 0.31 0.33 0.35 0.40 0.45 0.50 0.55 0.60 0.65 0.70 0.75 0.80 0.85 0.90 0.95 1.0"

#techs="n5 1276p4 n3 n3e"
techs="n5 1276p4 n3e"

modes="dyn leak"

# short test

techs="n3e"
corners="tt"
volts="0.29"

######################################################################

taskfile=full-${DATE}.task

mkdir ${RUNDIR}

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

for corn in ${corners}; do
for mode in ${modes}; do
for temp in ${temps}; do
for volt in ${volts}; do
for tech in ${techs}; do

    if [ "${tech}" == "n5" ]; then
        trantypes="elvt ulvt ulvtll lvt lvtll svt svtll"
    elif [ "${tech}" == "n3e" ]; then
        trantypes="elvt ulvt ulvtll lvt lvtll svt"
    else
        trantypes="ulvt lvt svt"
    fi

    for tran in ${trantypes}; do
        echo "#!/bin/sh -x" > ${RUNDIR}/${tasknum}.sh
        echo "${PROG} \
              -tech ${tech} -corn ${corn} -tran ${tran} -topo intc \
              -mode ${mode} -all -simu xa -T ${TEMPLATE} \
              -volt ${volt} -temp ${temp} \
              -d ${RUNDIR}/${tasknum}.run -C" >> ${RUNDIR}/${tasknum}.sh
        chmod +x ${RUNDIR}/${tasknum}.sh
        
        tasknum=`expr $tasknum + 1`
    done
    
done
done
done
done
done

launchnum=0

while [ "${launchnum}" -lt "${tasknum}" ]; do
    echo "nbjob run --log-file ${RUNDIR}/${launchnum}.log ${SRCDIR}/launcher.sh ${RUNDIR} ${launchnum} ${step}"    >> $taskfile
    launchnum=`expr $launchnum + $step`
done



cat >> ${taskfile} <<EOF
       }
}
EOF
