#!/bin/sh 

CLOCKGENSIM=${M3UTILS}/spice/clock_gen4_latch/AMD64_LINUX/clockgen
DATE=`date -Is`
RUNDIR=`pwd`/nb.run-${DATE}
SRCDIR=${M3UTILS}/spice/clock_gen4_latch/src

nb_queue=${NBPOOL}
nb_qslot=${NBQSLOT}
step=1

sweeps="40"
temps="-40 0 25 75 85 100 125"
procs="tttt rcff rcss rxsf rxfs"
volts="0.225 0.250 0.275 0.300 0.325 0.350 0.375 0.450"
#speeds="16"
speeds="0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15"
#rises="30e-12 50e-12 70e-12 90e-12"
rises="30e-12 75e-12"
#cells="latch latch_therm latch_onehot"
cells="latch_onehot"
cycles="500"

# for testing:

testing=0

if [ "${testing}" == "1" ]; then
    temps="85"
    volts="0.30"
    sweeps="4"
    procs="tttt"
    speeds="0 1 5 15"
    rises="30e-12"
fi


######################################################################

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

######################################################################
#
# make all the tasks
#

tasknum=0

for cycl in ${cycles}; do
for cell in ${cells};  do
for rise in ${rises};  do
for sped in ${speeds}; do
for proc in ${procs};  do
for swep in ${sweeps}; do
for volt in ${volts};  do
for temp in ${temps};  do
            
    runfile=${RUNDIR}/${tasknum}.sh
    
    runsubdir=${RUNDIR}/${tasknum}.run
    
    mkdir ${runsubdir}
    
    ln -s ${SRCDIR}/CLOCK_GEN4*.hspice ${runsubdir}
    
    echo "#!/bin/sh -x"        >  ${runfile}
    echo "hostname"            >> ${runfile}
    echo "pwd"                 >> ${runfile}
    echo "cd ${runsubdir}"     >> ${runfile}
    
    
    basecmd="${CLOCKGENSIM} -vdd ${volt} -temp ${temp} -process ${proc} -rise ${rise} -cycle ${cycl} -cell ${cell}"
    
    echo >> ${runfile}
    
    realcmd="${basecmd} -speed ${sped} -sweeps ${swep}"
    
    echo "${realcmd} -p pre"   >> ${runfile}
    echo "${realcmd} -p sim"   >> ${runfile}
    echo "${realcmd} -p conv"  >> ${runfile}
    echo "${realcmd} -p clean" >> ${runfile}
    echo "${realcmd} -p post"  >> ${runfile}
    chmod +x ${runfile}
    
    tasknum=`expr $tasknum + 1`
            
done
done
done
done
done
done
done
done

echo "${tasknum} jobs"

launchnum=0

echo "tasknum ${tasknum} launchnum ${launchnum}"

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

