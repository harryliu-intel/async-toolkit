#!/bin/sh 

RINGOSCSIM=${M3UTILS}/spice/ringosc/AMD64_LINUX/ringosc
DATE=`date -Is`
RUNDIR=`pwd`/nb.run-${DATE}
SRCDIR=${M3UTILS}/spice/ringosc/src

nb_queue=${NBPOOL}
nb_qslot=${NBQSLOT}
step=1

temps="0 25 50 60 75 85 100 125"
sweeps="100"
#sweeps=10

#volts="0.20 0.22 0.24 0.26 0.28 0.30 0.32 0.34 0.36 0.38 0.40 0.45 0.50 0.55 0.60 0.65 0.70 0.80 0.90"
volts="0.18 0.20 0.22 0.24 0.26 0.28 0.30 0.32 0.34 0.36 0.38 0.40 0.45"

# for testing:

speeds="0 1 2 3 4 10 20 30 50 159 160"

testing=1

if [ "${testing}" == "1" ]; then
    temps="85"
    volts="0.30"
    trantypes="ulvt"
    sweeps="4"
    speeds="0"
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

for speed in ${speeds}; do
for sweep in ${sweeps}; do
        for volt in ${volts}; do
        for temp in ${temps}; do
            
            runfile=${RUNDIR}/${tasknum}.sh

            runsubdir=${RUNDIR}/${tasknum}.run

            mkdir ${runsubdir}

            echo "#!/bin/sh -x"        >  ${runfile}
            echo "hostname"            >> ${runfile}
            echo "pwd"                 >> ${runfile}
            echo "cd ${runsubdir}"     >> ${runfile}

	    cmdargs="${RINGOSCSIM} -vdd ${volt} -temp ${temp} -sweeps ${sweep} -speed ${speed}"

            echo "${cmdargs} -p pre"   >> ${runfile}
            echo "${cmdargs} -p sim"   >> ${runfile}
            echo "${cmdargs} -p conv"  >> ${runfile}
            echo "${cmdargs} -p clean" >> ${runfile}
            echo "${cmdargs} -p post"  >> ${runfile}
            chmod +x ${runfile}
        
            tasknum=`expr $tasknum + 1`
            
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

