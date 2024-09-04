#!/bin/sh 

LAUNCHER=${M3UTILS}/spice/adder/src/launcher.sh
DATE=`date -Is`
RUNDIR=`pwd`/nb.run-${DATE}
PARENT=${M3UTILS}/spice/ringosc/lowtemp
SRCDIR=${PARENT}/src
BINDIR=AMD64_LINUX
GENOPT=${M3UTILS}/spice/genopt/${BINDIR}/chopstix
DEFS=${SRCDIR}/defs.scm
OPTSCM=${SRCDIR}/lowtempopt.scm

nb_queue=${NBPOOL}
nb_qslot=${NBQSLOT}
step=1

sweeps="20"

stages="2 5 11 22 44 88"

#trantypes="lvt ulvt" # svt doesnt work yet
trantypes="ulvt lvt" # svt doesnt work yet
#libs="i0s i0m"
libs="i0s"

steps="1e-9"  # really just the reset time

cscales="1.0"

modleaves="true"

# efficiency only defined to 50C (since that's the assumed hot side)
temps="-80 -70 -60 -50 -40 -30 -20 -10 0 10 25 50" 


# for testing:

testing=0

if [ "${testing}" == "1" ]; then
    stages="10"
    temps="0"
    trantypes="lvt"
    sweeps="4"
    cscales="1"
fi


######################################################################

taskfile=full-${DATE}.task

mkdir ${RUNDIR}

cat > ${taskfile} <<EOF
    
JobsTask {
  WorkArea ${RUNDIR}
  SubmissionArgs --class 'SLES12SP5&&32G&&4C'

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

for stpp  in ${steps};     do
for stgs  in ${stages};    do
for cscl  in ${cscales};   do
for sweep in ${sweeps};    do
for lib   in ${libs};      do
for tran  in ${trantypes}; do
for temp  in ${temps};     do
            
            runfile=${RUNDIR}/${tasknum}.sh

            runsubdir=${RUNDIR}/${tasknum}.run

            mkdir ${runsubdir}

            echo "#!/bin/sh -x"        >  ${runfile}
            echo "hostname"            >> ${runfile}
            echo "pwd"                 >> ${runfile}
            echo "cd ${runsubdir}"     >> ${runfile}

	    cmdargs="${GENOPT} -S ${DEFS} -setparam temp ${temp} -setparam thresh ${tran} -setparam sweeps ${sweep} -setparam cscale ${cscl} -setparam stages ${stgs} ${OPTSCM}"

            echo "${cmdargs}"   >> ${runfile}
            chmod +x ${runfile}
        
            tasknum=`expr $tasknum + 1`
            
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
    echo "nbjob run --log-file ${RUNDIR}/${launchnum}.log ${LAUNCHER} ${RUNDIR} ${launchnum} ${step}"    >> $taskfile
    launchnum=`expr $launchnum + $step`
done


cat >> ${taskfile} <<EOF
       }
}
EOF

rm -f nb.latest
ln -sf ${RUNDIR} nb.latest
ln -sf ${taskfile} latest.task

