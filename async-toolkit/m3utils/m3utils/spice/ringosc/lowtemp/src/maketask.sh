#!/bin/sh 

LAUNCHER=${M3UTILS}/spice/adder/src/launcher.sh
DATE=`date -Is`
RUNDIR=`pwd`/nb.run-${DATE}
PARENT=${M3UTILS}/spice/ringosc/lowtemp
SRCDIR=${PARENT}/src
BINDIR=AMD64_LINUX
LOWTEMP=${PARENT}/${BINDIR}/lowtemp

nb_queue=${NBPOOL}
nb_qslot=${NBQSLOT}
step=1

temps="0 25 50 60 75 85 100 125"
sweeps="100"
#sweeps=10

stages="10 20 40 80"
volts="0.16 0.18 0.20 0.22 0.24 0.26 0.28 0.30 0.32 0.34 0.36 0.38 0.40 0.45"

#trantypes="lvt ulvt" # svt doesnt work yet
trantypes="ulvt" # svt doesnt work yet
#libs="i0s i0m"
libs="i0s"

delns="0"
delps="0"
steps="1e-9"  # really just the reset time

cscales="1.0"

modleaves="true"

delns="0.000 0.010 0.020 0.040 0.060 0.085 0.100 0.120"
delps="0.000 0.010 0.020 0.040 0.060 0.085 0.100 0.120"
temps="-80 -40 -20 0 25 50 75 85 105"

# for testing:

testing=1

if [ "${testing}" == "1" ]; then
    stages="10"
    temps="0"
    volts="0.34"
    trantypes="ulvt"
    sweeps="4"
    cscales="2"
    delns="0.20"
    delps="0.20"
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

for stpp in ${steps}; do
for stgs in ${stages}; do
for modl in ${modleaves}; do
for deln in ${delns}; do
for delp in ${delps}; do
for cscl in ${cscales}; do
for sweep in ${sweeps}; do
for lib in ${libs}; do
        for volt in ${volts}; do
        for tran in ${trantypes}; do
        for temp in ${temps}; do
            
            runfile=${RUNDIR}/${tasknum}.sh

            runsubdir=${RUNDIR}/${tasknum}.run

            mkdir ${runsubdir}

            echo "#!/bin/sh -x"        >  ${runfile}
            echo "hostname"            >> ${runfile}
            echo "pwd"                 >> ${runfile}
            echo "cd ${runsubdir}"     >> ${runfile}

	    cmdargs="${LOWTEMP} -vdd ${volt} -temp ${temp} -lib ${lib} -thresh ${tran} -sweeps ${sweep} -cscale ${cscl} -delp ${delp} -deln ${deln} -modleaves ${modl} -stages ${stgs} -step ${stpp}"

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

