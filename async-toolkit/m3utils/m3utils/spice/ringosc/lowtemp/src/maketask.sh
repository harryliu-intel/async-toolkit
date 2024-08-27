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

sweeps="20"

stages="10 20 40 80 160"
volts="0.16 0.18 0.21 0.24 0.27 0.30 0.34 0.39 0.44 0.52"

#trantypes="lvt ulvt" # svt doesnt work yet
trantypes="ulvt lvt" # svt doesnt work yet
#libs="i0s i0m"
libs="i0s"

delns="0"
delps="0"
steps="1e-9"  # really just the reset time

cscales="1.0"

modleaves="true"

delns="0.000 0.020 0.040 0.080 0.120"
delps="0.000 0.010 0.020"
temps="-80 -40 0 50 85 105"

if [ "$1" == "-Aug22.0" ]; then
    stages="5 11 20"
    trantypes="ulvt"
    temps="-40 0"
    volts="0.32"
    delns="0.000 0.001 0.010 0.020"
    delps="0.000 0.001 0.002 0.004 0.008"
fi

if [ "$1" == "-supplemental" ]; then
    stages="2 5 80"
fi

if [ "$1" == "-base11" ]; then
    stages="11"
    delns="0.000"
    delps="0.000"
    temps="-80 -70 -60 -50 -40 -35 -30 -25 -20 -15 -10 -5 0 5 10 15 20 25 30 35 40 45 50 55 60 65 70 75 85 95 105 115 125 135"
    trantypes="ulvt lvt"
fi

if [ "$1" == "-thresh11" ]; then
    stages="11"
    delns="0.000 0.020 0.040 0.080 0.120"
    delps="0.000 0.010 0.020"
    temps="-80 -40 -20 0 20 30 40 50 60 70 75 80 85 95 105 115 125 135"
    trantypes="ulvt"
fi

# for testing:

testing=0

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

