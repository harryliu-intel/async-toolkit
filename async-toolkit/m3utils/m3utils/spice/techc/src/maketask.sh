#!/bin/sh

# run this script in the "work" subdirectory
# m3utils/spice/techc/work

ROOTDIR=`pwd`/..
SRCDIR=${ROOTDIR}/src
BINDIR=${ROOTDIR}/AMD64_LINUX
PROG=${BINDIR}/techc
TEMPLATE=${SRCDIR}/ckt.sp
DATE=`date -Is`
RUNDIR=`pwd`/nb.run-${DATE}

nb_queue=zsc3_express
nb_qslot=/XCC/LBM/SD

nb_queue=zsc3_normal
nb_qslot=/XCC/LBM/RTL

step=4
# we get about 4 CPUs per machine?  6 ought to be more than enough, 4 might be optimal?

fo="1"
corners="ss tt ff"
xor_corners="tt"
aoi_corners="tt"

temps="-40 -20 0 25 50 75 85 100 125"
xor_temps="50 75"
aoi_temps="25 50 75 85 100 125"

buf_volts="0.11 0.13 0.15 0.17 0.19 0.21 0.23 0.25 0.27 0.29 0.31 0.33 0.35 0.40 0.45 0.50 0.55 0.60 0.65 0.70 0.75 0.80 0.85 0.90 0.95 1.0"
volts="0.11 0.13 0.15 0.17 0.19 0.21 0.23 0.25 0.27 0.29 0.31 0.33 0.35 0.40 0.45 0.50"

#techs="n5 1276p4 n3 n3e"
techs="n5 1276p4 1276p4_g1m n3e 1278p3"

#modes="dyn leak"
# we no longer need "leak"
modes="dyn"

#paras="true false"
# parasitics or not?
paras="true"

# short test

#techs="n3e"
#corners="tt"
#volts="0.29"

#gates="xor buf aoi"
gates="xor buf aoi"

#allvts="true"
allvts="false"

runmode="default"

trantypes=""

if [ "$1" == "-2023-01-18" ]; then
    runmode="override"
    volts="0.20 0.22 0.24 0.26 0.28 0.30 0.32 0.34 0.36 0.38 0.40 0.42 0.44"
    temps="50 75 125"
    modes="dyn"
    paras="true"
    corners="tt ss ff"
    step=4
    techs="n5 1276p4 1276p4_g1m n3e"
fi

if [ "$1" == "-fins" ]; then
    runmode="override"
    volts="0.20 0.22 0.24 0.26 0.28 0.30 0.32 0.34 0.36 0.38 0.40 0.42 0.44"
    temps="50 75 125"
    modes="dyn"
    paras="true"
    corners="tt"
    step=4
    techs="n5 1276p4 1276p4_g1m 1276p4_aml1 1276p4_aml2 n3e"
    trantypes="ulvt"
fi

if [ "$1" == "-quick" ]; then
    # quick simulation, just to test
    # note all technologies are included
    runmode="override"
    volts="0.29 0.40 0.60 0.90"
    temps="50 125"
    modes="dyn"
    paras="true false"
    corners="tt"
    step=15
fi

if [ "$1" == "-1278p3" ]; then
    runmode="override"
    techs="1278p3"
fi

if [ "$1" == "-gates" ]; then
    runmode="override"
    volts="0.40"
    temps="50"
    modes="dyn"
    paras="true false"
    corners="tt"
    step=4
fi

if [ "$1" == "-temp" ]; then
    runmode="override"
    volts=${buf_volts}
    gates="buf"
    modes="dyn"
    paras="true"
    corners="tt"
    step=4
    techs="n3e"
    
    mintemp=0
    maxtemp=105
    stptemp=5
    t=${mintemp}
    temps=""
    
    while [ "${t}" -lt "${maxtemp}" ]; do
        temps="${temps} ${t}"
        t=`expr ${t} + ${stptemp}`
    done

fi

if [ "$1" == "-tempi3" ]; then
    runmode="override"
    volts=${buf_volts}
    gates="aoi"
    modes="dyn"
    paras="true"
    corners="ff tt ss"
    step=4
    techs="1276p4"
    
    mintemp=0
    maxtemp=105
    stptemp=5
    t=${mintemp}
    temps=""
    
    while [ "${t}" -lt "${maxtemp}" ]; do
        temps="${temps} ${t}"
        t=`expr ${t} + ${stptemp}`
    done

fi

if [ "$1" == "-i3sensor" ]; then
    runmode="override"
    volts=${buf_volts}
    gates="buf"
    modes="dyn"
    paras="true"
    corners="tt ss ff"
    step=4
    techs="1276p4"
    
    mintemp=0
    maxtemp=125
    stptemp=2
    t=${mintemp}
    temps=""
    
    while [ "${t}" -le "${maxtemp}" ]; do
        temps="${temps} ${t}"
        t=`expr ${t} + ${stptemp}`
    done

    minvolts=500
    maxvolts=1100
    stpvolts=20
    v=${minvolts}
    volts=""

    while [ "${v}" -le "${maxvolts}" ]; do
        thisv=`echo "${v} * 0.001" | bc`
        echo $thisv
        volts="${thisv} ${volts}"

        v=`expr ${v} + ${stpvolts}`
    done

fi

if [ "$1" == "-tempcorners" ]; then
    runmode="override"
    volts=${buf_volts}
    gates="buf"
    modes="dyn"
    paras="true"
#    corners="tt ff ss"
    corners="ff ss"
    step=4
    techs="n3e"
    
    mintemp=20
    maxtemp=105
    stptemp=5
    t=${mintemp}
    temps=""
    
    while [ "${t}" -lt "${maxtemp}" ]; do
        temps="${temps} ${t}"
        t=`expr ${t} + ${stptemp}`
    done

fi

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

for gate in ${gates}; do

    if [ "${runmode}" == "default" ]; then
        if   [ "${gate}" == "xor" ]; then
            temps=${xor_temps}
            corners=${xor_corners}
        elif [ "${gate}" == "aoi" ]; then
            temps=${aoi_temps}
            corners=${aoi_corners}
        elif [ "${gate}" == "buf" ]; then
            volts=${buf_volts}
        fi
    fi
    
for para in ${paras}; do
for corn in ${corners}; do
for mode in ${modes}; do
for temp in ${temps}; do
for volt in ${volts}; do
for tech in ${techs}; do

    if [ "${trantypes}" == "" ]; then
        if [ "${tech}" == "n5" ]; then
            if [ "${allvts}" == "true" ]; then
                trantypes="elvt ulvt ulvtll lvt lvtll svt" 
                # svtll seems some weird option -- delete for now
            else
                trantypes="elvt ulvt ulvtll"
            fi
        elif [ "${tech}" == "n3e" ]; then
            trantypes="elvt ulvt ulvtll lvt lvtll svt"
            if [ "${allvts}" == "true" ]; then
                trantypes="elvt ulvt ulvtll lvt lvtll svt" 
            else
                trantypes="elvt ulvt ulvtll"
            fi
        elif [ "${tech}" == "1278p3" ]; then
            if [ "${allvts}" == "true" ]; then
                trantypes="ulvt lvt svt svtll"
            else
                trantypes="ulvt lvt"
            fi
        else
            if [ "${allvts}" == "true" ]; then
                trantypes="ulvt lvt svt"
            else
                trantypes="ulvt lvt"
            fi
        fi
    fi

    for tran in ${trantypes}; do
        echo "#!/bin/sh -x" > ${RUNDIR}/${tasknum}.sh
        echo "hostname" >> ${RUNDIR}/${tasknum}.sh
        echo "pwd" >> ${RUNDIR}/${tasknum}.sh

        torun="${PROG} \
              -tech ${tech} -corn ${corn} -tran ${tran} \
              -mode ${mode} -simu xa -T ${TEMPLATE} \
              -volt ${volt} -temp ${temp} \
              -para ${para} \
              -gate ${gate} -fo ${fo} \
              -d ${RUNDIR}/${tasknum}.run -C"
        
        echo "${torun} -p setup -p simulate" \
             >> ${RUNDIR}/${tasknum}.sh

        echo "${torun} -p convert -p clean" \
             >> ${RUNDIR}/${tasknum}.sh
        
        echo "${torun} -p measure" \
             >> ${RUNDIR}/${tasknum}.sh
        
        chmod +x ${RUNDIR}/${tasknum}.sh
        
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
