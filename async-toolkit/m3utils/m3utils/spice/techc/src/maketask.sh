#!/bin/sh

# run this script in the "work" subdirectory
# m3utils/spice/techc/work

#M3UTILS=/nfs/site/disks/zsc3_fon_fe_0001/mnystroe/m3utils
ROOTDIR=${M3UTILS}/spice/techc
SRCDIR=${ROOTDIR}/src
BINDIR=${ROOTDIR}/program/AMD64_LINUX
PROG=${BINDIR}/techc
TEMPLATEDIR=${SRCDIR}
DATE=`date -Is`
RUNDIR=`pwd`/nb.run-${DATE}

nb_queue=zsc3_express
nb_qslot=/XCC/LBM/SD

nb_queue=zsc3_normal
nb_qslot=/XCC/LBM/RTL

nb_queue=${NBPOOL}
nb_qslot=${NBQSLOT}

nb_maxjobs=2000

#SIM=xa
SIM=hspice

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

SETUP_ARGS=""

roots="default:default"

p1278p3roots="0p5:/p/hdk/cad/pdk/pdk783_r0.5_22ww52.5/models/core/hspice/m15_2x_1xa_1xb_4ya_2yb_2yc_3yd__bm5_1ye_1yf_2ga_mim3x_1gb__bumpp 0p9e:/p/hdk/cad/pdk/pdk783_r0.9e_23ww29.2_beta/models/core/hspice/m15_2x_1xa_1xb_4ya_2yb_2yc_3yd__bm5_1ye_1yf_2ga_mim3x_1gb__bumpp asfit2023ww29:/nfs/site/disks/zsc9_fwr_sd_001/mnystroe/1278_lowvoltage/2023ww29d2/models_core_hspice/1/m14_2x_1xa_1xb_6ya_2yb_2yc__bm5_1ye_1yf_2ga_mim3x_1gb__bumpp 0p8:/p/hdk/cad/pdk/pdk783_r0.8_23ww24.2/models/core/hspice/m14_2x_1xa_1xb_6ya_2yb_2yc__bm5_1ye_1yf_2ga_mim3x_1gb__bumpp"

p1278p3_0p8roots='0p8:/p/hdk/cad/pdk/pdk783_r0.8_23ww24.2/models/core/hspice/m14_2x_1xa_1xb_6ya_2yb_2yc__bm5_1ye_1yf_2ga_mim3x_1gb__bumpp'

p1278p3_0p9eroot='/p/hdk/cad/pdk/pdk783_r0.9e_23ww29.2_beta/models/core/hspice/m15_2x_1xa_1xb_4ya_2yb_2yc_3yd__bm5_1ye_1yf_2ga_mim3x_1gb__bumpp'

p1278p3_0p9eroots="0p9e:${p1278p3_0p9eroot}"

p1278p3_0p9eu1root='/nfs/site/disks/zsc9_fwr_sd_001/mnystroe/p1278_3x0p9eu1/2023ww43d5/models_core_hspice/m14_2x_1xa_1xb_6ya_2yb_2yc__bm5_1ye_1yf_2ga_mim3x_1gb__bumpp'


stdcells="skip"
sigmas="skip"
hspicemodelroot=""

if [ "$1" == "-aoitech" ]; then
    runmode="override"
    volts="0.30"
    temps="75"
    modes="dyn"
    paras="true"
    corners="tt"
    step=4
    techs="1278p3 n3e n5"
    gates="aoi_z1_0p0sigma aoi_z2_0p0sigma"
    fo="4"
    SETUP_ARGS="export SETUP_MC_FILE_ONLY=''"
fi

if [ "$1" == "-variation1278test" ]; then
    runmode="override"
    volts="0.30"
    temps="25"
    modes="dyn"
    paras="true"
    corners="tt"
    step=1
    techs="1278p3"
    gates="xor_z1 xor_z2 xor_z3"
    stdcells="i0m i0s"
    sigmas="0.0 0.5 5.3"
    fo="3"
    SIM="xa"
    hspicemodelroot=${p1278p3_0p9eu1root}
fi

if [ "$1" == "-variation1278" ]; then
    runmode="override"
    volts="0.10 0.12 0.14 0.16 0.18 0.20 0.22 0.24 0.26 0.28 0.30 0.32 0.34 0.36 0.38 0.40 0.45 0.50 0.55 0.60 0.65 0.75 0.80 0.90 1.00"
    temps="0 25 50 65 70 75 85 100 125"
    modes="dyn"
    paras="true"
    corners="tt"
    step=4
    techs="1278p3"
    gates="xor_z1 xor_z2 xor_z3"
    stdcells="i0m i0s"
    sigmas="0.0 0.5 1.0 4.0 4.5 5.3 6.0"
    fo="3"
    SIM="xa"
    hspicemodelroot=${p1278p3_0p9eu1root}
fi

if [ "$1" == "-variation1278_0p9e" ]; then
    runmode="override"
    volts="0.10 0.12 0.14 0.16 0.18 0.20 0.22 0.24 0.26 0.28 0.30 0.32 0.34 0.36 0.38 0.40 0.45 0.50 0.55 0.60 0.65 0.75 0.80 0.90 1.00"
    temps="0 25 50 65 70 75 85 100 125"
    modes="dyn"
    paras="true"
    corners="tt"
    step=4
    techs="1278p3"
    gates="xor_z1 xor_z2 xor_z3"
    stdcells="i0m i0s"
    sigmas="0.0 0.5 1.0 4.0 4.5 5.3 6.0"
    fo="3"
    SIM="xa"
    hspicemodelroot=${p1278p3_0p9eroot}
fi

if [ "$1" == "-variationlow" ]; then
    runmode="override"
    volts="0.08 0.09 0.10 0.11 0.12 0.13 0.14 0.15 0.16"
    temps="25 50 75 85 105 125"
    modes="dyn"
    paras="true"
    corners="tt"
    step=4
    techs="1278p3"
    gates="xor_z1_0p0sigma xor_z1_5p3sigma"
    fo="4"
fi

if [ "$1" == "-variation" ]; then
    runmode="override"
    volts="0.08 0.09 0.10 0.11 0.12 0.13 0.14 0.15 0.16 0.17 0.18 0.19 0.20 0.22 0.24 0.26 0.28 0.30 0.32 0.34 0.36 0.38 0.40 0.42 0.44 0.45 0.46 0.48 0.50"
    temps="0 25 50 75 85 105 125"
    modes="dyn"
    paras="true"
    corners="tt"
    step=4
    techs="1278p3"
    gates="xor_z1_0p0sigma xor_z1_5p3sigma xor_z2_0p0sigma xor_z2_5p3sigma"
    fo="4"
fi

if [ "$1" == "-variationpdk" ]; then
    runmode="override"
    volts="0.16 0.18 0.20 0.22 0.24 0.26 0.28 0.30 0.32 0.34 0.36 0.38 0.40 0.45 0.50"
    temps="-40 0 25 50 65 75 85 95 105 125"
    modes="dyn"
    paras="true"
    corners="tt"
    step=4
    techs="1278p3"
    gates="xor_z1_0p0sigma xor_z1_5p3sigma xor_z2_0p0sigma xor_z2_5p3sigma"
    fo="4"
    roots=${p1278p3roots}
fi

if [ "$1" == "-variationpdk0p8" ]; then
    runmode="override"
    volts="0.16 0.18 0.20 0.22 0.24 0.26 0.28 0.30 0.32 0.34 0.36 0.38 0.40 0.45 0.50"
    temps="-40 0 25 50 65 75 85 95 105 125"
    modes="dyn"
    paras="true"
    corners="tt"
    step=4
    techs="1278p3"
    gates="xor_z1_0p0sigma xor_z1_5p3sigma xor_z2_0p0sigma xor_z2_5p3sigma"
    fo="4"
    roots=${p1278p3_0p8roots}
fi

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

if [ "$1" == "-i0m_i0s" ]; then
    runmode="override"
    volts="0.20 0.22 0.24 0.26 0.28 0.30 0.32 0.34 0.36 0.38 0.40 0.45 0.50 0.55"
    temps="0 25 50 75 85 105 125"
    modes="dyn"
    paras="true"
    corners="tt"
    step=4
    techs="1278p3 1278p3_i0m"
    gates="xor_z1 xor_z2 xor_z3"
    roots=${p1278p3_0p9eroots}
fi

if [ "$1" == "-i0m_i0s_hv" ]; then
    runmode="override"
    volts="0.60 0.65 0.70 0.75 0.80 0.85 0.90 0.95 1.00 1.05 1.10 1.15 1.20"
    temps="0 25 50 75 85 105 125"
    modes="dyn"
    paras="true"
    corners="tt"
    step=4
    techs="1278p3 1278p3_i0m"
    gates="xor_z1 xor_z2 xor_z3"
    roots=${p1278p3_0p9eroots}
fi

if [ "$1" == "-i0m_i0s_hv_all" ]; then
    runmode="override"
    volts="0.20 0.22 0.24 0.26 0.28 0.30 0.32 0.34 0.36 0.38 0.40 0.45 0.50 0.55 0.60 0.65 0.70 0.75 0.80 0.85 0.90 0.95 1.00 1.05 1.10 1.15 1.20"
    temps="0 25 50 75 85 105 125"
    modes="dyn"
    paras="true"
    corners="tt"
    step=4
    techs="1278p3 1278p3_i0m"
    gates="xor_z1 xor_z2 xor_z3"
    roots=${p1278p3_0p9eroots}
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
  SubmissionArgs --class SLES12SP5

  Queue ${nb_queue} {
    Qslot ${nb_qslot}
    MaxJobs ${nb_maxjobs}
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

for sigm in ${sigmas}; do
for stdc in ${stdcells}; do
for para in ${paras}; do
for corn in ${corners}; do
for mode in ${modes}; do
for temp in ${temps}; do
for volt in ${volts}; do
for tech in ${techs}; do
for root in ${roots}; do

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

    forbidden=0

    if [ "$gate" == "xor_z1" ] && [ "$tech" == "1278p3_i0m" ]; then
        echo "skipping forbidden combo $gate $para $corn $mode $temp $volt $tech $root"
        forbidden=1
    fi

    if [ "$gate" == "xor_z1" ] && [ "$stdc" == "i0m" ]; then
        echo "skipping forbidden combo $gate $para $corn $mode $temp $volt $tech $root $stdc"
        forbidden=1
    fi

    sigarg=""
    
    if [ "$sigm" != "skip" ]; then
        sigarg="-sigma $sigm"
    fi

    if [ "$stdc" != "skip" ]; then
        stdcarg="-stdcells $stdc"
    fi

    if [ "$forbidden" != "1" ]; then
    for tran in ${trantypes}; do
        runfile=${RUNDIR}/${tasknum}.sh
        echo "#!/bin/sh -x" > ${runfile}
        echo "hostname" >> ${runfile}
        echo "pwd" >> ${runfile}

	echo ${SETUP_ARGS} >> ${runfile}

        if [ "${hspicemodelroot}" != "" ] ; then
            hsmrarg="-hspicemodelroot ${hspicemodelroot}"
        else
            hsmrarg=""
        fi
        torun="${PROG} \
              ${hsmrarg} \
              -tech ${tech} -corn ${corn} -tran ${tran} \
              -mode ${mode} -simu ${SIM} -T ${TEMPLATEDIR} \
              -volt ${volt} -temp ${temp} \
              -para ${para} \
              -gate ${gate} -fo ${fo} \
              ${sigarg} ${stdcarg} \
              -d ${RUNDIR}/${tasknum}.run -C"

        if [ "${root}" != "default:default" ]; then
            shortroot=`echo ${root} | awk -F: '{print $1}'`
            rootpath=`echo ${root} | awk -F: '{print $2}'`

            torun="${torun} -hspicemodelroot ${rootpath} ${shortroot}"
        fi

        echo "if [ -f ${RUNDIR}/measure.dat ]; then" >> ${runfile}
        echo "    exit 0"                            >> ${runfile}
        echo "fi"                                    >> ${runfile}
        echo ""                                      >> ${runfile}
        
        echo "${torun} -p setup -p simulate" \
             >> ${runfile}

        echo "${torun} -p convert -p clean" \
             >> ${runfile}
        
        echo "${torun} -p measure" \
             >> ${runfile}
        
        chmod +x ${runfile}
        
        tasknum=`expr $tasknum + 1`
    done
    fi

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
