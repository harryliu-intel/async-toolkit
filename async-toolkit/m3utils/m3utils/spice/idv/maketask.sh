#!/bin/sh 

SRCDIR=${M3UTILS}/spice/idv
TEMPLATEDIR=${SRCDIR}
DATE=`date -Is`
RUNDIR=`pwd`/nb.run-${DATE}

nb_queue=${NBPOOL}
nb_qslot=${NBQSLOT}
step=10

PDMI_LIB="/p/hdk/cad/pdk/pdk783_r0.8_23ww24.2/cmi/hspice/pdmi/lnx86/64bit/pdmi.so"
XA="/p/hdk/cad/xa/U-2023.03-1/bin/xa"
HSPICE="/p/hdk/cad/hspice/U-2023.03-2/hspice/bin/hspice"

cells="aoi bfn inv mdn nan nor"

corners="tttt rcss rcff rxsf rxfs"

temps="0 25 50 60 75 85 100 125"

volts="0.20 0.22 0.24 0.26 0.28 0.30 0.32 0.34 0.36 0.38 0.40 0.45 0.50 0.55 0.60 0.65 0.70 0.75 0.80 0.85 0.90 0.95 1.00"

trantypes="lvt ulvt"

heights="s m"

strengths="1 2 3"


if [ "$1" == "-lv" ]; then
    volts="0.10 0.11 0.12 0.13 0.14 0.15 0.16 0.17 0.18 0.19"
fi

# for testing:

testing=0

if [ "${testing}" == "1" ]; then
    cells="nor"
    corners="tttt"
    temps="0"
    volts="0.34"
    trantypes="ulvt"
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

for cell in ${cells}; do
for heig in ${heights}; do
for strn in ${strengths}; do

    forbidden=0

    if [ "$strn" == "1" ] && [ "$heig" == "m" ]; then
        echo "skipping forbidden combo $cell $heig $strn"
        forbidden=1
    fi

    if [ "$forbidden" != "1" ]; then
        for volt in ${volts}; do
        for tran in ${trantypes}; do
        for corn in ${corners}; do
        for temp in ${temps}; do

            if   [ "${tran}" == "ulvt" ]; then
                xx="aa"
            elif [ "${tran}" == "lvt" ]; then
                xx="bb"
            else
                echo "unknown trantype ${tran}"
                exit 0
            fi

            cellname="i70idv${heig}${cell}11${strn}${strn}11${xx}2top"

            
            runfile=${RUNDIR}/${tasknum}.sh

            runsubdir=${RUNDIR}/${tasknum}.run

            mkdir ${runsubdir}
            spfile=${runsubdir}/circuit.sp

            pushd ${runsubdir}
            ln -sf ${SRCDIR}/idvosc.spf .
            ln -sf ${SRCDIR}/do_measure.sh .
            popd


            echo "#!/bin/sh -x" > ${runfile}
            echo "hostname" >> ${runfile}
            echo "pwd" >> ${runfile}
            echo "export PDMI_LIB="${PDMI_LIB} >> ${runfile}
            echo "cd ${runsubdir}" >> ${runfile}

            echo "#${XA} circuit.sp" >> ${runfile}
            echo "${HSPICE} circuit.sp" >> ${runfile}

            echo "./do_measure.sh hspice ${tech}_${heig},${corn},${tran},${cell}_z${strn},dyn,xa,1,${volt},${temp}, > measure.dat" >> ${runfile}

            

            cat > ${spfile} <<EOF
* IDV 18A
.OPTION POST=fsdb BRIEF probe accurate=1
.option cmiflag=1
.option cmiusrflag=3
.option scale=1.0
.option pdmi=1
.option measform=2
.option mixed_num_format=1
.global vss

.option cmipath='/p/hdk/cad/pdk/pdk783_r0.9e_23ww29.2_beta/cmi/hspice/cmi/lnx86/64bit'
.LIB '/p/hdk/cad/pdk/pdk783_r0.8_23ww24.2/models/core/hspice/m12_2x_1xa_1xb_6ya_2yc__bm5_1ye_1yf_2ga_mim3x_1gb__bumpp/p1278_3.hsp' ${corn}

.include 'idvosc.spf'

* tend  is end of sim
* twait is how long to wait before starting circuit

.param avdd=${volt} atemp=${temp} hvdd='avdd/2' twait=10n tend=1000n
.temp atemp
.tran 1p tend
*.probe tran V(*) I(V*)
.probe tran v(o) i(vvdd)
.option autostop=1
vdd vdd 0 avdd
vss vss 0 0
ven en 0 pwl 0 0 twait 0 'twait+10p' avdd

.param measure_cycles=10
.param skip_cycles=2

.measure tran tmstart when v(o)=hvdd td=twait rise='skip_cycles'
.measure tran tmstop  when v(o)=hvdd td=twait rise='skip_cycles+measure_cycles'
.measure tran tcyc    param='(tmstop - tmstart)/measure_cycles'

.measure tran i_dyn avg I(vvdd) from=tmstart to=tmstop
.measure tran i_lea avg I(vvdd) from='twait/2' to='twait'


EOF

            echo "Vvdd vdd vddx 0" >> $spfile
            echo "X0 en vdd o vddx vss $cellname" >> $spfile
            
            
            echo ".end" >> $spfile

            chmod +x ${runfile}
        
            tasknum=`expr $tasknum + 1`
            
        done
        done
        done
        done
    fi
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

