#!/bin/sh 

testing=0

stopargs=0

while [ "$stopargs" = "0" ]; do
  if [ "$1" = "-testing" ]; then
    testing=1
    shift
  else	
    stopargs=1
  fi
done

myname=$0

mydir=`dirname $myname`
realdir=`realpath $mydir`

dirlast=`basename $realdir`

SRCDIR=${realdir}
TEMPLATEDIR=${SRCDIR}
MEASUREDIR=${SRCDIR}/..

measure=`realpath ${MEASUREDIR}/AMD64_LINUX/measure`

DATE=`date -Is`
RUNDIR=`pwd`/nb.run-${dirlast}-${DATE}

nstages=100
ncycles=`expr $nstages / 2`
startt=5000 # picoseconds
resett=2000 # picoseconds

src_node="li0"
tgt_node="li${nstages}"

src_aplot=`echo ${src_node} | tr 'A-Z/' 'a-z^'`
tgt_aplot=`echo ${tgt_node} | tr 'A-Z/' 'a-z^'`

nb_queue=${NBPOOL}
nb_qslot=${NBQSLOT}
step=1

PDMI_LIB="/p/hdk/cad/pdk/pdk783_r0.8_23ww24.2/cmi/hspice/pdmi/lnx86/64bit/pdmi.so"
XA="/p/hdk/cad/xa/U-2023.03-1/bin/xa"
HSPICE="/p/hdk/cad/hspice/U-2023.03-2/hspice/bin/hspice"

cells="i0scinv00aa1d36x5"

corners="tttt rcss rcff rxsf rxfs"
types="inv di"

temps="-40 0 50 85 100"

volts="0.22 0.24 0.26 0.28 0.30 0.32 0.34 0.40 0.45"

#cycles="225 250 333 400 500 555 600 750 800 900 1000 1100 1200 1300 1400 1500 1750 2000 2500 3000"

cycles="200 400 600 800 1000 1200 1400 1600"

wireresistances="200"
wirecapacitances="5f"

swps="20"


# for testing:




if [ "${testing}" == "1" ]; then
    corners="tttt"
    temps="0"
    volts="0.34"
    cycles="1000"
    swps="3"
fi

######################################################################

taskfile=full-${dirlast}-${DATE}.task

mkdir ${RUNDIR}

cat > ${taskfile} <<EOF
JobsTask {
  WorkArea ${RUNDIR}
  SubmissionArgs --class SLES12SP5&&16C&&16G

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


    forbidden=0


    if [ "$forbidden" != "1" ]; then
        for styp in ${types};   do
            sed -e "s/@STAGE@/stage_${styp}/" ${SRCDIR}/dut.sp.tmpl > ${SRCDIR}/dut.sp.${styp}
        for cell in ${cells};   do
        for cycl in ${cycles};  do
            pulsewidth=`expr ${cycl} / 2`
            endsim="`expr $cycl \* $ncycles + $startt`p"
        for volt in ${volts};   do
        for corn in ${corners}; do
        for temp in ${temps};   do
        for wres in ${wireresistances}; do
        for wcap in ${wirecapacitances}; do
            
            runfile=${RUNDIR}/${tasknum}.sh

            runsubdir=${RUNDIR}/${tasknum}.run

            mkdir ${runsubdir}
            spfile=${runsubdir}/circuit.sp

            pushd ${runsubdir} 
            ln -sf ${SRCDIR}/../do_measure.sh .
            ln -sf ${SRCDIR}/dut.sp.${styp} ./dut.sp
            ln -sf ${SRCDIR}/src.sp .
            popd


	    cat > ${runfile} << EOF
#!/bin/sh -x
# This is from : ${SRCDIR} 
#
# This is $0
# at `date`

EOF
            echo "hostname" >> ${runfile}
            echo "pwd" >> ${runfile}
            echo "export PDMI_LIB="${PDMI_LIB} >> ${runfile}
            echo "cd ${runsubdir}" >> ${runfile}

            echo "#${XA} circuit.sp" >> ${runfile}
            echo "${HSPICE} circuit.sp -mt 4 || exit 1" >> ${runfile}

            echo "./do_measure.sh hspice 1278p3,${corn},${cycl},${cell},dyn,hspice,1,${volt},${temp}, > measure.dat" >> ${runfile}
            echo "${M3UTILS}/spice/ct/AMD64_LINUX/ct -F -threads 16 -R 1e-12 -z -translate -C *.fsdb && /bin/rm -rf *.ctwork && /bin/rm -f *.fsdb" >> ${runfile}
	    echo "${measure} -inputwidth ${pulsewidth}e-12 -vtrue ${volt} -tgt ${tgt_aplot} -src ${src_aplot}" >> ${runfile} 

            
            
            cat > ${spfile} <<EOF

* clock path

.option cmipath='/p/hdk/cad/pdk/pdk783_r0.9_23ww26.5_alpha/cmi/hspice/cmi/lnx86/64bit'

.option redefsub=2

.option cmiflag=1
.option cmi02opt=1
.option cmiusrflag=3


* To enable use of Intelspecific instance parameter pes/ped/pe, etc.
*.option process_dev_params=1
* To enable use of Intel's shrink factor .option shrink=...
*.option cmiusrflag=3

* To enable the use of the PDMI models
*.option pdmi="1"

* To change the output formatting of .mt0 file
.option measform="1"
.option runlvl=5
.options method=gear

* Updated HSP file below

.option Add_Variation=yes

.option POST=fsdb probe=1

.probe tran v(li*)
.probe tran v(resetn)

.lib '/nfs/site/disks/zsc9_fwr_sd_001/mnystroe/p1278_3x0p9eu1/2023ww43d5/models_core_hspice/m14_2x_1xa_1xb_6ya_2yb_2yc__bm5_1ye_1yf_2ga_mim3x_1gb__bumpp/p1278_3.hsp' ${corn}

.param vtrue=${volt}
.param temp=${temp}

.param rise=10ps 
.param halfrise='rise*0.5'
.param reset=${resett}ps
.param start=${startt}ps
.param cycle=${cycl}ps
.param halfcycle='0.5*cycle'

vreset resetn vssx pwl(0.0ns 0 'reset-halfrise' 0 'reset+halfrise' vtrue)

.include 'src.sp'

.include '/nfs/site/disks/zsc9_fwr_sd_001/amlines/FIN3/fin3_release.7/export/i0sctree2aa1n12x5.cdl'
.include '/p/hdk/cad/stdcells/lib783_i0s_160h_50pp/pdk080_r4v2p0_efv/clk_ulvt/spice/lib783_i0s_160h_50pp_clk_ulvt.sp'
.include '/p/hdk/cad/stdcells/lib783_i0s_160h_50pp/pdk080_r4v2p0_efv/base_ulvt/spice/lib783_i0s_160h_50pp_base_ulvt.sp'

.subckt stage_di li lo ro ri resetn vdd vssx
xrinv0 resetn intreset vdd vssx i0sinv000aa1n02x5
xnor0  intreset ri ack vdd vssx i0snor002aa1n06x5
xcel0  ack li q        vdd vssx i0sctree2aa1n12x5
xrdr0  q ro            vdd vssx i0scbf000aa1n18x5
xldr0  q lo            vdd vssx i0scbf000aa1n18x5
.ends

.subckt stage_inv li lo ro ri resetn vdd vssx
xinv li ro vdd vssx ${cell}
.ends 

.include 'dut.sp'

vvdd  vdd  0 'vtrue'
vvss  vss 0 0
vvssx vssx 0 0

.temp 'temp'

.tran 1ps ${endsim} sweep monte=${swps}

.end
EOF


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
    fi

echo "${tasknum} jobs"

launchnum=0

echo "tasknum ${tasknum} launchnum ${launchnum}"

while [ "${launchnum}" -lt "${tasknum}" ]; do
    echo "nbjob run --log-file ${RUNDIR}/${launchnum}.log ${SRCDIR}/../launcher.sh ${RUNDIR} ${launchnum} ${step}"    >> $taskfile
    launchnum=`expr $launchnum + $step`
done


cat >> ${taskfile} <<EOF
       }
}
EOF

rm -f nb.latest
ln -sf ${RUNDIR} nb.latest
ln -sf ${taskfile} latest.task

