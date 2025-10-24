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

tgt_node="right_stack_inst/mgtile_inst3/myOctaFlop/clk"
#src_node="bzmnotchwrap/bzmnotch/clock_opt_cts_bzmpll_inst0_fast_clk_vip124088/clk"
src_node="bzmnotchwrap/bzmnotch/dcc0_bypass_clkmux_ctech_clk_mux/clk2"

src_aplot=`echo ${src_node} | tr 'A-Z/' 'a-z^'`
tgt_aplot=`echo ${tgt_node} | tr 'A-Z/' 'a-z^'`

nb_queue=${NBPOOL}
nb_qslot=${NBQSLOT}
step=1

PDMI_LIB="/p/hdk/cad/pdk/pdk783_r0.8_23ww24.2/cmi/hspice/pdmi/lnx86/64bit/pdmi.so"
XA="/p/hdk/cad/xa/U-2023.03-1/bin/xa"
HSPICE="/p/hdk/cad/hspice/U-2023.03-2/hspice/bin/hspice"

#cells="i0scbf000aa1d36x5 i0scinv00aa1n18x5 i0scinv00aa1d24x5 i0scinv00aa1n36x4 i0scinv00aa1d36x5 i0scinv00aa1d48x5"
cells="i0scinv00aa1d36x5"

corners="tttt rcss rcff rxsf rxfs"

temps="-40 0 50 85 100"

volts="0.22 0.24 0.26 0.28 0.30 0.32 0.34 0.40 0.45"

#cycles="225 250 333 400 500 555 600 750 800 900 1000 1100 1200 1300 1400 1500 1750 2000 2500 3000"

cycles="555 714 2000 4000"

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


    forbidden=0


    if [ "$forbidden" != "1" ]; then
        for cell in ${cells};   do
            sed "s/@CLOCKCELL@/${cell}/" ${SRCDIR}/dut.sp.tmpl > ${SRCDIR}/dut.sp.${cell}
        for cycl in ${cycles};  do
        for volt in ${volts};   do
        for corn in ${corners}; do
        for temp in ${temps};   do
            
            runfile=${RUNDIR}/${tasknum}.sh

            runsubdir=${RUNDIR}/${tasknum}.run

            mkdir ${runsubdir}
            spfile=${runsubdir}/circuit.sp

            pushd ${runsubdir} 
            ln -sf ${SRCDIR}/dut.sp.${cell} ./dut.sp
            ln -sf ${SRCDIR}/circuit_v.sp .
            ln -sf ${SRCDIR}/probes.sp .
            ln -sf ${SRCDIR}/../tc2.spi
            ln -sf ${SRCDIR}/../do_measure.sh .
            popd


	    cat > ${runfile} << EOF
#!/bin/sh -x
# This is from : ${SRCDIR} 
#
# This is $0
# at `date`

EOF
            pulsewidth=`expr ${cycl} / 2`
            echo "hostname" >> ${runfile}
            echo "pwd" >> ${runfile}
            echo "export PDMI_LIB="${PDMI_LIB} >> ${runfile}
            echo "cd ${runsubdir}" >> ${runfile}

            echo "#${XA} circuit.sp" >> ${runfile}
            echo "${HSPICE} circuit.sp || exit 1" >> ${runfile}

            echo "./do_measure.sh hspice 1278p3,${corn},${cycl},${cell},dyn,hspice,1,${volt},${temp}, > measure.dat" >> ${runfile}
            echo "${M3UTILS}/spice/ct/AMD64_LINUX/ct -F -threads 4 -R 1e-12 -z -translate -C *.fsdb && /bin/rm -rf *.ctwork && /bin/rm -f *.fsdb" >> ${runfile}
	    echo "${measure} -inputwidth ${pulsewidth}e-12 -vtrue ${volt} -tgt ${tgt_aplot} -src ${src_aplot}" >> ${runfile} 

            

            cat > ${spfile} <<EOF

* clock path / from Matt

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


.lib '/nfs/site/disks/zsc9_fwr_sd_001/mnystroe/p1278_3x0p9eu1/2023ww43d5/models_core_hspice/m14_2x_1xa_1xb_6ya_2yb_2yc__bm5_1ye_1yf_2ga_mim3x_1gb__bumpp/p1278_3.hsp' ${corn}

*.lib /p/hdk/cad/pdk/pdk783_r0.8HP2_23ww40.4//models/core/hspice/m14_2x_1xa_1xb_6ya_2yb_2yc__bm5_1ye_1yf_2ga_mim3x_1gb__bumpp/p1278_3.hsp ${corn}

.param vminer=${volt}
.param vtrue='2*vminer'
.param temp=${temp}

.include "tc2.spi"
*.include "pathtc2_stim.spo"
.include 'dut.sp'
.include 'circuit_v.sp'
.include 'probes.sp'

.probe tran v(*clk)
*.probe tran v(*)

.param rise=10ps 
.param halfrise='rise*0.5'
.param start=1ns
.param cycle=${cycl}ps
.param halfcycle='0.5*cycle'

vsrc_victim ${src_node} 0 pwl(0.0ns	 0
+ 'start+0*halfcycle-halfrise'           0
+ 'start+0*halfcycle+halfrise'           vtrue
+ 'start+1*halfcycle-halfrise'           vtrue
+ 'start+1*halfcycle+halfrise'           0
+ 'start+2*halfcycle-halfrise'           0
+ 'start+2*halfcycle+halfrise'           vtrue
+ 'start+3*halfcycle-halfrise'           vtrue
+ 'start+3*halfcycle+halfrise'           0
+ 'start+4*halfcycle-halfrise'           0
+ 'start+4*halfcycle+halfrise'           vtrue
+ 'start+5*halfcycle-halfrise'           vtrue
+ 'start+5*halfcycle+halfrise'           0
+ 'start+6*halfcycle-halfrise'           0
+ 'start+6*halfcycle+halfrise'           vtrue
+ 'start+7*halfcycle-halfrise'           vtrue
+ 'start+7*halfcycle+halfrise'           0
+)

vvdd  vdd  0 'vtrue'
vvddm vddm 0 'vminer'
vvss  vss 0 0
vvssx vssx 0 0

.temp 'temp'

.tran 1ps 40ns sweep monte=${swps}

.end
EOF


            chmod +x ${runfile}
        
            tasknum=`expr $tasknum + 1`
            
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

