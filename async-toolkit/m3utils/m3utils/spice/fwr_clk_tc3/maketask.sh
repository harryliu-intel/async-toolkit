#!/bin/sh 
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0


SRCDIR=${M3UTILS}/spice/fwr_clk_tc3
TEMPLATEDIR=${SRCDIR}
MEASUREDIR=${SRCDIR}/../fwr_clk_tc2
DATE=`date -Is`
RUNDIR=`pwd`/nb.run-${DATE}

measure=`realpath ${MEASUREDIR}/AMD64_LINUX/measure`

nb_queue=${NBPOOL}
nb_qslot=${NBQSLOT}
step=1

PDMI_LIB="/p/hdk/cad/pdk/pdk783_r0.8_23ww24.2/cmi/hspice/pdmi/lnx86/64bit/pdmi.so"
XA="/p/hdk/cad/xa/U-2023.03-1/bin/xa"
HSPICE="/p/hdk/cad/hspice/U-2023.03-2/hspice/bin/hspice"

cells="i0sgdsi00bb1co0x5 i0sgdsi00bb1c18x5"

corners="tttt rcss rcff rxsf rxfs"

temps="0 50 85 100"

#volts="0.20 0.22 0.24 0.26 0.28 0.30 0.32 0.34"
volts="0.21 0.24 0.27 0.30 0.33"

#cycles="225 250 333 400 500 555 600 750 800 900 1000 1100 1200 1300 1400 1500 1750 2000 2500 3000"

cycles="500 555 714 1000 2000"

swps="20"

src_node="bzmnotchwrap/bzmnotch/bzmgio/gpio_trip_rx_out/orxout"
#tgt_node="right_stack_inst/mgtile_inst0/OCTA14_i_octa_wrapper_14/eng_divclk1"
tgt_node="right_stack_inst/mgtile_channel_l0r1/SPINE2PAR_BUF_15_RIGHT_STACK_ENG_CLK_left/ctech_lvlshift/clkout"

src_aplot=`echo ${src_node} | tr '/' '^'`
tgt_aplot=`echo ${tgt_node} | tr '/' '^'`

# for testing

testing=0

if [ "${testing}" == "1" ]; then
    corners="tttt"
    temps="0"
    volts="0.30"
    cycles="1000"
    swps="3"
fi

######################################################################

taskfile=full-${DATE}.task

mkdir ${RUNDIR}

cat > ${taskfile} <<EOF
    
JobsTask {
  WorkArea ${RUNDIR}
  SubmissionArgs --class SLES12SP5 --class 4C --class 256G

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
            ln -sf ${SRCDIR}/tc3.spi
            ln -sf ${SRCDIR}/do_measure.sh .
            popd

	    cat > ${runfile} << EOF
#!/bin/sh -x
# This is from : ${SRCDIR} 
#
# This is $0
# at `date`

EOF

            pulsewidth=`expr ${cycl} / 2`
            vchannel=`expr ${volt} * 2.0`
            echo "hostname" >> ${runfile}
            echo "pwd" >> ${runfile}
            echo "export PDMI_LIB="${PDMI_LIB} >> ${runfile}
            echo "cd ${runsubdir}" >> ${runfile}

            echo "#${XA} circuit.sp" >> ${runfile}
            echo "${HSPICE} -mt 4 -i circuit.sp" >> ${runfile}

            echo "./do_measure.sh hspice 1278p3,${corn},${cycl},${cell},dyn,hspice,1,${volt},${temp}, > measure.dat" >> ${runfile}

            echo "${M3UTILS}/spice/ct/AMD64_LINUX/ct -F -threads 4 -R 1e-12 -z -translate -C *.fsdb && /bin/rm -rf *.ctwork && /bin/rm -f *.fsdb" >> ${runfile}
	    echo "${measure} -inputwidth ${pulsewidth}e-12 -vtgt ${volt} ${tgt_aplot} -vsrc ${vchannel} ${src_aplot}" >> ${runfile} 

            cat > ${spfile} <<EOF

* clock path / from Matt
.option cmipath='/p/hdk/cad/pdk/pdk783_r0.9HP2_24ww08.5/cmi/hspice/cmi/lnx86/64bit/'
.option pdmi_lib='/p/hdk/cad/pdk/pdk783_r0.9HP2_24ww08.5/cmi/hspice/pdmi/lnx86/64bit/pdmi.so'

.option cmiflag=1
.option cmi02opt=1
.option cmiusrflag=3
.option pdmi=1

* To change the output formatting of .mt0 file
.option measform="1"
.option runlvl=5
.options method=gear

* Updated HSP file below

.option Add_Variation=yes

.option POST=fsdb probe=1


.lib /p/fdk/p1278/shelf/repo/models_core_hspice/dot3/ad-1278.3-x0p9u2/1/m14_2x_1xa_1xb_6ya_2yb_2yc__bm5_1ye_1yf_2ga_mim3x_1gb__bumpp/p1278_3.hsp ${corn}

.param vminer=${volt}
.param vchannel='2*vminer'
.param vnotch=0.75
.param vanalog=1.2
.param temp=${temp}

.include "tc3.spi"
.include 'dut.sp'
.include 'circuit_v.sp'
.include 'probes.sp'

.probe tran v(*clk)
.probe tran v(*clk0)
.probe tran v(*clk1)
.probe tran v(*clk2)
.probe tran v(*clk3)
.probe tran v(*clk4)

*.probe tran v(*)

.param rise=10ps 
.param halfrise='rise*0.5'
.param start=1ns
.param cycle=${cycl}ps
.param halfcycle='0.5*cycle'

vsrc_victim ${src_node} 0 pwl(0.0ns	 0
+ 'start+0*halfcycle-halfrise'           0
+ 'start+0*halfcycle+halfrise'           vchannel
+ 'start+1*halfcycle-halfrise'           vchannel
+ 'start+1*halfcycle+halfrise'           0
+ 'start+2*halfcycle-halfrise'           0
+ 'start+2*halfcycle+halfrise'           vchannel
+ 'start+3*halfcycle-halfrise'           vchannel
+ 'start+3*halfcycle+halfrise'           0
+ 'start+4*halfcycle-halfrise'           0
+ 'start+4*halfcycle+halfrise'           vchannel
+ 'start+5*halfcycle-halfrise'           vchannel
+ 'start+5*halfcycle+halfrise'           0
+ 'start+6*halfcycle-halfrise'           0
+ 'start+6*halfcycle+halfrise'           vchannel
+ 'start+7*halfcycle-halfrise'           vchannel
+ 'start+7*halfcycle+halfrise'           0
+)

vvdd vdd 0 'vchannel'
vvss vss 0 0
vvssx vssx 0 0

.temp 'temp'

.tran 1ps 6ns sweep monte=${swps}

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

