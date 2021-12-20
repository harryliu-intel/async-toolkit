#!/bin/sh -x

depth=$1
width=$2
freq=$3
prog=$4
temp=$5


ROOT=/nfs/sc/disks/bfn_pd_cb_02/mnystroe/m3utils
BUILD=${ROOT}/lamb/lambbuilder/AMD64_LINUX/lambbuilder
CT=${ROOT}/spice/ct/AMD64_LINUX/ct
APLOTDIR=${ROOT}/lamb/lambbuilder/src/aplot
APLOT=${APLOTDIR}/aplot

out="lambout_${depth}d${width}b_${prog}_${freq}"

export LD_LIBRARY_PATH=${APLOTDIR}:${LD_LIBRARY_PATH}

echo groups:
groups

echo PWD
pwd

# hacks
export hspice_model_root=/nonexistent
export hspice_model=nonexistent

echo ========== RUNLAMB ${out} STARTING =========

${BUILD} -temp ${temp} -vdd 0.75 -w ${width} -d ${depth} -clk ${freq} -prog ${prog} -f hspice -pm io -step 1e-11 -global VDD -global VSS -o ${out} 

hspice -i ${out}.spice -o ${out} -x

${CT} ${out}.tr0 ${out}

psname=`grep "CURRENT-MEASUREMENT VDD" ${out}.spice| awk '{print $3}' | head -1 | tr A-Z a-z | sed s/i// | sed 's/[\( \)]//g'`

${APLOT} ${out} <<EOF > ${out}.avg
range 10:20
avg I_${psname}
EOF
