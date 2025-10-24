#!/usr/intel/bin/zsh -x

source rechar_setup_env.zsh

cd ${WARD}/${RECHAR_SIS_WORKDIR}/import

echo "===========================     SCALE `date`    ==========================="
pwd

# ######################################################################ctor is
# ######################################################################would
# ######################################################################
# Perform the rescaling.  See env.zsh for the settings.            #####ion.
for control in */*/control; do
    mkdir -p $control.updated
    for inst in $control/*.inst; do
        $TOP/scripts/half_load.pl ${load_factor}  < $inst  \
                                  |\
        $TOP/scripts/adjust_slews.pl ${slew_factor} > $control.updated/${inst:t}
        
        if [[ ${inst:t} =~ "i0[ms]l[a-z][a-z][0-9]10[^/]*$" ]]; then
            echo "Inverting latch : $inst"
            $TOP/scripts/invert_d < $inst > $control.updated/${inst:t}
        fi
    done
done

seq_bundles=(`(pushd $stdcell_dir ; echo *seq_*vt)`)

#seq_bundles=(seq_ulvt seq_lvt seq_svt seq_hvt)
#seq_bundles=(dsedrseq_hvt dsedrseq_svt dsiseq_hvt dsiseq_lvt dsiseq_svt dsiseq_ulvt dsldrseq_hvt dsldrseq_svt dsldrsupseq_hvt dsldrsupseq_svt dsseq_hvt dsseq_svt dssupseq_hvt dssupseq_svt edrseq_hvt edrseq_lvt edrseq_svt edrseq_ulvt ldrdsiseq_hvt ldrdsiseq_lvt ldrdsiseq_svt ldrdsiseq_ulvt ldrseq_hvt ldrseq_lvt ldrseq_svt ldrseq_ulvt ldrsupseq_hvt ldrsupseq_lvt ldrsupseq_svt ldrsupseq_ulvt seq_hvt seq_lvt seq_svt seq_ulvt supseq_hvt supseq_lvt supseq_svt supseq_ulvt)
for bundle in $seq_bundles; do
    for dir in *; do
        if [ -d $dir/$bundle ]; then
            for inst in $dir/$bundle/control.updated/*.inst; do
                clockname=`grep "add_pin.*-clock" ${inst} | awk '{print $2}'`
                echo "set_config_opt -type mpw -from ${clockname} smc_degrade ${seq_char_pushout}" >> $inst
            done
        fi
    done
done
