#!/bin/sh -x

# usage :
# $0 [1|2] [ulvt|lvt]

Z=$1
THRESH=$2
WORKROOT=run_${Z}_${THRESH}_sigma

defaultroots="0p5:/p/hdk/cad/pdk/pdk783_r0.5_22ww52.5/models/core/hspice/m15_2x_1xa_1xb_4ya_2yb_2yc_3yd__bm5_1ye_1yf_2ga_mim3x_1gb__bumpp"

roots=${defaultroots}

1278p3roots="0p5:/p/hdk/cad/pdk/pdk783_r0.5_22ww52.5/models/core/hspice/m15_2x_1xa_1xb_4ya_2yb_2yc_3yd__bm5_1ye_1yf_2ga_mim3x_1gb__bumpp 0p9e:/p/hdk/cad/pdk/pdk783_r0.9e_23ww29.2_beta/models/core/hspice/m15_2x_1xa_1xb_4ya_2yb_2yc_3yd__bm5_1ye_1yf_2ga_mim3x_1gb__bumpp asfit2023ww29:${HOME}/1278_lowvoltage/2023ww29d2/models_core_hspice/1/m14_2x_1xa_1xb_6ya_2yb_2yc__bm5_1ye_1yf_2ga_mim3x_1gb__bumpp"

/bin/rm -rf  $WORKROOT
mkdir $WORKROOT

roots=${1278p3roots}

do_batch()
{
    shortroot=`echo ${root} | awk -F: '{print $1}'`
    rootpath=`echo ${root} | awk -F: '{print $2}'`
    
    for sigma in $@; do
    sx=`echo $sigma | tr . p`
    WORKDIR=${WORKROOT}/${sx}_${shortroot}

    ../AMD64_LINUX/varopt -T ../../varosc/src/circuit.sp -thresh $THRESH -z $Z -r $WORKDIR -sumsq $sigma -hspicemodelroot ${rootpath} ${shortroot} |& tee ${WORKROOT}/run_opt_${sx}_${shortroot}.0 &
done

wait
}

for r in ${roots}; do
    
    root=${r}
    do_batch 0.5 1.5 2.5 3.5 
    do_batch 4.0 4.3 4.6 5.0
    do_batch 5.4 6.0 7.0 7.5
#   do_batch 8.5 9.0 9.5 9.9
    
done


