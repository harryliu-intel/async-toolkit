#!/bin/sh -x

# usage :
# $0 [i0s|i0m] [1|2] [ulvt|lvt]

LIB=$1
Z=$2
THRESH=$3
WORKROOT=run_${LIB}_${Z}_${THRESH}_sigma

defaultroots="0p5:/p/hdk/cad/pdk/pdk783_r0.5_22ww52.5/models/core/hspice/m15_2x_1xa_1xb_4ya_2yb_2yc_3yd__bm5_1ye_1yf_2ga_mim3x_1gb__bumpp"

roots=${defaultroots}

1278p3roots="0p5:/p/hdk/cad/pdk/pdk783_r0.5_22ww52.5/models/core/hspice/m15_2x_1xa_1xb_4ya_2yb_2yc_3yd__bm5_1ye_1yf_2ga_mim3x_1gb__bumpp 0p9e:/p/hdk/cad/pdk/pdk783_r0.9e_23ww29.2_beta/models/core/hspice/m15_2x_1xa_1xb_4ya_2yb_2yc_3yd__bm5_1ye_1yf_2ga_mim3x_1gb__bumpp asfit2023ww29:${HOME}/1278_lowvoltage/2023ww29d2/models_core_hspice/1/m14_2x_1xa_1xb_6ya_2yb_2yc__bm5_1ye_1yf_2ga_mim3x_1gb__bumpp"

/bin/rm -rf  $WORKROOT
mkdir $WORKROOT

p1278_3x0p9eu1roots="0p9eu1:/nfs/site/disks/zsc9_fwr_sd_001/mnystroe/p1278_3x0p9eu1/2023ww43d5/models_core_hspice/m14_2x_1xa_1xb_6ya_2yb_2yc__bm5_1ye_1yf_2ga_mim3x_1gb__bumpp"

roots=${p1278_3x0p9eu1roots}

NB="nbjob run --target ${NBPOOL} --class 4C --class 16G --class SLES12 --mode interactive"
do_batch()
{
    shortroot=`echo ${root} | awk -F: '{print $1}'`
    rootpath=`echo ${root} | awk -F: '{print $2}'`
    
    for sigma in $@; do
        sx=`echo $sigma | tr . p`
        WORKDIR=${WORKROOT}/${sx}_${shortroot}

        ${NB} ../AMD64_LINUX/varopt -T ../../varosc/src/circuit.sp -thresh $THRESH -lib ${LIB} -z $Z -r $WORKDIR -sumsq $sigma -hspicemodelroot ${rootpath} ${shortroot} |& tee ${WORKROOT}/run_opt_${sx}_${shortroot}.0 &
    done

    wait
}

for r in ${roots}; do
    
    root=${r}

    # we have more machines now!
    do_batch 0.5 1.0 4.0 4.5 5.0 5.3 6.0

#   do_batch 8.5 9.0 9.5 9.9
    
done


