#!/bin/sh -x

Z=1
THRESH=lvt
WORKROOT=run_${Z}_${THRESH}_sigma

/bin/rm -rf  $WORKROOT
mkdir $WORKROOT

do_batch()
{
for sigma in $@; do
    sx=`echo $sigma | tr . p`
    WORKDIR=${WORKROOT}/${sx}

    ../AMD64_LINUX/varopt -T ../../varosc/src/circuit.sp -thresh $THRESH -z $Z -r $WORKDIR |& tee ${WORKROOT}/run_opt_${sx}.0 &
done

wait
}

do_batch 4.0 4.2 4.4 4.6
do_batch 4.8 5.0 5.2 5.4
do_batch 5.6 5.8 6.0 6.2



