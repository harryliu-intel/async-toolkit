#!/bin/sh -x

# usage :
# $0

Z=1
THRESH=lvt
WORKROOT=run_${Z}_${THRESH}_sigma

export DEBUGLEVEL=10

/bin/rm -rf  $WORKROOT
mkdir $WORKROOT

do_batch()
{
for sigma in $@; do
    sx=`echo $sigma | tr . p`
    WORKDIR=${WORKROOT}/${sx}

    ../AMD64_LINUX/varopt -T ../../varosc/src/circuit.sp -thresh $THRESH -z $Z -r $WORKDIR -sumsq $sigma |& tee ${WORKROOT}/run_opt_${sx}.0 &
done

wait
}

do_batch 6.5 7.0 7.5 8.0
do_batch 8.5 9.0 9.5 9.9



