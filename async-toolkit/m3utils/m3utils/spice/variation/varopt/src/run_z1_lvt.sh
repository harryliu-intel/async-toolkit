#!/bin/sh -x 

Z=1
THRESH=lvt
WORKDIR=run_${Z}_${THRESH}

/bin/rm -rf  $WORKDIR

../AMD64_LINUX/varopt -T ../../varosc/src/circuit.sp -thresh $THRESH -z $Z -r $WORKDIR $* |& tee run_opt_$WORKDIR.0
