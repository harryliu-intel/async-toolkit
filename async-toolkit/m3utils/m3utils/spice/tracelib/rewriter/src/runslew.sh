#!/bin/sh -x

ROOT=$1

cp /nfs/site/disks/zsc3_fon_fe_0001/mnystroe/BTC4/BTC4.5/${ROOT}.* .
time ../AMD64_LINUX/rewriter -root ${ROOT} -slew 0.15 0.075 -master -threads 75 -resettime 10e-9 -rewriter `pwd`/rewriter.nb
../AMD64_LINUX/rewriter -root ${ROOT} -slewprint
