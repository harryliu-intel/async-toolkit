#!/bin/sh -x
SLOW=5e8
FAST=1e9

./allsizes.sh idle ${SLOW}

for prog in idle read write rw; do
    ./allsizes.sh ${prog} ${FAST}
done
