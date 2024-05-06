#!/bin/sh -x

MYDIR=${M3UTILS}/spice/clock_gen4_latch/src
PROG=${M3UTILS}/spice/schemagraph/AMD64_LINUX/graph

${PROG} -schema ${MYDIR}/clockgen4.schema -S ${MYDIR}/clockgen4.scm -dir graphs *.run/measure.dat.stat
