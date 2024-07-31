#!/bin/sh -x

MYDIR=${M3UTILS}/spice/adder/src
PROG=${M3UTILS}/spice/schemagraph/AMD64_LINUX/graph

${PROG} -schema ${MYDIR}/adder_rc.schema -S ${MYDIR}/adder_rc.scm -dir graphs */measure.dat.stat
