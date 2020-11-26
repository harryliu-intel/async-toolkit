#!/bin/sh -x

DERIVEDDIR=AMD64_LINUX
PROG=netlist2goxrpt
SPECIALS=transistor.cells
WORKDIR=/p/cloudbreak/sta/mnystroe

MYDIR=`pwd`
XPATH=${MYDIR}/../${DERIVEDDIR}/${PROG}
NET=chip.net.gz

cd ${WORKDIR}

gzip -dc ${NET} | ${XPATH} -f - -r chip -t ${MYDIR}/${SPECIALS} -T gox -l 3

