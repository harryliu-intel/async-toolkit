#!/bin/sh -x
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0


DERIVEDDIR=AMD64_LINUX
PROG=netlist2goxrpt
SPECIALS=transistor.cells
WORKDIR=/p/cloudbreak/sta/mnystroe

MYDIR=`pwd`
XPATH=${MYDIR}/../${DERIVEDDIR}/${PROG}
NET=chip.net.gz

cd ${WORKDIR}

gzip -dc ${NET} | ${XPATH} -f - -r chip -t ${MYDIR}/${SPECIALS} -T gox -l 3  -w '(lambda(nfin) (if (= nfin 1) 30 (- (* 30 nfin) 22)))'

