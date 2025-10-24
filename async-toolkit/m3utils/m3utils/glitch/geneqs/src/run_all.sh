#!/bin/sh -x
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0


prog=`pwd`/../AMD64_LINUX/geneqs
workdir=rundir
indir=../../inputs   # relative to rundir/*


cd inputs
pfxs=`ls | sed 's/afifo.*//' | sort | uniq | xargs`
cd ..

cd ${workdir}
for pfx in ${pfxs}; do
	pwd
	echo $pfx
	rm -rf ${pfx}
	mkdir ${pfx}
 	cd ${pfx}
	aliases=${indir}/${pfx}afifo_rdata_logic_term_info_with_flops.txt
	logic=${indir}/${pfx}afifo_to_rdata_logic.txt
	pwd
	${prog} -a ${aliases} -f ${logic} -flopregex "^SDFQ.*"
	cd ..
done

#wait

