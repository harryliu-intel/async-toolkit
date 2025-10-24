#!/bin/sh -x
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0


src=$1

grep ^v ${src} | grep -v "^vvdd " | grep -v "^vvss " | grep -v "^vvssx " | sed -e 's/0\.3/vminer/' -e 's/0\.6/vtrue/' > circuit_v.sp
grep -v ^v ${src} > dut.0.sp
grep ^x dut.0.sp | awk '{print $NF}' | sort | uniq -c | sort -g
sed s/i0scinv00aa1d36x5/@CLOCKCELL@/ dut.0.sp > dut.sp.tmpl
grep ^x ${src} | awk '{for (i=1; i<NF-1; ++i) printf(".probe tran v(%s)\n", $i)}' > probes.sp

echo "Now go do manual edits! : "
echo "Clean up start of dut.sp.tmpl"
echo "Clean up end of dut.sp.tmpl"

echo "Ignore stim file"
