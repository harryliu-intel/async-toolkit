#!/bin/sh -x
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0



cat > cell_list.fwr_plus <<EOF
dsiclk_ulvt i0scfvn13aa1n03x5
ldrdsibase_ulvt i0sxnr002aa1n01x1
ldrdsibase_ulvt i0sxor002aa1n01x1
dsibase_ulvt i0sxor002aa1n02x5
spcl_ulvt i0sddtih0aa1d02x5
spcl_ulvt i0sddtil0aa1d02x5
spcl_lvt i0sddtih0ab1d02x5
spcl_lvt i0sddtil0ab1d02x5
EOF

cat cell_list.andrews cell_list.fwr cell_list.fwr_plus > cell_list.fwr_x0

grep '_ulvt.*.a....x. *$' cell_list.fwr_x0 | sed -e 's/_ulvt/_lvt/' -e 's/\([0-9][a-z]\)a\([0-9][a-z][0-9][0-9]x[0-9]\)/\1b\2/' > cell_list.fwr_x1
grep '_lvt.*.b....x. *$' cell_list.fwr_x0 | sed -e 's/_lvt/_ulvt/' -e 's/\([0-9][a-z]\)b\([0-9][a-z][0-9][0-9]x[0-9]\)/\1a\2/'  > cell_list.fwr_x2

cat cell_list.fwr_x0 cell_list.fwr_x1 cell_list.fwr_x2 | awk '{printf("%s %s\n", $1, $2)}' | sort | uniq > cell_list.fwr_extended


