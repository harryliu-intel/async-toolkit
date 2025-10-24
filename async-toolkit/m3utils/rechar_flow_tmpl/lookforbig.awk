#!/usr/bin/awk -f
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0


# print any number in scientific notation in a lib file

/cell.i0s/ { cell = $0; gotcell=1 }

/e\+/ { if (gotcell) { print NR ":" cell ; print $0; gotcell=0 } }

