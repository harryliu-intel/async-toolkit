# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0

 awk -F, '{printf("T { %d.0d0, %e, %e, %e },\n", $1, $2, $3, $4)}' CIE_xyz_1931_2deg.csv | sed s/e/d/g
