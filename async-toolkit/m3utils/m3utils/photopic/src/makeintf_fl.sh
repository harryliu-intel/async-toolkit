# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0

 awk '{printf("T { %d.0d0, FL { %e, %e, %e, %e, %e, %e, %e, %e, %e, %e, %e, %e } },\n", $1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13)}' FlIlluminant.dat | sed s/e/d/g
