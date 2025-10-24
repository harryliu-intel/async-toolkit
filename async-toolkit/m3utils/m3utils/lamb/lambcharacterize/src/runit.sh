#!/bin/sh -x
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0

# -ridge 1.0d-6 cleans up the coefficent signs (ugly)
# -leakscale 0.2768 is from scaling down from 105C to 85C and FFGNP to TT
# -processscale 0.6 is from scaling from N7 to N5 (average of Anurag and Pat)

../AMD64_LINUX/lambcharacterize -d ../../src/ -vdd 0.75 -ridge 1.0d-6 -leakscale 0.2768 -processscale 0.6
