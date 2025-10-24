#!/bin/sh -x
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0

SLOW=5e8
FAST=1e9

./allsizes.sh idle ${SLOW}

for prog in idle read write rw; do
    ./allsizes.sh ${prog} ${FAST}
done
