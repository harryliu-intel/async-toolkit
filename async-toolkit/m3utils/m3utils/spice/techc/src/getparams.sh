#!/bin/sh -x
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0


length=`wc circuit.mc0 | awk '{print $1}'`

hnum=`expr $length - 3`

echo $hnum

head -${hnum} circuit.mc0 | tail -1 | tr '\040' '\012' | grep 'X4.xstage.X[01]' | grep ILN | sort


