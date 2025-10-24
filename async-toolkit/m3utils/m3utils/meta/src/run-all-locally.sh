#!/bin/sh -x
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0


chmod +x *.script

for s in *.script; do
		echo launching $s
		./$s 2> ../output/${s}.e > ../output/${s}.o &
done

echo waiting...
wait
