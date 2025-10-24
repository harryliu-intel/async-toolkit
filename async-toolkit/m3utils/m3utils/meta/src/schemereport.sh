#!/bin/sh
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0


results=*.scm.out

rm -f /tmp/tmp$$

for r in $results; do
		echo -n $r " " | sed s/.scm.out// >> /tmp/tmp$$
		grep RESULT $r | awk '{print $2}' >> /tmp/tmp$$
done

sort -g -n -k 2 /tmp/tmp$$  
