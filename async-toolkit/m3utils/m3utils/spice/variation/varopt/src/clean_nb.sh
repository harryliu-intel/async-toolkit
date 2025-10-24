#!/bin/sh -x
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0


rm \##* & 

for cycle in A B; do

for i in 0 1 2 0; do
    ps uxww | grep varopt | awk '{print $2}' | xargs kill -9

    ps uxww | grep vary | awk '{print $2}' | xargs kill -9

    sleep $i
done

nbstatus --target $NBPOOL jobs | grep Run | awk '{print $2}' | xargs -n1 nbjob --target $NBPOOL remove

done

wait
