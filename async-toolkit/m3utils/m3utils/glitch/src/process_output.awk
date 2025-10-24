#!/usr/bin/awk -f
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0


BEGIN { found = 0; }

/START GLITCH SEARCH/ { fn = $4; }

/FOUND GLITCH/ {
    to = $4;
    from = $8;
    printf("%s   --->   %s     %s\n", from, to, fn);
    found = 1;
}


