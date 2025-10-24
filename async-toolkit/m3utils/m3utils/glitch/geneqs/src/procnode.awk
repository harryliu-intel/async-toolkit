#!/usr/bin/awk -f
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0


BEGIN {
    search = ARGV[1];
    delete ARGV[1];
    trigger=0;
}

$0~search {
    trigger=1;
}

/FOUND GLITCH/ {
    if (trigger == 1) {
        printf("%s\n", $0);
        exit(0);
    }
}

