#!/usr/bin/awk -f
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0


/^#define/ {
    gsub(/,/,"");
    gsub(/\/\/.*$/,"");
    gsub(/0x/,"16_");
    "../../wm_support/idstyles/AMD64_LINUX/idstyles -is underscore -ic upper -os hyphen -oc lower -sym " $2 | getline nm;
    printf("(%s\t\t%s)\n",nm,$3);
}
