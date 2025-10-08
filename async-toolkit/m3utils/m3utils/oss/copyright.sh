#!/bin/sh -x

YEAR=2025
LINE1="Copyright (c) %Y Intel Corporation.  All rights reserved."
LINE2="SPDX-License-Identifier: Apache-2.0"
M3INSTALL=/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils

(find . -type f -and -name \*.\* | xargs -n1 ${M3INSTALL}/copyright/AMD64_LINUX/copyright -year 2025 -line1 "${LINE1}" -line2 "${LINE2}" ) | tee copyright.out 2>&1

(find . -type f -and -name m3makefile | xargs -n1 ${M3INSTALL}/copyright/AMD64_LINUX/copyright -type tex -year 2025 -line1 "${LINE1}" -line2 "${LINE2}" ) | tee -a copyright.out 2>&1

