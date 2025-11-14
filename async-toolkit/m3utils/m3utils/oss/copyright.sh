#!/bin/sh -x

YEAR=2025
LINE1="Copyright (c) %Y Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information."
LINE2="SPDX-License-Identifier: Apache-2.0"
M3INSTALL=/nfs/pdx/disks/or_n3a_disk001/w138/mnystroe/m3utils

(find . -type f -and -name \*.\* | xargs -r -n1 ${M3INSTALL}/copyright/AMD64_LINUX/copyright -year 2025 -line1 "${LINE1}" -line2 "${LINE2}" ) 

(find . -type f -and -name m3makefile | xargs -r -n1 ${M3INSTALL}/copyright/AMD64_LINUX/copyright -type tex -year 2025 -line1 "${LINE1}" -line2 "${LINE2}" )

(find . -type f -and -name Makefile | xargs -r -n1 ${M3INSTALL}/copyright/AMD64_LINUX/copyright -type sh -year 2025 -line1 "${LINE1}" -line2 "${LINE2}" )

(find . -type f -and -name GNUmakefile | xargs -r -n1 ${M3INSTALL}/copyright/AMD64_LINUX/copyright -type sh -year 2025 -line1 "${LINE1}" -line2 "${LINE2}" )

