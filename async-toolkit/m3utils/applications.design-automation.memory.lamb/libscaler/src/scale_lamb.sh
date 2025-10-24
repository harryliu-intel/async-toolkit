#!/bin/sh
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0


GTR_ROOT=${GTR_HOME}/..

# we need readlinefe for interactive use
M3UTILS=${GTR_ROOT}/m3utils

PATH=${M3UTILS}/bin:${PATH}

SCMLIBS="-scm ${M3UTILS}/liberty/src/types.scm -scm ${M3UTILS}/liberty/src/liberty-utils.scm"
SCM0=${GTR_ROOT}/libscaler/scm/liberty-scaler.scm
SCM1=${GTR_ROOT}/libscaler/scm/do-scale.scm

PROG=${M3UTILS}/liberty/AMD64_LINUX/editliberty

echo "running " ${PROG} ${SCMLIBS} -scm ${SCM0} -scm ${SCM1} $*
${PROG} ${SCMLIBS} -scm ${SCM0} -scm ${SCM1} $*
