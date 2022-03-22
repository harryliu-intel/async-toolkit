#!/bin/sh -x

# we need readlinefe
PATH=../../m3utils/bin:${PATH}

SCMLIBS="../../m3utils/liberty/src/types.scm ../../m3utils/liberty/src/liberty-utils.scm"
SCM=../scm/liberty-scaler.scm
PROG=../../m3utils/liberty/AMD64_LINUX/testparse

${PROG} ${SCMLIBS} ${SCM}
