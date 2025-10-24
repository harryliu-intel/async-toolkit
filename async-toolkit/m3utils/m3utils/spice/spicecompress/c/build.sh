#!/bin/sh -x
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0


ASPICE_DIR=/nfs/site/home/mnystroe/work/p4/sw/cad/c/aspice/
#INCLUDES= -I${ASPICE_DIR}
INCLUDES=

cc -ansi -std=c99 -Wall -pedantic ${INCLUDES} -c -g arithdecode.c
cc -ansi -std=c99 -Wall -pedantic ${INCLUDES} -c -g ztrace.c
cc -ansi -std=c99 -Wall -pedantic ${INCLUDES} -c -g testmain.c
cc -ansi -std=c99 -Wall -pedantic ${INCLUDES} -c -g rep16.c
cc -ansi -std=c99 -Wall -pedantic ${INCLUDES} -c -g ztrace_read.c

cc -o testmain testmain.o ztrace.o arithdecode.o rep16.o ztrace_read.o -lm
