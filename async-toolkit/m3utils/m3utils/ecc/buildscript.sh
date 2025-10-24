#!/bin/sh
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0


DBITS=$1

MAXFANIN=4 
#OPTS=-syntactic
OPTS=-structural

AMD64_LINUX/buildecc -w -m write_ecc_${DBITS} -d ${DBITS} -maxfanin ${MAXFANIN} ${OPTS} -defs ecc_defs.v
AMD64_LINUX/buildecc -r -m read_ecc_${DBITS}  -d ${DBITS} -maxfanin ${MAXFANIN} ${OPTS}  

