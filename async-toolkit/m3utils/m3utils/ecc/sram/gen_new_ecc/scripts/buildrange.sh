#!/bin/sh
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0


DBITS=$1

OPTS=-syntactic
#OPTS=-structural
MAXFANIN=4

rm -rf output
mkdir output

AMD64_LINUX/buildecc -maxfanin ${MAXFANIN} ${OPTS} -range 1 299 output

