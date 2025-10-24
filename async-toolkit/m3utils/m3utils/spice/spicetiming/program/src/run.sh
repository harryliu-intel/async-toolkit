#!/bin/sh -x
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0

ROOT=../../BTC_STAGES.88f.xa

QUICK=
#QUICK=-quick
PLOT="-graph 10"
EXTRAARGS=$*

../AMD64_LINUX/spicetiming -t ${ROOT}/xa -i ${ROOT}/flat.cdl_gds2 -root core_D_crypto_D_bitcoin_D_stage_D_TEST_U_BTC_U_STAGES_D_1000 -vdd 0.325 ${QUICK} ${PLOT} ${EXTRAARGS}

