#!/usr/intel/bin/zsh -x
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0


source rechar_setup_env.zsh

cd ${WARD}/${RECHAR_SIS_WORKDIR}

# Bundle new collateral

echo "===========================     REBUNDLE `date`    ==========================="
pwd

mkdir -p $WARD/${RECHAR_SIS_WORKDIR}/bundles
cd $WARD/${RECHAR_SIS_WORKDIR}/bundles
pwd
$TOP/scripts/rebundle $WARD/${RECHAR_SIS_WORKDIR}/ndms $bundles
