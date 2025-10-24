#!/usr/intel/bin/zsh -x
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0


source rechar_setup_env.zsh

cd ${WARD}/${RECHAR_SIS_WORKDIR}

# Create NDMs
echo "===========================     CREATE-NDMS `date`    ==========================="
pwd

mkdir -p $WARD/${RECHAR_SIS_WORKDIR}/ndms
cd $WARD/${RECHAR_SIS_WORKDIR}/ndms
cp $WARD/${RECHAR_SIS_WORKDIR}/*/*/models/liberty/${techlib}*.{lib,ldb} .
$TOP/scripts/gen_ndms $bundles

