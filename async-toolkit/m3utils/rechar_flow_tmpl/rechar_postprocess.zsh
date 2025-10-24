#!/usr/intel/bin/zsh -x
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0


source rechar_setup_env.zsh

cd ${WARD}/${RECHAR_SIS_WORKDIR}

# Post-process libs to create all the metal variants, and also to update
# operating condition and comments.

echo "===========================     POSTPROCESS `date`    ==========================="
pwd


$TOP/scripts/run_postprocess */*/models/liberty/liberty*.lib
