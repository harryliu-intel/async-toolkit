#!/usr/intel/bin/zsh -x
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0


source rechar_setup_env.zsh

echo "===========================     EXTRACT `date`    ==========================="
pwd
# Extract the cells of interest specified in cell_list from the source library
# into:
# .lib.lvf has complete content of each cell
# .lib is the same as .lib.lvf but without OCV and receiver capacitance
#
# This was a workaround to deal with the unintended direct copying of OCV and
# receiver_capacitance tables from the source library, for example, because the
# LVF tables are of different dimensions between the source and the
# recharacterization.  First import from .lib.lvf to generate the .inst files
# that control characterization, then switch to import from .lib files to
# actually run the characterization.  This should be done in a better way.
mkdir -p $WARD/${RECHAR_SIS_WORKDIR}/extracted
cd $WARD/${RECHAR_SIS_WORKDIR}/extracted
$TOP/scripts/extract.zsh $cell_list || exit 1

bundles=($(awk '{print $1}' $cell_list |sort -u))
export bundles
echo "bundles are " $bundles
