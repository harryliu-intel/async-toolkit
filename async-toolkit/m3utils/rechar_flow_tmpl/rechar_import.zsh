#!/usr/intel/bin/zsh -x
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0


source rechar_setup_env.zsh

# Import library to create Siliconsmart .inst files

echo "===========================     IMPORT `date`    ==========================="
pwd

mkdir -p $WARD/${RECHAR_SIS_WORKDIR}/import
cd $WARD/${RECHAR_SIS_WORKDIR}/import

if [[ -z $internal ]]; then
    rm -f $sislaunchfile
    echo "STDCELLDIR:"${stdcell_dir} > $sislaunchfile
fi

for corner in $corners_list; do
    celsius=${corner:h:h:h:h:h}
    millivolts=${corner:h:h:h:h:t}
    trancorner=${corner:h:h:h:t}
    metalcorner=${corner:h:h:t}
    capcorner=${corner:h:t}
    metaltemp=${corner:t}
    echo "importing millivolts=" $millivolts " celsius=" $celsius
    import_only=1 $TOP/scripts/launch $millivolts $celsius $trancorner $capcorner $metaltemp $metalcorner $bundles 
done

if [[ -z $internal ]]; then
    date
    pwd
    $sislaunch -alpha $sislaunch_alpha -clearenv -pllcmds $pllcmds -sisworkers $workers -sispath $sispath -cl ${cell_list} $sislaunchfile
fi
