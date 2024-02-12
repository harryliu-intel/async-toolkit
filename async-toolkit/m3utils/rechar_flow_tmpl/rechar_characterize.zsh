#!/usr/intel/bin/zsh -x

source rechar_setup_env.zsh

# Run new characterization.  
# It would be better if we could do all the corners at once, and parallelize across them all.
cd ${WARD}/${RECHAR_SIS_WORKDIR}

echo "===========================     CHARACTERIZE `date`     ==========================="
pwd


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
    echo "launching millivolts=" $millivolts
    $TOP/scripts/launch $millivolts $celsius $trancorner $capcorner $metaltemp $metalcorner $bundles 
done

if [[ -z $internal ]]; then
    date
    pwd
    $sislaunch -alpha $sislaunch_alpha -clearenv -pllcmds $pllcmds -sisworkers $workers -sispath $sispath -cl ${cell_list} $sislaunchfile
fi

echo "===========================     CHARACTERIZE-DONE `date`     ==========================="
pwd
