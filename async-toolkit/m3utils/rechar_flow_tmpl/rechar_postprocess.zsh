#!/usr/intel/bin/zsh -x

source rechar_setup_env.zsh

cd ${WARD}/${RECHAR_SIS_WORKDIR}

# Post-process libs to create all the metal variants, and also to update
# operating condition and comments.

echo "===========================     POSTPROCESS `date`    ==========================="
pwd


$TOP/scripts/run_postprocess */*/models/liberty/liberty*.lib
