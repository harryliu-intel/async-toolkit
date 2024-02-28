#!/usr/intel/bin/zsh -x

source rechar_setup_env.zsh

cd ${WARD}/${RECHAR_SIS_WORKDIR}

# Bundle new collateral

echo "===========================     REBUNDLE `date`    ==========================="
pwd

mkdir -p $WARD/${RECHAR_SIS_WORKDIR}/bundles
cd $WARD/${RECHAR_SIS_WORKDIR}/bundles
pwd
$TOP/scripts/rebundle $WARD/${RECHAR_SIS_WORKDIR}/ndms $bundles
