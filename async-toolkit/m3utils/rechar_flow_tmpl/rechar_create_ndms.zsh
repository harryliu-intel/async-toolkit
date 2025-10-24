#!/usr/intel/bin/zsh -x

source rechar_setup_env.zsh

cd ${WARD}/${RECHAR_SIS_WORKDIR}

# Create NDMs
echo "===========================     CREATE-NDMS `date`    ==========================="
pwd

mkdir -p $WARD/${RECHAR_SIS_WORKDIR}/ndms
cd $WARD/${RECHAR_SIS_WORKDIR}/ndms
cp $WARD/${RECHAR_SIS_WORKDIR}/*/*/models/liberty/${techlib}*.{lib,ldb} .
$TOP/scripts/gen_ndms $bundles

