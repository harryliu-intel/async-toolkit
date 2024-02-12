#!/usr/intel/bin/zsh -x

source rechar_setup_env.zsh

cd ${WARD}/${RECHAR_SIS_WORKDIR}

# Compile .lib to .ldb
echo "===========================     COMPILE-LDBS `date`     ==========================="
pwd
$TOP/scripts/compile_ldb */*/models/liberty/${techlib}*.lib

