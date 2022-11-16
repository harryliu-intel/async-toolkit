#!/bin/sh -x
ROOT=../../BTC_STAGES.88f.xa

QUICK=
#QUICK=-quick


../AMD64_LINUX/spicetiming -t ${ROOT}/xa -i ${ROOT}/flat.cdl_gds2 -root core_D_crypto_D_bitcoin_D_stage_D_TEST_U_BTC_U_STAGES_D_1000 -vdd 0.325 ${QUICK}

