#!/bin/sh -x
DIR= /nfs/sc/disks/hlp_0100/yuqisong/hlp-pg-rtl/hlp-work/src/srdl
DIR=.
time ../AMD64_LINUX/genpg -skipholes -bits 28 -sv full.sv -copyrightpath ${MODEL_ROOT}/scripts/intelcopyright.txt ${DIR}/policy_group.csv
