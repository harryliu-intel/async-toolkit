#!/bin/sh -x

FILE=$1
DEST=test2_out/src

../AMD64_LINUX/bnfgrammar -f ${FILE} -r expression -d ${DEST} -Hy test2.prec -Hl test2.lex
pushd ${DEST}
rm -rf ../AMD64_LINUX
cm3 -x > blah 2>&1 

grep T_Lth blah
grep T_Col blah

