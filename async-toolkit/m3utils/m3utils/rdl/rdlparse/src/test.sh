#!/bin/sh -x

rm -f intermediate??.rdl

WD=/p/hlp/mnystroe/git/hlp-hw/src/srdl

METAROOT=/p/hlp/mnystroe/git/meta-git/

SVPP=${METAROOT}/rdl/svpp/AMD64_LINUX/svpp 
PERLFE=${METAROOT}/perlfe/AMD64_LINUX/perlfe

PATHSPEC="--path ${WD}:/p/hdk/rtl/cad/x86-64_linux30/dt/nebulon/d17ww32.5/include"

for file in ${WD}/*.rdl; do

	echo ${file}

	${SVPP} ${PATHSPEC} < ${file} > intermediate01.rdl

	${PERLFE} < intermediate01.rdl > intermediate02.rdl

	cat intermediate02.rdl | (cd ${WD} ; perl) > intermediate03.rdl

	../program/AMD64_LINUX/parserdl < intermediate03.rdl | exit

done
