#!/bin/sh -x

. builddefs.sh

WD=/p/hlp/mnystroe/git/hlp-hw/src/srdl

METAROOT=/p/hlp/mnystroe/git/meta-git/

SVPP=${METAROOT}/rdl/svpp/AMD64_LINUX/svpp 
PERLFE=${METAROOT}/perlfe/AMD64_LINUX/perlfe

PATHSPEC="--path ${WD}:/p/hdk/rtl/cad/x86-64_linux30/dt/nebulon/d17ww32.5/include"

rm -f build/src/*
for file in ${files}; do
	echo ${file}

	${SVPP} ${PATHSPEC} < ${file} > work/intermediate01.rdl

	${PERLFE} < work/intermediate01.rdl > work/intermediate02.rdl

	cat work/intermediate02.rdl | (cd ${WD} ; perl) > work/intermediate03.rdl

	../AMD64_LINUX/genviews -top ${top_map} < work/intermediate03.rdl | exit
done

######################################################################
cp mains/${whichtest}_Main.m3 build/src/Main.m3
######################################################################
cat > build/src/m3overrides << _EOF_
%include("../../../../../../m3overrides")
include("../../m3overrides")
_EOF_

######################################################################
cat > build/src/m3makefile << _EOF_
import ("libm3")
import ("wm_support")
include ("m3makefile.maps")
implementation ("Main")
program ("testme")

_EOF_

######################################################################
cd build
cm3 -x
