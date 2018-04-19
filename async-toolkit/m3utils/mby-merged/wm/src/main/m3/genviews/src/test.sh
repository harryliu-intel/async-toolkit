#!/bin/sh -ex


WD=${MODEL_ROOT}/tools/srdl/mby
# allow security.pm to be found
export PERL5LIB=$MODEL_ROOT/tools/srdl
. builddefs.sh

METAROOT=`ToolConfig.pl get_tool_path meta`

SVPP=${METAROOT}/rdl/svpp/AMD64_LINUX/svpp 
PERLFE=${METAROOT}/perlfe/AMD64_LINUX/perlfe

NEBULON=`ToolConfig.pl get_tool_path nebulon`
CM3_EXEC=`ToolConfig.pl get_tool_exec cm3`
PATHSPEC="--path ${WD}:${NEBULON}/include"

rm -f build/src/*
mkdir -p work
for file in ${files}; do
	echo ${file}

	${SVPP} ${PATHSPEC} < ${file} > work/intermediate01.rdl
	${PERLFE} < work/intermediate01.rdl > work/intermediate02.rdl
	cat work/intermediate02.rdl | (cd ${WD} ; perl) > work/intermediate03.rdl
	../AMD64_LINUX/genviews -top ${top_map} < work/intermediate03.rdl | exit
done

mkdir -p build/src
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
$CM3_EXEC -x
