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

GENROOT=build_c_api

REGSET=mby
GENDIR=${GENROOT}/${REGSET}_c/src
rm -f ${GENDIR}/*

CFLAGS="-g -c -I../../../../../../c -std=c99"

mkdir -p $GENDIR

mkdir -p work
for file in ${files}; do
	echo ${file}

	${SVPP} ${PATHSPEC} < ${file} > work/intermediate01.rdl
	${PERLFE} < work/intermediate01.rdl > work/intermediate02.rdl
	cat work/intermediate02.rdl | (cd ${WD} ; perl) > work/intermediate03.rdl
	../AMD64_LINUX/genviews -L c-api -top ${top_map} -o ${GENDIR} < work/intermediate03.rdl
	cd ${GENDIR}
	cc ${CFLAGS} mby_top_map.c
#	cc ${CFLAGS} mby_top_map_build.c
	cd -
done

