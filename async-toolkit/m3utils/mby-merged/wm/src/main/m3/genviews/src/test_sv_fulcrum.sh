#!/bin/sh -ex

. sel_model.sh

pkg=${mapf}_constants_pkg
outfile=${pkg}.vh
     
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
REGSET=mby
LANG=sv-fulcrum
GENDIR=build_${LANG}/${REGSET}/src
METASCMDIR=${METAROOT}/meta/src
ARITH="${METASCMDIR}/algebra.scm ${METASCMDIR}/calculus.scm"

//rm -rf ${GENDIR} || true
mkdir -p ${GENDIR}

rm -rf work || true
mkdir -p work
for file in ${files}; do
	echo ${file}

	${SVPP} ${PATHSPEC} < ${file} > work/intermediate01.rdl
	${PERLFE} < work/intermediate01.rdl > work/intermediate02.rdl
	cat work/intermediate02.rdl | (cd ${WD} ; perl) > work/intermediate03.rdl
	mkdir -p ${GENDIR}
	../AMD64_LINUX/genviews -L ${LANG} -bits ${bits} -top ${ana_map} -o ${GENDIR} -f ../fieldvisitor/src/${mapf}.mapfields -i work/intermediate03.rdl  -packagename ${pkg} -of ${outfile}
done


