#!/bin/sh -ex


TARG=$1

if     [ "$1" = "mby"   ]; then
    ana_map="mby_top_map"
    mapf="mby"
    bits=30
elif   [ "$1" = "rx_ppe" ]; then
    ana_map="mby_ppe_rx_top_map"
    mapf="tx_ppe"
    bits=28
elif   [ "$1" = "tx_ppe" ]; then
    ana_map="mby_ppe_tx_top_map"
    mapf="rx_ppe"
    bits=28
else
    echo "targ must be mby, rx_ppe, or tx_ppe"
    exit 1
fi
     
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
GENDIR=build_scm/${REGSET}/src
METASCMDIR=${METAROOT}/meta/src
ARITH="${METASCMDIR}/algebra.scm ${METASCMDIR}/calculus.scm"

rm -rf ${GENDIR} || true
mkdir -p ${GENDIR}

rm -rf work || true
mkdir -p work
for file in ${files}; do
	echo ${file}

	${SVPP} ${PATHSPEC} < ${file} > work/intermediate01.rdl
	${PERLFE} < work/intermediate01.rdl > work/intermediate02.rdl
	cat work/intermediate02.rdl | (cd ${WD} ; perl) > work/intermediate03.rdl
	mkdir -p ${GENDIR}
	../AMD64_LINUX/genviews -L sv-hlp -bits ${bits} -top ${ana_map} -o ${GENDIR} -f ../fieldvisitor/src/${mapf}.mapfields -i work/intermediate03.rdl 
done


