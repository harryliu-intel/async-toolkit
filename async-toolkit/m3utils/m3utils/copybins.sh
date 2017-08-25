#!/bin/sh -x

. ./bins.script

mkdir ${TGTDIR} || echo Proceeding...

for p in ${PROGS}; do
	cp $p ${TGTDIR}
done

mkdir ${SGTDIR} || echo Proceeding...

for p in ${SCRIPTS}; do
	cp $p ${SGTDIR}
done

