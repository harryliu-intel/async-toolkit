#!/bin/sh -x

. ./bins.script

mkdir ${TGTDIR} || echo Proceeding...

for p in ${PROGS}; do
	ln -sf $p ${TGTDIR}
done

mkdir ${SGTDIR} || echo Proceeding...

for p in ${SCRIPTS}; do
	ln -sf $p ${SGTDIR}
done

