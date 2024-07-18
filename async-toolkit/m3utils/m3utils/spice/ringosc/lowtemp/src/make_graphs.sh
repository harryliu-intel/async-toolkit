#!/bin/sh

GRAPH=${M3UTILS}/spice/schemagraph/AMD64_LINUX/graph
HOME=${M3UTILS}/spice/ringosc/lowtemp/src
SCHEMA=${HOME}/schema.dat
DEFS=${HOME}/defs.scm

${GRAPH} -schema ${SCHEMA} -S ${DEFS} -dir graphs */measure.dat.stat
