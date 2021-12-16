#!/bin/sh -x

prog=$1
speed=$2

NB="nbjob run --target ${EC_SITE}_normal --qslot /bfn/fe --class \"SLES12&&20G\""

${NB} ./runlamb.sh 16 30 ${speed} ${prog}
${NB} ./runlamb.sh 16 38 ${speed} ${prog}
${NB} ./runlamb.sh 16 80 ${speed} ${prog}

${NB} ./runlamb.sh 16 96 ${speed} ${prog}
${NB} ./runlamb.sh 16 144 ${speed} ${prog}
${NB} ./runlamb.sh 24 144 ${speed} ${prog}

${NB} ./runlamb.sh 32 137 ${speed} ${prog}
${NB} ./runlamb.sh 36 144 ${speed} ${prog}
${NB} ./runlamb.sh 48 142 ${speed} ${prog}

${NB} ./runlamb.sh 64 137 ${speed} ${prog}
${NB} ./runlamb.sh 72 110 ${speed} ${prog}
${NB} ./runlamb.sh 80 137 ${speed} ${prog}

${NB} ./runlamb.sh 96 137 ${speed} ${prog}


