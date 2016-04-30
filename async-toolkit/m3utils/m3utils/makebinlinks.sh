#!/bin/sh -x

# link binaries to proper spot, run from root of dev tree

ROOT=`pwd`
BINDIR=`./m3arch.sh`
TGTDIR="${HOME}/bin.`uname -m`"
SGTDIR="${HOME}/bin.script"

PROGS="
${ROOT}/wrap/${BINDIR}/wrap
${ROOT}/timing/${BINDIR}/timing
${ROOT}/lockf/${BINDIR}/lockf
${ROOT}/calarm/anova/program/${BINDIR}/anova
${ROOT}/calarm/regress/calcmetric/${BINDIR}/calcmetric
${ROOT}/calarm/htmltable/cgidb/${BINDIR}/cgidb
${ROOT}/calarm/cumulativeave/${BINDIR}/cumulativeave
${ROOT}/calarm/anova/curves/${BINDIR}/curves
${ROOT}/calarm/regress/dcheck/${BINDIR}/dcheck
${ROOT}/deriv/src/../${BINDIR}/deriv
${ROOT}/calarm/findate/${BINDIR}/findate
${ROOT}/calarm/regress/fusedata/${BINDIR}/fusedata
${ROOT}/calarm/gcoms/gcomsdataextractor/${BINDIR}/gcomsdataextractor
${ROOT}/calarm/traderulelib/getmktdata/${BINDIR}/getmktdata
${ROOT}/calarm/getquotes/${BINDIR}/getticker
${ROOT}/calarm/grabtable/${BINDIR}/grabtable
${ROOT}/calarm/gridctl/${BINDIR}/gridctl
${ROOT}/calarm/regress/hcdriver/${BINDIR}/hcdriver
${ROOT}/calarm/histogram/${BINDIR}/histogram
${ROOT}/calarm/keypaste/${BINDIR}/keypaste
${ROOT}/calarm/until/${BINDIR}/until
${ROOT}/calarm/mktsim/tradeisolator/${BINDIR}/mktisolator
${ROOT}/mscheme/interactive_r/${BINDIR}/mscheme
${ROOT}/calarm/picktickers/${BINDIR}/picktickers
${ROOT}/calarm/polyfit/${BINDIR}/polyfit
/usr/local/pgsql/bin/psql
${ROOT}/calarm/regress/qmctl/${BINDIR}/qmctl
${ROOT}/calarm/m3readline/c/${BINDIR}/readlinefe
${ROOT}/calarm/regress/${BINDIR}/regress
${ROOT}/calarm/regress/resample/${BINDIR}/resample
${ROOT}/calarm/twslib/switchtickers/${BINDIR}/switchtickers
${ROOT}/calarm/anova/tabulate/${BINDIR}/tabulate
${ROOT}/throttle/${BINDIR}/throttle
${ROOT}/calarm/trade/${BINDIR}/trade
${ROOT}/calarm/twslib/testtrade2/client/${BINDIR}/tradeclient
${ROOT}/calarm/twslib/testtrade2/server/${BINDIR}/tradeserver
${ROOT}/calarm/trailingave/${BINDIR}/trailingave
${ROOT}/calarm/dumponeperiod/${BINDIR}/dumponeperiod
${ROOT}/calarm/fastrw/datagen/${BINDIR}/datagen
${ROOT}/calarm/transposecsv/${BINDIR}/transposecsv
${ROOT}/calarm/csvalign/${BINDIR}/csvalign
${ROOT}/calarm/comet/cometcatenate/${BINDIR}/cometcatenate
${ROOT}/spice/ct/${BINDIR}/ct
${ROOT}/spice/vplot/${BINDIR}/vplot
${ROOT}/spice/asserter/${BINDIR}/asserter
${ROOT}/spice/prsgen/${BINDIR}/prsgen
${ROOT}/tcam/schmoozer/${BINDIR}/schmoozer
${ROOT}/tcam/spicebuilder/${BINDIR}/spicebuilder
${ROOT}/nb/nbavail/${BINDIR}/nbavail
"

SCRIPTS="
${ROOT}/calarm/regress/scripts/byticker.awk
${ROOT}/calarm/regress/scripts/sectorovertime.awk
${ROOT}/calarm/regress/scripts/tickerovertime.awk
${ROOT}/calarm/finlib/src/parsegoogle.awk
${ROOT}/calarm/finlib/src/googledata_canada.sh
${ROOT}/calarm/expense/src/oanda.sh
${ROOT}/invoicing/proc.awk
${ROOT}/tcam/schmoozer/src/square.sh
${ROOT}/tcam/spicebuilder/src/runspice.sh
"

mkdir ${TGTDIR} || echo Proceeding...

for p in ${PROGS}; do
	ln -sf $p ${TGTDIR}
done

mkdir ${SGTDIR} || echo Proceeding...

for p in ${SCRIPTS}; do
	ln -sf $p ${SGTDIR}
done

