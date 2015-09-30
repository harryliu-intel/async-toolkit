#!/bin/sh

ROOT=/home/user/mnystrom/meta/meta
SCRIPTS=.
SRC=${ROOT}/src

if [ "$1" = "-summary" ]; then
		MAKEREPORT=${SRC}/make-summary-report.sh
else
		MAKEREPORT=${SRC}/make-report.sh
fi

cd $SCRIPTS

rm -f /tmp/report$$
for script in *.script; do
		chop=`echo $script | sed 's/\.script$//'`
		$MAKEREPORT . ../output/tt/0.9V/125C $chop >> /tmp/report$$
done


if [ "$1" = "-summary" ]; then
		cat /tmp/report$$ | sort -g -k 3
else
		cat /tmp/report$$
fi

rm -f /tmp/report$$

