#!/bin/sh
# $Id$
#
# dirsexist.sh -- take $* and print it, but only for those words that are
# existing directories

PRINTDIRECTION=$1
shift
DIRS=$*
DIRSEXIST=""

for dir in $DIRS; do
	if [ -d $dir ]; then
		DIRSEXIST="${DIRSEXIST} ${dir}"
	fi
done

if [ "X${PRINTDIRECTION}" = "X-f" ]; then
	echo ${DIRSEXIST}
elif [ "X${PRINTDIRECTION}" = "X-r" ]; then 
	echo ${DIRSEXIST} | xargs -n1 echo | tail -r | xargs echo
else
	echo "ERROR, wrong args"
	exit 1
fi

