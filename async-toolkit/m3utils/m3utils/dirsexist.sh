#!/bin/sh
# $Id$
#
# dirsexist.sh -- take $* and print it, but only for those words that are
# existing directories

DIRS=$*
DIRSEXIST=""

for dir in $DIRS; do
	if [ -d $dir ]; then
		DIRSEXIST="${DIRSEXIST} ${dir}"
	fi
done
echo ${DIRSEXIST}
